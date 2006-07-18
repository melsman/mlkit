/* The Bytecode Interpreter for the Kit Abstract Machine */

/* Registers for the KAM
	pc         the code pointer
	sp         the stack pointer (grows downward)
        acc        the accumulator
        env        the closure environment
	exn_ptr    pointer to the current exception frame
	freelist   pointer to the free list -- declared in Region.h
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <setjmp.h>       /* to allow user-defined C-functions to raise exceptions using
			   * the raise_exn primitive */
#include <dlfcn.h> /* Dynamic linking */
#include <string.h>

#include "Runtime.h"
#include "Stack.h"
#include "Tagging.h"
#include "KamInsts.h"
#include "Region.h"
#include "LoadKAM.h"
#include "List.h"
#include "Exception.h"
#include "Interp.h"
#include "String.h"
#include "Math.h"
#include "Table.h"
#include "Locks.h"
#include "Dlsym.h"
#include "Prims.h"

#ifdef KAM
Exception *exn_OVERFLOW;   // Initialized in Interp.c
Exception *exn_INTERRUPT;  // Initialized in Interp.c
Exception *exn_BIND;       // Initialized in Interp.c
Exception *exn_DIV;        // Initialized in Interp.c
Exception *exn_MATCH;      // Initialized in Interp.c
jmp_buf global_exn_env;    // 
void raise_exn(int exn) { 
  longjmp(global_exn_env, exn);   // never returns 
} 
#endif

size_t
printList (uintptr_t l) {    /* function to print out a list */
  printf("\nList = [");
  for (; isCONS(l); l = tl(l))
    printf("%x : elem = %d\n", l, (size_t) hd(l));
  printf("]\n");
  return mlUNIT;
}

/* A sequence of bytecodes */
// typedef unsigned char * bytecode_t;
// bytecode_t start_code;

typedef int int32;
typedef unsigned int uint32;

#define SHORT  (sizeof(short))
#define LONG   (sizeof(int32))
#define DOUBLE (sizeof(double))

#define s32(p) (* (int32 *) (p))
#define s32_1(p) (* (int32 *) (p+4))
#define s32_2(p) (* (int32 *) (p+8))
#define u32_1(p) (* (uint32 *) (p+4))
#define u32_2(p) (* (uint32 *) (p+8))
#define u32(p) (* (uint32 *) (p))

#define u8pc  (unsigned char)(*pc)
#define s32pc s32(pc)
#define s32_1pc s32_1(pc)
#define s32_2pc s32_2(pc)
#define u32pc u32(pc)
#define u32_1pc u32_1(pc)
#define u32_2pc u32_2(pc)
#define inc32pc pc += 4
#define inc2_32pc pc += 8

#define Raise(EXNVALUE) {                                                                 \
 	debug(printf("RAISE; EXNVALUE = %x\n", EXNVALUE));                                \
	deallocateRegionsUntil((Region)exnPtr, topRegionCell);                            \
	debug(printf(" after deallocateRegionsUntil\n"));                                 \
                                                                                          \
	sp = exnPtr - 1;                      /* reset stack pointer */         \
	exnPtr = (uintptr_t *)*exnPtr;               /* enable the previous handler */ \
                                                                                          \
	debug(printf(" now calling the handler function\n"));                             \
                                                        /* now do the function call! The  \ 
						         * closure and the return address \
							 * are on the stack... */         \
	env = (int *) selectStackDef(0);                /* one argument */                \
	debug(printf("Writing to sp = 0x%x\n", sp -1));                                   \
	pushDef(EXNVALUE);                                                                \
	pc = (bytecode_t) *env;                                                           \
}

#define Setup_for_c_call  int return_value; \
                          if( (return_value = setjmp(global_exn_env)) == 0 ) {

                         
#define Restore_after_c_call  } else { \
                                debug(printf("\n***Exception raised***\n")); \
                                acc = return_value; \
                                goto raise_exception; \
                              }

#define JUMPTGT(offset) (bytecode_t)(pc + offset)
#define branch() pc = JUMPTGT(s32pc)

#ifdef LAB_THREADED
#define Instruct(name) lbl_##name
#define Next { temp = (uintptr_t)pc; inc32pc;  goto **(void **)temp; }
#else
#define Instruct(name) case name
#define Next break
#endif /*LAB_THREADED*/

#ifdef DEBUG
#define debug(Arg) Arg
#else
#define debug(Arg) {}
#endif

#define primintbinop(name,msg,bop)                \
  Instruct(name): {                           \
    acc = ((int)(popValDef)) bop ((int)acc);  \
    debug(printf("%s gives %d\n", msg,acc));  \
    Next;                                     \
  }

#define primfbinop(name,msg,bop)                  \
  Instruct(name): {                           \
    *(double*)acc = (*(double*)selectStackDef(-2)) bop (*(double*)selectStackDef(-1));  \
    popNDef(2);                               \
    Next;                                     \
  }

#define primfunaryop(name,msg,uop)                \
  Instruct(name): {                           \
    *(double*)acc = uop (*(double*)popValDef);  \
    Next;                                     \
  }

#define primwbinop(name,msg,bop)            \
  Instruct(name): {                           \
    acc = ((unsigned long)(popValDef)) bop ((unsigned long)acc);  \
    debug(printf("%s gives %x\n", msg,acc));  \
    Next;                                     \
  }

#define priminttest(name,msg,tst)	              \
    Instruct(name): {		                      \
      if (((int)popValDef) tst ((int)acc))            \
        acc = mlTRUE;                                 \
      else                                            \
        acc = mlFALSE;                                \
      debug(printf("%s gives acc = %d\n", msg, acc)); \
      Next;                                           \
    }                     

#define primftest(name,msg,tst)	                      \
    Instruct(name): {		                      \
      if (get_d(popValDef) tst get_d(acc))            \
        acc = mlTRUE;                                 \
      else                                            \
        acc = mlFALSE;                                \
      debug(printf("%s gives acc = %d\n", msg, acc)); \
      Next;                                           \
    }                     


#define primwtest(name,msg,tst)	                      \
    Instruct(name): {		                      \
      unsigned long t1, t2;                           \
      t1 = (unsigned long)popValDef;                  \
      t2 = (unsigned long)acc;                        \
      if ((t1) tst (t2))                              \
        acc = mlTRUE;                                 \
      else                                            \
        acc = mlFALSE;                                \
      debug(printf("%s(%d,%d) gives acc = %d\n", msg, t1, t2, acc)); \
      Next;                                           \
    }                     

/* the following doesn't work with gcc 2.96 under Redhat 7.0 ...
#define primwtest(name,msg,tst)	                      \
    Instruct(name): {		                      \
      if (((unsigned long)popValDef) tst ((unsigned long)acc))  \
        acc = mlTRUE;                                 \
      else                                            \
        acc = mlFALSE;                                \
      debug(printf("%s gives acc = %d\n", msg, acc)); \
      Next;                                           \
    }                     
*/
// Do not pop value on stack, as used by the binary search
// on switches.

/*
#define iftest(name, msg,tst)                   \
   Instruct(name): {                            \
     if (((int)selectStackDef(-1)) tst ((int)acc))    \
       branch();                                \
     else                                       \
       inc32pc;                                 \
     debug(printf("%s %d and %d\n", msg,selectStackDef(-1),acc));     \
	Next;                                   \
   }
*/

#define iftestimmed(name, msg, tst)             \
   Instruct(name): {                            \
     debug(printf("%s %d and %d\n",msg,acc,s32_1pc));  \
     if (((int)acc) tst ((int)s32_1pc))         \
       branch();                                \
     else {                                     \
       inc32pc;                                 \
       inc32pc;                                 \
     }                                          \
     Next;                                      \
   }

#define allocN {                 \
  debug(printf("allocN %d\n", s32pc)); \
  acc = (int) alloc((Region)acc, s32pc); \
}

#define allocIfInfN {              \
  debug(printf("allocIfInfN %d acc = 0x%x\n", s32pc, acc)); \
  if (is_inf(acc)) {                 \
    debug(printf("  allocating\n")); \
    acc = (int) alloc((Region)acc, s32pc);   \
  }                                  \
}

#define allocSatInfN {           \
  debug(printf("allocSatInfN %d\n", s32pc)); \
  if (is_atbot((Region)acc))             \
    resetRegion((Region)acc);            \
  acc = (int) alloc((Region)acc, s32pc); \
}

#define allocSatIfInfN {            \
  debug(printf("allocSatIfInfN %d acc = 0x%x\n", s32pc,acc)); \
  if (is_inf_and_atbot((Region)acc)) {       \
    resetRegion((Region)acc);               \
    debug(printf("  resetting\n")); \
  }                                 \
  if (is_inf((Region)acc)) {                \
    debug(printf("  allocating\n")); \
    acc = (int) alloc((Region)acc, s32pc);  \
  }                                 \
}

#define allocAtbotN {            \
  debug(printf("allocAtbotN %d\n", s32pc)); \
  resetRegion((Region)acc);              \
  acc = (int) alloc((Region)acc, s32pc); \
}

#define blockCopy2 { \
  *((int *)acc + 1) = popValDef; \
  *((int *)acc) = popValDef; \
}  

#define blockCopyN {                  \
  debug(printf("blockCopyN %d at %x\n", s32pc,acc)); \
  for (temp=s32pc-1;temp>=0;temp--)   \
    *(((int *)acc)+temp) = popValDef; \
}


/* To get things to work with threadding, we need to be able to 
 * transform instruction numbers to instruction addresses (pointers to 
 * labels, e.g.: &&lbl_RETURN). Unfortunately, the address of a label 
 * in a C function can be taken only inside the C function. In our case, 
 * the instruction addresses within the interp function can be resolved
 * with the notation &&lbl_RETURN only within the function interp. 
 *
 * Thus, to make it possible to transform code sequences separately from the 
 * execution step (e.g., for caching), we arrange that interp can be 
 * in two modes, `RESOLVEINSTS' and `INTERPRET'. When interp is called in `RESOLVEINSTS' 
 * mode, instructions are resolved in the code and the interp function returns 
 * without the code being executed. In this mode, the value of sp, ds, and 
 * exnCnt are not used. Contrary, when interp is called in mode 
 * `INTERPRET', the interp function executes the code, assuming that instructions
 * have been resolved already.
 */

/* replace instruction numbers with instruction addresses */
void 
resolveInstructions(int sizeW, bytecode_t start_code,
                    void * jumptable [], unsigned int jumptableSize,
                    void *ccalltable[]) {
  unsigned long *real_code;
  int tmp, tmp2;
  int j, i = 0;
  real_code = (unsigned long*)start_code;

  while ( i < sizeW ) {
    int arity;
    unsigned long inst;
    inst = *(real_code + i);
    arity = getInstArity(inst);
    if ( arity == -100 )
    { // Check to see if we already resolved this code
      // This is not entirely sound, but it would be very coincidential 
      // if an instrution number without an arity is the same as
      // a pointer to an instruction in our interpreter.
      // This is needed to let apache restart without trouble
      for (j = 0; j < jumptableSize; j++)
      {
        if (((unsigned long) jumptable[j]) == inst)
        {
          return;
        }
      }
      fprintf(stderr, "No arity for inst %ld\n", inst);
      die("Interp.resolveInstructions");
    }
    debug(printf("i=%d ; inst = %d; arity = %d\n", i, inst, arity));
    if (inst > 1000) {
      printf ("sizeW = %d, i= %d, inst = %ld\n", sizeW, i, inst);
      die ("resolveInstructions: Hmm - inst number > 1000");
    }
    *(real_code + i) = (unsigned long)(jumptable[inst]);
    for (tmp = 0, tmp2 = 0; tmp < 7; tmp++)
    {
      if (jumptable[inst] == ccalltable[tmp]) tmp2 = 1;
    }
    if (tmp2)
    {
      inst = real_code[i+1];
      if (inst != 0) // Static Ccall
      {
        //printf("converting %d to %x\n", inst, cprim[inst-1]);
        real_code[i+1] = (unsigned long) cprim[inst-1];
      }
    }
    switch (arity) {  /* IMMED_STRING -- compute arity... */
    case -1: 
      {
	int str_size;
	int str_size_bytes = get_string_size(*(real_code + i + 1));
	str_size_bytes += 1;            // zero-termination
	if (str_size_bytes % 4 != 0)
	  str_size_bytes += (4 - (str_size_bytes % 4));
	str_size = str_size_bytes / 4;	
	arity = str_size + 1;   /*tag*/
	break;
      } 
    case -2: 
      {  /* JMP_VECTOR -- compute arity */
	int jvec_size = *(real_code + i + 3);
	arity = jvec_size + 3;      
	debug(printf("jvec_size = %d; arity = %d\n", jvec_size, arity));
	break;
      }
    case -3: 
      {
	die ("resolveInstructions: DOT_LABEL - opcode not expected!");
	break;
      }
    case -4: 
      {
	die ("resolveInstructions: LABEL - opcode not expected!");
	break;
      }
    };
    i += (arity + 1);   /* 1 for the opcode */
  }
}

enum interp_mode {
  RESOLVEINSTS,
  INTERPRET
};

static ssize_t 
interp(Interp* interpreter,    // Interp; NULL if mode=RESOLVEINSTS
       uintptr_t * sp0,    // Stack pointer
       uintptr_t * ds,     // Data segment pointer
       uintptr_t * exnPtr, // Pointer to next exn-handler on stack
       Ro ** topRegionCell,    // Cell for holding a pointer to the top-most region
       char ** errorStr,       // Cell to store error-string in case of an uncaught exception
       size_t *exnCnt,  // Exception name counter
       bytecode_t b_prog,      // The actual code
       size_t sizeW,              // Size of code in words
       int interp_mode,        // Mode: RESOLVEINSTS or INTERPRET
       serverstate serverCtx)        // Apache request_rec pointer
{

/* Declarations for the registers of the abstract machine.
   The most heavily used registers come first.
   For reasonable performance, "pc" MUST reside in a register.
   Many ``optimizing'' compilers underestimate the importance of "pc",
   and don't put it in a register. 
   For GCC users, registers are hans-assigned for some architectures. 
*/

  register ssize_t acc;
  
#if defined(__GNUC__) && defined(i386)
  register bytecode_t pc asm("%esi");
  register uintptr_t * sp asm("%edi");
#else
  register bytecode_t pc;
  register uintptr_t * sp;
#endif

  bytecode_t pc_temp;
  int *env = NULL;
  uint32 cur_instr = 0;
  ssize_t temp;
  ssize_t *tmp2;
  //  c_primitive primtmp;


#ifdef LAB_THREADED
  static void * jumptable[] = {
#   include "jumptbl.h"
  };
  static void *ccalltable[] = 
  {
    &&lbl_C_CALL0,
    &&lbl_C_CALL1,
    &&lbl_C_CALL2,
    &&lbl_C_CALL3,
    &&lbl_C_CALL4,
    &&lbl_C_CALL5,
    &&lbl_C_CALL6,
    &&lbl_C_CALL7
  };
  static size_t jumptableSize = sizeof(jumptable) / sizeof(void *);
#endif

  acc = convertIntToML(0);
  pc = b_prog;
  sp = sp0;

  debug(printf("Entering interp\n"));

  if ( interp_mode == RESOLVEINSTS ) {
#ifdef LAB_THREADED
    resolveInstructions(sizeW, b_prog, jumptable, jumptableSize, ccalltable);
    debug(printf("returning from interp\n"));
#endif
    return 0;
  }

#ifdef LAB_THREADED
  Next;                 // jump to first instruction
#else
  while (1) {
    debug(if ( (unsigned long)pc < 10000 ) printf("*** LOW PC ***\n") );
    cur_instr = u32pc;
    debug(printf("0x%x: ", pc));
    inc32pc;
    switch (cur_instr) {
#endif /*LAB_THREADED*/

      Instruct(ALLOC_N): {
	allocN;
	inc32pc;
	Next;
      }
      Instruct(ALLOC_IF_INF_N): {
	allocIfInfN;
	inc32pc;
	Next;
      }
      Instruct(ALLOC_SAT_INF_N): {
	allocSatInfN;
	inc32pc;
	Next;
      }
      Instruct(ALLOC_SAT_IF_INF_N): {
	allocSatIfInfN;
	inc32pc;
	Next;
      }
      Instruct(ALLOC_ATBOT_N): {
	allocAtbotN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_ALLOC_2): {
	acc = (int) alloc((Region)acc, 2);
	blockCopy2;
	Next;
      }
      Instruct(BLOCK_ALLOC_N): {
	allocN;
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_ALLOC_IF_INF_N): {
	allocIfInfN;
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_ALLOC_SAT_INF_N): {
	allocSatInfN;
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_N): {
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_ALLOC_SAT_IF_INF_N): {
	allocSatIfInfN;
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(BLOCK_ALLOC_ATBOT_N): {
	allocAtbotN;
	blockCopyN;
	inc32pc;
	Next;
      }
      Instruct(CLEAR_ATBOT_BIT): {
	debug(printf("clearAtbotBit\n"));
	acc = clearAtbotBit(acc);
	Next;
      }

      Instruct(SET_BIT_30):
      Instruct(SET_ATBOT_BIT): {
	debug(printf("setAtbotBit\n"));
	acc = setAtbotBit(acc);
	Next;
      }
      Instruct(SET_BIT_31): {
	debug(printf("setInfiniteBit\n"));
	acc = setInfiniteBit(acc);
	Next;
      }

      Instruct(CLEAR_BIT_30_AND_31): {
	debug(printf("clearBitStatusBits\n"));
	acc = (int)clearStatusBits((Region)acc);
	Next;
      }

      Instruct(PUSH): {
	pushDef(acc);
	debug(printf("PUSH with acc %d (0x%x) - sp = 0x%x\n", acc,acc,sp));
	Next;
      }
      Instruct(PUSH_LBL): {
	debug(printf("PUSH_LBL: %x\n", JUMPTGT(s32pc)));
	pushDef((int) JUMPTGT(s32pc));
	inc32pc;
	Next;
      }

      Instruct(POP_1): { popNDef(1); Next; }
      Instruct(POP_2): { popNDef(2); Next; }

      Instruct(POP_N): {
	popNDef(s32pc);
	debug(printf("POP_N(%d) - sp = 0x%x\n",s32pc, sp));
	inc32pc;
	Next;
      }

      Instruct(APPLY_FN_CALL): {   /*mael: ok*/
	debug(printf("APPLY_FN_CALL(acc %d, num args %d, return address %x on stack address %x)\n",acc,s32pc,selectStackDef(-s32pc-1), sp-s32pc-1));
	temp = (int) env;
	env = (int *) selectStackDef(-s32pc);
	selectStackDef(-s32pc) = temp;
	debug(printf("Writing to sp = 0x%x\n", sp -s32pc));
	pushDef(acc);
	pc = (bytecode_t) *env;
	Next;
      }
      Instruct(APPLY_FN_JMP): {   /*mael: ok*/
	debug(printf("APPLY_FN_JMP(acc %d, num args = %d, num rets = %d)\n",acc,s32pc,s32_1pc));
	env = (int *) selectStackDef(-s32pc);
	for (temp=0;temp<s32pc-1;temp++) {
	  selectStackDef(-s32pc-s32_1pc+temp) = selectStackDef(-s32pc+1+temp);
	  debug(printf("Writing to sp = 0x%x\n", sp -s32pc-s32_1pc+temp));
	}
	popNDef(s32_1pc+1);	
	pushDef(acc);
	pc = (bytecode_t) *env;
	Next;
      }
      Instruct(APPLY_FUN_CALL1): {  /*mael: ok*/
	temp = (int) env;
	env = (int *) selectStackDef(-1);
	selectStackDef(-1) = temp;
	pushDef(acc);
	branch();
	Next;
      }
      Instruct(APPLY_FUN_CALL2): {  /*mael: ok*/
	temp = (int) env;
	env = (int *) selectStackDef(-2);
	selectStackDef(-2) = temp;
	pushDef(acc);
	branch();
	Next;
      }
      Instruct(APPLY_FUN_CALL3): {  /*mael: ok*/
	temp = (int) env;
	env = (int *) selectStackDef(-3);
	selectStackDef(-3) = temp;
	pushDef(acc);
	branch();
	Next;
      }
      Instruct(APPLY_FUN_CALL): {  /*mael: ok*/
	debug(printf("APPLY_FUN_CALL with first arg %d and target rel. addr %d and num args \n", acc, s32pc, s32_1pc));
	temp = (int) env;
	env = (int *) selectStackDef(-s32_1pc);
	selectStackDef(-s32_1pc) = temp;
	debug(printf("Writing to sp = 0x%x\n", sp -s32_1pc));
	pushDef(acc);
	branch();
	Next;
      }
      Instruct(APPLY_FUN_JMP): {   /*mael: ok*/
	debug(printf("APPLY_FUN_JMP(%d,%d,%d)\n",s32pc,s32_1pc,s32_2pc));
	env = (int *) selectStackDef(-s32_1pc);
	for (temp=0;temp<s32_1pc-1;temp++) {
	  selectStackDef(-s32_1pc-s32_2pc+temp) = selectStackDef(-s32_1pc+1+temp);
	  debug(printf("Writing to sp = 0x%x\n", sp -s32_1pc-s32_2pc+temp));
	}
	popNDef(s32_2pc+1);	
	pushDef(acc);
	branch();
	Next;
      }
      Instruct(LETREGION_FIN): {
	debug(printf("LETREGION_FIN %d at %x\n", u32pc, (int)sp));
	offsetSP(u32pc);
	inc32pc;
	Next;
      }
      Instruct(LETREGION_INF): {
	debug(printf("LETREGION_INF desc at 0x%x, region at 0x%x\n",(int)sp,acc));
	acc = (int) allocateRegion((Ro*) sp, topRegionCell);
	offsetSP(sizeRo);
	Next;
      }
      Instruct(ENDREGION_INF): {
	debug(printf("endregionInf\n"));
	deallocateRegion(topRegionCell);
	popNDef(sizeRo);
	Next;
      }
      Instruct(RESET_REGION): {
	debug(printf("resetRegion\n"));
	resetRegion((Region)acc);
	Next;
      }
      Instruct(MAYBE_RESET_REGION): {
	debug(printf("maybeRegionRegion\n"));
	if (is_inf_and_atbot((Region)acc))
	  resetRegion((Region)acc);
	Next;
      }
      Instruct(RESET_REGION_IF_INF): {
	debug(printf("resetRegionIfInf\n"));
	if (is_inf((Region)acc))
	  resetRegion((Region)acc);
	Next;
      }

      Instruct(IMMED_INT0): { acc = 0; Next; }
      Instruct(IMMED_INT1): { acc = 1; Next; }
      Instruct(IMMED_INT2): { acc = 2; Next; }
      Instruct(IMMED_INT3): { acc = 3; Next; }

      Instruct(IMMED_INT): {
	acc = s32pc;
	inc32pc;
	debug(printf("IMMED_INT: %d\n", acc));
	Next;
      }

      Instruct(IMMED_REAL): {
	acc = (unsigned long)pc;
	inc32pc;
	inc32pc;
	debug(printf("IMMED_REAL(%f): acc=%d\n", *(double*)acc, acc));
	Next;
      }

      Instruct(IMMED_STRING): {
	acc = (int) pc; 
	temp = get_string_size(s32pc) + 1;  // zero-termination
	if (temp % 4 != 0) 
	  temp += (4 - (temp % 4));
	pc += (temp + 4);
	debug(printf("IMMED STRING with aligned size %d and acc %d\n", temp, acc));
	Next;
      }

      Instruct(PRIM_NEG_I): {
	if ( acc == -2147483647 - 1)
	  goto raise_overflow;
	acc = -acc;
	debug(printf("PRIM_NEG_I gives %d\n", acc));
	Next;
      }
/*
    raise_bind:
      acc = (int)&exn_BIND;
      goto raise_exception;
    raise_match:
      acc = (int)&exn_MATCH;
      goto raise_exception;
    raise_div:
      acc = (int)&exn_DIV;
      goto raise_exception;
*/
    raise_overflow:
      acc = (int)&exn_OVERFLOW;
      goto raise_exception;
/*
    raise_interrupt:
      acc = (int)&exn_INTERRUPT;
      goto raise_exception;
*/
      Instruct(PRIM_ABS_I): {
	if ( acc < 0 ) {
	  if ( acc == -2147483647 - 1)
	    goto raise_overflow;
	  acc = -acc;
	}
	debug(printf("PRIM_ABS_I gives %d\n",acc));
	Next;
      }

      Instruct(PRIM_SUB_I1): {
	if ( acc == Min_Int ) goto raise_overflow;
	acc = acc - 1;
	Next;
	/*
	temp = acc;
	acc = acc - 1;
	if ( acc > temp ) goto raise_overflow;
	Next;
	*/
      }

      Instruct(PRIM_SUB_I2): {
	if ( acc == Min_Int || acc == Min_Int + 1 ) goto raise_overflow;
	acc = acc - 2;
	Next;
	/*
	temp = acc;
	acc = acc - 2;
	if ( acc > temp ) goto raise_overflow;
	Next;
	*/
      }

      Instruct(PRIM_SUB_I): {
	int temp1 = popValDef;
	temp = acc;
	acc = temp1 - temp;
	debug(printf("PRIM_SUB_I gives %d\n", acc));
	if ( ( temp1 > 0 && temp < 0 && (acc < -temp || acc < temp1) ) 
	     || ( temp1 <= 0 && temp > 0 && (acc > -temp || acc > temp1) ) )	     
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_ADD_I1): {
	if ( acc == Max_Int ) goto raise_overflow;
	acc = acc + 1;
	Next;
	/*      
	temp = acc;
	acc = acc + 1;
	if ( acc < temp ) goto raise_overflow;
	Next;
	*/
      }

      Instruct(PRIM_ADD_I2): {
	if ( acc == Max_Int || acc == Max_Int - 1 ) goto raise_overflow;
	acc = acc + 2;
	Next;
	/*
	temp = acc;
	acc = acc + 2;
	if ( (int)acc < (int)temp ) goto raise_overflow;
	Next;
	*/
      }

      Instruct(PRIM_ADD_I): {
	int temp1 = popValDef;
	temp = acc;
	acc = temp1 + acc;
	debug(printf("PRIM_ADD_I gives %d\n", acc));
	if ( ( temp1 > 0 && temp > 0 && (acc < temp || acc < temp1) ) 
	     || ( temp1 <= 0 && temp < 0 && (acc > temp || acc > temp1) ) )	     
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_MUL_I): {
	int temp1 = popValDef;
	temp = acc;
	acc = temp1 * temp;
	debug(printf("PRIM_MUL_I gives %d\n", acc));
	if ( (temp1 != 0) && (acc / temp1 != temp) )
	  goto raise_overflow;
	Next;
      }

      primwbinop(PRIM_SUB_W,"PRIM_SUB_W",-);
      primwbinop(PRIM_ADD_W,"PRIM_ADD_W",+);
      primwbinop(PRIM_MUL_W,"PRIM_MUL_W",*);

      primwbinop(PRIM_AND_W,"PRIM_AND_W", &);
      primwbinop(PRIM_OR_W,"PRIM_OR_W", |);
      primwbinop(PRIM_XOR_W,"PRIM_XOR_W", ^);
      primwbinop(PRIM_SHIFT_LEFT_W,"PRIM_SHIFT_LEFT_W", <<);
      primintbinop(PRIM_SHIFT_RIGHT_SIGNED_W,"PRIM_SHIFT_RIGHT_SIGNED_W", >>);
      primwbinop(PRIM_SHIFT_RIGHT_UNSIGNED_W,"PRIM_SHIFT_RIGHT_UNSIGNED_W", >>);

      priminttest(PRIM_EQUAL_I,"PRIM_EQUAL_I",==);
      priminttest(PRIM_LESS_EQUAL,"PRIM_LESS_EQUAL",<=);
      priminttest(PRIM_LESS_THAN,"PRIM_LESS_THAN",<);
      priminttest(PRIM_GREATER_THAN,"PRIM_GREATER_THAN",>);
      priminttest(PRIM_GREATER_EQUAL,"PRIM_GREATER_EQUAL",>=);

      primwtest(PRIM_LESS_EQUAL_UNSIGNED,"PRIM_LESS_EQUAL_UNSIGNED",<=);
      primwtest(PRIM_LESS_THAN_UNSIGNED,"PRIM_LESS_THAN_UNSIGNED",<);
      primwtest(PRIM_GREATER_THAN_UNSIGNED,"PRIM_GREATER_THAN_UNSIGNED",>);
      primwtest(PRIM_GREATER_EQUAL_UNSIGNED,"PRIM_GREATER_EQUAL_UNSIGNED",>=);

      // Special instructions for binary search on switches.

      Instruct(IF_NOT_EQ_JMP_REL_IMMED3): {
	if (((int)acc) != 3)
	  branch();
	else {
	  inc32pc;
	}
	Next;
      }

      iftestimmed(IF_NOT_EQ_JMP_REL_IMMED,"IF_NOT_EQ_JMP_REL_IMMED",!=);
      iftestimmed(IF_LESS_THAN_JMP_REL_IMMED,"IF_LESS_THAN_JMP_REL_IMMED",<);
      iftestimmed(IF_GREATER_THAN_JMP_REL_IMMED,"IF_GREATER_THAN_JMP_REL_IMMED",>);

      // Floating point instructions
      primfbinop(PRIM_ADD_F, "PRIM_ADD_F", +);
      primfbinop(PRIM_SUB_F, "PRIM_SUB_F", -);
      primfbinop(PRIM_MUL_F, "PRIM_MUL_F", *);
      primfbinop(PRIM_DIV_F, "PRIM_DIV_F", /);
      primfunaryop(PRIM_NEG_F, "PRIM_NEG_F", -);
      primfunaryop(PRIM_ABS_F, "PRIM_ABS_F", fabs);
      primftest(PRIM_LESS_EQUAL_F,"PRIM_LESS_EQUAL_F",<=);
      primftest(PRIM_LESS_THAN_F,"PRIM_LESS_THAN_F",<);
      primftest(PRIM_GREATER_THAN_F,"PRIM_GREATER_THAN_F",>);
      primftest(PRIM_GREATER_EQUAL_F,"PRIM_GREATER_EQUAL_F",>=);

      Instruct(JMP_VECTOR): {
	temp = s32pc + (acc-s32_1pc)*4;
	debug(printf("s32pc = %d \n",s32pc));
	debug(printf("s32_1pc = %d \n",s32_1pc));
	debug(printf("acc = %d \n",acc));
	debug(printf("JMP_VECTOR(%x) with offset %d\n", cur_instr,temp));
	debug(printf("value in slot %x \n",(*((int32 *)(pc+temp)))));
	pc = JUMPTGT((*((int32 *)(pc+temp)))+temp);
	debug(printf("instruct in slot pc %x\n",s32pc));
	Next;
      }
      
      Instruct(JMP_REL): {
	debug(printf("JMP_REL with offset %d\n", s32pc));
	branch();
	Next;
      }
      Instruct(C_CALL0): { 
	Setup_for_c_call;
	debug(printf("C_CALL0(%d)\n", u32pc));
	acc = ((c_primitive) u32pc)();
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL0 end\n"));
	Next;
      }
      Instruct(C_CALL1): { 
	Setup_for_c_call;
	//debug(printf("C_CALL1(%d) with acc %d (0x%x)\n", cprim[u32pc], acc, acc));
	debug(printf("C_CALL1(%d) with acc %d (0x%x)\n", u32pc, acc, acc));
	acc = ((c_primitive) u32pc)(acc);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL1 end\n"));
	Next;
      }
      Instruct(C_CALL2): { 
	Setup_for_c_call;
	debug(printf("C_CALL2(%d) with acc %d and arg %d\n", u32pc, acc, selectStackDef(-1)));
	//debug(printf("C_CALL2(%d) with acc %d and arg %d\n", cprim[u32pc], acc, selectStackDef(-1)));
	acc = ((c_primitive) u32pc)(popValDef, acc);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL2 end\n"));
	Next;
      }
      Instruct(C_CALL3): { 
	Setup_for_c_call;
	debug(printf("C_CALL3(%d) with acc %d and arg %d\n", u32pc, acc, selectStackDef(-1)));
	temp = popValDef;
	acc = ((c_primitive) u32pc)(popValDef, temp, acc);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL3 end\n"));
	Next;
      }
      Instruct(C_CALL4): { 
	Setup_for_c_call;
	debug(printf("C_CALL4 - %d - (%d,%d,%d,%d)\n", u32pc, selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc));
	acc = ((c_primitive) u32pc)(selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc);
	popNDef(3);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL4 end\n"));
	Next;
      }

      Instruct(C_CALL5): { 
	Setup_for_c_call;
	debug(printf("C_CALL5 - %d - (%d,%d,%d,%d,%d)\n", u32pc, selectStackDef(-4), 
		     selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc));
	acc = ((c_primitive) u32pc)(selectStackDef(-4), selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc);
	popNDef(4);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL5 end\n"));
	Next;
      }

      Instruct(C_CALL6): { 
	Setup_for_c_call;
	debug(printf("C_CALL6 - %d - (%d,%d,%d,%d,%d,%d)\n", u32pc, selectStackDef(-5), 
		     selectStackDef(-4), selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc));
	acc = ((c_primitive) u32pc)(selectStackDef(-5), selectStackDef(-4), selectStackDef(-3), selectStackDef(-2),
                       selectStackDef(-1), acc);
	popNDef(5);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL6 end\n"));
	Next;
      }

      Instruct(C_CALL7): { 
	Setup_for_c_call;
	debug(printf("C_CALL7 - %d - (%d,%d,%d,%d,%d,%d,%d)\n", u32pc, selectStackDef(-6), 
		     selectStackDef(-5), selectStackDef(-4), selectStackDef(-3), selectStackDef(-2),
         selectStackDef(-1), acc));
	acc = ((c_primitive) u32pc)(selectStackDef(-6), selectStackDef(-5), selectStackDef(-4),
                              selectStackDef(-3), selectStackDef(-2), selectStackDef(-1), acc);
	popNDef(6);
	inc32pc; /* index in c_prim */
	Restore_after_c_call;
	debug(printf("C_CALL7 end\n"));
	Next;
      }

      Instruct(UB_TAG_CON): {
	// If temp = (11xxxxxxxxxxxxx), then we are dealing with a nullary 
	// constructor and all bits are used.
	debug(printf("UB_TAG_CON: %x\n", acc));
	temp = acc;
	acc = acc & 0x00000003;
	if (acc == 0x00000003)
	  acc = temp;
	Next;
      }

      Instruct(SELECT_STACK_M1): { acc = selectStackDef(-1); Next; }
      Instruct(SELECT_STACK_M2): { acc = selectStackDef(-2); Next; }
      Instruct(SELECT_STACK_M3): { acc = selectStackDef(-3); Next; }
      Instruct(SELECT_STACK_M4): { acc = selectStackDef(-4); Next; }
      Instruct(SELECT_STACK_N): {
	debug(printf("SELECT_STACK_N %d\n", s32pc));
	acc = selectStackDef(s32pc);
	inc32pc;
	Next;
      }

      Instruct(SELECT_0): { acc = *(int *)acc; Next; }
      Instruct(SELECT_1): { acc = *((int *)acc + 1); Next; }
      Instruct(SELECT_2): { acc = *((int *)acc + 2); Next; }
      Instruct(SELECT_3): { acc = *((int *)acc + 3); Next; }
      Instruct(SELECT_N): {
	debug(printf("SELECT_N %d\n", s32pc));	
	acc = *(((int *)acc) + s32pc);
	inc32pc;
	Next;
      }

      Instruct(SELECT_ENV_N): {
	debug(printf("SELECT_ENV_N %d - env = 0x%x\n", s32pc, env));
	acc = *(env + s32pc);
	inc32pc;
	Next;
      }
      Instruct(ENV_TO_ACC): {
	debug(printf("ENV_TO_ACC\n"));
	acc = (int) env;
	Next;
      }

      Instruct(STORE_0): { *(int *)popValDef = acc; acc = mlUNIT; Next; }
      Instruct(STORE_1): { *((int *)popValDef + 1) = acc; acc = mlUNIT; Next; }
      Instruct(STORE_2): { *((int *)popValDef + 2) = acc; acc = mlUNIT; Next; }
      Instruct(STORE_3): { *((int *)popValDef + 3) = acc; acc = mlUNIT; Next; }
      Instruct(STORE_N): {
	debug(printf("STORE_N %d \n", acc));
	temp = (int)(((int *)popValDef) + s32pc); 
	*((int *)temp) = acc;
	debug(printf("Writing to sp = 0x%x\n", temp));
	acc = mlUNIT;
	inc32pc;
	Next;
      }

      Instruct(STACK_ADDR_INF_BIT) : {
	acc = (int) (sp + s32pc);
	acc = setInfiniteBit(acc);              /* bug fix - inserted acc = ... */
	debug(printf("STACK_ADDR_INF_BIT %d at %d (0x%x)\n", s32pc, acc, acc));
	inc32pc;
	Next;
      }
      Instruct(STACK_ADDR): {
	acc = (int) (sp + s32pc);
	debug(printf("STACK_ADDR %d at %x\n", s32pc, acc));
	inc32pc;
	Next;
      }

      Instruct(RETURN_1_1): {
	pc_temp = (bytecode_t) selectStackDef(-3);
	env = (int *) selectStackDef(-2);
	popNDef(3);
	pc = pc_temp;
	Next;
      }
      Instruct(RETURN_N_1): {
	pc_temp = (bytecode_t) selectStackDef(-s32pc-2);
	env = (int *) selectStackDef(-s32pc-1);
	popNDef(s32pc+2);
	pc = pc_temp;
	Next;
      }
      Instruct(RETURN): {
	debug(printf("RETURN(old_args %d,res %d)\n",s32pc,s32_1pc));
	pc_temp = (bytecode_t) selectStackDef(-s32_1pc-s32pc-1);
	debug(printf("Return-pointer stack-slot = 0x%x\n", sp -s32_1pc-s32pc-1));
	env = (int *) selectStackDef(-s32pc-s32_1pc);
	for (temp=0;temp<s32_1pc-1;temp++) {
	  selectStackDef(-s32_1pc-s32pc-1+temp) = selectStackDef(-s32_1pc+1+temp);
	  debug(printf("Writing to sp = 0x%x\n", sp -s32_1pc-s32pc-1+temp));
	}
	popNDef(s32pc+2);
	pc = pc_temp;
	Next;
      }

      Instruct(STORE_DATA): { 
	debug(printf("STORE_DATA(%x) (acc = %d; 0x%x) - slot = 0x%x - ds = 0x%x\n", s32pc, acc, acc, ds + s32pc, ds));
	*(ds + s32pc) = acc;
	inc32pc;
	Next;
      }
      Instruct(FETCH_DATA): { 
	debug(printf("FETCH_DATA(%x)\n", s32pc));
	acc = *(ds + s32pc);
	inc32pc;
	Next;
      }
      Instruct(HALT): { 
	debug(printf("HALT; acc = %d\n", acc));
	// deallocate regions on the stack except top-level regions
	deallocateRegionsUntil((Region)sp0, topRegionCell);
	return acc;     // the accumulator is the result of the entire computation
      }

    raise_exception:

      Instruct(RAISE): {
	Raise(acc);
	Next;
      }

      Instruct(PUSH_EXN_PTR): {
	debug(printf("PUSH_EXN_PTR\n"));
	pushDef((unsigned long)exnPtr);
	exnPtr = sp - 1;                    // there is no stack-macro to do this!
	Next;
      }
      
      Instruct(POP_EXN_PTR): {
	debug(printf("POP_EXN_PTR\n"));
	exnPtr = (uintptr_t *)popValDef;
	Next;
      }

      Instruct(GLOBAL_EXN_HANDLER_REPORT): {
	size_t exn_sz;
	debug(printf("GLOBAL_EXN_HANDLER_REPORT, acc = %d\n", acc));
  tmp2 = (ssize_t *) acc+1;
	acc = *(ssize_t *)acc;        
	temp = *((ssize_t *)acc+1);        /* Fetch pointer to exception string */
	acc = *((ssize_t *)acc);           /* Fetch exn number */	

	/* write uncaught exception string into errorStr */
	exn_sz = 1 + sizeStringDefine((String)temp);
  if (acc == failNumber) exn_sz += 1 + sizeStringDefine((String) *tmp2);
	if ( (*errorStr = (char*)malloc(exn_sz) ) <= 0 ) 
	  {
	    die ("GLOBAL_EXN_HANDLER_REPORT - malloc failed");
	  }
	convertStringToC ((String)temp, *errorStr, exn_sz, 0);
  if (acc == failNumber)
  {
    (*errorStr)[sizeStringDefine((String)temp)] = ' ';
    convertStringToC( (String) *tmp2, (*errorStr) + 1 + sizeStringDefine((String)temp), 
                      1 + sizeStringDefine((String) *tmp2), 0);
  }

	if ( acc == 4 ) {
	  acc = -2;         /* we return -2 for Interrupt */
	} else {
	  acc = -1;         /* otherwise, we return -1 */
	}
	Next;
      } 

      Instruct(PRIM_FRESH_EXNAME): {
	acc = (*exnCnt)++;
	debug(printf("PRIM_FRESH_EXNAME; acc = %x\n", acc));
	Next;
      }

      Instruct(STACK_OFFSET): {   /*mael*/
	debug(printf("STACK_OFFSET %d at %x\n", u32pc, (int)sp));
	offsetSP(u32pc);
	inc32pc;
	Next;
      }
	
      Instruct(POP_PUSH): {   /*mael*/
	popNDef(s32pc);
	pushDef(acc);
	debug(printf("POP_PUSH(%d) - sp = 0x%x\n",s32pc, sp));
	inc32pc;
	Next;
      }

      Instruct(IMMED_INT_PUSH0): { pushDef(0); Next; }  /*mael*/
      Instruct(IMMED_INT_PUSH1): { pushDef(1); Next; }  /*mael*/
      Instruct(IMMED_INT_PUSH2): { pushDef(2); Next; }  /*mael*/
      Instruct(IMMED_INT_PUSH3): { pushDef(3); Next; }  /*mael*/
      Instruct(IMMED_INT_PUSH): {                       /*mael*/
	pushDef(s32pc);
	inc32pc;
	debug(printf("IMMED_INT_PUSH\n"));
	Next;
      }

      Instruct(SELECT_PUSH0): {	pushDef(*(ssize_t *)acc); Next; }          /*mael*/
      Instruct(SELECT_PUSH1): {	pushDef(*((ssize_t *)acc + 1)); Next; }    /*mael*/
      Instruct(SELECT_PUSH2): {	pushDef(*((ssize_t *)acc + 2)); Next; }    /*mael*/
      Instruct(SELECT_PUSH3): {	pushDef(*((ssize_t *)acc + 3)); Next; }    /*mael*/
      Instruct(SELECT_PUSH): {                                         /*mael*/
	debug(printf("SELECT_PUSH(%d)\n", s32pc));	
	pushDef(*(((ssize_t *)acc) + s32pc));
	inc32pc;
	Next;
      }

      Instruct(SELECT_ENV_PUSH): {   /*mael*/
	debug(printf("SELECT_ENV_PUSH %d - env = 0x%x\n", s32pc, env));
	pushDef(*(env + s32pc));
	inc32pc;
	Next;
      }

      Instruct(SELECT_ENV_CLEAR_ATBOT_BIT_PUSH): {   /*mael*/
	debug(printf("SELECT_ENV_CLEAR_ATBOT_BIT_PUSH %d - env = 0x%x\n", s32pc, env));
	pushDef(clearAtbotBit(*(env + s32pc)));
	inc32pc;
	Next;
      }

      Instruct(STACK_ADDR_PUSH): {   /*mael*/
	pushDef((ssize_t)(sp + s32pc));
	debug(printf("STACK_ADDR_PUSH %d\n", s32pc));
	inc32pc;
	Next;
      }

      Instruct(STACK_ADDR_INF_BIT_ATBOT_BIT_PUSH): {   /*mael*/
	pushDef(setStatusBits((ssize_t)(sp + s32pc)));
	debug(printf("STACK_ADDR_INF_BIT_ATBOT_BIT_PUSH(%d)\n", s32pc));
	inc32pc;
	Next;
      }

      Instruct(SELECT_STACK_PUSH): {   /*mael*/
	debug(printf("SELECT_STACK_PUSH(%d)\n", s32pc));
	pushDef(selectStackDef(s32pc));
	inc32pc;
	Next;
      }

      Instruct(ENV_PUSH): {   /*mael*/
	debug(printf("ENV_PUSH\n"));
	pushDef((ssize_t)env);
	Next;
      }

      Instruct(PRIM_SUB_I31): {
	debug(printf("PRIM_SUB_I31\n"));
	temp = i31_to_i32ub((ssize_t)(popValDef)) - i31_to_i32ub(acc);
	acc = i32ub_to_i31(temp);
	if ( i31_to_i32ub(acc) != temp )
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_ADD_I31): {
	debug(printf("PRIM_ADD_I31\n"));
	temp = i31_to_i32ub((int)(popValDef)) + i31_to_i32ub(acc);
	acc = i32ub_to_i31(temp);
	if ( i31_to_i32ub(acc) != temp )
	  goto raise_overflow;
	Next;
      }

#define ChunkLen (4 * sizeof(int) - 1)
#define MaxChunk ((1L << ChunkLen) - 1)

      Instruct(PRIM_MUL_I31): {    /* from MosML */
	register int x, y;
        register int isNegative = 0;
	debug(printf("PRIM_MUL_I31\n"));
        x = i31_to_i32ub((int)(popValDef));
        y = i31_to_i32ub(acc);
        if( x < 0 ) { x = -x; isNegative = 1; }
        if( y < 0 ) { y = -y; isNegative = !isNegative; }
        if( y > x ) { temp = y; y = x; x = temp; }
        if( y > MaxChunk ) 
	  goto raise_overflow;
        if( x <= MaxChunk ) { 
	  acc = i32ub_to_i31(isNegative?(-(x * y)):(x * y)); 
	} else { /* x > MaxChunk */
	  temp = (x >> ChunkLen) * y;
	  if( temp > MaxChunk + 1) 
	    goto raise_overflow;
	  temp = (temp << ChunkLen) + (x & MaxChunk) * y;
	  if( isNegative ) temp = - temp;
	  acc = i32ub_to_i31(temp);
	  if( i31_to_i32ub(acc) != temp ) 
	    goto raise_overflow;
	}
	Next;
      }

      Instruct(PRIM_NEG_I31): {
	debug(printf("PRIM_NEG_I31\n"));
	temp = - i31_to_i32ub(acc);
	acc = i32ub_to_i31(temp);
	if( i31_to_i32ub(acc) != temp ) 
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_ABS_I31): {
	debug(printf("PRIM_ABS_I31\n"));
	if ( acc < 0 ) {
	  temp = - i31_to_i32ub(acc);
	  acc = i32ub_to_i31(temp);
	  if( i31_to_i32ub(acc) != temp ) 
	    goto raise_overflow;
	}
	Next;
      }

      Instruct(PRIM_XOR_W31): {
	debug(printf("PRIM_XOR_W31\n"));
	acc = 1 + (acc ^ ((int)(popValDef)));
	Next;
      }

      Instruct(PRIM_SHIFT_LEFT_W31): {      /* shift amount is untagged */
	debug(printf("PRIM_SHIFT_LEFT_W31\n"));
	acc = 1 + ( (((int)(popValDef)) - 1) << acc );
	Next;
      }
      
      Instruct(PRIM_SHIFT_RIGHT_SIGNED_W31): {     /* shift amount is untagged */
	debug(printf("PRIM_SHIFT_RIGHT_SIGNED_W31\n"));
	acc = 1 | ( (((int)(popValDef)) - 1) >> acc );
	Next;
      }

      Instruct(PRIM_SHIFT_RIGHT_UNSIGNED_W31): {   /* shift amount is untagged */
	debug(printf("PRIM_SHIFT_RIGHT_UNSIGNED_W31\n"));
	acc = 1 | ( ((unsigned int)(popValDef) - 1) >> acc );
	Next;
      }

      /* Unsigned integer arithmetic modulo 2^(wordsize-1) */

      Instruct(PRIM_ADD_W31): {
	debug(printf("PRIM_ADD_W31\n"));
	acc = (int)((unsigned int)(popValDef) + (unsigned int)(acc - 1));
	Next;
      }

      Instruct(PRIM_SUB_W31): {
	debug(printf("PRIM_SUB_W31\n"));
	acc = (int)((unsigned int)(popValDef) - (unsigned int)(acc - 1));
	Next;
      }
      
      Instruct(PRIM_MUL_W31): {
	debug(printf("PRIM_MUL_W31\n"));
	acc = (int)(1 + (unsigned int)((popValDef) >> 1) * (unsigned int)(acc - 1));
	Next;
      }

      Instruct(PRIM_I31_TO_I): {
	debug(printf("PRIM_I31_TO_I\n"));
	acc = i31_to_i32ub(acc);
	Next;
      }

      Instruct(PRIM_I_TO_I31): {
	debug(printf("PRIM_I_TO_I31\n"));
	temp = acc;
	acc = i32ub_to_i31(acc);
	if ( i31_to_i32ub(acc) != temp )
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_W31_TO_W): {
	debug(printf("PRIM_W31_TO_W\n"));
	acc = i31_to_i32ub(acc);
	Next;
      }

      Instruct(PRIM_W_TO_W31): {
	debug(printf("PRIM_W_TO_W31\n"));
	acc = i32ub_to_i31(acc);
	Next;
      }

      Instruct(PRIM_W31_TO_W_X): {
	debug(printf("PRIM_W31_TO_W_X\n"));
	acc = i31_to_i32ub(acc);
	Next;
      }

      Instruct(PRIM_W_TO_I): {
	debug(printf("PRIM_W_TO_I\n"));
	if ( acc < 0 ) 
	  goto raise_overflow;
	Next;
      }

      Instruct(PRIM_BYTETABLE_SUB): {
	debug(printf("PRIM_BYTETABLE_SUB(%d,%d)\n", selectStackDef(-1), acc));
	acc = (int)(*(&(((String)(popValDef))->data) + acc));
	Next;
      }

      Instruct(PRIM_BYTETABLE_UPDATE): {
	debug(printf("PRIM_BYTETABLE_UPDATE(%d,%d,%d)\n", selectStackDef(-2), selectStackDef(-1), acc));
	*(&(((String)(selectStackDef(-2)))->data) + (selectStackDef(-1))) = (unsigned char)acc;
	popNDef(2);
	Next;
      }

      Instruct(PRIM_WORDTABLE_SUB): {
	debug(printf("PRIM_WORDTABLE_SUB(%d,%d)\n", selectStackDef(-1), acc));
	acc = *(&(((Table)(popValDef))->data) + acc);
	Next;
      }

      Instruct(PRIM_WORDTABLE_UPDATE): {
	debug(printf("PRIM_WORDTABLE_UPDATE(%d,%d,%d)\n", selectStackDef(-2), selectStackDef(-1), acc));
	*(&(((Table)(selectStackDef(-2)))->data) + (selectStackDef(-1))) = acc;
	popNDef(2);
	Next;
      }

      Instruct(PRIM_TABLE_SIZE): {
	debug(printf("PRIM_TABLE_SIZE\n"));
	acc = get_table_size(((String)acc)->size);  // get_table_size == get_string_size
	Next;
      }

      Instruct(PRIM_IS_NULL): {
	debug(printf("PRIM_IS_NULL\n"));
	if ( acc == 0 ) acc = mlTRUE;
	else acc = mlFALSE;
	Next;
      }

	  // Passing state around; used in apache to pass request_rec with the connection
      Instruct(GET_CONTEXT): {
	 debug(printf("GET_CONTEXT\n"));
	 acc = (int) serverCtx->aux;
	 Next;
	  }
      
      Instruct(CHECK_LINKAGE): {
		if (u32pc == 0)
		{
			acc = popValDef;
			inc32pc;  /* Index in dynamic_funcs */
			Next;
		}
		else 
		{
			Setup_for_c_call;
			if (u32pc == 1) 
			{
				localResolveLibFnAuto(((void **) pc)+2, (char *) (&(((String) acc)->data)));
			}
			else if (u32pc == 2)
			{
				localResolveLibFnAuto(((void **) pc)+2, (char *) acc);
			}
      if (u32_2pc == 0) 
      {
        raise_exn((int) &exn_MATCH);
      }
			u32pc = 0;
			acc = popValDef;
			inc32pc;  /* Index in dynamic_funcs */
			Restore_after_c_call;
			Next;
		}
      }

#ifdef LAB_THREADED
//  lbl_EVENT:
    lbl_DOT_LABEL:
    lbl_LABEL:
#else
    default: {
#endif /*LAB_THREADED*/ 
	printf("Default: Instruction %d(hex %x) not recognized\n", cur_instr, cur_instr);
	printf("Stack pointer sp = %p\n", sp);
	printf("Code pointer pc = %p\n", pc);
	die("Instruction not recognized");
	return -1;
#ifndef LAB_THREADED
      }
    }
  }
#endif
}


/* Interpret code; assumes that code is already resolved; i.e., that
 * instruction numbers are turned into instruction addresses. */
ssize_t 
interpCode(Interp* interpreter,         // The interpreter
       register uintptr_t * sp, // Stack pointer
       uintptr_t * ds,          // Data segment pointer
       uintptr_t * exnPtr,      // Pointer to next exn-handler on stack
       Ro** topRegionCell,          // Cell for holding a pointer to the top-most region
       char ** errorStr,            // Cell to store error-string in case of an uncaught exception
       size_t *exnCnt,       // Exception name counter
       bytecode_t b_prog,           // The actual code
       void *serverCtx)  {          // Apache request_rec pointer
       ssize_t res = interp(interpreter, sp, ds, exnPtr, topRegionCell, errorStr,
		   exnCnt, b_prog, 0, INTERPRET, serverCtx);    
                                            // sizeW not used when mode is INTERPRET
  return res;
}


/* Resolve code; i.e., turn instruction numbers into instruction
 * addresses. */
void
resolveCode(bytecode_t b_prog,              // Code to resolve
	    int sizeW) {                    // Size of code in words
  interp(NULL, NULL, NULL, NULL, NULL, NULL, 0, b_prog, sizeW, RESOLVEINSTS, NULL);  
}

void print_code(bytecode_t b_prog, int code_size) {
  int j;
  for (j=0;j<code_size/4;j++)
    printf("start_code[%d]=%x\n",j,*(((int *)b_prog) + j));
  return;
}

extern int  commandline_argc;
extern char **commandline_argv;

#ifndef APACHE
void report(enum reportLevel level, char *data, void *notused)
{
  switch (level)
  {
    case DIE:
      fprintf(stderr, "Runtime Error: %s\n", data); 
      exit(-1); 
      break;
    case CONTINUE:
      fprintf(stderr, "%s\n", data);
      break;
  }
  return;
}

ssize_t
main_interp(int argc, char * argv[]) {
  ssize_t res, start, c;
  Interp* interp;
  char* errorStr = NULL;
  Serverstate ss;

  if (argc < 2)
  { // No argument... Nothing to do.
    return EXIT_SUCCESS;
  }
  
  ss.report = &report;
  ss.aux = NULL;

  debug(printf("[Resolving global code fragments]\n"));
  resolveGlobalCodeFragments();
  resolveGlobalCodeFragments();

  debug(printf("[Creating new interpreter]\n"));
  interp = interpNew();
  
  debug(printf("[Number of command-line arguments (including name of executable): %d]\n", argc));

  // the arguments after ``--args'' are the command-line arguments 
  // passed by the user to the executable ./run

  for (c = 1; c < argc && strcmp(argv[c], "--args") != 0; c ++) {
    debug(printf("[Loading bytecode file %s]\n", argv[c]));
    interpLoadExtend(interp, argv[c], &ss);
  }

  if ( strcmp(argv[c], "--args") != 0 ) {
    (*(ss.report)) (CONTINUE, "expecting ``--args'' option to command", ss.aux);
    return -1;
  }
  
  start = c + 1;
  
  for (c = 0 ; start + c < argc; c++) {
    argv[c] = argv[start + c];
  }

  commandline_argc = c;
  commandline_argv = argv;

  debug(printf("[Running interpreter]\n"));
  res = interpRun(interp, NULL, &errorStr, &ss);
  debug(printf ("[Result of running interpreter is %d]\n", res));

  if ( res < 0 ) {     // uncaught exception
    fprintf(stderr,"uncaught exception %s\n", errorStr); 
    fflush(stderr);
    free(errorStr);    // free the malloced string
    errorStr = NULL;   // - and nullify field    
  }
  return res;
}
#endif // APACHE

