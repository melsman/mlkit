/* c-calls-fns.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "cache-flush.h"

#include "c-calls.h"

/* layout of a code_header must match offsets in c-entry.asm */
typedef struct code_header {
    ml_val_t	the_fn;
    char	*argtypes[N_ARGS];
    char	*rettype;
    int		nargs;
} code_header_t;

extern Addr_t grabPC();
extern Addr_t grabPCend();

Word_t		*last_entry;  /* points to the beginning of the last c-entry */
			      /* executed set by grabPC in c-entry.asm */
PVT code_header_t *last_code_hdr = NULL;  /* last code hdr used */

#define CODE_HDR_START(p) \
    ((code_header_t *)((Byte_t *)(p)-sizeof(code_header_t)))


/* code for maintaining ML objects (currently only functions) potentially
 * only reachable from C.  Currently, we use a list.  Objects on this
 * list persist until the program completes; this is a known space leak...
 */

ml_val_t CInterfaceRootList = LIST_nil;/* see gc/call-gc.c and gc/major-gc.c */

PVT void recordFnAsRoot(ml_state_t *msp,ml_val_t *rp)
{
    LIST_cons(msp,CInterfaceRootList,(ml_val_t) rp,CInterfaceRootList);
#ifdef DEBUG_C_CALLS
    printf("recordFnAsRoot: added %x\n", rp);
#endif
}

PVT ml_val_t saveState(ml_state_t *msp,ml_val_t cont)
{
    int n, i, j;
    Word_t mask;

    /* compute space for save record */
    n = 0;
    /* link, closure, arg, cont, and misc regs are in mask ... */
    mask = msp->ml_liveRegMask; /* should also be mask from REQ_CALLC */
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    n++;
    }
    /* ... but pc, exnCont, varreg, and basereg (if defined) aren't */
    n += 3;
#ifdef BASE_INDX
    n++;
#endif
    /* also need to save the liveRegMask. we'll do this first */
    /* others?? */
    n++;
#if defined(SOFT_POLL)
#error
#endif
#ifdef DEBUG_C_CALLS
    printf("saveState: size %d\n", n);
#endif
    if (cont == (ml_val_t) NULL) {
      ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_record));
      j = 1;
    } else {
      n++;
      ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_record));
      ML_AllocWrite (msp, 1, cont);
      j = 2;
    }
    ML_AllocWrite (msp, j++, INT_CtoML(msp->ml_liveRegMask));
    ML_AllocWrite (msp, j++, msp->ml_pc);
    ML_AllocWrite (msp, j++, msp->ml_exnCont);
    ML_AllocWrite (msp, j++, msp->ml_varReg);
#ifdef BASE_INDX
    ML_AllocWrite (msp, j++, msp->ml_baseReg);
#endif
    mask = msp->ml_liveRegMask;
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    ML_AllocWrite (msp, j++, msp->ml_roots[ArgRegMap[i]]);
    }
    ASSERT(j-1 == n);
    return ML_Alloc(msp, n);
}

PVT void restoreState(ml_state_t *msp,ml_val_t state,int holds_cont)
{
    int n, i, j;
    Word_t mask;

    n = OBJ_LEN(state);
#ifdef DEBUG_C_CALLS
    printf("restoreState: state size %d\n", n);
#endif
    /* link, closure, arg, cont, and misc regs are in mask ... */
    /* ... but pc, exnCont, varreg, and basereg (if defined) aren't */
    /* and also need the liveRegMask. get this first */
    /* others?? */
    if (!holds_cont) {
      j = 0;
    } else {
      /* skip function ptr */
      j = 1;
    }
    msp->ml_liveRegMask = REC_SELINT(state,j++);
    msp->ml_pc = REC_SEL(state,j++);
    msp->ml_exnCont = REC_SEL(state,j++);
    msp->ml_varReg = REC_SEL(state,j++);
#ifdef BASE_INDX
    msp->ml_baseReg = REC_SEL(state,j++);
#endif
    mask = msp->ml_liveRegMask;
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    msp->ml_roots[ArgRegMap[i]] = REC_SEL(state,j++);
    }
    ASSERT(j == n);
}

PVT void setup_msp(ml_state_t *msp,ml_val_t f, ml_val_t arg)
{
#if (CALLEESAVE == 0)
    extern ml_val_t return_a[];
#endif

    /* save necessary state from current msp in calleesave register */
#if (CALLEESAVE > 0)
    msp->ml_calleeSave(1) = saveState(msp,NULL);
    msp->ml_cont = PTR_CtoML(return_c);
#else
    msp->ml_cont = saveState(msp,PTR_CtoML(return_a));
#endif

    /* inherit exnCont (?) */
    /* leave msp->ml_exnCon as is */
    msp->ml_varReg      = ML_unit;
    msp->ml_arg		= arg;
    msp->ml_closure	= f;
    msp->ml_pc		=
    msp->ml_linkReg	= GET_CODE_ADDR(f);
}

PVT void restore_msp(ml_state_t *msp)
{
    /* restore previous msp */
#if (CALLEESAVE > 0)
    restoreState(visible_msp,visible_msp->ml_calleeSave(1),FALSE);
#else
    restoreState(visible_msp,visible_msp->ml_cont,TRUE);
#endif
}

/* convert result to C */
PVT Word_t convert_result(ml_state_t *msp,code_header_t *chp,ml_val_t val)
{
    Word_t p, *q = &p;
    char *t = chp->rettype;
    int err;

    /* front-end of interface guarantees that ret is a valid 
     * return value for a C function: Word_t or some pointer
     */
    err = datumMLtoC(msp,&t,&q,val);
    if (err)
	/* need better error reporting here ... */
	Die("convert_result: error converting return value to C");
    /* return C result*/
    return p;
}


/* entry points; must be visible to c-entry.asm
 */

int no_args_entry()
{
    ml_val_t ret;

#ifdef DEBUG_C_CALLS
    printf("no_args_entry: entered\n");
#endif
    last_code_hdr = CODE_HDR_START(last_entry);
#ifdef DEBUG_C_CALLS
    printf("no_args_entry: nargs in header is %d\n", last_code_hdr->nargs);
#endif

    /* setup msp for RunML evaluation of (f LIST_nil) */
    setup_msp(visible_msp, last_code_hdr->the_fn, LIST_nil);

    /* call ML fn, returns an ml_val_t (which is cdata) */
#ifdef DEBUG_C_CALLS
    printf("no_arg_entry: calling ML from C\n");
#endif
    RunML (visible_msp);

    
#ifdef DEBUG_C_CALLS
    printf("no_args_entry: return value is %d\n", visible_msp->ml_arg);
#endif

    ret = visible_msp->ml_arg;

    restore_msp(visible_msp);

    return convert_result(visible_msp,last_code_hdr,ret);
}

int some_args_entry(Word_t first,...)
{
    va_list ap;
    ml_val_t lp = LIST_nil, ret;
    Word_t next;
    int i;
    ml_val_t args[N_ARGS];

#ifdef DEBUG_C_CALLS
    printf("some_args_entry: entered\n");
#endif
    last_code_hdr = CODE_HDR_START(last_entry);
#ifdef DEBUG_C_CALLS
    printf("some_args_entry: nargs in header is %d\n", last_code_hdr->nargs);
    printf("arg 0: %x\n",first);
#endif
    ret = datumCtoML(visible_msp,last_code_hdr->argtypes[0],first,&lp);
    LIST_cons(visible_msp,lp,ret,lp);
    va_start(ap,first);
    for (i = 1; i < last_code_hdr->nargs; i++) {
	next = va_arg(ap,Word_t);
#ifdef DEBUG_C_CALLS
	printf("arg %d: %x\n",i,next);
#endif
	ret = datumCtoML(visible_msp,last_code_hdr->argtypes[i],next,&lp);
	LIST_cons(visible_msp,lp,ret,lp);
    }
    va_end(ap);

    /* lp is backwards */
    lp = revMLList(lp,LIST_nil);

    /* setup msp for RunML evaluation of (f lp) */
    setup_msp(visible_msp, last_code_hdr->the_fn, lp);

    /* call ML fn, returns an ml_val_t (which is cdata) */
#ifdef DEBUG_C_CALLS
    printf("some_arg_entry: calling ML from C\n");
#endif
    RunML (visible_msp);

    
#ifdef DEBUG_C_CALLS
    printf("some_args_entry: return value is %d\n", visible_msp->ml_arg);
#endif

    ret = visible_msp->ml_arg;

    restore_msp(visible_msp);

    return convert_result(visible_msp,last_code_hdr,ret);
}

PVT void *build_entry(ml_state_t *msp,code_header_t h)
{
    int szb = ((Byte_t *)grabPCend) - ((Byte_t *)grabPC);
    Byte_t *p;


#ifdef DEBUG_C_CALLS
    printf ("grabPC=%lx, grabPCend=%lx, code size is %d\n", 
 	    grabPC, grabPCend, szb);
    printf ("code_header size is %d\n", sizeof(code_header_t));
#endif
    ASSERT((sizeof(code_header_t) & 0x3) == 0);
    p = (Byte_t *) memalign(sizeof(Word_t),szb+sizeof(code_header_t));
    *(code_header_t *)p = h;
    recordFnAsRoot(msp,&(((code_header_t *)p)->the_fn));
    /* NB: to free this thing, we'll have to subtract sizeof(code_header_t) */
    p += sizeof(code_header_t);
#ifdef DEBUG_C_CALLS
    printf ("new code starts at %x and ends at %x\n", p, p+szb);
#endif
    memcpy (p, (void *)grabPC, szb);
    FlushICache(p,szb);
    return p;   
}

Word_t mk_C_function(ml_state_t *msp,
		     ml_val_t f,int nargs,char *argtypes[],char *rettype)
{
    code_header_t ch;
    int i;

    /* create a code header; this will be copied by build entry */
    ch.the_fn = f;
    ch.nargs = nargs;
    for (i = 0; i < nargs; i++)
	ch.argtypes[i] = argtypes[i];  /* argtypes[i] is a copy we can have */
    ch.rettype = rettype;              /* rettype is a copy we can have */

    /* build and return a C entry for f */
    return (Word_t) build_entry(msp,ch);
}

/* end of c-calls-fns.c */
