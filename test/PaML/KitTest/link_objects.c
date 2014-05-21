#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>

/* Begin generated C code. */

#include <stdio.h>
#include <errno.h>
//#include <math.h>


#ifndef Runtime_C_h 
#define Runtime_C_h 

/*----------------------------------------------------------------*
 *                   Declaration of constants                     *
 *----------------------------------------------------------------*/

struct litFloat { Int32 t; Int32 dummy; double n; };

struct litString1 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[1];};
struct litString2 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[2];};
struct litString4 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[4];};
struct litString8 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[8];};
struct litString16 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[16];};
struct litString32 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[32];};
struct litString64 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[64];};
struct litString128 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[128];};
struct litString256 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[256];};
struct litString512 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[512];};
struct litString1024 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[1024];};
struct litString2048 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[2048];};
struct litString4096 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[4096];};
struct litString8192 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[8192];};
struct litString16384 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[16384];};
struct litString32768 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[32768];};
struct litString65536 {Int32 totalsize;Int32 fragmentsize;Int32 *next;char s[65536];};

struct litNullaryExn {Int32 inDirectionPtr;Int32 exnNo;Int32 stringPtr;};
struct litUnaryExn {Int32 ptrExnName;Int32 exnValue;Int32 exnNo;Int32 stringPtr;};

#endif /* Runtime_C_h */


#ifndef Stack_h 
#define Stack_h 
/* ----------------------------------------------------------------------- *
 *   Stack operations                                                      *
 * The stack grows toward higher addresses, and consists of words of data. *
 *     int pop()          : Return the top element, and ajust sp.          *
 *     void push(val)     : Push val onto the stack, and ajust sp.         *
 *     int offsetSP(val)  : Offsets sp val words, and returns first free   *
 *                          address on the stack.                          *
 * ----------------------------------------------------------------------- */

#define STACK_SIZE_INIT  (10 * 1024)
#define STACK_SIZE_THRESHOLD (4 * 200) 

#define sp IntReg30

#define init_stackDef() { if ((sp = (Int32) MemPtrNew(STACK_SIZE_INIT)) == 0) \
                            panic("init: Cannot increase vm address space\n"); \
                          stackBot = sp; }

#define popDef(Res) { sp -= sizeof(Int32); \
                       (Res) = *((Int32 *)sp); }

#define pushDef(Arg) { *((Int32 *)sp) = (Arg); \
  	               sp += sizeof(Int32); }

/* This version does not return the first free address. */
#define offsetSPDef(Arg) { sp += sizeof(Int32)*(Arg); }

#endif /* Stack_h */


/* ----------------------------------------------------------------------- *
 *  Generated code starts here.                                            *
 * ----------------------------------------------------------------------- */

extern Int32 Lab78667(void);
extern Int32 Lab160(void);
extern Int32 freelist;
extern Int32 topRegion;
static Int32 TopLevelHandlerLab(void);
static Int32 do_raise_exn(Int32 exn_val);
Int32 code(void);
Int32 raise_exn(Int32 exn_val_arg);
static Int32 c_code(void);
static Int32 Lab78684(void);
Int32 exnPtr,exnameCounter; 

static double FloatReg0,FloatReg1,FloatReg2,FloatReg3,FloatReg4,FloatReg5; 

Int32 IntReg0,IntReg1,IntReg2,IntReg3,IntReg4,IntReg5,IntReg6,IntReg7,IntReg8,IntReg9,IntReg10,IntReg11,IntReg12,IntReg13,IntReg14,IntReg15,IntReg16,IntReg17,IntReg18,IntReg19,IntReg20,IntReg21,IntReg22,IntReg23,IntReg24,IntReg25,IntReg26,IntReg27,IntReg28,IntReg29,IntReg30,IntReg31,IntReg32,IntReg33,IntReg34,IntReg35,IntReg36,IntReg37,IntReg38,IntReg39,IntReg40,IntReg41; 
Int32 DatLab83; 
Int32 DatLab82; 
Int32 DatLab81; 
Int32 timeToProfile,maxStack,stackBot,exn_flag,exn_val; 
static struct litString4 String373= { 25, 3, NULL, "Div" }; 
struct litNullaryExn exn_DIV= {0,0,0}; 
static struct litString16 String372= { 73, 9, NULL, "Interrupt" }; 
struct litNullaryExn exn_INTERRUPT= {0,0,0}; 
static struct litString8 String371= { 65, 8, NULL, "Overflow" }; 
struct litNullaryExn exn_OVERFLOW= {0,0,0}; 
static struct litString4 String370= { 33, 4, NULL, "Bind" }; 
struct litNullaryExn exn_BIND= {0,0,0}; 
static struct litString8 String369= { 41, 5, NULL, "Match" }; 
struct litNullaryExn exn_MATCH= {0,0,0};
static Int32 TopLevelHandlerLab(){
    IntReg3 = *(int *)((IntReg3) + 4*((int)0));
    IntReg3 = *(Int32 *)((IntReg3) + 4*((Int32)1));
    //    uncaught_exception((IntReg3));
}
static Int32 do_raise_exn(Int32 exn_val){
    IntReg3 = (exn_val);
    IntReg5 = *(Int32 *)(((Int32)(&exnPtr)) + 4*((Int32)0));
    dealloc_regions_until((IntReg5));
    IntReg30 = (IntReg5);
    popDef((IntReg5));
    *(Int32 *)(((Int32)(&exnPtr)) + 4*((Int32)0)) = (IntReg5);
    popDef((IntReg6));
    exn_val = *(Int32 *)((IntReg6) + 4*((Int32)0));
    return (exn_val);
}
Int32 code(){

    
  init_stackDef(); 
  /* dispatch loop; start calling c_code() */ 
   { Int32 p = (Int32) c_code; 
    while (p) {
      if (exn_flag) {
        exn_flag = 0;
        p = do_raise_exn(exn_val);
      }
      else
        p =  ((Int32 (*) (void))p)();
    } 
  }
  return 0;


}
Int32 raise_exn(Int32 exn_val_arg){
    exn_val = (exn_val_arg);
    exn_flag = ((Int32)1);
    return ((Int32)1);
}
static Int32 c_code(){
    /***** Init Link Code *****/
    topRegion = ((Int32)0);
    freelist = ((Int32)0);
    maxStack = ((Int32)0);
    exn_flag = ((Int32)0);
    exn_val = ((Int32)0);
    /*     Reset global exn flag.*/
    exn_flag = ((Int32)0);
    /*     Exn name*/
    exnameCounter = ((Int32)5);
    /*     Exn ptr.*/
    *(Int32 *)(((Int32)(&exnPtr)) + 4*((Int32)0)) = ((Int32)0);
    /*Setup primitive exception: Div*/
    IntReg25 = ((Int32)(&exn_DIV))+((Int32)4);
    *(Int32 *)(((Int32)(&exn_DIV)) + 4*((Int32)0)) = (IntReg25);
    *(Int32 *)(((Int32)(&exn_DIV)) + 4*((Int32)1)) = ((Int32)4);
    IntReg25 = ((Int32)(&String373));
    *(Int32 *)(((Int32)(&exn_DIV)) + 4*((Int32)2)) = (IntReg25);
    /*Setup primitive exception: Interrupt*/
    IntReg25 = ((Int32)(&exn_INTERRUPT))+((Int32)4);
    *(Int32 *)(((Int32)(&exn_INTERRUPT)) + 4*((Int32)0)) = (IntReg25);
    *(Int32 *)(((Int32)(&exn_INTERRUPT)) + 4*((Int32)1)) = ((Int32)3);
    IntReg25 = ((Int32)(&String372));
    *(Int32 *)(((Int32)(&exn_INTERRUPT)) + 4*((Int32)2)) = (IntReg25);
    /*Setup primitive exception: Overflow*/
    IntReg25 = ((Int32)(&exn_OVERFLOW))+((Int32)4);
    *(Int32 *)(((Int32)(&exn_OVERFLOW)) + 4*((Int32)0)) = (IntReg25);
    *(Int32 *)(((Int32)(&exn_OVERFLOW)) + 4*((Int32)1)) = ((Int32)2);
    IntReg25 = ((Int32)(&String371));
    *(Int32 *)(((Int32)(&exn_OVERFLOW)) + 4*((Int32)2)) = (IntReg25);
    /*Setup primitive exception: Bind*/
    IntReg25 = ((Int32)(&exn_BIND))+((Int32)4);
    *(Int32 *)(((Int32)(&exn_BIND)) + 4*((Int32)0)) = (IntReg25);
    *(Int32 *)(((Int32)(&exn_BIND)) + 4*((Int32)1)) = ((Int32)1);
    IntReg25 = ((Int32)(&String370));
    *(Int32 *)(((Int32)(&exn_BIND)) + 4*((Int32)2)) = (IntReg25);
    /*Setup primitive exception: Match*/
    IntReg25 = ((Int32)(&exn_MATCH))+((Int32)4);
    *(Int32 *)(((Int32)(&exn_MATCH)) + 4*((Int32)0)) = (IntReg25);
    *(Int32 *)(((Int32)(&exn_MATCH)) + 4*((Int32)1)) = ((Int32)0);
    IntReg25 = ((Int32)(&String369));
    *(Int32 *)(((Int32)(&exn_MATCH)) + 4*((Int32)2)) = (IntReg25);
    /*Allocate global regions and push them on teh stack.*/
    IntReg26 = (IntReg30);
    offsetSPDef(((Int32)4));
    DatLab83 = alloc_region((IntReg26));
    IntReg26 = (IntReg30);
    offsetSPDef(((Int32)4));
    DatLab82 = alloc_region((IntReg26));
    IntReg26 = (IntReg30);
    offsetSPDef(((Int32)4));
    DatLab81 = alloc_region((IntReg26));
    /*    Setup top level handler code*/
    IntReg25 = (IntReg30);
    IntReg26 = ((Int32)TopLevelHandlerLab);
    pushDef((IntReg26));
    pushDef((IntReg25));
    IntReg5 = *(Int32 *)(((Int32)(&exnPtr)) + 4*((Int32)0));
    pushDef((IntReg5));
    *(Int32 *)(((Int32)(&exnPtr)) + 4*((Int32)0)) = (IntReg30);
    /*Push addresses of program units on stack, starting with*/
    /*the exit label and ending with label for the second program unit.*/
    IntReg26 = ((Int32)Lab78684);
    pushDef((IntReg26));
    IntReg26 = ((Int32)Lab78667);
    pushDef((IntReg26));
    /*Jump to first block*/
    return ((Int32)Lab160);
}
static Int32 Lab78684(){/***** Link Exit code *****/terminate(((Int32)0));}

/* End generated C code */

