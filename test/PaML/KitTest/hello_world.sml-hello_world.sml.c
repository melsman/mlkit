#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>


/* Begin generated C code. */

#include <stdio.h>
#include <errno.h>
// #include <math.h>


#ifndef Runtime_C_h 
#define Runtime_C_h 

/*----------------------------------------------------------------*
 *                   Declaration of constants                     *
 *----------------------------------------------------------------*/
#define NULL 0
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

extern Int32 DatLab81;
//extern int exn_DIV;
//extern int exn_INTERRUPT;
//extern int exn_OVERFLOW;
//extern int exn_BIND;
//extern int exn_MATCH;
//extern int exnPtr;
//extern int exnameCounter;
extern Int32 exn_flag;
static double FloatReg5;
static double FloatReg4;
static double FloatReg3;
static double FloatReg2;
static double FloatReg1;
static double FloatReg0;
extern Int32 IntReg41;
extern Int32 IntReg40;
extern Int32 IntReg39;
extern Int32 IntReg38;
extern Int32 IntReg37;
extern Int32 IntReg36;
extern Int32 IntReg35;
extern Int32 IntReg34;
extern Int32 IntReg33;
extern Int32 IntReg32;
extern Int32 IntReg31;
extern Int32 IntReg30;
extern Int32 IntReg29;
extern Int32 IntReg28;
extern Int32 IntReg27;
extern Int32 IntReg26;
extern Int32 IntReg25;
extern Int32 IntReg24;
extern Int32 IntReg23;
extern Int32 IntReg22;
extern Int32 IntReg21;
extern Int32 IntReg20;
extern Int32 IntReg19;
extern Int32 IntReg18;
extern Int32 IntReg17;
extern Int32 IntReg16;
extern Int32 IntReg15;
extern Int32 IntReg14;
extern Int32 IntReg13;
extern Int32 IntReg12;
extern Int32 IntReg11;
extern Int32 IntReg10;
extern Int32 IntReg9;
extern Int32 IntReg8;
extern Int32 IntReg7;
extern Int32 IntReg6;
extern Int32 IntReg5;
extern Int32 IntReg4;
extern Int32 IntReg3;
extern Int32 IntReg2;
extern Int32 IntReg1;
extern Int32 IntReg0;
Int32 Lab78667(void);Int32 Lab78670(void);static Int32 Lab78668(void);
 
static struct litString16 String368= { 89, 11, NULL, "SML on PaLM" }; 
Int32 DatLab78671;
Int32 Lab78667(){return ((Int32)Lab78668);}
Int32 Lab78670(){
    printString((IntReg3));
    if (exn_flag==1) {return ((Int32)1);} else {};
    IntReg7 = ((Int32)1);
    popDef((IntReg8));
    return (IntReg8);
}
static Int32 Lab78668(){
    IntReg8 = (DatLab81);
    IntReg8 = alloc((IntReg8),((Int32)1));
    IntReg9 = (DatLab81);
    *(Int32 *)((IntReg8) + 4*((Int32)0)) = (IntReg9);
    IntReg9 = ((Int32)(&String368));
    printString((IntReg9));
    if (exn_flag==1) {return ((Int32)1);} else {};
    DatLab78671 = (IntReg8);
    popDef((IntReg8));
    return (IntReg8);
}

/* End generated C code */

