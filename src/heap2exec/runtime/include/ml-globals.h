/* ml-globals.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * These are global reference variables allocated in the run-time system that
 * are visible to the ML tasks.
 */

#ifndef _ML_GLOBALS_
#define ML_GLOBALS_

#ifndef _ML_VALUES_
#include "ml-values.h"
#endif

extern ml_val_t	*CRoots[];
extern int	NumCRoots;

/* "current function" hook for profiling */
extern ml_val_t	_ProfCurrent[];
#define ProfCurrent PTR_CtoML(_ProfCurrent+1)

extern ml_val_t	_PervStruct[];		/* Pointer to the pervasive structure */
#define PervStruct PTR_CtoML(_PervStruct+1)

extern ml_val_t _MLSignalHandler[];
#define MLSignalHandler PTR_CtoML(_MLSignalHandler+1)

extern ml_val_t SysErr_id0[];
#define SysErrId PTR_CtoML(SysErr_id0+1)

extern ml_val_t RunTimeCompUnit;
#ifdef ASM_MATH
extern ml_val_t MathVec;
#endif

extern ml_val_t _Div_id0[];
extern ml_val_t _Div_e0[];
#define DivId		PTR_CtoML(_Div_id0+1)
#define DivExn		PTR_CtoML(_Div_e0+1)

extern ml_val_t _Overflow_id0[];
extern ml_val_t _Overflow_e0[];
#define OverflowId	PTR_CtoML(_Overflow_id0+1)
#define OverflowExn	PTR_CtoML(_Overflow_e0+1)

#if defined(ASM_MATH)
extern ml_val_t _Ln_e0[];
#define LnExn PTR_CtoML(_Ln_e0+1)
extern ml_val_t _Sqrt_e0[];
#define SqrtExn PTR_CtoML(_Sqrt_e0+1)
#endif

extern ml_val_t sigh_resume[];
extern ml_val_t *sigh_return_c;
extern ml_val_t pollh_resume[];
extern ml_val_t *pollh_return_c;
extern ml_val_t callc_v[];
extern ml_val_t handle_v[];
extern ml_val_t *return_c;

extern ml_val_t _MLPollHandler[];
#define MLPollHandler PTR_CtoML(_MLPollHandler+1)

/** polling and MP references **/
extern ml_val_t _PollFreq0[];
#define PollFreq PTR_CtoML(_PollFreq0+1)
extern ml_val_t _PollEvent0[];
#define PollEvent PTR_CtoML(_PollEvent0+1)
extern ml_val_t _ActiveProcs0[];
#define ActiveProcs PTR_CtoML(_ActiveProcs0+1)

/* Initialize the C function list */
extern void InitCFunList ();
/* Record the C symbols that are visible to SML */
extern void RecordGlobals ();
/* Initialize the ML globals that are supported by the runtime system */
extern void AllocGlobals (ml_state_t *msp);
/* Bind a C function */
extern ml_val_t BindCFun (char *moduleName, char *funName);

#ifdef SIZES_C64_ML32
/* patch the 32-bit addresses */
extern void PatchAddrs ();
#endif

#endif /* !_ML_GLOBALS_ */
