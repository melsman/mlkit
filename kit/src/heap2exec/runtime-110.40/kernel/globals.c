/* globals.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#include "ml-base.h"
#include "machine-id.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-globals.h"
#include "ml-objects.h"
#include "ml-limits.h"
#include "c-globals-tbl.h"

#ifdef SIZES_C64_ML32
void PatchAddrs ();
#endif

#ifndef SIZES_C64_ML32

typedef struct {
	ml_val_t	desc;
	char		*s;
	ml_val_t	len;
} ml_string_t;

#define ML_STRING(id, s)				\
    ml_string_t id = {					\
	DESC_string,					\
	s,						\
	INT_CtoML(sizeof(s))				\
    }

/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    ML_STRING(CONCAT(ex,_s), name);			\
    ml_val_t CONCAT(ex,_id0) [2] = {			\
	DESC_ref,					\
	PTR_CtoML(&(CONCAT(ex,_s).s))			\
    }

#define ASM_CLOSURE(name)				\
    extern ml_val_t CONCAT(name,_a)[];			\
    ml_val_t CONCAT(name,_v)[2] = {			\
	MAKE_DESC(1,DTAG_record),			\
	PTR_CtoML(CONCAT(name,_a))			\
    }

#else /* SIZES_C64_ML32 */
/* When the size of Addr_t is bigger than the size of an Word_t, we need
 * to dynamically patch the static ML objects.
 */

typedef struct {
	ml_val_t	desc;
	ml_val_t	s;
	ml_val_t	len;
} ml_string_t;

#define ML_STRING(id,s)					\
    PVT char CONCAT(id,_data)[] = s;			\
    ml_string_t id = {					\
	DESC_string, ML_unit, INT_CtoML(sizeof(s))	\
    }

#define PATCH_ML_STRING(id)				\
    id.s = PTR_CtoML(CONCAT(id,_data))

/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    ML_STRING(CONCAT(ex,_s),name);			\
    ml_val_t CONCAT(ex,_id0) [2] = { DESC_ref, }

#define PATCH_ML_EXNID(ex)				\
    PATCH_ML_STRING(CONCAT(ex,_s));			\
    CONCAT(ex,_id0)[1] = PTR_CtoML(&(CONCAT(ex,_s).s))

#define ASM_CLOSURE(name)				\
    extern ml_val_t CONCAT(name,_a)[];			\
    ml_val_t CONCAT(name,_v)[2] = {			\
	MAKE_DESC(1, DTAG_record),			\
    }

#define PATCH_ASM_CLOSURE(name)				\
    CONCAT(name,_v)[1] = PTR_CtoML(CONCAT(name,_a))

#endif


#if (CALLEESAVE > 0)
#define ASM_CONT(name) 							\
    extern ml_val_t CONCAT(name,_a)[];					\
    ml_val_t *CONCAT(name,_c) = (ml_val_t *)(CONCAT(name,_a))
#else
#define ASM_CONT(name)							\
    ASM_CLOSURE(name);							\
    ml_val_t *CONCAT(name,_c) = (ml_val_t *)(CONCAT(name,_v)+1)
#endif

/* machine identification strings */
ML_STRING(machine_id, MACHINE_ID);


ASM_CLOSURE(array);
ASM_CLOSURE(bind_cfun);
ASM_CLOSURE(callc);
ASM_CLOSURE(create_b);
ASM_CLOSURE(create_r);
ASM_CLOSURE(create_s);
ASM_CLOSURE(create_v);
ASM_CLOSURE(floor);
ASM_CLOSURE(logb);
ASM_CLOSURE(scalb);
ASM_CLOSURE(try_lock);
ASM_CLOSURE(unlock);
ASM_CLOSURE(handle);

ASM_CONT(return);
ASM_CONT(sigh_return);
ASM_CONT(pollh_return);


/* A ref cell initialized to unit. */
#define REFCELL(z)	ml_val_t z[2] = {DESC_ref, ML_unit}

REFCELL(_ProfCurrent);
REFCELL(_PervStruct);
REFCELL(_MLSignalHandler);
REFCELL(_MLPollHandler);
REFCELL(_PollEvent0);
REFCELL(_PollFreq0);
REFCELL(_ActiveProcs0);

ml_val_t		RunTimeCompUnit = ML_unit;
#ifdef ASM_MATH
ml_val_t		MathVec = ML_unit;
#endif

/* aggregate structures of length zero */
const char _ML_string0_data[1]  = {0};
ml_val_t _ML_string0[3]		= {DESC_string, PTR_CtoML(_ML_string0_data), INT_CtoML(0)};
ml_val_t _ML_vector0[3]		= {DESC_polyvec, ML_unit, INT_CtoML(0)};

ML_EXNID(_Div,"Div");
ML_EXNID(_Overflow,"Overflow");
ML_EXNID(SysErr, "SysErr");

extern ml_val_t externlist0[];

#ifdef ASM_MATH
ML_EXNID(_Ln,"Ln");
ML_EXNID(_Sqrt,"Sqrt");
#endif


/* A table of pointers to global C variables that are potential roots. */
ml_val_t	*CRoots[MAX_C_ROOTS] = {
    &RunTimeCompUnit,
    _PervStruct+1,
    _MLSignalHandler+1,
    _MLPollHandler+1,
#ifdef ASM_MATH
    &MathVec,
#else
    NIL(ml_val_t *),
#endif
    NIL(ml_val_t *), NIL(ml_val_t *)
};
#ifdef ASM_MATH
int		NumCRoots = 5;
#else
int		NumCRoots = 4;
#endif


/* AllocGlobals:
 */
void AllocGlobals (ml_state_t *msp)
{
    ml_val_t	RunVec;
    ml_val_t    CStruct;

#ifdef SIZES_C64_ML32
    PatchAddrs ();
#endif

  /* allocate the RunVec */
#define RUNVEC_SZ	12
    ML_AllocWrite(msp,  0, MAKE_DESC(RUNVEC_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, PTR_CtoML(array_v+1));
    ML_AllocWrite(msp,  2, PTR_CtoML(bind_cfun_v+1));
    ML_AllocWrite(msp,  3, PTR_CtoML(callc_v+1));
    ML_AllocWrite(msp,  4, PTR_CtoML(create_b_v+1));
    ML_AllocWrite(msp,  5, PTR_CtoML(create_r_v+1));
    ML_AllocWrite(msp,  6, PTR_CtoML(create_s_v+1));
    ML_AllocWrite(msp,  7, PTR_CtoML(create_v_v+1));
    ML_AllocWrite(msp,  8, PTR_CtoML(floor_v+1));
    ML_AllocWrite(msp,  9, PTR_CtoML(logb_v+1));
    ML_AllocWrite(msp, 10, PTR_CtoML(scalb_v+1));
    ML_AllocWrite(msp, 11, PTR_CtoML(try_lock_v+1));
    ML_AllocWrite(msp, 12, PTR_CtoML(unlock_v+1));
    RunVec = ML_Alloc(msp, RUNVEC_SZ);

  /* allocate the CStruct */
#define CSTRUCT_SZ	12
    ML_AllocWrite(msp,  0, MAKE_DESC(CSTRUCT_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, RunVec);
    ML_AllocWrite(msp,  2, DivId);
    ML_AllocWrite(msp,  3, OverflowId);
    ML_AllocWrite(msp,  4, SysErrId);
    ML_AllocWrite(msp,  5, ProfCurrent);
    ML_AllocWrite(msp,  6, PollEvent);
    ML_AllocWrite(msp,  7, PollFreq);
    ML_AllocWrite(msp,  8, MLPollHandler);
    ML_AllocWrite(msp,  9, ActiveProcs);
    ML_AllocWrite(msp, 10, PervStruct);
    ML_AllocWrite(msp, 11, MLSignalHandler);
    ML_AllocWrite(msp, 12, ML_vector0);
    CStruct = ML_Alloc(msp, CSTRUCT_SZ);

  /* allocate 1-elem SRECORD just containing the CStruct */
    REC_ALLOC1(msp, RunTimeCompUnit, CStruct);

#ifdef ASM_MATH
#define MATHVEC_SZ	8
    ML_AllocWrite(msp,  0, MAKE_DESC(MATHVEC_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, LnId);
    ML_AllocWrite(msp,  2, SqrtId);
    ML_AllocWrite(msp,  3, PTR_CtoML(arctan_v+1));
    ML_AllocWrite(msp,  4, PTR_CtoML(cos_v+1));
    ML_AllocWrite(msp,  5, PTR_CtoML(exp_v+1));
    ML_AllocWrite(msp,  6, PTR_CtoML(ln_v+1));
    ML_AllocWrite(msp,  7, PTR_CtoML(sin_v+1));
    ML_AllocWrite(msp,  8, PTR_CtoML(sqrt_v+1));
    MathVec = ML_Alloc(msp, MATHVEC_SZ);
#endif

} /* end of AllocGlobals */


/* RecordGlobals:
 *
 * Record all global symbols that may be referenced from the ML heap.
 */
void RecordGlobals ()
{
  /* Misc. */
    RecordCSymbol ("handle",		PTR_CtoML(handle_v+1));
    RecordCSymbol ("return",		PTR_CtoML(return_c));
#if (CALLEESAVE == 0)
    RecordCSymbol ("return_a",		PTR_CtoML(return_a));
#endif

  /* RunVec */
    RecordCSymbol ("RunVec.array",	PTR_CtoML(array_v+1));
    RecordCSymbol ("RunVec.bind_cfun",	PTR_CtoML(bind_cfun_v+1));
    RecordCSymbol ("RunVec.callc",	PTR_CtoML(callc_v+1));
    RecordCSymbol ("RunVec.create_b",	PTR_CtoML(create_b_v+1));
    RecordCSymbol ("RunVec.create_r",	PTR_CtoML(create_r_v+1));
    RecordCSymbol ("RunVec.create_s",	PTR_CtoML(create_s_v+1));
    RecordCSymbol ("RunVec.create_v",	PTR_CtoML(create_v_v+1));
    RecordCSymbol ("RunVec.floor",	PTR_CtoML(floor_v+1));
    RecordCSymbol ("RunVec.logb",	PTR_CtoML(logb_v+1));
    RecordCSymbol ("RunVec.scalb",	PTR_CtoML(scalb_v+1));
    RecordCSymbol ("RunVec.try_lock",	PTR_CtoML(try_lock_v+1));
    RecordCSymbol ("RunVec.unlock",	PTR_CtoML(unlock_v+1));

  /* CStruct */
    RecordCSymbol ("CStruct.DivId",		DivId);
    RecordCSymbol ("CStruct.OverflowId",	OverflowId);
    RecordCSymbol ("CStruct.SysErrId",		SysErrId);
    RecordCSymbol ("CStruct.machine_id",	PTR_CtoML(machine_id.s));
    RecordCSymbol ("CStruct.PervStruct",	PervStruct);
    RecordCSymbol ("CStruct.MLSignalHandler",	MLSignalHandler);
    RecordCSymbol ("CStruct.vector0",		ML_vector0);
    RecordCSymbol ("CStruct.profCurrent",	ProfCurrent);
    RecordCSymbol ("CStruct.MLPollHandler",     MLPollHandler);
    RecordCSymbol ("CStruct.pollEvent",		PollEvent);
    RecordCSymbol ("CStruct.pollFreq",		PollFreq);
    RecordCSymbol ("CStruct.activeProcs",	ActiveProcs);

  /* null string */
    RecordCSymbol ("string0",			ML_string0);

#if defined(ASM_MATH)
  /* MathVec */
    RecordCSymbol ("MathVec.LnId",	LnId);
    RecordCSymbol ("MathVec.SqrtId",	SqrtId);
    RecordCSymbol ("MathVec.arctan",	PTR_CtoML(arctan_v+1));
    RecordCSymbol ("MathVec.cos",	PTR_CtoML(cos_v+1));
    RecordCSymbol ("MathVec.exp",	PTR_CtoML(exp_v+1));
    RecordCSymbol ("MathVec.ln",	PTR_CtoML(ln_v+1));
    RecordCSymbol ("MathVec.sin",	PTR_CtoML(sin_v+1));
    RecordCSymbol ("MathVec.sqrt",	PTR_CtoML(sqrt_v+1));
#endif

} /* end of RecordGlobals. */

#ifdef SIZES_C64_ML32

/* PatchAddrs:
 *
 * On machines where the size of Addr_t is bigger than the size of an Word_t,
 * we need to dynamically patch the static ML objects.
 */
void PatchAddrs ()
{
    PATCH_ML_STRING(machine_id);

    PATCH_ML_EXNID(_Div);
    PATCH_ML_EXNID(_Overflow);
    PATCH_ML_EXNID(SysErr);

    PATCH_ASM_CLOSURE(array);
    PATCH_ASM_CLOSURE(bind_cfun);
    PATCH_ASM_CLOSURE(callc);
    PATCH_ASM_CLOSURE(create_b);
    PATCH_ASM_CLOSURE(create_r);
    PATCH_ASM_CLOSURE(create_s);
    PATCH_ASM_CLOSURE(create_v);
    PATCH_ASM_CLOSURE(floor);
    PATCH_ASM_CLOSURE(logb);
    PATCH_ASM_CLOSURE(scalb);
    PATCH_ASM_CLOSURE(try_lock);
    PATCH_ASM_CLOSURE(unlock);
    PATCH_ASM_CLOSURE(handle);

#if (CALLEESAVE <= 0)
    PATCH_ASM_CLOSURE(return);
    PATCH_ASM_CLOSURE(sigh_return);
#endif

} /* end of PatchAddrs */

#endif /* SIZES_C64_ML32 */
