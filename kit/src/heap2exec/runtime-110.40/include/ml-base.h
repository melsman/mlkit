/* ml-base.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 */

#ifndef _ML_BASE_
#define _ML_BASE_

/* macro concatenation (ANSI CPP) */
#define CONCAT(a,b)	a ## b
#define CONCAT3(a,b,c)	a ## b ## c

#define ONE_K		1024
#define ONE_MEG 	(ONE_K*ONE_K)

/* The generated file ml-sizes.h defines various size macros, and
 * the following types:
 *
 * Int16_t	-- 16-bit signed integer
 * Int32_t	-- 32-bit signed integer
 * Int64_t	-- 64-bit signed integer (64-bit machines only)
 * Unsigned16_t	-- 16-bit unsigned integer
 * Unsigned32_t	-- 32-bit unsigned integer
 * Unsigned64_t	-- 64-bit unsigned integer (64-bit machines only)
 * Byte_t	-- unsigned 8-bit integer.
 * Word_t	-- unsigned integer that is large enough to hold an ML value.
 * Int_t	-- signed integer that is large enough to hold an ML value.
 * Addr_t	-- an unsigned integer that is large enough to hold an address.
 */
#ifndef _ML_SIZES_
#include "ml-sizes.h"
#endif

/* the size of a pair */
#define PAIR_SZB	(2*WORD_SZB)
/* the number of Word_t's per double */
#define REALD_SZW	(REALD_SZB / WORD_SZB)
/* the number of Word_t's per pair object */
#define PAIR_SZW	2
/* the number of Word_t's per special object */
#define SPECIAL_SZW	2

/* convert a number of bytes to an even number of words */
#define BYTES_TO_WORDS(N)	(((N)+(WORD_SZB-1)) >> LOG_BYTES_PER_WORD)

/* convert a number of doubles to an even number of words */
#define DOUBLES_TO_WORDS(N)	((N) * REALD_SZW)

/* on 32-bit machines it is useful to align doubles on 8-byte boundries */
#ifndef SIZES_C64_ML64
#  define ALIGN_REALDS
#endif


#ifndef _ASM_

#include <stdlib.h>

#define PVT	static

typedef Int32_t bool_t;
#ifndef TRUE		/* Some systems already define TRUE and FALSE */
#  define TRUE 1
#  define FALSE 0
#endif

typedef Int32_t status_t;
#define SUCCESS 1
#define FAILURE 0

/* nil pointers */
#define NIL(ty)		((ty)0)

/* assertions for debugging */
#ifdef ASSERT_ON
extern void AssertFail (const char *a, const char *file, int line);
/* #define ASSERT(A)	((A) ? ((void)0) : AssertFail(#A, __FILE__, __LINE__)) */
#define ASSERT(A)	{ if (!(A)) AssertFail(#A, __FILE__, __LINE__); }
#else
#define ASSERT(A)	{ }
#endif

/* Convert a bigendian 32-bit quantity into the host machine's representation. */
#if defined(BYTE_ORDER_BIG)
#  define BIGENDIAN_TO_HOST(x)	(x)
#elif defined(BYTE_ORDER_LITTLE)
   extern Unsigned32_t SwapBytes (Unsigned32_t x);
#  define BIGENDIAN_TO_HOST(x)	SwapBytes(x)
#else
#  error must define endianess
#endif

/* round i up to the nearest multiple of n, where n is a power of 2 */
#define ROUNDUP(i, n)		(((i)+((n)-1)) & ~((n)-1))


/* extract the bitfield of width WID starting at position POS from I */
#define XBITFIELD(I,POS,WID)		(((I) >> (POS)) & ((1<<(WID))-1))

/* aliases for malloc/free, so that we can easily replace them */
#define MALLOC(sz)	malloc(sz)
#define _FREE		free
#define FREE(p)		_FREE(p)

/* Allocate a new C object of type t. */
#define NEW_OBJ(t)	((t *)MALLOC(sizeof(t)))
/* Allocate a new C array of type t objects. */
#define NEW_VEC(t,n)	((t *)MALLOC((n)*sizeof(t)))

/* clear memory */
#define CLEAR_MEM(m, sz)	(memset((m), 0, (sz)))

/* The size of a page in the BIBOP memory map (in bytes) */
#define BIBOP_PAGE_SZB		((Addr_t)(64*ONE_K))
#define RND_MEMOBJ_SZB(SZ)	ROUNDUP(SZ,BIBOP_PAGE_SZB)

/** C types used in the run-time system **/
#ifdef SIZES_C64_ML32
typedef Unsigned32_t ml_val_t;
#else
typedef struct { Word_t v[1]; } ml_object_t; /* something for an ml_val_t to point to */
typedef ml_object_t *ml_val_t;
#endif
typedef struct vproc_state vproc_state_t;
typedef struct ml_state ml_state_t;
typedef struct heap heap_t;


/* In C, system constants are usually integers.  We represent these in the ML
 * system as (int * string) pairs, where the integer is the C constant, and the
 * string is a short version of the symbolic name used in C (e.g., the constant
 * EINTR might be represented as (4, "INTR")).
 */
typedef struct {	/* The representation of system constants */
    int		id;
    char	*name;
} sys_const_t;

typedef struct {	/* a table of system constants. */
    int		numConsts;
    sys_const_t	*consts;
} sysconst_tbl_t;


/* run-time system messages */
extern void Say (char *fmt, ...);
extern void SayDebug (char *fmt, ...);
extern void Error (char *, ...);
extern void Exit (int code);
extern void Die (char *, ...);

/* heap_params is an abstract type, whose representation depends on the
 * particular GC being used.
 */
typedef struct heap_params heap_params_t;

extern heap_params_t *ParseHeapParams (char **argv);
extern ml_state_t *AllocMLState (bool_t isBoot, heap_params_t *params);
extern void BootML (const char *bootlist, heap_params_t *params);
extern void LoadML (const char *loadImage, heap_params_t *params);

extern bool_t QualifyImageName (char *buf);
extern void InitMLState (ml_state_t *msp);
extern void SaveCState (ml_state_t *msp, ...);
extern void RestoreCState (ml_state_t *msp, ...);
extern void InitTimers ();
extern void ResetTimers (vproc_state_t *vsp);
extern ml_val_t ApplyMLFn (ml_state_t *msp, ml_val_t f, ml_val_t arg, bool_t useCont);
extern void RunML (ml_state_t *msp);
extern void RaiseMLExn (ml_state_t *msp, ml_val_t exn);
extern void InitFaultHandlers ();

#ifdef SOFT_POLL
extern void ResetPollLimit (ml_state_t *msp);
#endif


/* These are two views of the command line arguments; RawArgs is essentially
 * argv[].  CmdLineArgs is argv[] with runtime system arguments stripped
 * out (e.g., those of the form @SMLxxx[=yyy]).
 */
extern char	**RawArgs;
extern char	**CmdLineArgs;	/* does not include the command name (argv[0]) */
extern char	*MLCmdName;	/* the command name used to invoke the runtime */
extern bool_t	SilentLoad;
extern bool_t   DumpObjectStrings;
extern bool_t	GCMessages;
#ifdef HACKED_STANDALONE
extern bool_t   StandAlone;
#endif

/* The table of virtual processor ML states */
extern vproc_state_t	*VProc[];
extern int		NumVProcs;

#endif /* !_ASM_ */

#endif /* !_ML_BASE_ */

