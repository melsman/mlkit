/* ml-limits.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Various limits and default settings for the SML/NJ run-time system.
 */

#ifndef _ML_DEFAULTS_
#define _ML_DEFAULTS_

#include "ml-base.h"

#ifndef DFLT_IMAGE
#define DFLT_IMAGE		"sml-image"
#endif


/* the maximum length of a boot-file pathname */
#ifndef MAX_BOOT_PATH_LEN
#  define MAX_BOOT_PATH_LEN	512
#endif

/* the maximum number of boot files */
#ifndef MAX_NUM_BOOT_FILES
#  define MAX_NUM_BOOT_FILES	1024
#endif

/** Multiprocessor limits **/
#ifdef MP_SUPPORT
#  ifndef MAX_NUM_PROCS
#    define MAX_NUM_PROCS	8
#  endif
#else
#  define MAX_NUM_PROCS		1
#endif


/** Default heap sizes **/
#ifndef DFLT_NGENS
#  define DFLT_NGENS	5
#endif
#define MAX_NGENS	14		    /* should agree with MAX_NUM_GENS in */
					    /* arena-id.h. */
#define DFLT_CACHE_GEN	2		    /* Cache from-space for gens 1 & 2 */
#ifndef DFLT_ALLOC
#  define DFLT_ALLOC	(256*ONE_K)
#endif
#ifdef OLD_POLICY
#define RATIO_UNIT	16		    /* ratios are measured in 1/16ths */
#define DFLT_RATIO1	(7*(RATIO_UNIT/2))  /* gen-1 arenas are small */
#define DFLT_RATIO	(3*RATIO_UNIT)
#define MAX_SZ1(NSZ)	(5*(NSZ))
#endif
#define DFLT_RATIO1	20
#define DFLT_RATIO2	10
#define DFLT_RATIO	5
#define MAX_SZ1(NSZ)	(6*(NSZ))

/* the generation to allocate code objects in */
#define CODE_ALLOC_GEN	2

/* the size (in words) of a "small object."  The C allocation routines allocate
 * small objects in the allocation space, while large objects are allocated
 * in the first generation.
 */
#define SMALL_OBJ_SZW	512

/* This is the size (in bytes) of the allocation buffer.  If A is the value
 * of the limit pointer, then A[HEAP_BUF_SZ-1] is the address of the next
 * store-vector location.
 */
#define HEAP_BUF_SZ	(1024 + 128)
#define HEAP_BUF_SZB	(HEAP_BUF_SZ*WORD_SZB)

/* The maximum number of global C variables that can be roots. */
#define  MAX_C_ROOTS	8

/* maximum number of additional roots that can be passed to GC */
#define NUM_EXTRA_ROOTS 16

/* The number of potential GC roots. This includes space for C global roots,
 * ML roots, and the terminating null pointer.
 */
#ifdef N_PSEUDO_REGS
#define N_PSEUDO_ROOTS	N_PSEUDO_REGS
#else
#define N_PSEUDO_ROOTS	0
#endif
#ifdef MP_SUPPORT
/* 
 * must assume that all other procs are supplying NUM_EXTRA_ROOTS
 * in addition to the standard roots
 */
#  define NUM_GC_ROOTS							\
	ROUNDUP(MAX_NUM_PROCS*(MAX_C_ROOTS+NROOTS+N_PSEUDO_ROOTS)+	\
		(MAX_NUM_PROCS-1)*NUM_EXTRA_ROOTS+1, 8)
#else
#  define NUM_GC_ROOTS	\
	ROUNDUP(MAX_NUM_PROCS*(MAX_C_ROOTS+NROOTS+N_PSEUDO_ROOTS)+1, 8)
#endif

#ifdef SOFT_POLL  
/* limits for polling */
#define POLL_GRAIN_CPSI 1024     /* power of 2, in cps instructions */
#define POLL_GRAIN_BITS 10       /* log_2 POLL_GRAIN_CPSI */
#endif

#endif /* !_ML_DEFAULTS_ */

