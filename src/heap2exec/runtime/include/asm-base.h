/* asm-base.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Common definitions for assembly files in the SML/NJ system.
 */

#ifndef _ASM_BASE_
#define _ASM_BASE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* bool_t values for assembly code */
#define FALSE	0
#define TRUE	1

#if (!defined(GLOBALS_HAVE_UNDERSCORE)) && (defined(OPSYS_SUNOS) || defined(OPSYS_NEXTSTEP) || defined(OPSYS_WIN32))
#  define GLOBALS_HAVE_UNDERSCORE
#endif

/* we should probably consider factoring this out into ml-unixdep.h -- JHR */
#ifdef GLOBALS_HAVE_UNDERSCORE
#  define CSYM(ID)	CONCAT(_,ID)
#else
#  define CSYM(ID)	ID
#endif

#if defined(HOST_SPARC)
#  if defined(OPSYS_SUNOS)
#    include <machine/asm_linkage.h>
#    include <machine/trap.h>
#    undef ENTRY
#  elif defined(OPSYS_SOLARIS)
#    define _ASM
#    include <sys/stack.h>
#    include <sys/trap.h>
#  endif
#  define GLOBAL(ID)	.global	CSYM(ID)
#  define LABEL(ID)	ID:
#  define ALIGN4        .align 4
#  define WORD(W)       .word W
#  if defined(OPSYS_NEXTSTEP)
#    define TEXT          .text
#    define DATA          .data
#  else
#    define TEXT          .seg "text"
#    define DATA          .seg "data"
#  endif
#  define BEGIN_PROC(P)
#  define END_PROC(P)

#elif defined(HOST_MIPS)
#  if defined(OPSYS_MACH)
#    include <mips/regdef.h>
#  elif defined(OPSYS_IRIX5)
#    define _MIPS_SIM	1	/* IRIX 5.x needs this in <regdef.h> */
#    include <regdef.h>
#  else
#    include <regdef.h>
#  endif
#  define GLOBAL(ID)	.globl	CSYM(ID)
#  define LABEL(ID)	ID:
#  define ALIGN4        .align 2
#  define WORD(W)       .word W
#  define TEXT          .text
#  define DATA          .data
#  define BEGIN_PROC(P)	.ent CSYM(P)
#  define END_PROC(P)	.end CSYM(P)

#elif defined(HOST_ALPHA32)
#  include <regdef.h>
#  define GLOBAL(ID)	.globl  CSYM(ID)
#  define LABEL(ID)	ID:
#  define ALIGN4	.align 2
#  define WORD(W)	.word W
#  define TEXT		.text
#  define DATA		.data
#  define BEGIN_PROC(P)	.align 3; .ent CSYM(P)
#  define END_PROC(P)	.end CSYM(P)

#elif defined(HOST_HPPA)
#  define GLOBAL(ID)    .export ID,DATA
#  define LABEL(ID)     .label ID
#  define ALIGN4        .align 8
#  define BEGIN_PROC(P)
#  define END_PROC(P)
#  define __SC__	!

#elif defined(HOST_RS6000)
#  if defined(OPSYS_AIX)
#    define CFUNSYM(ID)	CONCAT(.,ID)
#    define USE_TOC
#    define GLOBAL(ID)	.globl CSYM(ID)
#    define TEXT	.csect [PR]
#    define DATA	.csect [RW]
#    define RO_DATA	.csect [RO]
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)   ID:
#  elif defined(OPSYS_MKLINUX)
#    define CFUNSYM(ID)	ID
#    define GLOBAL(ID)	.globl CSYM(ID)
#    define TEXT	.section ".text"
#    define DATA	.section ".data"
#    define RO_DATA	.section ".rodata"
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:
#  endif

#  define CENTRY(ID)		\
    .globl CFUNSYM(ID);		\
    LABEL(CFUNSYM(ID))

#elif defined(HOST_X86)
#  if defined(OPSYS_WIN32)
#    include "x86-masm.h"
#    define WORD(W)     WORD32(W)
#  else
#    define GLOBAL(ID)	  .globl	CSYM(ID)
#    define LABEL(ID)	  CONCAT(ID,:)
#    define IMMED(ID)	  CONCAT($,ID)
#    define ALIGN4        .align 2
#    define WORD(W)       .word W
#    define TEXT          .text
#    define DATA          .data
#    define BEGIN_PROC(P) .ent P
#    define END_PROC(P)	  .end P
#  endif

#else

#  error missing asm definitions

#endif

#ifndef __SC__
#  define __SC__ 	;
#endif

#define ENTRY(ID)		\
    GLOBAL(ID) __SC__		\
    LABEL(CSYM(ID))

#define ML_CODE_HDR(name)			\
	    GLOBAL(name) __SC__			\
	    ALIGN4 __SC__			\
    LABEL(CSYM(name))

#endif /* !_ASM_BASE_ */

