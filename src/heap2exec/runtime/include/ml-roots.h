/* ml-roots.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * The root register indices for various machines.
 *
 * NROOTS gives the size of the variable-size portion (roots[]) of the
 * ML state vector.  Note that the name "roots" is slightly misleading;
 * while every entry in the vector must be saved over calls to C, not 
 * every entry is a valid root on every entry to C.  The valididity of
 * most entries is indicated using the register map convention (via
 * ArgRegMap); these entries are valid (and live) iff the corresponding
 * bit in the register mask is set (see cps/generic.sml).  N_ARG_REGS
 * gives the number of such entries. The pc, exncont, varptr, and baseptr
 * (if defined) are always valid roots, and the icounter (if defined) never is.
 */

#ifndef _ML_ROOTS_
#define _ML_ROOTS_

#if defined(TARGET_ALPHA32)
#   define NROOTS           22		/* $0-$4, pc, $6-$8, $10, $14, $16-26 */
#   define N_ARG_REGS       18
#   define N_PSEUDO_REGS    2
#   define ARG_INDX         0		/* $0  */
#   define CONT_INDX        1		/* $1  */
#   define CLOSURE_INDX     2		/* $2  */
#   define LINK_INDX        3		/* $3  */
#   define PC_INDX          4
#   define MISC0_INDX       5		/* $6  */
#   define MISC1_INDX       6		/* $7  */
#   define MISC2_INDX       7		/* $8  */
#   define MISC3_INDX       8		/* $15 */
#   define MISC4_INDX       9		/* $16 */
#   define MISC5_INDX       10		/* $17 */
#   define MISC6_INDX       11		/* $18 */
#   define MISC7_INDX       12		/* $19 */
#   define MISC8_INDX	    13		/* $20 */
#   define MISC9_INDX	    14		/* $20 */
#   define MISC10_INDX	    15		/* $20 */
#   define MISC11_INDX	    16		/* $20 */
#   define MISC12_INDX	    17		/* $20 */
#   define MISC13_INDX	    18		/* $20 */
#   define VAR_INDX         19		/* $10 */
#   define EXN_INDX         20		/* $14 */
#   define BASE_INDX        21		/* $4  */

#elif defined(TARGET_HPPA)
#   define NROOTS	    23
#   define N_PSEUDO_REGS    2
#   define N_ARG_REGS       19	 /* 4 std. regs & 15 miscregs */
#   define PC_INDX	    0
#   define LINK_INDX        1
#   define CLOSURE_INDX	    2
#   define ARG_INDX	    3
#   define CONT_INDX	    4
#   define VAR_INDX         5
#   define BASE_INDX	    6
#   define EXN_INDX	    7
 
#   define MISC0_INDX	    8
#   define MISC1_INDX	    9
#   define MISC2_INDX	    10
#   define MISC3_INDX	    11
#   define MISC4_INDX	    12
#   define MISC5_INDX	    13
#   define MISC6_INDX	    14
#   define MISC7_INDX	    15
#   define MISC8_INDX	    16
#   define MISC9_INDX	    17
#   define MISC10_INDX	    18
#   define MISC11_INDX	    19
#   define MISC12_INDX	    20
#   define MISC13_INDX	    21
#   define MISC14_INDX	    22
 
#elif defined (TARGET_M68)

#   define NROOTS	    8		/* d7, a0-a4, d3, pc */
#   define N_ARG_REGS       5
#   define PC_INDX	    7
#   define EXN_INDX	    0		/* d7 */
#   define ARG_INDX	    1		/* a0 */
#   define CONT_INDX	    2		/* a1 */
#   define CLOSURE_INDX	    3		/* a2 */
#   define VAR_INDX         6           /* d3 */
#   define LINK_INDX        4

#elif defined(TARGET_MIPS)

#   define NROOTS	    21		/* $2-$4, $30, pc, $5-$18, $20, $24 */
#   define N_ARG_REGS       17
#   define N_PSEUDO_REGS    2
#   define PC_INDX	    4
#   define EXN_INDX	    3		/* $30 */
#   define ARG_INDX	    0		/* $2 */
#   define CONT_INDX	    1		/* $3 */
#   define CLOSURE_INDX	    2		/* $4 */
#   define VAR_INDX         19          /* $20 */
#   define BASE_INDX	    20          /* $24 */
#   define LINK_INDX        5		/* $5 */
#   define MISC0_INDX	    6		/* $6 */
#   define MISC1_INDX	    7		/* $7 */
#   define MISC2_INDX	    8		/* $8 */
#   define MISC3_INDX	    9		/* $9 */
#   define MISC4_INDX	    10		/* $10 */
#   define MISC5_INDX	    11		/* $11 */
#   define MISC6_INDX	    12		/* $12 */
#   define MISC7_INDX	    13		/* $13 */

#elif defined(TARGET_RS6000)
#   define NROOTS	    24
#   define N_ARG_REGS       19
#   define N_PSEUDO_REGS    2

#   define LINK_INDX        0	
#   define CLOSURE_INDX	    1	
#   define ARG_INDX	    2	
#   define CONT_INDX	    3	
#   define EXN_INDX	    4	
#   define VAR_INDX         5   
#   define BASE_INDX	    6   
#   define PC_INDX	    8

#   define MISC0_INDX	    9
#   define MISC1_INDX	    10
#   define MISC2_INDX	    11
#   define MISC3_INDX	    12
#   define MISC4_INDX	    13
#   define MISC5_INDX	    14
#   define MISC6_INDX	    15
#   define MISC7_INDX	    16

#elif defined(TARGET_SPARC)

#   define NROOTS	    21		/* pc, %i0-i5, %g7, %g1-%g3, %l0-%l7, %o0-%o1 */
#   define N_ARG_REGS       17          /* exclude baseptr */
#   define N_PSEUDO_REGS    2
#   define PC_INDX	    6
#   define EXN_INDX	    7		/* %g7 */
#   define ARG_INDX	    0		/* %i0 */
#   define CONT_INDX	    1		/* %i1 */
#   define CLOSURE_INDX	    2		/* %i2 */
#   define BASE_INDX	    3		/* %i3 */
#   define VAR_INDX         5		/* %i5 */
#   define LINK_INDX        4		/* %g1 */
#   define MISC0_INDX	    8		/* %g2 */
#   define MISC1_INDX	    9		/* %g3 */
#   define MISC2_INDX	    10		/* %o0 */
#   define MISC3_INDX	    11		/* %o1 */
#   define MISC4_INDX	    12		/* %l0 */
#   define MISC5_INDX	    13		/* %l1 */
#   define MISC6_INDX	    14		/* %l2 */
#   define MISC7_INDX	    15		/* %l3 */

#elif defined (TARGET_X86)

#   define NROOTS	    26
#   define N_ARG_REGS	    23
#   define N_PSEUDO_REGS    2
#   define EXN_INDX	    0		/* 8(esp) */
#   define ARG_INDX	    1		/* ebp	   */
#   define CONT_INDX	    2		/* esi	   */
#   define CLOSURE_INDX	    3		/* 16(esp) */
#   define VAR_INDX	    4		/* 28(esp) */ 
#   define LINK_INDX	    5		/* 20(esp) */
#   define PC_INDX	    6		/* eip	   */
#   define MISC0_INDX	    7		/* ebx	   */
#   define MISC1_INDX	    8		/* ecx	   */
#   define MISC2_INDX	    9		/* edx	   */
    /* MISCn, where n > 2, is a virtual register */
#   define MISC3_INDX	    10		/* 40(esp) */
#   define MISC4_INDX	    11		/* 44(esp) */
#   define MISC5_INDX	    12		/* 48(esp) */
#   define MISC6_INDX	    13		/* 52(esp) */
#   define MISC7_INDX	    14		/* 56(esp) */
#   define MISC8_INDX	    15		/* 60(esp) */
#   define MISC9_INDX	    16		/* 64(esp) */
#   define MISC10_INDX	    17		/* 68(esp) */
#   define MISC11_INDX	    18		/* 72(esp) */
#   define MISC12_INDX	    19		/* 76(esp) */
#   define MISC13_INDX	    20		/* 80(esp) */
#   define MISC14_INDX	    21		/* 84(esp) */
#   define MISC15_INDX	    22		/* 88(esp) */
#   define MISC16_INDX	    23		/* 92(esp) */
#   define MISC17_INDX	    24		/* 96(esp) */
#   define MISC18_INDX	    25		/* 100(esp) */

#elif defined (TARGET_BYTECODE)

#   define NROOTS	    18		/* GPR[0-14], PC, exnPtr, varPtr */
#   define N_ARG_REGS       15		/* GPR[0-14] */
#   define PC_INDX	    15
#   define EXN_INDX	    16		/* exnPtr */
#   define ARG_INDX	    0		/* GPR[0] */
#   define CONT_INDX	    1		/* GPR[1] */
#   define CLOSURE_INDX	    2		/* GPR[2] */
#   define LINK_INDX	    3		/* GPR[3] */
#   define VAR_INDX         17          /* varPtr */

#endif

#endif /* !_ML_ROOTS_ */

