/* gen-regmask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * This file generates default definitions of some compiler flags and
 * various register masks.  The masks define the registers that are
 * live in the following situations:
 *
 *   FUN_MASK	-- polymorphic (wrapped) function entry.
 *
 *   RET_MASK	-- return continuation mask
 *
 *   CONT_MASK	-- wrapped callcc continuation entry.
 *
 *   EXN_MASK	-- exception handler entry
 *
 * The defined constants are:
 *
 *   CALLEESAVE
 *   FLOATCALLEESAVE
 */

#include "gen.h"

#ifndef DST_FILE
#define DST_FILE "reg-mask.h"
#endif

#ifndef CALLEESAVE
#  define GEN_CALLEESAVE
#  if defined(TARGET_M68)
#    define CALLEESAVE	0
#  else
#    define CALLEESAVE	3
#  endif
#endif
#ifndef FLOAT_CALLEESAVE
#  define GEN_FLOAT_CALLEESAVE
#  define FLOAT_CALLEESAVE 0
#endif

#if (CALLEESAVE > 0)
#  define FUN_MASK ((1 << (CALLEESAVE + 4)) - 1)
#  define RET_MASK ((1 << (CALLEESAVE + 4)) - 0x10 + 0xc)
#  define CONT_MASK FUN_MASK
#  define EXN_MASK FUN_MASK
#else
#  define FUN_MASK ((1 << (CALLEESAVE + 4)) - 1)
#  define RET_MASK (0xd)
#  define CONT_MASK FUN_MASK
#  define EXN_MASK CONT_MASK
#endif

main ()
{
    FILE	    *f;

    f = OpenFile (DST_FILE, "_REG_MASK_");

    fprintf (f, "\n");
#ifdef GEN_CALLEESAVE
    fprintf (f, "#define CALLEESAVE       %d\n", CALLEESAVE);
#endif
#ifdef GEN_FLOAT_CALLEESAVE
    fprintf (f, "#define FLOAT_CALLEESAVE %d\n", FLOAT_CALLEESAVE);
#endif
    fprintf (f, "\n");
    fprintf (f, "#define FUN_MASK\t\t%d\t/*\t%#010x\t*/\n", 
	     FUN_MASK, FUN_MASK);
    fprintf (f, "#define RET_MASK\t\t%d\t/*\t%#010x\t*/\n", 
	     RET_MASK, RET_MASK);
    fprintf (f, "#define CONT_MASK\t\t%d\t/*\t%#010x\t*/\n",
	     CONT_MASK, CONT_MASK);
    fprintf (f, "#define EXN_MASK\t\t%d\t/*\t%#010x\t*/\n", 
	     EXN_MASK, EXN_MASK);
    fprintf (f, "\n");
    CloseFile (f, "_REG_MASK_");

    exit (0);

}
