/* execp.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_execp : string * string list -> 'a
 *
 * Overlay a new process image; resolve file to pathname using PATH
 */
ml_val_t _ml_P_Process_execp (ml_state_t *msp, ml_val_t arg)
{
    int             sts;
    ml_val_t	    file = REC_SEL(arg, 0);
    ml_val_t        arglst = REC_SEL(arg, 1);
    char            **argv;
    ml_val_t        p;
    char            **cp;

  /* use the heap for temp space for the argv[] vector */
    cp = (char **)(msp->ml_allocPtr);
#ifdef SIZES_C64_ML32
  /* must 8-byte align this */
    cp = (char **)ROUNDUP((Unsigned64_t)cp, ADDR_SZB);
#endif
    argv = cp;
    for (p = arglst;  p != LIST_nil;  p = LIST_tl(p))
        *cp++ = STR_MLtoC(LIST_hd(p));
    *cp++ = 0;  /* terminate the argv[] */

    sts = execvp(STR_MLtoC(file), argv);

    CHK_RETURN (msp, sts)

} /* end of _ml_P_Process_execp */
