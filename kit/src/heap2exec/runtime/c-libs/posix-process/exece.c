/* exece.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_exece : string * string list * string list -> 'a
 *
 * Overlay a new process image, using specified environment.
 */
ml_val_t _ml_P_Process_exece (ml_state_t *msp, ml_val_t arg)
{
    int             sts;
    char*           path = REC_SELPTR(char, arg, 0);
    ml_val_t        arglst = REC_SEL(arg, 1);
    ml_val_t	    envlst = REC_SEL(arg, 2);
    char            **argv, **envp;
    ml_val_t        p;
    char            **cp;

      /* use the heap for temp space for the argv[] and envp[] vectors */
    cp = (char **)(msp->ml_allocPtr);
#ifdef SIZES_C64_ML32
      /* must 8-byte align this */
    cp = (char **)ROUNDUP((Unsigned64_t)cp, ADDR_SZB);
#endif
    argv = cp;
    for (p = arglst;  p != LIST_nil;  p = LIST_tl(p))
        *cp++ = PTR_MLtoC(char, LIST_hd(p));
    *cp++ = 0;  /* terminate the argv[] */

    envp = cp;
    for (p = envlst;  p != LIST_nil;  p = LIST_tl(p))
        *cp++ = PTR_MLtoC(char, LIST_hd(p));
    *cp++ = 0;  /* terminate the envp[] */

    sts = execve(path,argv,envp);

    CHK_RETURN (msp, sts)

} /* end of _ml_P_Process_exece */
