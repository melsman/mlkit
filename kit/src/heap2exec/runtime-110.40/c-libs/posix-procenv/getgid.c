/* getgid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include <unistd.h>
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_getgid: unit -> word
 *
 * Return group id
 */
ml_val_t _ml_P_ProcEnv_getgid (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	p;

    WORD_ALLOC (msp, p, (Word_t)(getgid()));
    return p;

} /* end of _ml_P_ProcEnv_getgid */

