/* getuid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-objects.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getuid: unit -> word
 *
 * Return user id
 */
ml_val_t _ml_P_ProcEnv_getuid (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	p;

    WORD_ALLOC (msp, p, (Word_t)(getuid()));
    return p;

} /* end of _ml_P_ProcEnv_getuid */

