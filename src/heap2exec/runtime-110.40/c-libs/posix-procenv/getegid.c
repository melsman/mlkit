/* getegid.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include <unistd.h>
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_getegid: unit -> word
 *
 * Return effective group id
 */
ml_val_t _ml_P_ProcEnv_getegid (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	p;

    WORD_ALLOC (msp, p, (Word_t)(getegid()));
    return p;

} /* end of _ml_P_ProcEnv_getegid */

