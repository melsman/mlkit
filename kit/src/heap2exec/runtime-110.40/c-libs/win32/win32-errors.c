/* win32-errors.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 error functions
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

/* _ml_win32_get_last_error: unit -> word
 */
ml_val_t _ml_win32_get_last_error(ml_state_t *msp, ml_val_t arg)
{
    Word_t	err = (Word_t)GetLastError();
    ml_val_t	res;

    WORD_ALLOC(msp, res, err);

    return res;
}

/* end of win32-errors.c */

