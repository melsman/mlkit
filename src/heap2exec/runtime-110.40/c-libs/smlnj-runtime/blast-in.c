/* blast_in.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "heap-io.h"
#include "cfun-proto-list.h"

/* _ml_RunT_blast_in : string -> 'a
 *
 * Build an ML object from a string.
 */
ml_val_t _ml_RunT_blast_in (ml_state_t *msp, ml_val_t arg)
{
    bool_t	errFlg = FALSE;
    ml_val_t	obj;

    obj = BlastIn (msp, PTR_MLtoC(Byte_t, arg), OBJ_LEN(arg), &errFlg);

    if (errFlg)
	return RAISE_ERROR(msp, "blast_in");
    else
	return obj;

} /* end of _ml_RunT_blast_in */

