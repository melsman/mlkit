/* localtime.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include <time.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Date_localtime : Int32.int -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a local time value (in seconds), and converts it to a 9-tuple with
 * the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday,
 * tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_localtime (ml_state_t *msp, ml_val_t arg)
{
    time_t	t = (time_t)INT32_MLtoC(arg);
    struct tm	*tm;

    tm = localtime (&t);

    ML_AllocWrite(msp, 0, MAKE_DESC(DTAG_record, 9));
    ML_AllocWrite(msp, 1, INT_CtoML(tm->tm_sec));
    ML_AllocWrite(msp, 2, INT_CtoML(tm->tm_min));
    ML_AllocWrite(msp, 3, INT_CtoML(tm->tm_hour));
    ML_AllocWrite(msp, 4, INT_CtoML(tm->tm_mday));
    ML_AllocWrite(msp, 5, INT_CtoML(tm->tm_mon));
    ML_AllocWrite(msp, 6, INT_CtoML(tm->tm_year));
    ML_AllocWrite(msp, 7, INT_CtoML(tm->tm_wday));
    ML_AllocWrite(msp, 8, INT_CtoML(tm->tm_yday));
    ML_AllocWrite(msp, 9, INT_CtoML(tm->tm_isdst));

    return ML_Alloc(msp, 9);

} /* end of _ml_Date_localtime */
