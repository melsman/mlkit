/* asctime.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include <time.h>
#include <string.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#define DATE_LEN	24	/* we discard the trailing \n\0 */


/* _ml_Date_asctime : (int * int * int * int * int * int * int * int * int) -> string
 *
 * This takes a nine-tuple date (fields sec, min, hour, mday, mon, year, wday,
 * yday, and isdst), and converts it into a string representation.
 */
ml_val_t _ml_Date_asctime (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	res;
    struct tm	tm;

    tm.tm_sec	= REC_SELINT(arg, 0);
    tm.tm_min	= REC_SELINT(arg, 1);
    tm.tm_hour	= REC_SELINT(arg, 2);
    tm.tm_mday	= REC_SELINT(arg, 3);
    tm.tm_mon	= REC_SELINT(arg, 4);
    tm.tm_year	= REC_SELINT(arg, 5);
    tm.tm_wday	= REC_SELINT(arg, 6);
    tm.tm_yday	= REC_SELINT(arg, 7);
    tm.tm_isdst	= REC_SELINT(arg, 8);

    res = ML_AllocString(msp, DATE_LEN);
    strncpy (STR_MLtoC(res), asctime(&tm), DATE_LEN);

    return res;

} /* end of _ml_Date_asctime */

