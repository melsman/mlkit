/* mktime.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include <time.h>
#include "ml-base.h"
#include "ml-c.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Date_mktime : (int * int * int * int * int * int * int * int * int)
 *	-> Int32.int
 *
 * This takes a 9-tuple with the fields: tm_sec, tm_min, tm_hour, tm_mday,
 * tm_mon, tm_year, tm_wday, tm_yday, tm_isdst, and returns the corresponding
 * localtime value (in seconds).
 */
ml_val_t _ml_Date_mktime (ml_state_t *msp, ml_val_t arg)
{
    struct tm	tm;
    time_t	t;

    tm.tm_sec	= REC_SELINT(arg, 0);
    tm.tm_min	= REC_SELINT(arg, 1);
    tm.tm_hour	= REC_SELINT(arg, 2);
    tm.tm_mday	= REC_SELINT(arg, 3);
    tm.tm_mon	= REC_SELINT(arg, 4);
    tm.tm_year	= REC_SELINT(arg, 5);
    /* tm.tm_wday = REC_SELINT(arg, 6); */  /* ignored by mktime */
    /* tm.tm_yday = REC_SELINT(arg, 7); */  /* ignored by mktime */
    tm.tm_isdst	= REC_SELINT(arg, 8);

    t = mktime (&tm);

    if (t < 0) {
	return RAISE_ERROR(msp, "Invalid date");
    }
    else {
	ml_val_t	res;

	INT32_ALLOC(msp, res, t);
	return res;
    }

} /* end of _ml_Date_mktime */
