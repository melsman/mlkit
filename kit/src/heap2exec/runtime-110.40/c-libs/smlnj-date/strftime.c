/* strftime.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include <time.h>
#include <string.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Date_strftime :
 *    (string * (int * int * int * int * int * int * int * int * int)) -> string
 *
 * This takes a format field and nine integer fields (sec, min, hour, mday, mon,
 * year, wday, yday, and isdst), and converts it into a string representation
 * according to the format string.
 */
ml_val_t _ml_Date_strftime (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	fmt = REC_SEL(arg, 0);
    ml_val_t	res, date;
    struct tm	tm;
    char	buf[512];
    size_t	sz;

    date	= REC_SEL(arg, 1);
    tm.tm_sec	= REC_SELINT(date, 0);
    tm.tm_min	= REC_SELINT(date, 1);
    tm.tm_hour	= REC_SELINT(date, 2);
    tm.tm_mday	= REC_SELINT(date, 3);
    tm.tm_mon	= REC_SELINT(date, 4);
    tm.tm_year	= REC_SELINT(date, 5);
    tm.tm_wday	= REC_SELINT(date, 6);
    tm.tm_yday	= REC_SELINT(date, 7);
    tm.tm_isdst	= REC_SELINT(date, 8);

    sz = strftime (buf, sizeof(buf), STR_MLtoC(fmt), &tm);
    if (sz > 0) {
	res = ML_AllocString(msp, sz);
	strncpy (STR_MLtoC(res), buf, sz);
	return res;
    }
    else
	return RAISE_ERROR(msp, "strftime failed");

} /* end of _ml_Date_strftime */

