/* record-ops.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Some (type unsafe) operations on records.
 */


#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "arena-id.h"
#include "gc.h"


/* GetLen:
 *
 * Check that we really have a record object, and return its length.
 */
PVT int GetLen (ml_val_t r)
{
    ml_val_t	d;
    int		t;

    if (! isBOXED(r))
	return -1;

    switch (EXTRACT_OBJC(ADDR_TO_PAGEID(BIBOP, r))) {
      case OBJC_new:
	d = OBJ_DESC(r);
	t = GET_TAG(d);
	if (t == DTAG_record)
	    return GET_LEN(d);
	else
	    return -1;
      case OBJC_pair: return 2;
      case OBJC_record:
	d = OBJ_DESC(r);
	t = GET_TAG(d);
	if (t == DTAG_record)
	    return GET_LEN(d);
	else
	    return -1;
      default:
	return -1;
    }

}

/* RecordConcat:
 *
 * Concatenate two records; returns unit if either argument is not
 * a record of length at least one.
 */
ml_val_t RecordConcat (ml_state_t *msp, ml_val_t r1, ml_val_t r2)
{
    int		l1 = GetLen(r1);
    int		l2 = GetLen(r2);

    if ((l1 > 0) && (l2 > 0)) {
	int		n = l1+l2;
	int		i, j;
	ml_val_t	*p, res;

	ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_record));
	j = 1;
	for (i = 0, p = PTR_MLtoC(ml_val_t, r1);  i < l1;  i++, j++) {
	    ML_AllocWrite (msp, j, p[i]);
	}
	for (i = 0, p = PTR_MLtoC(ml_val_t, r2);  i < l2;  i++, j++) {
	    ML_AllocWrite (msp, j, p[i]);
	}
	res = ML_Alloc(msp, n);
	return res;
    }
    else {
	return ML_unit;
    }

} /* end of RecordConcat */

