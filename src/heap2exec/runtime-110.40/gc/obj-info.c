/* obj-info.c
 *
 * COPYRIGHT (c) 1993 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "heap.h"
#include "gc.h"

/* GetObjGen:
 *
 * Get the generation of an object (return -1 for external/unboxed objects).
 */
int GetObjGen (ml_val_t obj)
{
    if (isBOXED(obj)) {
	aid_t	aid = ADDR_TO_PAGEID(BIBOP, obj);
	if (IS_BIGOBJ_AID(aid)) {
	    int		i;
	    bigobj_region_t	*region;
	    bigobj_desc_t	*dp;

	    for (i = BIBOP_ADDR_TO_INDEX(obj);  !BO_IS_HDR(aid);  aid = BIBOP[--i])
		continue;
	    region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);
	    dp = ADDR_TO_BODESC(region, obj);

	    return dp->gen;
	}
	else if (aid == AID_NEW)
	    return 0;
	else if (isUNMAPPED(aid))
	    return -1;
	else
	    return EXTRACT_GEN(aid);
    }
    else
	return -1;

} /* end of GetObjGen */

