/* obj-info.c
 *
 * COPYRIGHT (c) 1993 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "heap.h"
#include "gc.h"

PVT void GetDescInfo (Word_t desc, obj_info_t *info);

/* GetObjInfo:
 *
 *    Get information about an object; return -1 for unboxed objects.
 */
obj_info_t GetObjInfo (ml_val_t obj)
{
    obj_info_t	result;

    result.isExternal = FALSE;
    result.isSpecial = FALSE;

    if (isBOXED(obj)) {
	aid_t	aid = ADDR_TO_PAGEID(BIBOP, obj);
	if (IS_BIGOBJ_AID(aid)) {
	    int			i;
	    bigobj_region_t	*region;
	    bigobj_desc_t	*dp;

	    for (i = BIBOP_ADDR_TO_INDEX(obj);  !BO_IS_HDR(aid);  aid = BIBOP[--i])
		continue;
	    region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);
	    dp = ADDR_TO_BODESC(region, obj);

	    result.objStart = (Word_t *)(dp->obj);
	    result.sizeB = dp->sizeB;
	    result.gen = dp->gen;
	    result.kind = CODE_INDX;
	    result.isBig = TRUE;
/* NOTE: the following assumes that raw code pointers are never passed to this */
	    result.isEmbedded = (PTR_CtoML(dp->obj + WORD_SZB) != obj);
	    result.isAtomic = TRUE;
	}
	else {
	    Word_t	*start, desc;

	    if (aid == AID_NEW) {
	      /* Find the object header (object may be derived) */
		for (start = PTR_MLtoC(Word_t, obj);  !isDESC(desc = start[-1]);  start--)
		    continue;
		GetDescInfo (desc, &result);
		result.objStart = start;
		result.gen = 0;
	    }
	    else if (isUNMAPPED(aid)) {
	      /* Runtime system reference */
		result.objStart = NIL(Word_t *);
		result.sizeB = 0;
		result.gen = -1;
		result.kind = -1;
		result.isExternal = TRUE;
	    }
	    else {
		result.gen = EXTRACT_GEN(aid);
		result.kind = EXTRACT_OBJC(aid);
		start = PTR_MLtoC(Word_t, obj);
		if (result.kind == OBJC_pair) {
		    result.objStart = (Word_t *)((Addr_t)start & ~(WORD_SZB-1));
		    result.sizeB = WORD_SZB*2;
		}
		else {
		    for (;  !isDESC(desc = start[-1]);  start--)
			continue;
		    GetDescInfo(desc, &result);
		    result.objStart = start;
		}
	    }
	    result.isBig = FALSE;
	    result.isEmbedded = FALSE;
	    result.isAtomic = (result.kind == OBJC_string);
	}
    }
    else {
	result.objStart = NIL(Word_t *);
	result.sizeB = 0;
	result.gen = -1;
	result.kind = -1;
	result.isBig = FALSE;
	result.isEmbedded = FALSE;
	result.isAtomic = FALSE;
    }

    return result;

} /* end of GetObjInfo */


/* GetDescInfo:
 */
PVT void GetDescInfo (Word_t desc, obj_info_t *info)
{
    switch (GET_TAG(desc)) {
      case DTAG_record:
	info->kind = OBJC_record;
	info->sizeB = WORD_SZB*GET_LEN(desc) + WORD_SZB;
	break;
      case DTAG_pair:
	info->kind = OBJC_pair;
	info->sizeB = WORD_SZB*3;
	break;
      case DTAG_array:
	info->kind = OBJC_array;
	info->sizeB = WORD_SZB*GET_LEN(desc) + WORD_SZB;
	break;
      case DTAG_string:
      case DTAG_bytearray:
	info->kind = OBJC_string;
	info->sizeB = WORD_SZB*GET_STR_LEN(desc) + WORD_SZB;
	break;
      case DTAG_reald:
	info->kind = OBJC_string;
	info->sizeB = sizeof(double) + WORD_SZB;
	break;
      case DTAG_realdarray:
	info->kind = OBJC_string;
	info->sizeB = WORD_SZB*GET_REALDARR_LEN(desc) + WORD_SZB;
	break;
      case DTAG_special:
	info->kind = OBJC_array;
	info->sizeB = WORD_SZB*2;
	break;
    }

} /* end of GetDescInfo */

