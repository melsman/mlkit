/* arena-id.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Definitions for the arena IDs and for mapping from addresses to arena IDs.
 *
 * An arena ID (aid_t) is an unsigned 16-bit value, with the following layout:
 *
 *   bits 0-7:	  heap block ID (0xFF for unmapped objects)
 *   bits 8-11:   object class:
 *                  0000 = new-space objects
 *		    1111 = unmapped objects
 *   bits 12-15:  generation number (0 for new space, 1-14 for older generations,
 *		  and 15 for non-heap memory)
 *
 * Heap pages in allocation space have the arena ID 0x0000, and unmapped heap
 * pages have the arena ID 0xffff.  The ID format is designed so that a
 * from-space page can be detected by having a generation field less than or
 * equal to the maximum generation being collected.
 */

#ifndef _ARENA_ID_
#define _ARENA_ID_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _BIBOP_
#include "bibop.h"
#endif

/* The indices of the different object arenas.  WIth four bits for the
 * object class, we have up to 7 regular objects and up to 7 big-object
 * arenas.
 */
/* The different classes of objects; each class lives in a different arena */
#define	RECORD_INDX		0
#define PAIR_INDX		1
#define STRING_INDX		2
#define ARRAY_INDX		3
#define NUM_ARENAS		4

/* the different classes of big-objects, which live in big-object regions */
#define CODE_INDX		0
#define NUM_BIGOBJ_KINDS	1

#define NUM_OBJ_KINDS		(NUM_ARENAS+NUM_BIGOBJ_KINDS)

/* arena IDs */
typedef page_id_t aid_t;

/* The number of bits in the arena ID fields. The number of bits should add
 * up to sizeof(aid_t)*8.
 */
#define HBLK_BITS		8
#define OBJC_BITS		4
#define GEN_BITS		4

#define HBLK_new		0
#define MAX_HBLK		0xff
#define HBLK_MASK		((1<<HBLK_BITS)-1)
#define HBLK_bigobj		0	/* non-header bigobj pages */
#define HBLK_bigobjhdr		1	/* header bigobj pages */

/* The different classes of objects. */
#define MAKE_OBJC(INDX)		(INDX+1)
#define MAKE_BIGOBJC(INDX)	(0x8|((INDX)<<1))
#define OBJC_new		0x0
#define OBJC_record		MAKE_OBJC(RECORD_INDX)
#define OBJC_pair		MAKE_OBJC(PAIR_INDX)
#define OBJC_string		MAKE_OBJC(STRING_INDX)
#define OBJC_array		MAKE_OBJC(ARRAY_INDX)
#define OBJC_bigobj		MAKE_BIGOBJC(CODE_INDX)
#define OBJC_MAX		0xf
#define OBJC_MASK		((1<<OBJC_BITS)-1)

#define OBJC_SHIFT		HBLK_BITS
#define GEN_SHIFT		(HBLK_BITS+OBJC_BITS)

#define MAX_NUM_GENS		((1 << GEN_BITS) - 2)
#define ALLOC_GEN		0		/* the generation # of the */
						/* allocation arena */

/* Macros on arena IDs */
#define MAKE_AID(GEN,OBJC,BLK)	\
	((aid_t)(((GEN)<<GEN_SHIFT) | ((OBJC)<<OBJC_SHIFT) | (BLK)))
#define MAKE_MAX_AID(GEN)	MAKE_AID((GEN), OBJC_MAX, MAX_HBLK)
#define EXTRACT_HBLK(AID)	((AID)&HBLK_MASK)
#define EXTRACT_OBJC(AID)	(((AID) >> OBJC_SHIFT)&OBJC_MASK)
#define EXTRACT_GEN(AID)	((AID) >> GEN_SHIFT)
#define IS_FROM_SPACE(AID,MAX_AID)	\
	((AID) <= (MAX_AID))

/* the arena IDs for new-space and unmapped heap pages, and for free big-objects */
#define AID_NEW			MAKE_AID(ALLOC_GEN,OBJC_new,HBLK_new)
#define AID_UNMAPPED		PAGEID_unmapped
#define AID_MAX			MAKE_MAX_AID(MAX_NUM_GENS)

#ifdef TOSPACE_ID /* for debugging */
#define TOSPACE_AID(OBJC,BLK)	MAKE_AID(0xf,(OBJC),BLK)
#define TOSPACE_GEN(AID)	EXTRACT_OBJC(AID)
#define IS_TOSPACE_AID(AID)	(((AID) != AID_UNMAPPED) && (EXTRACT_GEN(AID) == 0xf))
#endif

/* AIds for big-object regions.  These are always marked as from-space, since
 * both from-space and two-space objects of different generations can occupy
 * the same big-object region.
 */
#define AID_BIGOBJ(GEN)		MAKE_AID(GEN,OBJC_bigobj,HBLK_bigobj)
#define AID_BIGOBJ_HDR(GEN)	MAKE_AID(GEN,OBJC_bigobj,HBLK_bigobjhdr)

/* return true if the AID is a AID_BIGOBJ_HDR (we assume that it is
 * either an AID_BIGOBJ or an AID_BIGOBJ_HDR id).
 */
#define BO_IS_HDR(AID)		(EXTRACT_HBLK(AID) == HBLK_bigobjhdr)

/* return true, if AID is a big-object AID */
#define IS_BIGOBJ_AID(ID)	(EXTRACT_OBJC(ID) == OBJC_bigobj)

#endif /* !_ARENA_ID_ */

