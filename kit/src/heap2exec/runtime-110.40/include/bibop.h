/* bibop.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * The BIBOP maps memory pages to page IDs.  The interpretation of most
 * of these IDs is defined by the GC (see ../gc/arena-id.h), but the
 * IDs for unmapped memory are defined here.
 *
 */

#ifndef _BIBOP_
#define _BIBOP_

typedef Unsigned16_t page_id_t;

#define PAGEID_unmapped	0xffff

#define isUNMAPPED(ID)	((ID) == PAGEID_unmapped)


/** The BIBOP **/

#ifdef TWO_LEVEL_MAP

#  error two level BIBOP mapping not implemented

typedef page_id_t **bibop_t;

#else

#define BIBOP_SHIFT		16		/* log2(BIBOP_PAGE_SZB) */
#define BIBOP_BITS		(BITS_PER_WORD-BIBOP_SHIFT)
#define BIBOP_SZ		(1<<BIBOP_BITS)
#define BIBOP_ADDR_TO_INDEX(a)	(((Addr_t)(a))>>BIBOP_SHIFT)

#define BIBOP_INDEX_TO_ADDR(i)	((Addr_t)((i) << BIBOP_SHIFT))
#define BIBOP_NBLKS_TO_SZB(i)	((Addr_t)((i) << BIBOP_SHIFT))

typedef page_id_t *bibop_t;

extern bibop_t        BIBOP;

#define ADDR_TO_PAGEID(bibop,a)	((bibop)[BIBOP_ADDR_TO_INDEX(a)])

#endif /* !TWO_LEVEL_MAP */

#endif /* !_BIBOP_ */
