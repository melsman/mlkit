/* addr-hash.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Hash tables for mapping addresses to objects.
 */

#include "ml-base.h"
#include "addr-hash.h"

typedef struct item {	    /* items in the hash table */
    Addr_t	    addr;	/* the address the object is keyed on */
    void	    *obj;	/* the object */
    struct item	    *next;	/* the next item in the bucket */
} item_t;

struct addr_tbl {
    int		    ignoreBits;
    int		    size;
    int		    numItems;
    Addr_t	    mask;
    item_t	    **buckets;
};

#define HASH(tbl,addr)	(((addr) >> (tbl)->ignoreBits) & (tbl)->mask)

/* MakeAddrTbl:
 *
 * Allocate an address hash table.
 */
addr_tbl_t *MakeAddrTbl (int ignoreBits, int size)
{
    unsigned int    nBuckets;
    int		    i;
    addr_tbl_t	    *tbl;

  /* find smallest power of 2 (but at least 16) that is greater than size */
    for (nBuckets = 16;  nBuckets < size;  nBuckets <<= 1)
	continue;

    tbl			= NEW_OBJ(addr_tbl_t);
    tbl->buckets	= NEW_VEC(item_t *, nBuckets);
    tbl->ignoreBits	= ignoreBits;
    tbl->size		= nBuckets;
    tbl->mask		= nBuckets-1;
    tbl->numItems	= 0;
    for (i = 0;  i < nBuckets;  i++)
	tbl->buckets[i] = NIL(item_t *);

    return tbl;

} /* end of MakeAddrTbl */

/* AddrTblInsert:
 *
 * Insert an object into a address hash table.
 */
void AddrTblInsert (addr_tbl_t *tbl, Addr_t addr, void *obj)
{
    int		h = HASH(tbl,addr);
    item_t	*p;

    for (p = tbl->buckets[h];  (p != NIL(item_t *)) && (p->addr != addr);  p = p->next)
	continue;
    if (p == NIL(item_t *)) {
	p		= NEW_OBJ(item_t);
	p->addr		= addr;
	p->obj		= obj;
	p->next		= tbl->buckets[h];
	tbl->buckets[h]	= p;
	tbl->numItems++;
    }
    else if (p->obj != obj)
	Die ("AddrTblInsert: %#x mapped to multiple objects", addr);

} /* end of AddrTblInsert */

/* AddrTblLookup:
 *
 * Return the object associated with the given address; return NIL, if not
 * found.
 */
void *AddrTblLookup (addr_tbl_t *tbl, Addr_t addr)
{
    int		h = HASH(tbl,addr);
    item_t	*p;

    for (p = tbl->buckets[h];  (p != NIL(item_t *)) && (p->addr != addr);  p = p->next)
	continue;

    if (p == NIL(item_t *))
	return NIL(void *);
    else
	return p->obj;

} /* end of AddrTblLookup */

/* AddrTblApply:
 *
 * Apply the given function to the elements of the table.
 */
void AddrTblApply (addr_tbl_t *tbl, void *clos, void (*f) (Addr_t, void *, void *))
{
    int		i;
    item_t	*p;

    for (i = 0;  i < tbl->size;  i++) {
	for (p = tbl->buckets[i];  p != NIL(item_t *);  p = p->next) {
	    (*f) (p->addr, clos, p->obj);
	}
    }

} /* end of AddrTblApply */

/* FreeAddrTbl:
 *
 * Deallocate the space for an address table; if freeObjs is true, also deallocate
 * the objects.
 */
void FreeAddrTbl (addr_tbl_t *tbl, bool_t freeObjs)
{
    int		i;
    item_t	*p, *q;

    for (i = 0;  i < tbl->size;  i++) {
	for (p = tbl->buckets[i];  p != NIL(item_t *);  ) {
	    q = p->next;
	    if (freeObjs)
		FREE (p->obj);
	    FREE (p);
	    p = q;
	}
    }
    FREE (tbl->buckets);
    FREE (tbl);

} /* end of FreeAddrTbl. */
