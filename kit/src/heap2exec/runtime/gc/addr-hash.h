/* addr-hash.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Hash tables for mapping addresses to objects.
 */

#ifndef _ADDR_HASH_
#define _ADDR_HASH_

typedef struct addr_tbl addr_tbl_t;

/* Allocate an address hash table.
 */
extern addr_tbl_t *MakeAddrTbl (int ignoreBits, int size);

/* Insert an object into a address hash table.
 */
extern void AddrTblInsert (addr_tbl_t *tbl, Addr_t addr, void *obj);

/* Return the object associated with the given address; return NIL, if not
 * found.
 */
extern void *AddrTblLookup (addr_tbl_t *tbl, Addr_t addr);

/* Apply the given function to the elements of the table.  The second
 * argument to the function is the function's "closure," and the third is
 * the associated info.
 */
extern void AddrTblApply (addr_tbl_t *tbl, void *clos, void (*f) (Addr_t, void *, void *));

/* Deallocate the space for an address table; if freeObjs is true, also deallocate
 * the objects.
 */
extern void FreeAddrTbl (addr_tbl_t *tbl, bool_t freeObjs);

#endif /* !_ADDR_HASH_ */
