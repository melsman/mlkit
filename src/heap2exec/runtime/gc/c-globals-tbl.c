/* c-globals-tbl.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * This implements a registry of global C symbols that may be referenced
 * in the ML heap (e.g., references to C functions).
 */

#include "ml-base.h"
#include "tags.h"
#include "ml-values.h"
#include "c-globals-tbl.h"

#define MAKE_EXTERN(index)	MAKE_DESC(index, DTAG_extern)

#define HASH_STRING(name, res)	{				\
	const char	*__cp = (name);				\
	int	__hash = 0, __n;				\
	for (;  *__cp;  __cp++) {				\
	    __n = (128*__hash) + (unsigned)*__cp;		\
	    __hash = __n - (8388593 * (__n / 8388593));		\
	}							\
	(res) = __hash;						\
    }

typedef struct item {		/* An item in the Symbol/Addr tables */
    ml_val_t	addr;		    /* The address of the external reference */
    const char	*name;		    /* The name of the reference */
    int		stringHash;	    /* The hash sum of the name */
    struct item	*nextSymb;	    /* The next item the SymbolTable bucket */
    struct item	*nextAddr;	    /* The next item the AddrTable bucket */
} item_t;

typedef struct item_ref {	/* an item in an export table */
    item_t	*item;
    int		index;
    struct item_ref *next;
} item_ref_t;

struct export_table {		/* A table of C symbols mapping strings to items, */
				/* which is used when loading a heap image. */
    item_ref_t	**table;
    int		tableSize;
    int		numItems;
    item_t	**itemMap;	    /* A map from item #s to items */
    int		itemMapSize;
};

/* hash key to index */
#define STRHASH_INDEX(h, sz)	((h) & ((sz)-1))
#define ADDRHASH_INDEX(a, sz)	(((Word_t)(a) >> 3) & ((sz)-1))


PVT item_t	**SymbolTable = NIL(item_t **);	/* Maps names to items */
PVT item_t	**AddrTable = NIL(item_t **);	/* Maps addresses to items */
PVT int		TableSize = 0;		/* The size of the tables; always */
					/* power of 2. */
PVT int		NumSymbols = 0;		/* The number of entries in the tables */

/* local routines */
PVT void GrowTable (export_table_t *tbl);


/* RecordCSymbol:
 *
 * Enter a global C symbol into the tables.
 */
void RecordCSymbol (const char *name, ml_val_t addr)
{
    int			n, i, hash;
    item_t		*item, *p;

    ASSERT ((((Word_t)addr & ~TAG_boxed) & TAG_desc) == 0);

    if (TableSize == NumSymbols) {
      /* double the table size */
	int	newTblSz = (TableSize ? 2*TableSize : 64);
	item_t	**newSTbl = NEW_VEC(item_t *, newTblSz);
	item_t	**newATbl = NEW_VEC(item_t *, newTblSz);

	memset ((char *)newSTbl, 0, sizeof(item_t *) * newTblSz);
	memset ((char *)newATbl, 0, sizeof(item_t *) * newTblSz);

	for (i = 0;  i < TableSize;  i++) {
	    for (p = SymbolTable[i];  p != NIL(item_t *); ) {
		item = p;
		p = p->nextSymb;
		n = STRHASH_INDEX(item->stringHash, newTblSz);
		item->nextSymb = newSTbl[n];
		newSTbl[n] = item;
	    }
	    for (p = AddrTable[i];  p != NIL(item_t *); ) {
		item = p;
		p = p->nextAddr;
		n = ADDRHASH_INDEX(item->addr, newTblSz);
		item->nextAddr = newATbl[n];
		newATbl[n] = item;
	    }
	}

	if (SymbolTable != NIL(item_t **)) {
	    FREE (SymbolTable);
	    FREE (AddrTable);
	}
	SymbolTable = newSTbl;
	AddrTable = newATbl;
	TableSize = newTblSz;
    }

  /* compute the string hash function */
    HASH_STRING(name, hash);

  /* Allocate the item */
    item = NEW_OBJ(item_t);
    item->name		= name;
    item->stringHash	= hash;
    item->addr		= addr;

  /* insert the item into the symbol table. */
    n = STRHASH_INDEX(hash, TableSize);
    for (p = SymbolTable[n];  p != NIL(item_t *);  p = p->nextSymb) {
	if ((p->stringHash == hash) && (strcmp(name, p->name) == 0)) {
	    if (p->addr != addr)
		Die ("global C symbol \"%s\" defined twice", name);
	    else {
		FREE (item);
		return;
	    }
	}
    }
    item->nextSymb	= SymbolTable[n];
    SymbolTable[n]	= item;

  /* insert the item into the addr table. */
    n = ADDRHASH_INDEX(addr, TableSize);
    for (p = AddrTable[n];  p != NIL(item_t *);  p = p->nextAddr) {
	if (p->addr == addr) {
	    if ((p->stringHash != hash) || (strcmp(name, p->name) != 0))
		Die ("address %#x defined twice: \"%s\" and \"%s\"",
		    addr, p->name, name);
	    else {
		FREE (item);
		return;
	    }
	}
    }
    item->nextAddr	= AddrTable[n];
    AddrTable[n]	= item;
    NumSymbols++;

} /* end of RecordCSymbol */

/* AddrToCSymbol:
 *
 * Return the name of the C symbol that labels the given address
 * (or NIL).
 */
const char *AddrToCSymbol (ml_val_t addr)
{
    item_t	*q;

  /* Find the symbol in the AddrTable */
    for (q = AddrTable[ADDRHASH_INDEX(addr, TableSize)];
	 q != NIL(item_t *);
	 q = q->nextAddr)
    {
	if (q->addr == addr)
	   return q->name;
    }

    return NIL(const char *);

} /* end of AddrToCSymbol */

/* NewExportTbl:
 */
export_table_t *NewExportTbl ()
{
    export_table_t	*tbl;

    tbl = NEW_OBJ(export_table_t);
    tbl->table		= NIL(item_ref_t **);
    tbl->tableSize	= 0;
    tbl->numItems	= 0;
    tbl->itemMap	= NIL(item_t **);
    tbl->itemMapSize	= 0;

    return tbl;

} /* end of NewExportTbl */

/* ExportCSymbol:
 *
 * Add an external address to an export table, returning its external reference
 * descriptor.
 */
ml_val_t ExportCSymbol (export_table_t *tbl, ml_val_t addr)
{
    Addr_t	a = PTR_MLtoADDR(addr);
    item_ref_t	*p;
    item_t	*q;
    int		h, index;

/*SayDebug("ExportCSymbol: addr = %#x, ", addr);*/

    if (tbl->numItems >= tbl->tableSize)
	GrowTable (tbl);

  /* First check to see if addr is already in tbl */
    h = ADDRHASH_INDEX(a, tbl->tableSize);
    for (p = tbl->table[h];  p != NIL(item_ref_t *);  p = p->next) {
	if (p->item->addr == addr) {
/*SayDebug("old name = \"%s\", index = %d\n", p->item->name, p->index);*/
	    return MAKE_EXTERN(p->index);
	}
    }

  /* Find the symbol in the AddrTable */
    for (q = AddrTable[ADDRHASH_INDEX(a, TableSize)];  q != NIL(item_t *);  q = q->nextAddr) {
	if (q->addr == addr)
	   break;
    }
    if (q == NIL(item_t *)) {
	Error("external address %#x not registered\n", addr);
	return ML_unit;
    }

  /* Insert the index into the address to index map. */
/*SayDebug("new name = \"%s\", index = %d\n", q->name, tbl->numItems);*/
    index		= tbl->numItems++;
    if (tbl->itemMapSize <= index) {
	int		newSz = ((tbl->itemMapSize == 0) ? 64 : 2*tbl->itemMapSize);
	item_t		**newMap = NEW_VEC(item_t *, newSz);
	int		i;

	for (i = 0;  i < tbl->itemMapSize;  i++)
	    newMap[i] = tbl->itemMap[i];
	if (tbl->itemMap != NIL(item_t **))
	    FREE (tbl->itemMap);
	tbl->itemMap = newMap;
	tbl->itemMapSize = newSz;
    }
    tbl->itemMap[index]	= q;

  /* Insert the address into the export table */
    p			= NEW_OBJ(item_ref_t);
    p->item		= q;
    p->index		= index;
    p->next		= tbl->table[h];
    tbl->table[h]	= p;

    return MAKE_EXTERN(index);

} /* end of ExportCSymbol */

/* AddrOfCSymbol:
 *
 * Given an external reference, return its address.
 */
ml_val_t AddrOfCSymbol (export_table_t *tbl, ml_val_t xref)
{
    int	index;

    index = GET_LEN(xref);

/*SayDebug("AddrOfCSymbol: %#x: %d --> %#x\n", xref, index, tbl->itemMap[index]->addr);*/
    if (index >= tbl->numItems)
	Die ("bad external object index %d", index);
    else
	return tbl->itemMap[index]->addr;

} /* end of AddrOfCSymbol */

/* ExportedSymbols:
 */
void ExportedSymbols (export_table_t *tbl, int *numSymbs, export_item_t **symbs)
{
    int			i, n = tbl->numItems;
    item_t		**p;
    export_item_t	*ep;

    *numSymbs = n;
    *symbs  = ep = NEW_VEC(export_item_t, n);
    for (p = tbl->itemMap, i = 0;  i < n;  i++) {
	*ep = (*p)->name;
	p++; ep++;
    }

} /* end of ExportedSymbols */


/* FreeExportTbl:
 *
 * Free the storage used by a import/export table.
 */
void FreeExportTbl (export_table_t *tbl)
{
    int		i;
    item_ref_t	*p, *q;

    for (i = 0;  i <  tbl->tableSize;  i++) {
	for (p = tbl->table[i];  p != NIL(item_ref_t *); ) {
	    q = p->next;
	    FREE (p);
	    p = q;
	}
    }

    if (tbl->itemMap != NIL(item_t **))
	FREE (tbl->itemMap);

    FREE (tbl);

} /* end of FreeExportTbl */


/* ImportCSymbol:
 */
ml_val_t ImportCSymbol (const char *name)
{
    int		hash, index;
    item_t	*p;

    HASH_STRING(name, hash);

  /* insert the item into the symbol table. */
    index = STRHASH_INDEX(hash, TableSize);
    for (p = SymbolTable[index];  p != NIL(item_t *);  p = p->nextSymb) {
	if ((p->stringHash == hash) && (strcmp(name, p->name) == 0)) {
	    return (p->addr);
	}
    }

    return ML_unit;

} /* end of ImportCSymbol */


/* ExportTableSz:
 *
 * Return the number of bytes required to represent the strings in an exported
 * symbols table.
 */
Addr_t ExportTableSz (export_table_t *tbl)
{
    int		i;
    Addr_t	nbytes;

    for (nbytes = 0, i = 0;  i < tbl->numItems;  i++) {
	nbytes += (strlen(tbl->itemMap[i]->name) + 1);
    }
    nbytes = ROUNDUP(nbytes, WORD_SZB);

    return nbytes;

} /* end of ExportTableSz */


/* GrowTable:
 */
PVT void GrowTable (export_table_t *tbl)
{
    int		newTblSz = (tbl->tableSize ? 2 * tbl->tableSize : 32);
    item_ref_t	**newTbl = NEW_VEC(item_ref_t *, newTblSz);
    int		i, n;
    item_ref_t	*p, *q;

    memset ((char *)newTbl, 0, newTblSz * sizeof(item_ref_t *));

    for (i = 0;  i < tbl->tableSize;  i++) {
	for (p = tbl->table[i];  p != NIL(item_ref_t *); ) {
	    q = p;
	    p = p->next;
	    n = ADDRHASH_INDEX(q->item->addr, newTblSz);
	    q->next = newTbl[n];
	    newTbl[n] = q;
	}
    }

    if (tbl->table != NIL(item_ref_t **)) FREE (tbl->table);
    tbl->table		= newTbl;
    tbl->tableSize	= newTblSz;

} /* end of GrowTable */

