/* blast-out.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#ifndef _BLAST_OUT_
#define _BLAST_OUT_

#ifndef _ADDR_HASH_
#include "addr-hash.h"
#endif

#ifndef _C_GLOBALS_TBL_
#include "c-globals-tbl.h"
#endif

#ifndef _WRITER_
#include "writer.h"
#endif

/* the table of referenced code objects, and embedded literals */

typedef enum {
    EMB_STRING,		/* embedded string */
    EMB_REALD,		/* embedded real */
    UNUSED_CODE,	/* code object with only embedded references */
    USED_CODE		/* code object with code references */
} embobj_kind_t;

typedef struct embobj_info {    /* info about an embedded object */
    embobj_kind_t	kind;
    struct embobj_info  *codeObj;	/* points to entry for the code */
					/* object that this literal is */
					/* embedded in. */
    ml_val_t		relAddr;	/* the relocated address of the literal */
					/* in the blasted heap image. */
} embobj_info_t;

/* find an embedded object */
#define FindEmbObj(tbl, addr)	\
	((embobj_info_t *)AddrTblLookup((tbl), (Addr_t)(addr)))


typedef struct {		/* the result of blasting out an object */
    bool_t	error;		    /* true, if there was an error during the */
				    /* blast GC (e.g., unrecognized external obj) */
    bool_t	needsRepair;	    /* true, if the heap needs repair; otherwise */
				    /* the collection must be completed. */
    int		maxGen;		    /* the oldest generation included in the blast. */
    export_table_t *exportTbl;	    /* the table of external objects */
    addr_tbl_t	*embobjTbl;	    /* the table of embedded objects */
} blast_res_t;

extern blast_res_t BlastGC (ml_state_t *msp, ml_val_t *root, int gen);
Addr_t BlastGC_AssignLitAddrs (blast_res_t *res, int id, Addr_t offset);
void BlastGC_BlastLits (writer_t *wr);
extern void BlastGC_FinishUp (ml_state_t *msp, blast_res_t *res);

#endif  /* _BLAST_OUT_ */
