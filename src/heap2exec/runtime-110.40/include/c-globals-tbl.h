/* c-globals-tbl.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 */

#ifndef _C_GLOBALS_TBL_
#define _C_GLOBALS_TBL_

typedef struct export_table export_table_t;

/* info about an exported external reference */
typedef const char *export_item_t;

extern void RecordCSymbol (const char *name, ml_val_t addr);
extern const char *AddrToCSymbol (ml_val_t addr);

extern export_table_t *NewExportTbl ();
extern void FreeExportTbl (export_table_t *tbl);

extern ml_val_t ExportCSymbol (export_table_t *tbl, ml_val_t addr);
extern ml_val_t AddrOfCSymbol (export_table_t *tbl, ml_val_t xref);
extern void ExportedSymbols (export_table_t *tbl, int *numSymbs, export_item_t **symbs);

extern ml_val_t ImportCSymbol (const char *name);

extern Addr_t ExportTableSz (export_table_t *tbl);

#endif /* !_C_GLOBALS_TBL_ */
