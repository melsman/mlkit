#ifndef LOADKAM_H
#define LOADKAM_H

#include "../CUtils/hashmap_typed.h"
#include "LogLevel.h"

/* LoadKAM.h : format of bytecode files */
/* This module loads a KAM module into the code segment, being */
/* a flat memory area containing KAM instructions.             */

/* Bytecode file: */
/*   beginning of file ---> header
 	      offset 0 ---> code block
			    import environment mapping relative addresses 
				    to those labels that need be resolved
			    export environment mapping labels 
					to relative addresses
	   end of file --->
*/

// Comment out the following line to disable caching of leaf-bytecode (for SMLserver)
#define CODE_CACHE 1

#define FILE_NOT_FOUND (-1)
#define TRUNCATED_FILE (-2)
#define BAD_MAGIC_NUM (-3)

// Labels
typedef struct {
  unsigned long id;
  char base;
} Label;
typedef Label* label;

// ServerState
typedef struct {
  void *aux;
  void (*report) (enum reportLevel level, char *data, void *aux);
} Serverstate;
typedef Serverstate* serverstate;

/* Compared to Moscow ML, we put the various information in front of the file. */

struct exec_header {
  unsigned long code_size;           /* Size of the code block (in bytes) */
  label main_lab_opt;                /* Optional main label; (0,"") is NONE */
  unsigned long import_size_code;    /* Number of code import entries */
  unsigned long import_size_data;    /* Number of data import entries */
  unsigned long export_size_code;    /* Number of code export entries */
  unsigned long export_size_data;    /* Number of code export entries */
  unsigned long magic;               /* A magic number */
};

#define HEADER_SIZE sizeof(struct exec_header)

/* Magic number for this release: "K001" */
#define EXEC_MAGIC 0x4b303031   

/* The type of loaded KAM code - each instruction takes 
 * up one word (i.e., a long) but we use a pointer to a 
 * char to locate the code...
 */
typedef unsigned char * bytecode_t;


/* ----------------------------------------------------------
 * Support for HashTables mapping strings to loaded bytecode
 *
 * The following type definition is for holding elements of 
 * the mapping from strings (file names) to loaded byte code.
 * See hashmap_typed.h
 * ---------------------------------------------------------- */

#ifdef CODE_CACHE
DECLARE_HASHMAP(strToCodeMap,char*,bytecode_t)
void strToCodeMapInsert(strToCodeMap m, char* s, bytecode_t code);
bytecode_t strToCodeMapLookup(strToCodeMap m, char* s);
strToCodeMap strToCodeMapClear(strToCodeMap m);
#endif


/* --------------------------------------------------
 * Support for HashTables mapping labels to absolute addresses
 *
 * The following type definition is for holding elements 
 * of the mapping from labels to resolved absolute addresses.
 * See hashmap_typed.h
 * -------------------------------------------------- */

DECLARE_HASHMAP(labelMap,label,unsigned long)
void labelMapInsert(labelMap labelMap, label label, unsigned long address);
labelMap labelMapNew(void);
unsigned long labelMapLookup(labelMap labelMap, label label);
void labelMapFree(labelMap labelMap);

typedef struct longList {
  unsigned long elem;        /* the element */
  struct longList * next;    /* the remainder of the list; terminated 
			      * with a NULL pointer */
} LongList;
void longListFree(LongList* longList);

typedef struct {
  labelMap codeMap;          /* Mapping code labels to absolute addresses */
  labelMap dataMap;          /* Mapping data labels to relative addresses 
			      * with respect to a data segment */
  LongList* codeList;        /* Addresses of all malloc'ed 
			      * code elements; used for freeing memory 
			      * occupied by interpreter. */
  LongList* exeList;         /* Labels for those program units that need be 
			      * initialized by running some code. */
#ifdef CODE_CACHE
  strToCodeMap codeCache;    /* Caching support for loaded leafs. */
#endif
  unsigned long data_size;   /* Accumulated size (in entries) of data segment */
} Interp;

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/


/* Create a new interpreter */
Interp *interpNew(void);

/* Extend an interpreter by loading a bytecode file */
int interpLoadExtend(Interp* interp, char* file,serverstate ss);

/* Load a bytecode file and run it, then release the loaded code;
 * later we can provide a version of this function that caches the
 * loaded code. */
ssize_t interpLoadRun(Interp* interp, char* file, char** errorStr, serverstate ss, ssize_t *result);

/* Run an interpreter */ 
ssize_t interpRun(Interp* interp, bytecode_t extra_code, char** errorStr, serverstate ss);

/* Free all loaded code */
void interpClear(Interp* interp);

/* Initialize global code fragments */
void resolveGlobalCodeFragments(void);

#endif /* LOADKAM_H */
