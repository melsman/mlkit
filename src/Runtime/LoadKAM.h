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

/* Compared to Moscow ML, we put the various information in front of the file. */

struct exec_header {
  unsigned long code_size;           /* Size of the code block (in bytes) */
  unsigned long main_lab_opt;        /* Optional main label; 0 is NONE */
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

/* Support for HashTables mapping strings to loaded bytecode */

/* --------------------------------------------------
 * The following type definition is for 
 * holding elements of the mapping from strings 
 * (file names) to loaded byte code.
 * -------------------------------------------------- */

#ifdef CODE_CACHE
typedef struct stringMapHashList {
  char* name;                           /* string (file name) */
  bytecode_t code;                      /* loaded byte code */
  struct stringMapHashList * next;      /* next hashed element */
} StringMapHashList;

typedef StringMapHashList* StringMap;

/* Size of string map hash table in entries -- (2^n-1) */
#define STRING_MAP_HASH_TABLE_SIZE 511

#endif

/* Support for HashTables mapping labels to absolute addresses */

/* --------------------------------------------------
 * The following type definition is for 
 * holding elements of the mapping from labels to 
 * resolved absolute addresses.
 * -------------------------------------------------- */

typedef struct labelMapHashList {
  unsigned long label;                  /* label that is resolved */
  unsigned long address;                /* resolved absolute address */
  struct labelMapHashList * next;       /* next hashed element */
} LabelMapHashList;

typedef LabelMapHashList* LabelMap;

/* Size of label map hash table in entries -- (2^n-1) */
#define LABEL_MAP_HASH_TABLE_SIZE 4095

typedef struct longList {
  unsigned long elem;        /* the element */
  struct longList * next;    /* the remainder of the list; terminated 
			      * with a NULL pointer */
} LongList;

typedef struct {
  LabelMap* codeMap;         /* Mapping code labels to absolute addresses */
  LabelMap* dataMap;         /* Mapping data labels to relative addresses 
			      * with respect to a data segment */
  LongList* codeList;        /* Addresses of all malloc'ed 
			      * code elements; used for freeing memory 
			      * occupied by interpreter. */
  LongList* exeList;         /* Labels for those program units that need be 
			      * initialized by running some code. */
#ifdef CODE_CACHE
  StringMap* codeCache;      /* Caching support for loaded leafs. */
#endif
  unsigned long data_size;   /* Accumulated size (in entries) of data segment */
} Interp;

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

#ifdef CODE_CACHE

StringMap* stringMapInsert(StringMap* stringMap, 
			   char* s, 
			   bytecode_t code);

StringMap* stringMapNew(void);

bytecode_t stringMapLookup(StringMap* stringMap, char* s);

void stringMapClear(StringMap* stringMap);

#endif /*CODE_CACHE*/

LabelMap* labelMapInsert(LabelMap* labelMap, 
			 unsigned long label, 
			 unsigned long address);

LabelMap* labelMapNew(void);

unsigned long labelMapLookup(LabelMap* labelMap, 
			     unsigned long label);

void labelMapFree(LabelMap* labelMap);
void longListFree(LongList* longList);

/* Create a new interpreter */
Interp *interpNew(void);

/* Extend an interpreter by loading a bytecode file */
int interpLoadExtend(Interp* interp, char* file);

/* Load a bytecode file and run it, then release the loaded code;
 * later we can provide a version of this function that caches the
 * loaded code. */
int interpLoadRun(Interp* interp, char* file, char** errorStr);

/* Run an interpreter */ 
int interpRun(Interp* interp, bytecode_t extra_code, char** errorStr);

/* Free all loaded code */
void interpClear(Interp* interp);

/* Initialize global code fragments */
void resolveGlobalCodeFragments(void);
