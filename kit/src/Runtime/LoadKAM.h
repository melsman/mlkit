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

/* Size of label map hash table in entries */
#define LABEL_MAP_HASH_TABLE_SIZE 3881

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
  unsigned long data_size;   /* Accumulated size (in entries) of data segment */
} Interp;

/*----------------------------------------------------------------*
 *        Prototypes for external and internal functions.         *
 *----------------------------------------------------------------*/

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

