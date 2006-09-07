/* LoadKAM.c */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

#include "LoadKAM.h"
#include "Runtime.h"
#include "Region.h"
#include "KamInsts.h"
#include "Stack.h"
#include "HeapCache.h"
#include "Exception.h"
#include "Interp.h"
#include "../CUtils/hashmap_typed.h"

#if ( THREADS && CODE_CACHE )
#include <string.h>
#include "Locks.h"
#include "LogLevel.h"
#endif

#ifdef DEBUG
#define debug(Arg) Arg
#else
#define debug(Arg) {}
#endif

/* -----------------------------------------------------
 * String to Code Map
 * ----------------------------------------------------- */

static int
streq(char* s1,char* s2)
{
  if ( strcmp(s1,s2) == 0 )
    return 1;
  return 0;
}

#if ( THREADS && CODE_CACHE )
// extern void logMsg1(char* msg, void *serverState);

DEFINE_HASHMAP(strToCodeMap,char*,bytecode_t)

void
strToCodeMapInsert(strToCodeMap m, char* name, bytecode_t code)
{
  char* name_copy;
  name_copy = (char*)malloc(strlen(name)+1);
  if ( name_copy == 0 )
    {
      die("strToCodeMapInsert: cannot allocate memory for name");
    }
  strcpy(name_copy,name);
  strToCodeMap_upd(m,name_copy,code);
  return;
}

// lookup bytecode in a strToCodeMap; returns 0 on failure

bytecode_t
strToCodeMapLookup(strToCodeMap m, char* k)
{
  bytecode_t b;
  if ( strToCodeMap_find(m,k,&b) == hash_OK )
    return b;
  else
    return 0;
}

void
strToCodeMapClear_fn(char* k,bytecode_t code)
{
  free(k);
  free(code);
}

strToCodeMap
strToCodeMapClear(strToCodeMap m)
{
  strToCodeMap_apply(m,strToCodeMapClear_fn);
  strToCodeMap_drop(m);
  return new_strToCodeMap(charhashfunction,streq);
}

#endif

/* -----------------------------------------------------
 * Label Map
 * ----------------------------------------------------- */

DEFINE_HASHMAP(labelMap,label,unsigned long)

void 
labelMapInsert(labelMap m, 
	       label k,
	       unsigned long v) 
{
  labelMap_upd(m,k,v);
}

unsigned long
label_hash(label lab)
{
  unsigned long acc;
  acc = charhashfunction(&(lab->base));
  return acc + lab->id;
}

int
label_eq(label lab1,label lab2)
{
  if ( lab1->id == lab2->id && streq(&(lab1->base),&(lab2->base))) 
    return 1;
  else return 0;
}

labelMap 
labelMapNew(void) 
{
  return new_labelMap(label_hash,label_eq);
}

void
printLabelId(label lab,unsigned long id)
{
  printf(" Lab(%ld,%s) -> %ld\n",lab->id,&(lab->base),id);
}

void
labelMapPrint(labelMap m)
{
  printf("LabelMap = {\n");
  labelMap_apply(m,printLabelId);
  printf("}\n");
}

// lookup a label in a labelMapHashTable; returns 0 on failure

unsigned long 
labelMapLookup(labelMap m, label lab) 
{
  unsigned long res;
  if ( labelMap_find(m,lab,&res) == hash_OK )
    return res;
  else return 0;
}

void
free_label(label lab,unsigned long res)
{
  free(lab);
}

labelMap 
labelMapClear(labelMap m)
{
  labelMap_apply(m,free_label);
  labelMap_drop(m);
  return labelMapNew();
}

/* Global regions 0-6, global exception 
 * constructors 7-11 are allocated in data segment
 * and a garbage field located in 12 */
#define INTERP_INITIAL_DATASIZE 13

/* Create a new interpreter:
 *   - We could perhaps allocate the interpreter stack when 
 *     we first create an interpreter - this way, each interpreter
 *     thread could reuse its own stack! Now we malloc a new stack
 *     whenever a script is run.
 */
Interp*
interpNew(void) 
{
  Interp* interp;
  
  if ( (interp = (Interp*)malloc (sizeof(Interp))) <= 0 ) {
    die("Unable to allocate memory for interpreter");
  }

  interp->codeMap = labelMapNew();
  interp->dataMap = labelMapNew();
  interp->codeList = NULL;
  interp->exeList = NULL;
  interp->data_size = INTERP_INITIAL_DATASIZE;
#if ( THREADS && CODE_CACHE )
  interp->codeCache = new_strToCodeMap(charhashfunction,streq);
#endif
  /*  debug(printf("interpNew4\n")); */
  return interp;
}

LongList* 
listCons(unsigned long elem, LongList* longList) 
{
  LongList* longList2;

  if ( (longList2 = (LongList*) malloc (sizeof(LongList))) <= 0 ) {
    die("Unable to allocate memory for list");
  }
  longList2->next = longList;
  longList2->elem = elem;
  return longList2;
}

void
longListFreeElem(LongList* longList)
{
  LongList* l;
  while ( longList )
    {
      l = longList->next;
      free((void*)(longList->elem));
      free(longList);
      longList = l;
    }
}

void
longListFree(LongList* longList)
{
  LongList* l;
  while ( longList )
    {
      l = longList->next;
      free(longList);
      longList = l;
    }
}

// read_long: read a long from a buffer
#define READ_ERROR -1
#define READ_OK 0

static int
read_unsigned_long(FILE* fd, unsigned long* v_ptr) 
{
  unsigned char buffer[sizeof(unsigned long)];
  int i,c;

  for ( i = 0 ; i < sizeof(unsigned long) ; i++ ) 
    {
      if ( (c = fgetc(fd)) == EOF )
	return READ_ERROR;
      buffer[i] = (unsigned char)c;
  }
  *v_ptr = *(unsigned long*)buffer;
  return READ_OK;
}

static int
read_string_buf(FILE* fd,unsigned long n,char* buf)
{
  unsigned long i;
  int c;
  for ( i = 0 ; i < n ; i++ ) 
    {
      if ( (c = fgetc(fd)) == EOF)
	return READ_ERROR;
      buf[i] = (char)c;
    }
  buf[i] = 0;
  return READ_OK;
}

static int
skip_string_buf(FILE* fd,unsigned long n)
{
  unsigned long i;
  int c;
  for ( i = 0 ; i < n ; i++ ) 
  {
    if ( (c = fgetc(fd)) == EOF)
      return READ_ERROR;
  }
  return READ_OK;
}

// A label is layed out in the file as  |id;sz_str;chars| - no trailing zero
static int
read_label(FILE* fd, label* lab_ptr) 
{
  label lab;
  unsigned long id, str_sz;
  if ( read_unsigned_long(fd, &id) == READ_ERROR )
    return READ_ERROR;
  if ( read_unsigned_long(fd, &str_sz) == READ_ERROR )
    return READ_ERROR;
  lab = (label)malloc(str_sz + 1 + sizeof(long));
  if ( lab == 0 )
    die ("read_label: failed to allocate memory for label");
  lab->id = id;
  if ( read_string_buf(fd,str_sz,&(lab->base)) == READ_ERROR )
    {
      free(lab);
      return READ_ERROR;
    }
  debug(printf("read_label: id = %d; str_sz = %d; base = %s\n", id, str_sz, &(lab->base)));
  *lab_ptr = lab;
  return READ_OK;
}

// A label is layed out in the file as  |id;sz_str;chars| - no trailing zero
static int
skip_label(FILE* fd) 
{
  unsigned long str_sz;
  if ( read_unsigned_long(fd, &str_sz) == READ_ERROR )
    return READ_ERROR;
  if ( read_unsigned_long(fd, &str_sz) == READ_ERROR )
    return READ_ERROR;
  if ( skip_string_buf(fd,str_sz) == READ_ERROR )
    {
      return READ_ERROR;
    }
  debug(printf("skip_label: str_sz = %d\n", str_sz));
  return READ_OK;
}
/*
// For debugging
static void  
print_exec_header(struct exec_header* exec_header) 
{
  printf("Header:\n\
             code_size: %ld\n\
             main_lab: Lab(%ld,%s)\n\
             import_size_code: %ld\n\
             import_size_data: %ld\n\
             export_size_code: %ld\n\
             export_size_data: %ld\n\
             magic: %lx\n", 
	 exec_header->code_size,
	 exec_header->main_lab_opt->id,
	 &(exec_header->main_lab_opt->base),	 
	 exec_header->import_size_code,
	 exec_header->import_size_data,
	 exec_header->export_size_code,
	 exec_header->export_size_data,
	 exec_header->magic);
}
*/
// read_exec_header: Leaves fd at the beginning of the code 
// segment on success
static int 
read_exec_header(FILE* fd, struct exec_header * exec_header) 
{
  if ( read_unsigned_long(fd, &(exec_header->code_size)) == READ_ERROR
       || read_label(fd, &(exec_header->main_lab_opt)) == READ_ERROR
       || read_unsigned_long(fd, &(exec_header->import_size_code)) == READ_ERROR
       || read_unsigned_long(fd, &(exec_header->import_size_data)) == READ_ERROR
       || read_unsigned_long(fd, &(exec_header->export_size_code)) == READ_ERROR
       || read_unsigned_long(fd, &(exec_header->export_size_data)) == READ_ERROR
       || read_unsigned_long(fd, &(exec_header->magic)) == READ_ERROR )
    return TRUNCATED_FILE; 
  if ( exec_header->magic == EXEC_MAGIC ) 
    return 0; 
  else
    return BAD_MAGIC_NUM;
}


/* attempt_open: Leaves fd at the beginning of the code segment on success 
 * remember to close the returned file descriptor when the file has been
 * read. 
 */

static int
attempt_open(char* name, struct exec_header* exec_header, serverstate ss, FILE **result) 
{
  FILE *fd;
  int res;

  debug(printf("opening file %s\n", name));
  fd = fopen(name, "r");
  if ( fd == NULL ) {
    die2("attempt_open: fopen returns NULL when trying to open file ", name);
    exit(-1);
  }
  if ( (res = read_exec_header(fd, exec_header)) < 0 ) {
    switch (res) {
    case FILE_NOT_FOUND:
      die2("attempt_open: cannot find the file ", name);
      break;
    case TRUNCATED_FILE:
      die2("attempt_open: truncated file: ", name);
      break;
    case BAD_MAGIC_NUM:
      die2("attempt_open: bad magic number in the bytecode file ", name);
      break;
    }
    exit(-1);
  }
  *result = fd;
  return 0;
}

static int 
loadCode(FILE *fd, unsigned long n, bytecode_t ch) 
{
  int c;
  while (n > 0) { 
    if ( (c = fgetc(fd)) == EOF ) {
      return -1; 
    }
    *ch++ = (unsigned char)c;
    n--; 
  } 
  return 0;
}

/* for each entry (relAddr,label) in the file do
 *     *(start_code + relAddr) = labelMap[label]
 */

#define PAIR_SIZE (2*sizeof(long))

static int 
resolveCodeImports(labelMap labelMap, 
		   FILE* fd,
		   unsigned long import_size,    // size is in entries
		   bytecode_t start_code) 
{	   
  unsigned long relAddr;
  label label;
  bytecode_t absTargetAddr;
  bytecode_t absSourceAddr;

  while ( import_size > 0 ) {
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR
	 || read_label(fd, &label) == READ_ERROR )
      return TRUNCATED_FILE;

    debug(printf("Importing relAddr = %d (0x%x), label = %d (0x%x) \n", 
		 relAddr, relAddr, label, label));

    if ( (absTargetAddr = (bytecode_t)labelMapLookup(labelMap, label)) == 0 ) 
      {
	free(label);
	return -4;
      }
    free(label);
    absSourceAddr = start_code + relAddr;
    * (unsigned long*)absSourceAddr = 
      (unsigned long)(absTargetAddr - absSourceAddr);
    import_size --;
  }
  return 0;
}

static int 
resolveDataImports(labelMap labelMap, 
		   FILE* fd,
		   unsigned long import_size,    // size is in entries
		   bytecode_t start_code) 
{	   
  unsigned long relAddr, dsAddr;
  label lab;

  while ( import_size > 0 ) {
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR
	 || read_label(fd, &lab) == READ_ERROR )
      return TRUNCATED_FILE;

    debug(printf("Importing relAddr = %d (0x%x), label = %d (0x%x) \n", 
		 relAddr, relAddr, lab, lab));
    debug_writer4("Importing relAddr = %d (0x%x), label = %d (0x%x) \n", 
		 relAddr, relAddr, lab, lab);

    if ( (dsAddr = labelMapLookup(labelMap, lab)) == 0 ) {
      free(lab);
      return -4;
    }
    free(lab);
    * (unsigned long*)(start_code + relAddr) = dsAddr;
    import_size --;
  }
  return 0;
}

/* for each entry (label, relAddr) in the file extend the
 * labelMap with the entry (label, start_code + relAddr)
 */
static int
addCodeExports(labelMap m, 
	       FILE* fd, 
	       unsigned long export_size,     // size is in entries
	       bytecode_t start_code) 
{	   
  label lab;
  unsigned long relAddr;
  bytecode_t absAddr;

  while ( export_size > 0 ) {
    if ( read_label(fd, &lab) == READ_ERROR )
      return TRUNCATED_FILE;
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR )
      {
	free(lab);
	return TRUNCATED_FILE;
      }
    absAddr = start_code + relAddr;

    debug(printf ("Reading export entry, label = %d (0x%x), relAddr = %d (0x%x), absAddr = %d (0x%x)\n", 
		  lab, lab, relAddr, relAddr, absAddr, absAddr));

    labelMapInsert(m, lab, (unsigned long)absAddr);
    export_size --;
  }
  return 0;
}

static int
skipCodeExports(labelMap m, 
	       FILE* fd, 
	       unsigned long export_size)     // size is in entries
{	   
  unsigned long relAddr;

  while ( export_size > 0 )
  {
    if ( skip_label(fd) == READ_ERROR )
      return TRUNCATED_FILE;
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR )
    {
      return TRUNCATED_FILE;
    }
    export_size --;
  }
  return 0;
}

/* Read entries (lab, relAddr), where lab is a compile time label for
 * a slot in the data segment and relAddr is the place in the bytecode
 * where lab appears in a `StoreData lab' instruction. For each pair,
 * a new slot is allocated in the data segment (data_size is
 * incremented), then the `StoreData lab' instruction is modified, and
 * finally, the label is associated with the new offset in the hash
 * table that maps labels to offsets.  */

static int 
addDataExports(Interp* interp, 
	       FILE* fd, 
	       unsigned long export_size,  // size is in entries
	       bytecode_t start_code)     
{
  label lab;
  unsigned long relAddr, newDsAddr;

  while ( export_size > 0 ) {
    if ( read_label(fd, &lab) == READ_ERROR )
      return TRUNCATED_FILE;
    // relAddr is the relative address of `StoreData lab' address in bytecode
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR )
      {
	free(lab);
	return TRUNCATED_FILE;
      }
    // newDsAddr is the new data segment address (relative to ds-register)
    newDsAddr = interp->data_size++;            

    debug(printf("Export label = %d (0x%x), relAddr = %d (0x%x), newDsAddr = %d\n", 
		 lab, lab, relAddr, relAddr, newDsAddr));
    debug_writer5("Export label = %d (0x%x), relAddr = %d (0x%x), newDsAddr = %d\n", 
		 lab, lab, relAddr, relAddr, newDsAddr);

    * (unsigned long*)(start_code + relAddr) = newDsAddr;
    labelMapInsert(interp->dataMap, lab, newDsAddr);
    export_size --;
  }
  return 0;
}

/* alias data export labels with the garbage pointer */
static int 
garbageDataExports(Interp* interp, 
	       FILE* fd, 
	       unsigned long export_size,  // size is in entries
	       bytecode_t start_code)     
{
  unsigned long relAddr;

  while ( export_size > 0 )
  {
    if ( skip_label(fd) == READ_ERROR )
      return TRUNCATED_FILE;
    // relAddr is the relative address of `StoreData lab' address in bytecode
    if ( read_unsigned_long(fd, &relAddr) == READ_ERROR )
    {
      return TRUNCATED_FILE;
    }

    debug(printf("Garbage export label relAddr = %d (0x%x), newDsAddr = %d\n", 
                   relAddr, relAddr, INTERP_INITIAL_DATASIZE - 1));
    debug_writer3("Garbage export label relAddr = %d (0x%x), newDsAddr = %d\n", 
		               relAddr, relAddr, INTERP_INITIAL_DATASIZE -1);

    * (unsigned long*)(start_code + relAddr) = INTERP_INITIAL_DATASIZE - 1;
    export_size --;
  }
  return 0;
}


static bytecode_t 
interpLoad(Interp* interp, char* file, FILE* fd, 
	   struct exec_header* exec_header_ptr, serverstate ss) 
{
  bytecode_t start_code;

  debug(print_exec_header(exec_header_ptr));

  // allocate space for loaded code
  if ( (start_code = (bytecode_t) malloc(exec_header_ptr->code_size)) == 0 ) 
    {
      die2("interpLoad: Cannot allocate start_code for ", file);
    }

  debug(printf("[Load code segment]\n"));
  if ( loadCode(fd, exec_header_ptr->code_size, start_code) < 0 ) {
    die2("interpLoad: Cannot load code for ", file);
  }

  debug(printf("[Resolving code imports]\n"));
  /* Now, resolve the labels in the import table - 
   * first the code labels then the data labels */
  if ( resolveCodeImports(interp->codeMap, fd, 
			  exec_header_ptr->import_size_code, 
			  start_code) < 0 ) 
    {
      die2("interpLoad: Cannot resolve code imports for ", file);
    }

  debug(printf("[Resolving data imports]\n"));
  if ( resolveDataImports(interp->dataMap, fd, 
			  exec_header_ptr->import_size_data, 
			  start_code) < 0 ) 
    {
      die2("interpLoad: Cannot resolve data imports for ", file);
    }

#ifdef LAB_THREADED
  debug(printf("[Resolving instructions]\n"));
  if ( (exec_header_ptr->code_size % 4) != 0 ) {
    die2("interpLoad: Code size not a multiple of 4 for ", file);
  }
  resolveCode(start_code, exec_header_ptr->code_size / 4);
#endif

  return start_code;
}

/* ------------------------------------------------------------  
 *  interpLoadExtend - load bytecode file and extend interpreter
 *  with information about the identifiers that this bytecode file
 *  declares.
 * ------------------------------------------------------------ */
int 
interpLoadExtend(Interp* interp, char* file, serverstate ss) 
{
  FILE *fd;
  struct exec_header exec_header;
  int tmp;
  bytecode_t start_code;

  tmp = attempt_open(file, &exec_header, ss, &fd);

  start_code = interpLoad(interp, file, fd, &exec_header, ss);

  debug(printf("[Extend hash table with code exports]\n"));
  if ( addCodeExports(interp->codeMap, fd, 
		      exec_header.export_size_code, 
		      start_code) < 0 ) 
    {
      die2("interpLoadExtend: Cannot extract code exports for ", file);
    }

  debug(printf("[Extend hash table with data exports]\n"));
  if ( addDataExports(interp, fd, exec_header.export_size_data, 
		      start_code) < 0 ) 
    {
      die2("interpLoadExtend: Cannot extract data exports for ", file);
    }
  
  fclose(fd);

  // extend the code list with the new code segment
  interp->codeList = listCons((unsigned long)start_code, interp->codeList);

  if ( exec_header.main_lab_opt->id == 0 
       && strcmp(&(exec_header.main_lab_opt->base),"") == 0 )
    return 0;
  else
    {
      unsigned long absAddr;       /* We need to look up this 
				    * label in the code export map */
      if ( (absAddr = labelMapLookup(interp->codeMap, 
				     exec_header.main_lab_opt)) == 0 ) 
	{
	  die2("interpLoadExtend: Failed to lookup absolute main-label address for ", 
	       file);
	}
      interp->exeList = listCons(absAddr, interp->exeList);
    }
  return 0;
}


/* allocate global region and store 
 * address in data segment at address n */

#define GLOBAL_REGION(n) { \
  debug(printf("Allocating global region %d at sp=%x\n",(n),sp)); \
  *(ds + (n)) = (int) allocateRegion((Ro*) sp, &topRegion); \
  offsetSP(sizeRo); \
}

#define GLOBAL_EXCON(X, NAME) { \
  debug(printf("Allocating global excon (%d) at sp=%x\n", (X), sp)); \
  selectStackDef(0) = (unsigned long)(sp + 1); \
  selectStackDef(1) = exnCnt++; \
  selectStackDef(2) = (unsigned long)convertStringToML((Region)*(ds + 2), (NAME)); \
  *(ds + (X)) = (unsigned long)sp; \
  offsetSP(3); \
}

/*
 * interpRun - Run the interpreter passed as argument; the second
 * argument extra_code is put on the stack for execution in case it is
 * not NULL (used by the interpLoadRun function). 
 *
 * Returns: whatever the interpreter returns 
 */

#define INIT_CODE_SIZE 3
static unsigned long init_code[INIT_CODE_SIZE] = {
  RETURN,0,1
};


// We don't actually deallocate the global regions, because in
// SMLserver, the global regions are restored to an initial state so
// that the result of executing library code can be reused for future
// requests. When used for non-SMLserver purposes, the memory for the
// entire process is collected by the OS, thus it is ok not to
// deallocate the global regions in this case.
 
#define EXIT_CODE_SIZE 2
static unsigned long exit_code[EXIT_CODE_SIZE] = {
  //  ENDREGION_INF,               // deallocate the four global regions
  //  ENDREGION_INF,
  //  ENDREGION_INF,
  //  ENDREGION_INF,
  IMMED_INT0,                  // success
  HALT                         // make the interpreter return the 
                               // content of the accumulator
};

static unsigned long global_exnhandler_closure[1] = {
  0   // place holder for code pointer
};
  
#define GLOBAL_EXNHANDLER_CODE_SIZE 2
static unsigned long global_exnhandler_code[GLOBAL_EXNHANDLER_CODE_SIZE] = {
  GLOBAL_EXN_HANDLER_REPORT,   // sets acc to error (-1 or -2)
  //  POP_N, 3, 
  //  ENDREGION_INF,               // deallocate the four global regions
  //  ENDREGION_INF,
  //  ENDREGION_INF,
  //  ENDREGION_INF,
  HALT                         // make the interpreter return the 
                               // content of the accumulator
};

/* resolveGlobalCodeFragments is called from main_interp and 
 * SMLserver's Ns_ModuleInit function; should be called exactly once
 * before execution of any bytecode. */
void 
resolveGlobalCodeFragments(void)
{
  resolveCode((bytecode_t)init_code, INIT_CODE_SIZE);
  resolveCode((bytecode_t)exit_code, EXIT_CODE_SIZE);
  resolveCode((bytecode_t)global_exnhandler_code, 
	      GLOBAL_EXNHANDLER_CODE_SIZE);
  // create closure (no env)
  * global_exnhandler_closure = (unsigned long)global_exnhandler_code;    
}

ssize_t 
interpRun(Interp* interpreter, bytecode_t extra_code, char**errorStr, serverstate ss) 
{
  uintptr_t *ds, *sp, *exnPtr, *sp0;
  size_t exnCnt = 0;
  Heap* h;
  ssize_t res = 0;
  LongList* p;
  Ro* topRegion = NULL;

//  debug_writer1("interpRun getHeap %d\n", 0);
  h = getHeap(ss);
  if ( h->status == HSTAT_UNINITIALIZED )
    {
      debug_writer1("interpRun %d init heap\n", 0);
      ds = h->ds;
      sp = ds;

      // make room for data space on the stack
      debug(printf("DATASPACE ds = 0x%x\n", ds));
      sp += interpreter->data_size;
      debug(printf("STACK sp = 0x%x, datasize = %d\n", sp, interpreter->data_size));
//      debug_writer3("interpRun data_size = 0x%x - sp = 0x%x - ds = 0x%x\n", interpreter->data_size, (int) sp, (int) ds);

      // Now, allocate global regions and store addresses in data segment
      // the indexes should be the same as those defined in Manager/Name.sml
      GLOBAL_REGION(0);   // rtype top, uses ds, modifies sp
      // GLOBAL_REGION(1);   // rtype bot
      GLOBAL_REGION(2);   // rtype pair
      GLOBAL_REGION(3);   // rtype string
      GLOBAL_REGION(4);   // rtype array
      GLOBAL_REGION(5);   // rtype ref
      GLOBAL_REGION(6);   // rtype triple

      // Initialize primitive exceptions
      GLOBAL_EXCON(7,"Div");     // uses ds, modifies sp
      GLOBAL_EXCON(8,"Match");
      GLOBAL_EXCON(9,"Bind");
      GLOBAL_EXCON(10,"Overflow");
      GLOBAL_EXCON(11,"Interrupt");
      // 12 is used for garbage

      exn_DIV = (Exception*)**(size_t **)(ds+7);
      exn_MATCH = (Exception*)**(size_t **)(ds+8);
      exn_BIND = (Exception*)**(size_t **)(ds+9);
      exn_OVERFLOW = (Exception*)**(size_t **)(ds+10);
      exn_INTERRUPT = (Exception*)**(size_t **)(ds+11);

      // Push global exception handler on the stack
      pushDef((size_t)exit_code);         // push return address on stack
      pushDef((size_t) 0);                // Dummy env for exit_code
      pushDef((size_t)global_exnhandler_closure); // push closure on stack (no env)
      pushDef(0);                                // no previous handler on stack

      exnPtr = sp - 1;                           // update exnPtr

      /* push address for exit-bytecode on the stack */
      debug(printf("Pushing exit-address %x on stack at sp = %x\n", 
		   (size_t)exit_code, sp));
      pushDef((size_t)exit_code);
      pushDef((size_t)0);

      sp0 = sp;

      // push all execution addresses on the stack
      for (p = interpreter->exeList; p ; p = p->next) {
	debug(printf("Pushing address %x on stack at sp = %x\n", 
		     (size_t)p->elem, sp));
	pushDef((size_t)p->elem);
	pushDef((size_t)0);
      }

      // start interpretation by interpreting the init_code
//      debug_writer1("interpRun %d interpCode init_code\n", 0);
   //   int tmp;
   //   debug_file_as(tmp, debug_file);
   //   debug_file_as(debug_file,-1);
      res = interpCode(interpreter,sp,ds,exnPtr,&topRegion,errorStr,
		       &exnCnt,(bytecode_t)init_code, ss);
  
   //   debug_file_as(debug_file,tmp);
//      debug_writer4("initializeHeap sp = 0x%x - sp0 = 0x%x - ds = 0x%x - topRegion = 0x%x\n", (int) sp, (int) sp0, (int) ds, (int) topRegion);

      if ( res >= 0 && extra_code )
      {
//      debug_writer1("interpRun %d initializeHeap\n", 0);
        initializeHeap(h,sp0,exnPtr, exnCnt, ss);
      }
      else 
      {
#ifdef THREADS
        (*ss->report) (NOTICE, "Exception raised during execution of library code", ss->aux);
#endif
        deleteHeap(h);
        return res;
      }
    }
  
  // no exception raised by code so far; perhaps jump to the extra bytecode
  if ( extra_code ) {

    // fetch heap data
    sp = (uintptr_t *)(h->sp);
    ds = (uintptr_t *)(h->ds);
    exnPtr = (uintptr_t *)(h->exnPtr);
    exnCnt = h->exnCnt;
    topRegion = h->r6copy->r;

    touchHeap(h,ss);

      debug_writer1("interpRun %d interpCode extra_code\n", 0);
    res = interpCode(interpreter,sp,ds,exnPtr,&topRegion,errorStr,
		     &exnCnt,(bytecode_t)extra_code, ss);

      debug_writer1("interpRun %d releaseHeap\n", 0);
    releaseHeap(h,ss);
  }    

  return res;   // return whatever the interpreter returns
}

/* ------------------------------------------------------
 * interpLoadRun - load a bytecode file, run it, and release the
 * loaded code.  
 * Returns 0 on success and 1 on error
 * ------------------------------------------------------ */

ssize_t 
interpLoadRun(Interp* interp, char* file, char** errorStr, serverstate ss, ssize_t *res) 
{
  bytecode_t start_code;
  FILE *fd;
  int tmp;
  debug_writer1("interpLoadRun %d starting\n", 0);

#if ( THREADS && CODE_CACHE )
  debug_writer1("interpLoadRun %d lock\n", 0);
  LOCK_LOCK(CODECACHEMUTEX);
  debug_writer1("interpLoadRun %d find code\n", 0);
  start_code = strToCodeMapLookup(interp->codeCache,file);  
  if ( start_code == NULL )
    {
#endif
      struct exec_header exec_header;
  debug_writer1("interpLoadRun %d open file\n", 0);
      tmp = attempt_open(file, &exec_header, ss, &fd);
  debug_writer1("interpLoadRun %d load\n", 0);
      start_code = interpLoad(interp, file, fd, &exec_header, ss);
      debug(printf("[skip code exports]\n"));
      if ( skipCodeExports(interp->codeMap, fd, 
              exec_header.export_size_code) < 0 ) 
      {
        die2("interpLoadRun: Cannot extract code exports for ", file);
      }

      debug(printf("[alias data exports labels with garbage field]\n"));
      if ( garbageDataExports(interp, fd, exec_header.export_size_data, 
            start_code) < 0 ) 
      {
        die2("interpLoadRun: Cannot extract data exports for ", file);
      }
  debug_writer1("interpLoadRun %d close file\n", 0);
      fclose(fd); // as we only read files we don't care about the return value
#if ( THREADS && CODE_CACHE )
  debug_writer1("interpLoadRun %d insert code\n", 0);
      strToCodeMapInsert(interp->codeCache,file,start_code);
      (*ss->report) (INFO, file,ss->aux);
    }
  debug_writer1("interpLoadRun %d unlock\n", 0);
  LOCK_UNLOCK(CODECACHEMUTEX);
#endif

  /*
   *  Run the code by passing to the interpRun function the newly
   *  loaded bytecode as an extra parameter. 
   */

  debug_writer1("interpLoadRun %d run\n", 0);
  *res = interpRun(interp, start_code, errorStr, ss);

#if !( THREADS && CODE_CACHE )
  debug_writer1("interpLoadRun %d free\n", 0);
  free(start_code);
#endif

  debug_writer1("interpLoadRun %d done\n", 0);
  return 0;    // return whatever the bytecode interpreter returns
}

void
interpClear(Interp* interp)
{
  interp->codeMap = labelMapClear(interp->codeMap);
  interp->dataMap = labelMapClear(interp->dataMap);
#if ( THREADS && CODE_CACHE )
  interp->codeCache = strToCodeMapClear(interp->codeCache);
#endif
  longListFreeElem(interp->codeList);
  interp->codeList = NULL;
  longListFree(interp->exeList); // here we free only the list - not the 
  interp->exeList = NULL;        // elements, which have already been freed
  interp->data_size = INTERP_INITIAL_DATASIZE;
}

