/* LoadKAM.c */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "LoadKAM.h"
#include "Runtime.h"
#include "Region.h"
#include "KamInsts.h"
#include "Stack.h"
#include "HeapCache.h"
#include "Exception.h"
#include "Interp.h"

#ifdef DEBUG
#define debug(Arg) Arg
#else
#define debug(Arg) {}
#endif

/* -----------------------------------------------------
 * String Map
 * ----------------------------------------------------- */

#if ( THREADS && CODE_CACHE )

static unsigned int 
hashString(const char *str)
{
  unsigned int hashval;
  int i;

  hashval = 0;
  i = strlen(str);
  while ( i > 0 )
    {
      hashval = ((hashval << 1) + str[--i]) & STRING_MAP_HASH_TABLE_SIZE;
    }
  return hashval;
}

StringMap* 
stringMapInsert(StringMap* stringMap, 
		char * name, bytecode_t code)
{
  int index;
  StringMapHashList* newElem;
  char* name_copy;

  newElem = (StringMapHashList*)malloc(sizeof(StringMapHashList));
  if ( newElem <= (StringMapHashList*)0 ) {
    die("stringMapInsert error");
  }

  name_copy = (char*)malloc(strlen(name)+1);
  strcpy(name_copy,name);

  newElem->name = name_copy;
  newElem->code = code;

  index = hashString(name);
  newElem->next = stringMap[index];

  stringMap[index] = newElem;
  return stringMap;              /* We want to allow for hash-table 
				  * resizing in the future */
}  


/* Create and allocate space for a new StringMapHashTable */

void 
stringMapZero(StringMap* stringMap)
{
  int i;
  for ( i = 0 ; i < STRING_MAP_HASH_TABLE_SIZE ; i++ ) 
    {
      stringMap[i] = NULL;
    }
}

StringMap* 
stringMapNew(void) 
{
  StringMap* stringMap;

  if ( (stringMap = (StringMap*) malloc(sizeof(unsigned long) * 
					STRING_MAP_HASH_TABLE_SIZE) ) <= 0 ) 
    {
      die("Unable to allocate memory for StringMapHashTable");
    }

  stringMapZero(stringMap);
  return stringMap;
}

// lookup bytecode in a stringMapHashTable; returns 0 on failure

bytecode_t
stringMapLookup(StringMap* stringMap, char* name)
{
  StringMapHashList* p;
  for ( p = stringMap[hashString(name)]; p != NULL ; p = p->next ) 
    {
      if ( strcmp(p->name,name) == 0 ) 
	{
	  return p->code;
	}
    }
  return NULL;
}

void
stringMapClear(StringMap* stringMap)
{
  int i;
  StringMapHashList *p, *n;

  for ( i = 0 ; i < STRING_MAP_HASH_TABLE_SIZE ; i++ ) 
    {
      p = stringMap[i];
      while ( p )
	{
	  free(p->code);
	  free(p->name);
	  n = p->next;
	  free(p);
	  p = n;
	}
      stringMap[i] = 0;
    }
}

#endif

/* -----------------------------------------------------
 * Label Map
 * ----------------------------------------------------- */

LabelMap* 
labelMapInsert(LabelMap* labelMap, 
	       unsigned long label, 
	       unsigned long absAddr) 
{
  int index = (int)(label & LABEL_MAP_HASH_TABLE_SIZE);
  LabelMapHashList* newElem;

  newElem = (LabelMapHashList*)malloc(sizeof(LabelMapHashList));
  if ( newElem <= (LabelMapHashList*)0 ) {
    die("labelMapInsert error");
  }

  newElem->label = label;
  newElem->address = absAddr;
  newElem->next = labelMap[index];

  labelMap[index] = newElem;
  return labelMap;              /* We want to allow for hash-table 
				 * resizing in the future */
}  


// Create and allocate space for a new LabelMapHashTable

void 
labelMapZero(LabelMap* labelMap)
{
  int i;
  for (i = 0 ; i < LABEL_MAP_HASH_TABLE_SIZE ; i++) {
    labelMap[i]=0;
  }
}

LabelMap* 
labelMapNew(void) 
{
  LabelMap* labelMap;

  if ( (labelMap = (LabelMap*) malloc(sizeof(unsigned long) * 
				      LABEL_MAP_HASH_TABLE_SIZE) ) <= 0 ) 
    {
      die("Unable to allocate memory for LabelMapHashTable");
    }

  labelMapZero(labelMap);
  return labelMap;
}

// lookup a label in a labelMapHashTable; returns 0 on failure

unsigned long 
labelMapLookup(LabelMap* labelMap, unsigned long label) 
{
  int index = (int)(label & LABEL_MAP_HASH_TABLE_SIZE);
  LabelMapHashList* p;
  for ( p = labelMap[index] ; p != NULL; p = p->next ) 
    {
      if ( p->label == label ) 
	{
	  return p->address;
	}
    }
  return (unsigned long)0;
}

/* Global regions 0-3 and global exception 
 * constructors 4-8 are allocated in data segment */
#define INTERP_INITIAL_DATASIZE 9

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
  interp->codeCache = stringMapNew();
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
      free(longList->elem);
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

void
labelMapHashListFree(LabelMapHashList* labelMapHashList)
{
  LabelMapHashList* l;
  while ( labelMapHashList )
    {
      l = labelMapHashList->next;
      free(labelMapHashList);
      labelMapHashList = l;
    }
}

void labelMapClear(LabelMap* labelMap)
{
  int i;
  for (i = 0 ; i < LABEL_MAP_HASH_TABLE_SIZE ; i++) {
    labelMapHashListFree(labelMap[i]);
    labelMap[i]=0;
  }
}

// read_long: read a long from a buffer

static unsigned long 
read_unsigned_long(unsigned char *buffer) 
{
  unsigned long l;
  l = *(unsigned long*)buffer;
  /*  printf("Reading long %x\n", l); */
  return l;
}


// For debugging
static void 
out_buffer(unsigned char* buffer, int s) 
{
  int i;
  for (i=0; i < s; i++) {
    printf("%x ", buffer[i]);
  }
  printf("\n");
}

static int 
my_read(FILE* fd, unsigned char* buffer, int s) 
{
  int i;
  int c;
  for (i=0; i < s; i++) {
    if ( (c = fgetc(fd)) == EOF) {
      return i;
    } 
    *buffer++ = (unsigned char)c;
  }
  return i;
}
      
// For debugging
static void  
print_exec_header(struct exec_header* exec_header) 
{
  printf("Header:\n\
             code_size: %d\n\
             main_lab_opt: %d\n\
             import_size_code: %d\n\
             import_size_data: %d\n\
             export_size_code: %d\n\
             export_size_data: %d\n\
             magic: %x\n", 
	 exec_header->code_size,
	 exec_header->main_lab_opt, 
	 exec_header->import_size_code,
	 exec_header->import_size_data,
	 exec_header->export_size_code,
	 exec_header->export_size_data,
	 exec_header->magic);
}
  

// read_exec_header: Leaves fd at the beginning of the code 
// segment on success

static int 
read_exec_header(FILE* fd, struct exec_header * exec_header) 
{
  unsigned char buffer[HEADER_SIZE]; 

  if ( my_read(fd, buffer, HEADER_SIZE) < HEADER_SIZE ) {
    return TRUNCATED_FILE; 
  }
  debug(printf("The header is:\n"));
  debug(out_buffer(buffer, HEADER_SIZE));
  exec_header->code_size = read_unsigned_long(buffer); 
  exec_header->main_lab_opt = read_unsigned_long(buffer+4); 
  exec_header->import_size_code = read_unsigned_long(buffer+8);
  exec_header->import_size_data = read_unsigned_long(buffer+12);
  exec_header->export_size_code = read_unsigned_long(buffer+16);
  exec_header->export_size_data = read_unsigned_long(buffer+20);
  exec_header->magic = read_unsigned_long(buffer+24);
  if ( exec_header->magic == EXEC_MAGIC ) {
    return 0; 
  } else {
    return BAD_MAGIC_NUM;
  }
}


/* attempt_open: Leaves fd at the beginning of the code segment on success 
 * remember to close the returned file descriptor when the file has been
 * read. 
 */

static FILE*
attempt_open(char* name, struct exec_header* exec_header) 
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
    switch ((int)res) {
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
  return fd;
}

// memo: maybe merge with my_read above!
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
resolveCodeImports(LabelMap* labelMap, 
		   FILE* fd,
		   unsigned long import_size,    // size is in entries
		   bytecode_t start_code) 
{	   
  unsigned long relAddr;
  unsigned long label;
  unsigned char buffer[PAIR_SIZE];
  bytecode_t absTargetAddr;
  bytecode_t absSourceAddr;

  while ( import_size > 0 ) {
    if ( my_read(fd, buffer, PAIR_SIZE) < PAIR_SIZE ) {
      return TRUNCATED_FILE;
    }
    relAddr = read_unsigned_long(buffer); 
    label = read_unsigned_long(buffer + 4); 

    debug(printf("Importing relAddr = %d (0x%x), label = %d (0x%x) \n", 
		 relAddr, relAddr, label, label));

    if ( (absTargetAddr = (bytecode_t)labelMapLookup(labelMap, label)) == 0 ) 
      {
	return -4;
      }
    absSourceAddr = start_code + relAddr;
    * (unsigned long*)absSourceAddr = 
      (unsigned long)(absTargetAddr - absSourceAddr);
    import_size --;
  }
  return 0;
}

static int 
resolveDataImports(LabelMap* labelMap, 
		   FILE* fd,
		   unsigned long import_size,    // size is in entries
		   bytecode_t start_code) 
{	   
  unsigned long relAddr, dsAddr;
  unsigned long label;
  unsigned char buffer[PAIR_SIZE];

  while ( import_size > 0 ) {
    if ( my_read(fd, buffer, PAIR_SIZE) < PAIR_SIZE ) {
      return TRUNCATED_FILE;
    }
    relAddr = read_unsigned_long(buffer); 
    label = read_unsigned_long(buffer + 4); 

    debug(printf("Importing relAddr = %d (0x%x), label = %d (0x%x) \n", 
		 relAddr, relAddr, label, label));

    if ( (dsAddr = labelMapLookup(labelMap, label)) == 0 ) {
      return -4;
    }
    * (unsigned long*)(start_code + relAddr) = dsAddr;
    import_size --;
  }
  return 0;
}

/* for each entry (label, relAddr) in the file extend the
 * labelMap with the entry (label, start_code + relAddr)
 */
static LabelMap* 
addCodeExports(LabelMap* labelMap, 
	       FILE* fd, 
	       unsigned long export_size,     // size is in entries
	       bytecode_t start_code) 
{	   
  unsigned long label;
  unsigned long relAddr;
  bytecode_t absAddr;
  unsigned char buffer[PAIR_SIZE];

  while ( export_size > 0 ) {
    if ( my_read(fd, buffer, PAIR_SIZE) < PAIR_SIZE ) {
      return (LabelMap*)TRUNCATED_FILE;
    }
    label = read_unsigned_long(buffer); 
    relAddr = read_unsigned_long(buffer + 4); 
    absAddr = start_code + relAddr;

    debug(printf ("Reading export entry, label = %d (0x%x), relAddr = %d (0x%x), absAddr = %d (0x%x)\n", 
		  label, label, relAddr, relAddr, absAddr, absAddr));

    labelMap = labelMapInsert(labelMap, label, (unsigned long)absAddr);
    export_size --;
  }
  return labelMap;
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
  unsigned long label, relAddr, newDsAddr;
  unsigned char buffer[PAIR_SIZE];

  while ( export_size > 0 ) {
    if ( my_read(fd, buffer, PAIR_SIZE) < PAIR_SIZE ) {
      return TRUNCATED_FILE;
    }
    label = read_unsigned_long(buffer); 
    // relAddr is the relative address of `StoreData lab' address in bytecode
    relAddr = read_unsigned_long(buffer + 4);   
    // newDsAddr is the new data segment address (relative to ds-register)
    newDsAddr = interp->data_size++;            

    debug(printf("Export label = %d (0x%x), relAddr = %d (0x%x), newDsAddr = %d\n", 
		 label, label, relAddr, relAddr, newDsAddr));

    * (unsigned long*)(start_code + relAddr) = newDsAddr;
    interp->dataMap = labelMapInsert(interp->dataMap, label, newDsAddr);
    export_size --;
  }
  return 0;
}


static bytecode_t 
interpLoad(Interp* interp, char* file, FILE* fd, 
	   struct exec_header* exec_header_ptr) 
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
interpLoadExtend(Interp* interp, char* file) 
{
  FILE *fd;
  struct exec_header exec_header;
  bytecode_t start_code;

  fd = attempt_open(file, &exec_header);
  start_code = interpLoad(interp, file, fd, &exec_header);

  debug(printf("[Extend hash table with code exports]\n"));
  if ( (interp->codeMap = addCodeExports(interp->codeMap, fd, 
					 exec_header.export_size_code, 
					 start_code)) < 0 ) 
    {
      die2("interpLoadExtend: Cannot resolve code exports for ", file);
    }

  debug(printf("[Extend hash table with data exports]\n"));
  if ( addDataExports(interp, fd, exec_header.export_size_data, 
		      start_code) < 0 ) 
    {
      die2("interpLoadExtend: Cannot resolve data exports for ", file);
    }
  
  fclose(fd);

  // extend the code list with the new code segment
  interp->codeList = listCons((unsigned long)start_code, interp->codeList);

  if ( exec_header.main_lab_opt ) {

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
  selectStackDef(1) = exname_counter++; \
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

int 
interpRun(Interp* interpreter, bytecode_t extra_code, char**errorStr) 
{
  unsigned long *ds, *sp, *exnPtr, *sp0;
  unsigned long exname_counter = 0;
  Heap* h;
  int res;
  LongList* p;
  Ro* topRegion = NULL;

  h = getHeap();
  if ( h->status == HSTAT_UNINITIALIZED )
    {
      (int*)ds = h->ds;
      sp = ds;

      // make room for data space on the stack
      debug(printf("DATASPACE ds = 0x%x\n", ds));
      sp += interpreter->data_size;
      debug(printf("STACK sp = 0x%x, datasize = %d\n", sp, interpreter->data_size));

      // Now, allocate global regions and store addresses in data segment
      // the indexes should be the same as those defined in Manager/Name.sml
      GLOBAL_REGION(0);   // rtype top, uses ds, modifies sp
      // GLOBAL_REGION(1);   // rtype bot
      GLOBAL_REGION(2);   // rtype pair
      GLOBAL_REGION(3);   // rtype string

      // Initialize primitive exceptions
      GLOBAL_EXCON(4,"Div");     // uses ds, modifies sp
      GLOBAL_EXCON(5,"Match");
      GLOBAL_EXCON(6,"Bind");
      GLOBAL_EXCON(7,"Overflow");
      GLOBAL_EXCON(8,"Interrupt");

      exn_OVERFLOW = (Exception*)**(unsigned long**)(ds+7);
      exn_INTERRUPT = (Exception*)**(unsigned long**)(ds+8);

      // Push global exception handler on the stack
      pushDef((unsigned long)exit_code);         // push return address on stack
      pushDef((unsigned long) 0);                // Dummy env for exit_code
      pushDef((unsigned long)global_exnhandler_closure); // push closure on stack (no env)
      pushDef(0);                                // no previous handler on stack

      exnPtr = sp - 1;                           // update exnPtr

      /* push address for exit-bytecode on the stack */
      debug(printf("Pushing exit-address %x on stack at sp = %x\n", 
		   (unsigned long)exit_code, sp));
      pushDef((unsigned long)exit_code);
      pushDef((unsigned long)0);

      sp0 = sp;

      // push all execution addresses on the stack
      for (p = interpreter->exeList; p ; p = p->next) {
	debug(printf("Pushing address %x on stack at sp = %x\n", 
		     (unsigned long)p->elem, sp));
	pushDef((unsigned long)p->elem);
	pushDef((unsigned long)0);
      }

      // start interpretation by interpreting the init_code
      res = interpCode(interpreter,sp,ds,exnPtr,&topRegion,errorStr,
		       exname_counter,(bytecode_t)init_code);
  
      if ( res >= 0 && extra_code )
	{
	  initializeHeap(h,(int*)sp0,(int*)exnPtr);
	}
      else 
	{
	  deleteHeap(h);
	  return res;
	}
    }
  
  // no exception raised by code so far

  // perhaps jump to the extra bytecode
  if ( extra_code ) {

    // fetch heap data
    (int*)sp = h->sp;
    (int*)ds = h->ds;
    (int*)exnPtr = h->exnPtr;
    topRegion = h->r3copy->r;

    touchHeap(h);

    res = interpCode(interpreter,sp,ds,exnPtr,&topRegion,errorStr,
		     exname_counter,(bytecode_t)extra_code);

    releaseHeap(h);
  }    

  return res;   // return whatever the interpreter returns
}

/* ------------------------------------------------------
 * interpLoadRun - load a bytecode file, run it, and release the
 * loaded code.  
 * ------------------------------------------------------ */

#if ( THREADS && CODE_CACHE )
extern void codeCacheMutexLock(void);
extern void codeCacheMutexUnlock(void);
extern void logLoading(char *file);
#endif

int 
interpLoadRun(Interp* interp, char* file, char** errorStr) 
{
  bytecode_t start_code;
  int res;

#if ( THREADS && CODE_CACHE )
  codeCacheMutexLock();
  start_code = stringMapLookup(interp->codeCache,file);  
  if ( start_code == NULL )
    {
#endif
      struct exec_header exec_header;
      FILE *fd = attempt_open(file, &exec_header);
      start_code = interpLoad(interp, file, fd, &exec_header);
      fclose(fd);
#if ( THREADS && CODE_CACHE )
      interp->codeCache = stringMapInsert(interp->codeCache,file,start_code);
      logLoading(file);
    }
  codeCacheMutexUnlock();
#endif

  /*
   *  Run the code by passing to the interpRun function the newly
   *  loaded bytecode as an extra parameter. 
   */

  res = interpRun(interp, start_code, errorStr);

#if !( THREADS && CODE_CACHE )
  free(start_code);
#endif

  return res;    // return whatever the bytecode interpreter returns
}

void
interpClear(Interp* interp)
{
  labelMapClear(interp->codeMap);    // hash table - don't nullify 
  labelMapClear(interp->dataMap);    // hash table - don't nullify 
#if ( THREADS && CODE_CACHE )
  stringMapClear(interp->codeCache); // hash table - don't nullify 
#endif
  longListFreeElem(interp->codeList);
  interp->codeList = NULL;
  longListFree(interp->exeList); // here we free only the list - not the 
  interp->exeList = NULL;        // elements, which have already been freed
  interp->data_size = INTERP_INITIAL_DATASIZE;
}

