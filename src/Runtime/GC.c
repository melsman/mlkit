/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include "Flags.h"
#include "Tagging.h"
#include "Region.h"
#include "String.h"
#include "CommandLine.h"
#include "Table.h"
#include "Exception.h"
#include "Profiling.h"

int time_to_gc = 0; 
unsigned int stack_bot_gc = 0;
unsigned int stack_top_gc;
unsigned int rp_from_space = 0;
unsigned int to_space_old = 0;
unsigned int alloc_period = 0;
unsigned int alloc_total = 0;
unsigned int gc_total = 0;
double FRAG_sum = 0.0;


int *data_lab_ptr = NULL; /* pointer at exported data labels part of the root-set. */
int num_gc = 0;

int doing_gc = 0;
int raised_exn_interupt = 0;
int raised_exn_overflow = 0;

/* This implementation assumes a down growing stack (e.g., X86). */
#define NUM_REGS 8

/* Layout of stack:

          Reg31
          Reg30
            |
          Reg01
          Reg00
       Return Address
          FD end
            |
          FD begin
      Return Address
          FD end
            |
          FD begin
      Return Address    pointing at value HEX: FFFFFFFF (no more FDs*) */

/* Layout of FD in the code:

         FrameMapN
             |
         FrameMap0
         FrameSize
         FrameMark (for debug)
  ReturnLabel: */

#ifdef ENABLE_GC 

Klump *from_space_begin, *from_space_end;

/*******************/
/* PRETTY PRINTING */
/*******************/
static void pw(char *s,unsigned int tag) {
  int idx;
  
  printf("%s(%x) is ",s,tag);
  for (idx=0;idx<32;idx++) {
    if (tag & 0x80000000)
      printf("1");
    else
      printf("0");
    tag = tag << 1;
  }
  printf("\n");
  return;
}

static void print(unsigned int *value) {
  char str[50];
  unsigned int val;

  if (((unsigned int)value) & 01) {
    strcpy(str,"INTEGER");
    val = (unsigned int)value;
  }
  else {
    switch (val_tag_kind_const(value)) {
    case TAG_RECORD:       strcpy(str,"TAG_RECORD"); break;
    case TAG_RECORD_CONST: strcpy(str,"TAG_RECORD_CONST"); break;
    case TAG_STRING:       strcpy(str,"TAG_STRING"); break;
    case TAG_STRING_CONST: strcpy(str,"TAG_STRING_CONST"); break;
    case TAG_CON0:         strcpy(str,"TAG_CON0"); break;
    case TAG_CON0_CONST:   strcpy(str,"TAG_CON0_CONST"); break;
    case TAG_CON1:         strcpy(str,"TAG_CON1"); break;
    case TAG_CON1_CONST:   strcpy(str,"TAG_CON1_CONST"); break;
    case TAG_REF:          strcpy(str,"TAG_REF"); break;
    case TAG_REF_CONST:    strcpy(str,"TAG_REF_CONST"); break;
    case TAG_TABLE:        strcpy(str,"TAG_TABLE"); break;
    case TAG_TABLE_CONST:  strcpy(str,"TAG_TABLE_CONST"); break;
    default: {
      pw("print.Can't recognize tag. ", *value);
      die("GC.print: can't recognize tag");
    }
    }
    val = *value;
  }
  pw("Tag: ", val);
  return;
}

/*#define copy_words(from,to,w) (memcpy((to),(from),4*(w)))*/
inline static void copy_words(unsigned int *from,unsigned int *to,int num) {
  int i;
  for (i=0;i<num;i++) 
    *(to+i) = *(from+i);
  return;
}

/*******************************/
/* SCAN STACK INFINITE REGIONS */
/*******************************/
unsigned int **scan_stack = NULL;
#define INIT_STACK_SIZE_W 1024
int size_scan_stack;
int scan_sp;

inline static void init_scan_stack() {
  if (scan_stack == NULL) {
    scan_stack = (unsigned int **) realloc((void *)scan_stack, INIT_STACK_SIZE_W*4);
    if (scan_stack == NULL)
      die("GC.init_scan_stack: Unable to allocate scan_stack");
    size_scan_stack = INIT_STACK_SIZE_W;
  }
  scan_sp = 0;
  return;
}

#define is_scan_stack_empty() (scan_sp == 0)

inline static void push_scan_stack(unsigned int *ptr) {
  scan_stack[scan_sp] = ptr;
  scan_sp++;
  if (scan_sp >= size_scan_stack) {
    size_scan_stack *= 2;
    scan_stack = (unsigned int **) realloc((void *)scan_stack, size_scan_stack*4);
    if (scan_stack == NULL)
      die("GC.push_scan_stack: Unable to increase scan_stack");
  }
  return;
}

inline static unsigned int *pop_scan_stack() {
  if (scan_sp < 1) 
    die("GC.pop_scan_stack: scan_sp below stack bot.");
  scan_sp--;
  return scan_stack[scan_sp];
}

/*********************************/
/* SCAN CONTAINER FINITE REGIONS */
/*********************************/
unsigned int **scan_container = NULL;
#define INIT_CONTAINER_SIZE_W 1024
int size_scan_container;
int container_alloc;
int container_scan;

inline static void init_scan_container() {
  if (scan_container == NULL) {
    scan_container = (unsigned int **) realloc((void *)scan_container, INIT_CONTAINER_SIZE_W*4);
    if (scan_container == NULL)
      die("GC.init_scan_container: Unable to allocate scan_container");
    size_scan_container = INIT_CONTAINER_SIZE_W;
  }
  container_alloc = 0;
  container_scan = 0;
  return;
}

#define is_scan_container_empty() (container_scan == container_alloc)

inline static void check_scan_container(unsigned int *ptr) {
  int i;
  for (i=0; i<container_alloc; i++)
    if (scan_container[i] == ptr)
      die("Error, scan_container not unique\n");
}

inline static void push_scan_container(unsigned int *ptr) {
  scan_container[container_alloc] = ptr;
  container_alloc++;
  if (container_alloc >= size_scan_container) {
    size_scan_container *= 2;
    scan_container = (unsigned int **) realloc((void *)scan_container, size_scan_container*4);
    if (scan_container == NULL)
      die("GC.push_scan_container: Unable to increase scan_container");
  }
  return;
}

inline static unsigned int *pop_scan_container() {
  unsigned int *v;
  //if (container_scan >= container_alloc) 
  //  die("GC.pop_scan_container: container_scan higher than container_alloc.");
  v = scan_container[container_scan];
  container_scan++;
  return v;
}

inline static void clear_scan_container() {
  int i;
  for (i=0; i<container_alloc; i++)
    *scan_container[i] = clear_tag_const(*scan_container[i]);
}

/* We mark all region pages such that we can distinguish them from to-space */
/* region pages by setting a bit in the next k.n pointer.                   */
static void mk_from_space() {
  Ro *rp;
  Klump *rp_tmp;

  #ifdef PROFILING
    int j;
  #endif

  from_space_begin = NULL;
  from_space_end = (((Klump *)TOP_REGION->b)-1); /* Points at last region page */

  for(rp=TOP_REGION;rp!=NULL;rp=rp->p) {
    #ifdef PROFILING
      // Similar to resetRegion in Region.c
      j = NoOfPagesInRegion(rp);
      noOfPages -= j;
      profTabDecrNoOfPages(rp->regionId, j);
      allocNowInf -= rp->allocNow;
      profTabDecrAllocNow(rp->regionId, rp->allocNow);
      allocProfNowInf -= rp->allocProfNow;
      rp->allocNow = 0;
      rp->allocProfNow = 0;
    #endif

    /* Move Region Pages To From--Space */
    (((Klump *)rp->b)-1)->k.n = from_space_begin;
    from_space_begin = rp->fp;

    /* Allocate New Region Page */
    rp->fp = NULL;
    alloc_new_block(rp);
  }

  /* Calculate size of from space */
  rp_from_space = 0;
  for (rp_tmp=from_space_begin;rp_tmp;rp_tmp=rp_tmp->k.n)
    rp_from_space ++;
  return;
}

#define is_stack_allocated(obj_ptr) (((obj_ptr) <= stack_bot_gc) && (((obj_ptr) >= stack_top_gc)))
#define is_scalar(obj_ptr)          ((obj_ptr) & 01)
#define is_constant(x)              ((x) & 0x20)         /* Bit 6 is the constant bit */
#define is_forward_ptr(x)           (((x) & 0x03) == 0)  /* Bit 0 and 1 must be zero */
#define clear_forward_ptr(x)        (x)
#define tag_forward_ptr(x)          (x)

/*#define val_tag_kind_and_const(x)   ((x) & 0x3F)      */    /* Least 6 significant bits  */

#define get_rp_header(x)            ((x) & 0xFFFFFC00)   /* We assume region pages of size 1Kb and aligned */

#define set_status_SOME(rd)          (rd->p = (Ro *)(((unsigned int)rd->p) | 0x01))       /* We use the previous pointer to hold the status bit. */
#define set_status_NONE(rd)          (rd->p = (Ro *)(((unsigned int)rd->p) & 0xFFFFFFFE))
#define is_status_NONE(rd)           ((((unsigned int)(rd->p)) & 0x01) == 0)

/*************/
/* DEBUGGING */
/*************/
static void check_rp_in_from_space(Klump *rp) {
  int ok = 0;
  Klump *rp_tmp;

  for (rp_tmp=from_space_begin;rp_tmp;rp_tmp=((Klump *)(((unsigned int)rp_tmp->k.n) & 0xFFFFFFFE)))
    if (rp_tmp == rp) ok = 1;
  
  if (!ok)
    die("check_rp_in_from_space: rp not in from_space");

  return;
}

static void check_ptr_to_forward(unsigned int *obj_ptr) {
  if (!(is_forward_ptr(*obj_ptr))) 
    die("check_ptr_to_forward: ptr is not a forward pointer");

  return;
}

/* -----------------------------------------------------
 * Find allocated bytes in from space; for measurements
 * ----------------------------------------------------- */

static unsigned int *allocated_bytes_table_fragment(Tree *treep, int *allocated_bytes ) {
  int i;
  unsigned int *scan_ptr;

  scan_ptr = (unsigned int*)&(treep->prim_table);
  scan_ptr += ALLOCATABLE_WORDS_IN_PRIM_ARRAY;
  (*allocated_bytes) += (8 + 4*ALLOCATABLE_WORDS_IN_PRIM_ARRAY);
  
  /* Scan rest of the fragments in the order they are allocated! */
  if (treep->child[1]) {
    if (treep->child[0])
      allocated_bytes_table_fragment(treep->child[0], allocated_bytes);
    return allocated_bytes_table_fragment(treep->child[1], allocated_bytes);
  }
  if (treep->child[0]) 
    return allocated_bytes_table_fragment(treep->child[0], allocated_bytes);
  return scan_ptr;
}

static int allocated_bytes_in_region(Ro *rd) 
{
  unsigned int * scan_ptr;
  Klump*rp;
  int allocated_bytes = 0;

  rp = rd->fp;
  scan_ptr = rp->k.i;
  
  while (((int *)scan_ptr) != rd->a) {
    rp = (Klump *)get_rp_header((unsigned int)scan_ptr);
#if PROFILING
    scan_ptr += sizeObjectDesc;
#endif
    switch (val_tag_kind_const(scan_ptr)) {
    case TAG_STRING: {
      /* Do Not GC the content of a string but adjust scan_ptr to after the LAST string fragment */
      /* We know that no objects are allocated in between string fragments.                      */
      StringDesc* str;
      StringFragment *fragPtr, *lastFragPtr;
      int sizeWords;
      str = (StringDesc *)scan_ptr;
      allocated_bytes += 4;
      for (fragPtr=&(str->sf);fragPtr;fragPtr=fragPtr->n) {
	lastFragPtr = fragPtr;
	allocated_bytes += (8 + fragPtr->fragmentSize);   // fragmentSize is in bytes
	rp = (Klump *)get_rp_header((unsigned int)fragPtr);
      }
      sizeWords = (((lastFragPtr->fragmentSize) % 4) ? 
		   ((lastFragPtr->fragmentSize) / 4)+1 : ((lastFragPtr->fragmentSize) / 4));
      scan_ptr = ((unsigned int*)(lastFragPtr+1))+sizeWords;
      break;
    }
    case TAG_TABLE: {
      Table* table = (Table *)scan_ptr;
      allocated_bytes += 4;
      scan_ptr = allocated_bytes_table_fragment(&(table->tree), &allocated_bytes);
	  /* -4 is a necessary and safe hack!                                  */
	  /* necessary: because scan_ptr may point after the region page       */
	  /*            if the table fragment occupies the entire region page. */
	  /* safe: because the region page header is 8 bytes, so even if the   */
	  /*       region page is empty, scan_ptr will remain inside the       */
	  /*       region page. 2001-02-15, Niels                              */
      rp = (Klump *)get_rp_header((unsigned int)scan_ptr-4);
      break;
    }
    case TAG_RECORD: {
      int sizeWords = get_record_size(*scan_ptr); /* Size excludes descriptor */
      scan_ptr += (1 + sizeWords);
      allocated_bytes += (4 + 4*sizeWords);
      break;
    }
    case TAG_CON0: {
      scan_ptr++;
      allocated_bytes += 4;
      break;
    }
    case TAG_CON1: {
      scan_ptr+=2;
      allocated_bytes += 8;
      break;
    }
    case TAG_REF: {
      scan_ptr+=2;
      allocated_bytes += 8;
      break;
    }
    default: {
      pw("*scan_ptr: ",*scan_ptr);
      printf("scan_ptr: %x, rd->a: %x, diff(scan_ptr,rd->a): %d, rp: %x, diff(scan_ptr,rp): %x\n",
	     scan_ptr,
	     rd->a,
	     ((int *)rd->a-(int *)scan_ptr),
	     rp,
	     (int *)scan_ptr-(int *)rp);
      die("do_scan_stack: unrecognised object descriptor by scan_ptr");
      break;
    }
    }
    /* Are we at end of region page or is the region page full, then go to next region page. */
    if ((((int *)scan_ptr) != rd->a) &&
	((((int *)scan_ptr) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) ||
	 (*((int *)scan_ptr) == notPP))) {
      scan_ptr = ((unsigned int*) (rp->k.n))+HEADER_WORDS_IN_REGION_PAGE;
    }
  }
  return allocated_bytes;
}

static int allocated_bytes_in_regions(void) {
  int n = 0;
  Ro* rp;
  for (rp=TOP_REGION; rp != NULL; rp = rp->p)
    {
      n += allocated_bytes_in_region(rp);
    }
  return n;
}

/********************/
/* OBJECT FUNCTIONS */
/********************/
/* Returns the size including the descriptor. */
inline static int get_size_obj(unsigned int *obj_ptr) {
  int size;

  /* The object must contain a descriptor at offset 0 */
  switch (val_tag_kind_const(obj_ptr)) {
  case TAG_RECORD: {
    size = get_record_size(*obj_ptr)+1;
    break;
  }
  case TAG_CON1:
  case TAG_REF: {
    size = 2;
    break;
  }
  case TAG_CON0: {
    size = 1;
    break;
  }
  default: {
    pw("Tag: ", *obj_ptr);
    print(obj_ptr);
    die("GC.get_size_obj: can't recognize tag");
    break;
  }
  }
  return size;
}

inline  unsigned int *copy_val(unsigned int *obj_ptr,Ro *rd) {
  int size;
  unsigned int *new_obj_ptr;
  StringDesc *res;
  StringFragment *fragPtr;

#ifdef PROFILING
  int pPoint, oSize;
  pPoint = (((ObjectDesc *)(obj_ptr))-1)->atId;
  oSize  = (((ObjectDesc *)(obj_ptr))-1)->size;
#endif /*PROFILING*/

  /* The object must contain a descriptor at offset 0 */
  switch (val_tag_kind_const(obj_ptr)) {
  case TAG_STRING: {
    #ifdef PROFILING
      res = allocStringProfiling((int)rd,get_string_size(*obj_ptr), pPoint);
    #else
      res = allocString((int)rd,get_string_size(*obj_ptr));
    #endif
    fragPtr = &(res->sf);
    copyString((StringDesc *)obj_ptr, &fragPtr, (char *) (res+1));
    new_obj_ptr = (unsigned int*)res;
    *obj_ptr = tag_forward_ptr((unsigned int)new_obj_ptr);  /* Install Forward Pointer */
    break;
  }
  case TAG_TABLE: {
    #ifdef PROFILING
      new_obj_ptr = (unsigned int*)copy_tableProf((int)rd,(int)obj_ptr, pPoint);
    #else
      new_obj_ptr = (unsigned int*)copy_table((int)rd,(int)obj_ptr);
    #endif
    *obj_ptr = tag_forward_ptr((unsigned int)new_obj_ptr); /* Install Forward Pointer */
    break;
  }
  default: {
    size = get_size_obj(obj_ptr);         /* size includes descriptor. */
    #ifdef PROFILING
      new_obj_ptr = allocProfiling((int)rd,size, pPoint);
    #else
      new_obj_ptr = alloc((int)rd,size);
    #endif
    copy_words(obj_ptr,new_obj_ptr,size);
    *obj_ptr = tag_forward_ptr((unsigned int)new_obj_ptr);  /* Install Forward Pointer */
    break;
  }
  }

  return new_obj_ptr;
}

/* We get a pointer to an old object (obj_ptr) and return a pointer to */
/* a new object (new_obj_ptr) and installs a forward pointer in the    */
/* old object.                                                         */
inline static unsigned int *copy_obj(unsigned int *obj_ptr) {
  Klump *rp;
  Ro *rd;
  unsigned int *new_obj_ptr;

  /* Get Region Page and Region Descriptor */
  rp = (Klump *)get_rp_header((unsigned int)obj_ptr);
  rd = rp->k.r;
  new_obj_ptr = copy_val(obj_ptr,rd);
  if (is_status_NONE(rd)) {
#ifdef PROFILING 
    push_scan_stack(new_obj_ptr - sizeObjectDesc);
#else
    push_scan_stack(new_obj_ptr);
#endif
    set_status_SOME(rd);
  }
  return new_obj_ptr;
}

inline static unsigned int gc_obj(unsigned int obj) {
  unsigned int *obj_ptr;
  if (is_scalar(obj)) return obj;

  /* obj is a pointer */
  obj_ptr = (unsigned int *)obj;
  if (is_forward_ptr(*obj_ptr)) /* The value has already been copied */
    return clear_forward_ptr(*obj_ptr);             

  /* At this time, the object must contain a descriptor. is_forward_ptr must go first! */
  if (is_constant(*obj_ptr)) return obj; /* A constant (e.g., string) is not moved */

  if (is_stack_allocated(obj)) { /* A stack allocated object is not moved. Note obj=obj_ptr except for the C-type! */
    *obj_ptr = set_tag_const(*obj_ptr); /* Set const bit temporarily */
    push_scan_container(obj_ptr);
    return obj;
  }

  return (unsigned int)(copy_obj(obj_ptr));  /* We have an unvisited object */
}

static unsigned int *scan_table_fragment(Tree *treep) {
  int i;
  unsigned int *scan_ptr;

  scan_ptr = (unsigned int*)&(treep->prim_table);
  for (i=0;i<ALLOCATABLE_WORDS_IN_PRIM_ARRAY;i++) {
    *scan_ptr = gc_obj(*scan_ptr);
    scan_ptr++;
  }
  
  /* Scan rest of the fragments in the order they are allocated! */
  if (treep->child[1]) {
    if (treep->child[0])
      scan_table_fragment(treep->child[0]);
    return scan_table_fragment(treep->child[1]);
  }
  if (treep->child[0]) 
    return scan_table_fragment(treep->child[0]);
  return scan_ptr;
}

static void do_scan_stack() {
  unsigned int *scan_ptr;
  Klump *rp;
  Ro *rd;
  StringDesc *str;
  Table *table;
  StringFragment *fragPtr, *lastFragPtr;
  int sizeWords;
  int num_to_skip;
  int remaining;

  /* Run Through The Scan Stack and the Container */
  while (!((is_scan_stack_empty()) && (is_scan_container_empty()))) {
    /* Run Through The Container - FINITE REGIONS */
    while (!(is_scan_container_empty())) {
      scan_ptr = pop_scan_container();

      /* All finite objects are temporarily annotated as constants. */
      /* We therefore use val_tag_kind and not val_tag_kind_const   */
      switch (val_tag_kind(scan_ptr)) { 
	case TAG_RECORD: {
	  sizeWords = get_record_size(*scan_ptr); /* Size excludes descriptor */
	  num_to_skip = get_record_skip(*scan_ptr);
	  //if (num_to_skip > sizeWords)
	  //  die("gc: num_to_skip larger than size");
	  scan_ptr = scan_ptr + 1 + num_to_skip;
	  remaining = sizeWords-num_to_skip;
	  while (remaining) {
	    *scan_ptr = gc_obj(*scan_ptr);
	    scan_ptr++;
	    remaining--;
	  }
	  break;
	}
	case TAG_CON0: {
	  /* Is a constant */
	  scan_ptr++;
	  break;
	}
	case TAG_CON1: {
	  scan_ptr++;
	  *scan_ptr = gc_obj(*scan_ptr);
	  scan_ptr++;
	  break;
	}
	case TAG_REF: {
	  scan_ptr++;
	  *scan_ptr = gc_obj(*scan_ptr);
	  scan_ptr++;
	  break;
	}
	default: {
	  pw("*scan_ptr: ",(unsigned int)(*scan_ptr));
#ifdef PROFILING
	  printf("size: %d, atId: %d, regionId: %d, finPtr: %x\n",
		 *(scan_ptr-1), *(scan_ptr-2), *(scan_ptr-3), *(scan_ptr-4));
#endif
	  die("do_scan_stack_finite: unrecognised object descriptor by scan_ptr (or string/table that cannot be allocated in a finite region)");
	  break;
	}
	}
    }

    while (!(is_scan_stack_empty())) {
      scan_ptr = pop_scan_stack();
      /* Get Region Page and Region Descriptor */
      rp = (Klump *)get_rp_header((unsigned int)scan_ptr);
      rd = rp->k.r;
      while (((int *)scan_ptr) != rd->a) {
	rp = (Klump *)get_rp_header((unsigned int)scan_ptr);
#if PROFILING
	scan_ptr += sizeObjectDesc;
#endif
	switch (val_tag_kind_const(scan_ptr)) {
	case TAG_STRING: {
	  /* Do Not GC the content of a string but adjust scan_ptr to after the LAST string fragment */
	  /* We know that no objects are allocated in between string fragments.                      */
	  str = (StringDesc *)scan_ptr;
	  for (fragPtr=&(str->sf);fragPtr;fragPtr=fragPtr->n) {
	    lastFragPtr = fragPtr;
	    rp = (Klump *)get_rp_header((unsigned int)fragPtr);
	  }
	  sizeWords = (((lastFragPtr->fragmentSize) % 4) ? 
		       ((lastFragPtr->fragmentSize) / 4)+1 : ((lastFragPtr->fragmentSize) / 4));
	  scan_ptr = ((unsigned int*)(lastFragPtr+1))+sizeWords;
	  break;
	}
	case TAG_TABLE: {
	  table = (Table *)scan_ptr;
	  scan_ptr = scan_table_fragment(&(table->tree));
	  /* -4 is a necessary and safe hack!                                  */
	  /* necessary: because scan_ptr may point after the region page       */
	  /*            if the table fragment occupies the entire region page. */
	  /* safe: because the region page header is 8 bytes, so even if the   */
	  /*       region page is empty, scan_ptr will remain inside the       */
	  /*       region page. 2001-02-15, Niels                              */
	  rp = (Klump *)get_rp_header((unsigned int)scan_ptr-4);
	  break;
	}
	case TAG_RECORD: {
	  sizeWords = get_record_size(*scan_ptr); /* Size excludes descriptor */
	  num_to_skip = get_record_skip(*scan_ptr);
	  
	  scan_ptr = scan_ptr + 1 + num_to_skip;
	  remaining = sizeWords-num_to_skip;
	  while (remaining) {
	    *scan_ptr = gc_obj(*scan_ptr);
	    scan_ptr++;
	    remaining--;
	  }
	  break;
	}
	case TAG_CON0: {
	  /* Is a constant */
	  scan_ptr++;
	  break;
	}
	case TAG_CON1: {
	  scan_ptr++;
	  *scan_ptr = gc_obj(*scan_ptr);
	  scan_ptr++;
	  break;
	}
	case TAG_REF: {
	  scan_ptr++;
	  *scan_ptr = gc_obj(*scan_ptr);
	  scan_ptr++;
	  break;
	}
	default: {
	  pw("*scan_ptr: ",*scan_ptr);
	  printf("scan_ptr: %x, rd->a: %x, diff(scan_ptr,rd->a): %d, rp: %x, diff(scan_ptr,rp): %x\n",
		 scan_ptr,
		 rd->a,
		 ((int *)rd->a-(int *)scan_ptr),
		 rp,
		 (int *)scan_ptr-(int *)rp);
	  die("do_scan_stack: unrecognised object descriptor by scan_ptr");
	  break;
	}
	}
	/* Are we at end of region page or is the region page full, then go to next region page. */
	if ((((int *)scan_ptr) != rd->a) &&
	    ((((int *)scan_ptr) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) ||
	     (*((int *)scan_ptr) == notPP))) {
	  scan_ptr = ((unsigned int*) (rp->k.n))+HEADER_WORDS_IN_REGION_PAGE;
	}
      }
      set_status_NONE(rd);
    }
  }
  return;
}

#define predSPDef(sp,n) ((sp)=(sp)+(n))
#define succSPDef(sp) (sp--)
 
void gc(unsigned int **sp, unsigned int reg_map) {
  extern Klump* freelist;
  extern int rp_to_space;
  unsigned int **sp_ptr;
  unsigned int *fd_ptr;
  unsigned int fd_size, fd_mark, fd_offset_to_return;
  unsigned int *w_ptr;
  int w_idx;
  unsigned int w;
  int offset;
  unsigned int *value_ptr;
  unsigned int **d_lab_ptr;
  int num_d_labs;
  int size_rcf, size_ccf, size_spilled_region_args;
  Klump *rp_tmp;
  extern int rp_used;
  extern int rp_total;
  double ratio;
  double to_allocate;
  struct rusage rusage_begin;
  struct rusage rusage_end;
  unsigned int size_from_space = 0;
  unsigned int alloc_period_save;

  doing_gc = 1; // Mutex on the garbage collector

  getrusage(RUSAGE_SELF, &rusage_begin);

  stack_top_gc = (unsigned int)sp;

  if ((int)(stack_bot_gc - stack_top_gc) < 0)
    {
      die("gc: stack_top_gc larger than stack_bot_gc on down-growing stack");
    }

  num_gc++;

  if ( verbose_gc ) 
    {
      fprintf(stderr,"[GC#%d", num_gc); fflush(stdout);
      size_from_space = allocated_bytes_in_regions();
      alloc_period_save = alloc_period;
      alloc_period = 0;
    }

  rp_to_space = 0;

  /* Initialize the scan stack (for Infinite Regions) and the container (for Finite Regions) */
  init_scan_stack();
  init_scan_container();

  mk_from_space();

  /* Search for live registers */
  sp_ptr = sp;
  w = reg_map;
  for (offset=0;offset<NUM_REGS;offset++) {
    if (w & 01) {
      value_ptr = ((unsigned int*)sp_ptr) + NUM_REGS - 1 - offset;  /* Address of live cell */
      *value_ptr = gc_obj(*value_ptr);
    }
    w = w >> 1;
  }

  /* Do spilled arguments and results to current function */
  sp_ptr = sp;
  sp_ptr = sp_ptr + NUM_REGS; /* Points at size_spilled_region_args. */

  size_spilled_region_args = *((int *)sp_ptr);
  predSPDef(sp_ptr,1); /* sp_ptr points at size_rcf */
  size_rcf = *((int *)sp_ptr);
  predSPDef(sp_ptr,1); /* sp_ptr points at size_ccf */
  size_ccf = *((int *)sp_ptr);
  predSPDef(sp_ptr,1); /* sp_ptr points at last arg. to current function. */

  /* All arguments to current function are live - except for region arguments. */
  for (offset=0;offset<size_ccf;offset++) {
    value_ptr = ((unsigned int*)sp_ptr);
    predSPDef(sp_ptr,1);
    if (offset >= size_spilled_region_args)
      *value_ptr = gc_obj(*value_ptr);    
  }

  /* sp_ptr points at first return address.                           */  
  /* Below the return address we may have slots for spilled results - */
  /* they are not live at this point!                                 */

  /* Search for Frame Descriptors (FD). A FD cover */
  /*   - function frame                            */
  /*   - spilled arguments                         */
  /*   - return address                            */
  /*   - spilled results                           */

  fd_ptr = *sp_ptr;
  fd_mark = *(fd_ptr-1);
  fd_offset_to_return = *(fd_ptr-2);
  fd_size = *(fd_ptr-3);
  predSPDef(sp_ptr,size_rcf);
  /* sp_ptr points at first address before FD */
  while (fd_size != 0xFFFFFFFF) {
    w_ptr = fd_ptr-4;

    /* Find RootSet in FD */
    if (fd_size)  /* It may happen fd_size = 0 in which case w_ptr points at arbitrary address. */
      w = *w_ptr;
    w_idx = 0;
    for(offset=0;offset<fd_size;offset++) {
      if (w & 01) {
	value_ptr = ((unsigned int*)sp_ptr) + fd_size - offset;
	*value_ptr = gc_obj(*value_ptr); 
      }
      w = w >> 1;
      w_idx++;
      if ((w_idx == 32) & (offset+1 < fd_size)) { /* Again, w_ptr may point arbitrarily if we are done. */
	w_ptr--;
	w = *w_ptr;
	w_idx = 0;
      }
    }

    sp_ptr = sp_ptr + fd_offset_to_return + 1; /* Points at next return address. */
    fd_ptr = *sp_ptr;
    fd_mark = *(fd_ptr-1);
    fd_offset_to_return = *(fd_ptr-2);
    fd_size = *(fd_ptr-3);
    predSPDef(sp_ptr,size_rcf);
  }

  /* Search for data labels; they are part of the root-set. */
  num_d_labs = *data_lab_ptr; /* Number of data labels */
  for (offset=1;offset<=num_d_labs;offset++) {
    value_ptr = *(((unsigned int**)data_lab_ptr) + offset);
    *value_ptr = gc_obj(*value_ptr);
  }

  do_scan_stack();

  /* We Are Done And Must Insert from-space Into The FreeList */
  from_space_end->k.n = freelist;
  freelist = from_space_begin;

  /* We Reset All Constant Bits in the Container - FINITE REGIONS */
  clear_scan_container();

  rp_used = rp_to_space;
  ratio = (((double)rp_total)/(double)rp_used);
  to_allocate = heap_to_live_ratio*((double)rp_used)-((double)rp_total);
  if (ratio < heap_to_live_ratio) {
    callSbrkArg(((int)(to_allocate))+REGION_PAGE_BAG_SIZE);
  }

  if (verbose_gc) 
    {
      double RI = 0.0, GC = 0.0, FRAG = 0.0;
      unsigned int size_to_space;

      size_to_space = alloc_period;
      alloc_period = alloc_period_save;
      alloc_total += alloc_period;
      gc_total += (size_from_space - size_to_space);

      getrusage(RUSAGE_SELF, &rusage_end);

      fprintf(stderr,"(%dms)",
	     ((rusage_end.ru_utime.tv_sec+rusage_end.ru_stime.tv_sec)*1000 + 
	      (rusage_end.ru_utime.tv_usec+rusage_end.ru_stime.tv_usec)/1000) - 
	     ((rusage_begin.ru_utime.tv_sec+rusage_begin.ru_stime.tv_sec)*1000 + 
	      (rusage_begin.ru_utime.tv_usec+rusage_begin.ru_stime.tv_usec)/1000));
      /*
      fprintf(stderr, " rp_total: %d\n", rp_total);
      fprintf(stderr, " size_scan_stack: %d\n", (size_scan_stack*4) / 1024);
      fprintf(stderr, " size_scan_container: %d\n", (size_scan_container*4) / 1024);
      fprintf(stderr, " to_space_old: %d\n", to_space_old);
      fprintf(stderr, " alloc_period: %d\n", alloc_period);
      fprintf(stderr, " alloc_period_save: %d\n", alloc_period_save);
      fprintf(stderr, " size_from_space: %d\n", size_from_space);
      fprintf(stderr, " size_to_space: %d\n", size_to_space);
      */

      if ( num_gc != 1 )
	{
	  RI = 100.0 * ( ((double)(to_space_old + alloc_period - size_from_space)) /
			 ((double)(to_space_old+alloc_period-size_to_space)));
	  
	  GC = 100.0 * ( ((double)(size_from_space - size_to_space)) /
			 ((double)(to_space_old+alloc_period-size_to_space)));

	  FRAG = 100.0 * (((double)(size_from_space/1024))/ (double)rp_from_space);
	  FRAG_sum = FRAG_sum + FRAG;
	}

      fprintf(stderr," %dkb(%dkb) (%4.1f%) --> %dkb(%dkb), free-list: %dkb, alloc: %dkb, RI: %4.1f%, GC: %4.1f%]\n",
	      size_from_space / 1024,
	      rp_from_space,
	      FRAG,
	      size_to_space / 1024, 
	      rp_used,
	      size_free_list(),
	      alloc_period / 1024,
	      RI, GC);	     
      
      to_space_old = size_to_space;
      alloc_period = 0;
    }

  time_to_gc = 0; 
  doing_gc = 0; // Mutex on the garbage collector
  
  if (raised_exn_interupt) 
    raise_exn((int)&exn_INTERRUPT);
  if (raised_exn_overflow)
    raise_exn((int)&exn_OVERFLOW);
  return;
}

#endif /* ENABLE_GC */
