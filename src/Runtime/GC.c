/*----------------------------------------------------------------*
 *                     Garbage Collection                         *
 *----------------------------------------------------------------*/

#ifdef ENABLE_GC 

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
#include "GC.h"

int time_to_gc = 0;                   // set to 1 by alloc if GC should occur at next
                                      //   function invocation 
unsigned int *stack_bot_gc = NULL;    // bottom and top of stack -- used during GC to
unsigned int *stack_top_gc;           //   determine if a value is stack-allocated
unsigned int rp_from_space = 0;       // number of region pages in from-space
unsigned int to_space_old = 0;        // size of to-space (live) at previous GC
unsigned int lobjs_aftergc_old = 0;   // size of large objects after previous GC
unsigned int lobjs_aftergc = 0;       // size of large objects after GC
unsigned int lobjs_beforegc = 0;      // size of large objects before GC
unsigned int lobjs_period = 0;        // allocated bytes in large objects between GC's
unsigned int alloc_period = 0;        // allocated bytes by alloc between GC's (excludes lobjs)
unsigned int alloc_total = 0;         // allocated bytes by alloc (total, includes lobjs)
unsigned int gc_total = 0;            // bytes recycled by GC (total)
unsigned int lobjs_current = 0;       // bytes currently occupied by large objects
unsigned int lobjs_gc_treshold = 0;   // set time_to_gc to 1 when lobjs_current exceeds
                                      //   lobjs_gc_treshold; this variable is adjusted
                                      //   after garbage collection.
unsigned int rp_gc_treshold = 0;      // set time_to_gc to 1 when rp_used exceeds
                                      //   rp_gc_treshold; this variable is adjusted
                                      //   after garbage collection.
double FRAG_sum = 0.0;                // fragmentation denotes how much of region
                                      //   pages are used by values -- and is computed
                                      //   as an average of percentages computed at
                                      //   each garbage collection; thus for programs
                                      //   that seldom garbage collect, the fragmentation
                                      //   figure makes little sense.

int *data_lab_ptr = NULL;             // pointer at exported data labels part of the root-set
int num_gc = 0;                       // number of garbage collections

int doing_gc = 0;                     // set to 1 when GC'ing; otherwise 0
int raised_exn_interupt = 0;          // set to 1 if signal occurred during GC
int raised_exn_overflow = 0;          // set to 1 if signal occurred during GC

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

Klump *from_space_begin, *from_space_end;

/*******************/
/* PRETTY PRINTING */
/*******************/
static void 
pw(char *s,unsigned int tag) 
{
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

static void 
print(unsigned int *value) 
{
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
inline static void 
copy_words(unsigned int *from,unsigned int *to,int num) 
{
  int i;
  for ( i = 0 ; i < num ; i++ ) 
    *(to + i) = *(from + i);
  return;
}

/*******************************/
/* SCAN STACK INFINITE REGIONS */
/*******************************/
unsigned int **scan_stack = NULL;
#define INIT_STACK_SIZE_W 1024
int size_scan_stack;
int scan_sp;

inline static void 
init_scan_stack() 
{
  if (scan_stack == NULL) 
    {
      scan_stack = (unsigned int **) realloc((void *)scan_stack, INIT_STACK_SIZE_W*4);
      if (scan_stack == NULL)
	{
	  die("GC.init_scan_stack: Unable to allocate scan_stack");
	}
      size_scan_stack = INIT_STACK_SIZE_W;
    }
  scan_sp = 0;
  return;
}

#define is_scan_stack_empty() (scan_sp == 0)

inline static void 
push_scan_stack(unsigned int *ptr) 
{
  scan_stack[scan_sp] = ptr;
  scan_sp++;
  if ( scan_sp >= size_scan_stack ) 
    {
      size_scan_stack *= 2;
      scan_stack = (unsigned int **) realloc((void *)scan_stack, size_scan_stack*4);
      if (scan_stack == NULL)
	{
	  die("GC.push_scan_stack: Unable to increase scan_stack");
	}
    }
  return;
}

inline static unsigned int *
pop_scan_stack() 
{
  if ( scan_sp < 1 ) 
    {
      die("GC.pop_scan_stack: scan_sp below stack bot.");
    }
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

inline static void 
init_scan_container() 
{
  if (scan_container == NULL) 
    {
      scan_container = (unsigned int **) realloc((void *)scan_container, INIT_CONTAINER_SIZE_W*4);
      if (scan_container == NULL)
	{
	  die("GC.init_scan_container: Unable to allocate scan_container");
	}
      size_scan_container = INIT_CONTAINER_SIZE_W;
    }
  container_alloc = 0;
  container_scan = 0;
  return;
}

#define is_scan_container_empty() (container_scan == container_alloc)

inline static void 
push_scan_container(unsigned int *ptr) 
{
  scan_container[container_alloc] = ptr;
  container_alloc++;
  if (container_alloc >= size_scan_container) 
    {
      size_scan_container *= 2;
      scan_container = (unsigned int **) realloc((void *)scan_container, size_scan_container*4);
      if (scan_container == NULL)
	{
	  die("GC.push_scan_container: Unable to increase scan_container");
	}
    }
  return;
}

inline static unsigned int *
pop_scan_container() 
{
  unsigned int *v;
  //if (container_scan >= container_alloc) 
  //  die("GC.pop_scan_container: container_scan higher than container_alloc.");
  v = scan_container[container_scan];
  container_scan++;
  return v;
}

inline static void 
clear_scan_container() 
{
  int i;
  for ( i = 0 ; i < container_alloc ; i ++ )
    {
      *scan_container[i] = clear_tag_const(*scan_container[i]);
    }
}

/* We mark all region pages such that we can distinguish them from to-space */
/* region pages by setting a bit in the next n pointer.                   */
static void mk_from_space() {
  Ro *r;
  Klump *rp;

  #ifdef PROFILING
    int j;
  #endif

  from_space_begin = NULL;
  from_space_end = (((Klump *)TOP_REGION->b)-1); /* Points at last region page */

  for( r = TOP_REGION ; r ; r = r->p ) {
    #ifdef PROFILING
      // Similar to resetRegion in Region.c
      j = NoOfPagesInRegion(r);
      noOfPages -= j;
      profTabDecrNoOfPages(r->regionId, j);
      allocNowInf -= r->allocNow;
      profTabDecrAllocNow(r->regionId, r->allocNow);
      allocProfNowInf -= r->allocProfNow;
      r->allocNow = 0;
      r->allocProfNow = 0;
    #endif

    /* Move region pages to from-space */
    (((Klump *)r->b)-1)->n = from_space_begin;
    from_space_begin = clear_rtype(r->fp);

    /* Allocate new region page */
    {
      int rt;
      if ( rt = rtype(r) )
	{
	  r->fp = NULL;
	  set_rtype(r,rt);
	}
      else
	r->fp = NULL;
    }
    alloc_new_block(r);
  }

  /* Calculate size of from space */
  rp_from_space = 0;
  for ( rp = from_space_begin ; rp ; rp = rp->n )
    rp_from_space ++;
  return;
}

inline static int 
points_into_dataspace (unsigned int *p) {
  return (p >= data_begin_addr) && (p <= data_end_addr);
}

#define is_stack_allocated(obj_ptr) (((obj_ptr) <= stack_bot_gc) && (((obj_ptr) >= stack_top_gc)))
#define is_integer(obj_ptr)         ((obj_ptr) & 1)
#define is_constant(x)              ((x) & 0x20)         /* Bit 6 is the constant bit */
#define is_forward_ptr(x)           (((x) & 0x03) == 0)  /* Bit 0 and 1 must be zero */
#define clear_forward_ptr(x)        (x)
#define tag_forward_ptr(x)          ((unsigned int)(x))

// Region pages are of size 1Kb and aligned
#define get_rp_header(x)            ((Klump *)(((unsigned int)(x)) & 0xFFFFFC00))  

// Previous-pointer holds status bit
#define set_status_SOME(r)          (r->p = (Ro *)(((unsigned int)((r)->p)) | 0x01))   
#define set_status_NONE(r)          (r->p = (Ro *)(((unsigned int)((r)->p)) & 0xFFFFFFFE))
#define is_status_NONE(r)           ((((unsigned int)((r)->p)) & 0x01) == 0)

unsigned int 
size_lobj (unsigned int tag)
{
  switch ( tag_kind(tag) ) {
  case TAG_STRING: {
    unsigned int sz_bytes;
    sz_bytes = get_string_size(tag) + 5;              // 1 for zero-termination, 4 for size field
    return sz_bytes%4 ? 4+4*(sz_bytes/4) : sz_bytes;  // alignment
  }
  case TAG_TABLE:
    return 4*(get_table_size(tag) + 1);
  default:
    printf("tag_kind(tag) = %x (%d) - tag = %d\n", tag_kind(tag), tag_kind(tag), tag);
    die("GC.size_lobj");
    exit(2);
  }
}

/* -----------------------------------------------------
 * Find allocated bytes in from space; for measurements
 * ----------------------------------------------------- */

// Assumes that region does not contain untagged pairs or 
// untagged refs
static int 
allocated_bytes_in_region(Region r) 
{
  unsigned int *s;  // scan pointer
  Klump *rp;
  int allocated_bytes = 0;

  rp = r->fp;
  s = rp->i;
  
  while (((int *)s) != r->a) {
    rp = get_rp_header(s);
#if PROFILING
    s += sizeObjectDesc;
#endif
    switch (val_tag_kind_const(s)) {
    case TAG_STRING: {
      // adjust scan_ptr to after the string
      int sz;
      String str = (String)s;
      sz = get_string_size(str->size) + 1 /*for zero*/ + 4 /*for tag*/;
      sz = (sz%4) ? (1+sz/4) : (sz/4);
      s += sz;
      allocated_bytes += (4*sz);
      break;
    }
    case TAG_TABLE: {
      int sz;
      Table table = (Table)s;
      sz = get_table_size(table->size) + 1;
      s += sz;
      allocated_bytes += (4*sz);
      break;
    }
    case TAG_RECORD: {
      int sz = get_record_size(*s); /* Size excludes descriptor */
      s += (1 + sz);
      allocated_bytes += (4 + 4*sz);
      break;
    }
    case TAG_CON0: {
      s++;
      allocated_bytes += 4;
      break;
    }
    case TAG_CON1: 
    case TAG_REF: {
      s += 2;
      allocated_bytes += 8;
      break;
    }
    default: {
      pw("*s: ",*s);
      printf("s: %x, r->a: %x, diff(s,r->a): %d, rp: %x, diff(s,rp): %x\n",
	     s,
	     r->a,
	     ((int *)r->a-(int *)s),
	     rp,
	     (int *)s-(int *)rp);
      die("allocated_bytes_in_region: unrecognised tag at *s");
      break;
    }
    }
    /* Are we at end of region page or is the region page full, then go to next region page. */
    /* notPP is distinct from all other value tags */
    if ((((int *)s) != r->a) &&
	((((int *)s) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) ||
	 (*((int *)s) == notPP))) {
      s = ((unsigned int*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE;
    }
  }
  return allocated_bytes;
}

static int
allocated_bytes_in_pairrefregion(Ro* r)
{
  Klump* rp;
  int n = 0;
  for ( rp = clear_rtype(r->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    {
      if ( clear_tospace_bit(rp->n) )
	n += 4 * ALLOCATABLE_WORDS_IN_REGION_PAGE;  // not last page
      else
	n += 4 * ((r->a) - (rp->i));  // last page
    }
  return n;
}

allocated_bytes_in_tripleregion(Ro* r)
{
  Klump* rp;
  int n = 0;
  for ( rp = clear_rtype(r->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    {
      if ( clear_tospace_bit(rp->n) )
	// Take care of alignment
	n += 4 * 3 * (ALLOCATABLE_WORDS_IN_REGION_PAGE / 3);  // not last page
      else
	n += 4 * ((r->a) - (rp->i));  // last page
    }
  return n;
}

static int 
allocated_bytes_in_regions(void) 
{
  int n = 0;
  Ro* r;
  for ( r = TOP_REGION ; r ; r = r->p )
    {
      switch (rtype(r)) {
      case RTYPE_PAIR:
      case RTYPE_REF:
	n += allocated_bytes_in_pairrefregion(r);
	break;
      case RTYPE_TRIPLE:
	n += allocated_bytes_in_tripleregion(r);
	break;	
      default:
	n += allocated_bytes_in_region(r);
      }
    }
  return n;
}

static int 
allocated_bytes_in_lobjs(void) 
{
  int n = 0;
  Ro* r;
  Lobjs *lobjs;

  for ( r = TOP_REGION ; r ; r = r->p )
    for ( lobjs = r->lobjs ; lobjs ; lobjs = clear_lobj_bit(lobjs->next) ) 
      {
	unsigned int tag;
#ifdef PROFILING
	tag = *(&(lobjs->value) + sizeObjectDesc);
#else
	tag = lobjs->value;
#endif
	n += size_lobj(tag);
      }
  return n;
}

/********************/
/* OBJECT FUNCTIONS */
/********************/
// Returns the size including the descriptor; the 
// object must contain a descriptor at offset 0.
// This function is never used to determine the 
// size of an untagged pair or an untagged ref.
inline static int 
get_size_obj(unsigned int *obj_ptr) 
{
  switch (val_tag_kind_const(obj_ptr)) {
  case TAG_RECORD: return get_record_size(*obj_ptr) + 1;
  case TAG_CON0:   return 1;
  case TAG_CON1:
  case TAG_REF:    return 2;
  case TAG_TABLE:  return get_table_size(*obj_ptr) + 1;
  case TAG_STRING: 
    {
      int size = get_string_size(*obj_ptr) + 5;   // 1 for zero-termination, 4 for tag
      return size%4 ? 1 + (size/4) : size/4;      // alignment
    }
  default: 
    {
      pw("Tag: ", *obj_ptr);
      print(obj_ptr);
      die("GC.get_size_obj: can't recognize tag");
    }
  }
}

inline unsigned int *
acopy(Ro *r, unsigned int *obj_ptr) 
{
  int size;
  unsigned int *new_obj_ptr;

#ifdef PROFILING
  int pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr))-1)->atId;
#endif /*PROFILING*/

  size = get_size_obj(obj_ptr);     // size includes tag
#ifdef PROFILING
  new_obj_ptr = allocProfiling(r,size,pPoint);
#else
  new_obj_ptr = alloc(r,size);
#endif
  copy_words(obj_ptr,new_obj_ptr,size);
  return new_obj_ptr;
}

inline unsigned int *
acopy_pair(Ro *r, unsigned int *obj_ptr) 
{
  unsigned int *new_obj_ptr;

#ifdef PROFILING
  int pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
#endif /*PROFILING*/

#ifdef PROFILING
  new_obj_ptr = allocProfiling(r,2,pPoint) - 1;
#else
  new_obj_ptr = alloc(r,2) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  *(new_obj_ptr+2) = *(obj_ptr+2);
  return new_obj_ptr;
}

inline unsigned int *
acopy_ref(Ro *r, unsigned int *obj_ptr) 
{
  unsigned int *new_obj_ptr;

#ifdef PROFILING
  int pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
#endif /*PROFILING*/

#ifdef PROFILING
  new_obj_ptr = allocProfiling(r,1,pPoint) - 1;
#else
  new_obj_ptr = alloc(r,1) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  return new_obj_ptr;
}

inline unsigned int *
acopy_triple(Ro *r, unsigned int *obj_ptr) 
{
  unsigned int *new_obj_ptr;

#ifdef PROFILING
  int pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
#endif /*PROFILING*/

#ifdef PROFILING
  new_obj_ptr = allocProfiling(r,3,pPoint) - 1;
#else
  new_obj_ptr = alloc(r,3) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  *(new_obj_ptr+2) = *(obj_ptr+2);
  *(new_obj_ptr+3) = *(obj_ptr+3);
  return new_obj_ptr;
}

inline static int
points_into_tospace (unsigned int x) 
{
  int ret;
  unsigned int *p;
  if ( is_integer(x) )
    return 0;
  p = (unsigned int*)x;
  if ( is_stack_allocated(p) )
    return 0;
  if ( points_into_dataspace(p) )
    return 0;
  // now either large object or in region
  // get the tospace bit
  ret = is_tospace_bit(get_rp_header(p)->n);
  return ret;
}

static unsigned int 
evacuate(unsigned int obj) 
{
  Klump* rp;
  Ro* r;
  unsigned int *obj_ptr, *new_obj_ptr;

  if (is_integer(obj)) 
    {
      return obj;                         // not subject to GC
    }

  obj_ptr = (unsigned int *)obj;          // object is a pointer

  if ( points_into_dataspace(obj_ptr) )
    {
      return obj;                         // not subject to GC
    }

  if ( is_stack_allocated(obj_ptr) )      
    {                                     // object immovable
      if ( is_constant(*obj_ptr) ) 
	{
	  return obj;      
	}
      *obj_ptr = set_tag_const(*obj_ptr); // set immovable-bit
      push_scan_container(obj_ptr);
      return obj;
    }

  // Object is in an infinite region or is a large object. Large
  // objects are aligned at 1K boundaries where a region page
  // descriptor is stored with large object bit set to 1 (large-object
  // bit is second least significant bit)

  rp = get_rp_header(obj_ptr);

  if ( is_lobj_bit(rp->n) )
    {                                     // object immovable
      if ( is_constant(*obj_ptr) )
	{
	  return obj;
	}
      *obj_ptr = set_tag_const(*obj_ptr); // set immovable-bit
      push_scan_container(obj_ptr);
      return obj;
    }

  // Object is in an infinite region
  r = rp->r;

  switch ( rtype(r) ) {
  case RTYPE_PAIR:
    {
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	{
	  return *(obj_ptr+1);
	}
      new_obj_ptr = acopy_pair(r, obj_ptr);
      *(obj_ptr+1) = (unsigned int)new_obj_ptr; // install forward pointer
      break;
    }
  case RTYPE_REF:
    {
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	{
	  return *(obj_ptr+1);
	}
      new_obj_ptr = acopy_ref(r, obj_ptr);
      *(obj_ptr+1) = (unsigned int)new_obj_ptr; // install forward pointer
      break;
    }
  case RTYPE_TRIPLE:
    {
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	{
	  return *(obj_ptr+1);
	}
      new_obj_ptr = acopy_triple(r, obj_ptr);
      *(obj_ptr+1) = (unsigned int)new_obj_ptr; // install forward pointer
      break;
    }
  default:   // Object is tagged 
    {
      if ( is_forward_ptr(*obj_ptr) ) 
	{                                       // object already copied
	  if ( points_into_tospace(*obj_ptr) )
	    {
	      return clear_forward_ptr(*obj_ptr);             
	    }
	  die ("forward ptr check failed\n");
	}
      new_obj_ptr = acopy(r, obj_ptr);
      *obj_ptr = tag_forward_ptr(new_obj_ptr);  // install forward pointer
    }
  }
  if ( is_status_NONE(r) ) 
    {
#ifdef PROFILING 
      push_scan_stack(new_obj_ptr - sizeObjectDesc);
#else
      push_scan_stack(new_obj_ptr);
#endif
      set_status_SOME(r);
    }
  return (unsigned int)new_obj_ptr;
}

static unsigned int*
scan_tagged_value(unsigned int *s)      // s is the scan pointer
{
  // All finite and large objects are temporarily annotated as immovable.
  // We therefore use val_tag_kind and not val_tag_kind_const

  switch ( val_tag_kind(s) ) { 
  case TAG_STRING: {                        // Do not GC the content of a string but
    int sz;                                 //    adjust s to point after the string
    String str = (String)s;
    sz = get_string_size(str->size) + 5;    // 1 for zero, 4 for tag
    sz = (sz%4) ? (1+sz/4) : (sz/4);
    return s + sz;
  }
  case TAG_TABLE: {
    int sz;
    Table table = (Table)s;
    sz = get_table_size(table->size);
    s++;
    while ( sz )
      {
	*s = evacuate(*s);
	s++;
	sz--;
      }
    return s;
  }
  case TAG_RECORD: {
    int remaining, num_to_skip, sz;
    sz = get_record_size(*s);          // Size excludes descriptor
    num_to_skip = get_record_skip(*s);
    s = s + 1 + num_to_skip;
    remaining = sz - num_to_skip;
    while ( remaining ) 
      {
	*s = evacuate(*s);
	s++;
	remaining--;
      }
    return s;
  }
  case TAG_CON0: {     // constant
    return s+1;
  }
  case TAG_CON1:
  case TAG_REF: {
    *(s+1) = evacuate(*(s+1));
    return s+2;
  }
  default: {
    pw("*s: ", *s);
    die("scan_tagged_value: unrecognised object descriptor pointed to by scan pointer");
    return 0;
  }
  }
}  

static void 
do_scan_stack() 
{
  unsigned int *s;   // scan pointer
  Klump *rp;
  Ro *r;

  // Run through scan stack and container
  while (!((is_scan_stack_empty()) && (is_scan_container_empty()))) {

    // Run through container - FINITE REGIONS and LARGE OBJECTS
    while (!(is_scan_container_empty())) 
      {
	s = scan_tagged_value(pop_scan_container());
      }

    while (!(is_scan_stack_empty())) {
      s = pop_scan_stack();
      /* Get Region Page and Region Descriptor */
      rp = get_rp_header(s);
      r = rp->r;
      
      switch ( rtype(r) ) {
      case RTYPE_PAIR:
	{
	  while ( ((int *)s+1) != r->a ) 
	    {
	      rp = get_rp_header(s);
#if PROFILING
	      s += sizeObjectDesc;
#endif
	      *(s+1) = evacuate(*(s+1));
	      *(s+2) = evacuate(*(s+2));
	      s += 2;
	      
	      /* If at end of region page or the region page is full, go 
	       * to next region page. */
	      if ((((int *)s+1) != r->a) && 
		  ((((int *)s+1) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) 
		   || (*((int *)s+1) == notPP)))
		// MEMO: For RTYPE_PAIR and RTYPE_REF, checking the content of *(s+1) against notPP (0) should not
		// be necessary because of alignment properties! mael 2003-05-15
		{
		  s = ((unsigned int*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE - 1;
		}
	    }
	  break;
	}
      case RTYPE_REF:
	{
	  while ( ((int *)s+1) != r->a ) 
	    {
	      rp = get_rp_header(s);
#if PROFILING
	      s += sizeObjectDesc;
#endif
	      *(s+1) = evacuate(*(s+1));
	      s += 1;
	      
	      /* If at end of region page or the region page is full, go 
	       * to next region page. */
	      if ((((int *)s+1) != r->a) && 
		  ((((int *)s+1) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) 
		   || (*((int *)s+1) == notPP)))
		{
		  s = ((unsigned int*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE - 1;
		}
	    }
	  break;
	}
      case RTYPE_TRIPLE:
	{
	  while ( ((int *)s+1) != r->a ) 
	    {
	      rp = get_rp_header(s);
#if PROFILING
	      s += sizeObjectDesc;
#endif
	      *(s+1) = evacuate(*(s+1));
	      *(s+2) = evacuate(*(s+2));
	      *(s+3) = evacuate(*(s+3));
	      s += 3;
	      
	      /* If at end of region page or the region page is full, go 
	       * to next region page. */
	      if ((((int *)s+1) != r->a) && 
		  ((((int *)s+1) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) 
		   || (*((int *)s+1) == notPP)))
		// MEMO: For RTYPE_PAIR and RTYPE_REF, checking the content of *(s+1) against notPP (0) should not
		// be necessary because of alignment properties! It is necessary for triples! mael 2003-05-15
		{
		  s = ((unsigned int*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE - 1;
		}
	    }
	  break;
	}
      default:
	{
	  while ( ((int *)s) != r->a ) 
	    {
	      rp = get_rp_header(s);
#if PROFILING
	      s += sizeObjectDesc;
#endif
	      s = scan_tagged_value(s);
	      
	      /* If at end of region page or the region page is full, go 
	       * to next region page. */
	      if ((((int *)s) != r->a) && 
		  ((((int *)s) == ((int *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE) 
		   || (*((int *)s) == notPP)))
		{
		  s = ((unsigned int*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE;
		}
	    }
	}
      }
      set_status_NONE(r);
    }
  }
  return;
}

#define predSPDef(sp,n) ((sp)+=(n))
#define succSPDef(sp) (sp--)
 
void 
gc(unsigned int **sp, unsigned int reg_map) 
{
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
  Klump *rp_tmp, *p;
  extern int rp_used;
  extern int rp_total;
  double ratio;
  double to_allocate;
  struct rusage rusage_begin;
  struct rusage rusage_end;
  unsigned int size_from_space = 0;
  unsigned int alloc_period_save;
  Ro *r;

  doing_gc = 1; // Mutex on the garbage collector

  getrusage(RUSAGE_SELF, &rusage_begin);

  stack_top_gc = (unsigned int*)sp;

  if ((int)(stack_bot_gc - stack_top_gc) < 0)
    {
      die("gc: stack_top_gc larger than stack_bot_gc on down-growing stack");
    }

  num_gc++;

  if ( verbose_gc ) 
    {
      fprintf(stderr,"[GC#%d", num_gc); fflush(stdout);
      size_from_space = allocated_bytes_in_regions();
      lobjs_beforegc = allocated_bytes_in_lobjs();
      alloc_period_save = alloc_period;
      alloc_period = 0;
    }

  rp_to_space = 0;

  // Initialize the scan stack (for Infinite Regions) and the
  // container (for Finite Regions and large objects)
  init_scan_stack();
  init_scan_container();

  mk_from_space();

  /* Search for live registers */
  sp_ptr = sp;
  w = reg_map;
  for ( offset = 0 ; offset < NUM_REGS ; offset++ ) {
    if ( w & 1 ) {
      value_ptr = ((unsigned int*)sp_ptr) + NUM_REGS - 1 - offset;  /* Address of live cell */
      *value_ptr = evacuate(*value_ptr);
    }
    w = w >> 1;
  }

  /* Do spilled arguments and results to current function */
  sp_ptr = sp;
  sp_ptr = sp_ptr + NUM_REGS;   // points at size_spilled_region_args

  size_spilled_region_args = *((int *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at size_rcf
  size_rcf = *((int *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at size_ccf
  size_ccf = *((int *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at last arg. to current function

  /* All arguments to current function are live - except for region arguments. */
  for ( offset = 0 ; offset < size_ccf ; offset++ ) {
    value_ptr = ((unsigned int*)sp_ptr);
    predSPDef(sp_ptr,1);
    if ( offset >= size_spilled_region_args ) 
      {
	*value_ptr = evacuate(*value_ptr);    
      }
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
  while ( fd_size != 0xFFFFFFFF ) {

    // printf("analysing frame\n");

    w_ptr = fd_ptr-4;

    /* Find RootSet in FD */
    if ( fd_size )  /* It may happen fd_size = 0 in which case w_ptr points at arbitrary address. */
      w = *w_ptr;
    w_idx = 0;
    for( offset = 0 ; offset < fd_size ; offset++ ) {
      if (w & 1) {
	// printf("evacuating value in frame begin\n");
	value_ptr = ((unsigned int*)sp_ptr) + fd_size - offset;
	*value_ptr = evacuate(*value_ptr); 
	// printf("evacuating value in frame end\n");
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
  for ( offset = 1 ; offset <= num_d_labs ; offset++ ) {
    // printf("evacuating value in data labels begin\n");
    value_ptr = *(((unsigned int**)data_lab_ptr) + offset);
    *value_ptr = evacuate(*value_ptr);
    // printf("evacuating value in data labels end\n");
  }

  do_scan_stack();

  /* We Are Done And Must Insert from-space Into The FreeList */
  from_space_end->n = freelist;
  freelist = from_space_begin;

  /* Run through all infinite regions and free all 
   * large objects that are not marked. */
  for( r = TOP_REGION ; r ; r = r->p ) 
    {
      Lobjs *lobjs;
      int first = 1;
      Lobjs **lobjs_ptr = &(r->lobjs); // last live next-slot
      lobjs = r->lobjs;
      while ( lobjs )
	{
	  unsigned int tag;
#ifdef PROFILING
	  tag = *(&(lobjs->value) + sizeObjectDesc);
#else
	  tag = lobjs->value;
#endif
	  if ( is_constant(tag) )
	    {                               // preserve object
	      if ( first )
		{
		  first = 0;
		  *lobjs_ptr = lobjs;
		}
	      else 
		*lobjs_ptr = set_lobj_bit(lobjs);
	      lobjs_ptr = &(lobjs->next);   // update slot
	      lobjs = clear_lobj_bit(lobjs->next);
	    }
	  else // do not preserve object
	    {	     
	      char* orig;
	      lobjs_current -= size_lobj(tag);
	      orig = lobjs->orig;
	      lobjs = clear_lobj_bit(lobjs->next);
	      free(orig);            // deallocate object
	    }
	}
      
      if ( first )
	*lobjs_ptr = NULL;
      else
	*lobjs_ptr = set_lobj_bit(NULL);

      // check consistency
      /*
      printf("check begin\n");
      lobjs = r->lobjs;
      if ( is_lobj_bit(lobjs) )
	die ("check: lobj bit set");
      while ( lobjs )
	{
	  unsigned int sz = size_lobj(lobjs->value);
	  printf("size_lobj: %d\n", sz);
	  lobjs = lobjs->next;
	  if ( is_lobj_bit(lobjs) )
	    lobjs = clear_lobj_bit(lobjs);
	  else
	    die ("check: lobj bit set");	  
	}
      printf("check end\n");
      */
    }

  // Unmark all tospace bits in region pages in regions on the stack
  for( r = TOP_REGION ; r ; r = r->p ) 
    for ( p = clear_rtype(r->fp) ; p ; p = p->n ) 
      {
	if ( is_tospace_bit(p->n) ) { 
	  // check that all region pages have their tospace bit set
	  p->n = clear_tospace_bit(p->n);
	} else {
	  die ("gc: page in tospace not marked");
	}
      }

  lobjs_gc_treshold = (int)(heap_to_live_ratio * (double)lobjs_current);

  // Reset all constant-bits in the scan container -- FINITE REGIONS 
  // and LARGE OBJECTS -- this clearance is safe because we have 
  // freed only those large objects that are unmarked and thus do
  // not occur in the scan container...
  clear_scan_container();

  rp_used = rp_to_space;

  // Update the GC treshold for region pages - we add -1.0 to
  // leave room for copying...
  rp_gc_treshold = (int)((heap_to_live_ratio - 1.0) * (double)rp_total / heap_to_live_ratio);
  if ( (int)((heap_to_live_ratio - 1.0) * (double)rp_used) > rp_gc_treshold )
    rp_gc_treshold = (int)((heap_to_live_ratio - 1.0) * (double)rp_used);
#ifdef SIMPLE_MEMPROF
  //  rp_really_used = rp_to_space;
#endif

  // ratio = (double)rp_total / (double)rp_used;
  // if ( ratio < heap_to_live_ratio ) {
  // to_allocate = heap_to_live_ratio * (double)rp_used - (double)rp_total;
  // callSbrkArg((int)to_allocate + REGION_PAGE_BAG_SIZE);
  // }

  if ( verbose_gc ) 
    {
      double RI = 0.0, GC = 0.0, FRAG = 0.0;
      unsigned int size_to_space;

      size_to_space = alloc_period;
      lobjs_aftergc = allocated_bytes_in_lobjs();
      alloc_period = alloc_period_save;
      alloc_total += alloc_period;
      alloc_total += lobjs_period;
      gc_total += (size_from_space + lobjs_beforegc - size_to_space - lobjs_aftergc);

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
	  RI = 100.0 * ( ((double)(to_space_old + lobjs_aftergc_old + alloc_period + 
				   lobjs_period - size_from_space - lobjs_beforegc)) /
			 ((double)(to_space_old + lobjs_aftergc_old + alloc_period +
				   lobjs_period - size_to_space - lobjs_aftergc)));
	  
	  GC = 100.0 * ( ((double)(size_from_space + lobjs_beforegc - size_to_space - lobjs_aftergc)) /
			 ((double)(to_space_old + lobjs_aftergc_old + alloc_period + lobjs_period - 
				   size_to_space - lobjs_aftergc)));

	  FRAG = 100.0 - 100.0 * (((double)(size_from_space + lobjs_beforegc)) / 
				  ((double)(4*ALLOCATABLE_WORDS_IN_REGION_PAGE*rp_from_space + lobjs_beforegc)));
	  FRAG_sum = FRAG_sum + FRAG;
	}

      fprintf(stderr," %dkb(%dkb) +L%dkb (%4.1f%) -> %dkb(%dkb) +L%dkb, free-list: %dkb, alloc: %dkb +L%dkb, RI: %4.1f%, GC: %4.1f%]\n",
	      size_from_space / 1024,
	      rp_from_space,
	      lobjs_beforegc / 1024,
	      FRAG,
	      size_to_space / 1024, 
	      rp_used,
	      lobjs_aftergc / 1024,
	      size_free_list(),
	      alloc_period / 1024,
	      lobjs_period / 1024,
	      RI, GC);	     
      
      to_space_old = size_to_space;
      lobjs_aftergc_old = lobjs_aftergc;
      lobjs_period = 0;
      alloc_period = 0;
    }

#ifdef SIMPLE_MEMPROF
  rp_really_used -= rp_from_space;
#endif
  time_to_gc = 0; 
  doing_gc = 0; // Mutex on the garbage collector
  
  if (raised_exn_interupt) 
    raise_exn((int)&exn_INTERRUPT);
  if (raised_exn_overflow)
    raise_exn((int)&exn_OVERFLOW);
  return;
}

void
report_dataspace(void) {
  printf ("data_begin_addr = %d\ndata_end_addr = %d\ndifference = %d\n",
	  data_begin_addr, data_end_addr,
	  data_end_addr - data_begin_addr);
  return;
}

#endif /* ENABLE_GC */
