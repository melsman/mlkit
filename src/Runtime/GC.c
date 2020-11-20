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
#include <stdint.h>

#include "Flags.h"
#include "Tagging.h"
#include "Region.h"
#include "String.h"
#include "CommandLine.h"
#include "Table.h"
#include "Exception.h"
#include "Profiling.h"
#include "Runtime.h"
#include "GC.h"

size_t time_to_gc = 0;          // set to 1 by alloc if GC should occur at next
                                //   function invocation
size_t *stack_bot_gc = NULL;    // bottom and top of stack -- used during GC to
size_t *stack_top_gc;           //   determine if a value is stack-allocated
size_t to_space_old = 0;        // size of to-space (live) at previous GC
size_t lobjs_aftergc_old = 0;   // size of large objects after previous GC
size_t lobjs_aftergc = 0;       // size of large objects after GC
size_t lobjs_beforegc = 0;      // size of large objects before GC
size_t lobjs_period = 0;        // allocated bytes in large objects between GC's
size_t alloc_period = 0;        // allocated bytes by alloc between GC's (excludes lobjs)
size_t alloc_total = 0;         // allocated bytes by alloc (total, includes lobjs)
size_t gc_total = 0;            // bytes recycled by GC (total)
size_t lobjs_current = 0;       // bytes currently occupied by large objects
size_t lobjs_gc_treshold = 0;   // set time_to_gc to 1 when lobjs_current exceeds
                                //   lobjs_gc_treshold; this variable is adjusted
                                //   after garbage collection.
size_t rp_gc_treshold = 0;      // set time_to_gc to 1 when rp_used exceeds
                                //   rp_gc_treshold; this variable is adjusted
                                //   after garbage collection.
double FRAG_sum = 0.0;          // fragmentation denotes how much of region
                                //   pages are used by values -- and is computed
                                //   as an average of percentages computed at
                                //   each garbage collection; thus for programs
                                //   that seldom garbage collect, the fragmentation
                                //   figure makes little sense.

uintptr_t *data_lab_ptr = NULL;           // pointer at exported data labels part of the root-set
ssize_t num_gc = 0;                       // number of garbage collections
#ifdef ENABLE_GEN_GC
ssize_t num_gc_major = 0;                 // number of major garbage collections
#endif
ssize_t doing_gc = 0;                     // set to 1 when GC'ing; otherwise 0
ssize_t raised_exn_interupt = 0;          // set to 1 if signal occurred during GC
ssize_t raised_exn_overflow = 0;          // set to 1 if signal occurred during GC

ssize_t time_gc_all_ms = 0;               // total time of GC (in milliseconds)

#ifdef ENABLE_GEN_GC
ssize_t time_majorgc_all_ms = 0;

ssize_t major_p = 0;                      // flag to specify whether gc should be major or minor
#define is_major_p (major_p == 1)
#define is_minor_p (major_p == 0)
#endif // ENABLE_GEN_GC

// This implementation assumes a down growing stack (e.g., X64)

#if defined(__LP64__) || (__WORDSIZE == 64)
#define NUM_REGS 16
#else
#define NUM_REGS 8
#endif

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
      Return Address    pointing at value HEX: FFFF_FFFF_FFFF_FFFF (no more FDs*) */

/* Layout of FD in the code:

         FrameMapN
             |
         FrameMap0
         FrameSize
         FrameMark (for debug)
  ReturnLabel: */

Rp *from_space_begin, *from_space_end;

#ifdef ENABLE_GEN_GC
//#define debug_gc(x) (x)
#define debug_gc(x) ({})
#else
#define debug_gc(x) ({})
#endif /* REMOVE */

/*******************/
/* PRETTY PRINTING */
/*******************/
static void
pw(char *s,unsigned long int tag0)
{
  int idx;
  unsigned int tag = (unsigned int)tag0; // discharge highest bits...

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
print(uintptr_t *value)
{
  char str[50];
  size_t val;

  if (((size_t)value) & 0x1) {
    strcpy(str,"INTEGER");
    val = (size_t)value;
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
  pw(str, val);
  return;
}

// #define copy_words(from,to,w) (memcpy((to),(from),sizeof(void *)*(w)))

inline static void
copy_words(uintptr_t *from,uintptr_t *to,size_t num)
{
  size_t i;
  for ( i = 0 ; i < num ; i++ )
    *(to + i) = *(from + i);
  return;
}

int
imax(int a, int b) {
  if (a > b) { return a; }
  else { return b; }
}

/*******************************/
/* SCAN STACK INFINITE REGIONS */
/*******************************/
uintptr_t **scan_stack = NULL;
#define INIT_STACK_SIZE_W 1024
long size_scan_stack;
long scan_sp;

inline static void
init_scan_stack()
{
  if (scan_stack == NULL)
    {
      scan_stack = (uintptr_t **) realloc((void *)scan_stack, INIT_STACK_SIZE_W*(sizeof(void *)));
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
push_scan_stack(uintptr_t *ptr)
{
  scan_stack[scan_sp] = ptr;
  scan_sp++;
  if ( scan_sp >= size_scan_stack )
    {
      size_scan_stack *= 2;
      scan_stack = (uintptr_t **) realloc((void *)scan_stack, size_scan_stack*(sizeof(void *)));
      if (scan_stack == NULL)
	{
	  die("GC.push_scan_stack: Unable to increase scan_stack");
	}
    }
  return;
}

inline static uintptr_t *
pop_scan_stack()
{
  if ( scan_sp < 1 )
    {
      die("GC.pop_scan_stack: scan_sp below stack bot.");
    }
  scan_sp--;
  return scan_stack[scan_sp];
}

/***************************************************/
/* SCAN CONTAINER FINITE REGIONS AND LARGE OBJECTS */
/***************************************************/
uintptr_t **scan_container = NULL;
#define INIT_CONTAINER_SIZE_W 1024
long size_scan_container;
long container_alloc;
long container_scan;

inline static void
init_scan_container()
{
  if (scan_container == NULL)
    {
      scan_container = (uintptr_t **) realloc((void *)scan_container, INIT_CONTAINER_SIZE_W*(sizeof(void *)));
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
push_scan_container(uintptr_t *ptr)
{
  //  printf("push_scan_container(%p) - size_scan_container=%d; container_alloc=%d; container_scan=%d\n",
  //         ptr, size_scan_container, container_alloc, container_scan);
  //print(ptr);
  //printf("\n");
  scan_container[container_alloc] = ptr;
  container_alloc++;
  if (container_alloc >= size_scan_container)
    {
      size_scan_container *= 2;
      scan_container = (uintptr_t **) realloc((void *)scan_container, size_scan_container*sizeof(void *));
      if (scan_container == NULL)
	{
	  die("GC.push_scan_container: Unable to increase scan_container");
	}
    }
  return;
}

inline static uintptr_t *
pop_scan_container()
{
  uintptr_t *v;
  v = scan_container[container_scan];
  container_scan++;
  return v;
}

inline static void
clear_scan_container()
{
  long i;
  for ( i = 0 ; i < container_alloc ; i ++ )
    {
      *scan_container[i] = clear_tag_const(*scan_container[i]);
    }
}

void pp_from_space()
{
  Rp *rp;

  for (rp = from_space_begin ; rp ; rp = rp->n )
#ifdef ENABLE_GEN_GC
    fprintf(stderr,
	    "[rp: %p, rp->i: %p, rp+1: %p, rp->colorPtr: %p, rp->n: %p]\n",
	    rp, &(rp->i), rp+1, rp->colorPtr, rp->n);
#else
    fprintf(stderr,
	    "[rp: %p, rp->i: %p, rp+1: %p, rp->n: %p]\n",
	    rp, &(rp->i), rp+1, rp->n);
#endif /* ENABLE_GEN_GC */
  return;
}

// We mark all region pages such that we can distinguish them from
// to-space region pages by setting a bit in the next n pointer.
static inline void
mk_from_space_gen(Gen *gen)
{
  // Move region pages to from-space
  (((Rp *)gen->b)-1)->n = from_space_begin;
  from_space_begin = clear_fp(gen->fp);

  // Allocate new region page
  {
    int rt;
    if ( (rt = all_marks_fp(*gen)) )
      {
	gen->fp = NULL;
	set_fp(*gen,rt);
      }
    else
      gen->fp = NULL;
  }
  alloc_new_block(gen);
}

static void mk_from_space()
{
  Ro *r;

#ifdef PROFILING
  int j;
#endif

  from_space_begin = NULL;
  from_space_end = (((Rp *)TOP_REGION->g0.b)-1); // Points at last region page

  for( r = TOP_REGION ; r ; r = r->p )
    {
     #ifdef PROFILING
      // Similar to resetRegion in Region.c
     #ifdef ENABLE_GEN_GC
      if ( is_major_p )
	{
     #endif // ENABLE_GEN_GC
	  j = NoOfPagesInRegion(r);
	  noOfPages -= j;
	  profTabDecrNoOfPages(r->regionId, j);
	  allocNowInf -= r->allocNow;
	  profTabDecrAllocNow(r->regionId, r->allocNow, "mk_from_space");
	  allocProfNowInf -= r->allocProfNow;
	  r->allocNow = 0;
	  r->allocProfNow = 0;
     #ifdef ENABLE_GEN_GC
	} else {
	  // We only reset generation g0
	  long allocNowG0 = 0;
	  long allocProfNowG0 = 0;
	  j = NoOfPagesInGen(&(r->g0));
	  noOfPages -= j;
	  profTabDecrNoOfPages(r->regionId, j);
	  calcAllocInGen(&(r->g0),&allocNowG0, &allocProfNowG0);
	  allocNowInf -= allocNowG0;
	  profTabDecrAllocNow(r->regionId, allocNowG0, "mk_from_space");
	  allocProfNowInf -= allocProfNowG0;
	  r->allocNow -= allocNowG0;
	  r->allocProfNow -= allocProfNowG0;
	}
      #endif // ENABLE_GEN_GC
    #endif // PROFILING

    mk_from_space_gen(&(r->g0));
#ifdef ENABLE_GEN_GC
    if ( is_major_p )
      mk_from_space_gen(&(r->g1));
#endif // ENABLE_GEN_GC
  }
  return;
}

#define is_stack_allocated(obj_ptr) (((obj_ptr) <= stack_bot_gc) && (((obj_ptr) >= stack_top_gc)))
#define is_integer(obj_ptr)         ((obj_ptr) & 1)
#define is_forward_ptr(x)           (((x) & 0x3) == 0)  /* Bit 0 and 1 must be zero */
#define clear_forward_ptr(x)        (x)
#define tag_forward_ptr(x)          ((unsigned long)(x))

// Region pages are of size 1Kb and aligned
#define get_rp_header(x)            ((Rp *)(((unsigned long)(x)) & 0xFFFFFFFFFFFFFC00))

size_t
size_lobj (size_t tag)
{
  switch ( tag_kind(tag) ) {
  case TAG_STRING: {
    size_t sz_bytes = get_string_size(tag) + 1 + (sizeof(void *));              // 1 for zero-termination, sizeof(void *) for size field
    return (sz_bytes % (sizeof(void *))) ? (sizeof(void *))+(sizeof(void *))*(sz_bytes/(sizeof(void *))) : sz_bytes;  // alignment
  }
  case TAG_TABLE:
    return (sizeof(void *))*(get_table_size(tag) + 1);
  default:
    printf("tag_kind(tag) = %zx (%zd) - tag = %zd\n", tag_kind(tag), tag_kind(tag), tag);
    die("GC.size_lobj");
    exit(2);
  }
}

// Return FALSE if we're at the last page and s=a.
static inline long
end_of_region_page_or_full(uintptr_t* s, uintptr_t* a, Rp* rp)
{
  return (s != a)
    && ((s == ((uintptr_t *)rp)+ALLOCATABLE_WORDS_IN_REGION_PAGE+HEADER_WORDS_IN_REGION_PAGE)
	|| (*s == notPP));
}

inline static uintptr_t*
next_value(uintptr_t* s, uintptr_t* a) {
  // If at end of region page or the region page is full,
  // go to next region page. Otherwise, s points to the next value or
  // there is no next value (s == a)
  Rp* rp = get_rp_header(s-1);    // s may exceed the region page
  if ( end_of_region_page_or_full(s, a, rp) )
    {
      s = ((uintptr_t*) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE;

#ifdef CHECK_GC
      rp = clear_tospace_bit(rp->n);
      uintptr_t *s1 = (uintptr_t *) &(rp->i);
      if (s1 != s) {
	die ("next_value err");
      }
#endif /*CHECK_GC*/
    }
  return s;
}

inline static uintptr_t*
next_untagged_value(uintptr_t* s, uintptr_t* a) {
  // If at end of region page or the region page is full,
  // go to next region page. Otherwise, s points to the next value or
  // there is no next value (s+1 == a)
  Rp* rp = get_rp_header(s);    // s+1 may exceed the region page
  if ( end_of_region_page_or_full(s+1, a, rp) )
    {
      s = ((uintptr_t *) (clear_tospace_bit(rp->n)))+HEADER_WORDS_IN_REGION_PAGE - 1;
    }
  return s;
}

/* --------------------------------------------------------------
 * Find allocated bytes in generations/regions; for measurements
 * -------------------------------------------------------------- */

static size_t
allocated_bytes_in_gen(Gen *gen)
{
  ////printf("allocated_bytes_in_gen begin\n");
  size_t allocated_bytes = 0;
  Rp *rp = clear_fp(gen->fp);                           // Maybe the generation-bit is set
  uintptr_t *s = (uintptr_t *) &(rp->i);                // scan pointer
  ////printf("before while; rp = %p; s = %p\n",rp,s);
  while (s != gen->a) {
    #ifdef PROFILING
    s += sizeObjectDesc;
    #endif
    ////printf("in while; s = %p, gen->a = %p\n",s,gen->a);
    ////printf("in while; tag: %lx\n",val_tag_kind_const(s));
    size_t sz = 0;
    switch (val_tag_kind(s)) {
    case TAG_STRING: {
      // adjust scan_ptr to after the string
      String str = (String)s;
      sz = get_string_size(str->size) + 1 /*for zero*/ + (sizeof(void *)) /*for tag*/;  // bytes
      sz = (sz%(sizeof(void *))) ? (1+sz/(sizeof(void *))) : (sz/(sizeof(void *)));     // words
      break;
    }
    case TAG_TABLE: {
      Table table = (Table)s;
      sz = get_table_size(table->size) + 1;
      break;
    }
    case TAG_RECORD: {
      sz = get_record_size(*s) + 1; /* Size excludes descriptor */
      ////printf("record of sz: %ld, skip: %ld\n", sz, get_record_skip(*s));
      ////printf("float? = %f; s(1) = %ld; s(2) = %ld\n", (double)get_d(s), *(s+1),*(s+2));
      break;
    }
    case TAG_CON0: {
      sz = 1;
      break;
    }
    case TAG_CON1: {
      sz = 2;
      break;
    }
    case TAG_REF: {
      die("allocated_bytes_in_gen: REFs are untagged...");
      break;
    }
    default: {
      Rp* rp = get_rp_header(s);
      pw("*s: ",*s);
      printf("s: %p, gen->a: %p, diff(s,gen->a): %ld, rp: %p, diff(s,rp): %ld\n",
	     s,
	     gen->a,
	     (long)((long *)gen->a-(long *)s),
	     rp,
	     (long)((long *)s-(long *)rp));
      if (is_lobj_bit(rp->n))
	printf("large object!!\n");
      die("allocated_bytes_in_gen: unrecognised tag at *s");
      break;
    }
    }
    ////printf("before next_value; s = %p\n",s);
    s += sz;
    allocated_bytes += ((sizeof(void *))*sz);
    s = next_value(s,gen->a);
    ////printf("after next_value; s = %p\n",s);
  }
  ////printf("allocated_bytes_in_gen end\n");
  return allocated_bytes;
}

// Assumes that region does not contain untagged pairs or
// untagged refs
static size_t
allocated_bytes_in_region(Region r)
{
  return allocated_bytes_in_gen(&(r->g0))
    #ifdef ENABLE_GEN_GC
    + allocated_bytes_in_gen(&(r->g1))
    #endif
    ;
}

static inline size_t
allocated_bytes_in_gen_untagged(Gen *gen, long obj_sz)  // obj_sz is in words
{
  Rp* rp;
  size_t n = 0;
  for ( rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    {
      if ( clear_tospace_bit(rp->n) )
	// Take care of alignment
	n += sizeof(void *) * obj_sz * (ALLOCATABLE_WORDS_IN_REGION_PAGE / obj_sz);  // not last page
      else
	n += sizeof(void *) * ((gen->a) - (rp->i));  // last page
    }
  return n;
}

static size_t
allocated_bytes_in_region_untagged(Ro* r, long obj_sz)   // obj_sz is in words
{
  return allocated_bytes_in_gen_untagged(&(r->g0),obj_sz)
    #ifdef ENABLE_GEN_GC
    + allocated_bytes_in_gen_untagged(&(r->g1),obj_sz)
    #endif
    ;
}

static size_t
allocated_bytes_in_regions(void)
{
  size_t n = 0;
  Ro* r;
  for ( r = TOP_REGION ; r ; r = r->p )
    {
      size_t m = 0;
      switch (rtype(r->g0)) { // g0 and g1 has the same rtype
      case RTYPE_PAIR:
	m = allocated_bytes_in_region_untagged(r,2);
	////printf("pair: %ld\n",m);
	break;
      case RTYPE_REF:
	m = allocated_bytes_in_region_untagged(r,1);
	////printf("ref: %ld\n",m);
	break;
      case RTYPE_TRIPLE:
	m = allocated_bytes_in_region_untagged(r,3);
	////printf("triple: %ld\n",m);
	break;
      default:
	m = allocated_bytes_in_region(r);
	////printf("other: %ld\n",m);
      }
      n += m;
    }
  return n;
}

static long
allocated_bytes_in_lobjs(void)
{
  long n = 0;
  Ro* r;
  Lobjs *lobjs;

  for ( r = TOP_REGION ; r ; r = r->p )
    for ( lobjs = r->lobjs ; lobjs ; lobjs = clear_lobj_bit(lobjs->next) )
      {
	unsigned long tag;
       #ifdef PROFILING
	tag = *(&(lobjs->value) + sizeObjectDesc);
       #else
	tag = lobjs->value;
       #endif
	n += size_lobj(tag);
      }
  return n;
}


// Find the number of allocated pages in a region/generation

static long
allocated_pages_in_gen(Gen *gen)
{
  long n = 0;
  Rp *rp;

  // Maybe the generation-bit is set
  for ( rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    n++;
  return n;
}

static long
allocated_pages_in_region(Region r)
{
  return allocated_pages_in_gen(&(r->g0))
    #ifdef ENABLE_GEN_GC
    + allocated_pages_in_gen(&(r->g1))
    #endif
    ;
}

static long
allocated_pages_in_regions(void)
{
  long n = 0;
  Ro* r;
  for ( r = TOP_REGION ; r ; r = r->p )
    {
      n += allocated_pages_in_region(r);
    }
  return n;
}

#ifdef CHECK_GC
#ifdef ENABLE_GEN_GC
// Check for no tospace-bits
static void
chk_no_tospacebits_gen(Gen *gen)
{
  Rp *rp;
  for ( rp = clear_fp(gen->fp) ; rp ; rp = clear_tospace_bit(rp->n) )
    {
      if ( is_tospace_bit(rp->n) )
	die ("chk_no_tospacebits failed");
      if ( gen != rp->gen )
	die ("chk_no_tospacebits_gen.wrong gen back-pointer");
    }
}

static void
chk_no_tospacebits_region(Region r)
{
  chk_no_tospacebits_gen(&(r->g0));
#ifdef ENABLE_GEN_GC
  chk_no_tospacebits_gen(&(r->g1));
  if ( is_gen_1(r->g0) )
    die ("chk_no_tospacebits_region.wrong gen 0");
  if ( !is_gen_1(r->g1) )
    die ("chk_no_tospacebits_region.wrong gen 1");
#endif
}

static void
chk_no_tospacebits_regions(void)
{
  Ro* r;
  for ( r = TOP_REGION ; r ; r = r->p )
    {
      chk_no_tospacebits_region(r);
    }
}
#endif // ENABLE_GEN_GC
#endif // CHECK_GC

/********************/
/* OBJECT FUNCTIONS */
/********************/
// Returns the size including the descriptor; the object must contain
// a descriptor at offset 0.  This function is never used to determine
// the size of an untagged pair, an untagged triple, or an untagged
// ref.
inline static ssize_t
get_size_obj(uintptr_t *obj_ptr)
{
  switch (val_tag_kind_const(obj_ptr)) {
  case TAG_RECORD: return get_record_size(*obj_ptr) + 1;
  case TAG_CON0:   return 1;
  case TAG_CON1:
  case TAG_REF:    return 2;
  case TAG_TABLE:  return get_table_size(*obj_ptr) + 1;
  case TAG_STRING:
    {
      ssize_t sz = get_string_size(*obj_ptr) + 1 + (sizeof(void *));   // 1 for zero-termination, sizeof(void *) for tag
      return sz%(sizeof(void *)) ? 1+sz/(sizeof(void *)) : sz/(sizeof(void *));      // alignment
    }
  default:
    {
      pw("Tag: ", *obj_ptr);
      print(obj_ptr);
      die("GC.get_size_obj: can't recognize tag");
      return -1;  // never gets here
    }
  }
}

// ToDo: GenGC remove print_tagged_rp_content
/*
void
print_tagged_rp_content(Rp *rp)
{
  unsigned int *obj_ptr;

  fprintf(stderr,"[tagged rp content...\n");
  for (obj_ptr = (unsigned int*)(&(rp->i))
	 ; obj_ptr < (unsigned int*)(rp+1) && obj_ptr != notPP
	 ; obj_ptr = obj_ptr + get_size_obj(obj_ptr))
    {
      fprintf(stderr,"Addr: %p - ",obj_ptr);
      print(obj_ptr);
    }
  fprintf(stderr,"]\n");
  return;
}
*/


/* ToDo: GenGC (1) allok skal tage højde for colorPtr
               (2) allok skal tage højde for om g1 indeholder en klump.
   gælder for alle acopy-funktioner
   MEMO: What does this comment mean?
*/
inline static uintptr_t *
acopy(Gen *gen, uintptr_t *obj_ptr)
{
  ssize_t size;
  uintptr_t *new_obj_ptr;
#ifdef PROFILING
  long pPoint;
#endif

  size = get_size_obj(obj_ptr);     // size includes tag

#ifdef CHECK_GC
  if ( size > ALLOCATABLE_WORDS_IN_REGION_PAGE )
    die ("acopy error");
#endif // CHECK_GC

#ifdef PROFILING
  pPoint = (((ObjectDesc *)(obj_ptr))-1)->atId;
  new_obj_ptr = allocGenProfiling(gen,size,pPoint);
#else
  new_obj_ptr = allocGen(gen,size);
#endif
  copy_words(obj_ptr,new_obj_ptr,size);

  return new_obj_ptr;
}

inline static uintptr_t *
acopy_pair(Gen *gen, uintptr_t *obj_ptr)
{
  uintptr_t *new_obj_ptr;

#ifdef PROFILING
  long pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
  new_obj_ptr = allocGenProfiling(gen,2,pPoint) - 1;
#else
  new_obj_ptr = allocGen(gen,2) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  *(new_obj_ptr+2) = *(obj_ptr+2);
  return new_obj_ptr;
}

inline static uintptr_t *
acopy_ref(Gen *gen, uintptr_t *obj_ptr)
{
  uintptr_t *new_obj_ptr;

#ifdef PROFILING
  long pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
  new_obj_ptr = allocGenProfiling(gen,1,pPoint) - 1;
#else
  new_obj_ptr = allocGen(gen,1) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  return new_obj_ptr;
}

inline static uintptr_t *
acopy_triple(Gen *gen, uintptr_t *obj_ptr)
{
  uintptr_t *new_obj_ptr;

#ifdef PROFILING
  long pPoint;
  pPoint = (((ObjectDesc *)(obj_ptr+1))-1)->atId;
  new_obj_ptr = allocGenProfiling(gen,3,pPoint) - 1;
#else
  new_obj_ptr = allocGen(gen,3) - 1;
#endif
  *(new_obj_ptr+1) = *(obj_ptr+1);
  *(new_obj_ptr+2) = *(obj_ptr+2);
  *(new_obj_ptr+3) = *(obj_ptr+3);
  return new_obj_ptr;
}

static int
points_into_tospace (uintptr_t x)
{
  uintptr_t *p;

  Rp *rp;
  if ( is_integer(x) )
    return 0;
  p = (uintptr_t *)x;
  if ( points_into_dataspace(p) ) {
    #ifdef CHECK_GC
    if ( is_stack_allocated(p) )
      die ("GC: p is both on stack and in dataspace!");
    #endif // CHECK_GC
    return 0;
  }
  if ( is_stack_allocated(p) )
    return 0;
  // now either large object or in region
  rp = get_rp_header(p);
  if (is_lobj_bit(rp->n))
    return 0;    /* large object as first component of pair, triple, or ref
		  * - not a forward-pointer */

#ifdef ENABLE_GEN_GC

  // In a minor collection, a value may point (via a forward pointer)
  // to another value v in to-space even though the to_space_bit is
  // not set on the page holding v (i.e., if v resides in a region
  // page in g1 (old generation) that holds values allocated before
  // this initiation of gc. We can determine that v is in to-space by
  // comparing the location of v to the colorPtr in the region
  // page. If the location (pointer) is larger than or equal to the
  // colorPtr then v resides in to-space.
  //
  // For this to work, colorPtr in old-generation last-pages should be
  // identical to the allocation pointer. This is so, even though
  // colorPtr's in old generations are updated after each gc - the
  // reason is that the mutator never allocates values in old
  // generations.

  if ( is_minor_p && is_gen_1(*(rp->gen)) )
    {
      switch ( rtype(*(rp->gen)) )
	{
	case RTYPE_PAIR:
	case RTYPE_REF:
	case RTYPE_TRIPLE:
	  {
	    if ( p+1 >= rp->colorPtr )
	      return 1;
	    else return 0;
	  }
	default:
	  {
	    if ( p >= rp->colorPtr )
	      return 1;
	    else return 0;
	  }
	}
    } else
#endif // ENABLE_GEN_GC
      // Here is the story about the tospace bit: By default, the
      // tospace-bit on region pages is set during region page
      // allocation (Region.c) when gc is enabled. For the old
      // generation, however, it is not sufficient to check the
      // tospace-bit in a region page to determine if the object (in
      // the page) is part of to-space (see comment above).
      return is_tospace_bit(rp->n);
}

inline static Gen *
target_gen(Gen *gen, Rp *rp, uintptr_t *obj_ptr)
{
#ifdef ENABLE_GEN_GC
  // We could also choose first to ask if gen is g1 (old generation)
  //  - in this case we could return gen immediately
    if ( obj_ptr < rp->colorPtr )
      return &(get_ro_from_gen(*gen)->g1);  // old gen
#ifdef CHECK_GC
    if ( is_gen_1(*gen) )
      {
	fprintf(stderr,"target_gen: obj_ptr: %p\n",obj_ptr);
	pp_gen(gen);
	die("not g0");
      }
#endif // CHECK_GC
#endif // ENABLE_GEN_GC
    return gen;
}

static uintptr_t
evacuate(uintptr_t obj)
{
  Rp* rp;
  Gen* gen;
  Gen* copy_to_gen;

  uintptr_t *obj_ptr, *new_obj_ptr;
  if (is_integer(obj))
    {
      return obj;                         // not subject to GC
    }

  obj_ptr = (uintptr_t *)obj;             // object is a pointer

  if ( points_into_dataspace(obj_ptr) )
    {
      #ifdef CHECK_GC
      if ( is_stack_allocated(obj_ptr) )
        die ("GC: obj_ptr is both on stack and in dataspace!");
      #endif // CHECK_GC
      return obj;                         // not subject to GC
    }

  if ( is_stack_allocated(obj_ptr) )
    {                                     // object immovable
      if ( is_const(*obj_ptr) )
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
      if ( is_const(*obj_ptr) )
	{
	  //fprintf(stderr, "Large object %p is constant (0x%x) ; rp=%p\n", obj_ptr, *obj_ptr, rp);
	  return obj;
	}
      //fprintf(stderr, "Large object %p is NOT constant (0x%x) ; rp=%p\n", obj_ptr, *obj_ptr, rp);
      *obj_ptr = set_tag_const(*obj_ptr); // set immovable-bit
      //fprintf(stderr, "Large object %p is NOW constant (0x%x) ; rp=%p\n", obj_ptr, *obj_ptr, rp);
      push_scan_container(obj_ptr);
      return obj;
    }

  // Object is in an infinite region
  gen = rp->gen;
#ifdef ENABLE_GEN_GC
  if (is_minor_p && is_gen_1(*gen))  // old generation
    {
      // obj_ptr points at old area in g1 and should be returned!
      return obj;
    }
#endif // ENABLE_GEN_GC
  switch ( rtype(*gen) ) {
  case RTYPE_PAIR:
    {
      // printf("RTYPE_PAIR\n");
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	return *(obj_ptr+1);
      // obj_ptr points at slot before the actual value
      copy_to_gen = target_gen(gen, rp, obj_ptr+1);
      new_obj_ptr = acopy_pair(copy_to_gen, obj_ptr);
      *(obj_ptr+1) = (uintptr_t)new_obj_ptr; // install forward pointer
      break;
    }
  case RTYPE_REF:
    {
      // printf("RTYPE_REF\n");
      // ToDo: GenGC det ser ud til at points_into_tospace checker for
      // mere end nødvendigt er - vi ved at det er i en inf-region
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	return *(obj_ptr+1);
      // obj_ptr points at slot before the actual value
      copy_to_gen = target_gen(gen, rp, obj_ptr+1);
      new_obj_ptr = acopy_ref(copy_to_gen, obj_ptr);
      *(obj_ptr+1) = (uintptr_t)new_obj_ptr; // install forward pointer
      break;
    }
  case RTYPE_TRIPLE:
    {
      // printf("RTYPE_TRIPLE\n");
      if ( points_into_tospace(*(obj_ptr+1)) )  // check for forward pointer
	return *(obj_ptr+1);
      // obj_ptr points at slot before the actual value
      copy_to_gen = target_gen(gen, rp, obj_ptr+1);
      new_obj_ptr = acopy_triple(copy_to_gen, obj_ptr);
      *(obj_ptr+1) = (uintptr_t)new_obj_ptr; // install forward pointer
      break;
    }
  default:   // Object is tagged
    {
      // printf("RTYPE_DEFAULT\n");
      if ( is_forward_ptr(*obj_ptr) )   // obj tag contains a fwd-ptr
	// ToDo: With GenGC, can't we just skip the following
	// comparison? What should we do if the pointer does not point
	// into to-space?
	{
	  // object already copied
#ifdef CHECK_GC
	  if ( ! points_into_tospace(*obj_ptr) )
	    {
	      printf("*obj_ptr=0x%zx - obj_ptr=%p - rp=%p - gen=%p - rtype(*gen)=%zx\n", *obj_ptr, obj_ptr, rp, gen, rtype(*gen));
	      die ("forward ptr check failed\n");
	    }
	  else
#endif // CHECK_GC
	    return clear_forward_ptr(*obj_ptr);
	}
      //printf("0x%x not a forward ptr - about to copy %p - rp=%p - gen=%p\n", *obj_ptr, obj_ptr, rp, gen);
      //printf("gen:\n");
      //pp_gen(gen);
      copy_to_gen = target_gen(gen, rp, obj_ptr);
      //printf("copy_to_gen:\n");
      //pp_gen(copy_to_gen);
      new_obj_ptr = acopy(copy_to_gen, obj_ptr);
      *obj_ptr = tag_forward_ptr(new_obj_ptr);  // install forward pointer
    }
  }
  if ( is_gen_status_NONE(*copy_to_gen) )
    {
     #ifdef PROFILING
      push_scan_stack(new_obj_ptr - sizeObjectDesc);
     #else
      //printf("push_scan_stack: %p - rt=%x, rt_target=%x\n",new_obj_ptr,rtype(*gen),rtype(*copy_to_gen));
      push_scan_stack(new_obj_ptr);
     #endif
      set_gen_status_SOME(*copy_to_gen);
    }
  return (uintptr_t)new_obj_ptr;
}

static uintptr_t*
scan_tagged_value(uintptr_t *s)      // s is the scan pointer
{
  // All large objects and objects in finite regions are temporarily
  // annotated as immovable. We therefore use val_tag_kind and not
  // val_tag_kind_const

  long sz;                                 // adjust s to point after the string
  switch ( val_tag_kind(s) ) {
  case TAG_STRING: {                        // Do not GC the content of a string but
    String str = (String)s;
    sz = get_string_size(str->size) + 1 + (sizeof(void *));    // 1 for zero, sizeof(void *) for tag
    sz = sz%(sizeof(void *)) ? 1+sz/(sizeof(void *)) : (sz/(sizeof(void *)));
    return s + sz;
  }
  case TAG_TABLE: {
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
    size_t remaining, num_to_skip;
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
    fprintf(stderr, "scan_tagged_value: obj %p\n", s);
    die("scan_tagged_value: unrecognised object descriptor pointed to by scan pointer");
    return 0;
  }
  }
}

static void
do_scan_stack()
{
  Rp *rp;
  Gen *gen;

  // Run through scan stack and container
  while (!((is_scan_stack_empty()) && (is_scan_container_empty()))) {

    // Run through container - FINITE REGIONS and LARGE OBJECTS
    while (!(is_scan_container_empty()))
      {
	uintptr_t* tmp;
	tmp = pop_scan_container();
	//printf("pop_scan_container: %p\n", tmp);
        scan_tagged_value(tmp);
      }

    while (!(is_scan_stack_empty()))
      {
	uintptr_t *s;   // scan pointer
	s = pop_scan_stack();
	//printf("pop_scan_stack: %p\n", s);
	// Get Region Page and Generation
	rp = get_rp_header(s);
	gen = rp->gen;
	switch ( rtype(*gen) )
	  {
	  case RTYPE_PAIR:
	    {
	      while ( s+1 != gen->a )
		{
		  #if PROFILING
		   s += sizeObjectDesc;
		  #endif
		   *(s+1) = evacuate(*(s+1));
		   *(s+2) = evacuate(*(s+2));
		   s = next_untagged_value(s+2,gen->a);
		}
	      break;
	    }
	  case RTYPE_REF:
	    {
	      while ( s+1 != gen->a )
		{
                  #if PROFILING
		  s += sizeObjectDesc;
                  #endif
		  *(s+1) = evacuate(*(s+1));
		  s = next_untagged_value(s+1,gen->a);
		}
	      break;
	    }
	  case RTYPE_TRIPLE:
	    {
	      while ( s+1 != gen->a )
		{
                  #if PROFILING
		  s += sizeObjectDesc;
                  #endif
		  *(s+1) = evacuate(*(s+1));
		  *(s+2) = evacuate(*(s+2));
		  *(s+3) = evacuate(*(s+3));
		  s = next_untagged_value(s+3,gen->a);
		}
	      break;
	    }
	  case RTYPE_ARRAY:
	    {
	      //printf("RTYPE_ARRAY\n");
	    }
	  default:
	    {
	      while ( s != gen->a )
		{
                  #if PROFILING
		  s += sizeObjectDesc;
                  #endif
		  // printf("calling scan_tagged_value %p ;gen->a=%p; gen=%d\n", s,gen->a,is_gen_1(*gen));
		  s = scan_tagged_value(s);
		  s = next_value(s, gen->a);
		}
	      break;
	    }
	  } // switch
	set_gen_status_NONE(*gen);
      }
  }
  return;
}

inline static void
clear_tospace_bit_and_set_colorPtr_in_gen(Gen *gen)
{ Rp *p;

  for ( p = clear_fp(gen->fp) ; p ; p = p->n )
    {
      // Clear tospace-bit - in minor gc, pages in g1 are not marked!
#ifdef CHECK_GC
      if ( ! is_tospace_bit(p->n)
#ifdef ENABLE_GEN_GC
	   && ( is_major_p || ! is_gen_1(*gen) )
#endif // ENABLE_GEN_GC
	   )
	die ("gc: page in tospace not marked in major gc");
#endif // CHECK_GC
      p->n = clear_tospace_bit(p->n);

#ifdef ENABLE_GEN_GC
      // Update colorPtr
      if ( p->n ) // tospace-bit has been cleared
	p->colorPtr = (uintptr_t *) (p+1); // Not last page so entire page is black
      else
	p->colorPtr = gen->a; // Last page so only allocated part of page is black
#endif // ENABLE_GEN_GC
    }
}

#define predSPDef(sp,n) ((sp)+=(n))
#define succSPDef(sp) (sp--)

#ifdef CHECK_GC
void
check_all_lobjs(void)    // used for debugging
{
  Region r;
  //printf("[check_all_lobjs begin]\n");
  for( r = TOP_REGION ; r ; r = r->p )
    {
      Lobjs *lobjs;
      for ( lobjs = r->lobjs ; lobjs ; lobjs = clear_lobj_bit(lobjs->next) )
	{
	  uintptr_t* tag_ptr;
	  //uintptr_t sz;
#ifdef PROFILING
	  tag_ptr = &(*(&(lobjs->value) + sizeObjectDesc));
#else
	  tag_ptr = &(lobjs->value);
#endif
	  //sz = size_lobj(*tag_ptr);
	  //printf("  size_lobj: %d\n", sz);
	  if ( is_const(*tag_ptr) )
	    die ("check_lobjs: lobj constant bit set");
	  if ( !is_lobj_bit(lobjs->next) )
	    die ("check_lobjs: lobj bit not set");
	}
    }
  //printf("[check_all_lobjs end]\n");
}
#endif // CHECK_GC

double
region_utilize(long pages, long bytes)
{
  if ( pages > 0.0 )
    return (100.0 * (double)bytes
	    / ((double)(pages * sizeof(void *) * ALLOCATABLE_WORDS_IN_REGION_PAGE)));
  else
    return 0.0;
}

void
gc(uintptr_t **sp, size_t reg_map)
{
  long time_gc_one_ms = 0;
  extern Rp* freelist;
  uintptr_t **sp_ptr;
  uintptr_t *fd_ptr;
  unsigned long fd_size, fd_offset_to_return;
  uintptr_t *w_ptr;
  long w_idx;
  unsigned long w;
  long offset;
  uintptr_t *value_ptr;
  long num_d_labs;
  long size_rcf, size_ccf, size_spilled_region_args;
  extern long rp_used;
  extern long rp_total;
  struct rusage rusage_begin;
  struct rusage rusage_end;
  size_t bytes_from_space = 0;
  unsigned long pages_from_space = 0;
  unsigned long alloc_period_save = 0;
  Ro *r;

  // Mutex on the garbage collector; used by alloc_new_block in
  // Region.c for determining whether the tospace-bit should be set on
  // new allocated pages.
  doing_gc = 1;

#ifdef CHECK_GC
  ////printf("entering gc\n");
#endif

#ifdef ENABLE_GEN_GC
  // See code below after GC proper
  //  major_p = (major_p==0)?(1):(0);
  //  major_p = 0;
  if ( only_major_gc )
    major_p = 1;
#endif

  stack_top_gc = (uintptr_t *)sp;

#ifdef CHECK_GC
  if ((ssize_t)(stack_bot_gc - stack_top_gc) < 0)
    {
      die("gc: stack_top_gc larger than stack_bot_gc on down-growing stack");
    }
#endif // CHECK_GC

  num_gc++;

  if ( verbose_gc || report_gc )
    {
      getrusage(RUSAGE_SELF, &rusage_begin);
#ifdef ENABLE_GEN_GC
      if ( major_p )
	num_gc_major++;
#endif // ENABLE_GEN_GC
    }

  if ( verbose_gc )
    {
      fprintf(stderr,"[%s#%zd",
#ifdef ENABLE_GEN_GC
	      (is_major_p)?("GC"):("gc"),
#else
	      "GC",
#endif
	      num_gc);
      fflush(stderr);
      ////fprintf(stderr,"[GC: allocated_bytes_in_regions]\n");
      bytes_from_space = allocated_bytes_in_regions();
      ////fprintf(stderr,"[GC: allocated_pages_in_regions]\n");
      pages_from_space = allocated_pages_in_regions();
      ////fprintf(stderr,"[GC: allocated_bytes_in_lobjs]\n");
      lobjs_beforegc = allocated_bytes_in_lobjs();
      alloc_period_save = alloc_period;
      alloc_period = 0;
    }

  // Initialize the scan stack (for Infinite Regions) and the
  // container (for Finite Regions and large objects)
  ////fprintf(stderr,"[GC: init_scan_stack]\n");
  init_scan_stack();
  ////fprintf(stderr,"[GC: init_scan_container]\n");
  init_scan_container();

#ifdef ENABLE_GEN_GC
#ifdef CHECK_GC
  chk_no_tospacebits_regions();

  for ( r = TOP_REGION ; r ; r = r->p )
    {
      Rp* rp;
      for ( rp = clear_fp(r->g1.fp) ; rp ; rp = clear_tospace_bit(rp->n) )
	{
	  if ( clear_tospace_bit(rp->n) )
	    {
	      if ( rp->colorPtr != (uintptr_t *)(rp+1) )
		{
		  pp_gen(&(r->g1));
		  printf("r->g1.b=%p\n", r->g1.b);
		  die ("problem with middle page");
		}
	    }
	  else
	    // last page
	    if ( rp->colorPtr != (uintptr_t *)(r->g1.a) )
	      {
		printf("rp->colorPtr=%p - r->g1.a=%p\n", rp->colorPtr, r->g1.a);
		pp_gen(&(r->g1));
		die ("problem with final page");
	      }
	}
    }
#endif // CHECK_GC
#endif // ENABLE_GEN_GC

  ////fprintf(stderr,"[GC: mk_from_space]\n");
  mk_from_space();

#ifdef ENABLE_GEN_GC
  if ( is_minor_p ) {
    // If minor gc then refs and arrays in old generations (g1)
    // and lobjs (i.e., for large arrays) are also considered
    // part of the root-set...
    for ( r = TOP_REGION ; r ; r = r->p ) {
      switch ( rtype(r->g1) ) {
      case RTYPE_REF: {      // Refs are untagged
	value_ptr = ((uintptr_t *)clear_fp(r->g1.fp))+HEADER_WORDS_IN_REGION_PAGE - 1;
	// evacuate content of refs in g1
	// refs occupies one word only!
	while ( (value_ptr + 1) != r->g1.a )
	  {
           #if PROFILING
	    value_ptr += sizeObjectDesc;
           #endif // PROFILING
	    *(value_ptr+1) = evacuate(*(value_ptr+1));
	    value_ptr = next_untagged_value(value_ptr+1,r->g1.a);
	  }
	break;
      }
      case RTYPE_ARRAY: {
	size_t tag;
	ssize_t i, sz;
	Lobjs *lobjs;

	value_ptr = ((uintptr_t *)clear_fp(r->g1.fp))+HEADER_WORDS_IN_REGION_PAGE;
	// evacuate content of arrays in g1
	while ( (value_ptr) != r->g1.a )
	  {
           #if PROFILING
	    value_ptr += sizeObjectDesc;
           #endif // PROFILING
	    tag = *value_ptr;
	    sz = get_table_size(tag);
	    value_ptr++; // Point at first element in array
	    for ( i = 0 ; i < sz ; i++ ) {
	      *value_ptr = evacuate(*value_ptr);
	      value_ptr++;
	    }
	    value_ptr = next_value(value_ptr, r->g1.a);
	  }

	// evacuate contents of arrays in lobjs
	for ( lobjs = r->lobjs ; lobjs ; lobjs = clear_lobj_bit(lobjs->next) )
	  {
	    value_ptr = &(lobjs->value);
            #ifdef PROFILING
	    value_ptr += sizeObjectDesc;
            #endif

	    // The const-bit could have been set already; the previous calls to
	    // evacuate could have caused large objects to be be pushed onto the
	    // scan container! mael 2005-11-14
	    if ( ! is_const(*value_ptr) )
	      {
		//fprintf(stderr, "Array %p is NOT visited (0x%x)\n", value_ptr, *value_ptr);
		*value_ptr = set_tag_const(*value_ptr); // set immovable-bit
		//fprintf(stderr, "Array %p is NOW visited (0x%x)\n", value_ptr, *value_ptr);
		push_scan_container(value_ptr);
	      }
	  }
	break;
      }
      default:
	{
	  // Do nothing - region does not contain mutable values
	}
      }
    }
  }
#endif // ENABLE_GEN_GC

  // Search for live registers
  ////fprintf(stderr,"[GC: search for live registers - sp=%p, reg_map=%zx]\n", sp, reg_map);
  sp_ptr = sp;
  w = reg_map;
  for ( offset = 0 ; offset < NUM_REGS ; offset++ ) {
    if ( w & 1 ) {
      value_ptr = ((uintptr_t *)sp_ptr) + NUM_REGS - 1 - offset;  /* Address of live cell */
      *value_ptr = evacuate(*value_ptr);
    }
    w = w >> 1;
  }

  // Do spilled arguments and results to current function
  /// fprintf(stderr,"[GC: do spilled arguments - sp=%p, reg_map=%zx, NUM_REGS=%d]\n", sp, reg_map, NUM_REGS);

  sp_ptr = sp;
  sp_ptr = sp_ptr + NUM_REGS;   // points at size_spilled_region_args

  size_spilled_region_args = *((long *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at size_rcf
  size_rcf = *((long *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at size_ccf
  size_ccf = *((long *)sp_ptr);
  predSPDef(sp_ptr,1);          // sp_ptr points at last arg. to current function

  /// fprintf(stderr,"[GC: calc done; size_spilled_region_args=%zx; size_rcf=%zx; size_ccf=%zx]\n",
  ///  	      size_spilled_region_args,size_rcf,size_ccf);

  // All arguments to current function are live - except for region arguments.
  for ( offset = 0 ; offset < size_ccf ; offset++ ) {
    value_ptr = ((uintptr_t *)sp_ptr);
    predSPDef(sp_ptr,1);
    if ( offset >= size_spilled_region_args )
      {
	*value_ptr = evacuate(*value_ptr);
      }
  }

  /* sp_ptr points at first return address.                           */
  /* Below the return address we may have slots for spilled results - */
  /* they are not live at this point!                                 */

  /* Search for Frame Descriptors (FD). An FD covers */
  /*   - function frame                              */
  /*   - spilled arguments                           */
  /*   - return address                              */
  /*   - spilled results                             */

  /// fprintf(stderr,"[GC: FD processing]\n");

  fd_ptr = *sp_ptr;
  fd_offset_to_return = *(fd_ptr-2);
  fd_size = *(fd_ptr-3);
  predSPDef(sp_ptr,size_rcf);

  /// fprintf(stderr,"[GC: FD entering loop; sp_ptr=%p; fd_ptr=%p]\n",sp_ptr,fd_ptr);

  // sp_ptr points at first address before FD
  while ( fd_size != /* 0xFFFFFFFFFFFFFFFF */ UINTPTR_MAX)
    {
      // Analyse frame
      /// fprintf(stderr,"[GC: FD in loop; fd_size=%zx]\n", fd_size);


      w_ptr = fd_ptr-4;   // 4 is ok, also for x64

      // Find RootSet in FD
      if ( fd_size )  // fd_size may be 0 in which case w_ptr points at arbitrary address.
	w = *w_ptr;
      w_idx = 0;
      for( offset = 0 ; offset < fd_size ; offset++ )
	{
	  if (w & 1) {
	    // Evacuate value in frame
	    value_ptr = ((uintptr_t *)sp_ptr) + fd_size - offset;
	    *value_ptr = evacuate(*value_ptr);
	  }
	  w = w >> 1;
	  w_idx++;
	  if ((w_idx == 32) & (offset+1 < fd_size))   // 32: code generator uses Word32
	    {
	      // Again, w_ptr may point arbitrarily if we are done.
	      w_ptr--;
	      w = *w_ptr;
	      w_idx = 0;
	    }
	}

      sp_ptr = sp_ptr + fd_offset_to_return + 1; // Points at next return address.
      fd_ptr = *sp_ptr;
      fd_offset_to_return = *(fd_ptr-2);
      fd_size = *(fd_ptr-3);
      predSPDef(sp_ptr,size_rcf);
    }

  // fprintf(stderr,"[GC: FD after loop]\n");

  // Search for data labels; they are part of the root-set.
  num_d_labs = *data_lab_ptr; /* Number of data labels */
  for ( offset = 1 ; offset <= num_d_labs ; offset++ ) {
    // Evacuate value in data labels
    value_ptr = *(((uintptr_t **)data_lab_ptr) + offset);
    *value_ptr = evacuate(*value_ptr);
  }

  do_scan_stack();

  // We Are Done And Can Now Insert from-space Into The FreeList
  from_space_end->n = freelist;
  freelist = from_space_begin;

  // If major GC run through all infinite regions and free all large
  // objects that have not been visited (are not marked as constant);
  // unmark large objects in the process.  For a minor GC, no large
  // objects are freed; but large objects need to be unmarked, which
  // is done when running clear_scan_container below.

#ifdef ENABLE_GEN_GC
  if ( is_major_p )
#endif
  for( r = TOP_REGION ; r ; r = r->p )
      {
	Lobjs *lobjs;
	long first = 1;
	Lobjs **lobjs_ptr = &(r->lobjs); // last live next-slot
	lobjs = r->lobjs;
	while ( lobjs )
	  {
	    uintptr_t* tag_ptr;
#ifdef PROFILING
	    tag_ptr = &(*(&(lobjs->value) + sizeObjectDesc));
#else
	    tag_ptr = &(lobjs->value);
#endif
	    if ( is_const(*tag_ptr)
#ifdef ENABLE_GEN_GC
		 || is_minor_p  /* Preserve all objects in a minor gc */
#endif /* ENABLE_GEN_GC */
		 )
	      {                               // preserve object
		// *tag_ptr = clear_const_bit(*tag_ptr);
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
		lobjs_current -= size_lobj(*tag_ptr);
		orig = lobjs->orig;
		lobjs = clear_lobj_bit(lobjs->next);
		free(orig);            // deallocate object
	      }
	  }

	if ( first )
	  *lobjs_ptr = NULL;
	else
	  *lobjs_ptr = set_lobj_bit(NULL);
      }

  // Unmark all tospace bits in region pages in regions on the stack
  // Update colorPtr in all region pages.

  // MEMO: I don't understand why old generation pages are marked
  // during a minor collection - all what should be necessary is to
  // update colorPtr to the generation allocation pointer in last
  // pages in old generation, but this should happen before garbage
  // collection; the colorPtr is used in old generations only to help gc
  // determine that an object points into to-space. mael 2005-03-19

  // MEMO: It is not nice that we need to run through all pages during
  // a minor collection - this means that we basically traverse ALL
  // live data during a minor collection. A better solution would be
  // the following: During a minor collection, the colorPtr is used as
  // the sole mechanism for determining if an object in an old
  // generation is part of to-space (allocated memory during
  // gc). After gc, we mark black all non-black data in old generation
  // pages. This can be done efficiently, by storing information about
  // old-generation last-pages before gc proper, so that the
  // information is available after gc. mael 2005-03-19


  for( r = TOP_REGION ; r ; r = r->p )
    {
      clear_tospace_bit_and_set_colorPtr_in_gen(&(r->g0));
#ifdef ENABLE_GEN_GC
      clear_tospace_bit_and_set_colorPtr_in_gen(&(r->g1));
#endif /* ENABLE_GEN_GC */
    }

  lobjs_gc_treshold = (long)(heap_to_live_ratio * (double)lobjs_current);

  // Reset all constant-bits in the scan container -- FINITE REGIONS
  // and LARGE OBJECTS -- this clearance is safe because we have
  // freed only those large objects that are unmarked and thus do
  // not occur in the scan container...
  clear_scan_container();

#ifdef CHECK_GC
  check_all_lobjs();  // debugging
#endif // CHECK_GC

  rp_used = rp_total - size_free_list();

  if ( verbose_gc || report_gc )
    {
      ////printf("in gc\n");
      getrusage(RUSAGE_SELF, &rusage_end);
      time_gc_one_ms = // 10x ms to get better precision in reporting
	((rusage_end.ru_utime.tv_sec+rusage_end.ru_stime.tv_sec)*10000 +
	 (rusage_end.ru_utime.tv_usec+rusage_end.ru_stime.tv_usec)/100) -
	((rusage_begin.ru_utime.tv_sec+rusage_begin.ru_stime.tv_sec)*10000 +
	 (rusage_begin.ru_utime.tv_usec+rusage_begin.ru_stime.tv_usec)/100);
      time_gc_all_ms += time_gc_one_ms;
#ifdef ENABLE_GEN_GC
      if ( !is_minor_p ) // this test should appear before we may set major_p below
	{
	  time_majorgc_all_ms += time_gc_one_ms;
	}
#endif
    }

  // Update the GC treshold for region pages - we add -1.0 to
  // leave room for copying...
  //rp_gc_treshold = (int)((heap_to_live_ratio - 1.0) * (double)rp_total / heap_to_live_ratio);
  if ( (int)((heap_to_live_ratio - 1.0) * (double)rp_used) > rp_gc_treshold )
    {
#ifdef ENABLE_GEN_GC
      if ( is_minor_p )
	major_p = 1;
      else
	{
	  major_p = 0;
#endif // ENABLE_GEN_GC
	  rp_gc_treshold = (int)((heap_to_live_ratio - 1.0) * (double)rp_used);
#ifdef ENABLE_GEN_GC
	}
#endif // ENABLE_GEN_GC
    }
#ifdef ENABLE_GEN_GC
  else
    {
      major_p = 0;
    }
#endif // ENABLE_GEN_GC

  // ratio = (double)rp_total / (double)rp_used;
  // if ( ratio < heap_to_live_ratio ) {
  // to_allocate = heap_to_live_ratio * (double)rp_used - (double)rp_total;
  // callSbrkArg((int)to_allocate + REGION_PAGE_BAG_SIZE);
  // }

  if ( verbose_gc )
    {
      double RI = 0.0, GC = 0.0, FRAG = 0.0;
      size_t bytes_to_space;
      size_t pages_to_space;
      //size_t copied_bytes = alloc_period;
      bytes_to_space = allocated_bytes_in_regions(); // ok gengc
      pages_to_space = allocated_pages_in_regions(); // ok gengc
      lobjs_aftergc = allocated_bytes_in_lobjs();  // ok gengc
      alloc_period = alloc_period_save;            // ok gengc
      alloc_total += alloc_period;                 // ok gengc
      alloc_total += lobjs_period;                 // ok gengc

      // +------- Run -------+------- GC ------+----- Run -- -
      // R0        dR        R1                R2
      // L0        dL        L1                L2
      //
      // R_ri = R0 + dR - R1   (bytes in regions freed by RI)
      // L_ri = L0 + dL - L1   (bytes in lobjs freed by RI)
      //
      // R_gc = R2 - R1        (bytes in regions freed by GC)
      // L_gc = L2 - L1        (bytes in lobjs freed by GC)
      //
      // P_ri = 100 * (R_ri + L_ri) / (R_ri + L_ri + R_gc + L_gc)
      // P_gc = 100 * (R_gc + L_gc) / (R_ri + L_ri + R_gc + L_gc)

      gc_total += (bytes_from_space + lobjs_beforegc - bytes_to_space - lobjs_aftergc);

      fprintf(stderr,"(%ld.%ldms)", time_gc_one_ms / 10, time_gc_one_ms % 10);

      //// fprintf(stderr, " rp_total: %ld\n", rp_total);
      //// fprintf(stderr, " size_scan_stack: %ld\n", (size_scan_stack*sizeof(void *)) / 1024);
      //// fprintf(stderr, " size_scan_container: %ld\n", (size_scan_container*sizeof(void *)) / 1024);
      //// fprintf(stderr, " to_space_old     - R0: %ld\n", to_space_old);
      //// fprintf(stderr, " alloc_period     - dR: %ld\n", alloc_period);
      //fprintf(stderr, " alloc_period_save: %ld\n", alloc_period_save);
      //// fprintf(stderr, " bytes_from_space - R1: %ld\n", bytes_from_space);
      //// fprintf(stderr, " bytes_to_space   - R2: %ld\n", bytes_to_space);
      //// fprintf(stderr, " copied_bytes     - (=R2?): %ld\n", copied_bytes);
      //// fprintf(stderr, " lobjs_beforegc: %ld\n", lobjs_beforegc);
      //// fprintf(stderr, " lobjs_aftergc: %ld\n", lobjs_aftergc);
      //// fprintf(stderr, " lobjs_period: %ld\n", lobjs_period);
      //// fprintf(stderr, " lobjs_aftergc_old: %ld\n", lobjs_aftergc_old);

      double P_ri = 0.0;
      double P_gc = 0.0;
      if ( num_gc != 1 )
	{
	  double R0, R1, R2, L0, L1, L2, R_ri, L_ri, R_gc, L_gc, dR, dL;
	  R0 = (double)to_space_old;
	  R1 = (double)bytes_from_space;
	  R2 = (double)bytes_to_space;
	  L0 = (double)lobjs_aftergc_old;
	  L1 = (double)lobjs_beforegc;
	  L2 = (double)lobjs_aftergc;
	  dL = (double)lobjs_period;
	  dR = (double)alloc_period;
	  R_ri = R0 + dR - R1;
	  L_ri = L0 + dL - L1;
	  R_gc = R1 - R2;
	  L_gc = L1 - L2;
	  P_ri = 100.0 * (R_ri + L_ri) / (R_ri + L_ri + R_gc + L_gc);
	  P_gc = 100.0 * (R_gc + L_gc) / (R_ri + L_ri + R_gc + L_gc);

	  RI = 100.0 * ( ((double)((double)to_space_old + (double)lobjs_aftergc_old + (double)alloc_period +
				   (double)lobjs_period - (double)bytes_from_space - (double)lobjs_beforegc)) /
			 ((double)((double)to_space_old + (double)lobjs_aftergc_old + (double)alloc_period +
				   (double)lobjs_period - (double)bytes_to_space - (double)lobjs_aftergc)));

	  GC = 100.0 * ( ((double)(bytes_from_space + lobjs_beforegc
				   - bytes_to_space - lobjs_aftergc)) /
			 ((double)(to_space_old + lobjs_aftergc_old
				   + alloc_period + lobjs_period -
				   bytes_to_space - lobjs_aftergc)));

	  FRAG = 100.0 - 100.0 * (((double)(bytes_from_space + lobjs_beforegc)) /
				  ((double)(sizeof(void *)*ALLOCATABLE_WORDS_IN_REGION_PAGE*pages_from_space
					    + lobjs_beforegc)));
	  FRAG_sum = FRAG_sum + FRAG;
	}

      fprintf(stderr,"%ldkb(%2.0f%%)+L%zdkb -> %ldkb(%2.0f%%)+L%zdkb, FL:%zdkb, ",
	      pages_from_space,
	      region_utilize(pages_from_space, bytes_from_space),
	      lobjs_beforegc / 1024,
	      pages_to_space,
	      region_utilize(pages_to_space, bytes_to_space),
	      lobjs_aftergc / 1024,
	      size_free_list());
      fprintf(stderr, "RI:%2.0lf%%, GC:%2.0lf%%, S:%luMb]\n",
	      P_ri, P_gc, (size_t)(stack_bot_gc - stack_top_gc) / 1024 / 1024);

      to_space_old = bytes_to_space;
      lobjs_aftergc_old = lobjs_aftergc;
      lobjs_period = 0;
      alloc_period = 0;
    }

  time_to_gc = 0;
  doing_gc = 0; // Mutex on the garbage collector

  if (raised_exn_interupt)
    raise_exn((uintptr_t)&exn_INTERRUPT);
  if (raised_exn_overflow)
    raise_exn((uintptr_t)&exn_OVERFLOW);
  return;
}

void
report_dataspace(void) {
  printf ("data_begin_addr = %p\ndata_end_addr = %p\ndifference = %td\n",
	  data_begin_addr, data_end_addr,
	  data_end_addr - data_begin_addr);
  return;
}

#endif // ENABLE_GC
