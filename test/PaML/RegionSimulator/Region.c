/*----------------------------------------------------------------*
 *                        Regions                                 *
 *----------------------------------------------------------------*/
#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>
#include "ri_sim.h"
#include "Region.h"

/*----------------------------------------------------------------*
 * Global declarations                                            *
 *----------------------------------------------------------------*/
ULong bytes_alloc = 0;
Regionpage* freelist;
Regiondesc* topRegion;
static UInt heapNo = 0; // 0 is dynamic, 1 is storage.
static Word cardNo = 0; // Always card number 0.
static UInt heapId = 0; // Set in function set_card_info.

void panic(CharPtr errorStr) { 
  FrmCustomAlert(alertID_panic,errorStr,"","");
  //exit(-1); //How do we exit the application, NH
}

void panicN(CharPtr errorStr, ULong n) { 
  char tmp_text[100];
  StrPrintF(tmp_text, "[%lu] ", n);
  StrCat(tmp_text,errorStr);
  FrmCustomAlert(alertID_panic,tmp_text,"","");
  //exit(-1); //How do we exit the application, NH
}

Regionpage *mem_ptr_new() {
  ULong free, max;
  Err err;
  Regionpage *rp;

  err = MemHeapFreeBytes(heapId, &free, &max);
  if (err)
    panic("mem_ptr_new: MemHeapFreeBytes");
  if (max < 5*1024) {
    // Use storage memory
    panic("Use storage memory.");
    rp = NULL;
  }
  else {
    // Use dynamic memory
    rp = (Regionpage *)MemPtrNew(sizeof(Regionpage));
    if (rp == NULL)
      panic("mem_ptr_new: I cound not allocate more memory");
  }
  return rp;
}

void alloc_regionpages() {
  Regionpage *np;
  ULong m = NUM_REG_PAGES_ALLOC_BY_SBRK;

  freelist = mem_ptr_new();
  m--;

  np = freelist;
  while (m) {
    np->n = mem_ptr_new();
    np = np->n;
    m--;
  }    

  np->n = NULL;
}

ULong *alloc_region(Regiondesc *rdAddr) { 
  Regionpage *rp;
  
  rdAddr = (Regiondesc *) clearStatusBits((ULong)rdAddr);

  if (freelist==NULL) alloc_regionpages();

  rp = freelist;
  freelist = freelist->n;

  rp->n = NULL;

  rdAddr->a = (ULong *)(&(rp->i)); /* We allocate from k.i in the page. */ 
  rdAddr->b = (ULong *)(rp+1);     /* The border is after this page. */
  rdAddr->p = topRegion;	   /* Push this region onto the region stack. */
  rdAddr->fp = rp;                 /* Update pointer to the first page. */
  topRegion = rdAddr;

  /* We have to set the infinitebit. */
  rdAddr = (Regiondesc *) setInfiniteBit((ULong)rdAddr);

  return (ULong *)rdAddr;
}  

ULong *dealloc_region() { 
  ULong *sp;

  sp = (ULong *) topRegion;   /* topRegion points at the bottom of the region 
			       * descriptor on the stack. */

  /* Insert the region pages in the freelist; there is always 
   * at-least one page in a region. */
  (((Regionpage *)topRegion->b)-1)->n = freelist;
  freelist = topRegion->fp;
  topRegion=topRegion->p;

  return sp;
}

/*----------------------------------------------------------------------*
 *alloc:                                                                *
 *  Allocates n words in region rAddr. It will make sure, that there    *
 *  is space for the n words before doing the allocation.               *
 *  Pre-condition: n <= ALLOCATABLE_WORDS_IN_REGION_PAGE                *
 *----------------------------------------------------------------------*/
void get_regionpage_from_freelist(Regiondesc* rd) {
  Regionpage* rp;

  if (freelist==NULL) alloc_regionpages();
 
  rp = freelist;
  freelist= freelist->n;
  rp->n = NULL;

  if (rd->fp)
    (((Regionpage *)(rd->b))-1)->n = rp; /* Updates the next field in the last region page. */
  else
    rd->fp = rp;                         /* Update pointer to the first page. */

  rd->a = (ULong *)(&(rp->i));           /* Updates the allocation pointer. */
  rd->b = (ULong *)(rp+1);               /* Updates the border pointer. */
}

ULong *alloc (ULong rdAddr, int n) { 
  ULong *t1;
  ULong *t2;
  ULong *t3;
  Regiondesc *rd;

  rd = (Regiondesc *) clearStatusBits(rdAddr);

  t1 = rd->a;
  t2 = t1 + n;

  t3 = rd->b;
  if (t2 > t3) {
    get_regionpage_from_freelist(rd);

    t1 = rd->a;
    t2 = t1 + n;
  }
  rd->a = t2;

  return t1;
}

/*----------------------------------------------------------------------*
 *resetRegion:                                                          *
 *  All regionpages except one are inserted into the free list, and     *
 *  the region administration structure is updated. The statusbits are  *
 *  not changed.                                                        *
 *----------------------------------------------------------------------*/
ULong reset_region(ULong rdAddr) { 
  Regiondesc *rd;

  rd = (Regiondesc *) clearStatusBits(rdAddr);

  /* There is always at least one page in a region. */
  if ( (rd->fp)->n != NULL ) {   /* There are more than one page in the region. */
    (((Regionpage *)rd->b)-1)->n = freelist;
    freelist = (rd->fp)->n;
    (rd->fp)->n = NULL;
  }

  rd->a = (ULong *)(&(rd->fp)->i); /* beginning of klump in first page */
  rd->b = (ULong *)((rd->fp)+1);   /* end of klump in first page */

  return rdAddr;                   /* We preserve rdAddr and the status bits. */
}

/*-------------------------------------------------------------------------*
 *deallocateRegionsUntil:                                                  *
 *  Called with rdAddr=sp, which do not nessesaraly point at a region      *
 *  descriptor. It deallocates all regions that are placed over sp.        *
 *  The function does not return or alter anything.                        *
 *-------------------------------------------------------------------------*/
void dealloc_regions_until(ULong rdAddr) { 
  Regiondesc *rd;

  rd = (Regiondesc *) clearStatusBits(rdAddr);
  
  while (rd <= topRegion) 
    dealloc_region();

  return;
} 

void init_runtime_system() {
  heapId = MemHeapID(cardNo,heapNo);
  freelist = NULL;
  topRegion = NULL;
  alloc_regionpages();
}
