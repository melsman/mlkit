#include <stdlib.h>
#include <string.h>
#include "Error.h"
#include "Types.h"

/***************************************************************************
 * Functions for allocating space to graph structure.                      *
 ***************************************************************************/

void *xrealloc(void *p, int n)
{
  void *r;

  r = realloc(p, n);
  if (!r) {
    /*NOTREACHED*/
    Disaster("sorry, out of memory");
  }
  return r;
}

/*
 * Allocate memory for data structures. The memory is parcelled out in large 
 * chunks to reduce the overhead of calling malloc().
 */
void* xmalloc(int n)
{
  void *r;

  r = malloc(n);
  if (!r) {
    /*NOTREACHED*/
    Disaster("sorry, out of memory");
  }
  return r;
}

char* MallocString(char *s)
{
  char* r;

  r = (char*) xmalloc(strlen(s)+1);
  strcpy(r, s);
  return (r);
}

 
#undef  N_MALLOC 
#define N_MALLOC        1000

static SAMPLEPTR samplepool;
static int samplepoolsize = 0;
static SAMPLEPTR samplefree = 0;

SAMPLEPTR MallocSample(void)
{
  SAMPLEPTR t;

  if(samplefree) {
    t = samplefree;
    samplefree = t->next;
  } else {
    if (samplepoolsize == 0)
      samplepool = (SAMPLEPTR) xmalloc(N_MALLOC * sizeof(SAMPLENODE));

    t = samplepool + samplepoolsize; 

    samplepoolsize = (samplepoolsize + 1) % N_MALLOC;
  }
  return (t);
}

void FreeSample(SAMPLEPTR s)
{
  s->next = samplefree;
  samplefree = s;
}

#undef  N_MALLOC  
#define N_MALLOC        200 

static ENTRYPTR entrypool;
static int entrypoolsize = 0; 

ENTRYPTR MallocEntry(void)
{
  ENTRYPTR t;
  
  if (entrypoolsize == 0) {
    entrypool = (ENTRYPTR) xmalloc(N_MALLOC * sizeof(ENTRYNODE));
  }   
 
  t = entrypool + entrypoolsize;
  
  entrypoolsize = (entrypoolsize + 1) % N_MALLOC;
 
  return (t);
}
