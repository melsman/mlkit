#include "Flags.h"
#include "Error.h"
#include "Sample.h"
#include "Alloc.h"
#include "Rp2Ps.h"

/***************************************************************************
 * Functions for constructing rp2graph structure.                          *
 ***************************************************************************/

/*
 *      The information associated with each identifier is stored
 *      in a linked list of chunks. The table below allows the list
 *      of chunks to be retrieved given an identifier name.
 */
#define NHASH           513
static ENTRYPTR hashtable[ NHASH ];
static int Hash(char* s)
{
  int r;
  
  for (r = 0; *s; s++) {
    r = r + r + r + *s;
  }

  if (r < 0)
    r = -r;
  
  return r % NHASH;
}

/*
 * GetEntry() -- Get the entry associated with "name", creating a new entry
 *               if necessary.
 */
static ENTRYPTR GetEntry(char* name)
{
  int h;
  ENTRYPTR e;

  h = Hash(name);

  for (e = hashtable[ h ]; e; e = e->next)
    if (strcmp(e->name, name) == 0)
      break;

  if (e) {
    return (e);
  } else {
    e = MallocEntry();
    e->name = MallocString(name);
    e->next = hashtable[ h ];
    e->samples = 0;
    hashtable[ h ] = e;
    return (e);
  }
}

/*
 * MakeIdentTable() -- The hash table is useful while reading the input,
 *                     but it becomes a liability thereafter. The code
 *                     below converts it to a more easily processed table.
 */
void MakeIdentTable()
{
  int i;
  ENTRYPTR e;

  #if CHAT
  printf("MakeIdentTable -- ENTER\n");
  #endif

  nidents = 0;

  for (i = 0; i < NHASH; i++) {
    for (e = hashtable[ i ]; e; e = e->next) {
      if(e->samples)
        nidents++;
    }
  }

  identtable = (ENTRYPTR*) xmalloc(nidents * sizeof(ENTRYPTR));

  nidents = 0;

  for (i = 0; i < NHASH; i++) {
    for (e = hashtable[ i ]; e; e = e->next) {
      if(e->samples)
        identtable[ nidents++ ] = e;
    }
  }

  #if CHAT
  printf("MakeIdentTable -- LEAVE\n");
  #endif

  return;
}

/* Allocates a new sample in sampletable.     */
/* SampleNo has to be smaller than sampleMax. */
void allocNewSample(int sampleNo, float sampleTime)
{

  if (sampleNo > SampleMax-1)
    Disaster("allocNewSample -- sampleNo larger than SampleMax-1.");

  #if DEBUG_ALLOC_SAMPLE
    printf("allocNewSample %3d, with sampletime %8.2f.\n", sampleNo, sampleTime);
  #endif

  sampletable[sampleNo] = sampleTime;
  nsamples++; /* may be set that elsewhere. */

  return;

}

/*
 *  StoreSampleEntry() -- Store information from a sample entry.
 */
void storeSampleEntry(int sampleNo, float sampleTime, char* id, float nbytes)
{
  ENTRYPTR e;
  SAMPLEPTR s;

  #if DEBUG_STORE_SAMPLE_ENTRY
    printf("StoreSampleEntry(%s), with sampleNo %d, sampleTime %5.2f and nbytes %5.0f\n", id, sampleNo, sampleTime, nbytes);
  #endif

  e = GetEntry(id);

  s = MallocSample();
  s->n = sampleNo;
  s->t = sampleTime;
  s->nbytes = nbytes;
  s->next = e->samples;
  e->samples = s;

  return;
}

void addComment(float commentTime, char *comment)
{
  if (ncomments >= ncommentmax) {
    if (!commenttable) {
      ncommentmax = N_MARKS;
      commenttable = (float*) xmalloc(ncommentmax * sizeof(float));
      commentstring = (char **) xmalloc(ncommentmax * sizeof(char *));
    } else {
      ncommentmax *= 2;
      commenttable = (float*) xrealloc(commenttable, ncommentmax * sizeof(float));
      commentstring = (char **) xrealloc(commentstring, ncommentmax * sizeof(char *));
    }
  }
  commenttable[ncomments] = commentTime;
  commentstring[ncomments++]  = MallocString(comment);

  return;
}

void addMark(float markTime)
{
  if (nmarks >= nmarkmax) {
    if (!marktable) {
      nmarkmax = N_MARKS;
      marktable = (float*) xmalloc(nmarkmax * sizeof(float));
    } else {
      nmarkmax *= 2;
      marktable = (float*) xrealloc(marktable, nmarkmax * sizeof(float));
    }
  }
  marktable[ nmarks++ ] = markTime;
  
  return;
}

void printIdentTable(void)
{
  int i;
  ENTRYPTR e;
  SAMPLEPTR s;

  for (i=0;i<nidents;i++) {
    e = identtable[i];
    printf("Identifier %s\n", e->name);
    for (s=e->samples;s;s=s->next) {
      printf("   Sample %d\n", s->n);
      printf("     Time   %10.4f\n", s->t);
      printf("     nBytes %10.4f\n", s->nbytes);
    }
  }
  return;
}

/***************************************************************************
 * We have to sort the ident table, such that the smallest are first       *
 ***************************************************************************/

/*
 * SortIdentTable() -- Compute the total volume for each identifier, and the 
 *                     grand total of these totals. The identifiers whose totals 
 *                     when added together amount to less that a threshold 
 *                     percentage (default 0%) of the grand total are considered 
 *                     to be ``trace elements'' and they are thrown away. 
 */
void SortIdentTable(void)
{
  int i;
  int j;
  float grandtotal;
  int min;
  float t;
  float p;
  float* totals; 
  ENTRYPTR e;
  SAMPLEPTR s;
  
  #if CHAT
  printf("SortIdentTable -- ENTER\n");
  #endif

  totals = (float*) xmalloc(nidents * sizeof(float));

  /* find totals */
  for (i = 0; i < nidents; i++) {
    totals[ i ] = 0;
  }
 
  for (i = 0; i < nidents; i++) {
    e = identtable[ i ];
    for (s = e->samples; s; s = s->next) {
      totals[ i ] += s->nbytes;
    }
  }

  /* sort on the basis of total */
  for (i = 0; i < nidents - 1; i++) {
    min = i;
    for (j = i+1; j < nidents; j++) {
      if (totals[ j ] < totals[ min ]) {
	min = j;
      }
    }    
    t = totals[ min ];
    totals[ min ] = totals[ i ];
    totals[ i ] = t;
    
    e = identtable[ min ];
    identtable[ min ] = identtable[ i ];
    identtable[ i ] = e;
  }
  
  /* find the grand total */
  grandtotal = 0;
  for (i = 0; i < nidents; i++) {
    grandtotal += totals[ i ];
  }

  t = 0;    /* cumulative percentage */
   
  for (i = 0; i < nidents; i++) {
    p = (100 * (float) totals[i]) / grandtotal;
    t = t + p; 
    if (t >= THRESHOLD_PERCENT) {
      break;
    }
  }

  /* identifiers from 0 to i-1 should be removed */
  for (j = 0; i < nidents; i++, j++) {
    identtable[ j ] = identtable[ i ]; 
  }

  nidents = j;
  free(totals);

  #if CHAT
  printf("SortIdentTable -- LEAVE\n");
  #endif

  return;
}
