#define LEFT(pos) ((pos + 1)*2 -1)
#define RIGHT(pos) ((pos + 1)*2)
#define PARENT(pos) (((pos + 1) / 2) - 1)

#include "stdlib.h"
#include "string.h"

#define MAX(a,b) (a < b ? b : a)

#define DEFINE_BINARYMAP(name,myorder,mynewpos,mysetkey)                             \
static int                                                                           \
name ## _heapresize (name ## _binaryheap_t *heap)                                    \
{                                                                                    \
  unsigned long newsize;                                                             \
  name ## _heapelement_t *tmptable;                                                  \
  if (heap->size == heap->maxsize)                                                   \
  {                                                                                  \
    newsize = 2*heap->size;                                                          \
  }                                                                                  \
  else if (heap->size > heap->maxsize / MINHEAP)                                     \
  {                                                                                  \
    return 0;                                                                        \
  } else                                                                             \
  {                                                                                  \
    newsize = heap->maxsize / (MINHEAP / 2);                                         \
  }                                                                                  \
  newsize = MAX (MINHEAPARRAY, newsize);                                             \
  if (heap->maxsize == newsize) return 0;                                            \
  tmptable = heap->heaptable;                                                        \
  tmptable = (name ## _heapelement_t *)                                              \
              realloc(heap->heaptable, newsize * sizeof(name ## _heapelement_t));    \
  if (tmptable == NULL)                                                              \
  {                                                                                  \
    tmptable = (name ## _heapelement_t *)                                            \
               malloc (newsize * sizeof (name ## _heapelement_t));                   \
    if (tmptable == NULL) return 1;                                                  \
    memmove(tmptable, heap->heaptable, heap->size * sizeof(name ## _heapelement_t)); \
    free(heap->heaptable);                                                           \
  }                                                                                  \
  heap->heaptable = tmptable;                                                        \
  heap->maxsize = newsize;                                                           \
  return 0;                                                                          \
}                                                                                    \
                                                                                     \
int                                                                                  \
name ## _heapinit (name ## _binaryheap_t *heap)                                      \
{                                                                                    \
  heap->heaptable = NULL;                                                            \
  heap->size = 0;                                                                    \
  heap->maxsize = 0;                                                                 \
  return name ## _heapresize (heap);                                                 \
}                                                                                    \
                                                                                     \
int                                                                                  \
name ## _reinit (name ## _binaryheap_t *heap)                                        \
{                                                                                    \
  heap->size = 0;                                                                    \
  heap->maxsize = 0;                                                                 \
  if (name ## _heapresize(heap)) return heap_OUTOFMEM;                               \
  return heap_OK;                                                                    \
}                                                                                    \
                                                                                     \
void static                                                                          \
name ## _heapex (name ## _binaryheap_t *heap, unsigned long pos, unsigned long smallest) \
{                                                                                    \
  name ## _heapelement_t *table;                                                     \
  name ## _heapelement_t temp;                                                         \
  table = heap->heaptable;                                                           \
  temp = table[pos];                                                                 \
  table[pos] = table[smallest];                                                      \
  table[smallest] = temp;                                                            \
  mynewpos (&(table[pos]), pos);                                                 \
  mynewpos (&(table[smallest]), smallest);                                       \
  return;                                                                            \
}                                                                                    \
                                                                                     \
                                                                                     \
void static                                                                          \
name ## _heapify (name ## _binaryheap_t *heap, unsigned long pos)                    \
{                                                                                    \
  name ## _heapelement_t *table = heap->heaptable;                                   \
  unsigned long size = heap->size;                                                   \
  unsigned long l = LEFT (pos);                                                      \
  unsigned long r = RIGHT (pos);                                                      \
  unsigned long smallest;                                                             \
  if (l < size && (myorder (&(table[l]), &(table[pos]))) < 0)                         \
  {                                                                                   \
    smallest = l;                                                                     \
  }                                                                                   \
  else                                                                                \
  {                                                                                   \
    smallest = pos;                                                                   \
  }                                                                                   \
  if (r < size && (myorder  (&(table[r]), &(table[smallest]))) < 0)                   \
  {                                                                                   \
    smallest = r;                                                                     \
  }                                                                                   \
  if (smallest != pos)                                                                \
  {                                                                                   \
    name ## _heapex (heap, pos, smallest);                                            \
    name ## _heapify (heap, smallest);                                                \
  }                                                                                   \
  return;                                                                              \
}                                                                                      \
                                                                                       \
int                                                                                    \
name ## _heapminimal (name ## _binaryheap_t *heap, name ## _heapelement_t *rv)         \
{                                                                                      \
  if (heap->size == 0)                                                                 \
    return heap_UNDERFLOW;                                                             \
  *rv = heap->heaptable[0];                                                            \
  return heap_OK;                                                                      \
}                                                                                      \
                                                                                       \
int                                                                                    \
name ## _heapextractmin (name ## _binaryheap_t *heap, name ## _heapelement_t *rv)      \
{                                                                                      \
  if (heap->size == 0) return heap_UNDERFLOW;                                          \
  *rv = heap->heaptable[0];                                                            \
  heap->heaptable[0] = heap->heaptable[heap->size - 1];                                \
  heap->size--;                                                                        \
  name ## _heapify (heap, 0);                                                          \
  name ## _heapresize(heap);                                                           \
  return heap_OK;                                                                      \
}                                                                                      \
                                                                                       \
int                                                                                    \
name ## _heapchangekey (name ## _binaryheap_t *heap, unsigned long pos,                \
                        name ## _keytype_t newkey)                                     \
{                                                                                      \
  unsigned long p;                                                                     \
  if (pos >= heap->size) return heap_DNE;                                              \
  mysetkey(&(heap->heaptable[pos]), newkey);                                       \
  name ## _heapify (heap, pos);                                                        \
  while (pos > 0 && (myorder (&(heap->heaptable[pos]),                           \
	                                  &(heap->heaptable[(p = PARENT (pos))]))) < 0)      \
  {                                                                                    \
    name ## _heapex (heap, pos, p);                                                             \
    pos = p;                                                                           \
  }                                                                                    \
  return heap_OK;                                                                       \
}                                                                                       \
                                                                                        \
int                                                                                     \
name ## _heapdelete (name ## _binaryheap_t *heap, unsigned long pos)                      \
{                                                                                       \
  if (pos >= heap->size) return heap_DNE;                                               \
  name ## _heapex (heap, pos, heap->size - 1);                                                   \
  heap->size--;                                                                         \
  name ## _heapify (heap, pos);                                                                  \
  name ## _heapresize(heap);                                                            \
  return heap_OK;                                                                       \
}                                                                                       \
                                                                                        \
int                                                                                     \
name ## _heapinsert (name ## _binaryheap_t *heap, name ## _heapelement_t data, name ## _keytype_t key) \
{                                                                                       \
  heap->size++;                                                                         \
  if (name ## _heapresize(heap))                                                        \
  {                                                                                     \
    heap->size--;                                                                       \
    return heap_OUTOFMEM;                                                               \
  }                                                                                     \
  heap->heaptable[heap->size-1] = data;                                                 \
  return name ## _heapchangekey (heap, heap->size - 1, key);                                     \
}
