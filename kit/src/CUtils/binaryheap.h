#ifndef _BINARYHEAP_H
#define _BINARYHEAP_H

// #include "stdlib.h"

// Smallest posible heaparray
#define MINHEAPARRAY 10
// When to descrease heaparray
#define MINHEAP 8

enum
{
  heap_OK = 0,
  heap_OUTOFMEM,		// Out Of memory
  heap_UNDERFLOW,
  heap_DNE			// Do not exists
};

#define DECLARE_BINARYHEAP(name,elemtype,keytype) \
  typedef elemtype name ## _heapelement_t;\
  typedef keytype name ## _keytype_t; \
  typedef struct \
  { \
    name ## _heapelement_t *heaptable; \
    unsigned long size; \
    unsigned long maxsize; \
    int (*order) (name ## _heapelement_t *elem1, name ## _heapelement_t *elem2); \
    void (*newpos) (name ## _heapelement_t *elem, unsigned long pos); \
    void (*setkey) (name ## _heapelement_t *elem, name ## _keytype_t newkey); \
  } name ## _binaryheap_t; \
int \
name ## _heapinit (name ## _binaryheap_t *heap); \
int name ## _heapreinit (name ## _binaryheap_t *heap); \
int name ## _heapminimal (name ## _binaryheap_t *heap, name ## _heapelement_t *rv); \
int name ## _heapextractmin (name ## _binaryheap_t *heap, name ## _heapelement_t *rv); \
int name ## _heapchangekey (name ## _binaryheap_t *heap, unsigned long pos, name ## _keytype_t newkey); \
int name ## _heapdelete (name ## _binaryheap_t *heap, unsigned long pos); \
int name ## _heapinsert (name ## _binaryheap_t *heap, name ## _heapelement_t elem, \
                         name ## _keytype_t key);

#include "binaryheap.c"

#endif
