#ifndef _HASHMAP_H
#define _HASHMAP_H

// Smallest possible hashtable
#define MINHASHSIZE 10
// Fill ratio before rehash
#define MINHASH 150
#define MAXHASH 800

enum
{
  hash_OK = 0,
  hash_DNE,			// did not exists
  hash_FULL,			// table full
  hash_AE,			// allready exists
  hash_ERR,
  hash_OUTOFMEM			// Out of memory
};

#define DECLARE_HASHMAP(name,elemtype,keytype,elemqual,keyqual)\
typedef elemtype name ## _valuetype_t; \
typedef elemqual elemtype name ## _valuetype_tc; \
typedef keytype name ## _keytype_t; \
typedef keyqual keytype name ## _keytype_tc; \
typedef struct \
{ \
  name ## _keytype_tc key; \
  name ## _valuetype_tc value; \
  int used; \
} name ## _hashelement_t; \
typedef struct \
{ \
  name ## _hashelement_t *table; \
  unsigned long hashTableSize; \
  unsigned long hashTableUsed; \
  unsigned long (*hash_function) (name ## _keytype_tc key);  \
  int (*equal_function) (name ## _keytype_tc key1, name ## _keytype_tc key2); \
} name ## _hashtable_t; \
int name ## _hashinit (name ## _hashtable_t *tinfo); \
int name ## _hashreinit (name ## _hashtable_t *tinfo); \
int name ## _hasherase (name ## _hashtable_t *tinfo, name ## _keytype_tc key); \
int name ## _hashupdate (name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc value); \
int name ## _hashinsert (name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc value); \
int name ## _hashclose (name ## _hashtable_t *tinfo); \
int name ## _hashfind (const name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc *returnValue); \
void * name ## _hashfold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _valuetype_tc value, void *acc), void *first); \
void * name ## _hashFold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _keytype_tc key, name ## _valuetype_tc value, void *acc), void *first); \
void name ## _hashapply(const name ## _hashtable_t *tinfo, void (*f)(name ## _valuetype_tc value)); \
void name ## _hashApply(const name ## _hashtable_t *tinfo, void (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)); \
void name ## _hashMap(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)); \
void name ## _hashmap(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _valuetype_tc value));

// an efficient hashfunction on char arrays
unsigned long charhashfunction (const char *key);
#include "polyhashmap.c"
#endif
