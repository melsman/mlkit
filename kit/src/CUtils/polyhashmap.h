#ifndef _HASHMAP_H
#define _HASHMAP_H

/* Smallest possible hashtable */
#define MINHASHSIZE 10
/* Fill ratio before rehash */
#define MINHASH 150
#define MAXHASH 800

enum
{
  hash_OK = 0,
  hash_DNE,			/* did not exists */
  hash_FULL,			/* table full */
  hash_AE,			/* allready exists */
  hash_ERR,
  hash_OUTOFMEM			/* Out of memory */
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
/* allocate memory to table and setup things \
   Return: hash_OK or hash_OUTOFMEM */ \
int name ## _init (name ## _hashtable_t *tinfo); \
/* loose the old table and start over \
   Return: hash_OK or hash_OUTOFMEM */\
int name ## _reinit (name ## _hashtable_t *tinfo); \
/* erase the entry with key \
   Return: hash_DNE or hash_OK */ \
int name ## _erase (name ## _hashtable_t *tinfo, name ## _keytype_tc key); \
/* update an already present entry or insert a new one \
   Return hash_OK or hash_OUTOFMEM \
   If hash_ERR is returned something really bad has occured \
     this indicates a bug */ \
int name ## _update (name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc value); \
/* insert a new entry if an entry with the same key is present \
   hashinsert returns hash_AE \
   Return: hash_AE or hash_OK */ \
int name ## _insert (name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc value); \
/* free internal used memory */ \
int name ## _close (name ## _hashtable_t *tinfo); \
/* lookup an entry \
   Return: hash_DNE or hash_OK */ \
int name ## _find (const name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc *returnValue); \
void * name ## _fold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _valuetype_tc value, void *acc), void *first); \
void * name ## _Fold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _keytype_tc key, name ## _valuetype_tc value, void *acc), void *first); \
void name ## _apply(const name ## _hashtable_t *tinfo, void (*f)(name ## _valuetype_tc value)); \
void name ## _Apply(const name ## _hashtable_t *tinfo, void (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)); \
void name ## _Map(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)); \
void name ## _map(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _valuetype_tc value));

#include "polyhashmap.c"
#endif
