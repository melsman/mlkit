#include <stdlib.h>
// #include "hashmap.h"

#define DEFINE_HASHMAP(name,hash,equal) \
static int name ## _hashrehash (name ## _hashtable_t * tinfo); \
int name ## _hashinit (name ## _hashtable_t * tinfo) \
{ \
  tinfo->table = NULL; \
  if (name ## _hashreinit (tinfo) == hash_OUTOFMEM) \
    return hash_OUTOFMEM; \
  tinfo->hash_function = hash; \
  tinfo->equal_function = equal; \
  return hash_OK; \
} \
int name ## _hashreinit (name ## _hashtable_t * tinfo) \
{ \
  int i; \
  name ## _hashelement_t *temp; \
  if (tinfo->table) \
  { \
    temp = tinfo->table; \
    tinfo->table = (name ## _hashelement_t *) realloc (tinfo->table, \
        sizeof (name ## _hashelement_t) * MINHASHSIZE); \
    if (tinfo->table == NULL) \
    { \
      free (temp); \
      tinfo->table = (name ## _hashelement_t *) malloc (sizeof (name ## _hashelement_t) * MINHASHSIZE); \
    } \
  } \
  else \
  { \
    tinfo->table = (name ## _hashelement_t *) malloc (sizeof (name ## _hashelement_t) * MINHASHSIZE); \
  } \
  if (tinfo->table == NULL) return hash_OUTOFMEM; \
  for (i = 0; i < MINHASHSIZE; i++) \
    { \
      tinfo->table[i].used = 0; \
    } \
  tinfo->hashTableSize = MINHASHSIZE; \
  tinfo->hashTableUsed = 0; \
  return hash_OK; \
} \
int \
name ## _hashclose (name ## _hashtable_t * tinfo) \
{ \
  free (tinfo->table); \
  return hash_OK; \
} \
int \
name ## _hashfind (const name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc *returnValue) \
{ \
  name ## _hashelement_t *table = tinfo->table; \
  unsigned long hashval = (*(tinfo->hash_function)) (key); \
  do \
    { \
      hashval %= tinfo->hashTableSize; \
      if (table[hashval].used == 0) \
	{ \
	  return hash_DNE; \
	} \
      hashval++; \
    } \
  while (!((*(tinfo->equal_function)) (table[hashval - 1].key, key))); \
  *returnValue = table[hashval - 1].value; \
  return hash_OK; \
} \
int \
name ## _hasherase (name ## _hashtable_t * tinfo, name ## _keytype_tc key) \
{                                                                         \
  unsigned long i, tmp;                                                   \
  name ## _hashelement_t *table = tinfo->table;                                       \
  unsigned long hashval = (*(tinfo->hash_function)) (key);                \
  do                                                                      \
    {                                                                     \
      hashval %= tinfo->hashTableSize;                                    \
      if (table[hashval].used == 0)                                       \
	{                                                                       \
	  return hash_DNE;                                                      \
	}                                                                       \
      hashval++;                                                          \
    }                                                                     \
  while (!((*(tinfo->equal_function)) (table[hashval - 1].key, key)));    \
  hashval--;                                                              \
  i = (hashval + 1) % tinfo->hashTableSize;                               \
  while (table[i].used)                                                   \
    {                                                                     \
      tmp = (*(tinfo->hash_function)) (table[i].key) % tinfo->hashTableSize; \
      if (!((hashval < tmp && tmp <= i) ||                                \
	    (tmp <= i && i < hashval) || (i < hashval && hashval < tmp)))       \
	{                                                                       \
	  table[hashval].value = table[i].value;                                \
	  table[hashval].key = table[i].key;                                    \
	  table[hashval].used = table[i].used;                                  \
	  hashval = i;                                                          \
	}                                                                       \
      i = (i + 1) % tinfo->hashTableSize;                                 \
    };                                                                    \
  table[hashval].used = 0;                                                \
  tinfo->hashTableUsed--;                                                 \
  name ## _hashrehash (tinfo);                                                     \
  return hash_OK;                                                         \
}                                                                         \
int                                                                       \
name ## _hashupdate (name ## _hashtable_t *tinfo, name ## _keytype_tc key, name ## _valuetype_tc value) \
{                                                                          \
  name ## _hashelement_t *table;                                                       \
  unsigned long hashval;                                                   \
  if (name ## _hashrehash (tinfo)) return hash_OUTOFMEM;                   \
  table = tinfo->table;                                                    \
  hashval = (*(tinfo->hash_function)) (key) % tinfo->hashTableSize;        \
  while (1)                                                                \
    {                                                                      \
      if (table[hashval].used == 0)                                        \
	{                                                                        \
	  table[hashval].key = key;                                              \
	  table[hashval].value = value;                                          \
	  table[hashval].used = 1;                                               \
	  tinfo->hashTableUsed++;                                                \
	  return hash_OK;                                                        \
	}                                                                        \
      else if ((*(tinfo->equal_function)) (table[hashval].key, key))       \
	{                                                                        \
	  table[hashval].key = key;                                              \
	  table[hashval].value = value;                                          \
	  return hash_OK;                                                        \
	}                                                                        \
      else                                                                 \
	{                                                                        \
	  hashval++;                                                             \
	  hashval = hashval % tinfo->hashTableSize;                              \
	}                                                                        \
    }                                                                      \
  return hash_ERR;                                                         \
}                                                                          \
int                                                                        \
name ## _hashinsert (name ## _hashtable_t * tinfo, name ## _keytype_tc key, name ## _valuetype_tc value) \
{                                                                          \
  name ## _hashelement_t *table;                                                       \
  unsigned long hashval;                                                   \
  if (name ## _hashrehash (tinfo))                                         \
    return hash_OUTOFMEM;                                                  \
  table = tinfo->table;                                                    \
  hashval = (*(tinfo->hash_function)) (key) % tinfo->hashTableSize;        \
  while (table[hashval].used)                                              \
    {                                                                      \
      if ((*(tinfo->equal_function)) (table[hashval].key, key))            \
	{                                                                        \
	  return hash_AE;                                                        \
	}                                                                        \
      hashval++;                                                           \
      hashval = hashval % tinfo->hashTableSize;                            \
    };                                                                     \
  table[hashval].key = key;                                                \
  table[hashval].value = value;                                            \
  table[hashval].used = 1;                                                 \
  tinfo->hashTableUsed++;                                                  \
  return hash_OK;                                                          \
} \
static int \
name ## _hashrehash (name ## _hashtable_t * tinfo) \
{ \
  unsigned long newsize; \
  name ## _hashelement_t *newtable; \
  unsigned long i, hashval; \
  unsigned long tmpupper = tinfo->hashTableSize / 1024; \
  unsigned long tmplower = tinfo->hashTableSize % 1024; \
  if (tinfo->hashTableUsed >= \
      (tmpupper * MAXHASH + (tmplower * MAXHASH) / 1024)) \
    { \
      newsize = 2 * tinfo->hashTableSize + 3; \
    } \
  else \
    { \
      if ((tinfo->hashTableUsed >= ((tinfo->hashTableSize / 1024) * MINHASH)) \
	  || tinfo->hashTableSize == MINHASHSIZE) \
	{ \
	  return 0; \
	} \
      else \
	{ \
	  newsize = (tinfo->hashTableSize - 3) / 2; \
	} \
    } \
  newtable = (name ## _hashelement_t *) malloc (sizeof (name ## _hashelement_t) * newsize); \
  if (newtable == NULL) \
    return 1; \
  for (i = 0; i < newsize; i++) \
    { \
      newtable[i].used = 0; \
    } \
  for (i = 0; i < tinfo->hashTableSize; i++) \
    { \
      if (tinfo->table[i].used) \
	{ \
	  hashval = (*(tinfo->hash_function)) (tinfo->table[i].key) % newsize; \
	  while (newtable[hashval].used) \
	    { \
	      hashval++; \
	      hashval %= newsize; \
	    } \
	  newtable[hashval].key = tinfo->table[i].key; \
	  newtable[hashval].value = tinfo->table[i].value; \
	  newtable[hashval].used = 1; \
	} \
    } \
  free (tinfo->table); \
  tinfo->table = newtable; \
  tinfo->hashTableSize = newsize; \
  return 0; \
} \
void \
name ## _hashapply(const name ## _hashtable_t *tinfo, void (*f)(name ## _valuetype_tc value)) \
{ \
  name ## _hashelement_t *table = tinfo->table; \
  int i;                                                                                 \
  for (i = 0; i < tinfo->hashTableSize; i++)                                             \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      (*f)(table[i].value);                                                              \
    }                                                                                    \
  }                                                                                      \
  return;                                                                                \
}                                                                                        \
                                                                                         \
void name ## _hashApply(const name ## _hashtable_t *tinfo, void (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)) \
{                                                                                        \
  name ## _hashelement_t *table = tinfo->table;                                                      \
  int i;                                                                                 \
  for (i = 0; i < tinfo->hashTableSize; i++)                                             \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      (*f)(table[i].key,table[i].value);                                                 \
    }                                                                                    \
  }                                                                                      \
  return;                                                                                \
}                                                                                        \
                                                                                         \
void                                                                                     \
name ## _hashmap(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _valuetype_tc value)) \
{                                                                                        \
  name ## _hashelement_t *table = tinfo->table;                                                      \
  int i;                                                                                 \
  for (i = 0; i < tinfo->hashTableSize; i++)                                             \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      table[i].value = (*f)(table[i].value);                                             \
    }                                                                                    \
  }                                                                                      \
  return;                                                                                \
}                                                                                        \
                                                                                         \
void                                                                                     \
name ## _hashMap(const name ## _hashtable_t *tinfo, name ## _valuetype_t (*f)(name ## _keytype_tc key, name ## _valuetype_tc value)) \
{                                                                                        \
  name ## _hashelement_t *table = tinfo->table;                                                      \
  int i;                                                                                 \
  for (i = 0; i < tinfo->hashTableSize; i++)                                             \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      table[i].value = (*f)(table[i].key,table[i].value);                                \
    }                                                                                    \
  }                                                                                      \
  return;                                                                                \
}                                                                                        \
                                                                                         \
void *                                                                                   \
name ## _hashfold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _valuetype_tc, void *), void *first) \
{                                                                                        \
  name ## _hashelement_t *table = tinfo->table;                                                      \
  int i;                                                                                 \
  void *t = first;                                                                       \
  for(i = 0; i < tinfo->hashTableSize; i++)                                              \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      t = (*f)(table[i].value, t);                                                       \
    }                                                                                    \
  }                                                                                      \
  return t;                                                                              \
}                                                                                        \
                                                                                         \
void *                                                                                   \
name ## _hashFold(const name ## _hashtable_t *tinfo, void *(*f)(name ## _keytype_tc, name ## _valuetype_tc, void *), void *first) \
{                                                                                        \
  name ## _hashelement_t *table = tinfo->table;                                                      \
  int i;                                                                                 \
  void *t = first;                                                                       \
  for(i = 0; i < tinfo->hashTableSize; i++)                                              \
  {                                                                                      \
    if (table[i].used)                                                                   \
    {                                                                                    \
      t = (*f)(table[i].key, table[i].value, t);                                         \
    }                                                                                    \
  }                                                                                      \
  return t;                                                                              \
}
