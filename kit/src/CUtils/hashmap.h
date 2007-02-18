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

typedef struct
{
  void *key;
  void *value;
  int used;
} hashmember;

typedef struct
{
  hashmember *table;
  unsigned long hashTableSize;
  unsigned long hashTableUsed;
  unsigned long (*hash_function) (const void *key);
  int (*equal_function) (const void *key1, const void *key2);
} hashtable;

// allocate memory to table and setup things
// Return: hash_OK or hash_OUTOFMEM
int
hashinit (hashtable * tinfo, unsigned long (*hash) (const void *key),
	  int (*equal) (const void *key1, const void *key2));

// loose the old table and start over
// Return: hash_OK or hash_OUTOFMEM
int hashreinit (hashtable * tinfo);

// erase the entry with key
// Return: hash_DNE or hash_OK
int hasherase (hashtable * tinfo, void *key);

// update an already present entry or insert a new one
// Return hash_OK or hash_OUTOFMEM
// If hash_ERR is returned something really bad has occured
//   this indicates a bug
int hashupdate (hashtable * tinfo, void *key, void *value);

// insert a new entry if an entry with the same key is present
// hashinsert returns hash_AE
// Return: hash_AE or hash_OK
int hashinsert (hashtable * tinfo, void *key, void *value);

// free internal used memory
int hashclose (hashtable * tinfo);

// lookup an entry
// Return: hash_DNE or hash_OK
int hashfind (hashtable * tinfo, const void * const key, void ** const returnValue);

// fold
void *hashfold(hashtable *tinfo, void *(*f)(void *value, void *acc), void *first);

// Fold
void *hashFold(hashtable *tinfo, void *(*f)(void *key, void *value, void *acc), void *first);

// apply
void hashapply(hashtable *tinfo, void (*f)(void *value));

// Apply
void hashApply(hashtable *tinfo, void (*f)(void *key,void *value));

// Map
void hashMap(hashtable *tinfo, void* (*f)(void *key, void *value));

// map
void hashmap(hashtable *tinfo, void* (*f)(void *value));

// an efficient hashfunction on char arrays
unsigned long charhashfunction (char *key);
#endif
