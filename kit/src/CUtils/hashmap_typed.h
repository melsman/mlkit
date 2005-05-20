#ifndef _HASHMAP_TYPED_H
#define _HASHMAP_TYPED_H

#include "hashmap.h"

// a new function for generating new hash tables (uses malloc)
hashtable* 
hashnew(unsigned long (*hash) (void *key),
	int (*equal) (void *key1, void *key2));

// as hashupdate, but exits with an error message in case of an error
void hashupd (hashtable * tinfo, void *key, void *value);

// free all entries in hashtable and memory allocated 
// for the hashtable struct
void hashdrop (hashtable*);

typedef unsigned long (*hash_key_function)(void*);
typedef int (*eq_key_function)(void*,void*);

#define DECLARE_HASHMAP(name,ktype,vtype) \
typedef struct name *name; \
typedef unsigned long (* name ## _hash_key_function)(ktype k); \
typedef int (* name ## _eq_key_function)(ktype k1,ktype k2); \
name new_ ## name (name ## _hash_key_function, name ## _eq_key_function); \
void name ## _upd (name ht, ktype k, vtype v); \
int name ## _find (name ht, ktype k, vtype* v_ptr); \
int name ## _erase (name ht, ktype k); \
void name ## _drop (name ht); \
void name ## _apply (name ht, void (*f)(ktype,vtype));

#define DEFINE_HASHMAP(name,ktype,vtype) \
name \
new_ ## name (name ## _hash_key_function hash, name ## _eq_key_function eq) \
{ \
 return (name)hashnew((hash_key_function)hash,(eq_key_function)eq); \
} \
void \
name ## _upd (name ht, ktype k, vtype v) \
{ \
 hashupd((hashtable*)ht,(void*)k,(void*)v); \
 return; \
} \
int \
name ## _find (name ht, ktype k, vtype* v_ptr) \
{ \
 return hashfind((hashtable*)ht,(void*)k,(void**)v_ptr); \
} \
int \
name ## _erase(name ht, ktype k) \
{ \
 return hasherase((hashtable*)ht, (void*)k); \
} \
void \
name ## _drop(name ht) \
{ \
 hashdrop((hashtable*)ht); \
} \
void \
name ## _apply (name ht, void (*f)(ktype,vtype)) \
{ \
  hashApply((hashtable*)ht,(void(*)(void*,void*))f); \
}
#endif
