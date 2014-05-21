#include "hashmap_typed.h"
#include <stdlib.h>
#include <stdio.h>

static void
die (char *s) 
{ 
  fprintf(stderr,"Hashmap internal error: %s\n",s); 
  exit(-1); 
}

hashtable* 
hashnew(unsigned long (*hash) (void *key),
	int (*equal) (void *key1, void *key2)) 
{
  hashtable *ht = (hashtable *) malloc (sizeof (hashtable));
  if ( ht == 0 )
    {
      die("hashnew: Cannot allocate memory for hash table");
    }
  if ( hashinit (ht,hash,equal) == hash_OUTOFMEM )
    {
      die ("hashnew: Failed to initialize hash table");
    }
  return ht;
}

void
hashupd (hashtable * ht, void *k, void *v)
{
  if ( hashupdate (ht,k,v) != hash_OK )
    {
      die ("hashupd: Out of memory or internal error");
    }
  return;
}

void 
hashdrop (hashtable* ht)
{
  hashclose(ht);
  free(ht);
}
