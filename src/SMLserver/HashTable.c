// Hash tables with strings as domain

#include <stdlib.h>
#include <string.h>
#include "HashTable.h"
// #include "ns.h"

static int
stringHash(char *s)
{
  int acc = 0;
  for ( ; *s ; s++ )
    acc = 19 * acc + *s;
  return acc;
}

HashTable 
emptyHashTable(int arraySize)
{
  HashTable h;
  int i;

  // MEMO: arraySize should be rounded up to a power of two minus one

  h = (HashTable)malloc(4 * arraySize + sizeof(struct hashTable));
  h->arraySize = arraySize;
  h->size = 0;
  for ( i = 0 ; i < arraySize ; i ++ )
    {
      h->array[i] = 0;
    }
  return h;
}

static char*
lookupObjectList(ObjectListHashTable *ol, char *key)
{
  for ( ; ol ; ol = ol->next )
    {
      if ( strcmp(ol->key, key) == 0 )
	{
	  return ol->value;
	}
    }
  return 0;
}

char*
lookupHashTable(HashTable h, char* key)
{
  int hash;
  hash = stringHash(key) & (h->arraySize);
  // Ns_Log(Notice, "nssml: lookup (hash=%d)", hash);
  return lookupObjectList(h->array[hash], key);
}

void insertHashTable(HashTable h, char* key, char* value)
{
  int hash;
  ObjectListHashTable *ol_new, *ol_old;
  key = strdup(key);
  hash = stringHash(key) & (h->arraySize);
  ol_old = h->array[hash];
  ol_new = (ObjectListHashTable *)malloc(sizeof(ObjectListHashTable));
  ol_new->key = key;
  ol_new->value = value;
  ol_new->next = ol_old;
  h->array[hash] = ol_new;
  h->size = h->size + 1;
  return;
}

static void
freeObjectList(ObjectListHashTable *ol)
{
  ObjectListHashTable *ol_prev = 0;
  for ( ; ol ; ol = ol->next )
    {
      if ( ol_prev )
	free(ol_prev);
      ol_prev = ol;
    }
  if ( ol_prev )
    {
      free(ol_prev->key);
      free(ol_prev);
    }
  return;
}

void 
freeHashTable(HashTable h)
{
  int i;
  for ( i = 0 ; i < h->arraySize ; i ++ )
    {
      freeObjectList(h->array[i]);
    }
  free(h);
  return;
}

