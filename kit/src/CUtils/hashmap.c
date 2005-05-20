#include <stdlib.h>
#include "hashmap.h"

static int hashrehash (hashtable * tinfo);

int
hashinit (hashtable * tinfo, unsigned long (*hash) (void *key),
    int (*equal) (void *key1, void *key2))
{
  tinfo->table = NULL;
  if (hashreinit (tinfo) == hash_OUTOFMEM)
    return hash_OUTOFMEM;
  tinfo->hash_function = hash;
  tinfo->equal_function = equal;
  return hash_OK;
}

int
hashreinit (hashtable * tinfo)
{
  int i;
  void *temp;
  if (tinfo->table)
  {
    temp = tinfo->table;
    tinfo->table = (hashmember *) realloc (tinfo->table,
        sizeof (hashmember) * MINHASHSIZE);
    if (tinfo->table == NULL)
    {
      free (temp);
      tinfo->table = (hashmember *) malloc (sizeof (hashmember) * MINHASHSIZE);
    }
  }
  else
  {
    tinfo->table = (hashmember *) malloc (sizeof (hashmember) * MINHASHSIZE);
  }
  if (tinfo->table == NULL) return hash_OUTOFMEM;
  for (i = 0; i < MINHASHSIZE; i++)
    {
      tinfo->table[i].used = 0;
    }
  tinfo->hashTableSize = MINHASHSIZE;
  tinfo->hashTableUsed = 0;
  return hash_OK;
}

int
hashclose (hashtable * tinfo)
{
  free (tinfo->table);
  return hash_OK;
}

int
hashfind (hashtable * tinfo, void *key, void **returnValue)
{
  hashmember *table = tinfo->table;
  unsigned long hashval = (*(tinfo->hash_function)) (key);
  do
    {
      hashval %= tinfo->hashTableSize;
      if (table[hashval].used == 0)
  {
    return hash_DNE;
  }
      hashval++;
    }
  while (!((*(tinfo->equal_function)) (table[hashval - 1].key, key)));
  *returnValue = table[hashval - 1].value;
  return hash_OK;
}

int
hasherase (hashtable * tinfo, void *key)
{
  unsigned long i, tmp;
  hashmember *table = tinfo->table;
  unsigned long hashval = (*(tinfo->hash_function)) (key);
  do
    {
      hashval %= tinfo->hashTableSize;
      if (table[hashval].used == 0)
	{
	  return hash_DNE;
	}
      hashval++;
    }
  while (!((*(tinfo->equal_function)) (table[hashval - 1].key, key)));
  hashval--;
  i = (hashval + 1) % tinfo->hashTableSize;
  while (table[i].used)
    {
      tmp = (*(tinfo->hash_function)) (table[i].key) % tinfo->hashTableSize;
      if (!((hashval < tmp && tmp <= i) ||
	    (tmp <= i && i < hashval) || (i < hashval && hashval < tmp)))
	{
	  table[hashval].value = table[i].value;
	  table[hashval].key = table[i].key;
	  table[hashval].used = table[i].used;
	  hashval = i;
	}
      i = (i + 1) % tinfo->hashTableSize;
    };
  table[hashval].used = 0;
  tinfo->hashTableUsed--;
  hashrehash (tinfo);
  return hash_OK;
}

int
hashupdate (hashtable * tinfo, void *key, void *value)
{
  hashmember *table;
  unsigned long hashval;
  if (hashrehash (tinfo)) return hash_OUTOFMEM;
  table = tinfo->table;
  hashval = (*(tinfo->hash_function)) (key) % tinfo->hashTableSize;
  while (1)
    {
      if (table[hashval].used == 0)
	{
	  table[hashval].key = key;
	  table[hashval].value = value;
	  table[hashval].used = 1;
	  tinfo->hashTableUsed++;
	  return hash_OK;
	}
      else if ((*(tinfo->equal_function)) (table[hashval].key, key))
	{
	  table[hashval].key = key;
	  table[hashval].value = value;
	  return hash_OK;
	}
      else
	{
	  hashval++;
	  hashval = hashval % tinfo->hashTableSize;
	}
    }
  // Never reached
  return hash_ERR;
}

int
hashinsert (hashtable * tinfo, void *key, void *value)
{
  hashmember *table;
  unsigned long hashval;
  if (hashrehash (tinfo))
    return hash_OUTOFMEM;
  table = tinfo->table;
  hashval = (*(tinfo->hash_function)) (key) % tinfo->hashTableSize;
  while (table[hashval].used)
    {
      if ((*(tinfo->equal_function)) (table[hashval].key, key))
	{
	  return hash_AE;
	}
      hashval++;
      hashval = hashval % tinfo->hashTableSize;
    };
  table[hashval].key = key;
  table[hashval].value = value;
  table[hashval].used = 1;
  tinfo->hashTableUsed++;
  return hash_OK;
}

// return 0 if everything is OK 1 otherwise (malloc fail)
static int
hashrehash (hashtable * tinfo)
{
//      char a[1000];
  unsigned long newsize;
  hashmember *newtable;
  unsigned long i, hashval;
  unsigned long tmpupper = tinfo->hashTableSize / 1024;
  unsigned long tmplower = tinfo->hashTableSize % 1024;

  if (tinfo->hashTableUsed >=
      (tmpupper * MAXHASH + (tmplower * MAXHASH) / 1024))
    {
      newsize = 2 * tinfo->hashTableSize + 3;
    }
  else 
    {
      if ((tinfo->hashTableUsed >= ((tinfo->hashTableSize / 1024) * MINHASH))
	  || tinfo->hashTableSize == MINHASHSIZE)
	{
	  return 0;
	}
      else
	{
	  newsize = (tinfo->hashTableSize - 3) / 2;
	}
    }
  newtable = (hashmember *) malloc (sizeof (hashmember) * newsize);
  if (newtable == NULL)
    return 1;
  for (i = 0; i < newsize; i++)
    {
      newtable[i].used = 0;
    }
  for (i = 0; i < tinfo->hashTableSize; i++)
    {
      if (tinfo->table[i].used)
	{
	  hashval = (*(tinfo->hash_function)) (tinfo->table[i].key) % newsize;
	  while (newtable[hashval].used)
	    {
	      hashval++;
	      hashval %= newsize;
	    }
	  newtable[hashval].key = tinfo->table[i].key;
	  newtable[hashval].value = tinfo->table[i].value;
	  newtable[hashval].used = 1;
	}
    }
  free (tinfo->table);
  tinfo->table = newtable;
  tinfo->hashTableSize = newsize;
  return 0;
}

// apply
void 
hashapply(hashtable *tinfo, void (*f)(void *value))
{
  hashmember *table = tinfo->table;
  int i;
  for (i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      (*f)(table[i].value);
    }
  }
  return;
}

// Apply
void 
hashApply(hashtable *tinfo, void (*f)(void *key,void *value))
{
  hashmember *table = tinfo->table;
  int i;
  for (i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      (*f)(table[i].key,table[i].value);
    }
  }
  return;
}

// map
void 
hashmap(hashtable *tinfo, void* (*f)(void *value))
{
  hashmember *table = tinfo->table;
  int i;
  for (i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      table[i].value = (*f)(table[i].value);
    }
  }
  return;
}

// Map
void 
hashMap(hashtable *tinfo, void* (*f)(void *key,void *value))
{
  hashmember *table = tinfo->table;
  int i;
  for (i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      table[i].value = (*f)(table[i].key,table[i].value);
    }
  }
  return;
}

// fold
void *
hashfold(hashtable *tinfo, void *(*f)(void *, void *), void *first)
{
  hashmember *table = tinfo->table;
  int i;
  void *t = first;
  for(i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      t = (*f)(table[i].value, t);
    }
  }
  return t;
}

// Fold
void *
hashFold(hashtable *tinfo, void *(*f)(void *, void *, void *), void *first)
{
  hashmember *table = tinfo->table;
  int i;
  void *t = first;
  for(i = 0; i < tinfo->hashTableSize; i++)
  {
    if (table[i].used)
    {
      t = (*f)(table[i].key, table[i].value, t);
    }
  }
  return t;
}

// char * -> unsigned long 
unsigned long
charhashfunction (char *key)
{
  unsigned long result = 0;
  unsigned char *k = (unsigned char *) key;
  while (*k)
    {
      // Will not overflow as the type is unsigned
      result = 31 * result + *k++;
    }
  return result;
}
