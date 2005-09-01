#include "stdlib.h"
#include "httpd.h"
#include "http_log.h"
#include "apr_pools.h"
#include "apr_thread_rwlock.h"
#include "apr_thread_mutex.h"
#include "../../Runtime/Region.h"
#include "time.h"
#include "mod_sml.h"
#include "cache.h"
#include "../../Runtime/String.h"


#define LINKEDLIST_REMOVE(ENTRY) {(ENTRY)->up->down = (ENTRY)->down; \
		(ENTRY)->down->up = (ENTRY)->up;}

#define LINKEDLIST_INSERTUNDER(A,B) {(A)->down->up = (B); (B)->down = (A)->down; \
		(A)->down = (B); (B)->up = (A);}

#define LINKEDLIST_INSERTOVER(A,B) {(A)->up->down = (B); (B)->up = (A)->up; \
		(A)->up = (B); (B)->down = (A);}

#define MIN(a,b) (a < b ? a : b)
#define MAX(a,b) (a < b ? b : a)

void ppCache (cache * c, request_data * rd);

static int 
order (entry **e1, entry **e2)/*{{{*/
{
  if ((*e1)->time < (*e2)->time) return -1;
  if ((*e1)->time == (*e2)->time) return 0;
  return 1;
}/*}}}*/

static void
newpos(entry **e, unsigned long pos)/*{{{*/
{
  (*e)->heappos = pos;
  return;
}/*}}}*/

static void
mysetkey (entry **e, time_t t)/*{{{*/
{
  (*e)->time = t;
  return;
}/*}}}*/

DEFINE_BINARYHEAP(cacheheap, order, newpos, mysetkey)

// return 1 if key1 == key2 and 0 if key1 <> key2
int
keyNhashEqual (void *key1, void *key2)	/*{{{ */
{
  keyNhash *k1 = (keyNhash *) key1;
  keyNhash *k2 = (keyNhash *) key2;
  if (k1->hash == k2->hash && strcmp (k1->key, k2->key) == 0)
    return 1;
  return 0;
}				/*}}} */

unsigned long
hashfunction (void *kn1)	/*{{{ */
{
  // We compute and save the hashvalue once
  return ((keyNhash *) kn1)->hash;
}				/*}}} */

// void ppCache(cache *, request_data *);

void
globalCacheTableInit (request_data * rd)	/*{{{ */
{
  rd->cachetable = (hashtable_with_lock *)
    apr_palloc (rd->pool, sizeof (hashtable_with_lock));
  rd->cachetable->ht = (hashtable *)
    apr_palloc (rd->pool, sizeof (hashtable));
  rd->cachetable->pool = rd->pool;
  apr_thread_rwlock_create (&(rd->cachetable->rwlock), rd->pool);
  hashinit (rd->cachetable->ht, hashfunction, keyNhashEqual);
}				/*}}} */

static cache *
cacheCreate (int maxsize, int timeout, unsigned long hash, request_data *rd)	/*{{{ */
{
  // create pool for cache to live in
  apr_pool_t *p;
  apr_pool_create (&p, (apr_pool_t *) NULL);
  cache *c = (cache *) apr_palloc (p, sizeof (cache));
  c->pool = p;
  c->hashofname = hash;
  apr_proc_mutex_lock(rd->ctx->cachelock.plock);
  unsigned long cachehash = hash % rd->ctx->cachelock.shmsize;
  c->version = rd->ctx->cachelock.version[cachehash];
  apr_proc_mutex_unlock(rd->ctx->cachelock.plock);

  // setup locks
  apr_thread_rwlock_create (&(c->rwlock), c->pool);
  apr_thread_mutex_create (&(c->mutex), APR_THREAD_MUTEX_DEFAULT, c->pool);

  // setup linked list
  c->sentinel = (entry *) apr_palloc (c->pool, sizeof (entry));
  c->sentinel->up = c->sentinel;
  c->sentinel->down = c->sentinel;
  c->sentinel->key.key = NULL;
  c->sentinel->key.hash = 0;
  c->sentinel->data = NULL;
  c->sentinel->size = 0;

  // setup hashtable & binary heap
  c->htable = (hashtable *) apr_palloc (c->pool, sizeof (hashtable) + sizeof(cacheheap_binaryheap_t));
  c->heap = (cacheheap_binaryheap_t *) (c->htable + 1);
  hashinit (c->htable, hashfunction, keyNhashEqual);
  cacheheap_heapinit(c->heap);

  // calculate size
  c->size = c->htable->hashTableSize * sizeof (hashmember);
  c->maxsize = maxsize;

  // set timeout scheme
  c->timeout = timeout;
  return c;
}				/*}}} */

// ML: string * int * int -> cache ptr_option
cache *
apsml_cacheCreate (String cacheName1, int maxsize, int timeout, request_data * rd)	/*{{{ */
{
  keyNhash kn1;
  kn1.key = &(cacheName1->data);
  kn1.hash = charhashfunction (kn1.key);
  apr_thread_rwlock_wrlock (rd->cachetable->rwlock);
  cache *c;
  void **c1 = (void **) &c;
//  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
//    "apsml_cacheCreate: cacheName == %s, maxsize == %i, timeout == %i",
//    &(cacheName1->data), maxsize, timeout);
  if (hashfind (rd->cachetable->ht, &kn1, c1) == hash_DNE)
    {
      int size = sizeStringDefine(cacheName1) + 1 + sizeof (keyNhash);
      keyNhash *kn = malloc (size);
//      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
//		    "apsml_cacheCreate: malloc 0x%x, length:%d", (unsigned long) kn, size);
      
      if (kn == NULL) return NULL;
      kn->key = (char *) (kn + 1);
      kn->hash = kn1.hash;
      strncpy (kn->key, kn1.key, sizeStringDefine(cacheName1));
      kn->key[sizeStringDefine(cacheName1)] = 0;
      c = cacheCreate (maxsize, timeout, kn1.hash, rd);
      hashinsert (rd->cachetable->ht, kn, c);
    }
  else
    {
      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
		    "apsml_cacheCreate: cacheName == %s already exists",
		    &(cacheName1->data));
    }
//  ppGlobalCache(rd);
  apr_thread_rwlock_unlock (rd->cachetable->rwlock);
  return c;
}				/*}}} */

// ML: string -> cache ptr_option
cache *
apsml_cacheFind (char *cacheName, request_data *rd)	/*{{{ */
{
//  ppGlobalCache(rd);
  apr_thread_rwlock_rdlock (rd->cachetable->rwlock);
  cache *c;
  void **c1 = (void **) &c;
//  ap_log_error(__FILE__,__LINE__, LOG_DEBUG, 0, rd->server, "apsml_cacheFind: cacheName == %s", cacheName);
  keyNhash kn;
  kn.key = cacheName;
  kn.hash = charhashfunction (cacheName);
  if (hashfind (rd->cachetable->ht, &kn, c1) == hash_DNE)
    {
//        ap_log_error(__FILE__,__LINE__, LOG_NOTICE, 0, rd->server, "apsml_cacheFind: cacheName == %s not in main cache", cacheName);
      c = NULL;
    }
//  ap_log_error(__FILE__,__LINE__, LOG_NOTICE, 0, rd->server, "apsml_cacheFind: 2");
  apr_thread_rwlock_unlock (rd->cachetable->rwlock);
  return c;
}				/*}}} */


// ML : cache * string -> string_ptr
String
apsml_cacheGet (Region rAddr, cache *c, String key1, request_data *rd)	/*{{{ */
{
//  ap_log_error(__FILE__,__LINE__, LOG_DEBUG, 0, rd->server, "apsml_cacheGet 1");
//  ppCache (c, rd);

  keyNhash kn;
  kn.key = &(key1->data);
  kn.hash = charhashfunction (kn.key);
//  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//		"apsml_cacheGet: key == %s, hash: %i", kn.key, kn.hash);
  int too_old = 0;
  apr_thread_rwlock_rdlock (c->rwlock);
  apr_proc_mutex_lock(rd->ctx->cachelock.plock);
  unsigned long cachehash = c->hashofname % rd->ctx->cachelock.shmsize;
  unsigned long cacheversion = rd->ctx->cachelock.version[cachehash];
  apr_proc_mutex_unlock(rd->ctx->cachelock.plock);

//  ap_log_error(__FILE__,__LINE__, LOG_DEBUG, 0, rd->server, 
//          "apsml_cacheGet global version: %d, local version %d", cacheversion, c->version);

  if (cacheversion != c->version)
  {
    apr_thread_rwlock_unlock (c->rwlock);
    apsml_cacheFlush(c, rd, 0);
    c->version = cacheversion;
    return (String) NULL;
  }
  entry *entry;
  void **entry1 = (void **) &entry;
  if (hashfind (c->htable, &kn, entry1) == hash_DNE)
  {
    entry = NULL;
//    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//	   "apsml_cacheGet: No such thing");
  }
  if (entry)
  {
    // we found an entry 
    // which should be put on top of the list
    // We require locking on the list
    // If time is too old then drop the entry

    time_t ct = time (NULL);
    apr_thread_mutex_lock (c->mutex);

    LINKEDLIST_REMOVE (entry);
    //time_t t = ct < entry->time ? 0 : ct - entry->time;
    if (entry->timeout)
    {
      if (ct > entry->time)
	    {			// entry too old
	      too_old = 1;
	      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
			    "apsml_cacheGet: Entry too old, ct == %i, entry->time == %i",
			    ct, entry->time);
	      LINKEDLIST_INSERTOVER (c->sentinel, entry);
	    }
      else
	    {
	      // keep entry fresh
	      LINKEDLIST_INSERTUNDER (c->sentinel, entry);
//	      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//			    "apsml_cacheGet: Entry fine ct == %i, entry->time == %i",
//			    ct, entry->time);
        cacheheap_heapchangekey(c->heap, entry->heappos, MAX (ct + entry->timeout, entry->time));
	      //entry->time = MAX (ct + entry->timeout, entry->time);
	    }
	  }
//    ap_log_rerror(__FILE__,__LINE__, LOG_NOTICE, 0, rd->request, "apsml_cacheGetFound: etime: %d, rtime: %d, too_old: %i key: %s, value %d, valuedata: %s", entry->time, time(NULL), too_old, key, entry, entry->data);
     apr_thread_mutex_unlock (c->mutex);
  }
  String s;
  if (too_old == 0 && entry)
  {
    s = convertStringToML (rAddr, entry->data);
  }
  else
  {
    s = (String) NULL;
  }
  apr_thread_rwlock_unlock (c->rwlock);
//    ap_log_rerror(__FILE__,__LINE__, LOG_NOTICE, 0, rd->request, "apsml_cacheGet: Returning");
  return s;
}				/*}}} */

void
listremoveitem (cache * c, entry * e, request_data *rd)	/*{{{ */
{
  if (e->timeout) cacheheap_heapdelete (c->heap, e->heappos);
  LINKEDLIST_REMOVE (e);
  c->size -= e->size;
  // free old entry
//      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
//		    "apsml_cacheCreate: free 0x%x", (unsigned long) e);
  free (e);
}				/*}}} */

void
cacheremoveitem (cache * c, entry * e, request_data *rd)	/*{{{ */
{
  hasherase (c->htable, &(e->key));
  listremoveitem (c, e, rd);
}				/*}}} */

void
ppCache (cache * c, request_data * rd)	/*{{{ */
{
  int pid = getpid ();
  unsigned long htsize = c->htable->hashTableSize;
  unsigned long htused = c->htable->hashTableUsed;
  int i;
  hashmember *table = c->htable->table;
  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
		"ppCache: pid %d, size %d, used %d", pid, htsize, htused);
  for (i = 0; i < htsize; i++)
  {
    if (table[i].used)
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "ppCache: index: %i, key: %s, keyhash: %i, value: %d, valuedata: %s, heappos: %d, time: %d",
        i, ((keyNhash *) (table[i].key))->key,
        ((keyNhash *) (table[i].key))->hash, table[i].value,
        ((entry *) table[i].value)->data,
        ((entry *) table[i].value)->heappos,
        ((entry *) table[i].value)->time);
    }
    else
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "ppCache: index %i not used, value: %i", i,
        table[i].value);
    }
  }
}				/*}}} */

void
ppGlobalCache (request_data * rd)	/*{{{ */
{
  int pid = getpid ();
  unsigned long htsize = rd->cachetable->ht->hashTableSize;
  unsigned long htused = rd->cachetable->ht->hashTableUsed;
  int i;
  hashmember *table = rd->cachetable->ht->table;
  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
		"ppGlobalCache: pid %d, size %d, used %d", pid, htsize,
		htused);
  for (i = 0; i < htsize; i++)
    {
      if (table[i].used)
	{
	  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
			"ppGlobalCache: index: %i, key: %s, value: %d", i,
			((keyNhash *) (table[i].key))->key, table[i].value);
	}
      else
	{
	  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
			"ppGlobalCache: index: %i not used, key: , value: %i",
			i, table[i].value);
	}
    }
}				/*}}} */

// ML: cache * String * String -> (int * string_ptr)
int
apsml_cacheSet (int resultPair, Region sAddr, cache * c, int keyValPair, request_data * rd)	/*{{{ */
{
  // allocate new entry and key,value placeholders
// ppCache(c, rd);
  String key1 = (String) elemRecordML (keyValPair, 0);
  String value1 = (String) elemRecordML (keyValPair, 1);
  time_t timeout = (time_t) elemRecordML (keyValPair, 2);
  char *value = &(value1->data);
  int valuesize = sizeStringDefine(value1);
  char *key = &(key1->data);
  int keysize = sizeStringDefine(key1);
  int size = sizeof (entry) + keysize + 1 + valuesize + 1;
  entry *newentry =
    (entry *) malloc (size);
//      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
//		    "apsml_cacheCreate: malloc 0x%x, length: %d, sizeof(entry): %d, keysize: %d, valuesize: %d, key: %s, val: %s", (unsigned long) newentry, size, sizeof(entry), keysize, valuesize, key, value);
  if (newentry == NULL)
    return 0;
  char *newkey = (char *) (newentry + 1);
  char *newvalue = newkey + (keysize + 1);

  // prepare entry by copy data to my space and set pointers
  strncpy (newkey, key, keysize);
  newkey[keysize] = 0;
  strncpy (newvalue, value, valuesize);
  newvalue[valuesize] = 0;
  newentry->key.key = newkey;
  newentry->key.hash = charhashfunction (newkey);
  newentry->data = newvalue;
  newentry->size = keysize + valuesize + sizeof (entry) + 2;
  time_t ct = time (NULL);
  if (timeout && c->timeout)
  {
    newentry->timeout = MIN (timeout, c->timeout);
  }
  else if (timeout)
  {
    newentry->timeout = timeout;
  }
  else
  {
    newentry->timeout = c->timeout;
  }
  newentry->time = ct + newentry->timeout;

  // We are going in !!! (as we get a writes lock we have 
  // complete control [no more locks])
  apr_thread_rwlock_wrlock (c->rwlock);
  int tmpsize = c->htable->hashTableSize;
  entry *oldentry = NULL;
  void **oldentry1 = (void **) &oldentry;
//  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//		"apsml_cacheSet: key %s, hash: %i", key, newentry->key.hash);
  if (hashfind (c->htable, &(newentry->key), oldentry1) == hash_DNE)
  {
    // No old entry with that key 
    if ((newentry->timeout == -1 
            && cacheheap_heapinsert(c->heap, newentry, (time_t) 0) == heap_OUTOFMEM) 
        || (newentry->timeout && newentry->timeout != (time_t) -1
            && cacheheap_heapinsert(c->heap, newentry, newentry->time) == heap_OUTOFMEM))
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "apsml_cacheSet: pid %d, received heap_OUTOFMEM", rd->ctx->pid);
      free(newentry);
      newentry = 0;
    }
    if (newentry && hashinsert (c->htable, &(newentry->key), newentry) == hash_OUTOFMEM)
	  {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "apsml_cacheSet: pid %d, received hash_OUTOFMEM", rd->ctx->pid);
	    free (newentry);
	    newentry = 0;
	  }
//  ppCache(c, rd);
    oldentry = 0;
  }
  else
  {
    // Old exists
    if ((newentry->timeout == (time_t) -1 
           && cacheheap_heapinsert(c->heap, newentry, (time_t) 0) == heap_OUTOFMEM)
        || (newentry->timeout && newentry->timeout != (time_t) -1
           && cacheheap_heapinsert(c->heap, newentry, newentry->time) == heap_OUTOFMEM))
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "apsml_cacheSet: pid %d, received heap_OUTOFMEM", rd->ctx->pid);
      free(newentry);
      newentry = 0;
    }
    if (newentry && hashupdate (c->htable, &(newentry->key), newentry) == hash_OUTOFMEM)
	  {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "apsml_cacheSet: pid %d, received hash_OUTOFMEM", rd->ctx->pid);
	    free (newentry);
	    newentry = 0;
	  }
  }

  if (newentry)
  {
    c->size += newentry->size;
    c->size += (tmpsize - c->htable->hashTableSize) * sizeof (hashmember);
  }
//  ppCache(c, rd);
  int too_old = 0;
  if (oldentry)
  {
    // Old entry needs removel
    // time_t t = (ct - oldentry->time) < 0 ? 0 : ct - oldentry->time;
    if (oldentry->timeout && ct > oldentry->time)
	  {
	    too_old = 1;
	    second (resultPair) = 0;
	  }
    else
    {
      second (resultPair) = (int) convertStringToML (sAddr, oldentry->data);
    }
    listremoveitem (c, oldentry, rd);
  }
  if (too_old)
    oldentry = 0;
  if (newentry)
    LINKEDLIST_INSERTUNDER (c->sentinel, newentry);
  // I think we are done now
//  ppCache(c, rd);
  entry *curentry;
//  ap_log_error(__FILE__,__LINE__, LOG_NOTICE, 0, rd->server, 
//          "apsml_cacheSet: size %d, maxsize = %d, ct: %d", c->size, c->maxsize, ct);
  while (cacheheap_heapminimal(c->heap, &curentry) != heap_UNDERFLOW)
  {
    if (curentry->time < ct)
    {
      cacheremoveitem(c, curentry, rd);
    }
    else break;
  }
//  ppCache(c, rd);
  if (c->maxsize != -1)
  {
    while (c->size > c->maxsize)
    {
      curentry = c->sentinel->up;
      if (curentry == c->sentinel)
        break;
      cacheremoveitem (c, curentry, rd);
    }
  }
  apr_thread_rwlock_unlock (c->rwlock);
//  ppCache(c, rd);
  if (oldentry && newentry)
    {
      first (resultPair) = 1;
      return resultPair;
    }
  second (resultPair) = 0;
  if (newentry)
    {
      first (resultPair) = 2;
      return resultPair;
    }
  first (resultPair) = 0;
  return resultPair;
}				/*}}} */

int
apsml_cacheFlush (cache * c, request_data *rd, int global)	/*{{{ */
{
  apr_thread_rwlock_wrlock (c->rwlock);
  if (global)
  {
    apr_proc_mutex_lock(rd->ctx->cachelock.plock);
    unsigned long cachehash = c->hashofname % rd->ctx->cachelock.shmsize;
    rd->ctx->cachelock.version[cachehash]++;
    apr_proc_mutex_unlock(rd->ctx->cachelock.plock);
  }
  while (c->sentinel->down != c->sentinel)
  {
    listremoveitem (c, c->sentinel->down, rd);
  }
  if (hashreinit (c->htable) == hash_OUTOFMEM)
  {
    apr_thread_rwlock_unlock (c->rwlock);
    return 0;
  }
  c->size = c->htable->hashTableSize * sizeof (hashmember);
  apr_thread_rwlock_unlock (c->rwlock);
  return 1;
}				/*}}} */

void
cacheDestroy (cache * c, request_data *rd)	/*{{{ */
{
  while (c->sentinel->down != c->sentinel)
    {
      listremoveitem (c, c->sentinel->down, rd);
    }
  cacheheap_heapclose(c->heap);
  hashclose (c->htable);
  apr_pool_clear (c->pool);
  return;
}				/*}}} */
