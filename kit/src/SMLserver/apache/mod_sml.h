#ifndef MOD_SML_H_
#define MOD_SML_H_

#include "time.h"
#include "stdint.h"
#include "../../Runtime/LoadKAM.h"
#include "httpd.h"
#include "apr_proc_mutex.h"
#include "apr_global_mutex.h"
#include "apr_shm.h"
#include "../../CUtils/polyhashmap.h"
#include "cache.h"
#include "../../Runtime/Exception.h"
#include "parseul.h"

#define APSML_PATH_MAX 255

int charEqual(const char *, const char *);

DECLARE_NHASHMAP(cachetable, cache *, char *, const, const)
DECLARE_NHASHMAP(conftable, char *, char *, const, const)

typedef struct			/*{{{ */
{
  cachetable_hashtable_t *ht;
  apr_pool_t *pool;
  apr_thread_rwlock_t *rwlock;
} cache_hashtable_with_lock;

typedef struct			/*{{{ */
{
  conftable_hashtable_t *ht;
  apr_pool_t *pool;
  apr_thread_rwlock_t *rwlock;
} conf_hashtable_with_lock;

typedef struct 
{
  apr_shm_t *shm;
  char *shmname;
  unsigned long *version;
  int shmsize;
  apr_proc_mutex_t *plock;
  char *plockname;
} cachelocks;

typedef struct
{
  apr_global_mutex_t *lock;
  int input;
  char *glockname;
  pid_t pid;
} schedule_t;

struct db_t
{
  struct db_t *next;
  int num;
  void *dbspec;
  void (*child_init)(void *, int, apr_pool_t *, server_rec *);
  void (*tmp_shutdown)(void *, server_rec *);
  void (*req_cleanup)(void *, void *);
};

typedef struct
{
  Interp *interp;
  char *prjid;
  char *trapscript;
  char *initscript;
  char *smlpath;
  char *auxdata;
  int extendedtyping;
  char *ulFileName;
  time_t timeStamp;
  cache_hashtable_with_lock *cachetable;
  conf_hashtable_with_lock *conftable;
  pid_t pid;
  pid_t mainproc;
  time_t starttime;
  int initDone;
  cachelocks cachelock;
  schedule_t sched;
  struct db_t *db;
  apr_thread_mutex_t *dblock;
  parseul_hashtable_t *smlTable;
  char *filebuf;
  unsigned long filebufLength;
} InterpContext;

struct request_db
{
  struct request_db *next;
  int num;
  void *dbdata;
};

/*
struct slist
{
  struct slist *next;
  int sz;
  char data;
};
*/

typedef struct
{
  apr_pool_t *pool;
  request_rec *request;
  server_rec *server;
  cache_hashtable_with_lock *cachetable;
  InterpContext *ctx;
  struct request_db *dbdata;
  //  struct slist *postdata;
} request_data;			/*}}} */

#endif
