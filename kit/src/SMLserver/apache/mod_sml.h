#ifndef MOD_SML_H_
#define MOD_SML_H_

#include "time.h"
#include "stdint.h"
#include "../../Runtime/LoadKAM.h"
#include "httpd.h"
#include "apr_proc_mutex.h"
#include "apr_global_mutex.h"
#include "apr_shm.h"
#include "../../CUtils/hashmap.h"
#include "cache.h"
#include "../HashTable.h"
#include "../../Runtime/Exception.h"

#define APSML_PATH_MAX 255


typedef struct			/*{{{ */
{
  hashtable *ht;
  apr_pool_t *pool;
  apr_thread_rwlock_t *rwlock;
} hashtable_with_lock;

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
  int extendedtyping;
  char *ulFileName;
  time_t timeStamp;
  HashTable scripts;
  hashtable_with_lock *cachetable;
  hashtable_with_lock *conftable;
  pid_t pid;
  pid_t mainproc;
  time_t starttime;
  int initDone;
  cachelocks cachelock;
  schedule_t sched;
  struct db_t *db;
  apr_thread_mutex_t *dblock;
  hashtable *smlTable;
  char *filebuf;
  unsigned long filebufLength;
} InterpContext;

struct request_db
{
  struct request_db *next;
  int num;
  void *dbdata;
};

typedef struct
{
  apr_pool_t *pool;
  request_rec *request;
  server_rec *server;
  hashtable_with_lock *cachetable;
  InterpContext *ctx;
  struct request_db *dbdata;
} request_data;			/*}}} */

typedef struct
{
  uint32_t length;
  uint32_t first;
  uint32_t interval;
  uint32_t type;
  uint32_t port;
  uint32_t serverlength;
} schedHeader;


#endif
