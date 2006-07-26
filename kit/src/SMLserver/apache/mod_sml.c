// The Apache module to serve SMLserver


#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "../../Runtime/String.h"
#include "mod_sml.h"
#include "Locks.h"
#include "parseFuncs.h"
#include "plog.h"
#include "parseul.h"
#include "sched.h"
#include "../../Runtime/HeapCache.h"
#include "greeting.h"

// #define APSML_SCRIPT_HASHTABLE_SZ 1023

#define DEFAULT_PRJID "sources"
#define DEFAULT_XT 0

#define SHMSIZE 0x1000

enum APSML_RETURNVALUES   /*{{{ */
{
  APSML_FILENOTFOUND = -1,
  APSML_ULFILENOTFOUND = -2,
  APSML_INTERRUPTRAISED = -3,
  APSML_ERROR = -4,
  APSML_OK = 0,
};        /*}}} */


module AP_MODULE_DECLARE_DATA sml_module;


static int apsml_processSmlFile (request_data * rd, char *uri, int kind);

// request_data *globalrd;

#if 0
void
logMsg (char *msg)
{
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, globalrd->server,
    "apsml: Notice; apsml: %s", msg);
}

void
logMsg1 (char *msg, request_data *rd)    /*{{{ */
{
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
    "apsml: Notice; apsml: %s", msg);
//  fprintf (stderr, "Notice; apsml: %s", msg);
}

void
logLoading (char *file)
{
  fprintf (stderr, "Notice, apsml: loaded %s", file);
}       /*}}} */
#endif

static void
logMsg (enum reportLevel level, char *msg, void *rd1)    /*{{{ */
{
  int rep_lev = LOG_DEBUG;
  request_data *rd = (request_data *) rd1;
  switch (level)
  {
    case DEBUG:
      rep_lev = LOG_DEBUG;
      break;
    case INFO:
      rep_lev = LOG_INFO;
      break;
    case NOTICE:
      rep_lev = LOG_NOTICE;
      break;
    case DIE:
      rep_lev = LOG_EMERG;
      break;
  }
  ap_log_error (__FILE__, __LINE__, rep_lev, 0, rd->server,
    "apsml: Notice; apsml: %s", msg);
  return;
} /*}}}*/

//#if 0
void
printserver (server_rec * s)  //{{{
{
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->defn_name is %s", s->defn_name);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->server_admin is %s", s->server_admin);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->server_hostname is %s", s->server_hostname);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->error_fname is %s", s->error_fname);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->path is %s", s->path);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->port is %d", s->port);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->is_virtual is %d", s->is_virtual);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->addrs->host_addr->hostname is %s", s->addrs->host_addr->hostname);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->addrs->host_addr->port is %d", s->addrs->host_addr->port);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->addrs->host_addr->servname is %s", s->addrs->host_addr->servname);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->addrs->virthost is %s", s->addrs->virthost);

  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, s,
    "apsml: server->addrs->host_port is %d", s->addrs->host_port);

  return;
}       //}}}
//#endif

static const char *
set_projid (cmd_parms * cmd, void *mconfig, const char *prjid)  /*{{{ */
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->prjid = (char *) prjid;

  return NULL;
}       /*}}} */

static const char *
set_initscript (cmd_parms * cmd, void *mconfig, const char *initScript) /*{{{ */
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->initscript = (char *) initScript;
  return NULL;
}       /*}}} */

static const char *
set_trapscript (cmd_parms * cmd, void *mconfig, const char *script) /*{{{ */
{
  return NULL;
}       /*}}} */

static const char *
setXt (cmd_parms * cmd, void *mconfig, int flag)  /*{{{ */
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->extendedtyping = flag;
  return NULL;
}       /*}}} */

static const char *
set_sml_path (cmd_parms * cmd, void *mconfig, const char *path)       /*{{{ */
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->smlpath = (char *) path;
  return NULL;
}       /*}}} */

static const char *
set_auxdata(cmd_parms *cmd, void *mconfig, const char *aux)/*{{{*/
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->auxdata = (char *) aux;
  return NULL;
}/*}}}*/

static const 
command_rec mod_sml_cmds[] = /*{{{ */
{
  AP_INIT_TAKE1 ("SmlPrjId", set_projid, NULL, RSRC_CONF,
     "apsml: You must set prjid in the config file"),
  AP_INIT_TAKE1 ("SmlInitScript", set_initscript, NULL, RSRC_CONF,
     "SMLSYNTAX ERR SmlInitScript"),
  AP_INIT_TAKE1 ("SmlTrapScript", set_trapscript, NULL, RSRC_CONF,
     "SMLSYNTAX ERR SmlTrapScript"),
  AP_INIT_TAKE1 ("SmlPath", set_sml_path, NULL, RSRC_CONF,
     "SMLSYNTAX ERR SmlPath"),
  AP_INIT_FLAG ("SmlExtendedTyping", setXt, NULL, RSRC_CONF,
    "SMLSYNTAX ERR SmlExtendedTyping"),
  AP_INIT_TAKE1 ("SmlAuxData", set_auxdata, NULL, RSRC_CONF,
      "SMLSYNTAX ERR SmlAuxData"),
  {NULL}
};        /*}}} */

static void *
perserver_init (apr_pool_t * p, server_rec * s) //{{{
{
  // apr_pcalloc return memory reset to 0
  InterpContext *ctx =
    (InterpContext *) apr_pcalloc (p, sizeof (InterpContext));

  ctx->prjid = DEFAULT_PRJID;
  ctx->extendedtyping = DEFAULT_XT;
  return (void *) ctx;
}       //}}}

unsigned long hashfunction (void *c2);
int keyNhashEqual (void *c1, void *c2);

void static
confinit (server_rec * s, apr_pool_t * pool, hashtable_with_lock ** ct) /*{{{ */
{
  hashtable_with_lock *htwl = (hashtable_with_lock *)
    apr_palloc (pool, sizeof (hashtable_with_lock));
  apr_thread_rwlock_create (&(htwl->rwlock), pool);
  htwl->pool = pool;
  htwl->ht = (hashtable *) apr_palloc (pool, sizeof (hashtable));
  if (hashinit (htwl->ht, hashfunction, keyNhashEqual) != hash_OK)
    {
      ap_log_error (__FILE__, __LINE__, LOG_EMERG, 0, s,
        "Config hash failed");
    }
  *ct = htwl;
  return;
}       /*}}} */

apr_status_t 
shutdownServer (void *ctx1) /*{{{*/
{
  InterpContext *ctx = (InterpContext *) ctx1;
  if (ctx->sched.pid)
  {
    kill(ctx->sched.pid, SIGTERM);
  }
  interpClear(ctx->interp);
  clearHeapCache ();
  clearSmlMap (ctx->smlTable);
  ctx->smlTable = NULL;
  return APR_SUCCESS;
}/*}}}*/

apr_status_t 
shutdownChild (void *ctx1)/*{{{*/
{
  InterpContext *ctx = (InterpContext *) ctx1;
  interpClear(ctx->interp);
  clearHeapCache ();
  clearSmlMap (ctx->smlTable);
  ctx->smlTable = NULL;
  return APR_SUCCESS;
}/*}}}*/

int
dbinit (server_rec *s, apr_pool_t *p, InterpContext *ctx)/*{{{*/
{
  apr_status_t status;
  status = apr_thread_mutex_create(&(ctx->dblock), APR_THREAD_MUTEX_DEFAULT, p);
  if (status != APR_SUCCESS) 
  {
    return 1;
  }
  ctx->db = NULL;
  return 0;
}/*}}}*/

static apr_thread_mutex_t *apache_locks[] = {NULL,NULL,NULL,NULL};

void
runtime_lock(unsigned int i)/*{{{*/
{
  apr_thread_mutex_lock(apache_locks[i]);
  return;
}/*}}}*/

void
runtime_unlock(unsigned int i)/*{{{*/
{
  apr_thread_mutex_unlock(apache_locks[i]);
  return;
}/*}}}*/

static char mlb[] = "/MLB/SMLserver";

int debug_file = -1;

static int
apsml_post_config (apr_pool_t * pconf, apr_pool_t * plog, apr_pool_t * ptemp, server_rec * s) //{{{
{
  int i;
  char *is;
  server_rec *ss;
  void *first_init_check = NULL;
  sml_greeting(s);
  apr_pool_userdata_get (&first_init_check, "mod_sml_first_init_check_HACK",
       s->process->pool);
  if (first_init_check == NULL)
    {       // first init round
      apr_pool_userdata_set ((const void *) 1,
           "mod_sml_first_init_check_HACK",
           apr_pool_cleanup_null, s->process->pool);
      return OK;
    }


  InterpContext *ctx = ap_get_module_config (s->module_config, &sml_module);
  request_data *rd =
    (request_data *) apr_pcalloc (ptemp, sizeof (request_data));
  if (rd == NULL) return 5;
  rd->request = NULL;
  rd->server = s;
  rd->ctx = ctx;
  rd->ctx->initDone = 0;
  rd->dbdata = NULL;
  rd->ctx->sched.pid = 0;
  rd->ctx->smlTable = NULL;
  apr_pool_cleanup_register(pconf, rd->ctx, shutdownServer, shutdownChild);

#ifdef REGION_PAGE_STAT
  rpMap = regionPageMapNew ();
#endif /* REGION_PAGE_STAT */

  // initialize stackPool Mutex, freelist Mutex, and codeCache Mutex
  for(i=0;i<4;i++)
  {
    apr_thread_mutex_create(&(apache_locks[i]), APR_THREAD_MUTEX_DEFAULT, pconf);
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: creating lock %i", i);
  }
//  apr_thread_mutex_create(&(codeCacheMutex), APR_THREAD_MUTEX_DEFAULT, pconf);

  resolveGlobalCodeFragments ();

  ctx->interp = interpNew ();

  rd->pool = pconf;
  globalCacheTableInit (rd);
  ctx->cachetable = rd->cachetable;

  confinit (s, pconf, &ctx->conftable);
  if (dbinit (s, pconf, ctx)) return 5;

//  ppGlobalCache(rd);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
    "apsml: server->path is %s", s->path);

  i = strlen(ctx->smlpath) + strlen(ctx->prjid) + 20 + strlen(mlb);
  ctx->ulFileName = (char *) apr_palloc(pconf, i);
  if (!ctx->ulFileName) return 5;

  sprintf (ctx->ulFileName, "%s%s/%s.ul", ctx->smlpath, mlb, ctx->prjid);

  ss = s;
  while (ss)
  {
    // printserver (ss);
    ss = ss->next;
  }

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
    "apsml: module is now loaded");

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
    "apsml: ulFileName is %s", ctx->ulFileName);

  ctx->mainproc = getpid();

  if (!ctx->smlpath)
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: You must set SmlPath");
    exit(0);
  }

  apr_status_t stat;
  rd->ctx->cachelock.plockname = tempnam(NULL, NULL);
  if (rd->ctx->cachelock.plockname == NULL) return 5;
  stat = apr_proc_mutex_create(&(rd->ctx->cachelock.plock), rd->ctx->cachelock.plockname, 
                               APR_LOCK_DEFAULT, pconf);
  if (stat != APR_SUCCESS)
  {
    return 5;
  }
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: test 1");
  rd->ctx->cachelock.shmname = tempnam(NULL, NULL);
  if (rd->ctx->cachelock.shmname == NULL)
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: Unable to get temporary name from tempnam");
    return APR_EINIT;
  }
  stat = apr_shm_create(&(rd->ctx->cachelock.shm), SHMSIZE, rd->ctx->cachelock.shmname, pconf);
  if (stat != APR_SUCCESS)
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: Unable to create shared memory using apr_shm_create");
    return stat;
  }
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: test 2");
  unsigned long *dbtmp = (unsigned long *) apr_shm_baseaddr_get(rd->ctx->cachelock.shm);
  *dbtmp = 0;
  rd->ctx->cachelock.version = dbtmp + 1;
  rd->ctx->cachelock.shmsize = (apr_shm_size_get(rd->ctx->cachelock.shm) / sizeof(unsigned long)) - 1;
  rd->ctx->starttime = time(NULL);
  rd->ctx->sched.glockname = tempnam(NULL,NULL);
  if (rd->ctx->sched.glockname == NULL) return 5;
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: test 3");
  stat = apr_global_mutex_create(&(rd->ctx->sched.lock), 
                                   rd->ctx->sched.glockname, 
                                   APR_LOCK_DEFAULT, pconf);
  if (stat != APR_SUCCESS)
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: global_mutex_create failed");
    return 5;
  }
  struct sched_init si;
  si = startsched(s->defn_name, s->port);
  if (si.pid == -1) return 5;
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: test 5");
  rd->ctx->sched.pid = si.pid;
  rd->ctx->sched.input = si.input;

    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: created process %d", rd->ctx->sched.pid);
  rd->pool = ptemp;
  int res = APSML_OK;
  if (rd->ctx->initscript != NULL)
  {
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: init script: %s about to start", rd->ctx->initscript);
    rd->ctx->pid = getpid();
  char *name = (char *) malloc (120);
  snprintf (name, 119, "/tmp/SMLServer_debug_file_%d_XXXXXX", rd->ctx->pid);
  debug_file = mkstemp(name);
    if (rd->ctx->initscript[0] == '/') 
    {
      res = apsml_processSmlFile (rd, rd->ctx->initscript, 1);
    }
    else
    {
      is = malloc(strlen(rd->ctx->initscript) + strlen(rd->ctx->smlpath) + 10);
      strcpy(is, rd->ctx->smlpath);
      strcat(is, "/");
      strcat(is, rd->ctx->initscript);
      res = apsml_processSmlFile (rd, is, 1);
      free(is);
    }
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: init script executed with return code %d", res);
    struct db_t *db_tmp = rd->ctx->db;
    for (; db_tmp; db_tmp = db_tmp->next)
    {
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: shutdown 1");
      db_tmp->tmp_shutdown(db_tmp->dbspec, s);
    }
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: shutdown 2");
  }
  else
  {
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: No init script executed");
  }
  rd->ctx->initDone = 1;
  if (res != APSML_OK) return 5;
  return OK;
}       //}}}


/*
void
debug_writer1(char *s, int a)
{
  if (debug_file != -1) dprintf (debug_file, s, a);
  return;
}

void
debug_writer2(char *s, int a, int b)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b);
  return;
}

void
debug_writer3(char *s, int a, int b, int c)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c);
  return;
}

void
debug_writer4(char *s, int a, int b, int c, int d)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c, d);
  return;
}

void
debug_writer5(char *s, int a, int b, int c, int d, int e)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c, d, e);
  return;
}

void
debug_writer6(char *s, int a, int b, int c, int d, int e, int f)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c, d, e, f);
  return;
}

void
debug_writer7(char *s, int a, int b, int c, int d, int e, int f, int g)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c, d, e, f, g);
  return;
}

void
debug_writer8(char *s, int a, int b, int c, int d, int e, int f, int g, int h)
{
  if (debug_file != -1) dprintf (debug_file, s, a,b, c, d, e, f, g, h);
  return;
}
*/

static void
apsml_child_init(apr_pool_t *p, server_rec *s)/*{{{*/
{
  struct db_t *tmp;
  InterpContext *ctx = ap_get_module_config (s->module_config, &sml_module);
  ctx->pid = getpid();
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: childInit; pid : %d", ctx->pid);
  apr_proc_mutex_child_init(&(ctx->cachelock.plock), ctx->cachelock.plockname, p);
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: childInit 1");
  for(tmp = ctx->db; tmp; tmp = tmp->next)
  {
    (*(tmp->child_init))(tmp->dbspec, tmp->num, p, s);
  }
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: childInit 2");
  apr_global_mutex_child_init(&(ctx->sched.lock), ctx->sched.glockname, p);
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
      "apsml: childInit 3");

  char *name = (char *) malloc (120);
  snprintf (name, 119, "/tmp/SMLServer_debug_file_%d_XXXXXX", ctx->pid);
  debug_file = mkstemp(name);
}/*}}}*/

#if 0
void
ppTable (apr_table_entry_t * table, request_data * rd)  //{{{
{
  int i;
  apr_array_header_t *ah = (apr_array_header_t *) apr_table_elts (table);
  for (i = 0; i < ah->nelts; i++)
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "table entry %i: (key,value) = (%s,%s)", i,
        (((apr_table_entry_t *) ah->elts) + i)->key,
        (((apr_table_entry_t *) ah->elts) + i)->val);
    }
}       //}}}

void
printrequest (request_rec * r)  //{{{
{
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->the_request is %s", r->the_request);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->protocol is %s", r->protocol);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->hostname is %s", r->hostname);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->status_line is %s", r->status_line);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->method is %s", r->method);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->range is %s", r->range);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->content_type is %s", r->content_type);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->handler is %s", r->handler);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->content_encoding is %s",
     r->content_encoding);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->content_languages is %s",
     r->content_languages);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->vlist_validator is %s",
     r->vlist_validator);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->user is %s", r->user);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->ap_auth_type is %s", r->ap_auth_type);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->unparsed_uri is %s", r->unparsed_uri);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->uri is %s", r->uri);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->filename is %s", r->filename);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->path_info is %s", r->path_info);
  ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r,
     "apsml: request_rec->args is %s", r->args);
  return;
}       //}}}
#endif

time_t
apsml_fileModTime (char *file)  //{{{
{
  struct stat buf;
  if (stat (file, &buf) != 0)
    return (time_t) - 1;
  return buf.st_mtime;
}       //}}}

char *
apsml_smlFileToUoFile(request_data *rd, char *smlfile, char *prjid,
                      char **old, unsigned long *oldLength)/*{{{*/
{
  char *pageRoot;
  char c, *p, *lp, *lu, *uo;
  unsigned long length;
  InterpContext *ctx = rd->ctx;
  pageRoot = ctx->smlpath;
  p = smlfile;
  lp = NULL;
  lu = NULL;
  length = strlen(smlfile) + strlen(mlb) + 10;
  if (length > *oldLength)
  {
    uo = *old;
    *old = (char *) malloc(length);
    if (!*old)
    {
      *old = uo;
      return NULL;
    }
    free(uo);
    *oldLength = length;
  }
  uo = *old;
  while ((c = *p))
  {
    if (c == '/')
    {
      lp = p;
      lu = uo;
    }
    *uo = c;
    uo++;
    p++;
  }
  strcpy(lu,mlb);
  while (*lu) lu++;
  strcpy(lu,lp);
  strcat(lu,".uo");
  return *old;
}/*}}}*/

static int
apsml_processSmlFile (request_data * rd, char *uri, int kind) //{{{
{
  struct char_charHashEntry he;
  char *file;
  struct parseCtx pCtx;
  int res;
  time_t t;
  Serverstate ss;
  char *errorStr = NULL;

  ss.report = logMsg;
  ss.aux = (void *) rd;

  InterpContext *ctx = rd->ctx;

  /*
   * Test to see if the ul-file exists
   */

  t = apsml_fileModTime (ctx->ulFileName);

  if (rd->request)
  {
    ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->request,
       "mod_sml: pid: %ld, Notice ul-file has time %ld", (long) rd->ctx->pid, (long) t);
  }
  else
  {
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "mod_sml: pid: %ld, Notice ul-file has time %ld", rd->ctx->pid, t);
  }

  if (t == (time_t) - 1)
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, rd->server,
      "mod_sml:Err ul-file %s does not exist - web service not working",
      ctx->ulFileName);
    return APSML_ULFILENOTFOUND;
  }

  /*
   * (Re)load interpreter if timeStamps do not match
   */

  if (ctx->timeStamp != t)
  {
    // Reload the interpreter


    // MEMO: somehow wait for all executions to finish!
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: (re)loading interpreter oldtime: %ld newtime %ld",
       ctx->timeStamp, t);

    // free all code elements present in the
    // interpreter, including code cache entries...
    interpClear (ctx->interp);

    // clear the heap cache
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: clearing heap cache");
    clearHeapCache ();

    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: opening ul-file %s", ctx->ulFileName);

    clearSmlMap(rd->ctx->smlTable);
    rd->ctx->smlTable = NULL;

    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: setting up pCtx");


    pCtx.interp = rd->ctx->interp;
    pCtx.ctx = rd;
    pCtx.ulTable = NULL;
    pCtx.smlTable = NULL;
    pCtx.uoTable = NULL;
    pCtx.fileprefix = rd->ctx->smlpath;
    pCtx.fpl = strlen(pCtx.fileprefix);
    pCtx.mapprefix = "/";
    pCtx.mpl = strlen(pCtx.mapprefix);
    pCtx.root = "/";
    pCtx.rl = strlen(pCtx.root);
    res = recurseParse(&pCtx, rd->ctx->ulFileName);
    if (res != Parse_OK) 
    {
      clearPCtx(&pCtx);
      return APSML_ERROR;
    }
    rd->ctx->smlTable = pCtx.smlTable;
    pCtx.smlTable = NULL;
    clearPCtx(&pCtx);
    ctx->timeStamp = t;
  }

  switch (kind)
  {
    case 0:
      he.key = uri;
      he.hashval = charhashfunction(he.key);
      if (hashfind(rd->ctx->smlTable, &he, (void **) &file) == hash_DNE)
      {
        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
           "apsml: Request not script: %s %ld %d", uri, he.hashval, strlen(uri));
        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
           "apsml: Size of hash table: %ld", ctx->smlTable->hashTableUsed);
        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
           "apsml: Scripts in table:");
        printSmlTable(rd->ctx->smlTable, rd);
        return APSML_FILENOTFOUND;
      }
      break;
    case 1:
      file = apsml_smlFileToUoFile (rd, uri, ctx->prjid, &(ctx->filebuf), &(ctx->filebufLength));
      if (!file)
      {
        return APSML_FILENOTFOUND;
      }
      break;
    default:
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
         "Internal errpr in mod_sml.c, apsml_processSmlFile called with kind == %d", kind);
      return APSML_FILENOTFOUND;
      break;
  }

  debug_writer1("Starting new file %d\n", 0);
  ap_log_error (__FILE__, __LINE__, LOG_INFO, 0, rd->server,
     "Starting interpreter on file %s, pid: %d", file, rd->ctx->pid);

//  globalrd = rd;
  if (interpLoadRun (ctx->interp, file, &errorStr, &ss, &res) != 0)
  {
    ap_log_error (__FILE__, __LINE__, LOG_INFO, 0, rd->server,
       "Interpretion on file %s went bad, pid: %d", file, rd->ctx->pid);
  debug_writer1("Stopping new file %d BAD\n", 0);
    return APSML_ERROR;
  }
  ap_log_error (__FILE__, __LINE__, LOG_INFO, 0, rd->server,
     "Interpretion on file %s was successful, pid: %d", file, rd->ctx->pid);
  debug_writer1("Stopping new file %d GOOD\n", 0);

//  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//     "Interpretation ended on file %s with result %d, errorStr: %d", uo_file, res, errorStr);
//  if (errorStr)
//    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//       "Interpretation ended on file %s with result %d, errorStr: %s", uo_file, res, errorStr);


  if (res < 0)
  {                   // uncaught exception; errorStr allocated
    if (res == -1)    // exception other than Interrupt raised
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
        "%s raised %s", file, errorStr);
    }
    free (errorStr);    // free the malloced string 
    errorStr = NULL;    // - and nullify field    
  }

  // clean up database connections if user forgot
  struct request_db *tmp;
  struct db_t *db_tmp;
  for (tmp = rd->dbdata; tmp; tmp=tmp->next)
  {
    for(db_tmp = rd->ctx->db; db_tmp; db_tmp = db_tmp->next)
    {
      if (tmp->num == db_tmp->num)
      {
        db_tmp->req_cleanup(rd,tmp->dbdata);
        break;
      }
    }
  }
  if (res == -1)
  {
    return APSML_INTERRUPTRAISED;
  }
  return APSML_OK;
}       //}}}

static int
mod_sml_method_handler (request_rec * r)  //{{{
{
  if (strcmp (r->handler, "sml-module")) return DECLINED;
//  printrequest (r);
//  ppTable(r->headers_in, r);
  request_data rd1;
  request_data *rd = &rd1;
  rd->pool = r->pool;
  rd->server = r->server;
  rd->request = r;
  rd->ctx = ap_get_module_config (r->server->module_config, &sml_module);
  rd->cachetable = rd->ctx->cachetable;
  rd->dbdata = NULL;
//  ppGlobalCache(rd);

//        ap_log_rerror(__FILE__, __LINE__, LOG_NOTICE, 0, r, "apsml cachetable: %d", rd->cachetable);
  if (r->args)
    {
//        ap_log_rerror(__FILE__, __LINE__, LOG_NOTICE, 0, r, "apsml args: %s", r->args);
    }
  int res;
  res = apsml_processSmlFile (rd, r->uri, 0);
  switch (res)
  {
  case APSML_FILENOTFOUND:
    {
//    printrequest (r);
      return HTTP_NOT_FOUND;
      break;
    }
  case APSML_ULFILENOTFOUND:
    {
//    printrequest (r);
      return HTTP_NOT_FOUND;
      break;
    }
  case APSML_INTERRUPTRAISED:
  case APSML_ERROR:
    {
      return HTTP_INTERNAL_SERVER_ERROR;
      break;
    }
  default:
    {
//ppTable(r->headers_out, r);
//    printrequest (r);
      return OK;
      break;
    }
  }
  return OK;
}       //}}}

static void
mod_sml_register_hooks (apr_pool_t * p) //{{{
{
  ap_hook_handler (mod_sml_method_handler, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_post_config (apsml_post_config, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_child_init (apsml_child_init, NULL, NULL, APR_HOOK_MIDDLE);
}       //}}}

module AP_MODULE_DECLARE_DATA sml_module =  //{{{
{
  STANDARD20_MODULE_STUFF,  /* stuff that needs to be declared in every 2.0 mod */
  NULL,       /* create per-directory config structure            */
  NULL,       /* merge per-directory config structures            */
  perserver_init,   /* create per-server config structure               */
  NULL,       /* merge per-server config structures               */
  mod_sml_cmds,     /* command apr_table_t                              */
  mod_sml_register_hooks  /* register hooks                                   */
};        //}}}

#if 0
void
apsml_ppheaders (request_data * rd) /*{{{ */
{
  ppTable (rd->request->headers_in, rd);
  return;
} /*}}} */
#endif
