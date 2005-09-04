// The Apache module to serve SMLserver


#include <sys/stat.h>
#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "../../Runtime/String.h"
#include "mod_sml.h"
#include "../../CUtils/binaryheap.h"
#include "sys/types.h"
#include "sys/socket.h"
#include "netdb.h"

#define APSML_SCRIPT_HASHTABLE_SZ 1023

#define BUFSIZE 1000

#define DEFAULT_PRJID "sources"
#define DEFAULT_XT 0

#define SHMSIZE 0x1000

enum APSML_RETURNVALUES   /*{{{ */
{
  APSML_FILENOTFOUND = -1,
  APSML_ULFILENOTFOUND = -2,
  APSML_INTERRUPTRAISED = -3,
  APSML_OK = 0,
};        /*}}} */

enum 
{
  SCHED_ADD = 0,
  SCHED_REMOVE = 1
};

#define MIN(a,b) (a < b ? a : b)

module AP_MODULE_DECLARE_DATA sml_module;


static int apsml_processSmlFile (request_data * rd, char *urlfile);

request_data *globalrd;

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
  {NULL}
};        /*}}} */

static void *
perserver_init (apr_pool_t * p, server_rec * s) //{{{
{
  InterpContext *ctx =
    (InterpContext *) apr_pcalloc (p, sizeof (InterpContext));

  ctx->prjid = DEFAULT_PRJID;
  ctx->extendedtyping = DEFAULT_XT;
  ctx->initscript = NULL;
  ctx->trapscript = NULL;
  ctx->smlpath = NULL;
  ctx->conftable = NULL;
  ctx->cachetable = NULL;
//  ctx->exn_OVERFLOW = NULL;
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

typedef struct 
{
  request_data *rd;
  apr_file_t *out;
  apr_pool_t *pconf;
} scheddata_t;

void myabort(int infile)
{
  close(infile);
  exit(0);
}

struct buflist
{
  struct buflist *next;
  char *data;
  int length;
  int maxsize;
};

static char *tail (char *p)
{
  while (*p) p++;
  return p;
}

int globallogfile;

static void 
getpage(char *server, char *address, unsigned short port)/*{{{*/
{
  struct addrinfo addr_hint, *addr, *myaddr;
  char *head, *ptr;
  fd_set writeset, readset;
  struct timeval tv;
  char sport[20];
  int sock, c;
  time_t now, curtime;
  int tmp, size, left, right;
  size = strlen(server) + strlen(address) + 200;
  snprintf(sport, 19, "%d", port);
  sport[19] = 0;
  head = (char *) malloc (size + 1000);
  if (head == NULL) return;
  memset(&addr_hint, 0, sizeof(struct addrinfo));
  ptr = head;
  strcpy (ptr, "GET ");
  ptr = tail (ptr);
  strcpy (ptr, address);
  ptr = tail (ptr);
  strcpy (ptr, " HTTP/1.1\r\nHost: ");
  ptr = tail (ptr);
  strcpy (ptr, server);
  ptr = tail (ptr);
  strcpy (ptr, "\r\nConnection: close\r\n\r\n");
  addr_hint.ai_family = PF_UNSPEC;
  addr_hint.ai_socktype = SOCK_STREAM;
  if (getaddrinfo (NULL, sport, &addr_hint, &addr) != 0) 
  {
    dprintf(globallogfile, "getaddrinfo failed\n");
    free(head);
    return;
  }
  myaddr = addr;
  while (myaddr)
  {
    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock == -1)
    {
      myaddr = myaddr->ai_next;
    }
    else 
    {
      c = connect(sock, myaddr->ai_addr, myaddr->ai_addrlen);
      if (c != 0)
      {
        close(sock);
        myaddr = myaddr->ai_next;
      }
      else break;
    }
  }
  freeaddrinfo (addr);
  if (sock == -1 || c == -1)
  {
    dprintf(globallogfile, "connect failed\n");
    free(head);
    return;
  }
  ptr = head + size;
  left = strlen(head);
  right = 0;
  curtime = time(NULL) + 60;
//  dprintf(globallogfile, "getpage ready: command: \n%s\n", head);
  while (1)
  {
    now = time(NULL);
    if (now > curtime)
    {
      free(head);
      close(sock);
      return;
    }
    FD_ZERO(&writeset);
    FD_ZERO(&readset);
    if (left > 0)
    {
      FD_SET(sock, &writeset);
    }
    FD_SET(sock, &readset);
    tv.tv_sec = 60;
    tv.tv_usec = 0;
    tmp = select(sock + 1, &readset, &writeset, NULL, &tv);
    if (tmp == -1)
    {
      free(head);
      close(sock);
      return;
    }
    if (FD_ISSET(sock, &readset))
    {
      tmp = recv(sock, ptr, 1000, MSG_DONTWAIT);
      if (tmp == -1)
      {
        tmp = errno;
        if (tmp == EAGAIN) continue;
        free(head);
        close(sock);
        return;
      }
      if (tmp == 0)
      {
        free (head);
        close(sock);
        return;
      }
    }
    if (FD_ISSET(sock, &writeset))
    {
      tmp = send(sock, head+right, left, MSG_DONTWAIT);
      if (tmp == -1)
      {
        tmp = errno;
        if (tmp == EAGAIN || tmp == EWOULDBLOCK) continue;
        free(head);
        close(sock);
        return;
      }
      left -= tmp;
      right += tmp;
    }
  }
  return;
}/*}}}*/

struct scriptsched_t/*{{{*/
{
  struct scriptsched_t *next;
  time_t nexttime;
  unsigned int interval;
  char *script;
  unsigned long scripthash;
  char *server;
  unsigned short port;
  unsigned long pos;
};/*}}}*/

int getpackage(int infile, struct buflist **blist, struct scriptsched_t **rv, int logfile)/*{{{*/
{
  int tmp, i,j;
  struct buflist *myblist, *myblist1;
  schedHeader *sched;
  char *tmpdata;
  time_t timetmp;
  myblist1 = *blist;
  while (myblist1 && myblist1->next)
  {
    myblist1 = myblist1->next;
  }
  myblist = myblist1;
  if (myblist == NULL)
  {
    myblist = (struct buflist *) malloc(BUFSIZE + sizeof(struct buflist));
    if (myblist == NULL) return -1;
    myblist->next = NULL;
    myblist->data = (char *)(myblist + 1);
    myblist->length = 0;
    myblist->maxsize = BUFSIZE;
    *blist = myblist;
  }
  if (myblist->length == myblist->maxsize) 
  {
    myblist->next = (struct buflist *) malloc(BUFSIZE + sizeof(struct buflist));
    if (myblist->next == NULL) return -1;
    myblist = myblist->next;
    myblist->next = NULL;
    myblist->data = (char *)(myblist + 1);
    myblist->length = 0;
    myblist->maxsize = BUFSIZE;
  }
  tmp = read(infile, myblist->data + myblist->length, myblist->maxsize - myblist->length);
//  dprintf(logfile, "GetPackage read result %d, filepos: %x, length %d\n", 
//                    tmp,myblist->data + myblist->length,myblist->length
//                    );
  if (tmp == -1)
  {
  dprintf(logfile, "GetPackage read err: %s\n", strerror(errno));
    return -1;
  }
  if (tmp == 0) return 0;
  myblist->length += tmp;
  if ((*blist)->length >= sizeof(schedHeader))
  {
    tmp = 0;
    for (myblist1 = *blist; myblist1; myblist1 = myblist1->next) tmp += myblist1->length;
    sched = (schedHeader *) (*blist)->data;
    if (tmp >= sched->length)
    {
      *rv = (struct scriptsched_t *) malloc (sched->length+1 + sizeof(struct scriptsched_t));
      if (*rv == NULL) return -1;
      (*rv)->next = NULL;
      timetmp = time(NULL);
      switch (sched->type)
      {
        case SCHED_ADD:
          (*rv)->nexttime = timetmp + sched->first > 0 ? timetmp + sched->first : timetmp;
          (*rv)->interval = sched->interval;
          break;
        case SCHED_REMOVE:
          (*rv)->nexttime = 0;
          (*rv)->interval = 0;
          break;
        default:
          return -1;
          break;
      }
      (*rv)->server = (char *) ((*rv)+1);
      (*rv)->port = sched->port;
      (*rv)->script = (*rv)->server + sched->serverlength + 1;
      tmp = 0;
      i = 0;
      j = sched->length;
      (*blist)->data += sizeof(schedHeader);
      (*blist)->length -= sizeof(schedHeader);
      for (myblist1 = *blist; myblist1; myblist1 = myblist1->next)
      {
        j -= i;
        i = MIN(j, myblist1->length);
        memcpy((*rv)->server + tmp, myblist1->data, i);
        tmp += i;
      }
      ((*rv)->server)[tmp] = 0;
      (*rv)->scripthash = charhashfunction((*rv)->script);
      for(myblist1 = *blist; myblist1->next; myblist1 = myblist1->next)
      {
        free(myblist1);
      }
      *blist = myblist1;
      if (myblist1->length > j)
      {
        memmove(myblist1->data, myblist1->data + (myblist1->length - j), myblist1->length - j);
        myblist1->length = myblist1->length-j;
      }
      else 
      {
        myblist1->length = 0;
      }
//        dprintf(logfile, "(*rv): next: %x, nexttime: %d, interval: %d, script: %s, scripthash: %d, pos: %d\n", (*rv)->next, (*rv)->nexttime, (*rv)->interval, (*rv)->script, (*rv)->scripthash, (*rv)->pos);
      return 1;
    }
  }
  *rv = NULL;
  return 1;
}/*}}}*/

static int 
order (struct scriptsched_t **a, struct scriptsched_t **b)/*{{{*/
{
  if ((*a)->nexttime == (*b)->nexttime) return 0;
  if ((*a)->nexttime < (*b)->nexttime) return -1;
  return 1;
}/*}}}*/

static void 
newpos (struct scriptsched_t **elem, unsigned long pos)/*{{{*/
{
  (*elem)->pos = pos;
  return;
}/*}}}*/

static void 
mysetkey (struct scriptsched_t **elem, time_t key)/*{{{*/
{
  (*elem)->nexttime = key;
  return;
}/*}}}*/

static unsigned long 
hashfunc(void *key)/*{{{*/
{
  return ((struct scriptsched_t *) key)->scripthash;
}/*}}}*/

static int 
hashequal(void *k1, void *k2)/*{{{*/
{
  struct scriptsched_t *key1 = (struct scriptsched_t *) k1;
  struct scriptsched_t *key2 = (struct scriptsched_t *) k2;
  if (key1->scripthash != key2->scripthash) return 0;
  return !(strcmp(key1->script, key2->script));
}/*}}}*/

DECLARE_BINARYHEAP(timeheap,struct scriptsched_t *, time_t)

DEFINE_BINARYHEAP(timeheap,order,newpos,mysetkey)

void 
removeAndFree(timeheap_binaryheap_t *heap, hashtable *hashmap, struct scriptsched_t *item)/*{{{*/
{
  struct scriptsched_t *rv;
  timeheap_heapdelete(heap, item->pos);
  if (hashfind(hashmap, item, (void **) &rv) == hash_OK)
  {
    if (rv == item)
    {
      hasherase(hashmap, item);
      if (rv->next != 0)
      {
        hashupdate(hashmap, rv->next, rv->next);
      }
    }
    else 
    {
      while (rv)
      {
        if (item == rv->next)
        {
          rv->next = rv->next->next;
          break;
        }
        else
        {
          rv = rv->next;
        }
      }
    }
  }
  free(item);
  return;
}/*}}}*/


static void
printheap(struct scriptsched_t **t)/*{{{*/
{
  dprintf(globallogfile, "heap: next: %x, nexttime: %d, interval: %d, script: %s, scripthash: %d, server: %s, pos: %d\n", (*t)->next, (*t)->nexttime, (*t)->interval, (*t)->script, (*t)->scripthash, (*t)->server, (*t)->pos);
  return;
}/*}}}*/

struct threaddata/*{{{*/
{
  char *newserver;
  char *newscript;
  unsigned short port;
};/*}}}*/

void * APR_THREAD_FUNC
childgetpage(apr_thread_t *thread, void *d1)/*{{{*/
{
  struct threaddata *td = (struct threaddata *) d1;
  getpage(td->newserver, td->newscript, td->port);
//  dprintf(globallogfile, "page fetched: script: %s, server: %s, port: %d\n", td->newscript, td->newserver, td->port);
  free(td);
  apr_thread_exit(thread, APR_SUCCESS);
  return NULL;
}/*}}}*/

void
forkandgetpage(char *server, char *script, unsigned short port, 
               apr_pool_t *pool, apr_threadattr_t *schedthread_attr)/*{{{*/
{
  int tmp;
  char *newserver, *newscript;
  struct threaddata *td;
  apr_status_t stat;
  apr_thread_t *schedthread;

  tmp = strlen (server) + strlen(script);
  td = (struct threaddata *) malloc(tmp + 2 + sizeof(struct threaddata));
  if (td == NULL) return;
  newserver = (char *) (td+1);
  newscript = newserver + strlen(server) + 1;
  strcpy(newserver, server);
  strcpy(newscript, script);
  td->port = port;
  td->newscript = newscript;
  td->newserver = newserver;
  stat = apr_thread_create(&schedthread, schedthread_attr, childgetpage, td, pool);
  return;
}/*}}}*/

static void 
scheduleproc (const char *server, int port, int infile)/*{{{*/
{
  int bufsize;
  int logfile;
  int tmp;
  apr_status_t stat;
  apr_pool_t *pool;
  int curpackage = 0;
  apr_threadattr_t *schedthread_attr;
  timeheap_binaryheap_t heap;
  hashtable map;
  struct scriptsched_t *header = NULL, *tmpheader, *tmpheader2;
  time_t curtime;
  fd_set readfd;
  struct timeval tv;
  unsigned int nexttimeout = 0;
  struct buflist *blist = NULL;
  timeheap_heapinit(&heap);
  hashinit(&map, hashfunc, hashequal);
  char *buf = (char *) malloc(BUFSIZE);
  if (buf == NULL) exit(1);
  stat = apr_pool_create(&pool, NULL);
  if (stat != APR_SUCCESS) exit(1);
  stat = apr_threadattr_create(&schedthread_attr, pool);
  if (stat != APR_SUCCESS) exit(1);
  stat = apr_threadattr_detach_set(schedthread_attr, 1);
  if (stat != APR_SUCCESS) exit(1);
  strcpy(buf, "/tmp/smlserver.XXXXXX");
  logfile = mkstemp(buf);
  if (logfile == -1) myabort(infile);
  dprintf(logfile, "server initiated with pid: %d\n", getpid());
  globallogfile = logfile;
  while (1)
  {
    curtime = time(NULL);
    FD_ZERO(&readfd);
    FD_SET(infile, &readfd);
//    timeheap_heapapply (&heap, printheap);
    if (timeheap_heapminimal(&heap, &tmpheader) != heap_OK)
    {
      dprintf(logfile, "No scheduled elements\n");
      tmp = select(infile+1, &readfd, NULL, NULL, NULL);
    }
    else 
    {
      tv.tv_sec = tmpheader->nexttime - curtime;
      tv.tv_sec = tv.tv_sec < 0 ? 0 : tv.tv_sec;
      tv.tv_usec = 0;
//      dprintf(logfile, "Next scheduled run: %d\n", tv.tv_sec);
      if (tv.tv_sec != 0)
      {
        tmp = select(infile+1, &readfd, NULL, NULL, &tv);
      }
      else 
      {
        tmp = 0;
        FD_ZERO(&readfd);
      }
    }
    if (tmp == -1) 
    {
      tmp = errno;
      dprintf(logfile, "select error: %s\n", strerror(tmp));
      continue;
    }
    if (FD_ISSET(infile, &readfd))
    {
      tmp = getpackage(infile, &blist, &header, logfile);
      if (tmp == -1) continue;
      if (tmp == 0) 
      {
        dprintf(logfile, "end of file received\n");
        continue;
      }
      if (header)
      {
        if (header->nexttime == 0 && header->interval == 0) 
        {
          if (hashfind(&map, header, (void **) &tmpheader2) == hash_OK)
          {
            while (tmpheader2)
            {
              tmpheader = tmpheader2->next;
              hasherase(&map, tmpheader2);
              timeheap_heapdelete (&heap, tmpheader2->pos);
              free (tmpheader2);
              tmpheader2 = tmpheader;
            }
          }
        }
        else 
        {
          if (hashfind(&map, header, (void **) &tmpheader2) == hash_OK)
          {
            while (tmpheader2->next) tmpheader2 = tmpheader2->next;
            tmpheader2->next = header;
          }
          else 
          {
            hashupdate(&map, header, header);
          }
          timeheap_heapinsert(&heap, header, header->nexttime);
        }
      }
    }
    else 
    { // timeout occured
      if (timeheap_heapminimal(&heap, &tmpheader) != heap_OK)
      {
        continue;
      }
      else 
      {
//        dprintf(logfile, "tmpheader: next: %x, nexttime: %d, interval: %d, script: %s, scripthash: %d, pos: %d\n", tmpheader->next, tmpheader->nexttime, tmpheader->interval, tmpheader->script, tmpheader->scripthash, tmpheader->pos);
        forkandgetpage(tmpheader->server, tmpheader->script, tmpheader->port, pool, schedthread_attr);
        if (tmpheader->interval == 0)
        {
          removeAndFree(&heap, &map, tmpheader);
        }
        else 
        {
          timeheap_heapchangekey(&heap, tmpheader->pos, tmpheader->nexttime + tmpheader->interval);
        }
      }
    }
  }
  myabort(infile);
  exit(0);
}/*}}}*/

static int 
startsched (const char *server, apr_port_t port, int infile)/*{{{*/
{
  int s;
  s = fork();
  if (s == 0) 
  { // child
    scheduleproc(server, port, infile);
    exit(0);
  }
  else 
  { // parent
    close(infile);
    return s;
  }
  return -1;
}/*}}}*/

apr_status_t 
killschedproc (void *p1) /*{{{*/
{
  pid_t *p = (pid_t *) p1;
  kill(*p, SIGTERM);
  return APR_SUCCESS;
}/*}}}*/

apr_status_t 
killschedprocchild (void *rd)/*{{{*/
{
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

apr_thread_mutex_t *apache_locks[] = {NULL,NULL,NULL,NULL};

static int
apsml_post_config (apr_pool_t * pconf, apr_pool_t * plog, apr_pool_t * ptemp, server_rec * s) //{{{
{
  int i;
  server_rec *ss;
  void *first_init_check = NULL;
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
  scheddata_t *scheddata = (scheddata_t *) apr_pcalloc(pconf, sizeof(scheddata_t));
  if (rd == NULL || scheddata == NULL) return 5;
  rd->request = NULL;
  rd->server = s;
  rd->ctx = ctx;
  rd->ctx->initDone = 0;
  rd->dbdata = NULL;

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

  sprintf (ctx->ulFileName, "%s/PM/%s.ul", ctx->smlpath, ctx->prjid);

  ctx->scripts = emptyHashTable (APSML_SCRIPT_HASHTABLE_SZ);

  ss = s;
  while (ss)
  {
    printserver (ss);
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
  if (rd->ctx->cachelock.shmname == NULL) return 5;
  stat = apr_shm_create(&(rd->ctx->cachelock.shm), SHMSIZE, rd->ctx->cachelock.shmname, pconf);
  if (stat != APR_SUCCESS)
  {
    return 5;
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

  int tmp;
  int mypipes[2];
  tmp = pipe(mypipes);
  if (tmp != 0) return 5;
  rd->ctx->sched.input = mypipes[1];
  tmp = startsched(s->defn_name, s->port, mypipes[0]);
  if (tmp == -1) return 5;
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
      "apsml: test 5");
  rd->ctx->sched.pid = tmp;
  apr_pool_cleanup_register(pconf, &(rd->ctx->sched.pid), killschedproc, killschedprocchild);
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: created process %d", rd->ctx->sched.pid);
  rd->pool = ptemp;
  int res = APSML_OK;
  if (rd->ctx->initscript != NULL)
  {
    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
      "apsml: init script: %s about to start", rd->ctx->initscript);
    rd->ctx->pid = getpid();
    res = apsml_processSmlFile (rd, rd->ctx->initscript);
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
}/*}}}*/

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

time_t
apsml_fileModTime (char *file)  //{{{
{
  struct stat buf;
  if (stat (file, &buf) != 0)
    return (time_t) - 1;
  return buf.st_mtime;
}       //}}}

//apr_time_t
//apsml_fileModTime (char *file,apr_pool_t *p)/*{{{*/
//{
//  apr_finfo_t f;
//  if (apr_stat(&f, file, APR_FINFO_MTIME, p) == APR_SUCCESS) return f.mtime;
//  return apr_time_t
//}/*}}}*/

int
apsml_next_sml0 (char *p) //{{{
{
  if (*(p + 1) == 's' && *(p + 2) == 'm'
      && *(p + 3) == 'l' && *(p + 4) == '\0')
    return 1;
  else
    return 0;
}       //}}}

int
apsml_smlFileToUoFile (request_data * rd, char *url, char *uo, char *prjid, int path_p) //{{{
{
//  request_rec *r = rd->request;
  const char *pageRoot;
  char *p;      /*  = strrchr(url, '/'); */
  int i;
  InterpContext *ctx = rd->ctx;
//    ap_get_module_config (rd->server->module_config, &sml_module);

//  pageRoot = r->server->path;
  pageRoot = ctx->smlpath;
  if (strstr (url, pageRoot) != url)
    {
      if (rd->request)
  {
    ap_log_rerror (__FILE__, __LINE__, LOG_ERR, 0, rd->request,
       "pageRoot %s is not a substring of the requested url %s",
       pageRoot, url);
  }
      else
  {
    ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, rd->server,
      "pageRoot %s is not a substring of the requested url %s",
      pageRoot, url);
  }
      return -1;
    }
  if (path_p)
    {
      strcpy (uo, pageRoot);
      strcat (uo, "/PM/");
      strcat (uo, prjid);
    }
  else
    {
      strcpy (uo, prjid);
    }
  strcat (uo, "-");
  i = strlen (uo);
  p = url + strlen (pageRoot);
  if (*p == '/')
    p++;
  while (*p != '\0')
    {
      char c = *p;
      if (c == '.')
  {
    if (ctx->extendedtyping && apsml_next_sml0 (p))
      {
        uo[i++] = '%';
        uo[i++] = 'g';
        uo[i++] = 'e';
        uo[i++] = 'n';
      }
    if (*(p + 1) == '.')
      {
        c = '%';
        p++;
      }
  }
      if (c == '/')
  c = '+';
      uo[i++] = c;
      p++;
    }
  uo[i] = '\0';
  strcat (uo, ".uo");
  return 0;
}       //}}}

static int
apsml_processSmlFile (request_data * rd, char *urlfile) //{{{
{
//  request_rec *r = rd->request;
//  char *urlfile = r->filename;        /* the requested url as file */
  char uo[APSML_PATH_MAX];
  char uo_file[APSML_PATH_MAX];
  int res;
  time_t t;
  char *errorStr = NULL;

  InterpContext *ctx = rd->ctx;

  /*
   * Test to see if the ul-file exists
   */

  t = apsml_fileModTime (ctx->ulFileName);

  if (rd->request)
    {
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->request,
         "mod_sml: pid: %d, Notice ul-file has time %i", rd->ctx->pid, t);
    }
  else
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
        "mod_sml: pid: %d, Notice ul-file has time %i", rd->ctx->pid, t);
    }

  if (t == (time_t) - 1)
    {
      ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, rd->server,
        "mod_sml:Err ul-file %s does not exist - web service not working",
        &ctx->ulFileName);
      return APSML_ULFILENOTFOUND;
    }

  /*
   * (Re)load interpreter if timeStamps do not match
   */

  if (ctx->timeStamp != t)
  {
    // Reload the interpreter

    FILE *is;
    char buff[APSML_PATH_MAX];
    int count = 0;

    // MEMO: somehow wait for all executions to finish!
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: (re)loading interpreter oldtime: %i newtime %i",
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
    is = fopen (ctx->ulFileName, "r");
    if (is == NULL)
    {
      ap_log_perror (__FILE__, __LINE__, LOG_ERR, 0, rd->pool,
         "apsml: Failed to open file %s for reading",
         &ctx->ulFileName);
      return APSML_ULFILENOTFOUND;
    }

    while (fgets (buff, APSML_PATH_MAX, is) != NULL)
    {
      if (buff[strlen (buff) - 1] == '\n')
        buff[strlen (buff) - 1] = '\0';

      if (!strcmp (buff, "scripts:")) break;

      interpLoadExtend (ctx->interp, buff);
//        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
//                       "apsml: Loading %s", buff);
      count++;
    }

    // clear the script-name hash table
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: clearing script-name hash table");
    freeHashTable (ctx->scripts);
    ctx->scripts = emptyHashTable (APSML_SCRIPT_HASHTABLE_SZ);

    if (!strcmp (buff, "scripts:"))
    {
      while (fgets (buff, APSML_PATH_MAX, is) != NULL)
      {
        if (buff[strlen (buff) - 1] == '\n')
          buff[strlen (buff) - 1] = '\0';
        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
           "apsml: Accepting script: %s", buff);
        insertHashTable (ctx->scripts, buff, "ok");
      }
    }

    // close the ul-file
    fclose (is);
    ctx->timeStamp = t;
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: (Re)loaded %d uo-files with timestamp %i", count,t);
  }

  if (apsml_smlFileToUoFile (rd, urlfile, uo, ctx->prjid, 1) == -1)
  {
    return APSML_FILENOTFOUND;
  }

  // See if uo-file is a script that can be served
  if (apsml_smlFileToUoFile (rd, urlfile, uo_file, ctx->prjid, 0) == -1)
  {
    return APSML_FILENOTFOUND;
  }

  if (!lookupHashTable (ctx->scripts, uo_file))
  {
    int i;
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: Request not script: %s", uo_file);
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: Size of hash table: %d", ctx->scripts->size);
    ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
       "apsml: Size of hash table array: %d",
       ctx->scripts->arraySize);
    for (i = 0; i < ctx->scripts->arraySize; i++)
    {
      ObjectListHashTable *ol;
      if ((ol = ctx->scripts->array[i]) == 0)
        continue;
      ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
         "apsml: array[%d]:", i);
      for (; ol; ol = ol->next)
        //  {
        ap_log_perror (__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool,
           "apsml:   %s: %s", ol->key, (char *) (ol->value));
      //  }
    }
    return APSML_FILENOTFOUND;
  }

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
     "Starting interpreter on file %s", uo_file);

  globalrd = rd;
  res = interpLoadRun (ctx->interp, uo, &errorStr, (void *) rd);

//  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//     "Interpretation ended on file %s with result %d, errorStr: %d", uo_file, res, errorStr);
//  if (errorStr)
//    ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//       "Interpretation ended on file %s with result %d, errorStr: %s", uo_file, res, errorStr);


  if (res < 0)
  {       // uncaught exception; errorStr allocated
    if (res == -1)    // exception other than Interrupt raised
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
        "%s raised %s", urlfile, errorStr);
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
  if (strcmp (r->handler, "sml-module"))
    return DECLINED;
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
  res = apsml_processSmlFile (rd, r->filename);
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

void
apsml_ppheaders (request_data * rd) /*{{{ */
{
  ppTable (rd->request->headers_in, rd);
}       /*}}} */
