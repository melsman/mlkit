

#include <sys/stat.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include "sys/types.h"
#include "sys/socket.h"
#include "netdb.h"
#include "sched.h"
#include "../../CUtils/binaryheap.h"
#include "../../CUtils/hashmap.h"
#include "apr_thread_proc.h"
#include "plog.h"

//typedef struct 
//{
//  request_data *rd;
//  apr_file_t *out;
//  apr_pool_t *pconf;
//} scheddata_t;

#define MIN(a,b) (a < b ? a : b)

static void 
myabort(int infile)/*{{{*/
{
  close(infile);
  exit(0);
}/*}}}*/

struct buflist
{
  struct buflist *next;
  char *data;
  int length;
  int maxsize;
};

enum 
{
  SCHED_ADD = 0,
  SCHED_REMOVE = 1
};

#define BUFSIZE 1000

static char *
tail (char *p)/*{{{*/
{
  while (*p) p++;
  return p;
}/*}}}*/

static int globallogfile;

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
  sock = -1;
  c = -1;
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
printheap(struct scriptsched_t **t)/*{{{*/
{
  dprintf(globallogfile, "heap: next: %p, nexttime: %ld, interval: %d, script: %s, scripthash: %ld, server: %s, pos: %ld\n", (*t)->next, (*t)->nexttime, (*t)->interval, (*t)->script, (*t)->scripthash, (*t)->server, (*t)->pos);
  return;
}/*}}}*/


static void 
scheduleproc (const char *server, int port, int infile)/*{{{*/
{
//  int bufsize;
  int logfile;
  int tmp;
  apr_status_t stat;
  apr_pool_t *pool;
//  int curpackage = 0;
  apr_threadattr_t *schedthread_attr;
  timeheap_binaryheap_t heap;
  hashtable map;
  struct scriptsched_t *header = NULL, *tmpheader, *tmpheader2;
  time_t curtime;
  fd_set readfd;
  struct timeval tv;
//  unsigned int nexttimeout = 0;
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
    timeheap_heapapply (&heap, printheap);
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
      dprintf(logfile, "Next scheduled run: %d\n", tv.tv_sec);
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
//        dprintf(logfile, "tmpheader: next: %x, nexttime: %d, interval: %d, script: %s, scripthash: %d, pos: %d, port %d\n", tmpheader->next, tmpheader->nexttime, tmpheader->interval, tmpheader->script, tmpheader->scripthash, tmpheader->pos, tmpheader->port);
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

struct sched_init
startsched (const char *server, apr_port_t port)/*{{{*/
{
  int tmp;
  int mypipes[2];
  int s;
  struct sched_init si;
  tmp = pipe(mypipes);
  if (tmp != 0)
  {
    si.pid = -1;
    si.input = 0; // Shut Up
    return si;
  }
  s = fork();
  if (s == 0) 
  { // child
    scheduleproc(server, port, mypipes[0]);
    exit(0);
    return si;
  }
  else 
  { // parent
    close(mypipes[0]);
    si.pid = s;
    si.input = mypipes[1];
    return si;
  }
}/*}}}*/


