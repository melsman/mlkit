#include "stdlib.h"
#include "stdio.h"
#include "stdint.h"
#include "stddef.h"
#include "sys/socket.h"
#include "netdb.h"
#include "time.h"
#include "sys/select.h"
#include "unistd.h"
#include "errno.h"
#include "limits.h"
//#include "netinet/in.h"
//#include "net/if.h"
//#include "arpa/inet.h"
#include "httpd.h"
#include "http_log.h"
#include "../../Runtime/String.h"
#include "../../Runtime/Exception.h"
#include "../../Runtime/List.h"
#include "../../CUtils/polyhashmap.h"
#include "string.h"
#include "mod_sml.h"

#define BUFFERSIZE 8000

#if defined (MAXHOSTNAMELEN) && !defined(HOST_NAME_MAX)
#define HOST_NAME_MAX MAXHOSTNAMELEN
#endif
#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 256
#endif

typedef struct
{
  int number;
  char *text;
} reply;

typedef struct
{
  int init;
  int ehlo;
  int mail;
  int rcpt;
  int data;
  int datablock;
  int dataterm;
} timeout;

typedef struct
{
  char *buf;
  int bufsize;
  int end;
  int nextline;
} buffer;

enum errtype
{
  mail_OK = 0,
  mail_ERRNO = 1,
  mail_TIMEOUT = 2,
  mail_PROTOCOL_FAILURE = 3,
  mail_INTERNALERR = 4,
  mail_CONNCLOSED = 5,
  mail_SERVICEERR = 6,
  mail_PARTIALSEND = 7
};

enum whichfail  //{{{
{
  mail_PERM,
  mail_TEMP,
  mail_OKEYED
}; //}}}

struct rcptlist //{{{
{
  struct rcptlist *next;
  int id;
  char *to;
  reply *response;
}; //}}}

struct charlist //{{{
{
  char *text;
  int len;
  struct charlist *next;
}; //}}}

typedef struct //{{{
{
  char *from;
  struct rcptlist *to;
  struct rcptlist *last;
  struct rcptlist *tosend;
  struct rcptlist *lastsend;
  struct rcptlist *toack;
  struct rcptlist *lastack;
  int size;
  char *data;
  reply *response;
} email; //}}}

DECLARE_NHASHMAP(answer_map, reply *, reply *,,)

typedef struct //{{{
{
  char *me;
  int socket;
  timeout timeout;
  reply lastreply;
  int pipelining;
  int size;
  int eightbitmime;
  enum errtype etype;
  int err;
  buffer buf;
  answer_map_hashtable_t answers;
  struct rcptlist *okeyed;
  struct rcptlist *lastokeyed;
  struct rcptlist *tempfailure;
  struct rcptlist *lasttempfailure;
  struct rcptlist *permfailure;
  struct rcptlist *lastpermfailure;
  struct charlist *partialanswer;
  email *lastemail;
  char tmp[512];
  request_data *rd;
} mailer; //}}}

void 
setdefaulttimeouts (timeout *t) //{{{
{
  if (t->init == 0) t->init = 300;
  if (t->ehlo == 0) t->ehlo = 300;
  if (t->mail == 0) t->mail = 300;
  if (t->rcpt == 0) t->rcpt = 300;
  if (t->data == 0) t->data = 120;
  if (t->datablock == 0) t->datablock = 180;
  if (t->dataterm == 0) t->dataterm = 600;
} //}}}

static unsigned long
hashfunc (reply *r) //{{{
{
  return (charhashfunction(r->text) + (unsigned long) r->number);
} //}}}

static int
htentryEqual (reply *key1, reply *key2) //{{{
{
  if (strcmp (key1->text, key2->text) == 0)
    return 1;
  return 0;
} //}}}

static int
cmdnumber (char *buf) //{{{
{
  char t1 = buf[0] - '0', t2 = buf[1] - '0', t3 = buf[2] - '0';
  if (t1 < 0 || t1 > 9 || t2 < 0 || t2 > 9 || t3 < 0 || t3 > 9)
    return -1;
  return t1 * 100 + t2 * 10 + t3;
} //}}}

static void
getint (int *dest, char *src) //{{{
{
  int p;
  unsigned int temp = 0;
  do
    {
      p = *src - '0';
      if (p < 0 || p > 9)
    break;
      temp = 10 * temp + ((unsigned int) p);
      src++;
    }
  while (1);
  *dest = (int) temp;
  if (*dest < 0)
    *dest = 0;
  return;
} //}}}


// Return -2 if error
// Return -1 if timed out
// Return 0 connection closed
// Return n number of chars received 
static int
mailer_receive (int timeout, mailer *mail, char *buf, int bufsize,
        int *timeused, int blocking) //{{{
{
  int socket = mail->socket, s;
  time_t before, now;
  ssize_t n;
  if (!blocking) 
  {
      n = recv(socket, buf, bufsize, MSG_DONTWAIT);
      if (n == -1)
      {
          mail->etype = mail_ERRNO;
          mail->err = errno;
          return -2;
      }
      return n;
  }
  else 
  {
    fd_set recvfds;
    struct timeval tv;
    tv.tv_sec = timeout;
    tv.tv_usec = 0;
    FD_ZERO (&recvfds);
    FD_SET (socket, &recvfds);
    before = time (NULL);
    s = select (socket + 1, &recvfds, NULL, NULL, &tv);
    if (s == -1)
      {
        mail->err = errno;
        mail->etype = mail_ERRNO;
        return -2;
      }
    if (FD_ISSET (socket, &recvfds))
      {
        n = recv (socket, buf, bufsize, 0);
        if (n == -1)
      {
        mail->err = errno;
        mail->etype = mail_ERRNO;
        return -2;
      }
        if (n == 0)
      {
        mail->etype = mail_CONNCLOSED;
        return 0;
      }
        now = time (NULL);
        *timeused = (now > before ? now - before : 0);
        return n;
      }
    else
      {
        mail->etype = mail_TIMEOUT;
        mail->err = __LINE__;
        return -1;
      }
  }
} //}}}

static int
mailer_getline (char **line, int timeout, mailer * mail, int *tu, int blocking) //{{{
{
  buffer *buf = &(mail->buf);
  int timeused = 0;
  int n = buf->nextline;
  int t = buf->end;
  time_t before = time (NULL), now;
  int newtime = timeout;
  int rc = 1, newend;
  int gotaline = 0;
  if (t > n)
    {
      while (n < t && gotaline == 0)
    {
      if (n > 0 && buf->buf[n - 1] == '\r' && buf->buf[n] == '\n')
        gotaline = 1;
      n++;
    }
      if (gotaline)
    {
      *line = buf->buf + buf->nextline;
      t = n - buf->nextline;
      buf->nextline = n;
      buf->buf[n - 1] = 0;
      buf->buf[n - 2] = '\n';
      *tu = 0;
      return t - 1;
    }
      // we dont have an entire line
      // go fetch the rest of the line 
      newend = buf->end - buf->nextline;
      memmove (buf->buf, buf->buf + buf->nextline, newend);
      buf->nextline = 0;
      buf->end = newend;
    }
  else
    {
      // end <= nextline
      buf->nextline = 0;
      buf->end = 0;
    }
  n = buf->end;
  rc = 1;
  gotaline = 0;
  do
  {
    if (buf->bufsize <= n)
    {
      mail->etype = mail_INTERNALERR;
      mail->err = __LINE__;
      return -2;
    }
    t = mailer_receive (newtime, mail, buf->buf + n, buf->bufsize - n,
            &timeused, blocking);
    if (t <= 0) return -2;
    n += t;
    newtime = timeout > timeused ? timeout - timeused : 0;
    if (newtime == 0)
    {
      mail->etype = mail_TIMEOUT;
      mail->err = __LINE__;
      return -1;
    }
    while (rc < n && gotaline == 0)
    {
      if (buf->buf[rc - 1] == '\r' && buf->buf[rc] == '\n')
        gotaline = 1;
      rc++;
    }
  }
  while (!gotaline);
  t = rc - buf->nextline;
  buf->nextline = rc;
  buf->end = n;
  *line = buf->buf;
  buf->buf[rc - 1] = 0;
  buf->buf[rc - 2] = '\n';
  now = time (NULL);
  *tu = (now > before ? now - before : 0);
  return t - 1;
} //}}}

static int
mailer_send (char *data, mailer * mail, int timeout) //{{{
{
  int len = strlen (data);
  int socket = mail->socket;
  fd_set writefds;
  struct timeval tv;
  int n = 0, t,s;
  time_t before = time (NULL);
  time_t now = before;
  int timeused = 0;
  while (n < len)
  {
    FD_ZERO (&writefds);
    FD_SET (socket, &writefds);
    timeused += now > before ? (now - before) : 0;
    tv.tv_sec = timeout > timeused ? timeout - timeused : 0;
    tv.tv_usec = 0;
    if (tv.tv_sec == 0 && timeout != 0) 
    {
      mail->etype = mail_TIMEOUT;
      mail->err = __LINE__;
      return -1;
    }
    if (timeout != 0)
    {
      s = select (socket + 1, NULL, &writefds, NULL, &tv);
    }
    else 
    {
      s = select (socket + 1, NULL, &writefds, NULL, NULL);
    }
    if (s == -1)
    {
      mail->err = errno;
      mail->etype = mail_ERRNO;
      return -2;
    }
    if (s == 0) 
    { // Timeout occured
      mail->etype = mail_TIMEOUT;
      mail->err = __LINE__;
      return -1;
    }
    now = time (NULL);
    if (FD_ISSET (socket, &writefds))
    {
      t = send (socket, data+n, len-n, 0);
      if (t == -1)
      {
        mail->etype = mail_ERRNO;
        mail->err = errno;
        return -2;
      }
      n+=t;
    }
  }
  return n;
} //}}}

DEFINE_NHASHMAP(answer_map, hashfunc, htentryEqual)

static int
getanswer (reply ** r, int timeout, mailer * mail, int blocking) //{{{
{
  char *line;
  struct charlist *t = mail->partialanswer, *oldt, *t2;
  int n, size;
  int timeleft = timeout;
  int timeused = 0;
  reply *entry, *e;
  do
  {
    timeleft = timeleft > timeused ? timeleft - timeused : 0;
    if (timeleft == 0)
    {
      n = -1;
      mail->etype = mail_TIMEOUT;
      mail->err = __LINE__;
      break;
    }
    n = mailer_getline (&line, timeleft, mail, &timeused, blocking);
    if (n <= 0) break;
    if (n <= 3)
    {
      mail->etype = mail_PROTOCOL_FAILURE;
      mail->err = __LINE__;
      break;
    }
    oldt = t;
    t = (struct charlist *) malloc (sizeof (struct charlist) + n + 1);
    // freed later in this function, unless non-blocking and partial answer
    // then it is put on the partialanswer list
    if (t == NULL)
    {
      t = oldt;
      n = -3;
      mail->etype = mail_INTERNALERR;
      mail->err = __LINE__;
      break;
    }
    t->text = ((char *) t) + sizeof (struct charlist);
    strcpy (t->text, line);
    t->next = oldt;
    t->len = n;
  }
  while (line[3] == '-');

  if (mail->etype == mail_ERRNO && (mail->err == EAGAIN || mail->err == EWOULDBLOCK))
  {
    mail->partialanswer = t;
    mail->etype = mail_OK;
    *r = NULL;
    return 0;
  }

  size = 0;
  oldt = t;
  while (oldt != 0)
  {
    size += oldt->len;
    oldt = oldt->next;
  }

  entry = NULL;
  if (t != NULL) 
  {
    entry = (reply *) malloc (sizeof (reply) + size + 1);
    // if answer already exists then freed later in this function 
    // else it is put in the answer collection to be freed when closing down
    if (entry == NULL)
    {
      mail->etype = mail_INTERNALERR;
      mail->err = __LINE__;
    }
  }
  if (n <= 3 || entry == NULL)
  {               // err
    while (t != NULL)
    {
      oldt = t->next;
      free (t);
      t = oldt;
    }
    mail->partialanswer = NULL;
    if (entry != NULL) free ((reply *) entry);
    return -2;
  }

  entry->text = ((char *) entry) + sizeof (reply);
  // lets turn the list around
  oldt = t->next;
  t->next = NULL;
  while (oldt != NULL)
    {
      t2 = oldt->next;
      oldt->next = t;
      t = oldt;
      oldt = t2;
    }
  // let gather the answer in a reply
  oldt = t;
  n = 0;
  while (oldt != NULL)
    {
      strcpy (entry->text + n, oldt->text);
      n += oldt->len;
      t2 = oldt->next;
      free (oldt);
      oldt = t2;
    }
  mail->partialanswer = NULL;
  entry->number = cmdnumber (entry->text);
  // put the answer in the answer collection
  if (answer_map_find (&(mail->answers), entry, &e) == hash_DNE)
  {
    if (answer_map_insert (&(mail->answers), entry, entry) == hash_OUTOFMEM)
    {
      n = -3;
      mail->etype = mail_INTERNALERR;
      mail->err = __LINE__;
      free (entry);
    }
  }
  else
  {
    n = 5; // needs to be possitive
    free (entry);
    entry = e;
  }
  if (n < 0) return n;
  *r = entry;
  mail->lastreply.text = (*r)->text;
  mail->lastreply.number = (*r)->number;
  return (*r)->number;
} //}}}

static mailer *
mailer_initconn (char *serv, mailer *mail) //{{{
{
  struct addrinfo addr_hint, *addr, *myaddr;
  char *line;
  int c = 0, n, finished, tryhelo;
  char *buf = mail->tmp;
  reply *r;
  mail->me = ((char *) mail) + sizeof (mailer);
  if (answer_map_init (&(mail->answers)) == hash_OUTOFMEM)
    {
      free (mail);
      return NULL;
    }
  mail->err = 0;
  mail->etype = mail_OK;
  mail->buf.buf = mail->me + HOST_NAME_MAX + 1;
  mail->buf.bufsize = BUFFERSIZE;
  mail->buf.nextline = 0;
  mail->buf.end = 0;
  mail->okeyed = NULL;
  mail->lastokeyed = NULL;
  mail->tempfailure = NULL;
  mail->lasttempfailure = NULL;
  mail->permfailure = NULL;
  mail->lastpermfailure = NULL;
  mail->partialanswer = NULL;
  mail->lastemail = NULL;
  if (gethostname (mail->me, HOST_NAME_MAX + 1) == -1)
    {
      mail->err = errno;
      mail->etype = mail_ERRNO;
      return mail;
    }
  mail->me[HOST_NAME_MAX] = 0;
  memset (&addr_hint, 0, sizeof (struct addrinfo));
  addr_hint.ai_family = PF_UNSPEC;
  addr_hint.ai_socktype = SOCK_STREAM;
  if (getaddrinfo (serv, "25", &addr_hint, &addr) != 0 || addr == 0)
  {
    free (mail);
    return NULL;
  }
  setdefaulttimeouts (&(mail->timeout));
  myaddr = addr;
  while (myaddr)
  {
    mail->socket = 
      socket (addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (mail->socket == -1)
    {
      mail->err = errno;
      myaddr = myaddr->ai_next;
    }
    else
    {
      c = connect (mail->socket, myaddr->ai_addr, myaddr->ai_addrlen);
      if (c != 0)
      {
        mail->err = errno;
        close(mail->socket);
        myaddr = myaddr->ai_next;
      }
      else break;
    }
  }
  freeaddrinfo (addr);
  if (mail->socket == -1 || c == -1)
  {
    mail->etype = mail_ERRNO;
    return mail;
  }

  // receive greeting

  n = getanswer (&r, mail->timeout.init, mail, 1);
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, mail->rd->server, 
//        "mailer_initconn 1, %i", n);
  switch (n / 100)
    {
    case 2:
      break;
    case 4:
    case 5:
      mail->etype = mail_SERVICEERR;
      mail->err = n;
      return mail;
    default:
      mail->etype = mail_PROTOCOL_FAILURE;
      mail->err = __LINE__;
      return mail;
      break;
    }

  // send ehlo
  strcpy(buf, "EHLO ");
  n = strlen (buf);
  c = strlen (mail->me);
  if (c > 450) 
  {
    mail->etype = mail_INTERNALERR;
    mail->err = __LINE__;
    return mail;
  }
  strcpy(buf+n, mail->me);
  n += c;
  strcpy(buf+n, "\r\n");
  n = mailer_send (buf, mail, mail->timeout.ehlo);
  if (n < 0) return mail;

  // receive ehlo answer

  tryhelo = 0;
  n = getanswer (&r, mail->timeout.ehlo, mail, 1);
  switch (n / 100)
  {
    case 2:
      break;
    default:
      switch (n)
    {
    case 500:
    case 501:
    case 504:
      // ehlo not working
      tryhelo = 1;
      break;
      // Service closed by policy or other reason
    case 421:
    case 550:
    case 554:
      mail->etype = mail_SERVICEERR;
      mail->err = n;
      return mail;
      break;
    default:
      mail->etype = mail_PROTOCOL_FAILURE;
      mail->err = __LINE__;
      return mail;
      break;
    }
  }
  mail->size = 0;
  mail->eightbitmime = 0;
  mail->pipelining = 0;
  finished = 0;
  line = r->text;
  if (!tryhelo)
  {
    do
    {
      if (strncmp (line + 4, "SIZE", 4) == 0)
      {
        getint (&(mail->size), line + 9);
      }
      if (strncmp (line + 4, "PIPELINING", 10) == 0)
      {
        mail->pipelining = 1;
      }
      if (strncmp (line + 4, "8BITMIME", 8) == 0)
      {
        mail->eightbitmime = 1;
      }
      if (line[3] == '-')
      {
        while (*line != 0 && *line != '\n') line++;
        if (*line == '\n') line++;
        if (*line == 0) finished = 1;
      }
      else
      {
        finished = 1;
      }
    }
    while (!finished);
  }
  else
    {
      n = mailer_send ("RSET\r\n", mail, mail->timeout.ehlo);
      if (n < 0) return mail;
      n = getanswer (&r, mail->timeout.ehlo, mail, 1);
      if (n != 250)
      {
        mail->etype = mail_PROTOCOL_FAILURE;
        mail->err = __LINE__;
      }
      strcpy(buf, "HELO ");
      n = strlen(buf);
      strcpy(buf+n, mail->me);
      n = strlen(buf);
      strcpy(buf+n, "\r\n");
      n = mailer_send (buf, mail, mail->timeout.ehlo);
      if (n < 0) return mail;
      mail->eightbitmime = 0;
      mail->size = 0;
      mail->pipelining = 0;
      n = getanswer (&r, mail->timeout.ehlo, mail, 1);
      if (n < 0) return mail;
      switch (n / 100)
      {
        case 2:
          break;
        default:
          switch (n)
          {
             case 500:
             case 501:
             case 504:
               // ehlo not working
               tryhelo = 1;
               break;
               // Service closed by policy or other reason
             case 421:
             case 550:
             case 554:
               mail->etype = mail_SERVICEERR;
               mail->err = n;
               return mail;
               break;
             default:
               mail->etype = mail_PROTOCOL_FAILURE;
               mail->err = __LINE__;
               return mail;
               break;
          }
      }
    }
  return mail;
} //}}}

mailer *
apsml_mailer_initconnCheckCon(mailer *mail, request_data *rd)/*{{{*/
{
  if (mail && mail->etype != mail_OK) 
  {
    raise_exn ((int ) &exn_OVERFLOW);
  }
  return mail;
}/*}}}*/

mailer *
apsml_mailer_initconn (String serv, int timeouts, request_data *rd) //{{{
{
  mailer *mail;
  mailer *newmail;
  newmail =
    (mailer *) malloc (sizeof (mailer) + HOST_NAME_MAX + 1 + BUFFERSIZE);
  // freed when closing down
  if (newmail == NULL) return NULL;
  mail = newmail;
  mail->rd = rd;
  mail->socket = 0;

  mail->timeout.init = elemRecordML(timeouts, 0);
  mail->timeout.ehlo = elemRecordML(timeouts, 1);
  mail->timeout.mail = elemRecordML(timeouts, 2);
  mail->timeout.rcpt = elemRecordML(timeouts, 3);
  mail->timeout.data = elemRecordML(timeouts, 4);
  mail->timeout.datablock = elemRecordML(timeouts, 5);
  mail->timeout.dataterm = elemRecordML(timeouts, 6);
  
  mail = mailer_initconn (&(serv->data), newmail);
  if (mail == NULL) 
  {
    free(newmail);
    return NULL;
  }
  return mail;
} //}}}

static void
putallonfail(email *e, enum whichfail type, mailer *mail, reply *r) //{{{
{
  struct rcptlist *temp, *list, *lastlist;
  // Put reply r on each receptient 
  temp = e->to;
  if (r != NULL) 
  {
    while (temp != NULL)
    {
      temp->response = r;
      temp = temp->next;
    }
    temp = e->tosend;
    while (temp != NULL)
    {
      temp->response = r;
      temp = temp->next;
    }
    temp = e->toack;
    while (temp != NULL)
    {
      temp->response = r;
      temp = temp->next;
    }
  }
  // Collect all the lists into list
  list = e->to;
  lastlist = e->last;
  if (list == NULL)
  {
    list = e->tosend;
    lastlist = e->lastsend;
  }
  else
  {
    lastlist->next = e->tosend;
    if (e->tosend != NULL)
    {
      lastlist = e->lastsend;
    }
  }
  if (list == NULL)
  {
    list = e->toack;
    lastlist = e->lastack;
  }
  else 
  {
    lastlist->next = e->toack;
    if (e->lastack != NULL)
    {
      lastlist = e->lastack;
    }
  }
  e->to = NULL;
  e->last = NULL;
  e->tosend = NULL;
  e->lastsend = NULL;
  e->toack = NULL;
  e->lastack = NULL;

  // put list on the right kind of list
  if (list != NULL)
  {
    switch (type)
    {
      case mail_PERM:
        if (mail->permfailure == NULL)
        {
          mail->permfailure = list;
          mail->lastpermfailure = lastlist;
        }
        else 
        {
          mail->lastpermfailure->next = list; 
          mail->lastpermfailure = lastlist;
        }
        break;
      case mail_TEMP:
        if (mail->tempfailure == NULL)
        {
          mail->tempfailure = list;
          mail->lasttempfailure = lastlist;
        }
        else 
        {
          mail->lasttempfailure->next = list;
          mail->lasttempfailure = lastlist;
        }
        break;
      case mail_OKEYED:
        if (mail->okeyed == NULL)
        {
          mail->okeyed = list;
          mail->lastokeyed = lastlist;
        }
        else 
        {
          mail->lastokeyed->next = list;
          mail->lastokeyed = lastlist;
        }
        break;
    }
  }
  return;
} //}}}

static void 
putonqueue(struct rcptlist **front, struct rcptlist **back, struct rcptlist *elem) //{{{
{
  if (*front == NULL)
  {
    *front = elem;
    *back = elem;
  }
  else 
  {
    (*back)->next = elem;
    *back = elem;
  }
} //}}}

static int
sendmailfrom(mailer *mail, email *e) //{{{
{
  char *buf  = mail->tmp;
  int n,p;
  if (mail->etype != mail_OK) return -1;
  strcpy(buf, "MAIL FROM:<");
  n = strlen(buf);
  if ((p = strlen(e->from)) > 450-n)
  {
    mail->etype = mail_INTERNALERR;
    mail->err = __LINE__;
    return -2;
  }
  strncpy(buf+n, e->from, 450-n);
  n += p;
  if (mail->size)
    {
      strcpy(buf+n, "> SIZE=");
      n += 7;
      p = snprintf (buf+n, 20, "%i", e->size);
      n+= p;
      strcpy(buf+n, "\r\n");
      n+=2;
    }
  else
  {
    strcpy(buf+n, ">\r\n");
  }
  p = mailer_send(buf, mail, mail->timeout.datablock);
  if (p < 0) return p;
  return 0;
} //}}}

static int
sendrcpt(mailer *mail, email *e) //{{{
{
  char *buf = mail->tmp;
  struct rcptlist *temp;
  int n, p;
  temp = e->to;
  if (temp == NULL) return -4;
  strcpy(buf, "RCPT TO:<");
  n = strlen(buf);
  p = strlen(temp->to);
  if (p > 450)
  {
    mail->etype = mail_INTERNALERR;
    mail->err = __LINE__;
    return -3;
  }
  strcpy(buf+n, temp->to); 
  n += p; 
  strcpy(buf+n, ">\r\n");
  n = mailer_send (buf, mail, mail->timeout.datablock);
  if (n < 0) return n;
  e->to = e->to->next;
  temp->next = 0;
  if (e->to == NULL) e->last = NULL;
  putonqueue(&(e->tosend), &(e->lastsend), temp);
  return 0;
} //}}}

static int
senddatacommand(mailer *mail, email *e) //{{{
{
  int n = mailer_send ("DATA\r\n", mail, mail->timeout.data);
  if (n < 0) return n;
  return 0;
} //}}}

static int
senddata(reply **r, char *data, int datasize, mailer *mail) //{{{
{
  int socket = mail->socket;
  int s,t,n = 0;
  int gotanswer = 0;
  fd_set recvfds;
  fd_set writefds;
  struct timeval tv;
  tv.tv_sec = mail->timeout.datablock;
  tv.tv_usec = 0;
  FD_ZERO (&recvfds);
  FD_ZERO (&writefds);
  FD_SET (socket, &recvfds);
  FD_SET (socket, &writefds);
  while (n < datasize)
  {
    s = select (socket + 1, &recvfds, &writefds, NULL, &tv);
    if (s == -1)
    {
      mail->err = errno;
      mail->etype = mail_ERRNO;
      return -2;
    }
    tv.tv_sec = mail->timeout.datablock;
    tv.tv_usec = 0;
    if (FD_ISSET(socket, &recvfds)) 
    {
      t = getanswer(r, mail->timeout.dataterm, mail, 0);
      if (t < 0) return -2;
      if (t > 0) 
      {
        gotanswer = 1;
        break;
      }
    }
    if (FD_ISSET(socket, &writefds))
    {
      t = send(socket, data + n, datasize - n, MSG_DONTWAIT);
      if (t < 0) 
      {
        mail->err = errno;
        mail->etype = mail_ERRNO;
        return -2;
      }
      n += t;
    }
  }
  if (n < datasize)
  {
    mail->etype = mail_PARTIALSEND;
    mail->err = n;
    return -2;
  }
  if (!gotanswer) t = getanswer(r, mail->timeout.dataterm, mail, 1);
  return t;
} //}}}

// return 0 if everything ok
// return -1 if mail could not be delivered (connection need a reset after this)
// return -2 if some weird thing happend (connection closed by other end, etc.)
// return -3 if servers reply did not parse (very bad)
// return -4 if internal invariants are broken (even worse)
// data must be as described in rfc 2821
// thus data must be terminated by .\r\n
// all lines in data must be less than 1000 chars long (\r\n inclusive)
static int
sendmail (mailer *mail, email *e, int *left) //{{{
{
  int sstate = 0, n, cstate = 0;
  struct rcptlist *temp;
  // Should we block, and how many messages are out there
  int blocking;
  reply *r;
  *left = 0;
  if (e->to == NULL || mail == NULL) return 0;
  blocking = !(mail->pipelining);
  // sstate defines the state of the server, cstate defines the state of the client (us)
  while (1)
  {
    if (e->to == NULL && e->tosend == NULL && e->toack == NULL && sstate != 3)
    { // No mail to send
      // if DATA command has been sent and server is breaking RFC2821 by answering
      // 354 without any receptients send empty mail ".\r\n" (2821 section ...)
      return -1;
    }
    switch (sstate)
    {
      case 0: //{{{
        n = sendmailfrom(mail, e);
        if (n < 0) return n;
        sstate = 1;
        (*left)++;
        break; //}}}
      case 1: //{{{
        if (e->to != NULL)
        {
          // send rcpt to: and move from to -> tosend
          n = sendrcpt(mail, e);
          if (n < 0) return -2;
          (*left)++;
          if (e->to == NULL) sstate = 2;
        }
        break; //}}}
      case 2: //{{{
        n = senddatacommand(mail, e);
        if (n < 0) return -2;
        blocking = 1;
        (*left)++;
        sstate = 3;
        break;  //}}}
      case 3: //{{{
        break; //}}}
    }
    switch (cstate)
    {
      case 0:  // get MAIL FROM: reply {{{
        n = getanswer (&r, mail->timeout.mail, mail, blocking);
        if (n < 0) return -2;
        if (r != NULL)
        {
          cstate = 1;
          (*left)--;
          switch (n / 100)
          {
            case 2:
              // do nothing
              break;
            case 4:
              e->response = r;
              putallonfail(e, mail_TEMP, mail, r);
              return -1;
              break;
            case 5:
              e->response = r;
              putallonfail(e, mail_PERM, mail, r);
              if (n == 503)
              {
                mail->etype = mail_PROTOCOL_FAILURE;
                mail->err = __LINE__;
              }
              return -1;
              break;
            default:
              e->response = r;
              putallonfail(e, mail_PERM, mail, r);
              mail->etype = mail_PROTOCOL_FAILURE;
              mail->err = __LINE__;
              return -3;
              break;
          }
        }
        break; //}}}
      case 1:  // get RCPT TO: replies {{{
          n = getanswer (&r, mail->timeout.rcpt, mail, blocking);
          if (n < 0) return -2;
          if (r != NULL)
          {
            if (e->tosend == NULL)
            { // invarient: cstate == 1 && e->tosend == NULL  => false
              mail->etype = mail_INTERNALERR;
              mail->err = __LINE__;
              e->response = r;
              putallonfail(e, mail_PERM, mail, NULL);
              return -4;
            }
            temp = e->tosend;
            temp->response = r;
            (*left)--;
            switch (n/100)
              {
              case 2:
                putonqueue(&(e->toack), &(e->lastack), temp);
                break;
              case 4:
                putonqueue(&(mail->tempfailure), &(mail->lasttempfailure), temp);
                break;
              case 5:
                putonqueue(&(mail->permfailure), &(mail->lastpermfailure), temp);
                break;
              default:
                e->response = r;
                mail->etype = mail_PROTOCOL_FAILURE;
                mail->err = __LINE__;
                putallonfail(e, mail_PERM, mail, r);
                return -3;
                break;
              }
            e->tosend = e->tosend->next;
            temp->next = 0;
            if (e->tosend == NULL) e->lastsend = NULL;
            if (e->to == NULL && e->tosend == NULL) cstate = 2;
          }
          break; //}}}
      case 2: // get DATA reply {{{
        n = getanswer (&r, mail->timeout.data, mail, 1);
        if (n < 0) return -2;
        (*left)--;
        switch (n/100)
        {
          case 3:
            // 354 -> we are ready to send data 
            if (n == 354) break;
            e->response = r;
            putallonfail(e, mail_PERM, mail, r);
            mail->etype = mail_PROTOCOL_FAILURE;
            mail->err = __LINE__;
            return -3;
            break;
          case 4:
            e->response = r;
            putallonfail(e, mail_TEMP, mail, r);
            return -1;
            break;
          case 5:
            e->response = r;
            putallonfail(e, mail_PERM, mail, r);
            if (n == 503)
            {
              mail->etype = mail_PROTOCOL_FAILURE;
              mail->err = __LINE__;
            }
            return -1;
            break;
          default:
            e->response = r;
            putallonfail(e, mail_PERM, mail, r);
            mail->etype = mail_PROTOCOL_FAILURE;
            mail->err = __LINE__;
            return -3;
            break;
        }
        cstate = 3;
        break; //}}}
      case 3: //{{{
        if (e->toack == NULL)
        {
          n = senddata(&r, ".\r\n", 3, mail);
        }
        else 
        {
          n = senddata (&r, e->data, e->size, mail);
        }
        if (n < 0) return -2;
        switch (n / 100)
        {
          case 2:
            e->response = r;
            putallonfail(e, mail_OKEYED, mail, NULL);
            return 0;
            break;
          case 4:
            e->response = r;
            putallonfail(e, mail_TEMP, mail, r);
            return -1;
            break;
          case 5:
            e->response = r;
            putallonfail(e, mail_PERM, mail, r);
            return -1;
            break;
          default:
            e->response = r;
            mail->etype = mail_PROTOCOL_FAILURE;
            mail->err = __LINE__;
            return -3;
            break;
        }
        break; //}}}
    }
  }
  return 0;
} //}}}

static void
closeconn(mailer *mail, email *e)/*{{{*/
{
  int n;
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, mail->rd->server, 
//        "closeconn 1");
  if (e != NULL) 
  {
    putallonfail(e, mail_PERM, mail, NULL); // &(mail->lastreply));
  }
  if (mail->socket)
  {
    n = close(mail->socket);
    if (n == -1)
    { // As we always get replies we cannot lose any data by a fail in close
      // thus nothing to do here
    }
    mail->socket = 0;
  }
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, mail->rd->server, 
//        "closeconn end");
  return;
}/*}}}*/

int
apsml_closeconn (mailer *mail, request_data *rd)/*{{{*/
{
  struct charlist *p;
  reply *r;
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, rd->server, 
//        "apsml_closeconn 1");
  if (mail->socket && mail->etype == mail_OK)
  {
    mailer_send("QUIT\r\n", mail, mail->timeout.mail);
    getanswer(&r, mail->timeout.mail, mail, 1);
  }
  closeconn(mail, NULL);
  answer_map_apply(&(mail->answers),  (void (*) (reply *)) free);
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, rd->server, 
//        "apsml_closeconn 6");
  answer_map_close(&(mail->answers));
  p = mail->partialanswer;
  while (p)
  {
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, rd->server, 
//        "apsml_closeconn freeing partial answers");
    free(mail->partialanswer);
    p = p->next;
    mail->partialanswer = p;
  }
  mail->partialanswer = NULL;
  if (mail->lastemail) free (mail->lastemail);
  free(mail);
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, rd->server, 
//        "apsml_closeconn end");
  return 0;
}/*}}}*/

static int
cleanup(mailer *mail,  email *e, int left)/*{{{*/
{
  int n,p;
  reply *r;
  putallonfail(e, mail_PERM, mail, &(mail->lastreply));
  while (left--)
  {
    n = getanswer (&r, mail->timeout.mail, mail, 1);
    if (n < 0) return -2;
  }
  p = mailer_send("RSET\r\n", mail, mail->timeout.mail);
  if (p < 0) return -2;
  n = getanswer (&r, mail->timeout.mail, mail, 1);
  if (n < 0) return -2;
  return 0;
}/*}}}*/

int
apsml_sendmail(int tolist, int tolistlength, String from, String data, mailer *mail) //{{{
{
  int elemML, n, left;
  struct rcptlist *temp, *to, *old;
  email *e;
  if (mail->socket == 0 || mail->etype != mail_OK)
  {
    raise_exn ((int ) &exn_OVERFLOW);
  }
  e = (email *) malloc (tolistlength * sizeof (struct rcptlist) + sizeof (email));
  if (e == NULL) 
  {
    raise_exn ((int ) &exn_OVERFLOW);
  }
  mail->lastemail = e;
  e->last = (struct rcptlist *) (e+1);
  temp = e->last;
  old = 0;
  for (n = 0 ; isCONS(tolist) && n < tolistlength ; tolist = tl(tolist), n++, temp++)
  {
    elemML = hd(tolist);
    temp->next = old;
    temp->id = first(elemML);
    temp->to = &(((String) second(elemML))->data);
    temp->response = NULL;
    old = temp;
  }
  to = old;
  e->to = to;
  e->from = &(from->data);
  e->tosend = NULL;
  e->lastsend = NULL;
  e->toack = NULL;
  e->lastack = NULL;
  e->data = &(data->data);
  e->size = strlen(e->data);
  e->response = NULL;
  n = sendmail(mail, e, &left);
    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, mail->rd->server, 
        "apsml_closeconn n = %d, etype = %d, err = %d", n, mail->etype, mail->err);
  switch (n)
  {
    case 0:
      if (e != NULL) free (e);
      e = NULL;
      mail->lastemail = NULL;
      break;
    case -1:
      n = cleanup(mail, e, left);
      if (e != NULL) free (e);
      e = NULL;
      mail->lastemail = NULL;
      if (n == 0) break;
    case -2: // do the same as in case 3
    case -3:
      closeconn(mail, e);
      if (e != NULL) free (e);
      e = NULL;
      mail->lastemail = NULL;
      break;
    default:
      closeconn(mail, e);
      if (e != NULL) free (e);
      e = NULL;
      mail->lastemail = NULL;
      break;
  }
  return n;
} //}}}

uintptr_t *
apsml_mailget(Region rAddrLPairs, Region rAddrEPairs, Region rAddrString, 
    int i, mailer *mail)  //{{{
{
  struct rcptlist *temp;
  String rs;
  char *res;
  uintptr_t *pair, *listpair, *list;
  makeNIL (list);
//    ap_log_error (APLOG_MARK, LOG_DEBUG, 0, mail->rd->server, 
//        "apsml_mailget 1");

  if (mail == NULL) return list;
  switch (i)
  {
    case 0:
      temp = mail->okeyed;
      break;
    case 1:
      temp = mail->tempfailure;
      break;
    case 2:
      temp = mail->permfailure;
      break;
    default:
      raise_exn ((int ) &exn_OVERFLOW);
      return NULL; // Shut up
      break;
  }
  for ( ; temp != 0 ; temp = temp->next)
  {
    switch (mail->etype)
    {
      case mail_OK:
        res = temp->response != NULL ? temp->response->text : mail->lastreply.text;
        break;
      case mail_TIMEOUT:
        res = temp->response != NULL ? temp->response->text : "Connection timeout";
        break;
      case mail_SERVICEERR:
        res = temp->response != NULL ? temp->response->text : "No active mail service";
        break;
      default:
        res = temp->response != NULL ? temp->response->text : "Bad server behavior";
        break;
    }
    if (res == NULL) res = "";
    rs = convertStringToML(rAddrString, res);
    allocRecordML (rAddrEPairs, 2, pair);
    allocRecordML (rAddrLPairs, 2, listpair);
    first (pair) = temp->id;
    second (pair) = (uintptr_t) rs;
    first (listpair) = (uintptr_t) pair;
    second (listpair) = (uintptr_t) list;
    makeCONS(listpair, list);
  }
  switch (i)
  {
    case 0:
      mail->okeyed = NULL;
      mail->lastokeyed = NULL;
      break;
    case 1:
      mail->tempfailure = NULL;
      mail->lasttempfailure = NULL;
      break;
    case 2:
      mail->permfailure = NULL;
      mail->lastpermfailure = NULL;
      break;
    default:
      raise_exn ((int ) &exn_OVERFLOW);
      break;
  }
  return list;
} //}}}

int 
apsml_mailGetError(int pair, mailer *mail)/*{{{*/
{
  if (mail)
  {
    first (pair) = mail->etype;
    second (pair) = mail->err;
  }
  else 
  {
    first(pair) = 8;
    second(pair) = 0;
  }
  return pair;
}/*}}}*/

/*
static void
ppError(mailer *mail) //{{{
{ 
  if (mail==NULL)
  {
    printf("No mailer created\n");
  }
  switch (mail->etype)
  {
    case mail_OK:
      return;
      break;
    case mail_ERRNO:
      printf ("Err: %s\n", strerror (mail->err));
      return;
      break;
    case mail_TIMEOUT:
      printf ("Connection timed out\n");
      return;
      break;
    case mail_PROTOCOL_FAILURE:
      printf ("Protocol failure registered in line %i\nLast reply was: %s\n",
          mail->err, mail->lastreply.text);
      return;
      break;
    case mail_INTERNALERR:
      printf ("Internal error registered in line %i\n", mail->err);
      return;
      break;
    case mail_CONNCLOSED:
      printf ("Connection closed by other end\n");
      return;
      break;
    case mail_SERVICEERR:
      printf ("Service err: %s", mail->lastreply.text);
      return;
      break;
    case mail_PARTIALSEND:
      printf ("Mail only partially sent: %s", mail->lastreply.text);
      return;
      break;
  }
  return;
} //}}}
*/

/*
int
main (int argc, char **argv) //{{{
{
  struct rcptlist to0, to1, *temp;
  int n;
  email e;
  mailer *mail;
  if (argc < 2)
    return 0;
  mail = mailer_initconn (argv[1]);
  if (mail == NULL || mail->etype != mail_OK) 
  {
    ppError(mail);
    return 0;
  }

  to0.next = &to1;
  to0.id = 1;
  to0.to = "varming@itu.dk";
  to1.next = 0;
  to1.id = 2;
  to1.to = "varming@diku.dk";
  e.from = "varming@diku.dk";
  e.to = &to0;
  e.last = &to1;
  e.tosend = NULL;
  e.lastsend = NULL;
  e.toack = NULL;
  e.lastack = NULL;
  e.response = NULL;
  e.data = "From: Carsten Varming <varming@acm.org>\r\nTo: CV <varming@diku.dk>\r\n.\r\n";
  e.size = strlen(e.data);
  
  n = sendmail(mail, &e);
  printf("sendmail returned %i\n", n);
  if (mail == NULL || mail->etype != mail_OK) ppError(mail);
  temp = mail->okeyed;
  while (temp != NULL)
  {
    printf("This was in okeyed: %s, %s\n", temp->to, temp->response->text); 
    temp = temp->next;
  }
  temp = mail->tempfailure;
  while (temp != NULL)
  {
    printf("This was in tempfailure : %s, %s\n", temp->to, temp->response->text); 
    temp = temp->next;
  }
  temp = mail->permfailure;
  while (temp != NULL)
  {
    printf("This was in permfailure : %s, %s\n", temp->to, temp->response->text); 
    temp = temp->next;
  }
  return 0;
} //}}} 
*/
