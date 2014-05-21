#include "stdlib.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "arpa/inet.h"
#include "errno.h"
#include "sys/select.h"
#include "string.h"
#include "unistd.h"
#include "stdio.h"

#define INBUF_SIZE 10000
#define OUTBUF_SIZE 10000

typedef struct
{
  int next;
  int current;
  int size;
  char *data;
  char *data2;
  char *line;
  int sleeptime;
} buf;

int
linelength(char *s)
{
  int l = 0;
  while (*s)
  {
    l++;
    if (*s == '\n') break;
    s++;
  }
  return l;
}

void
linemove(char *dest, char *src)
{
  while (*src)
  {
    if (*src == '\n') break;
    *dest = *src;
    dest++;
    src++;
  }
  *dest = '\n';
  dest++;
  *dest = 0;
  return;
}

int lineno; 
int
parse(char *src, buf *b)
{
  int r, t;
  char *d;
  char *dest = b->data2;
  d = dest;
  r = sscanf(src, "S:%i:%1000[^\n]\n", &t, dest);
  switch (r)
  {
    case 0:
      r = sscanf(src, "C%*[^\n]\n");
      switch (r)
      {
        case 0:
          lineno++;
          return -1;
          break;
        case EOF:
          return 0;
          break;
        default:
          fprintf(stderr, "syntax error in line %i\n", lineno);
          return -2;
          break;
      }
      break;
    case 2:
      lineno++;
      b->sleeptime = t;
      r = strlen(dest);
      linemove(b->line, src);
      while (*d) d++;
      *d = '\r';
      d++;
      *d = '\n';
      d++;
      *d = 0;
      return strlen(dest);
      break;
    case EOF:
      return 0;
      break;
    default:
      fprintf(stderr, "syntax error in line %i\n", lineno);
      return -2;
      break;
  }
  return strlen(dest);
}

void
printlines(char *src)
{
  char *tmp = src;
  int i;
  if (*src == 0) return;
  for(i = 1; tmp[i]; i++)
  {
    if (tmp[i-1] == '\r' && tmp[i] == '\n')
    {
      tmp[i-1] = '\n';
      tmp[i] = 0;
      printf("C:%s", src);
      src = tmp+i+1;
      tmp = tmp+i;
      i = 0;
    }
  }
  return;
}

int
getline(buf *b, int r, char **rv)
{
  int n,m;
  char *tmp;
  if (r)
  {
    m = b->next > b->current ? b->next - b->current : 0;
    if (m)
    {
      memmove(b->data, b->data + b->current, m);
      b->current = 0;
      b->next -= m;
    }
      n = read(STDIN_FILENO, b->data + b->next, b->size - b->next);
    if (n == -1) 
    {
      perror("stdin err ");
      return -3;
    }
    if (n == 0) 
    {
      fprintf(stderr, "stdin EOF\n");
      return -1;
    }
    b->next += n;
  }
  for (n = 0; b->current+n < b->next; n++)
  {
    if (b->data[b->current+n] == '\n')
    {
      tmp = b->data+b->current;
      b->current = b->current+n+1;
      m = parse(tmp, b); 
      if (m == -1)
      {
        n = -1;
        continue;
      }
      *rv = b->data2;
      return m;
    }
  }
  return 0;
}

void
sendline(int conn, char *inbuf, int length)
{
  int acc,m;
  acc = 0;
  do 
  {
    m = send(conn, inbuf+acc, length-acc, 0);
    acc += m;
    if (m == -1) 
    {
      perror("send err\n");
      close(conn);
      exit(EXIT_FAILURE);
    }
  } while (acc < length);
}

int
main(int argc, char **argv)
{
	if (argc < 2) return EXIT_FAILURE;
	int port;
	unsigned int sin_size;
	struct sockaddr_in addr;
	int n = sscanf( argv[1], "%i", &port);
	if (n == 0) 
	{
		fprintf(stderr, "arg 1 == port must be a number\nYou typed %s\n", argv[1]);
		return EXIT_FAILURE;
	}
	else
	{
		fprintf(stderr, "Port == %i\n", port);
	}

	int sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock == -1) 
	{
		perror("socket err");
		return EXIT_FAILURE;
	}
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)))
	{
		perror("bind Err ");
		return EXIT_FAILURE;
	}

	if (listen(sock, SOMAXCONN) == -1)
	{
		if (errno == EADDRINUSE) perror("port in use\n");
		perror("listen err\n");
		return EXIT_FAILURE;
	}

	sin_size = sizeof(struct sockaddr_in);
	int conn = accept(sock, (struct sockaddr *) &addr, &sin_size);
	if (conn == -1) 
	{
		perror("accept err\n");
		return EXIT_FAILURE;
	}
	if (close (sock)) perror("close on socket failed\n");
	fprintf(stderr, "Connection accepted\n");

  buf b;
	char *inbuf;
  b.data = (char *) malloc(INBUF_SIZE);
  b.data2 = (char *) malloc(INBUF_SIZE);
  b.line = (char *) malloc(INBUF_SIZE);
  b.size = INBUF_SIZE;
  b.current = 0;
  b.next = 0;
  b.sleeptime = 0;
	char *outbuf = (char *) malloc(OUTBUF_SIZE);
  int m,r, haveline = 0, blocking = 0;
  long t1,t2;
  struct timeval tv1;
  struct timeval *tv;
	fd_set readdata;
	do 
  {
    FD_ZERO(&readdata);
    tv = NULL;
    if (haveline == 0)
    {
      FD_SET(STDIN_FILENO, &readdata);
    }
    else 
    {
      if (b.sleeptime)
      {
        tv1.tv_sec = b.sleeptime / 1000;
        tv1.tv_usec = (b.sleeptime % 1000) * 1000;
        tv = &tv1;
      }
    }
    FD_SET(conn, &readdata);
    r = select(conn + 1, &readdata, NULL, NULL, tv);
    if (r == -1)
    {
      perror("select Err\n");
      exit(EXIT_FAILURE);
    }
    if (r > 0) 
    { // timeout did not occur
      if (tv)
      {
        t1 = tv->tv_sec > 0 ? tv->tv_sec : 0;
        t2 = tv->tv_usec > 0 ? tv->tv_usec : 0;
        b.sleeptime = t1 * 1000 + (t1 / 1000);
      }
    }
    else 
    { // Timeout occured
      b.sleeptime = 0;
    }
    n = 0;
    if (FD_ISSET(STDIN_FILENO, &readdata))
    {    // copy from stdin to conn
      n = getline(&b, 1, &inbuf);
      if (n < 0)
      {
        close(conn);
        if (n == -1) return EXIT_SUCCESS;
        return EXIT_FAILURE;
      }
      if (n > 0) 
      {
        haveline = n;
      }
    }

    if (FD_ISSET(conn, &readdata)) 
    {    // print conn to stdout
      m = recv(conn, outbuf, OUTBUF_SIZE-1, 0);
      if (m == -1)
      {
        perror("recv error:\n");
        exit(EXIT_FAILURE);
      }
      if (m == 0)
      {
        fprintf(stderr, "connection closed by other end\n");
        close(conn);
        exit(EXIT_SUCCESS);
      }
      outbuf[m] = 0;
      printlines(outbuf);
      for (n = 0; n<m; n++)
      {
        if (outbuf[n] == '\n') blocking--;
      }
    }
    if (haveline && blocking <= 0 && b.sleeptime <= 0)
    {
      sendline(conn, inbuf, haveline);
      haveline = 0;
      blocking++;
      printf("%s", b.line);
    }
    if (haveline == 0)
    {
      n = getline(&b, 0, &inbuf);
      if (n < 0) 
      {
        close(conn);
        if (n == -1) return EXIT_SUCCESS;
        return EXIT_FAILURE;
      }
      if (n > 0) 
      {
        haveline = n;
      }
    }
	} while (1);
	return EXIT_SUCCESS;
}

