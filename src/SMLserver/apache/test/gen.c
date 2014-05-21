#include "stdlib.h"
#include "stdio.h"
#include "unistd.h"
#include "time.h"

#define BUFSIZE 1001

int
put(int time, char *data)
{
  struct timespec *tv;
  int r;
  char *d = data;
  while (*d) d++;
  *d = '\r';
  d++;
  *d = '\n';
  d++;
  *d = 0;
  if (time != 0)
  {
    tv = (struct timespec *) malloc(sizeof(struct timespec));
    if (tv == NULL) return -1;
    tv->tv_sec = time / 1000;
    tv->tv_nsec = (time % 1000) * 1000000;
    r = nanosleep(tv, tv);
    free(tv);
    if (r == -1)
    {
      perror("Nanosleep in put:\n");
      free(tv);
      return -1;
    }
  }
  r = printf("%s", data);
  return 0;
}

int 
main(int argc, char **argv)
{
  char *buf;
  ssize_t r;
  int time, lineno = 1;
  buf = (char *) malloc (BUFSIZE);
  if (buf == NULL) return EXIT_FAILURE;
  while (1)
  {
    r = scanf("%c", buf);
    if (r == EOF)
    {
      return EXIT_SUCCESS;
    }
    switch (buf[0])
    {
      case 'S':
        r = scanf(":%i:%1000[^\n]\n", &time, buf);
        if (r != 2)
        {
          fprintf(stderr, "syntax error in line %i\n", lineno);
          return EXIT_FAILURE;
        }
        if (put(time, buf) == -1)
        {
          return EXIT_FAILURE;
        }
        break;
      case 'C':
        scanf("%*[^\n]\n");
        lineno++;
        break;
      default:
        fprintf(stderr, "syntax error in line %i\n", lineno);
        return EXIT_FAILURE;
        break;
    }
  }
}
