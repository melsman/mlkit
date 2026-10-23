/* Manual benchmark timer: retain wait4 CPU precision for short programs. */
#define _DEFAULT_SOURCE
#define _DARWIN_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
static double seconds(struct timeval t) { return t.tv_sec+t.tv_usec/1e6; }
int main(int argc,char **argv) {
  if (argc<3) return 2;
  int output=open(argv[1],O_WRONLY|O_CREAT|O_TRUNC,0666);
  if (output<0) { perror("open"); return 2; }
  struct timespec begin,end;
  if (clock_gettime(CLOCK_MONOTONIC,&begin)) return 2;
  pid_t pid=fork();
  if (pid<0) { perror("fork"); return 2; }
  if (!pid) {
    if (dup2(output,STDOUT_FILENO)<0) _exit(126);
    close(output);
    execvp(argv[2],argv+2);
    perror("execvp"); _exit(127);
  }
  close(output);
  struct rusage usage;
  int status;
  pid_t result;
  do { result=wait4(pid,&status,0,&usage); } while (result<0 && errno==EINTR);
  if (result<0 || clock_gettime(CLOCK_MONOTONIC,&end)) return 2;
  if (!WIFEXITED(status) || WEXITSTATUS(status)) {
    fprintf(stderr,"Benchmark failed, wait status %d\n",status); return 1;
  }
  double rss=usage.ru_maxrss;
#ifdef __APPLE__
  rss/=1024;
#endif
  printf("%.6f,%.6f,%.9f,%.0f\n",seconds(usage.ru_utime),seconds(usage.ru_stime),
         end.tv_sec-begin.tv_sec+(end.tv_nsec-begin.tv_nsec)/1e9,rss);
  return 0;
}
