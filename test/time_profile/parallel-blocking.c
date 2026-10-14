#define _POSIX_C_SOURCE 200809L
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
long tp_parallel_sleep(long masked) {
  sigset_t mask, old;
  sigemptyset(&mask);sigaddset(&mask,SIGUSR2);
  if(masked && pthread_sigmask(SIG_BLOCK,&mask,&old)) return -1;
  struct timespec left={0,250000000};
  while(nanosleep(&left,&left) && errno==EINTR) {}
  if(masked && pthread_sigmask(SIG_SETMASK,&old,NULL)) return -1;
  return 0;
}
