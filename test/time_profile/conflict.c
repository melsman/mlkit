#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
__attribute__((constructor)) static void conflict(void) {
  const char *mode=getenv("MLKIT_TP_TEST_CONFLICT");
  if(!mode)return;
  if(!strcmp(mode,"mask")) {
    sigset_t mask;sigemptyset(&mask);sigaddset(&mask,SIGALRM);
    if(sigprocmask(SIG_BLOCK,&mask,NULL))abort();
  } else if(!strcmp(mode,"handler")) {
    struct sigaction action={0};action.sa_handler=SIG_IGN;sigemptyset(&action.sa_mask);
    if(sigaction(SIGALRM,&action,NULL))abort();
  } else if(!strcmp(mode,"timer")) {
    struct itimerval timer={{1,0},{1,0}};
    if(setitimer(ITIMER_REAL,&timer,NULL))abort();
  } else abort();
}
long tp_conflict_probe(long x) { return x; }
