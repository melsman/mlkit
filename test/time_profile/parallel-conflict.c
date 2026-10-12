#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
static void existing(int sig) { (void)sig; }
__attribute__((constructor)) static void conflict(void) {
  const char *mode=getenv("MLKIT_TP_PAR_CONFLICT");
  if(!mode) return;
  if(!strcmp(mode,"handler")) {struct sigaction action={0};action.sa_handler=existing;sigemptyset(&action.sa_mask);sigaction(SIGUSR2,&action,NULL);}
  if(!strcmp(mode,"mask")) {sigset_t mask;sigemptyset(&mask);sigaddset(&mask,SIGUSR2);pthread_sigmask(SIG_BLOCK,&mask,NULL);}
}
long tp_parallel_probe(long n) { return n; }
