#define _POSIX_C_SOURCE 200809L
#include <stdint.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
static uintptr_t tp_wait_raw(uintptr_t milliseconds) {
  struct timespec remaining = {milliseconds/1000,(milliseconds%1000)*1000000};
  while (nanosleep(&remaining,&remaining) && errno == EINTR) {}
  return 1;
}
uintptr_t tp_wait(uintptr_t milliseconds) {
#ifdef TP_TAGGED
  milliseconds >>= 1;
#endif
  return tp_wait_raw(milliseconds);
}
uintptr_t tp_masked_wait(void) {
  sigset_t mask, previous;
  sigemptyset(&mask); sigaddset(&mask,SIGALRM);
  sigprocmask(SIG_BLOCK,&mask,&previous);
  tp_wait_raw(80);
  sigprocmask(SIG_SETMASK,&previous,0);
  return 1;
}
#ifdef TP_SLOW_READER
#include <unistd.h>
int main(void) {
  char bytes[256];
  ssize_t n;
  while ((n = read(0,bytes,sizeof(bytes))) > 0) {
    ssize_t done = 0;
    while (done < n) {
      ssize_t written = write(1,bytes+done,n-done);
      if (written < 0) { if (errno == EINTR) continue; return 1; }
      done += written;
    }
    tp_wait_raw(10);
  }
  return n < 0;
}
#endif
