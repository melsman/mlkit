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
/* Preserve the wire stream, delaying samples rather than the much larger
 * static code/IR metadata. Return 0 only for EOF before a new frame. */
static int transfer(unsigned char *bytes, size_t size) {
  size_t done = 0;
  while (done < size) {
    ssize_t n = read(0,bytes+done,size-done);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) return done == 0 && n == 0 ? 0 : -1;
    done += (size_t)n;
  }
  done = 0;
  while (done < size) {
    ssize_t n = write(1,bytes+done,size-done);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) return -1;
    done += (size_t)n;
  }
  return 1;
}
int main(void) {
  unsigned char bytes[4096];
  if (transfer(bytes,8) != 1) return 1;
  for (;;) {
    int status = transfer(bytes,4);
    if (status == 0) return 0;
    if (status < 0) return 1;
    uint32_t remaining = (uint32_t)bytes[0] | (uint32_t)bytes[1]<<8 |
                         (uint32_t)bytes[2]<<16 | (uint32_t)bytes[3]<<24;
    if (remaining == 0 || transfer(bytes,1) != 1) return 1;
    if (bytes[0] == 24) tp_wait_raw(1); /* time_sample */
    remaining--;
    while (remaining) {
      size_t n = remaining < sizeof(bytes) ? remaining : sizeof(bytes);
      if (transfer(bytes,n) != 1) return 1;
      remaining -= (uint32_t)n;
    }
  }
}
#endif
