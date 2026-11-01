/* Stub the syscall to cover positions that real filesystems may not support.
 * Link this test with the actual Posix.c wrapper, compiled with lseek renamed. */
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "Tagging.h"

extern ssize_t sml_lseek(ssize_t, ssize_t, ssize_t);

static off_t syscall_result;
static int syscall_errno;
static unsigned calls;

/* The signed offset also checks argument decoding at native integer width. */
off_t mlkit_test_lseek(int fd, off_t offset, int whence)
{
  assert(fd == 17);
  assert(offset == -(off_t)5368709243LL);
  assert(whence == SEEK_CUR);
  calls++;
  errno = syscall_errno;
  return syscall_result;
}

static ssize_t seek_result(off_t value, int error)
{
  syscall_result = value;
  syscall_errno = error;
  calls = 0;
  errno = 0;
  ssize_t result = sml_lseek(convertIntToML(17),
                            convertIntToML(-(ssize_t)5368709243LL),
                            convertIntToML(2));
  assert(calls == 1);
  return result;
}

int main(void)
{
  _Static_assert(sizeof(off_t) >= sizeof(intptr_t), "native-width off_t required");
#ifdef TAG_VALUES
  const off_t maximum = (off_t)(INTPTR_MAX >> 1);
#else
  const off_t maximum = (off_t)INTPTR_MAX;
#endif
  ssize_t result = seek_result(maximum, 0);
  assert(result == (ssize_t)convertIntToML(maximum));
  assert(convertIntToC(result) == maximum);
  assert(errno == 0);

#ifdef TAG_VALUES
  result = seek_result(maximum + 1, 0);
  assert(result == (ssize_t)convertIntToML(-1));
  assert(errno == EOVERFLOW);
#else
  /* The tagged boundary remains representable in the untagged runtime. */
  const off_t above_tagged = (off_t)(INTPTR_MAX >> 1) + 1;
  result = seek_result(above_tagged, 0);
  assert(convertIntToC(result) == above_tagged);
  assert(errno == 0);
#endif

  result = seek_result(-1, EBADF);
  assert(result == (ssize_t)convertIntToML(-1));
  assert(errno == EBADF);
  puts("POSIX seek result boundaries passed");
  return 0;
}
