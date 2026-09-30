/* Test fixture: shell has no portable Unix datagram primitive. */
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
int main(int argc, char **argv) {
  struct sockaddr_un addr = {0};
  const char *commands[] = {"sample", "start", "pause", "sample", "flush"};
  struct timespec delay = {0, 20000000};
  if (argc != 2 || strlen(argv[1]) >= sizeof(addr.sun_path)) return 1;
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, argv[1]);
  int fd = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (fd < 0) { perror("socket"); return 1; }
  for (unsigned i = 0; i < sizeof(commands)/sizeof(*commands); ++i) {
    int retries = 100;
    while (sendto(fd, commands[i], strlen(commands[i]), 0,
                  (struct sockaddr *)&addr, sizeof(addr)) < 0) {
      if (errno != ENOENT || --retries == 0) { perror("sendto"); close(fd); return 1; }
      nanosleep(&delay, NULL);
    }
    nanosleep(&delay, NULL);
  }
  close(fd);
  return 0;
}
