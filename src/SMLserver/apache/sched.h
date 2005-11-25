#include "httpd.h"

int startsched (const char *server, apr_port_t port, int infile);

typedef struct
{
  uint32_t length;
  uint32_t first;
  uint32_t interval;
  uint32_t type;
  uint32_t port;
  uint32_t serverlength;
} schedHeader;

