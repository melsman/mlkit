#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parseul.h"

int
main(int argc, char **argv)
{
  int r;
  struct parseCtx c;
  if (argc < 2) return EXIT_FAILURE;
  c.fileprefix = "/home/varming/mlkit/kit/src/SMLserver/apache";
  c.mapprefix = "/mapprefix/path";
  c.root = "/root//";
  c.fpl = strlen(c.fileprefix);
  c.mpl = strlen(c.mapprefix);
  c.rl = strlen(c.root);
  c.uoTable = NULL;
  c.smlTable = NULL;
  c.ulTable = NULL;
  r = recurseParse(&c, argv[1]);
  printf("recurseParse returned %d\n", r);
  return EXIT_SUCCESS;
}
