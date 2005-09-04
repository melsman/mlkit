#include "stdlib.h"
#include "stdio.h"
#include "binaryheap.h"

/* Try running this program with a sequence of numbers on the command line */

struct elem_t
{
  int pos;
  long key;
};

int order (struct elem_t *a, struct elem_t *b) 
{
  if (a->key == b->key) return 0;
  if (a->key < b->key) return -1;
  return 1;
}

void newpos (struct elem_t *a, unsigned long pos)
{
  return;
}

void setkey (struct elem_t *a, long newkey)
{
  a->key = newkey;
  return;
}

DECLARE_BINARYHEAP(test,struct elem_t,long)

DEFINE_BINARYHEAP(test,order,newpos,setkey)

int main(int argc, char **argv)
{
  int i, n;
  test_binaryheap_t heap;
  struct elem_t tmp;
  test_heapinit(&heap);
  for (i=1;i<argc;i++) 
  {
    sscanf(argv[i], "%d", &n);
    tmp.pos = i;
    test_heapinsert(&heap, tmp, n);
  }
  while (test_heapextractmin(&heap, &tmp) != heap_UNDERFLOW)
  {
    printf ("%d ", tmp.pos);
  }
  printf("\n");
  return EXIT_SUCCESS;
}
