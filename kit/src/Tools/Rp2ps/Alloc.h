#ifndef ALLOC
#define ALLOC

#include "Types.h"

void *xrealloc(void *p, int n);
void* xmalloc(int n);
char* MallocString(char *s);
SAMPLEPTR MallocSample(void);
void FreeSample(SAMPLEPTR s);
ENTRYPTR MallocEntry(void);

#endif /* ALLOC */
