#ifndef _TYPES_H
#define _TYPES_H

typedef struct sample {
    struct sample* next;
    int n;
    float t;
    int   born;
    float nbytes;
} SAMPLENODE, *SAMPLEPTR;

typedef struct entry {
    struct entry* next;
    char*  name;
    SAMPLEPTR samples;
} ENTRYNODE, *ENTRYPTR;

#endif /* _TYPES_H */
