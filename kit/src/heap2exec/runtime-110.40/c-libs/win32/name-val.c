/* name-val.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * support for string-to-int lookup.
 */

#include <stdlib.h>
#include <search.h>
#include "name-val.h"

static int cmp (const void *key, const void *item)
{
    return strcmp(((name_val_t*)key)->name, ((name_val_t*)item)->name);
}

/* nv_lookup
 *
 * Given a string key, an array of name/value pairs and the size of the
 * array, find element in the array with matching key and return a pointer
 * to it. If not found, return NULL. We use binary search, so we assume
 * the array is sorted.
 */
name_val_t *nv_lookup (char *key, name_val_t *array, int numelms)
{
    name_val_t   k;

    k.name = key;
    return ((name_val_t *)bsearch(&k,array,numelms,sizeof (name_val_t),cmp));
}

