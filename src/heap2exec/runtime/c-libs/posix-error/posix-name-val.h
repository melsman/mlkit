/* posix-name-val.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Header file for handling string-to-int lookup.
 */

#ifndef _ML_POSIX_NV_
#define _ML_POSIX_NV_

typedef struct {
  char*      name;
  int        val;
} name_val_t;

extern name_val_t *_ml_posix_nv_lookup (char *, name_val_t *, int);

#endif /* !_ML_POSIX_NV_ */
