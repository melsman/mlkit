/* osval.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "../posix-error/posix-name-val.h"

/* NOTE: the following table must be in alphabetical order!!! */
PVT name_val_t values [] = {
  {"A_EXEC",	   X_OK},
  {"A_FILE",       F_OK},
  {"A_READ",       R_OK},
  {"A_WRITE",      W_OK},
  {"O_APPEND",     O_APPEND},
  {"O_CREAT",      O_CREAT},
#ifdef O_DSYNC
  {"O_DSYNC",      O_DSYNC},
#else
  {"O_DSYNC",      0},
#endif
  {"O_EXCL",       O_EXCL},
  {"O_NOCTTY",     O_NOCTTY},
  {"O_NONBLOCK",   O_NONBLOCK},
  {"O_RDONLY",     O_RDONLY},
  {"O_RDWR",       O_RDWR},
#ifdef O_RSYNC
  {"O_RSYNC",      O_RSYNC},
#else
  {"O_RSYNC",      0},
#endif
#ifdef O_SYNC
  {"O_SYNC",       O_SYNC},
#else
  {"O_SYNC",       0},
#endif
  {"O_TRUNC",      O_TRUNC},
  {"O_WRONLY",     O_WRONLY},
  {"irgrp",        S_IRGRP},
  {"iroth",        S_IROTH},
  {"irusr",        S_IRUSR},
  {"irwxg",        S_IRWXG},
  {"irwxo",        S_IRWXO},
  {"irwxu",        S_IRWXU},
  {"isgid",        S_ISGID},
  {"isuid",        S_ISUID},
  {"iwgrp",        S_IWGRP},
  {"iwoth",        S_IWOTH},
  {"iwusr",        S_IWUSR},
  {"ixgrp",        S_IXGRP},
  {"ixoth",        S_IXOTH},
  {"ixusr",        S_IXUSR},
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* _ml_P_FileSys_osval : string -> int
 *
 * Return the OS-dependent, compile-time constant specified by the string.
 */
ml_val_t _ml_P_FileSys_osval (ml_state_t *msp, ml_val_t arg)
{
    name_val_t      *res;
    
    res = _ml_posix_nv_lookup (STR_MLtoC(arg), values, NUMELMS);
    if (res)
	return INT_CtoML(res->val);
    else {
	return RAISE_ERROR(msp, "system constant not defined");
    }

} /* end of _ml_P_FileSys_osval */
