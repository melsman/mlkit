/* osval.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include <fcntl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "../posix-error/posix-name-val.h"

static name_val_t values [] = {
  {"F_GETLK",  F_GETLK},
  {"F_RDLCK",  F_RDLCK},
  {"F_SETLK",  F_SETLK},
  {"F_SETLKW", F_SETLKW},
  {"F_UNLCK",  F_UNLCK},
  {"F_WRLCK",  F_WRLCK},
  {"SEEK_CUR", SEEK_CUR},
  {"SEEK_END", SEEK_END},
  {"SEEK_SET", SEEK_SET},
  {"append",   O_APPEND},
  {"cloexec",  FD_CLOEXEC},
#ifdef O_DSYNC
  {"dsync",    O_DSYNC},
#else
  {"dsync",    0},
#endif
  {"nonblock", O_NONBLOCK},
#ifdef O_RSYNC
  {"rsync",    O_RSYNC},
#else
  {"rsync",    0},
#endif
#ifdef O_SYNC
  {"sync",     O_SYNC},
#else
  {"sync",     0},
#endif
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* _ml_P_IO_osval : string -> int
 *
 * Return the OS-dependent, compile-time constant specified by the string.
 */
ml_val_t _ml_P_IO_osval (ml_state_t *msp, ml_val_t arg)
{
    name_val_t      *res;
    
    res = _ml_posix_nv_lookup (STR_MLtoC(arg), values, NUMELMS);
    if (res)
	return INT_CtoML(res->val);
    else {
	return RAISE_ERROR(msp, "system constant not defined");
    }

} /* end of _ml_P_IO_osval */
