/* osval.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <signal.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "../posix-error/posix-name-val.h"

static name_val_t values [] = {
  {"abrt", SIGABRT},
  {"alrm", SIGALRM},
  {"bus",  SIGBUS},
  {"chld", SIGCHLD},
  {"cont", SIGCONT},
  {"fpe",  SIGFPE},
  {"hup",  SIGHUP},
  {"ill",  SIGILL},
  {"int",  SIGINT},
  {"kill", SIGKILL},
  {"pipe", SIGPIPE},
  {"quit", SIGQUIT},
  {"segv", SIGSEGV},
  {"stop", SIGSTOP},
  {"term", SIGTERM},
  {"tstp", SIGTSTP},
  {"ttin", SIGTTIN},
  {"ttou", SIGTTOU},
  {"usr1", SIGUSR1},
  {"usr2", SIGUSR2},
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* _ml_P_Signal_osval : string -> word
 *
 * Return the OS-dependent, compile-time constant specified by the string.
 */
ml_val_t _ml_P_Signal_osval (ml_state_t *msp, ml_val_t arg)
{
    name_val_t      *res;
    
    res = _ml_posix_nv_lookup (STR_MLtoC(arg), values, NUMELMS);
    if (res)
	return INT_CtoML(res->val);
    else {
	return RAISE_ERROR(msp, "system constant not defined");
    }

} /* end of _ml_P_Signal_osval */
