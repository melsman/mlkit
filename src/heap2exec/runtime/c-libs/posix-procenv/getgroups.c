/* getgroups.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* some OSs use int[] as the second argument to getgroups(), when gid_t
 * is not int.
 */
#ifdef INT_GIDLIST
typedef int  gid;
#else
typedef gid_t gid;
#endif


/* mkList:
 *
 * Convert array of gid_t into a list of gid_t
 */
PVT ml_val_t mkList (ml_state_t *msp, int ngrps, gid gidset[])
{
    ml_val_t    p, w;

/** NOTE: we should do something about possible GC!!! **/

    p = LIST_nil;
    while (ngrps-- > 0) {
        WORD_ALLOC (msp, w, (Word_t)(gidset[ngrps]));
	LIST_cons(msp, p, w, p);
    }

    return p;
}

/* _ml_P_ProcEnv_getgroups: unit -> word list
 *
 * Return supplementary group access list ids.
 */
ml_val_t _ml_P_ProcEnv_getgroups (ml_state_t *msp, ml_val_t arg)
{
    gid		gidset[NGROUPS_MAX];
    int		ngrps;
    ml_val_t	p;

    ngrps = getgroups (NGROUPS_MAX, gidset);

    if (ngrps == -1) {
	gid      *gp;

      /* If the error was not due to too small buffer size,
       * raise exception.
       */
	if (errno != EINVAL)
	    return RAISE_SYSERR(msp, -1);

      /* Find out how many groups there are and allocate enough space. */
	ngrps = getgroups (0, gidset);
	gp = (gid *)MALLOC(ngrps * (sizeof (gid)));
	if (gp == 0) {
	    errno = ENOMEM;
	    return RAISE_SYSERR(msp, -1);
	}

	ngrps = getgroups (ngrps, gp);

	if (ngrps == -1)
	    p = RAISE_SYSERR(msp, -1);
	else
	    p = mkList (msp, ngrps, gp);
        
	FREE ((void *)gp);
    }
    else
	p = mkList (msp, ngrps, gidset);
    
    return p;

} /* end of _ml_P_ProcEnv_getgroups */

