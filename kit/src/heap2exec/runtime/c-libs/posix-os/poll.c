/* poll.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * The run-time code for OS.IO.poll.  Note that this implementation should
 * satisfy the following two properties:
 *
 *   1) the list of return items should be in the same order as the
 *	corresponding list of arguments.
 *
 *   2) return items should contain no more information than was queried for
 *	(this matters when the same descriptor is covered by multiple items).
 */

#include "ml-unixdep.h"
#if defined(HAS_SELECT)
#  include INCLUDE_TYPES_H
#  include <sys/time.h>
#elif defined(HAS_POLL)
#  include <stropts.h>
#  include <poll.h>
#else
#  error no support for I/O polling
#endif
#include INCLUDE_TIME_H
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* bit masks for polling descriptors (see src/sml-nj/boot/Unix/os-io.sml) */
#define RD_BIT		0x1
#define WR_BIT		0x2
#define ERR_BIT		0x4

PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout);


/* _ml_OS_poll : ((int * word) list * (Int32.int * int) option) -> (int * word) list
 */
ml_val_t _ml_OS_poll (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	    pollList = REC_SEL(arg, 0);
    ml_val_t	    timeout  = REC_SEL(arg, 1);
    struct timeval  tv, *tvp;

    if (timeout == OPTION_NONE)
	tvp = NIL(struct timeval *);
    else {
	timeout		= OPTION_get(timeout);
	tv.tv_sec	= REC_SELINT32(timeout, 0);
	tv.tv_usec	= REC_SELINT(timeout, 1);
	tvp = &tv;
    }

    return ML_Poll (msp, pollList, tvp);

} /* end of _ml_OS_poll */


#ifdef HAS_POLL

#ifdef POLLMSG
#define POLL_ERROR	(POLLRDBAND|POLLPRI|POLLHUP|POLLMSG)
#else
#define POLL_ERROR	(POLLRDBAND|POLLPRI|POLLHUP)
#endif

/* ML_Poll:
 *
 * The version of the polling operation for systems that provide SVR4 polling.
 */
PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout)
{
    int		    tout, sts;
    struct pollfd   *fds, *fdp;
    int		    nfds, i, flag;
    ml_val_t	    l, item;

    if (timeout == NIL(struct timeval *))
	tout = -1;
    else
      /* convert to miliseconds */
	tout = (timeout->tv_sec * 1000) + (timeout->tv_usec / 1000);

  /* count the number of polling items */
    for (l = pollList, nfds = 0;  l != LIST_nil;  l = LIST_tl(l))
	nfds++;

  /* allocate the fds vector */
    fds = NEW_VEC(struct pollfd, nfds);
    CLEAR_MEM (fds, sizeof(struct pollfd)*nfds);

  /* initialize the polling descriptors */
    for (l = pollList, fdp = fds;  l != LIST_nil;  l = LIST_tl(l), fdp++) {
	item = LIST_hd(l);
	fdp->fd	= REC_SELINT(item, 0);
	flag = REC_SELINT(item, 1);
	if ((flag & RD_BIT) != 0)
	    fdp->events |= POLLIN;
	if ((flag & WR_BIT) != 0)
	    fdp->events |= POLLOUT;
	if ((flag & ERR_BIT) != 0)
	    fdp->events |= POLL_ERROR;
    }

    sts = poll (fds, nfds, tout);

    if (sts < 0) {
	FREE(fds);
	return RAISE_SYSERR(msp, sts);
    }
    else {
	for (i = nfds-1, l = LIST_nil;  i >= 0;  i--) {
	    fdp = &(fds[i]);
	    if (fdp->revents != 0) {
		flag = 0;
		if ((fdp->revents & POLLIN) != 0)
		    flag |= RD_BIT;
		if ((fdp->revents & POLLOUT) != 0)
		    flag |= WR_BIT;
		if ((fdp->revents & POLL_ERROR) != 0)
		    flag |= ERR_BIT;
		REC_ALLOC2(msp, item, INT_CtoML(fdp->fd), INT_CtoML(flag));
		LIST_cons(msp, l, item, l);
	    }
	}
	FREE(fds);
	return l;
    }

} /* end of ML_Poll */

#else /* HAS_SELECT */

/* ML_Poll:
 *
 * The version of the polling operation for systems that provide BSD select.
 */
PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout)
{
    fd_set	rset, wset, eset;
    fd_set	*rfds, *wfds, *efds;
    int		maxFD, sts, fd, flag;
    ml_val_t	l, item;

    rfds = wfds = efds = NIL(fd_set *);
    maxFD = 0;
    for (l = pollList;  l != LIST_nil;  l = LIST_tl(l)) {
	item	= LIST_hd(l);
	fd	= REC_SELINT(item, 0);
	flag	= REC_SELINT(item, 1);
	if ((flag & RD_BIT) != 0) {
	    if (rfds == NIL(fd_set *)) {
		rfds = &rset;
		FD_ZERO(rfds);
	    }
	    FD_SET (fd, rfds);
	}
	if ((flag & WR_BIT) != 0) {
	    if (wfds == NIL(fd_set *)) {
		wfds = &wset;
		FD_ZERO(wfds);
	    }
	    FD_SET (fd, wfds);
	}
	if ((flag & ERR_BIT) != 0) {
	    if (efds == NIL(fd_set *)) {
		efds = &eset;
		FD_ZERO(efds);
	    }
	    FD_SET (fd, efds);
	}
	if (fd > maxFD) maxFD = fd;
    }

    sts = select (maxFD+1, rfds, wfds, efds, timeout);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else if (sts == 0)
	return LIST_nil;
    else {
	ml_val_t	*resVec = NEW_VEC(ml_val_t, sts);
	int		i, resFlag;

	for (i = 0, l = pollList;  l != LIST_nil;  l = LIST_tl(l)) {
	    item	= LIST_hd(l);
	    fd		= REC_SELINT(item, 0);
	    flag	= REC_SELINT(item, 1);
	    resFlag	= 0;
	    if (((flag & RD_BIT) != 0) && FD_ISSET(fd, rfds))
		resFlag |= RD_BIT;
	    if (((flag & WR_BIT) != 0) && FD_ISSET(fd, wfds))
		resFlag |= WR_BIT;
	    if (((flag & ERR_BIT) != 0) && FD_ISSET(fd, efds))
		resFlag |= ERR_BIT;
	    if (resFlag != 0) {
		REC_ALLOC2 (msp, item, INT_CtoML(fd), INT_CtoML(resFlag));
		resVec[i++] = item;
	    }
	}

	ASSERT(i == sts);

	for (i = sts-1, l = LIST_nil;  i >= 0;  i--) {
	    item = resVec[i];
	    LIST_cons (msp, l, item, l);
	}

	FREE(resVec);

	return l;
    }

} /* end of ML_Poll */

#endif

