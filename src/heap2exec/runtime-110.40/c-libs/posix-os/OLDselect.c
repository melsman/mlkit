/* select.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-osdep.h"
#if defined(HAS_SELECT)
#include <sys/types.h>
#include <sys/time.h>
#elif defined(HAS_POLL)
#include <stropts.h>
#include <poll.h>
#endif
#include <signal.h>
#include <setjmp.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "tags.h"
#include "ml-state.h"
#include "ml-signal.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

#ifdef HAS_SELECT
PVT fd_set *ListToFDSet (ml_val_t fdl, fd_set *fds, int *width);
PVT ml_val_t FDSetToList (ml_state_t *msp, fd_set *fds, int width);
#endif


/* _ml_IO_select : (int list * int list * int list * (int * int) option)
 *                 -> (int list * int list * int list)
 *
 * Check file descriptors for the readiness of I/O operations.
 */
ml_val_t _ml_IO_select (ml_state_t *msp, ml_val_t arg)
{
#if ((! defined(HAS_SELECT)) && (! defined(HAS_POLL)))
    return RAISE_ERROR (msp, "SMLNJ-IO.select unsupported");
#else
    ml_val_t	    rl = REC_SEL(arg, 0);
    ml_val_t	    wl = REC_SEL(arg, 1);
    ml_val_t	    el = REC_SEL(arg, 2);
    ml_val_t	    timeout = REC_SEL(arg, 3);
#ifdef HAS_SELECT
    fd_set	    rset, wset, eset;
    fd_set	    *rfds, *wfds, *efds;
    int		    width = 0, sts;
    struct timeval  t, *tp;

    rfds = ListToFDSet (rl, &rset, &width);
    wfds = ListToFDSet (wl, &wset, &width);
    efds = ListToFDSet (el, &eset, &width);

    if (isBOXED(timeout)) {
	timeout = REC_SEL(timeout, 0);  /* strip the SOME */
	t.tv_sec = REC_SELINT(timeout, 0);
	t.tv_usec = REC_SELINT(timeout, 1);
	tp = &t;
    }
    else
	tp = 0;

#else /* HAS_POLL */
    struct pollfd   *fds;
    int		    nr, nw, ne, nfds, i, t, sts;

#define COUNT(cntr, l) {					\
	ml_val_t	__p = (l);				\
	for (cntr = 0;  __p != LIST_nil;  __p = LIST_tl(__p))	\
	    cntr++;						\
    }
#define INSERT(req, l) {					\
	ml_val_t	__p = (l);				\
	while (__p != LIST_nil) {				\
	    fds[i].fd = INT_MLtoC(LIST_hd(__p));		\
	    fds[i].events = (req);				\
	    i++;						\
	    __p = LIST_tl(__p);					\
	}							\
    }

    COUNT(nr, rl);
    COUNT(nw, wl);
    COUNT(ne, el);
    nfds = nr+nw+ne;
    fds = NEW_VEC(struct pollfd, nfds);
    i = 0;
    INSERT(POLLIN, rl);
    INSERT(POLLOUT, wl);
#ifdef POLLMSG
    INSERT(POLLRDBAND|POLLPRI|POLLMSG|POLLHUP, el);
#else
    INSERT(POLLRDBAND|POLLPRI|POLLHUP, el);
#endif

    if (isBOXED(timeout)) {
	long	sec, usec;
	timeout = REC_SEL(timeout, 0);  /* strip the SOME */
	sec = REC_SELINT(timeout, 0);
	usec = REC_SELINT(timeout, 1);
	t = (usec/1000 + sec*1000);
    }
    else
	t = INFTIM;
#endif

    if (msp->ml_inSigHandler || msp->ml_maskSignals
    || ((! SETJMP (msp->ml_syscallEnv)) &&
	(((msp->ml_ioWaitFlag = TRUE), (msp->ml_numPendingSigs == 0)))))
    {
#ifdef HAS_SELECT
	DO_SYSCALL (select (width, rfds, wfds, efds, tp), sts);
#else /* HAS_POLL */
	DO_SYSCALL (poll (fds, nfds, t), sts);
#endif
	msp->ml_ioWaitFlag = FALSE;
    }
    else {
#ifdef HAS_POLL
	FREE (fds);
#endif
	BackupMLCont(msp);
      /* re-enable signals */
	RESET_SIG_MASK();
	return msp->ml_arg;
    }

    if (sts == -1) {
#ifdef HAS_POLL
	FREE (fds);
#endif
	return RAISE_SYSERR (msp, sts);
    }
    else {
	ml_val_t	    rfdl, wfdl, efdl, res;

	if (sts == 0)
	    rfdl = wfdl = efdl = LIST_nil;
	else {
#ifdef HAS_SELECT
	    rfdl = FDSetToList (msp, rfds, width);
	    wfdl = FDSetToList (msp, wfds, width);
	    efdl = FDSetToList (msp, efds, width);
#else /* HAS_POLL */
#define BUILD_RESULT(l,n)	{				\
	l = LIST_nil;						\
	while ((sts > 0) && (n > 0)) {				\
	    if (fds[i].revents != 0) {				\
		sts--;						\
		LIST_cons(msp, l, INT_CtoML(fds[i].fd), l);	\
	    }							\
	    n--;  i++;						\
	}							\
    }
	    i = 0;
	    BUILD_RESULT(rfdl, nr);
	    BUILD_RESULT(wfdl, nw);
	    BUILD_RESULT(efdl, ne);
#endif
	}
	REC_ALLOC3 (msp, res, rfdl, wfdl, efdl);

#ifdef HAS_POLL
	FREE (fds);
#endif

	return res;
    }
#endif
} /* end of _ml_IO_select */


#ifdef HAS_SELECT

/* ListToFDSet:
 *
 * Map a ML list of file descriptors to a fd_set.
 */
PVT fd_set *ListToFDSet (ml_val_t fdl, fd_set *fds, int *width)
{
    register int    fd, maxfd = -1;

    FD_ZERO(fds);
    while (fdl != LIST_nil) {
	fd = INT_MLtoC(LIST_hd(fdl));
	if (fd > maxfd)
	    maxfd = fd;
	FD_SET (fd, fds);
	fdl = LIST_tl(fdl);
    }

    if (maxfd >= 0) {
	if (maxfd >= *width)
	    *width = maxfd+1;
	return fds;
    }
    else
	return (fd_set *)0;

} /* end of ListToFDSet */

/* FDSetToList:
 *
 * Map a fd_set to a ML list of ready file descriptors.
 */
PVT ml_val_t FDSetToList (ml_state_t *msp, fd_set *fds, int width)
{
    register ml_val_t p;
    register int    i;

    if (fds == NIL(fd_set *))
	return LIST_nil;

    for (i = 0, p = LIST_nil;  i < width;  i++) {
	if (FD_ISSET(i, fds))
	    LIST_cons (msp, p, INT_CtoML(i), p);
    }

    return p;

} /* end of FDSetToList */

#endif /* HAS_SELECT */
