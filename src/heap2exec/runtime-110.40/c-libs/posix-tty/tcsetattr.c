/* tcsetattr.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_TTY_tcsetattr : int * int * termio_rep -> unit
 *    termio_rep = (word * word * word * word * string * word * word)
 *
 * Set parameters associated with tty.
 *
 * NOTE: the calls to cfset[io]speed by making the code more OS-dependent
 * and using the structure of struct termios.
 */
ml_val_t _ml_P_TTY_tcsetattr (ml_state_t *msp, ml_val_t arg)
{
    int              sts, fd = REC_SELINT(arg, 0);
    int              action = REC_SELINT(arg, 1);
    ml_val_t         termio_rep = REC_SEL(arg, 2);
    struct termios   data;

    data.c_iflag = REC_SELWORD(termio_rep, 0);
    data.c_oflag = REC_SELWORD(termio_rep, 1);
    data.c_cflag = REC_SELWORD(termio_rep, 2);
    data.c_lflag = REC_SELWORD(termio_rep, 3);
    memcpy (data.c_cc, REC_SELPTR(void, termio_rep, 4), NCCS);
    sts = cfsetispeed (&data, REC_SELWORD(termio_rep, 5));
    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    sts = cfsetospeed (&data, REC_SELWORD(termio_rep, 6));
    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    sts = tcsetattr(fd, action, &data);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_TTY_tcsetattr */
