/* tcgetattr.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include INCLUDE_TIME_H
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_TTY_tcgetattr : int -> (word * word * word * word * string * word * word)
 *
 * Get parameters associated with tty.
 *
 * NOTE: the calls to cfget[io]speed by making the code more OS-dependent
 * and using the structure of struct termios.
 */
ml_val_t _ml_P_TTY_tcgetattr (ml_state_t *msp, ml_val_t arg)
{
    int             sts, fd = INT_MLtoC(arg);
    ml_val_t        iflag, oflag, cflag, lflag;
    ml_val_t        cc, ispeed, ospeed, obj;
    struct termios  data;

    sts = tcgetattr(fd, &data);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);

    WORD_ALLOC (msp, iflag, data.c_iflag);
    WORD_ALLOC (msp, oflag, data.c_oflag);
    WORD_ALLOC (msp, cflag, data.c_cflag);
    WORD_ALLOC (msp, lflag, data.c_lflag);
    WORD_ALLOC (msp, ispeed, cfgetispeed (&data));
    WORD_ALLOC (msp, ospeed, cfgetospeed (&data));
    
  /* allocate the vector; note that this might cause a GC */
    cc = ML_AllocString (msp, NCCS);
    memcpy (PTR_MLtoC(void, cc), data.c_cc, NCCS);

    ML_AllocWrite (msp, 0, MAKE_DESC(DTAG_record, 7));
    ML_AllocWrite (msp, 1, iflag);
    ML_AllocWrite (msp, 2, oflag);
    ML_AllocWrite (msp, 3, cflag);
    ML_AllocWrite (msp, 4, lflag);
    ML_AllocWrite (msp, 5, cc);
    ML_AllocWrite (msp, 6, ispeed);
    ML_AllocWrite (msp, 7, ospeed);
    obj = ML_Alloc(msp, 7);

    return obj;

} /* end of _ml_P_TTY_tcgetattr */
