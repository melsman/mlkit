/* osval.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "../posix-error/posix-name-val.h"

PVT name_val_t values [] = {
  {"B0", B0},
  {"B110", B110},
  {"B1200", B1200},
  {"B134", B134},
  {"B150", B150},
  {"B1800", B1800},
  {"B19200", B19200},
  {"B200", B200},
  {"B2400", B2400},
  {"B300", B300},
  {"B38400", B38400},
  {"B4800", B4800},
  {"B50", B50},
  {"B600", B600},
  {"B75", B75},
  {"B9600", B9600},
  {"BRKINT", BRKINT},
  {"CLOCAL", CLOCAL},
  {"CREAD", CREAD},
  {"CS5", CS5},
  {"CS6", CS6},
  {"CS7", CS7},
  {"CS8", CS8},
  {"CSIZE", CSIZE},
  {"CSTOPB", CSTOPB},
  {"ECHO", ECHO},
  {"ECHOE", ECHOE},
  {"ECHOK", ECHOK},
  {"ECHONL", ECHONL},
  {"EOF", VEOF},
  {"EOL", VEOL},
  {"ERASE", VERASE},
  {"HUPCL", HUPCL},
  {"ICANON", ICANON},
  {"ICRNL", ICRNL},
  {"IEXTEN", IEXTEN},
  {"IGNBRK", IGNBRK},
  {"IGNCR", IGNCR},
  {"IGNPAR", IGNPAR},
  {"INLCR", INLCR},
  {"INPCK", INPCK},
  {"INTR", VINTR},
  {"ISIG", ISIG},
  {"ISTRIP", ISTRIP},
  {"IXOFF", IXOFF},
  {"IXON", IXON},
  {"KILL", VKILL},
  {"MIN", VMIN},
  {"NCCS", NCCS},
  {"NOFLSH", NOFLSH},
  {"OPOST", OPOST},
  {"PARENB", PARENB}, 
  {"PARMRK", PARMRK},
  {"PARODD", PARODD},
  {"QUIT", VQUIT},
  {"START", VSTART},
  {"STOP", VSTOP},
  {"SUSP", VSUSP},
  {"TCIFLUSH", TCIFLUSH},
  {"TCIOFF", TCIOFF},
  {"TCIOFLUSH", TCIOFLUSH},
  {"TCION", TCION},
  {"TCOFLUSH", TCOFLUSH},
  {"TCOOFF", TCOOFF},
  {"TCOON", TCOON},
  {"TCSADRAIN", TCSADRAIN},
  {"TCSAFLUSH", TCSAFLUSH},
  {"TCSANOW", TCSANOW},
  {"TIME", VTIME},
  {"TOSTOP", TOSTOP},
};

#define NUMELMS ((sizeof values)/(sizeof (name_val_t)))

/* _ml_P_TTY_osval : string -> word
 *
 * Return the OS-dependent, compile-time constant specified by the string.
 */
ml_val_t _ml_P_TTY_osval (ml_state_t *msp, ml_val_t arg)
{
    name_val_t      *res;
    
    res = _ml_posix_nv_lookup (STR_MLtoC(arg), values, NUMELMS);
    if (res)
	return INT_CtoML(res->val);
    else {
	return RAISE_ERROR(msp, "system constant not defined");
    }

} /* end of _ml_P_TTY_osval */
