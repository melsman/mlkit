/* ml-options.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Command-line argument processing utilities.
 */

#include <ctype.h>
#include "ml-base.h"
#include "ml-options.h"

/* isRuntimeOption:
 *
 * Check a command line argument to see if it is a possible runtime
 * system argument (i.e., has the form "@SMLxxx" or "@SMLxxx=yyy").
 * If the command-line argument is a runtime-system argument, then
 * return TRUE, and copy the "xxx" part into option, and set arg to
 * point to the start of the "yyy" part.
 */
bool_t isRuntimeOption (char *cmdLineArg, char *option, char **arg)
{
    char	*cp = cmdLineArg, c;

    if ((*cp++ == '@') && (*cp++ == 'S') && (*cp++ == 'M') && (*cp++ == 'L')) {
	while (((c = *cp++) != '\0') && (c != '='))
	  *option++ = c;
	*option = '\0';
	*arg = cp;
	return TRUE;
    }
    else
	return FALSE;

} /* end of isRuntimeOption */


/* GetSzOption:
 * Get a size specification (accepting K and M suffixes).
 */
int GetSzOption (int scale, char *sz)
{
    char	*p;

  /* find first non-digit in the string */
    for (p = sz;  isdigit(*p); p++)
	continue;

    if (p == sz)
	return -1;
    else {
	switch (*p) {
	  case '\0':
	    break;
	  case 'k':
	  case 'K':
	    scale = ONE_K;
	    break;
	  case 'm':
	  case 'M':
	    scale = ONE_MEG;
	    break;
	  default:
	    return -1;
	} /* end of switch */
	return (scale * atoi(sz));
    }

} /* end of GetSzOption */

