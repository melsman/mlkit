/* error.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Run-time system error messages.
 */

#include <stdio.h>
#include <stdarg.h>
#include "ml-base.h"

extern FILE	*DebugF;

#ifdef TARGET_BYTECODE
extern FILE	*BC_stdout;
#endif


/* Say:
 * Print a message to the standard output.
 */
void Say (char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    vfprintf (stdout, fmt, ap);
    va_end(ap);
    fflush (stdout);

} /* end of Say */

/* SayDebug:
 * Print a message to the debug output stream.
 */
void SayDebug (char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    vfprintf (DebugF, fmt, ap);
    va_end(ap);
    fflush (DebugF);

} /* end of SayDebug */

/* Error:
 * Print an error message.
 */
void Error (char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    fprintf (stderr, "%s: Error -- ", MLCmdName);
    vfprintf (stderr, fmt, ap);
    va_end(ap);

} /* end of Error */


/* Die:
 * Print an error message and then exit.
 */
void Die (char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    fprintf (stderr, "%s: Fatal error -- ", MLCmdName);
    vfprintf (stderr, fmt, ap);
    fprintf (stderr, "\n");
    va_end(ap);

#if (defined(TARGET_BYTECODE) && defined(INSTR_HISTORY))
    {
	extern void PrintRegs (FILE *);
	extern void PrintInstrHistory (FILE *);

	PrintRegs (BC_stdout);
	PrintInstrHistory (BC_stdout);
    }
#endif

#ifdef MP_SUPPORT
    MP_Shutdown ();
#endif

    Exit (1);

} /* end of Die */


#ifdef ASSERT_ON
/* AssertFail:
 *
 * Print an assertion failure message.
 */
void AssertFail (const char *a, const char *file, int line)
{
    fprintf (stderr, "%s: Assertion failure (%s) at \"%s:%d\"\n",
	MLCmdName, a, file, line);

#ifdef MP_SUPPORT
    MP_Shutdown ();
#endif

    Exit (2);

} /* end of AssertFail */
#endif

