/* gen-win32-signals.c
 *
 * COPYRIGHT (c) 1996 by Bell Laboratories, Lucent Technologies
 *
 * Generate the "system-signals.h" file for Win32 systems.
 * signals aren't currently implemented (since Win32 doesn't have signals)
 */

#include <signal.h>
#include <stdio.h>
#include "gen.h"
#include "win32-sigtab.h"

#ifndef DST_FILE
#define DST_FILE "system-signals.h"
#endif

main ()
{
    FILE	    *f;
    int             numSigs = 1;
    int             i;

    f = OpenFile (DST_FILE, "_SYSTEM_SIGNALS_");

    fprintf (f, "#define NUM_SYSTEM_SIGS %2d\n", 0);
    fprintf (f, "#define MAX_SYSTEM_SIG  %2d /* %s */\n",
	     0, "none");
    fprintf (f, "#define NUM_SIGS        %2d\n", NUM_SIGS);
    fprintf (f, "#define MAX_SIG         %2d\n", NUM_SIGS);
    fprintf (f, "\n");

    /* the signals */
    for (i = 0; i < NUM_SIGS; i++) {
      fprintf(f, "#define %s %2d\n",  win32SigTab[i].lname, win32SigTab[i].n);
    }

    fprintf (f, "#define IS_SYSTEM_SIG(S) (0)\n");

    CloseFile (f, "_SYSTEM_SIGNALS_");

    exit (0);

}

/* end of gen-win32-signals.c */
