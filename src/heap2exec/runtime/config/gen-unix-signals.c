/* gen-unix-signals.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Generate the "system-signals.h" file for UNIX systems.
 */

#include <signal.h>
#include <stdio.h>
#include "gen.h"
#include "gen-unix-signals.h"

#ifndef DST_FILE
#define DST_FILE "system-signals.h"
#endif


main ()
{
    sig_info_t	    *sigInfo;
    int		    i;
    int		    numSigs;
    FILE	    *f;

    sigInfo = SortSignalTbl ();
    numSigs = sigInfo->numSysSigs+sigInfo->numRunSigs;

    f = OpenFile (DST_FILE, "_SYSTEM_SIGNALS_");

    fprintf (f, "#define NUM_SYSTEM_SIGS %2d\n", sigInfo->numSysSigs);
    fprintf (f, "#define MAX_SYSTEM_SIG  %2d /* %s */\n",
	sigInfo->maxSysSig, sigInfo->sigs[sigInfo->numSysSigs-1]->sigName);
    fprintf (f, "#define NUM_SIGS        %2d\n", numSigs);
    fprintf (f, "#define MAX_SIG         %2d /* %s */\n",
	sigInfo->sigs[numSigs-1]->sig, sigInfo->sigs[numSigs-1]->sigName);
    fprintf (f, "\n");
    for (i = sigInfo->numSysSigs;  i < numSigs;  i++) {
	fprintf(f, "#define %s %2d\n",
	    sigInfo->sigs[i]->sigName, sigInfo->sigs[i]->sig);
    }
    fprintf (f, "\n");
    fprintf (f, "#define IS_SYSTEM_SIG(S) ((S) <= MAX_SYSTEM_SIG)\n");

    CloseFile (f, "_SYSTEM_SIGNALS_");

    exit (0);

} /* end of main */

