/* gen-unix-sigtbl.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Generate the "unix-sigtbl.c" file.
 */

#include <signal.h>
#include <stdio.h>
#include "gen.h"
#include "gen-unix-signals.h"

#ifndef DST_FILE
#define DST_FILE "unix-sigtbl.c"
#endif

int main (void)
{
    sig_info_t	    *sigInfo;
    int		    i;
    int		    numSigs;
    FILE	    *f;

    sigInfo = SortSignalTbl ();
    numSigs = sigInfo->numSysSigs+sigInfo->numRunSigs;

    f = OpenFile (DST_FILE, NIL(char *));

    fprintf (f, "\n");
    fprintf (f, "PVT sys_const_t SigInfo[NUM_SIGS] = {\n");
    for (i = 0;  i < sigInfo->numSysSigs;  i++) {
	fprintf(f, "    { %s, \"%s\" },\n",
	    sigInfo->sigs[i]->sigName, sigInfo->sigs[i]->shortName);
    }
    fprintf (f, "  /* Run-time signals */\n");
    for (i = sigInfo->numSysSigs;  i < numSigs;  i++) {
	fprintf(f, "    { %s, \"%s\" },\n",
	    sigInfo->sigs[i]->sigName, sigInfo->sigs[i]->shortName);
    }
    fprintf (f, "};\n");
    fprintf (f, "PVT sysconst_tbl_t SigTbl = {\n");
    fprintf (f, "    /* numConsts */ NUM_SIGS,\n");
    fprintf (f, "    /* consts */    SigInfo\n");
    fprintf (f, "};\n");

    CloseFile (f, NIL(char *));

    exit (0);

} /* end of main */

