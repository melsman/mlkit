/* gen-win32-sigtbl.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * generate the "win32-sigtbl.c" file.
 */

#include <signal.h>
#include <stdio.h>
#include "gen.h"
#include "win32-sigtab.h"

#ifndef DST_FILE
#define DST_FILE "win32-sigtbl.c"
#endif

main ()
{
    FILE	    *f;
    int i;

    f = OpenFile (DST_FILE, NIL(char *));

    fprintf (f, "\n");

    fprintf (f, "PVT sys_const_t SigInfo[NUM_SIGS] = {\n");
    for (i = 0; i < NUM_SIGS; i++) {
      fprintf(f, "\t{ %d, \"%s\" },\n", win32SigTab[i].n, win32SigTab[i].sname);
    }
    fprintf (f, "};\n");

    fprintf (f, "PVT sysconst_tbl_t SigTbl = {\n");
    fprintf (f, "    /* numConsts */ NUM_SIGS,\n");
    fprintf (f, "    /* consts */    SigInfo\n");
    fprintf (f, "};\n");

    CloseFile (f, NIL(char *));

    exit (0);

}

/* end of gen-win32-sigtbl.c */

