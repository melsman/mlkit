/* gen-common.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Common code for generating header files.
 */

#include <stdio.h>
#include "gen.h"

/* OpenFile:
 *
 * Open a generated file, and generate its header comment.
 */
FILE *OpenFile (char *fname, char *flag)
{
    FILE	*f;

    if ((f = fopen(fname, "w")) == NULL) {
	fprintf (stderr, "unable to open file \"%s\"\n", fname);
	exit (1);
    }

    fprintf (f, "/* %s\n", fname);
    fprintf (f, " *\n");
    fprintf (f, " * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.\n");
    fprintf (f, " *\n");
    fprintf (f, " * NOTE: this file is generated --- do not edit!!!\n");
    fprintf (f, " */\n");
    fprintf (f, "\n");
    if (flag != (char *)0) {
	fprintf (f, "#ifndef %s\n", flag);
	fprintf (f, "#define %s\n", flag);
	fprintf (f, "\n");
    }

    return f;

} /* end of OpenFile */


/* CloseFile:
 *
 * Generate the file trailer, and close the generated file.
 */
void CloseFile (FILE *f, char *flag)
{
    if (flag != (char *)0) {
	fprintf (f, "\n");
	fprintf (f, "#endif /* !%s */\n", flag);
    }

    fclose (f);

} /* CloseFile */
