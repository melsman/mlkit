/* gen-bc-instr-def.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * A program to generate #defines for the bytecode instruction opcodes.
 */

#include <stdio.h>
#include "gen.h"
#include "bc.h"
#include "print-bc.h"

main ()
{
    int		i;
    FILE	*f;

    f = OpenFile ("bc-instr-def.h", "_BC_INSTR_DEF_");

    for (i = 0;  i < NUM_BC_INSTRS;  i++)
	fprintf(f, "#define %-23s %d\n", instrTbl[i].instrName, i);

    CloseFile (f, "_BC_INSTR_DEF_");

    exit (0);
}

