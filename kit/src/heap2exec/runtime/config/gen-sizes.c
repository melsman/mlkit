/* gen-sizes.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This program generates the "ml-sizes.h" header file; this file is
 * usable in both C and assembly files.
 */

#include <stdio.h>
#include "gen.h"

#define NIL(ty)	((ty)0)

#if defined(SIZES_C64_ML64)
#  error "64 bits not supported yet"
#else
#  define WORD_SZB	4
#endif
#  define ADDR_SZB	sizeof(char *)

static union {
    char	    bytes[sizeof(unsigned long)];
    unsigned long   l;
} U;

int log2 (int x)
{
    int		i, j;

    for (i = 1, j = 2;  j <= x;  i++, j += j)
	continue;

    return i-1;

} /* end of log2 */

main ()
{
    char	*i16, *i32, *i64;
    FILE	*f;

    i16 = i32 = i64 = NIL(char *);

    switch (sizeof(short)) {
      case 2: i16 = "short"; break;
      case 4: i32 = "short"; break;
      case 8: i64 = "short"; break;
    }

    switch (sizeof(int)) {
      case 2: i16 = "int"; break;
      case 4: i32 = "int"; break;
      case 8: i64 = "int"; break;
    }

    switch (sizeof(long)) {
      case 2: i16 = "long"; break;
      case 4: i32 = "long"; break;
      case 8: i64 = "long"; break;
      default:
	fprintf(stderr, "gen-sizes: Error -- no 32-bit integer type\n");
	exit (1);
    }

    if (i16 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 16-bit integer type\n");
	exit (1);
    }
    if (i32 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 32-bit integer type\n");
	exit (1);
    }
#if (defined(SIZES_C64_ML32) || defined(SIZES_C64_ML64))
    if (i64 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 64-bit integer type\n");
	exit (1);
    }
#endif

    f = OpenFile ("ml-sizes.h", "_ML_SIZES_");

    fprintf (f, "#define WORD_SZB           %d\n", WORD_SZB);
    fprintf (f, "#define ADDR_SZB           %d\n", ADDR_SZB);
    fprintf (f, "#define REALD_SZB          %d\n", sizeof(double));
    fprintf (f, "#define BITS_PER_WORD      %d\n", 8*WORD_SZB);
    fprintf (f, "#define LOG_BITS_PER_WORD  %d\n", log2(8*WORD_SZB));
    fprintf (f, "#define LOG_BYTES_PER_WORD %d\n", log2(WORD_SZB));
    fprintf (f, "\n");

    U.bytes[0] = 0x01;
    U.bytes[sizeof(unsigned long)-1] = 0x02;
    switch (U.l & 0xFF) {
      case 0x01:
	fprintf(f, "#define BYTE_ORDER_LITTLE\n");
	break;
      case 0x02:
	fprintf(f, "#define BYTE_ORDER_BIG\n");
	break;
      default:
	fprintf(stderr, "gen-sizes: Error -- unable to determine endianess\n");
	exit(1);
    } /* end of switch */
    fprintf (f, "\n");

  /* the C part */
    fprintf (f, "#ifndef _ASM_\n");

    fprintf (f, "typedef %s Int16_t;\n", i16);
    fprintf (f, "typedef unsigned %s Unsigned16_t;\n", i16);
    fprintf (f, "typedef %s Int32_t;\n", i32);
    fprintf (f, "typedef unsigned %s Unsigned32_t;\n", i32);
#if (defined(SIZES_C64_ML32) || defined(SIZES_C64_ML64))
    fprintf (f, "typedef %s Int64_t;\n", i64);
    fprintf (f, "typedef unsigned %s Unsigned64_t;\n", i64);
#endif
    fprintf (f, "\n");
    fprintf (f, "typedef unsigned char Byte_t;\n");
#if defined(SIZES_C64_ML32)
    fprintf (f, "typedef Unsigned32_t Word_t;\n");
    fprintf (f, "typedef Int32_t      Int_t;\n");
    fprintf (f, "typedef Unsigned64_t Addr_t;\n");
#elif defined(SIZES_C64_ML64)
    fprintf (f, "typedef Unsigned64_t Word_t;\n");
    fprintf (f, "typedef Int64_t      Int_t;\n");
    fprintf (f, "typedef Unsigned64_t Addr_t;\n");
#else
    fprintf (f, "typedef Unsigned32_t Word_t;\n");
    fprintf (f, "typedef Int32_t      Int_t;\n");
    fprintf (f, "typedef Unsigned32_t Addr_t;\n");
#endif

    fprintf (f, "#endif\n");

    CloseFile (f, "_ML_SIZES_");

    exit (0);
}
