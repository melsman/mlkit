/* bin-file.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The layout is:
 *   header
 *   import PerIDs (16 bytes each)
 *   export PerIDs (16 bytes each)
 *   CM dependency information
 *   inlinable lambda expression
 *   reserved area 1 (typically empty)
 *   reserved area 2 (typically empty)
 *   code objects
 *     This section contains a sequence of code objects, each of
 *     which is lead by its size.  The individual sizes must sum up to
 *     codeSzB.
 *   pickled static environment
 */

#ifndef _BIN_FILE_
#define _BIN_FILE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif


/** Persistent IDs **/
#define PERID_LEN	16

typedef struct {	    /* a persistent ID (PerID) */
    Byte_t	bytes[PERID_LEN];
} pers_id_t;


typedef struct {	    /* The header of a .bin file; note that the fields */
			    /* are in big-endian representation. */
    Byte_t	magic[16];	/* magic number */
    Int32_t	importCnt;	/* the number of imported PerIDs. */
    Int32_t	exportCnt;	/* the number of exported PerIDs. */
    Int32_t	cmInfoSzB;	/* the size of the CM dependency information area */
    Int32_t	lambdaSzB;	/* the size of inlinable lambda expressions */
    Int32_t	reserved1;	/* reserved for future use */
    Int32_t	reserved2;	/* reserved for future use */
    Int32_t	codeSzB;	/* the number of bytes of code */
    Int32_t	envSzB;		/* the size of the environment */
} binfile_hdr_t;

/* the value of the run-time system persistent ID */
#define RUNTIME_PERID	"runtimePidxxxxxx"

#endif /* !_BIN_FILE_ */
