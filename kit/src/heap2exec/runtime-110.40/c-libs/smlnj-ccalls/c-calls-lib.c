/* c-calls-lib.c
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "c-library.h"
#include "cfun-proto-list.h"


/* the table of C functions and ML names */
#define C_CALLS_CFUNC(NAME, FUNC, CTYPE, CARGS)	\
            CFUNC_BIND(NAME, (cfunc_t) FUNC, "")
#define CFUNC(NAME, FUNC, MLTYPE)	CFUNC_BIND(NAME, FUNC, MLTYPE)
PVT cfunc_binding_t CFunTable[] = {
#include "cfun-list.h"
	CFUNC_NULL_BIND
    };
#undef CFUNC


/* the c-calls library */
c_library_t	    SMLNJ_CCalls_Library = {
	CLIB_NAME,
	CLIB_VERSION,
	CLIB_DATE,
	NIL(clib_init_fn_t),
	CFunTable
    };

