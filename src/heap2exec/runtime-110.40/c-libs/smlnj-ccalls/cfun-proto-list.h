/* cfun-proto-list.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

#ifndef _CFUN_PROTO_LIST_
#define _CFUN_PROTO_LIST_

#ifndef _C_LIBRARY_
#  include "c-library.h"
#endif


#define C_CALLS_CFUNC_PROTO(NAME, FUNC, CTYPE, CARGS)	\
	extern CTYPE FUNC CARGS; 

/* the external definitions for the C functions */
#define C_CALLS_CFUNC(NAME, FUNC, CTYPE, CARGS)    \
           C_CALLS_CFUNC_PROTO(NAME,FUNC,CTYPE,CARGS)
#define CFUNC(NAME, FUNC, MLTYPE)	CFUNC_PROTO(NAME, FUNC, MLTYPE)
#include "cfun-list.h"
#undef CFUNC
#undef C_CALLS_CFUNC

#endif /* !_CFUN_PROTO_LIST_ */
