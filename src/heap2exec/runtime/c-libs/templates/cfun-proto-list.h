/* cfun-proto-list.h
 *
 * COPYRIGHT (c) 1194 AT&T Bell Laboratories.
 */

#ifndef _CFUN_PROTO_LIST_
#define _CFUN_PROTO_LIST_

#ifndef _C_LIBRARY_
#  include "c-library.h"
#endif

/* the external definitions for the C functions */
#define CFUNC(NAME, FUNC, MLTYPE)	CFUNC_PROTO(NAME, FUNC, MLTYPE)
#include "cfun-list.h"
#undef CFUNC

#endif /* !_CFUN_PROTO_LIST_ */
