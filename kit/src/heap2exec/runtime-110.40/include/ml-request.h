/* ml-request.h
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories
 *
 * These are the service request codes used C/ML interface.
 */

#ifndef _ML_REQUEST_
#define _ML_REQUEST_

#define REQ_GC			0
#define REQ_RETURN		1
#define REQ_EXN			2
#define REQ_FAULT		3
#define REQ_BIND_CFUN		4
#define REQ_CALLC		5
#define REQ_ALLOC_STRING	6
#define REQ_ALLOC_BYTEARRAY	7
#define REQ_ALLOC_REALDARRAY	8
#define REQ_ALLOC_ARRAY		9
#define REQ_ALLOC_VECTOR	10
#define REQ_SIG_RETURN		11
#define REQ_SIG_RESUME		12
#define REQ_POLL_RETURN         13
#define REQ_POLL_RESUME         14
#define REQ_BUILD_LITERALS	15

#endif /* !_ML_REQUEST_ */

