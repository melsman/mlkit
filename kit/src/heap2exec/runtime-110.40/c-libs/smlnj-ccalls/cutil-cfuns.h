/* cutil-cfuns.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Declarations for some useful user-level C functions.
 *
 * This file is included by cfun-list.h; it should only have C_CALLS_CFUNCs.
 */

C_CALLS_CFUNC("ptos",	ptos,	char *,	(void *))
C_CALLS_CFUNC("ptoi",	ptoi,	int,	(void *))
C_CALLS_CFUNC("free",	_FREE,	void,	(void *))

/* end of cutil-cfuns.h */
