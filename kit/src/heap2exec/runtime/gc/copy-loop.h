/* copy-loop.h
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * A dirty, but quick, copy loop for the GC.
 */

#ifndef _COPY_LOOP_
#define _COPY_LOOP_

#ifdef HAS_INCREMENT

#define COPYLOOP(SRC,DST,LEN)	{			\
	Word_t	*__src = (Word_t *)(SRC);		\
	Word_t	*__dst = (Word_t *)(DST);		\
	int	__len = (LEN);				\
	int	__m;					\
	switch (__len & 0x3) {				\
	  case 3: *__dst++ = *__src++;			\
	  case 2: *__dst++ = *__src++;			\
	  case 1: *__dst++ = *__src++;			\
	  case 0: break;				\
	}						\
	__m = __len >> 2;				\
	while (--__m >= 0) {				\
	    *__dst++ = *__src++; *__dst++ = *__src++;	\
	    *__dst++ = *__src++; *__dst++ = *__src++;	\
	}						\
    }

#else

#define COPYLOOP(SRC,DST,LEN)	{			\
	Word_t	*__src = (Word_t *)(SRC);		\
	Word_t	*__dst = (Word_t *)(DST);		\
	int	__len = (LEN);				\
	int	__m;					\
	switch (__len & 0x3) {				\
	  case 3: *__dst++ = *__src++;			\
	  case 2: *__dst++ = *__src++;			\
	  case 1: *__dst++ = *__src++;			\
	  case 0: break;				\
	}						\
	__m = __len >> 2;				\
	while (--__m >= 0) {				\
	    __dst[0] = __src[0]; __dst[1] = __src[1];	\
	    __dst[2] = __src[2]; __dst[3] = __src[3];	\
	    __dst += 4; __src += 4;			\
	}						\
    }

#endif

#endif /* !_COPY_LOOP_ */

