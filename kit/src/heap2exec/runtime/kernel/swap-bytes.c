/* swap-bytes.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#include "ml-base.h"

#ifdef BYTE_ORDER_LITTLE

#if (WORD_SZB == 8)  /* 64-bit ML words */
/* SwapBytesOfWord:
 */
Word_t SwapBytes (Word_t x)
{
    unsigned int	b0 = x & 0x00000000000000FF;
    unsigned int	b1 = x & 0x000000000000FF00;
    unsigned int	b2 = x & 0x0000000000FF0000;
    unsigned int	b3 = x & 0x00000000FF000000;
    unsigned int	b4 = x & 0x000000FF00000000;
    unsigned int	b5 = x & 0x0000FF0000000000;
    unsigned int	b6 = x & 0x00FF000000000000;
    unsigned int	b7 = x & 0xFF00000000000000;

    return ((b0 << 56) | (b1 << 40) | (b2 << 24) | (b3 << 8)
	 | (b4 >> 8) | (b5 >> 24) | (b6 >> 40) | (b7 >> 56));

} /* end of SwapBytesOfWord */
#endif 

/* SwapBytes:
 */
Unsigned32_t SwapBytes (Unsigned32_t x)
{
    unsigned int	b0 = x & 0x000000FF;
    unsigned int	b1 = x & 0x0000FF00;
    unsigned int	b2 = x & 0x00FF0000;
    unsigned int	b3 = x & 0xFF000000;

    return ((b0 << 24) | (b1 << 8) | (b2 >> 8) | (b3 >> 24));

} /* end of SwapBytes */

#endif
