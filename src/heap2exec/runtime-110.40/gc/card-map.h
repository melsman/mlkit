/* card-map.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Card maps for marking object updates.
 */

#ifndef _CARD_MAP_
#define _CARD_MAP_

#ifndef BIT_CARDS
typedef struct {		      /* A dirty card map */
    ml_val_t		*baseAddr;	/* The base address of the mapped region */
    Word_t		numCards;	/* The number of cards covered by the map */
    int			mapSzB;		/* The number of bytes allocated for this */
					/* map. */
    Byte_t		map[WORD_SZB];	/* The card map */
} card_map_t;

#define CARD_CLEAN	0xff

#define CARD_BITS	8				/* 256 byte cards */
#define CARD_SZB	(1<<CARD_BITS)
#define CARD_SZW	(CARD_SZB / WORD_SZB)
#define CARD_SHIFTW	(BITS_PER_WORD-CARD_BITS)
#define CARD_MAP_SZ(n)	\
    (sizeof(card_map_t) + ((((n)+(WORD_SZB-1)) >> LOG_BYTES_PER_WORD)-1)*WORD_SZB)

/* Map an address to a card index */
#define CARD_INDEX(cm, addr)						\
	(((Addr_t)(addr) - (Addr_t)(cm->baseAddr)) >> CARD_BITS)

/* Map a card index to its base address */
#define CARD_TO_ADDR(cm, index)						\
	((Word_t *)((Addr_t)(cm->baseAddr) + ((index) << CARD_BITS)))

/* Get the value of a card */
#define CARD(cm, addr)			((cm)->map[CARD_INDEX(cm,addr)])

/* Mark the card containing addr */
#define MARK_CARD(cm, addr, gen)	{			\
	card_map_t  *__cm = (cm);				\
	int	    __i = CARD_INDEX(__cm, (addr));		\
	int	    __g = (gen);				\
	if (__g < __cm->map[__i])				\
	    __cm->map[__i] = __g;				\
    }

/* test a card to see if it is marked */
#define isDIRTY(cm, indx, maxGen)	((cm)->map[(indx)] <= (maxGen))

/* Iterate over the dirty cards of a card map. The argument indexVar
 * should be an integer variable; it is used to pass the index of dirty
 * cards to cmd.
 */
#ifdef COUNT_CARDS
#define COUNT_DIRTY(indexVar)	\
    if(__cm->map[indexVar] != CARD_CLEAN) cardCnt2[i]++
#else
#define COUNT_DIRTY(indexVar)	/* null */
#endif
#define FOR_DIRTY_CARD(cm, maxGen, indexVar, cmd)	{	\
	card_map_t  *__cm = (cm);				\
	int	    __n = __cm->numCards;			\
	int	    __g = (maxGen);				\
	for (indexVar = 0;  indexVar < __n;  indexVar++) {	\
	    COUNT_DIRTY(indexVar);				\
	    if (isDIRTY(__cm, indexVar, __g)) {			\
		cmd						\
	    }							\
	}							\
    }

#else
/** Memory cards **
 * The type of "cards" of memory.  These are used to keep track of
 * dirty regions in the array arenas.
 * NOTE: we use bitvectors to implement card maps.  It has been suggested that
 * byte arrays are more efficient, since they avoid the read-modify-write
 * required when setting a bit, but this doesn't seem to hold for SML.  I
 * think that this is because updates are less frequent, so that the savings
 * on marking cards dirty doesn't offset the added cost of sweeping.
 */
typedef struct {		      /* A dirty card map */
    ml_val_t		*baseAddr;	/* The base address of the mapped region */
    Word_t		numCards;	/* The number of cards covered by the map */
    int			mapSzB;		/* The number of bytes allocated for this */
					/* map. */
    Word_t		map[1];		/* The card map */
} card_map_t;

/* #define OLD_CARDS */
#ifdef OLD_CARDS
#define CARD_BITS	10				/* 1024 byte cards */
#else
#define CARD_BITS	8				/* 256 byte cards */
#endif
#define CARD_SZB	(1<<CARD_BITS)
#define CARD_SZW	(CARD_SZB / WORD_SZB)
#define CARD_SHIFTW	(BITS_PER_WORD-CARD_BITS)
#define CARD_MAP_SZ(n)	\
    (sizeof(card_map_t) + ((((n)+(BITS_PER_WORD-1)) >> LOG_BITS_PER_WORD)-1)*WORD_SZB)

/* Map an address to a card index */
#define CARD_INDEX(cm, addr)						\
	(((Addr_t)(addr) - (Addr_t)(cm->baseAddr)) >> CARD_BITS)

/* Map a card index to its base address */
#define CARD_TO_ADDR(cm, index)						\
	((Word_t *)((Addr_t)(cm->baseAddr) + ((index) << CARD_BITS)))

/* Mark the card containing addr */
#define MARK_CARD(cm, addr)	{					\
	card_map_t  *__cm = (cm);					\
	Word_t	    __offset = CARD_INDEX(__cm, addr);			\
	__cm->map[__offset >> LOG_BITS_PER_WORD] |=			\
	    (1 << (__offset & (BITS_PER_WORD-1)));			\
    }

/* test a card to see if it is marked */
#define isDIRTY(cm, indx)	\
    ((cm)->map[(indx) >> LOG_BITS_PER_WORD] & (1 << ((indx) & (BITS_PER_WORD-1))))

/* Iterate over the dirty cards of a card map. The argument indexVar
 * should be an integer variable; it is used to pass the index of dirty
 * cards to cmd.
 */
#define FOR_DIRTY_CARD(cm, indexVar, cmd)	{	\
	card_map_t  *__cm = (cm);			\
	int	    __i = 0, __j = 0;			\
	while (__j < __cm->numCards) {			\
	    Word_t	__m = __cm->map[__i];		\
	    indexVar = __j;				\
	    for (; __m != 0; __m >>= 1) {		\
		if (__m & 1) {				\
		    cmd;				\
		}					\
		indexVar++;				\
	    }						\
	    __i++;  __j += BITS_PER_WORD;		\
	}						\
    }
#endif

#endif /* !_CARD_MAP_ */
