/* tags.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * These are the macros for object tags and descriptors.
 */

#ifndef _TAGS_
#define _TAGS_

#if defined(_ASM_) && defined(OPSYS_WIN32)
#define HEXLIT(x)          CONCAT3(0,x,h)
#define OROP OR
#define ANDOP AND
#else
#define HEXLIT(y)          CONCAT(0x,y)
#define OROP |
#define ANDOP &
#endif

#define MAJOR_MASK	HEXLIT(3)	/* bits 0-1 are the major tag */

#ifdef BOXED1
					/* Major tag: */
#define TAG_boxed	HEXLIT(1)	/*   01 - pointers */
#define TAG_desc	HEXLIT(3)	/*   11 - descriptors */
#define TAG_unboxed_b0	HEXLIT(0)	/*   00, 10 - unboxed (bit 0 is 0) */
					/* High bit of descriptor for objects with */
					/* a length operation is unboxed bit-0: */
#define HASLEN		HEXLIT(0)	/*   0xxx - objects with length */
#define NOLEN		HEXLIT(8)	/*   1xxx - objects without a length */

/* mark/unmark an ML pointer to make it look like an unboxed object */
#define MARK_PTR(p)	((ml_val_t)((Addr_t)(p) ANDOP ~HEXLIT(1)))
#define UNMARK_PTR(p)	((ml_val_t)((Addr_t)(p)  OROP  HEXLIT(1)))

#else /* BOXED1 */
					/* Major tag: */
#define TAG_boxed	HEXLIT(0)	/*   00 - pointers */
#define TAG_desc	HEXLIT(2)	/*   10 - descriptors */
#define TAG_unboxed_b0	HEXLIT(1)	/*   01, 11 - unboxed (bit 0 is 1) */
					/* High bit of descriptor for objects with */
					/* a length operation is unboxed bit-0: */
#define HASLEN		HEXLIT(8)	/*   1xxx - objects with length */
#define NOLEN		HEXLIT(0)	/*   0xxx - objects without length */

/* mark/unmark an ML pointer to make it look like an unboxed object */
#define MARK_PTR(p)	((ml_val_t)((Addr_t)(p) OROP HEXLIT(1)))
#define UNMARK_PTR(p)	((ml_val_t)((Addr_t)(p) ANDOP ~HEXLIT(1)))

#endif /* BOXED1 */

/* Descriptors have four more tag bits (defined below).  For objects that
 * have a ML length operation, the high bit is the unboxed bit-0 value. */
#define DTAG_SHIFTW	2
#define DTAG_WID	4
#define DTAG_MASK	(((1 << DTAG_WID)-1) << DTAG_SHIFTW)
#define TAG_SHIFTW	(DTAG_SHIFTW+DTAG_WID)

#define DTAG_record	(HASLEN OROP HEXLIT(0)) /* records (len != 2) and vectors */
#define DTAG_vector	DTAG_record
#define DTAG_array	(HASLEN OROP HEXLIT(1)) /* arrays and refs */
#define DTAG_string	(HASLEN OROP HEXLIT(2)) /* strings */
/*			(HASLEN OROP HEXLIT(3)) unused */
#define DTAG_bytearray	(HASLEN OROP HEXLIT(4)) /* bytearrays */
#define DTAG_realdarray	(HASLEN OROP HEXLIT(5)) /* real array (double precision) */
/*			(HASLEN OROP HEXLIT(6)) unused */
/*			(HASLEN OROP HEXLIT(7)) unused */
#define DTAG_pair	(NOLEN OROP HEXLIT(0)) /* pairs */
#define DTAG_reald	(NOLEN OROP HEXLIT(1)) /* reals (double precision) */
/*			(NOLEN OROP HEXLIT(2)) unused */
#define DTAG_variant	(NOLEN OROP HEXLIT(3)) /* tagged variant record (see below) */
#define DTAG_special	(NOLEN OROP HEXLIT(4)) /* special (susp, weak ptr, ...) */
#define DTAG_backptr	(NOLEN OROP HEXLIT(5)) /* a back pointer (obsolete) */
#define DTAG_cont	(NOLEN OROP HEXLIT(5)) /* a quasi-stack frame */
#define DTAG_extern	(NOLEN OROP HEXLIT(6)) /* an external symbol reference (used in */
					/* exported heap images) */
#define DTAG_forwarded	(NOLEN OROP HEXLIT(7)) /* a forwarded object */

/* Variant tags have a small length field, plus other bits */
#define VARIANT_LEN_BITS	4
#define VARIANT_OTHER_BITS	(BITS_PER_WORD-(DTAG_SHIFTW+VARIANT_LEN_BITS))

/* Build a descriptor from a descriptor tag and a length */
#ifndef _ASM_
#define MAKE_TAG(t)	(((t) << DTAG_SHIFTW) OROP TAG_desc)
#define MAKE_DESC(l,t)	((ml_val_t)(((l) << TAG_SHIFTW) OROP MAKE_TAG(t)))
#else
#define MAKE_TAG(t)	(((t)*4) + TAG_desc)
#define MAKE_DESC(l,t)	(((l)*64) + MAKE_TAG(t))
#endif

#define DESC_pair	MAKE_DESC(2, DTAG_pair)
#define DESC_exn	MAKE_DESC(3, DTAG_record)
#define DESC_ref	MAKE_DESC(1, DTAG_array)
#define DESC_reald	MAKE_DESC(2, DTAG_reald)

#define DESC_forwarded	MAKE_DESC(0, DTAG_forwarded)

/* There are two kinds of special objects: suspensions and weak pointers
 * The length field of these defines the state and kind of special object:
 */
#define SPCL_evaled_susp	0	/* unevaluated suspension */
#define SPCL_unevaled_susp	1	/* evaluated suspension */
#define SPCL_weak		2	/* weak pointer */
#define SPCL_null_weak		3	/* nulled weak pointer */

#define DESC_evaled_susp	MAKE_DESC(SPECIAL_evaled_susp, DTAG_special)
#define DESC_unevaled_susp	MAKE_DESC(SPCL_unevaled_susp, DTAG_special)
#define DESC_weak		MAKE_DESC(SPCL_weak, DTAG_special)
#define DESC_null_weak		MAKE_DESC(SPCL_null_weak, DTAG_special)

/* tests on words:
 *   isBOXED(W)   -- true if W is tagged as an boxed value
 *   isUNBOXED(W) -- true if W is tagged as an unboxed value
 *   isDESC(W)    -- true if W is tagged as descriptor
 */
#define isBOXED(W)	(((Word_t)(W) ANDOP MAJOR_MASK) == TAG_boxed)
#define isUNBOXED(W)	(((Word_t)(W) ANDOP HEXLIT(1)) == TAG_unboxed_b0)
#define isDESC(W)	(((Word_t)(W) ANDOP MAJOR_MASK) == TAG_desc)

/* extract descriptor fields */
#define GET_LEN(D)		(((Word_t)(D)) >> TAG_SHIFTW)
#define GET_STR_LEN(D)		((((Word_t)(D))+(3<<TAG_SHIFTW)) >> (TAG_SHIFTW+2))
#define GET_REALDARR_LEN(D)	(GET_LEN(D)*(REALD_SZB/WORD_SZB))
#define GET_TAG(D)		((((Word_t)(D)) ANDOP DTAG_MASK) >> DTAG_SHIFTW)
#define GET_VARIANT_LEN(D)	(GET_LEN(D) ANDOP ((1 << VARIANT_LEN_BITS)-1))

#endif /* !_TAGS_ */
