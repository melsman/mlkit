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

/* mark/unmark an ML pointer to make it look like an unboxed object */
#define MARK_PTR(p)	((ml_val_t)((Addr_t)(p) ANDOP ~HEXLIT(1)))
#define UNMARK_PTR(p)	((ml_val_t)((Addr_t)(p)  OROP  HEXLIT(1)))

#else /* BOXED1 */
					/* Major tag: */
#define TAG_boxed	HEXLIT(0)	/*   00 - pointers */
#define TAG_desc	HEXLIT(2)	/*   10 - descriptors */
#define TAG_unboxed_b0	HEXLIT(1)	/*   01, 11 - unboxed (bit 0 is 1) */

/* mark/unmark an ML pointer to make it look like an unboxed object */
#define MARK_PTR(p)	((ml_val_t)((Addr_t)(p) OROP HEXLIT(1)))
#define UNMARK_PTR(p)	((ml_val_t)((Addr_t)(p) ANDOP ~HEXLIT(1)))

#endif /* BOXED1 */

/* Descriptors have five more tag bits (defined below). */
#define DTAG_SHIFTW	2
#define DTAG_WID	5
#define DTAG_MASK	(((1 << DTAG_WID)-1) << DTAG_SHIFTW)
#define TAG_SHIFTW	(DTAG_SHIFTW+DTAG_WID)

#define DTAG_record	HEXLIT(0)	/* records (including pairs) */
#define DTAG_vec_hdr	HEXLIT(1)	/* vector header; length is kind */
#define DTAG_vec_data	DTAG_record	/* polymorphic vector data */
#define DTAG_arr_hdr	HEXLIT(2)	/* array header; length is kind */
#define DTAG_arr_data	HEXLIT(3)	/* polymorphic array data */
#define DTAG_ref	DTAG_arr_data	/* reference cell */
#define DTAG_raw32	HEXLIT(4)	/* 32-bit aligned non-pointer data */
#define DTAG_raw64	HEXLIT(5)	/* 64-bit aligned non-pointer data */
#define DTAG_special	HEXLIT(6)	/* Special object; length is kind */
#define DTAG_extern	HEXLIT(10)	/* external symbol reference (used in */
					/* exported heap images) */
#define DTAG_forward	HEXLIT(1F)	/* a forwarded object */

/* Vector and array headers come in different kinds; the kind tag is stored
 * in the length field of the descriptor.  We need these codes for polymorphic
 * equality and pretty-printing.
 */
#define SEQ_poly	HEXLIT(0)
#define SEQ_word8	HEXLIT(1)
#define SEQ_word16	HEXLIT(2)
#define SEQ_word31	HEXLIT(3)
#define SEQ_word32	HEXLIT(4)
#define SEQ_real32	HEXLIT(5)
#define SEQ_real64	HEXLIT(6)

/* Build a descriptor from a descriptor tag and a length */
#ifndef _ASM_
#define MAKE_TAG(t)	(((t) << DTAG_SHIFTW) OROP TAG_desc)
#define MAKE_DESC(l,t)	((ml_val_t)(((l) << TAG_SHIFTW) OROP MAKE_TAG(t)))
#else
#define MAKE_TAG(t)	(((t)*4) + TAG_desc)
#define MAKE_DESC(l,t)	(((l)*128) + MAKE_TAG(t))
#endif

#define DESC_pair	MAKE_DESC(2, DTAG_record)
#define DESC_exn	MAKE_DESC(3, DTAG_record)
#define DESC_ref	MAKE_DESC(1, DTAG_ref)
#define DESC_reald	MAKE_DESC(2, DTAG_raw64)
#define DESC_polyvec	MAKE_DESC(SEQ_poly, DTAG_vec_hdr)
#define DESC_polyarr	MAKE_DESC(SEQ_poly, DTAG_arr_hdr)
#define DESC_word8arr	MAKE_DESC(SEQ_word8, DTAG_arr_hdr)
#define DESC_word8vec	MAKE_DESC(SEQ_word8, DTAG_vec_hdr)
#define DESC_string	MAKE_DESC(SEQ_word8, DTAG_vec_hdr)
#define DESC_real64arr	MAKE_DESC(SEQ_real64, DTAG_arr_hdr)

#define DESC_forwarded	MAKE_DESC(0, DTAG_forward)

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
#define isBOXED(W)	(((Word_t)(W) & MAJOR_MASK) == TAG_boxed)
#define isUNBOXED(W)	(((Word_t)(W) & 1) == TAG_unboxed_b0)
#define isDESC(W)	(((Word_t)(W) & MAJOR_MASK) == TAG_desc)

/* extract descriptor fields */
#define GET_LEN(D)		(((Word_t)(D)) >> TAG_SHIFTW)
#define GET_TAG(D)		((((Word_t)(D)) ANDOP DTAG_MASK) >> DTAG_SHIFTW)

#endif /* !_TAGS_ */
