/* c-entry.asm 
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "asm-base.h"

#if defined(TARGET_X86)
#define CALL_BIAS 	5
#define cresult	%eax

#elif defined(TARGET_MIPS)

#define CALL_BIAS	20

#define cresult	$2
#define carg0	$4
#define carg1	$5
#define carg2   $6
#define carg3	$7
/* #define ra      $31 */

#define tmp0	$3
#define tmp1    $8

#elif defined(TARGET_SPARC)

#define CALL_BIAS	8
#define DELAY 	nop

#endif

/* offsets must match the C declaration of code_header_t */
#define NARGS_OFFSET	-4

/* grabPC:
 * routine to return the PC at entry to this function
 *
 * NOTE: this code must relocatable using bcopy.
 */

#ifdef OPSYS_WIN32

ENTRY_M MACRO name
	PUBLIC &name
	&name LABEL FAR
	EVEN
ENDM

	.386
	.MODEL FLAT


	EXTRN	_last_entry:DWORD
	EXTRN	_no_args_entry:FAR
	EXTRN	_some_args_entry:FAR

	TEXT
	EVEN

#else

	.text

	.align 2
	.globl CSYM(grabPC)
	.globl CSYM(grabPCend)

#endif

#if defined(TARGET_X86)
#if defined(OPSYS_LINUX)
CSYM(grabPC):
/*->*/	call	grabPCaux		/* put pc in %eax */
	subl	$CALL_BIAS,%eax		/* adjust pc to point at "->" */
	lea	CSYM(last_entry),%ecx	/* save it */
	movl	%eax,(%ecx)
	cmpl	$0,NARGS_OFFSET(%eax)
	jne	some_args
	lea	CSYM(no_args_entry),%ecx
	jmp	%ecx
	/* should never get here */
some_args:
	lea	CSYM(some_args_entry),%ecx
	jmp	%ecx
	/* should never get here */

/* WARNING: this is x86-linux assembler specific!
 * Above call must be relative.
 */
grabPCaux:
	pop	%eax	/* grab return address */
	push	%eax	/* put it back */
	ret
CSYM(grabPCend):
	nop
#elif defined(OPSYS_WIN32)
	PUBLIC CSYM(grabPCend)
	PUBLIC CSYM(grabPC)
CSYM(grabPC) LABEL FAR
/*->*/	call	grabPCaux		/* put pc in %eax */
	sub	eax,CALL_BIAS		/* adjust pc to point at "->" */
	lea	ecx,CSYM(last_entry)	/* save it */
	mov     dword ptr 0 [ecx],eax
	cmp	dword ptr (NARGS_OFFSET) [eax],0
	jne	some_args
	lea	ecx,CSYM(no_args_entry)
	jmp	ecx
	/* should never get here */
some_args:
	lea	ecx,CSYM(some_args_entry)
	jmp	ecx
	/* should never get here */

grabPCaux:
	pop	eax	/* grab return address */
	push	eax	/* put it back */
	ret
CSYM(grabPCend) LABEL FAR

	DATA
	PUBLIC 	CSYM(asm_entry_szb)
CSYM(asm_entry_szb) DWORD CSYM(grabPCend) - CSYM(grabPC)

#else
#error unknown x86 opsys
#endif
#elif defined(TARGET_MIPS)

grabPCaux:
	j	tmp1
		
	.align 	2
	.ent	grabPC 2
CSYM(grabPC):
/*->*/	la	tmp0,grabPCaux		  /* load address of grabPCaux */ 
	jalr	tmp1,tmp0		  /* call it, putting pc in tmp1 */
	subu	tmp1,CALL_BIAS		  /* adjust pc to point at "->" */
	la	tmp0,CSYM(last_entry)	  /* save it */
	sw	tmp1,0(tmp0)
	lw	tmp0,NARGS_OFFSET(tmp1) 
	bnez	tmp0,some_args
	la	t9,CSYM(no_args_entry)	  
	j	t9			  /* call C, must use t9 here */
	/* should never get here */
some_args:
	la	t9,CSYM(some_args_entry)
	j	t9			  /* call C, must use t9 here */
	/* should never get here */
CSYM(grabPCend):

#elif defined(TARGET_SPARC)
	.align 	4
CSYM(grabPC):
/*->*/	st	%o0,[%sp-4]		  /* get some temps */
	mov	%o7,%g1			  /* save ret addr in %g1 */
	call	grabPCaux		  /* call leaves pc in %o7 */ 
	DELAY
	mov	%o7,%o0			  /* restore ret addr */
	mov	%g1,%o7
	sub	%o0,CALL_BIAS,%o0	  /* unbias saved pc */
	set	CSYM(last_entry),%g1	  /* store it */
	st	%o0,[%g1]    
	ld	[%o0+NARGS_OFFSET],%o0  /* get # of args */
	tst	%o0
	ld	[%sp-4],%o0		  /* relinquish temps */
	bnz	some_args
	nop
	set	CSYM(no_args_entry),%g1
	jmp	%g1
	nop
	/* should never get here */
some_args:
	set	CSYM(some_args_entry),%g1
	jmp	%g1
	nop
	/* should never get here */

grabPCaux:
	/* return address is in %o7 */
	retl
	DELAY
CSYM(grabPCend):
#else
#error unknown target arch
#endif

#ifdef OPSYS_WIN32
	END
#elif !defined(TARGET_SPARC)
	.end
#endif

/* end of c-entry.asm */
