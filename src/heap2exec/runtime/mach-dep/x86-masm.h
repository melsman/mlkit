/* x86-masm.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * macros that make MS MASM assembler look somewhat like unix assembler
 */

/* note: every 32-bit machine word must be treated as an MS "double word" */

/* directives */
#define GLOBAL(ID)	PUBLIC ID
#define LABEL(ID)	ID:
#define ALIGN4        	EVEN
#define WORD16(n,w)   	n WORD w
#define WORD32(n,w)   	n DWORD w 
#define TEXT          	.CODE
#define DATA          	.DATA
#define BEGIN_PROC(P)	.ent P
#define END_PROC(P)	.end P

/* operands */
#define IMMED(X)      	X
#define REG(x)		x
#define FP_REG(x)     	x
#define IND_OFF(t,r,o)  t PTR o [r]
#define IND_DW_OFF(r,o) IND_OFF(DWORD,r,o)
#define IND_W_OFF(r,o)  IND_OFF(WORD,r,o)

/* labels */
#define FLAB(x) 	x
#define BLAB(x) 	x
#define ANON_LAB    	@@
#define BLAB_ANON     	@b
#define FLAB_ANON     	@f

/* instructions */
#define MOVL(a,b)     	mov    b, a
#define MOVW(a,b)     	mov    b, a
#define ADDL(a,b)     	add    b, a
#define SUBL(a,b)     	sub    b, a
#define SARL(a,b)     	sar    b, a
#define SALL(a,b)     	sal    b, a
#define SHRL(a,b)     	shr    b, a
#define SHLL(a,b)     	shl    b, a
#define ORL(a,b)      	or     b, a
#define ORW(a,b)      	or     b, a
#define ANDL(a,b)     	and    b, a
#define ANDW(a,b)     	and    b, a

#define INCL          	inc
#define XCHGL(a,b)    	xchg   a, b
#define PUSHL         	push
#define POPL          	pop
#define CMPL(a,b)     	cmp    b, a
#define LEA(a,b)      	lea    b, a
#define JMP           	jmp
#define JB            	jb
#define JGE           	jge
#define JNE           	jne
#define RET           	ret
#define CALL          	call
#define STOSL         	stosd

/* fp */
#define FICOML        	ficom
#define FLDL          	fld
#define FISTPL        	fistp
#define FSTPL         	fstp
#define FISTL         	fist
#define FILDL         	fild
#define FSTCW           fstcw

/* end of x86-masm.h */

