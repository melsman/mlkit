allocinreg_protected:
	cmpq $1022,%r10
	ja __allocate@PLT
	movq %rax,%xmm14
	movq %r10,%xmm15
	movq (%r11),%rax
	leaq (%rax,%r10,8),%r10
	subq $1,%rax
	orq $0x1FFF,%rax
	addq $1,%rax
	cmpq %rax,%r10
	ja L_alloc_fallback_dziDRpFvLP4z9nrUXFmD9b
	movq %xmm15,%rax
	salq $3,%rax
	negq %rax
	addq %r10,%rax
	lock cmpxchgq %r10,(%r11)
	jne L_alloc_fallback_dziDRpFvLP4z9nrUXFmD9b
	movq %rax,%r11
	movq %xmm14,%rax
	ret
L_alloc_fallback_dziDRpFvLP4z9nrUXFmD9b:
	movq %xmm15,%r10
	movq %xmm14,%rax
	jmp __allocate@PLT
