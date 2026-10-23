.text
.globl _mlkit_arm64_reset_preserving
_mlkit_arm64_reset_preserving:
	fmov d30, x16
	fmov d31, x30
	and x16, x16, #-4
	ldr x17, [x16, #40]
	cbnz x17, L_arm64_gH1z68AymUVtfEL0Rc0Qks
	ldr x17, [x16, #8]
	and x17, x17, #-32
	ldr x30, [x17, #0]
	cbnz x30, L_arm64_gH1z68AymUVtfEL0Rc0Qks
	ldr x17, [x16, #24]
	and x17, x17, #-32
	ldr x30, [x17, #0]
	cbnz x30, L_arm64_gH1z68AymUVtfEL0Rc0Qks
	ldr x17, [x16, #8]
	and x17, x17, #-32
	add x17, x17, #24
	str x17, [x16, #0]
	sub x30, x17, #8
	str x17, [x30, #0]
	ldr x17, [x16, #24]
	and x17, x17, #-32
	add x17, x17, #24
	str x17, [x16, #16]
	sub x30, x17, #8
	str x17, [x30, #0]
	fmov x16, d30
	fmov x30, d31
	ret
L_arm64_gH1z68AymUVtfEL0Rc0Qks:
	fmov x16, d30
	fmov x30, d31
	b _mlkit_arm64_reset_slow
.text
.p2align 2
.globl _mlkit_arm64_reset_slow
_mlkit_arm64_reset_slow:
	sub sp, sp, #320
	stp x0, x1, [sp, #0]
	stp x2, x3, [sp, #16]
	stp x4, x5, [sp, #32]
	stp x6, x7, [sp, #48]
	stp x8, x9, [sp, #64]
	stp x10, x11, [sp, #80]
	stp x12, x13, [sp, #96]
	stp x14, x15, [sp, #112]
	stp d0, d1, [sp, #128]
	stp d2, d3, [sp, #144]
	stp d4, d5, [sp, #160]
	stp d6, d7, [sp, #176]
	stp d16, d17, [sp, #192]
	stp d18, d19, [sp, #208]
	stp d20, d21, [sp, #224]
	stp d22, d23, [sp, #240]
	stp d24, d25, [sp, #256]
	stp d26, d27, [sp, #272]
	stp d28, d29, [sp, #288]
	str x30, [sp, #304]
	mov x0, x16
	bl _resetRegion
	mov x16, x0
	ldp x0, x1, [sp, #0]
	ldp x2, x3, [sp, #16]
	ldp x4, x5, [sp, #32]
	ldp x6, x7, [sp, #48]
	ldp x8, x9, [sp, #64]
	ldp x10, x11, [sp, #80]
	ldp x12, x13, [sp, #96]
	ldp x14, x15, [sp, #112]
	ldp d0, d1, [sp, #128]
	ldp d2, d3, [sp, #144]
	ldp d4, d5, [sp, #160]
	ldp d6, d7, [sp, #176]
	ldp d16, d17, [sp, #192]
	ldp d18, d19, [sp, #208]
	ldp d20, d21, [sp, #224]
	ldp d22, d23, [sp, #240]
	ldp d24, d25, [sp, #256]
	ldp d26, d27, [sp, #272]
	ldp d28, d29, [sp, #288]
	ldr x30, [sp, #304]
	add sp, sp, #320
	ret
L_arm64_bOMJX57FdrkCqMiV5uafba:
.text
.p2align 2
