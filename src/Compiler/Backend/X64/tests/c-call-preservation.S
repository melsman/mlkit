/* Deliberately clobber every SysV caller-save GPR and low FP lane.
 * Keep the argument as the result; C-preserved registers remain untouched. */
.text
.globl preservation_identity
.type preservation_identity,@function
preservation_identity:
  movq %rdi,%rax
  xorq %rdi,%rdi
  xorq %rsi,%rsi
  xorq %rdx,%rdx
  xorq %rcx,%rcx
  xorq %r8,%r8
  xorq %r9,%r9
  xorq %r10,%r10
  xorq %r11,%r11
  pxor %xmm0,%xmm0
  pxor %xmm1,%xmm1
  pxor %xmm2,%xmm2
  pxor %xmm3,%xmm3
  pxor %xmm4,%xmm4
  pxor %xmm5,%xmm5
  pxor %xmm6,%xmm6
  pxor %xmm7,%xmm7
  pxor %xmm8,%xmm8
  pxor %xmm9,%xmm9
  pxor %xmm10,%xmm10
  pxor %xmm11,%xmm11
  pxor %xmm12,%xmm12
  pxor %xmm13,%xmm13
  pxor %xmm14,%xmm14
  pxor %xmm15,%xmm15
  ret
.size preservation_identity,.-preservation_identity
.section .note.GNU-stack,"",@progbits
