/* C ABI bridge to the generated private allocation ABI. */
.text
.p2align 2
.globl _arm64_generated_alloc
_arm64_generated_alloc:
  stp x29, x30, [sp, #-16]!
  mov x16, x0
  mov x17, x1
  bl _mlkit_arm64_allocate_preserving
  mov x0, x16
  ldp x29, x30, [sp], #16
  ret
