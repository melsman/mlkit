/* C entry into the private allocator; verify non-scratch register preservation. */
#ifdef __APPLE__
#define NAME(x) _##x
#else
#define NAME(x) x
#endif
.text
.p2align 4
.globl NAME(x64_generated_alloc)
NAME(x64_generated_alloc):
  subq $8, %rsp
  movq %rdi, %r11
  movq %rsi, %r10
  movq $1234567, %rax
  movq %rax, %xmm13
  call NAME(allocinreg)
  cmpq $1234567, %rax
  jne 1f
  movq %xmm13, %rax
  cmpq $1234567, %rax
  jne 1f
  movq %r11, %rax
  addq $8, %rsp
  ret
1:
  call NAME(abort)
/* The C stress fixture must never raise an ML exception. */
.globl NAME(raise_exn)
NAME(raise_exn):
  jmp NAME(abort)
#ifndef __APPLE__
.section .note.GNU-stack,"",@progbits
#endif
