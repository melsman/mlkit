/* The aligned SSE store deterministically faults on a misaligned call.
 * Use the red zone without changing rsp or any argument registers. */
.text
.globl alignment0
.type alignment0, @function
alignment0:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check0
.size alignment0, .-alignment0
.globl alignment6
.type alignment6, @function
alignment6:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check6
.size alignment6, .-alignment6
.globl alignment7
.type alignment7, @function
alignment7:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check7
.size alignment7, .-alignment7
.globl alignment8
.type alignment8, @function
alignment8:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check8
.size alignment8, .-alignment8
.globl alignment9
.type alignment9, @function
alignment9:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check9
.size alignment9, .-alignment9
.globl alignment10
.type alignment10, @function
alignment10:
  movaps %xmm0, -24(%rsp)
  jmp alignment_check10
.size alignment10, .-alignment10
.section .note.GNU-stack,"",@progbits
