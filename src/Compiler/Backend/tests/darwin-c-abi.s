.text
.p2align 2
.globl _mlkit_abi_packed
_mlkit_abi_packed:
    cmn w0, #1
    b.ne Lfail
    cmn w7, #8
    b.ne Lfail
    ldrsb w8, [sp]
    cmn w8, #9
    b.ne Lfail
    ldrsb w8, [sp, #1]
    cmn w8, #10
    cset w0, eq
    ret
Lfail:
    mov x0, #0
    ret

.p2align 2
.globl _mlkit_abi_varargs
_mlkit_abi_varargs:
    ldrsw x9, [sp]
    cmn x9, #7
    b.ne Lfail
    ldr d0, [sp, #8]
    fmov d1, #1.5
    fcmp d0, d1
    b.ne Lfail
    ldr x9, [sp, #16]
    cmp x9, #99
    cset w0, eq
    ret

.p2align 2
.globl _mlkit_abi_callback
_mlkit_abi_callback:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    mov x16, x0
    mov x0, #41
    fmov d0, #1.5
    blr x16
    ldp x29, x30, [sp], #16
    ret
