/* Verify all Darwin callee-saved integer and low-64-bit FP registers. */
.text
.p2align 2
.globl _arm64_callback
_arm64_callback:
sub sp, sp, #160
str x19, [sp, #0]
str x20, [sp, #8]
str x21, [sp, #16]
str x22, [sp, #24]
str x23, [sp, #32]
str x24, [sp, #40]
str x25, [sp, #48]
str x26, [sp, #56]
str x27, [sp, #64]
str x28, [sp, #72]
str x29, [sp, #80]
str x30, [sp, #88]
str d8, [sp, #96]
str d9, [sp, #104]
str d10, [sp, #112]
str d11, [sp, #120]
str d12, [sp, #128]
str d13, [sp, #136]
str d14, [sp, #144]
str d15, [sp, #152]
add x29, sp, #80
mov x19, #119
mov x20, #120
mov x21, #121
mov x22, #122
mov x23, #123
mov x24, #124
mov x25, #125
mov x26, #126
mov x27, #127
mov x28, #128
mov x16, #208
fmov d8, x16
mov x16, #209
fmov d9, x16
mov x16, #210
fmov d10, x16
mov x16, #211
fmov d11, x16
mov x16, #212
fmov d12, x16
mov x16, #213
fmov d13, x16
mov x16, #214
fmov d14, x16
mov x16, #215
fmov d15, x16
bl _arm64_hook
cmp x19, #119
b.ne 1f
cmp x20, #120
b.ne 1f
cmp x21, #121
b.ne 1f
cmp x22, #122
b.ne 1f
cmp x23, #123
b.ne 1f
cmp x24, #124
b.ne 1f
cmp x25, #125
b.ne 1f
cmp x26, #126
b.ne 1f
cmp x27, #127
b.ne 1f
cmp x28, #128
b.ne 1f
fmov x16, d8
cmp x16, #208
b.ne 1f
fmov x16, d9
cmp x16, #209
b.ne 1f
fmov x16, d10
cmp x16, #210
b.ne 1f
fmov x16, d11
cmp x16, #211
b.ne 1f
fmov x16, d12
cmp x16, #212
b.ne 1f
fmov x16, d13
cmp x16, #213
b.ne 1f
fmov x16, d14
cmp x16, #214
b.ne 1f
fmov x16, d15
cmp x16, #215
b.ne 1f
add x16, sp, #80
cmp x29, x16
b.ne 1f
ldr x19, [sp, #0]
ldr x20, [sp, #8]
ldr x21, [sp, #16]
ldr x22, [sp, #24]
ldr x23, [sp, #32]
ldr x24, [sp, #40]
ldr x25, [sp, #48]
ldr x26, [sp, #56]
ldr x27, [sp, #64]
ldr x28, [sp, #72]
ldr x29, [sp, #80]
ldr x30, [sp, #88]
ldr d8, [sp, #96]
ldr d9, [sp, #104]
ldr d10, [sp, #112]
ldr d11, [sp, #120]
ldr d12, [sp, #128]
ldr d13, [sp, #136]
ldr d14, [sp, #144]
ldr d15, [sp, #152]
add sp, sp, #160
ret
1:
bl _abort
