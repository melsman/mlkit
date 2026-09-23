.text
.p2align 6
.globl _b_ret
_b_ret:
stp x19, x30, [sp, #-16]!
mov x19, x0
mov x0, #0
L_b_ret_loop:
adr x30, L_b_ret_cont
b L_b_ret_leaf
L_b_ret_cont:
subs x19, x19, #1
b.ne L_b_ret_loop
ldp x19, x30, [sp], #16
ret
L_b_ret_leaf:
add x0, x0, #1
ret
.p2align 6
.globl _b_br
_b_br:
stp x19, x30, [sp, #-16]!
mov x19, x0
mov x0, #0
L_b_br_loop:
adr x30, L_b_br_cont
b L_b_br_leaf
L_b_br_cont:
subs x19, x19, #1
b.ne L_b_br_loop
ldp x19, x30, [sp], #16
ret
L_b_br_leaf:
add x0, x0, #1
br x30
.p2align 6
.globl _bl_ret
_bl_ret:
stp x19, x30, [sp, #-16]!
mov x19, x0
mov x0, #0
L_bl_ret_loop:
nop
bl L_bl_ret_leaf
L_bl_ret_cont:
subs x19, x19, #1
b.ne L_bl_ret_loop
ldp x19, x30, [sp], #16
ret
L_bl_ret_leaf:
add x0, x0, #1
ret
.p2align 6
.globl _bl_br
_bl_br:
stp x19, x30, [sp, #-16]!
mov x19, x0
mov x0, #0
L_bl_br_loop:
nop
bl L_bl_br_leaf
L_bl_br_cont:
subs x19, x19, #1
b.ne L_bl_br_loop
ldp x19, x30, [sp], #16
ret
L_bl_br_leaf:
add x0, x0, #1
br x30
