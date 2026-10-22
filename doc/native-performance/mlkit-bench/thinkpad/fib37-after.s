.text
F.main1_j24xhvTe9q1fKIKuKKcAEt:
	movq time_to_gc@GOTPCREL(%rip),%r9
	cmpq $1,(%r9)
	je L_gc_do_yvBCpeSn2OIyroobgF1Zpp
	.p2align 0x4
L_gc_done_jLn7oPTgzyxVckZc3Bi8pc:
	leaq -88(%rsp),%rsp
	leaq 40(%rsp),%r11
	movq %r14,%rdi
	movq %r11,%rsi
	call allocateRegion@PLT
	leaq (%rsp),%r11
	movq %r14,%rdi
	movq %r11,%rsi
	call allocateRegion@PLT
	movq $75,%rax
	leaq L_ret_funcall_XqPJRUZh3t3rFQU73qoEmq(%rip),%r9
	pushq %r9
	jmp F.fib2_loPAGCtpIVvYESaeJAMrrC@PLT
	.quad 0x0
	.quad 0xC
	.quad 0xB
	.quad 0x4
L_ret_funcall_XqPJRUZh3t3rFQU73qoEmq:
	leaq (%rsp),%rdx
	orq $3,%rdx
	movq D.cc772_lab64_Hah4K34pvN8ax68sSgBOfh@GOTPCREL(%rip),%rax
	movq (%rax),%rax
	movq %rdi,%rbx
	movq %rdx,%rdi
	leaq L_ret_funcall_ZDfpsys1VfGmay7Gf2TeKb(%rip),%r9
	pushq %r9
	jmp F.toString47_UVDoBkKNgRfMX21heoYNib@PLT
	.quad 0x0
	.quad 0xC
	.quad 0xB
	.quad 0x3
L_ret_funcall_ZDfpsys1VfGmay7Gf2TeKb:
	movq %rdi,%rsi
	movq D.StringLab6_lNdycfnDPQSuu7xb5HtQll@GOTPCREL(%rip),%rdx
	leaq 40(%rsp),%rdi
	orq $3,%rdi
	call concatStringML@PLT
	movq %rax,%rbx
	movq %r14,%rdi
	call deallocateRegion@PLT
	movq D.cc90_lab28_juAshEbDvp1tveDRFlf2tk@GOTPCREL(%rip),%rax
	movq (%rax),%rax
	leaq L_ret_funcall_jIIopHOZK92yQL0aJCRtzb(%rip),%r9
	pushq %r9
	jmp F.print17_KgGDQr00dX4vq3kGgCKc4j@PLT
	.quad 0x0
	.quad 0xC
	.quad 0xB
	.quad 0x2
L_ret_funcall_jIIopHOZK92yQL0aJCRtzb:
	movq %r14,%rdi
	call deallocateRegion@PLT
	leaq 88(%rsp),%rsp
	ret
	.p2align 0x4
L_gc_do_yvBCpeSn2OIyroobgF1Zpp:
	movq $0x0,%r11
	leaq L_gc_done_jLn7oPTgzyxVckZc3Bi8pc(%rip),%r10
	pushq $0
	pushq $0
	pushq $0
	jmp __gc_stub@PLT
mlkit_tp_end_main1_j24xhvTe9q1fKIKuKKcAEt:
F.fib2_loPAGCtpIVvYESaeJAMrrC:
	leaq -24(%rsp),%rsp
	movq %rax,16(%rsp)
	movq %rax,%r10
	movq $3,%r11
	cmpq %r11,%r10
	jge L_k5F_kSbodDYdePZ62flcgttkTf
	movq $3,%rdi
	leaq 24(%rsp),%rsp
	ret
	.p2align 0x4
L_k5F_kSbodDYdePZ62flcgttkTf:
	movq 16(%rsp),%rax
	subq $3,%rax
	jo __raise_overflow@PLT
	addq $1,%rax
	jo __raise_overflow@PLT
	leaq L_ret_funcall_CCRP8iCd04C6RLMxCILcjf(%rip),%r9
	pushq %r9
	jmp F.fib2_loPAGCtpIVvYESaeJAMrrC@PLT
	.quad 0x2
	.quad 0x4
	.quad 0x3
	.quad 0x1
L_ret_funcall_CCRP8iCd04C6RLMxCILcjf:
	movq %rdi,8(%rsp)
	movq 16(%rsp),%rax
	subq $5,%rax
	jo __raise_overflow@PLT
	addq $1,%rax
	jo __raise_overflow@PLT
	leaq L_ret_funcall_ecTikvluaUzIzPhU2gZfdj(%rip),%r9
	pushq %r9
	jmp F.fib2_loPAGCtpIVvYESaeJAMrrC@PLT
	.quad 0x4
	.quad 0x4
	.quad 0x3
	.quad 0x0
L_ret_funcall_ecTikvluaUzIzPhU2gZfdj:
	movq 8(%rsp),%r10
	movq %rdi,%r11
	sarq $1,%r11
	sarq $1,%r10
	addq %r10,%r11
	movq %r11,%rdi
	leaq 1(%rdi,%rdi),%rdi
	sarq $1,%rdi
	cmpq %rdi,%r11
	jne __raise_overflow@PLT
	leaq 1(%rdi,%rdi),%rdi
	leaq 24(%rsp),%rsp
	ret
mlkit_tp_end_fib2_loPAGCtpIVvYESaeJAMrrC:
.section 	.note.GNU-stack,"",@progbits
.data
.globl main1_j24xhvTe9q1fKIKuKKcAEt_data_begin
.type main1_j24xhvTe9q1fKIKuKKcAEt_data_begin,@object
main1_j24xhvTe9q1fKIKuKKcAEt_data_begin:
.data
	.p2align 3
.globl mlkit_rp_code_main1_j24xhvTe9q1fKIKuKKcAEt
.type mlkit_rp_code_main1_j24xhvTe9q1fKIKuKKcAEt,@object
mlkit_rp_code_main1_j24xhvTe9q1fKIKuKKcAEt:
	.quad 1
	.quad 2
	.quad F.main1_j24xhvTe9q1fKIKuKKcAEt
	.quad mlkit_tp_end_main1_j24xhvTe9q1fKIKuKKcAEt
	.quad D.StringLab3_Gzam6MfXNiD5HyTsmqIX4l
	.quad D.StringLab5_2OU6ge14NEGZb6DgQZ8iRk
	.quad D.StringLab1_ne4aslrYNModmyvQ61CXOq
	.quad D.StringLab2_7RaZlDtuYYABYtJJGXou4e
	.quad F.fib2_loPAGCtpIVvYESaeJAMrrC
	.quad mlkit_tp_end_fib2_loPAGCtpIVvYESaeJAMrrC
	.quad D.StringLab3_Gzam6MfXNiD5HyTsmqIX4l
	.quad D.StringLab4_JCTuZHsx6UQhqyjUPtk3am
	.quad D.StringLab1_ne4aslrYNModmyvQ61CXOq
	.quad D.StringLab2_7RaZlDtuYYABYtJJGXou4e
.data
	.align 8
D.StringLab6_lNdycfnDPQSuu7xb5HtQll:
	.quad 0X61
	.byte 10
	.byte 0
.data
	.align 8
D.StringLab5_2OU6ge14NEGZb6DgQZ8iRk:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 106
	.byte 50
	.byte 52
	.byte 120
	.byte 104
	.byte 118
	.byte 84
	.byte 101
	.byte 57
	.byte 113
	.byte 49
	.byte 102
	.byte 75
	.byte 73
	.byte 75
	.byte 117
	.byte 75
	.byte 75
	.byte 99
	.byte 65
	.byte 69
	.byte 116
	.byte 0
.data
	.align 8
D.StringLab4_JCTuZHsx6UQhqyjUPtk3am:
	.quad 0X6E1
	.byte 102
	.byte 105
	.byte 98
	.byte 50
	.byte 95
	.byte 108
	.byte 111
	.byte 80
	.byte 65
	.byte 71
	.byte 67
	.byte 116
	.byte 112
	.byte 73
	.byte 86
	.byte 118
	.byte 89
	.byte 69
	.byte 83
	.byte 97
	.byte 101
	.byte 74
	.byte 65
	.byte 77
	.byte 114
	.byte 114
	.byte 67
	.byte 0
.data
.data
.globl F.main1_j24xhvTe9q1fKIKuKKcAEt
.type F.main1_j24xhvTe9q1fKIKuKKcAEt,@function
.data
	.align 8
D.StringLab3_Gzam6MfXNiD5HyTsmqIX4l:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 106
	.byte 50
	.byte 52
	.byte 120
	.byte 104
	.byte 118
	.byte 84
	.byte 101
	.byte 57
	.byte 113
	.byte 49
	.byte 102
	.byte 75
	.byte 73
	.byte 75
	.byte 117
	.byte 75
	.byte 75
	.byte 99
	.byte 65
	.byte 69
	.byte 116
	.byte 0
.data
	.align 8
D.StringLab2_7RaZlDtuYYABYtJJGXou4e:
	.quad 0X821
	.byte 101
	.byte 98
	.byte 55
	.byte 100
	.byte 54
	.byte 56
	.byte 52
	.byte 55
	.byte 49
	.byte 51
	.byte 52
	.byte 54
	.byte 100
	.byte 55
	.byte 98
	.byte 51
	.byte 57
	.byte 101
	.byte 49
	.byte 53
	.byte 98
	.byte 101
	.byte 57
	.byte 100
	.byte 57
	.byte 98
	.byte 99
	.byte 48
	.byte 98
	.byte 52
	.byte 102
	.byte 55
	.byte 0
.data
	.align 8
D.StringLab1_ne4aslrYNModmyvQ61CXOq:
	.quad 0XE21
	.byte 47
	.byte 116
	.byte 109
	.byte 112
	.byte 47
	.byte 109
	.byte 108
	.byte 107
	.byte 105
	.byte 116
	.byte 45
	.byte 98
	.byte 101
	.byte 110
	.byte 99
	.byte 104
	.byte 45
	.byte 112
	.byte 114
	.byte 50
	.byte 53
	.byte 51
	.byte 46
	.byte 90
	.byte 52
	.byte 105
	.byte 119
	.byte 70
	.byte 66
	.byte 47
	.byte 97
	.byte 102
	.byte 116
	.byte 101
	.byte 114
	.byte 47
	.byte 98
	.byte 101
	.byte 110
	.byte 99
	.byte 104
	.byte 109
	.byte 97
	.byte 114
	.byte 107
	.byte 115
	.byte 47
	.byte 102
	.byte 105
	.byte 98
	.byte 51
	.byte 55
	.byte 46
	.byte 115
	.byte 109
	.byte 108
	.byte 0
.globl main1_j24xhvTe9q1fKIKuKKcAEt_data_end
.type main1_j24xhvTe9q1fKIKuKKcAEt_data_end,@object
main1_j24xhvTe9q1fKIKuKKcAEt_data_end:
.data
	.align 8
.globl main1_j24xhvTe9q1fKIKuKKcAEt_data_roots
.type main1_j24xhvTe9q1fKIKuKKcAEt_data_roots,@object
main1_j24xhvTe9q1fKIKuKKcAEt_data_roots:
	.quad 0
