.text
F.main1_5uadoJ85Qg0Ti5ECxO7eYo:
	movq time_to_gc@GOTPCREL(%rip),%r9
	cmpq $1,(%r9)
	je L_gc_do_InqaTCCKdmi3kj92onEGgj
	.p2align 0x4
L_gc_done_MXhdalcSjZpLtMTvr71lml:
	leaq -104(%rsp),%rsp
	leaq 56(%rsp),%r11
	movq %r14,%rdi
	movq %r11,%rsi
	call allocateRegion@PLT
	leaq 8(%rsp),%r11
	movq %r14,%rdi
	movq %r11,%rsi
	call allocateRegion@PLT
	movq $75,%rax
	leaq L_ret_funcall_VL6x2HgXLDmB49nMDULVOb(%rip),%r9
	pushq %r9
	jmp F.fib2_cpRH0SSemiunXFCOyEbTim@PLT
	.quad 0x0
	.quad 0xE
	.quad 0xD
	.quad 0x4
L_ret_funcall_VL6x2HgXLDmB49nMDULVOb:
	leaq 8(%rsp),%rdx
	orq $3,%rdx
	movq D.cc772_lab64_Hah4K34pvN8ax68sSgBOfh@GOTPCREL(%rip),%rax
	movq (%rax),%rax
	movq %rdi,%rbx
	movq %rdx,%rdi
	leaq L_ret_funcall_foHlpxZuHz8UOLloSm1yEj(%rip),%r9
	pushq %r9
	jmp F.toString47_UVDoBkKNgRfMX21heoYNib@PLT
	.quad 0x0
	.quad 0xE
	.quad 0xD
	.quad 0x3
L_ret_funcall_foHlpxZuHz8UOLloSm1yEj:
	movq %rdi,%rsi
	movq D.StringLab6_1Y4fe9XGbAF7SBIrtDR0Tz@GOTPCREL(%rip),%rdx
	leaq 56(%rsp),%rdi
	orq $3,%rdi
	call concatStringML@PLT
	movq %rax,%rbx
	movq %rbx,48(%rsp)
	movq %r14,%rdi
	call deallocateRegion@PLT
	movq 48(%rsp),%rbx
	movq D.cc90_lab28_juAshEbDvp1tveDRFlf2tk@GOTPCREL(%rip),%rax
	movq (%rax),%rax
	leaq L_ret_funcall_hgpe9aRqf7hTojqQolfriz(%rip),%r9
	pushq %r9
	jmp F.print17_KgGDQr00dX4vq3kGgCKc4j@PLT
	.quad 0x0
	.quad 0xE
	.quad 0xD
	.quad 0x2
L_ret_funcall_hgpe9aRqf7hTojqQolfriz:
	movq %r14,%rdi
	call deallocateRegion@PLT
	leaq 104(%rsp),%rsp
	ret
	.p2align 0x4
L_gc_do_InqaTCCKdmi3kj92onEGgj:
	movq $0x0,%r11
	leaq L_gc_done_MXhdalcSjZpLtMTvr71lml(%rip),%r10
	pushq $0
	pushq $0
	pushq $0
	jmp __gc_stub@PLT
mlkit_tp_end_main1_5uadoJ85Qg0Ti5ECxO7eYo:
F.fib2_cpRH0SSemiunXFCOyEbTim:
	leaq -24(%rsp),%rsp
	movq %rax,16(%rsp)
	movq %rax,%r10
	movq $3,%r11
	cmpq %r11,%r10
	jge L_k5F_kTHq3U3yeLG3O9IKGcJ4zs
	movq $3,%rdi
	leaq 24(%rsp),%rsp
	ret
	.p2align 0x4
L_k5F_kTHq3U3yeLG3O9IKGcJ4zs:
	movq 16(%rsp),%rax
	subq $3,%rax
	jo __raise_overflow@PLT
	addq $1,%rax
	jo __raise_overflow@PLT
	leaq L_ret_funcall_7EoOjQFsOUz7LVY5q0oe6c(%rip),%r9
	pushq %r9
	jmp F.fib2_cpRH0SSemiunXFCOyEbTim@PLT
	.quad 0x2
	.quad 0x4
	.quad 0x3
	.quad 0x1
L_ret_funcall_7EoOjQFsOUz7LVY5q0oe6c:
	movq %rdi,8(%rsp)
	movq 16(%rsp),%rax
	subq $5,%rax
	jo __raise_overflow@PLT
	addq $1,%rax
	jo __raise_overflow@PLT
	leaq L_ret_funcall_cYsmN2NS0flOLtYAfEfQHc(%rip),%r9
	pushq %r9
	jmp F.fib2_cpRH0SSemiunXFCOyEbTim@PLT
	.quad 0x4
	.quad 0x4
	.quad 0x3
	.quad 0x0
L_ret_funcall_cYsmN2NS0flOLtYAfEfQHc:
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
mlkit_tp_end_fib2_cpRH0SSemiunXFCOyEbTim:
.section 	.note.GNU-stack,"",@progbits
.data
.globl main1_5uadoJ85Qg0Ti5ECxO7eYo_data_begin
.type main1_5uadoJ85Qg0Ti5ECxO7eYo_data_begin,@object
main1_5uadoJ85Qg0Ti5ECxO7eYo_data_begin:
.data
	.p2align 3
.globl mlkit_rp_code_main1_5uadoJ85Qg0Ti5ECxO7eYo
.type mlkit_rp_code_main1_5uadoJ85Qg0Ti5ECxO7eYo,@object
mlkit_rp_code_main1_5uadoJ85Qg0Ti5ECxO7eYo:
	.quad 1
	.quad 2
	.quad F.main1_5uadoJ85Qg0Ti5ECxO7eYo
	.quad mlkit_tp_end_main1_5uadoJ85Qg0Ti5ECxO7eYo
	.quad D.StringLab3_7PdJfQYctdUqA8dBZvZl6h
	.quad D.StringLab5_ilQNkiZkiEiOzrZeEKvB7q
	.quad D.StringLab1_qAdHZHAVly5gVSRr3aZLac
	.quad D.StringLab2_rxNojJxDqMJeHf5NNLJCfp
	.quad F.fib2_cpRH0SSemiunXFCOyEbTim
	.quad mlkit_tp_end_fib2_cpRH0SSemiunXFCOyEbTim
	.quad D.StringLab3_7PdJfQYctdUqA8dBZvZl6h
	.quad D.StringLab4_loPfMkuyduK6pVV7UhNkog
	.quad D.StringLab1_qAdHZHAVly5gVSRr3aZLac
	.quad D.StringLab2_rxNojJxDqMJeHf5NNLJCfp
.data
	.align 8
D.StringLab6_1Y4fe9XGbAF7SBIrtDR0Tz:
	.quad 0X61
	.byte 10
	.byte 0
.data
	.align 8
D.StringLab5_ilQNkiZkiEiOzrZeEKvB7q:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 53
	.byte 117
	.byte 97
	.byte 100
	.byte 111
	.byte 74
	.byte 56
	.byte 53
	.byte 81
	.byte 103
	.byte 48
	.byte 84
	.byte 105
	.byte 53
	.byte 69
	.byte 67
	.byte 120
	.byte 79
	.byte 55
	.byte 101
	.byte 89
	.byte 111
	.byte 0
.data
	.align 8
D.StringLab4_loPfMkuyduK6pVV7UhNkog:
	.quad 0X6E1
	.byte 102
	.byte 105
	.byte 98
	.byte 50
	.byte 95
	.byte 99
	.byte 112
	.byte 82
	.byte 72
	.byte 48
	.byte 83
	.byte 83
	.byte 101
	.byte 109
	.byte 105
	.byte 117
	.byte 110
	.byte 88
	.byte 70
	.byte 67
	.byte 79
	.byte 121
	.byte 69
	.byte 98
	.byte 84
	.byte 105
	.byte 109
	.byte 0
.data
.data
.globl F.main1_5uadoJ85Qg0Ti5ECxO7eYo
.type F.main1_5uadoJ85Qg0Ti5ECxO7eYo,@function
.data
	.align 8
D.StringLab3_7PdJfQYctdUqA8dBZvZl6h:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 53
	.byte 117
	.byte 97
	.byte 100
	.byte 111
	.byte 74
	.byte 56
	.byte 53
	.byte 81
	.byte 103
	.byte 48
	.byte 84
	.byte 105
	.byte 53
	.byte 69
	.byte 67
	.byte 120
	.byte 79
	.byte 55
	.byte 101
	.byte 89
	.byte 111
	.byte 0
.data
	.align 8
D.StringLab2_rxNojJxDqMJeHf5NNLJCfp:
	.quad 0X821
	.byte 102
	.byte 100
	.byte 51
	.byte 52
	.byte 99
	.byte 98
	.byte 53
	.byte 100
	.byte 97
	.byte 52
	.byte 51
	.byte 51
	.byte 100
	.byte 56
	.byte 100
	.byte 55
	.byte 100
	.byte 50
	.byte 54
	.byte 52
	.byte 53
	.byte 51
	.byte 98
	.byte 48
	.byte 52
	.byte 102
	.byte 56
	.byte 51
	.byte 53
	.byte 49
	.byte 99
	.byte 56
	.byte 0
.data
	.align 8
D.StringLab1_qAdHZHAVly5gVSRr3aZLac:
	.quad 0XE61
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
	.byte 98
	.byte 101
	.byte 102
	.byte 111
	.byte 114
	.byte 101
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
.globl main1_5uadoJ85Qg0Ti5ECxO7eYo_data_end
.type main1_5uadoJ85Qg0Ti5ECxO7eYo_data_end,@object
main1_5uadoJ85Qg0Ti5ECxO7eYo_data_end:
.data
	.align 8
.globl main1_5uadoJ85Qg0Ti5ECxO7eYo_data_roots
.type main1_5uadoJ85Qg0Ti5ECxO7eYo_data_roots,@object
main1_5uadoJ85Qg0Ti5ECxO7eYo_data_roots:
	.quad 0
