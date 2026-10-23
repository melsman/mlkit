.text
F.main1_BkcXQAlujjTtXHs1oHqkJm:
	leaq -8(%rsp),%rsp
	xorq %rbx,%rbx
	movq $200000000,%rax
	call F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei@PLT
	movq %rdi,%rbx
	movq %rbx,(%rsp)
	movq $20000000100000000,%r11
	cmpq %r11,%rbx
	jne L_k1F_aiRdndKqZNlx6NBBeqKYEA
	movq D.StringLab8_Ie9chc3J4ECA6pFfSMFS1B@GOTPCREL(%rip),%rdi
	jmp L_done_J1bbEfi9iEMj2VGRDfFSXg
	.p2align 0x4
L_k1F_aiRdndKqZNlx6NBBeqKYEA:
	movq D.StringLab7_2RpjVehhSiu3MnoOczmfTo@GOTPCREL(%rip),%rdi
	.p2align 0x4
L_done_J1bbEfi9iEMj2VGRDfFSXg:
	call printStringML@PLT
	movq (%rsp),%rbx
	movq D.result_lab4_cdQt8KUkKePP5KqsNjl1zd@GOTPCREL(%rip),%r11
	movq %rbx,(%r11)
	leaq 8(%rsp),%rsp
	ret
mlkit_tp_end_main1_BkcXQAlujjTtXHs1oHqkJm:
F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei:
	leaq -8(%rsp),%rsp
	movq %rbx,%rdi
	movq %rax,%rbx
	movq %rbx,(%rsp)
	cmpq $0,%rbx
	jne L_newSelLab_kTxjB3Nr3NzgCfqxSAr1em
	leaq 8(%rsp),%rsp
	ret
	.p2align 0x4
L_newSelLab_kTxjB3Nr3NzgCfqxSAr1em:
	call preservation_identity@PLT
	movq (%rsp),%rbx
	movq %rbx,%rdx
	subq $1,%rdx
	jo __raise_overflow@PLT
	addq %rax,%rbx
	jo __raise_overflow@PLT
	movq %rdx,%rax
	leaq 8(%rsp),%rsp
	jmp F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei@PLT
mlkit_tp_end_loop__noinline3_xmZQ4DTPuTAEg91aKyunei:
F._2_xONxaTfVKYnVtycMJRCoTb:
	movq (%rsp),%rdx
	btq $0,%rdx
	jnc L_no_alloc_BTlhmIqHTLVCmcXebrIM6u
	movq %rdx,%r11
	andq $-4,%r11
	pushq %r11
	movq (%r11),%r11
	addq $-1,%r11
	movq %r11,%r10
	orq $0x1FFF,%r10
	addq $16,%r11
	cmpq %r10,%r11
	jg L_expand_GIejUJkx5zmOeQXBJkeNid
	leaq 1(%r11),%r10
	popq %r11
	movq %r10,(%r11)
	leaq -16(%r10),%r11
	.p2align 0x4
L_ret_alloc_aM2NyMVej8drL7lz5ObTAr:
	movq %r11,%rdx
	.p2align 0x4
L_no_alloc_BTlhmIqHTLVCmcXebrIM6u:
	movq %rbx,(%rdx)
	movq %rdi,8(%rdx)
	movq %rdx,%rbx
	movq (%rax),%r11
	leaq 8(%rsp),%rsp
	jmp *%r11
	.p2align 0x4
L_expand_GIejUJkx5zmOeQXBJkeNid:
	popq %r11
	leaq L_ret_alloc_aM2NyMVej8drL7lz5ObTAr(%rip),%r10
	pushq %r10
	movq $2,%r10
	jmp __allocate@PLT
mlkit_tp_end__2_xONxaTfVKYnVtycMJRCoTb:
.section 	.note.GNU-stack,"",@progbits
.data
.data
	.p2align 3
.globl mlkit_rp_code_main1_BkcXQAlujjTtXHs1oHqkJm
.type mlkit_rp_code_main1_BkcXQAlujjTtXHs1oHqkJm,@object
mlkit_rp_code_main1_BkcXQAlujjTtXHs1oHqkJm:
	.quad 1
	.quad 3
	.quad F.main1_BkcXQAlujjTtXHs1oHqkJm
	.quad mlkit_tp_end_main1_BkcXQAlujjTtXHs1oHqkJm
	.quad D.StringLab3_sgJv07SHYim4oukztUBith
	.quad D.StringLab6_oRsnkEEfcdsqaX1vYfCbZx
	.quad D.StringLab1_gVHcvtvUg0RhQsFkoDHbQz
	.quad D.StringLab2_yT1QmpLcdNGgNzElXY2uKd
	.quad F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei
	.quad mlkit_tp_end_loop__noinline3_xmZQ4DTPuTAEg91aKyunei
	.quad D.StringLab3_sgJv07SHYim4oukztUBith
	.quad D.StringLab5_m4pLvSPDB8DZTjcCiaUlGc
	.quad D.StringLab1_gVHcvtvUg0RhQsFkoDHbQz
	.quad D.StringLab2_yT1QmpLcdNGgNzElXY2uKd
	.quad F._2_xONxaTfVKYnVtycMJRCoTb
	.quad mlkit_tp_end__2_xONxaTfVKYnVtycMJRCoTb
	.quad D.StringLab3_sgJv07SHYim4oukztUBith
	.quad D.StringLab4_Z0m0Y0kb97Bs2eHV5LRR2i
	.quad D.StringLab1_gVHcvtvUg0RhQsFkoDHbQz
	.quad D.StringLab2_yT1QmpLcdNGgNzElXY2uKd
.data
	.align 8
D.StringLab8_Ie9chc3J4ECA6pFfSMFS1B:
	.quad 0XE1
	.byte 79
	.byte 75
	.byte 10
	.byte 0
.data
	.align 8
D.StringLab7_2RpjVehhSiu3MnoOczmfTo:
	.quad 0X121
	.byte 66
	.byte 65
	.byte 68
	.byte 10
	.byte 0
.data
	.align 8
D.result_lab4_cdQt8KUkKePP5KqsNjl1zd:
	.quad 1
.data
	.align 8
D.StringLab6_oRsnkEEfcdsqaX1vYfCbZx:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 66
	.byte 107
	.byte 99
	.byte 88
	.byte 81
	.byte 65
	.byte 108
	.byte 117
	.byte 106
	.byte 106
	.byte 84
	.byte 116
	.byte 88
	.byte 72
	.byte 115
	.byte 49
	.byte 111
	.byte 72
	.byte 113
	.byte 107
	.byte 74
	.byte 109
	.byte 0
.data
	.align 8
D.StringLab5_m4pLvSPDB8DZTjcCiaUlGc:
	.quad 0X9A1
	.byte 108
	.byte 111
	.byte 111
	.byte 112
	.byte 95
	.byte 95
	.byte 110
	.byte 111
	.byte 105
	.byte 110
	.byte 108
	.byte 105
	.byte 110
	.byte 101
	.byte 51
	.byte 95
	.byte 120
	.byte 109
	.byte 90
	.byte 81
	.byte 52
	.byte 68
	.byte 84
	.byte 80
	.byte 117
	.byte 84
	.byte 65
	.byte 69
	.byte 103
	.byte 57
	.byte 49
	.byte 97
	.byte 75
	.byte 121
	.byte 117
	.byte 110
	.byte 101
	.byte 105
	.byte 0
.data
	.align 8
D.StringLab4_Z0m0Y0kb97Bs2eHV5LRR2i:
	.quad 0X661
	.byte 61
	.byte 50
	.byte 95
	.byte 120
	.byte 79
	.byte 78
	.byte 120
	.byte 97
	.byte 84
	.byte 102
	.byte 86
	.byte 75
	.byte 89
	.byte 110
	.byte 86
	.byte 116
	.byte 121
	.byte 99
	.byte 77
	.byte 74
	.byte 82
	.byte 67
	.byte 111
	.byte 84
	.byte 98
	.byte 0
.data
.globl D.result_lab4_cdQt8KUkKePP5KqsNjl1zd
.type D.result_lab4_cdQt8KUkKePP5KqsNjl1zd,@object
.data
.globl F.main1_BkcXQAlujjTtXHs1oHqkJm
.type F.main1_BkcXQAlujjTtXHs1oHqkJm,@function
.globl F._2_xONxaTfVKYnVtycMJRCoTb
.type F._2_xONxaTfVKYnVtycMJRCoTb,@function
.globl F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei
.type F.loop__noinline3_xmZQ4DTPuTAEg91aKyunei,@function
.data
	.align 8
D.StringLab3_sgJv07SHYim4oukztUBith:
	.quad 0X721
	.byte 109
	.byte 97
	.byte 105
	.byte 110
	.byte 49
	.byte 95
	.byte 66
	.byte 107
	.byte 99
	.byte 88
	.byte 81
	.byte 65
	.byte 108
	.byte 117
	.byte 106
	.byte 106
	.byte 84
	.byte 116
	.byte 88
	.byte 72
	.byte 115
	.byte 49
	.byte 111
	.byte 72
	.byte 113
	.byte 107
	.byte 74
	.byte 109
	.byte 0
.data
	.align 8
D.StringLab2_yT1QmpLcdNGgNzElXY2uKd:
	.quad 0X821
	.byte 97
	.byte 57
	.byte 48
	.byte 98
	.byte 51
	.byte 50
	.byte 56
	.byte 101
	.byte 99
	.byte 56
	.byte 53
	.byte 53
	.byte 52
	.byte 49
	.byte 55
	.byte 51
	.byte 102
	.byte 57
	.byte 98
	.byte 55
	.byte 54
	.byte 98
	.byte 98
	.byte 100
	.byte 49
	.byte 99
	.byte 101
	.byte 98
	.byte 48
	.byte 53
	.byte 52
	.byte 56
	.byte 0
.data
	.align 8
D.StringLab1_gVHcvtvUg0RhQsFkoDHbQz:
	.quad 0XBE1
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
	.byte 99
	.byte 45
	.byte 112
	.byte 114
	.byte 101
	.byte 115
	.byte 101
	.byte 114
	.byte 118
	.byte 97
	.byte 116
	.byte 105
	.byte 111
	.byte 110
	.byte 45
	.byte 98
	.byte 101
	.byte 110
	.byte 99
	.byte 104
	.byte 46
	.byte 51
	.byte 116
	.byte 100
	.byte 103
	.byte 70
	.byte 71
	.byte 47
	.byte 108
	.byte 111
	.byte 111
	.byte 112
	.byte 46
	.byte 115
	.byte 109
	.byte 108
	.byte 0
