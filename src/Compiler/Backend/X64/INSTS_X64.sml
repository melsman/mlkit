signature INSTS_X64 =
  sig

    type lvar

    datatype reg = rax | rbx | rcx | rdx | rsi | rdi | rbp | rsp
                 | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
                 | eax | ebx | ecx | edx | esi | edi | ebp | esp
                 | r8d | r9d | r10d | r11d | r12d | r13d | r14d | r15d
                 | ah (* for float conditionals *)
                 | al (* for byte operations *)
                 | cl (* for shift operations *)
                 | r10b (* for bytetable_update, e.g. *)
                 | xmm0 | xmm1 | xmm2 | xmm3
                 | xmm4 | xmm5 | xmm6 | xmm7
                 | xmm8 | xmm9 | xmm10 | xmm11
                 | xmm12 | xmm13 | xmm14 | xmm15
                 | ymm0 | ymm1 | ymm2 | ymm3
                 | ymm4 | ymm5 | ymm6 | ymm7
                 | ymm8 | ymm9 | ymm10 | ymm11
                 | ymm12 | ymm13 | ymm14 | ymm15

    val pr_reg : reg -> string
    val is_xmm : reg -> bool

    val tmp_reg0 : reg (*=r10*)
    val tmp_reg1 : reg (*=r11*)

    val tmp_freg0 : reg (*=xmm0*)
    val tmp_freg1 : reg (*=xmm1*)

    val doubleOfQuadReg : reg -> reg (* fails if given a non-quad register *)

    type freg

    type label
    datatype lab =
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        jumps to millicode, code label, finish
			        label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

    val eq_lab : lab * lab -> bool

    datatype ea = R of reg   (* register *)
                | L of lab   (* label *)
                | LA of lab  (* label address *)
                | I of string   (* immediate *)
                | D of string * reg   (* displaced *)
                | DD of string * reg * reg * string (* double displaced *)
    val pr_ea : ea -> string
    val eq_ea : ea * ea -> bool

    datatype inst =                 (* general instructions *)
      movq of ea * ea
    | mov of ea * ea                (* e.g. for zero extension for moving 32-bit values into 64-bit registers *)
    | movb of ea * ea
    | movzbq of ea * ea
    | movslq of ea * ea
    | cmove of ea * ea (* conditional move *)
    | cmovne of ea * ea  (* conditional move *)
    | push of ea
    | leaq of ea * ea
    | pop of ea
    | andb of ea * ea

    | addl of ea * ea   (* LONG OPERATIONS (32 bit) *)
    | subl of ea * ea
    | negl of ea
    | decl of ea
    | incl of ea
    | imull of ea * ea
    | notl of ea
    | orl of ea * ea
    | xorl of ea * ea
    | andl of ea * ea
    | sarl of ea * ea
    | shrl of ea * ea   (* unsigned *)
    | sall of ea * ea
    | cmpl of ea * ea
    | btl of ea * ea    (* bit test; sets carry flag *)
    | btrl of ea * ea   (* bit test and reset; sets carry flag *)

    | addq of ea * ea   (* QUAD OPERATIONS (64 bit) *)
    | subq of ea * ea
    | negq of ea
    | decq of ea
    | incq of ea
    | imulq of ea * ea
    | notq of ea
    | orq of ea * ea
    | xorq of ea * ea
    | andq of ea * ea
    | sarq of ea * ea
    | shrq of ea * ea   (* unsigned *)
    | salq of ea * ea
    | cmpq of ea * ea
    | testq of ea * ea
    | btq of ea * ea    (* bit test; sets carry flag *)
    | btrq of ea * ea   (* bit test and reset; sets carry flag *)
    | cmpxchgq of ea * ea
    | xaddq of ea * ea

    | movsd of ea * ea  (* FLOAT OPERATIONS *)
    | mulsd of ea * ea
    | divsd of ea * ea
    | addsd of ea * ea
    | subsd of ea * ea
    | maxsd of ea * ea
    | minsd of ea * ea
    | ucomisd of ea * ea
    | xorps of ea * ea
    | sqrtsd of ea * ea
    | cvtsi2sdq of ea * ea

    | vmovupd of ea * ea

    | vaddpd of ea * ea * ea (* AVX OPERATIONS *)
    | vaddpd_128 of ea * ea * ea (* 128 bit version *)
    | vaddsd of ea * ea * ea (* 64 bit version *)

    | vsubpd of ea * ea * ea

    | vmulpd of ea * ea * ea
    | vmulpd_128 of ea * ea * ea (* 128 bit version *)
    | vmulsd of ea * ea * ea (* 64 bit version *)

    | vdivpd of ea * ea * ea

    | vandpd of ea * ea * ea
    | vorpd of ea * ea * ea

    | vbroadcastsd of ea * ea
    | vblendvpd of ea * ea * ea * ea (* conditional move based on mask *)
    | vcmppd of ea * ea * ea * ea (* compare and make mask *)
    | vmovmskpd of ea * ea (* extract mask *)
    | vpcmpeqd of ea * ea * ea (* equality of packed vector, useful for generating all ones *)
    | vpxor of ea * ea * ea (* xor of packed vector, useful for generating all zeroes *)
    | vextractf128 of ea * ea * ea (* extract 128 bits of a 256 bit register *)

    | vunpckhpd of ea * ea * ea (* unpack and interleave *)
    | vunpckhpd_128 of ea * ea * ea (* unpack and interleave, 128 bit version *)

    | fstpq of ea       (* store float and pop float stack *)
    | fldq of ea        (* push float onto the float stack *)
    | fldz              (* push 0.0 onto the float stack *)
    | faddp             (* add st(0) to st(1) and pop *)
    | fsubp             (* subtract st(0) from st(1) and pop *)
    | fmulp             (* multiply st(0) to st(1) and pop *)
    | fdivp             (* divide st(1) with st(0) and pop *)
    | fcompp            (* compare st(0) and st(1) and pop twice *)
    | fabs              (* st(0) = abs(st(0)) *)
    | fchs              (* st(0) = neg(st(0)) *)
    | fnstsw            (* store float status word *)

    | jmp of ea         (* jump instructions *)
    | jl of lab
    | jg of lab
    | jle of lab
    | jge of lab
    | je of lab         (* = jz *)
    | jne of lab        (* = jnz *)
    | jc of lab         (* jump on carry *)
    | jnc of lab        (* jump on non-carry *)
    | ja of lab         (* jump if above---unsigned *)
    | jb of lab         (* jump if below---unsigned *)
    | jae of lab        (* jump if above or equal---unsigned *)
    | jbe of lab        (* jump if below or equal---unsigned *)
    | jo of lab         (* jump on overflow *)

    | cmovlq of ea * ea
    | cmovgq of ea * ea
    | cmovleq of ea * ea
    | cmovgeq of ea * ea
    | cmoveq of ea * ea
    | cmovneq of ea * ea
    | cmovaq of ea * ea
    | cmovbq of ea * ea
    | cmovaeq of ea * ea
    | cmovbeq of ea * ea

    | call of lab       (* C function calls and returns *)
    | call' of ea       (* C function calls and returns *)
    | ret
    | leave

    | dot_align of int      (* pseudo instructions *)
    | dot_p2align of string
    | dot_globl of lab
    | dot_text
    | dot_data
    | dot_section of string
    | dot_byte of string
    | dot_long of string
    | dot_quad of string
    | dot_quad' of lab
    | dot_double of string
    | dot_string of string
    | dot_size of lab * int
    | lab of lab
    | comment of string

    datatype top_decl =
        FUN of label * inst list
      | FN of label * inst list

    type AsmPrg = {top_decls: top_decl list,
		   init_code: inst list,
		   static_data: inst list}

    (* General purpose registers *)

    val emit : AsmPrg * string -> unit   (* may raise IO *)

    val pr_lab : lab -> string

    structure RI : REGISTER_INFO
      where type reg = reg
      where type lvar = lvar

    val sysname : unit -> string

    val rem_dead_code : inst list -> inst list

    val optimise : AsmPrg -> AsmPrg

    type StringTree
    val layout : AsmPrg -> StringTree

  end
