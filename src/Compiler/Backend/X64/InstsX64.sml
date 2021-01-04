structure InstsX64: INSTS_X64 =
  struct
    structure PP = PrettyPrint
    structure Labels = AddressLabels

    fun die s = Crash.impossible("InstX64." ^ s)

    fun memoize f =
        let val r = ref NONE
        in fn () => case !r of SOME v => v
                             | NONE => let val v = f()
                                       in r:=SOME v; v
                                       end
        end

    val sysname =
        memoize (fn () =>
                    case List.find (fn (f,_) => f = "sysname") (Posix.ProcEnv.uname()) of
                        SOME (_, name) => name
                      | NONE => "unknown"
                )

    type lvar = Lvars.lvar
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

    type freg = int

    type label = Labels.label
    datatype lab =
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
                                jumps to millicode, code label, finish
                                label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

    fun eq_lab (DatLab label1, DatLab label2) = Labels.eq(label1,label2)
      | eq_lab (LocalLab label1, LocalLab label2) = Labels.eq(label1,label2)
      | eq_lab (NameLab s1, NameLab s2) = s1 = s2
      | eq_lab (MLFunLab label1, MLFunLab label2) = Labels.eq(label1,label2)
      | eq_lab _ = false

    datatype ea =
        R of reg          (* register *)
      | L of lab          (* label *)
      | LA of lab         (* label address *)
      | I of string       (* immediate *)
      | D of string * reg (* displaced *)
      | DD of string * reg * reg * string (* double displaced *)

    fun eq_ea (R r, R r') = r=r'
      | eq_ea (I i, I i') = i=i'
      | eq_ea (L l, L l') = eq_lab(l,l')
      | eq_ea (LA l, LA l') = eq_lab(l,l')
      | eq_ea (D p,D p') = p=p'
      | eq_ea (DD p,DD p') = p=p'
      | eq_ea _ = false

    datatype inst =                 (* general instructions *)
      movq of ea * ea
    | mov of ea * ea
    | movb of ea * ea
    | movzbq of ea * ea
    | movslq of ea * ea
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
    | btq of ea * ea    (* bit test; sets carry flag *)
    | btrq of ea * ea   (* bit test and reset; sets carry flag *)
    | cmpxchgq of ea * ea

    | movsd of ea * ea
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

    | call of lab       (* C function calls and returns *)
    | call' of ea       (* C function calls and returns *)
    | ret
    | leave

    | dot_align of int  (* pseudo instructions *)
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

    fun pr_reg rax = "%rax"
      | pr_reg rbx = "%rbx"
      | pr_reg rcx = "%rcx"
      | pr_reg rdx = "%rdx"
      | pr_reg rsi = "%rsi"
      | pr_reg rdi = "%rdi"
      | pr_reg rbp = "%rbp"
      | pr_reg rsp = "%rsp"
      | pr_reg r8 = "%r8"
      | pr_reg r9 = "%r9"
      | pr_reg r10 = "%r10"
      | pr_reg r11 = "%r11"
      | pr_reg r12 = "%r12"
      | pr_reg r13 = "%r13"
      | pr_reg r14 = "%r14"
      | pr_reg r15 = "%r15"
      | pr_reg eax = "%eax"
      | pr_reg ebx = "%ebx"
      | pr_reg ecx = "%ecx"
      | pr_reg edx = "%edx"
      | pr_reg esi = "%esi"
      | pr_reg edi = "%edi"
      | pr_reg ebp = "%ebp"
      | pr_reg esp = "%esp"
      | pr_reg r8d = "%r8d"
      | pr_reg r9d = "%r9d"
      | pr_reg r10d = "%r10d"
      | pr_reg r11d = "%r11d"
      | pr_reg r12d = "%r12d"
      | pr_reg r13d = "%r13d"
      | pr_reg r14d = "%r14d"
      | pr_reg r15d = "%r15d"
      | pr_reg ah = "%ah"
      | pr_reg al = "%al"
      | pr_reg cl = "%cl"
      | pr_reg r10b = "%r10b"
      | pr_reg xmm0 = "%xmm0"
      | pr_reg xmm1 = "%xmm1"
      | pr_reg xmm2 = "%xmm2"
      | pr_reg xmm3 = "%xmm3"
      | pr_reg xmm4 = "%xmm4"
      | pr_reg xmm5 = "%xmm5"
      | pr_reg xmm6 = "%xmm6"
      | pr_reg xmm7 = "%xmm7"
      | pr_reg xmm8 = "%xmm8"
      | pr_reg xmm9 = "%xmm9"
      | pr_reg xmm10 = "%xmm10"
      | pr_reg xmm11 = "%xmm11"
      | pr_reg xmm12 = "%xmm12"
      | pr_reg xmm13 = "%xmm13"
      | pr_reg xmm14 = "%xmm14"
      | pr_reg xmm15 = "%xmm15"

    fun is_xmm (r:reg) =
        case r of
            xmm0 => true
          | xmm1 => true
          | xmm2 => true
          | xmm3 => true
          | xmm4 => true
          | xmm5 => true
          | xmm6 => true
          | xmm7 => true
          | xmm8 => true
          | xmm9 => true
          | xmm10 => true
          | xmm11 => true
          | xmm12 => true
          | xmm13 => true
          | xmm14 => true
          | xmm15 => true
          | _ => false

    fun remove_ctrl s =
        String.implode (List.filter (fn c =>
                                     Char.isAlphaNum c orelse
                                     c = #"_" orelse c = #".") (String.explode s))

    fun pr_namelab s =
        if sysname() = "Darwin" then "_" ^ s
        else s

    fun pr_lab (DatLab l) = "DLab." ^ remove_ctrl(Labels.pr_label l)
      | pr_lab (LocalLab l) = ".LLab." ^ remove_ctrl(Labels.pr_label l)
      | pr_lab (NameLab s) = pr_namelab(remove_ctrl s)
      | pr_lab (MLFunLab l) = "FLab." ^ remove_ctrl(Labels.pr_label l)

    (* Convert ~n to -n *)
    fun int_to_string i = if i >= 0 then Int.toString i
                          else "-" ^ Int.toString (~i)

    fun pr_ea (R r) = pr_reg r
      | pr_ea (L l) = pr_lab l ^ "(%rip)"
      | pr_ea (LA l) =
      	if sysname() = "Darwin" then
	  pr_lab l ^ "@GOTPCREL(%rip)"
	else "$" ^ pr_lab l
      | pr_ea (I s) = "$" ^ s
      | pr_ea (D(d,r)) = if d="0" then "(" ^ pr_reg r ^ ")"
                         else d ^ "(" ^ pr_reg r ^ ")"
      | pr_ea (DD(d,r1,r2,m)) =
        let val m = if m = "1" orelse m = "" then "" else "," ^ m
            val d = if d = "0" orelse d = "" then "" else d
        in d ^ "(" ^ pr_reg r1 ^ "," ^ pr_reg r2 ^ m ^ ")"
        end

    fun emit_insts (os, insts: inst list): unit =
      let
          val linecomments = true
          fun emit s = TextIO.output(os, s)
          val (set_comm : string -> unit, emit_comm : unit -> unit) =
              let val comm : string option ref = ref NONE
              in (fn c => comm := SOME c,
                  fn () => case !comm of
                               SOME c => (emit ("\t\t\t" ^ c); comm := NONE)
                             | NONE => ())
              end
          fun emit_n i = emit(Int.toString i)
          fun emit_nl () = (emit_comm(); emit "\n")
          fun emit_bin (s, (ea1, ea2)) = (emit "\t"; emit s; emit " ";
                                          emit(pr_ea ea1); emit ",";
                                          emit(pr_ea ea2); emit_nl())
          fun emit_unary (s, ea) = (emit "\t"; emit s; emit " "; emit(pr_ea ea); emit_nl())
          fun emit_nullary s = (emit "\t"; emit s; emit_nl())
          fun emit_nullary0 s = (emit s; emit_nl())
          fun emit_jump (s,l) = (emit "\t"; emit s; emit " "; emit(pr_lab l); emit_nl())
          fun emit_inst i =
             case i of
                 movq a => emit_bin ("movq", a)
               | mov a => emit_bin ("mov", a)
               | movb a => emit_bin ("movb", a)
               | movzbq a => emit_bin ("movzbq", a)
               | movslq a => emit_bin ("movslq", a)
               | leaq a => emit_bin ("leaq", a)
               | push ea => emit_unary ("push", ea)
               | pop ea => emit_unary ("pop", ea)
               | andb a => emit_bin("andb", a)

               | addl a => emit_bin("addl", a)
               | subl a => emit_bin("subl", a)
               | negl ea => emit_unary("negl", ea)
               | decl ea => emit_unary("decl", ea)
               | incl ea => emit_unary("incl", ea)
               | imull a => emit_bin("imull", a)
               | notl ea => emit_unary("notl", ea)
               | orl a => emit_bin("orl", a)
               | xorl a => emit_bin("xorl", a)
               | andl a => emit_bin("andl", a)
               | sarl a => emit_bin("sarl", a)
               | shrl a => emit_bin("shrl", a)
               | sall a => emit_bin("sall", a)
               | cmpl a => emit_bin("cmpl", a)
               | btl a => emit_bin("btl", a)
               | btrl a => emit_bin("btrl", a)

               | addq a => emit_bin("addq", a)
               | subq a => emit_bin("subq", a)
               | negq ea => emit_unary("negq", ea)
               | decq ea => emit_unary("decq", ea)
               | incq ea => emit_unary("incq", ea)
               | imulq a => emit_bin("imulq", a)
               | notq ea => emit_unary("notq", ea)
               | orq a => emit_bin("orq", a)
               | xorq a => emit_bin("xorq", a)
               | andq a => emit_bin("andq", a)
               | sarq a => emit_bin("sarq", a)
               | shrq a => emit_bin("shrq", a)
               | salq a => emit_bin("salq", a)
               | cmpq a => emit_bin("cmpq", a)
               | btq a => emit_bin("btq", a)
               | btrq a => emit_bin("btrq", a)
               | cmpxchgq a => emit_bin("lock cmpxchgq", a)

               | movsd a => emit_bin("movsd", a)
               | mulsd a => emit_bin("mulsd", a)
               | divsd a => emit_bin("divsd", a)
               | addsd a => emit_bin("addsd", a)
               | subsd a => emit_bin("subsd", a)
               | maxsd a => emit_bin("maxsd", a)
               | minsd a => emit_bin("minsd", a)
               | ucomisd a => emit_bin("ucomisd", a)
               | xorps a => emit_bin("xorps", a)
               | sqrtsd a => emit_bin("sqrtsd", a)
               | cvtsi2sdq a => emit_bin("cvtsi2sdq", a)

               | fstpq ea => emit_unary("fstpq", ea)
               | fldq ea => emit_unary("fldq", ea)
               | fldz => emit_nullary "fldz"
               | faddp => emit_nullary "faddp"
               | fsubp => emit_nullary "fsubp"
               | fmulp => emit_nullary "fmulp"
               | fdivp => emit_nullary "fdivp"
               | fcompp=> emit_nullary "fcompp"
               | fabs => emit_nullary "fabs"
               | fchs => emit_nullary "fchs"
               | fnstsw => emit_nullary "fnstsw"

               | jmp (L l) => emit_jump("jmp", l)
               | jmp ea => (emit "\tjmp *"; emit(pr_ea ea); emit_nl())
               | jl l => emit_jump("jl", l)
               | jg l => emit_jump("jg", l)
               | jle l => emit_jump("jle", l)
               | jge l => emit_jump("jge", l)
               | je l => emit_jump("je", l)
               | jne l => emit_jump("jne", l)
               | jc l => emit_jump("jc", l)
               | jnc l => emit_jump("jnc", l)
               | ja l => emit_jump("ja", l)
               | jb l => emit_jump("jb", l)
               | jae l => emit_jump("jae", l)
               | jbe l => emit_jump("jbe", l)
               | jo l => emit_jump("jo", l)

               | call l => emit_jump("call", l)
               | call' ea => (emit "\tcall *"; emit(pr_ea ea); emit_nl())
               | ret => emit_nullary "ret"
               | leave => emit_nullary "leave"

               | dot_align i => (emit "\t.align "; emit_n i; emit_nl())
               | dot_p2align s => (emit "\t.p2align "; emit s; emit_nl())
               | dot_globl l => (emit ".globl "; emit(pr_lab l); emit_nl())
               | dot_text => emit_nullary0 ".text"
               | dot_data => emit_nullary0 ".data"
               | dot_byte s => (emit "\t.byte "; emit s; emit_nl())
               | dot_long s => (emit "\t.long "; emit s; emit_nl())
               | dot_quad s => (emit "\t.quad "; emit s; emit_nl())
               | dot_quad' l => (emit "\t.quad "; emit(pr_lab l); emit_nl())
               | dot_double s => (emit "\t.double "; emit s; emit_nl())
               | dot_string s => (emit "\t.string \""; emit s; emit "\""; emit_nl())
               | dot_section s => (emit ".section \t"; emit s; emit_nl())
               | dot_size (l, i) => (emit "\t.size "; emit(pr_lab l); emit ",";
                                     emit_n i; emit_nl())
               | lab l => (emit(pr_lab l); emit":"; emit_nl())
               | comment s => if linecomments then
                                set_comm (" # " ^ s)
                              else (emit " # "; emit s; emit_nl())
      in app emit_inst insts
      end

    fun emit_topdecl os t =
      case t
        of FUN (l, insts) => emit_insts(os, lab (MLFunLab l)::insts)
         | FN (l, insts) =>  emit_insts(os, lab (MLFunLab l)::insts)

    fun emit ({top_decls: top_decl list,
               init_code: inst list,
               static_data: inst list}, filename) =
      let
        val os : TextIO.outstream = TextIO.openOut filename
        val static_data =
            if sysname() = "Darwin" then
              (* dot_section ".note.GNU-stack,\"\"" :: *) static_data
            else dot_section ".note.GNU-stack,\"\",@progbits" :: static_data
      in (emit_insts (os, init_code);
          app (emit_topdecl os) top_decls;
          emit_insts (os, static_data);
          TextIO.closeOut os) handle E => (TextIO.closeOut os; raise E)
      end

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)

    structure RI : REGISTER_INFO =
      struct
        type lvar = lvar
        type lvarset = Lvarset.lvarset
        type reg = reg

        val pr_reg = pr_reg

        structure LvarFinMap = Lvars.Map

        val regs = [rax,rbx,rcx,rdx,rsi,rdi,rbp,rsp,r8,r9,r10,r11,r12,r13,r14,r15]
        val lvs_with_phregs = map (fn r => (Lvars.new_named_lvar (pr_reg r),r)) regs
        val all_regs = map #1 lvs_with_phregs

        val (rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
             r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv) =
            case all_regs of
                [rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
                 r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv] =>
                (rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
                 r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv)
            | _ => die "RI.all_regs mismatch"

        val xmm_regs = [xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,
                        xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15]

        val f64_lvs_with_phregs = map (fn r => (Lvars.new_named_lvar (pr_reg r),r)) xmm_regs
        val f64_phregs =
            case map #1 f64_lvs_with_phregs of
                _ :: _ :: rest => rest
              | _ => die "f64_phregs.impossible"

        val f64_phregset = Lvarset.lvarsetof f64_phregs

        val map_lvs_to_reg =
            LvarFinMap.fromList(lvs_with_phregs @ f64_lvs_with_phregs)

        fun is_reg lv =
            case LvarFinMap.lookup map_lvs_to_reg lv of
                SOME reg => true
              | NONE  => false

        fun lv_to_reg lv =
            case LvarFinMap.lookup map_lvs_to_reg lv of
                SOME r => r
              | NONE => die "lv_to_reg: lv not a register"

        fun reg_to_lv r =
            case r of
                rax => rax_lv | rbx => rbx_lv | rcx => rcx_lv | rdx => rdx_lv
              | rsi => rsi_lv | rdi => rdi_lv | rbp => rbp_lv | rsp => rsp_lv
              | r8 => r8_lv | r9 => r9_lv | r10 => r10_lv | r11 => r11_lv
              | r12 => r12_lv | r13 => r13_lv | r14 => r14_lv | r15 => r15_lv
              | _ => die ("reg_to_lv: " ^ pr_reg r ^ " not available for register allocation")

        val reg_args = [rax,rbx,rdi]
        val args_phreg = map reg_to_lv reg_args
        val reg_res = [rdi,rbx,rax]
        val res_phreg = map reg_to_lv reg_res

        val args_reg_ccall = [rdi,rsi,rdx,rcx,r8,r9]
        val args_phreg_ccall = map reg_to_lv args_reg_ccall
        val args_ccall_phregset = Lvarset.lvarsetof args_phreg_ccall
        val res_reg_ccall = [rax]
        val res_phreg_ccall = map reg_to_lv res_reg_ccall

        fun reg_eq (reg1,reg2) = reg1 = reg2
        val callee_save_regs_ccall = [rbx,rbp,r12,r13,r14,r15]
        val callee_save_ccall_phregs = map reg_to_lv callee_save_regs_ccall
        val callee_save_ccall_phregset = Lvarset.lvarsetof callee_save_ccall_phregs
        fun is_callee_save_ccall phreg = false

        val caller_save_regs_mlkit = [rax,rbx,rdi,rdx,rsi]
        val caller_save_phregs = map reg_to_lv caller_save_regs_mlkit
        val caller_save_phregset = Lvarset.lvarsetof caller_save_phregs
        fun is_caller_save phreg = Lvarset.member(phreg,caller_save_phregset)

        val caller_save_regs_ccall = [] (*[r10,r11]*)
        val caller_save_ccall_phregs   = map reg_to_lv caller_save_regs_ccall
        val caller_save_ccall_phregset = Lvarset.lvarsetof caller_save_ccall_phregs
        fun is_caller_save_ccall phreg = Lvarset.member(phreg,caller_save_ccall_phregset)
      end

    val tmp_reg0 = r10 (* CALLER saves scratch registers *)
    val tmp_reg1 = r11

    val tmp_freg0 = xmm0
    val tmp_freg1 = xmm1

    fun doubleOfQuadReg r =
        case r of
            rax => eax | rbx => ebx | rcx => ecx | rdx => edx
            | rsi => esi | rdi => edi | rbp => ebp | rsp => esp
            | r8 => r8d | r9 => r9d | r10 => r10d | r11 => r11d
            | r12 => r12d | r13 => r13d | r14 => r14d | r15 => r15d
            | _ => die ("doubleOfQuadReg: " ^ pr_reg r ^ " is not a quad register")

    (* Helper functions *)

    fun rem_dead_code nil = nil
      | rem_dead_code (C as i :: C') =
        case i of
            lab _ => C
          | dot_long _ => C
          | dot_quad _ => C
          | dot_quad' _ => C
          | dot_byte _ => C
          | dot_align _ => C
          | dot_p2align _ => C
          | dot_globl _ => C
          | dot_text => C
          | dot_data => C
          | comment s => i :: rem_dead_code C'
          | _ => rem_dead_code C'

    fun onI (Rm:reg->reg) (Lm:lab->lab) (i:inst) : inst =
        let fun Em ea =
                case ea of
                    R r => R(Rm r)
                  | L l => L (Lm l)
                  | LA l => LA (Lm l)
                  | I s => ea
                  | D (s,r) => D(s,Rm r)
                  | DD (s1,r1,r2,s2) => DD(s1,Rm r1, Rm r2,s2)
        in case i of
               movq (ea1,ea2) => movq (Em ea1,Em ea2)
             | mov (ea1,ea2) => mov (Em ea1,Em ea2)
             | movb (ea1,ea2) => movb (Em ea1,Em ea2)
             | movzbq (ea1,ea2) => movzbq (Em ea1,Em ea2)
             | movslq (ea1,ea2) => movslq (Em ea1,Em ea2)
             | push ea => push (Em ea)
             | leaq (ea1,ea2) => leaq (Em ea1,Em ea2)
             | pop ea => pop (Em ea)
             | andb (ea1,ea2) => andb (Em ea1,Em ea2)
             | addl (ea1,ea2) => addl (Em ea1,Em ea2)
             | subl (ea1,ea2) => subl (Em ea1,Em ea2)
             | negl ea => negl (Em ea)
             | decl ea => decl (Em ea)
             | incl ea => incl (Em ea)
             | imull (ea1,ea2) => imull (Em ea1,Em ea2)
             | notl ea => notl (Em ea)
             | orl (ea1,ea2) => orl (Em ea1,Em ea2)
             | xorl (ea1,ea2) => xorl (Em ea1,Em ea2)
             | andl (ea1,ea2) => andl (Em ea1,Em ea2)
             | sarl (ea1,ea2) => sarl (Em ea1,Em ea2)
             | shrl (ea1,ea2) => shrl (Em ea1,Em ea2)
             | sall (ea1,ea2) => sall (Em ea1,Em ea2)
             | cmpl (ea1,ea2) => cmpl (Em ea1,Em ea2)
             | btl (ea1,ea2) => btl (Em ea1,Em ea2)
             | btrl (ea1,ea2) => btrl (Em ea1,Em ea2)
             | addq (ea1,ea2) => addq (Em ea1,Em ea2)
             | subq (ea1,ea2) => subq (Em ea1,Em ea2)
             | negq ea => negq (Em ea)
             | decq ea => decq (Em ea)
             | incq ea => incq (Em ea)
             | imulq (ea1,ea2) => imulq (Em ea1,Em ea2)
             | notq ea => notq (Em ea)
             | orq (ea1,ea2) => orq (Em ea1,Em ea2)
             | xorq (ea1,ea2) => xorq (Em ea1,Em ea2)
             | andq (ea1,ea2) => andq (Em ea1,Em ea2)
             | sarq (ea1,ea2) => sarq (Em ea1,Em ea2)
             | shrq (ea1,ea2) => shrq (Em ea1,Em ea2)
             | salq (ea1,ea2) => salq (Em ea1,Em ea2)
             | cmpq (ea1,ea2) => cmpq (Em ea1,Em ea2)
             | btq (ea1,ea2) => btq (Em ea1,Em ea2)
             | btrq (ea1,ea2) => btrq (Em ea1,Em ea2)
             | cmpxchgq (ea1,ea2) => cmpxchgq (Em ea1,Em ea2)
             | movsd (ea1,ea2) => movsd (Em ea1,Em ea2)
             | mulsd (ea1,ea2) => mulsd (Em ea1,Em ea2)
             | divsd (ea1,ea2) => divsd (Em ea1,Em ea2)
             | addsd (ea1,ea2) => addsd (Em ea1,Em ea2)
             | subsd (ea1,ea2) => subsd (Em ea1,Em ea2)
             | maxsd (ea1,ea2) => maxsd (Em ea1,Em ea2)
             | minsd (ea1,ea2) => minsd (Em ea1,Em ea2)
             | ucomisd (ea1,ea2) => ucomisd (Em ea1,Em ea2)
             | xorps (ea1,ea2) => xorps (Em ea1,Em ea2)
             | sqrtsd (ea1,ea2) => sqrtsd (Em ea1,Em ea2)
             | cvtsi2sdq (ea1,ea2) => cvtsi2sdq (Em ea1,Em ea2)
             | fstpq ea => fstpq (Em ea)
             | fldq ea => fldq (Em ea)
             | jmp ea => jmp (Em ea)
             | jl l => jl (Lm l)
             | jg l => jg (Lm l)
             | jle l => jle (Lm l)
             | jge l => jge (Lm l)
             | je l => je (Lm l)
             | jne l => jne (Lm l)
             | jc l => jc (Lm l)
             | jnc l => jnc (Lm l)
             | ja l => ja (Lm l)
             | jb l => jb (Lm l)
             | jae l => jae (Lm l)
             | jbe l => jbe (Lm l)
             | jo l => jo (Lm l)
             | call l => call (Lm l)
             | call' ea => call' (Em ea)
             | dot_globl l => dot_globl (Lm l)
             | dot_size (l, i) => dot_size (Lm l, i)
             | lab l => lab (Lm l)
             | fldz => i
             | faddp => i
             | fsubp => i
             | fmulp => i
             | fdivp => i
             | fcompp => i
             | fabs => i
             | fchs => i
             | fnstsw => i
             | ret => i
             | leave => i
             | dot_align n => i
             | dot_p2align s => i
             | dot_text => i
             | dot_data => i
             | dot_section s => i
             | dot_byte s => i
             | dot_long s => i
             | dot_quad s => i
             | dot_quad' l => dot_quad' (Lm l)
             | dot_double s => i
             | dot_string s => i
             | comment s => i
        end

    (* various peephole optimisations *)

    structure IM = IntFinMap
    type im = (label*label) IM.map
    fun im_add (l1:label) (l2:label) (im:im) : im =
        IM.add(#1(Labels.key l1),(l1,l2),im)
    fun im_look (im:im) (l:label) : label option =
        case IM.lookup im (#1(Labels.key l)) of
            SOME (_,l) => SOME l
          | NONE => NONE

    fun labs_used_insts (is:inst list) : label IM.map =
        let val im : label IM.map ref = ref IM.empty
            fun add (l:label) : unit =
                im := IM.add(#1(Labels.key l),l,!im)
            fun addlab l =
                case l of
                    LocalLab l => add l
                  | DatLab l => add l
                  | MLFunLab l => add l
                  | _ => ()
            fun on (lab _) = ()
              | on (i:inst) : unit = ignore (onI (fn r => r) (fn l => (addlab l; l)) i)
        in app on is; !im
        end

    fun elim_jmp_jmp is =
        let
            (* make sure to preserve .quad-prefixes to lab-instructions, as
             * these are required for the garbage collector *)
            fun labs (im:im) is =
                case is of
                    nil => im
                  | (i as lab (LocalLab l1)) :: lab (LocalLab l2) :: is => labs (im_add l2 l1 im) (i :: is)
                  | (i as dot_quad q) :: (i' as lab (LocalLab l1)) :: lab (LocalLab l2) :: is => labs (im_add l2 l1 im) (i :: i' :: is)
                  | dot_quad _ :: lab _ :: is => labs im is
                  | lab (LocalLab l1) :: jmp (L(LocalLab l2)) :: is => labs (im_add l1 l2 im) is
                  | _ :: is => labs im is
            fun close im l =
                case im_look im l of
                    SOME l' => if Labels.eq(l,l') then l
                               else close im l'
                  | NONE => l
            fun closure im = IM.composemap (fn (l,l') => (l,close im l')) im
            val im = closure(labs IM.empty is)
            fun Lm (LocalLab l) = (case im_look im l of
                                       SOME l' => LocalLab l'
                                     | NONE => LocalLab l)
              | Lm l = l
            val S : inst -> inst = onI (fn r => r) Lm
            fun loop (nil,acc) = rev acc
              | loop ((i as lab (LocalLab l))::is,acc) =
                (case im_look im l of
                     SOME _ => loop(is,acc)
                   | NONE => loop(is,i::acc))
              | loop (i::is,acc) = loop (is,S i::acc)
        in loop (is,nil)
        end

    fun simpl_jmp (is,acc) =
        let fun lab_eq (LocalLab l1, LocalLab l2) = Labels.eq(l1,l2)
              | lab_eq _ = false
            fun opt (c:lab->inst) (nc:lab->inst) (l1:lab) (l2:lab) (l3:lab) (is:inst list) : inst list =
                simpl_jmp(is,
                          if lab_eq(l1,l3) then lab l3 :: nc l2 :: acc
                          else lab l3 :: jmp (L l2) :: c l1 :: acc)
        in case is of
               nil => rev acc
             | jg l1 :: jmp (L l2) :: lab l3 :: is => opt jg jle l1 l2 l3 is
             | jge l1 :: jmp (L l2) :: lab l3 :: is => opt jge jl l1 l2 l3 is
             | jl l1 :: jmp (L l2) :: lab l3 :: is => opt jl jge l1 l2 l3 is
             | jle l1 :: jmp (L l2) :: lab l3 :: is => opt jle jg l1 l2 l3 is
             | ja l1 :: jmp (L l2) :: lab l3 :: is => opt ja jbe l1 l2 l3 is
             | jae l1 :: jmp (L l2) :: lab l3 :: is => opt jae jb l1 l2 l3 is
             | jb l1 :: jmp (L l2) :: lab l3 :: is => opt jb jae l1 l2 l3 is
             | jbe l1 :: jmp (L l2) :: lab l3 :: is => opt jbe ja l1 l2 l3 is
             | je l1 :: jmp (L l2) :: lab l3 :: is => opt je jne l1 l2 l3 is
             | jne l1 :: jmp (L l2) :: lab l3 :: is => opt jne je l1 l2 l3 is
             | i :: is => simpl_jmp (is,i::acc)
        end

    fun peep is =
        let fun p (is,acc) =
                case is of
                    nil => rev acc
                  | (i1 as movq(I "1",R r1)) :: (i2 as movq(I "1",R r2)) :: is =>
                    if r1 = r2 then p (i2 :: is,acc)
                    else p (i2 :: is, i1::acc)
                  | (i as jmp (L(LocalLab l1))) :: is =>
                    (case rem_dead_code is of
                         is as (lab (LocalLab l2) :: is') =>
                         if Labels.eq(l1,l2) then p (is,acc)
                         else p (is,i::acc)
                       | is => p (is,i::acc))
                  | i::is => p (is,i::acc)
            val is = p (is,nil)
            val is = simpl_jmp(is,nil)
            val used_labs = labs_used_insts is
            fun rem_unused_labs (nil,acc) = rev acc
              | rem_unused_labs (i::is,acc) =
                case i of
                    lab (LocalLab l) =>
                    rem_unused_labs(is,
                                    case IM.lookup used_labs (#1(Labels.key l)) of
                                        SOME _ => i::acc
                                      | NONE => acc)
                  | _ => rem_unused_labs(is,i::acc)
            val is = rem_unused_labs(is,nil)
        in is
        end

    fun alignlabs is =
        let fun loop (is,acc) =
                case is of
                    nil => rev acc
                  | (i1 as dot_quad _)::(i2 as lab _)::is => loop(is,i2::i1::acc)
                  | (i as lab (LocalLab l))::is => loop(is,i::dot_p2align "0x4"::acc)
                  | i::is => loop(is,i::acc)
        in loop (is,nil)
        end

    (* optimise: e.g., eliminate jmp-to-jmps, etc. *)
    fun optimise {top_decls: top_decl list,
                  init_code: inst list,
                  static_data: inst list} =
        let fun opt is = alignlabs(peep(elim_jmp_jmp is))
            fun onT t =
                case t of FUN (l, insts) => FUN (l, opt insts)
                        | FN (l, insts) => FN(l, opt insts)
        in {top_decls=map onT top_decls,
            init_code=init_code,
            static_data=static_data}
        end

    type StringTree = PP.StringTree
    fun layout _ = PP.LEAF "not implemented"
  end
