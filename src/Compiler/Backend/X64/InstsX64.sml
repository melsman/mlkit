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
                 | xmm0 | xmm1

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
    | movb of ea * ea
    | movzbq of ea * ea
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

    | movsd of ea * ea
    | mulsd of ea * ea
    | divsd of ea * ea
    | addsd of ea * ea
    | subsd of ea * ea
    | maxsd of ea * ea
    | ucomisd of ea * ea
    | xorps of ea * ea

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
    | dot_globl of lab
    | dot_text
    | dot_data
    | dot_section of string
    | dot_byte of string
    | dot_long of string
    | dot_quad of string
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

    fun remove_ctrl s =
        String.implode (List.filter (fn c =>
                                     Char.isAlphaNum c orelse
                                     c = #"_" orelse c = #".") (String.explode s))

    fun pr_namelab s =
        if sysname() = "Darwin" then "_" ^ s
        else s

    fun pr_lab (DatLab l) = "DLab." ^ remove_ctrl(Labels.pr_label l)
      | pr_lab (LocalLab l) = ".LLab." ^ remove_ctrl(Labels.pr_label l)
      | pr_lab (NameLab s) = (* "NLab." ^ *)  pr_namelab(remove_ctrl s)
      | pr_lab (MLFunLab l) = "FLab." ^ remove_ctrl(Labels.pr_label l)

    (* Convert ~n to -n *)
    fun int_to_string i = if i >= 0 then Int.toString i
                          else "-" ^ Int.toString (~i)

    fun pr_ea (R r) = pr_reg r
      | pr_ea (L l) = pr_lab l ^ "(%rip)"
      | pr_ea (LA l) = pr_lab l ^ "@GOTPCREL(%rip)"
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
          fun emit s = TextIO.output(os, s)
          fun emit_n i = emit(Int.toString i)
          fun emit_nl() = emit "\n"
          fun emit_bin (s, (ea1, ea2)) = (emit "\t"; emit s; emit " ";
                                          emit(pr_ea ea1); emit ",";
                                          emit(pr_ea ea2); emit_nl())
          fun emit_unary(s, ea) = (emit "\t"; emit s; emit " "; emit(pr_ea ea); emit_nl())
          fun emit_nullary s = (emit "\t"; emit s; emit_nl())
          fun emit_nullary0 s = (emit s; emit_nl())
          fun emit_jump(s,l) = (emit "\t"; emit s; emit " "; emit(pr_lab l); emit_nl())
          fun emit_inst i =
            case i
              of movq a => emit_bin ("movq", a)
               | movb a => emit_bin ("movb", a)
               | movzbq a => emit_bin ("movzbq", a)
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

               | movsd a => emit_bin("movsd", a)
               | mulsd a => emit_bin("mulsd", a)
               | divsd a => emit_bin("divsd", a)
               | addsd a => emit_bin("addsd", a)
               | subsd a => emit_bin("subsd", a)
               | maxsd a => emit_bin("maxsd", a)
               | ucomisd a => emit_bin("ucomisd", a)
               | xorps a => emit_bin("xorps", a)

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
               | dot_globl l => (emit ".globl "; emit(pr_lab l); emit_nl())
               | dot_text => emit_nullary0 ".text"
               | dot_data => emit_nullary0 ".data"
               | dot_byte s => (emit "\t.byte "; emit s; emit_nl())
               | dot_long s => (emit "\t.long "; emit s; emit_nl())
               | dot_quad s => (emit "\t.quad "; emit s; emit_nl())
               | dot_double s => (emit "\t.double "; emit s; emit_nl())
               | dot_string s => (emit "\t.string \""; emit s; emit "\""; emit_nl())
               | dot_section s => (emit ".section \t"; emit s; emit_nl())
               | dot_size (l, i) => (emit "\t.size "; emit(pr_lab l); emit ",";
                                     emit_n i; emit_nl())
               | lab l => (emit(pr_lab l); emit":"; emit_nl())
               | comment s => (emit " # "; emit s; emit_nl())
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
        val section =
            if sysname() = "Darwin" then ".note.GNU-stack,\"\""
            else ".note.GNU-stack,\"\",@progbits"
        val static_data = dot_section section :: static_data
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
        val reg_lvs = map (fn r => Lvars.new_named_lvar (pr_reg r)) regs
        val (rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
             r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv) =
            case reg_lvs of
                [rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
                 r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv] =>
                (rax_lv,rbx_lv,rcx_lv,rdx_lv,rsi_lv,rdi_lv,rbp_lv,rsp_lv,
                 r8_lv,r9_lv,r10_lv,r11_lv,r12_lv,r13_lv,r14_lv,r15_lv)
            | _ => die "RI.reg_lvs mismatch"
        val map_lvs_to_reg = LvarFinMap.fromList(ListPair.zip(reg_lvs,regs))

        val all_regs = reg_lvs

        fun is_reg lv =
          (case LvarFinMap.lookup map_lvs_to_reg lv of
             SOME reg => true
           | NONE  => false)

        fun lv_to_reg lv =
          (case LvarFinMap.lookup map_lvs_to_reg lv of
             NONE => die "lv_to_reg: lv not a register"
           | SOME i => i)

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
        val res_reg_ccall = [rax]
        val res_phreg_ccall = map reg_to_lv res_reg_ccall

        fun reg_eq(reg1,reg2) = reg1 = reg2
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

    fun doubleOfQuadReg r =
        case r of
            rax => eax | rbx => ebx | rcx => ecx | rdx => edx
            | rsi => esi | rdi => edi | rbp => ebp | rsp => esp
            | r8 => r8d | r9 => r9d | r10 => r10d | r11 => r11d
            | r12 => r12d | r13 => r13d | r14 => r14d | r15 => r15d
            | _ => die ("doubleOfQuadReg: " ^ pr_reg r ^ " is not a quad register")

    type StringTree = PP.StringTree
    fun layout _ = PP.LEAF "not implemented"
  end
