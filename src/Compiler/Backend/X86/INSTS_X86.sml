signature INSTS_X86 =
  sig
    
    datatype reg = eax | ebx | ecx | edx | esi | edi | ebp | esp 
                 | ah (* for float conditionals *)
                 | cl (* for shift operations *)

    val tmp_reg0 : reg (*=ecx*)
    val tmp_reg1 : reg (*=ebp*)

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
                | D of int * reg   (* displaced *)

    val pr_ea : ea -> string
    val eq_ea : ea * ea -> bool

    datatype inst =                 (* general instructions *)
      movl of ea * ea
    | pushl of ea
    | popl of ea
    | addl of ea * ea
    | subl of ea * ea
    | negl of ea
    | imull of ea * ea
    | notl of ea
    | orl of ea * ea
    | xorl of ea * ea
    | andl of ea * ea
    | andb of ea * ea
    | sarl of ea * ea
    | shrl of ea * ea   (* unsigned *)    
    | sall of ea * ea
    | cmpl of ea * ea
    | btl of ea * ea    (* bit test; sets carry flag *)
    | btrl of ea * ea   (* bit test and reset; sets carry flag *)

    | fstpl of ea       (* store float and pop float stack *)
    | fldl of ea        (* push float onto the float stack *) 
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

    | call of lab       (* C function calls and returns *)
    | ret
    | leave

    | dot_align of int  (* pseudo instructions *)
    | dot_globl of lab
    | dot_text
    | dot_data
    | dot_byte of string
    | dot_long of string
    | dot_double of string
    | dot_string of string
    | dot_size of lab * int
    | dot_section of string
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

    val pr_reg : reg -> string
    val pr_lab : lab -> string

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)
    type lvar
    val is_reg                  : lvar -> bool
    val lv_to_reg               : lvar -> reg
    val all_regs_as_lvs         : lvar list
    val reg_args_as_lvs         : lvar list
    val reg_res_as_lvs          : lvar list
    val reg_args_ccall_as_lvs   : lvar list
    val reg_res_ccall_as_lvs    : lvar list
    val callee_save_regs_mlkit_as_lvs : lvar list
    val caller_save_regs_mlkit_as_lvs : lvar list
    val callee_save_regs_ccall_as_lvs : lvar list
    val caller_save_regs_ccall_as_lvs : lvar list

    type StringTree
    val layout : AsmPrg -> StringTree

  end