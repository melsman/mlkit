signature INSTS_X86 =
  sig
    
    eqtype reg
    type freg

    type label

    datatype ea = R of reg   (* register *)
                | I of int   (* immediate *)
                | L of label
                | D of int * reg   (* displaced *)

    datatype inst =                 (* general instructions *)
      movl of ea * ea
    | pushl of ea
    | addl of ea * ea
    | subl of ea * ea
    | orl of ea * ea
    | andl of ea * ea
    | sarl of ea * ea
    | shrl of ea * ea  (* unsigned *)    
    | sall of ea * ea
    | cmpl of ea * ea

    | jmp of label     (* jump instructions *)
    | jl of label
    | jg of label
    | jle of label        
    | jge of label
    | je of label         (* = jz *)
    | jne of label        (* = jnz *)

    | call of label    (* C function calls and returns *)
    | ret
    | leave

    | dot_align of int        (* pseudo instructions *)
    | dot_globl of label
    | dot_text
    | dot_section of string
    | lab of label

    datatype top_decl =
        FUN of label * inst list
      | FN of label * inst list

    type AsmPrg = {top_decls: top_decl list,
		   init_code: inst list,
		   exit_code: inst list,
		   static_data: inst list}


    val eax : reg        (* General purpose registers *)
    val ebx : reg
    val ecx : reg
    val edx : reg
    val esi : reg
    val edi : reg
    val ebp : reg
    val esp : reg

    val emit : TextIO.outstream * AsmPrg -> unit   (* may raise IO *)

    val pr_reg : reg -> string

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)
    type lvar
    val is_reg                  : lvar -> bool
    val lv_to_reg               : lvar -> reg
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