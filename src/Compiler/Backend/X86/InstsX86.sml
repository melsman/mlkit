functor InstsX86(structure Labels : ADDRESS_LABELS
		 structure Lvars : LVARS
		 structure Crash : CRASH
		 structure PP : PRETTYPRINT) : INSTS_X86 =
  struct

    fun die s = Crash.impossible("X86Inst." ^ s)
    
    type reg = int
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
        R of reg   (* register *)
      | I of int   (* immediate *)
      | L of lab
      | D of int * reg     (* displaced *)

    fun eq_ea (R r, R r') = r=r'
      | eq_ea (I i, I i') = i=i'
      | eq_ea (L l, L l') = eq_lab(l,l')
      | eq_ea (D p,D p') = p=p'
      | eq_ea _ = false
      
    datatype inst =               (* general instructions *)
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

      | jmp of lab     (* jump instructions *)
      | jl of lab
      | jg of lab
      | jle of lab        
      | jge of lab
      | je of lab         (* = jz *)
      | jne of lab        (* = jnz *)

      | call of lab    (* C function calls and returns *)
      | ret
      | leave

      | dot_align of int        (* pseudo instructions *)
      | dot_globl of lab
      | dot_text
      | dot_section of string
      | lab of lab
      | comment of string

    datatype top_decl =
        FUN of label * inst list
      | FN of label * inst list

    type AsmPrg = {top_decls: top_decl list,
		   init_code: inst list,
		   exit_code: inst list,
		   static_data: inst list}

    val eax : reg = 0        (* General purpose registers *)
    val ebx : reg = 1
    val ecx : reg = 2
    val edx : reg = 3
    val esi : reg = 4
    val edi : reg = 5
    val ebp : reg = 6
    val esp : reg = 7

    fun pr_reg 0 = "%eax"
      | pr_reg 1 = "%ebx"
      | pr_reg 2 = "%ecx"
      | pr_reg 3 = "%edx"
      | pr_reg 4 = "%esi"
      | pr_reg 5 = "%edi"
      | pr_reg 6 = "%ebp"
      | pr_reg 7 = "%esp"
      | pr_reg _ = die "pr_phreg. no such register"

    fun pr_lab (DatLab l) = Labels.pr_label l
      | pr_lab (LocalLab l) = Labels.pr_label l
      | pr_lab (NameLab s) = s
      | pr_lab (MLFunLab l) = Labels.pr_label l 

    fun pr_ea (R r) = pr_reg r
      | pr_ea (I i) = "$" ^ Int.toString i
      | pr_ea (L l) = pr_lab l
      | pr_ea (D(i,r)) = if i=0 then pr_reg r
			 else Int.toString i ^ "(" ^ pr_reg r ^ ")"

    fun emit_insts (os, insts: inst list): unit = 
      let fun emit s = TextIO.output(os, s ^ "\n")
	  fun emit_inst i =  
	    case i
	      of movl (ea1, ea2) => emit("\tmovl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | pushl ea => emit("\tpushl " ^ pr_ea ea)
	       | addl (ea1,ea2) => emit("\taddl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | subl (ea1,ea2) => emit("\tsubl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | orl (ea1, ea2) => emit("\torl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | andl (ea1, ea2) => emit("\tandl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | sarl (ea1, ea2) => emit("\tsarl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | shrl (ea1, ea2) => emit("\tshrl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)    
	       | sall (ea1, ea2) => emit("\tsall " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)
	       | cmpl (ea1, ea2) => emit("\tcmpl " ^ pr_ea ea1 ^ "," ^ pr_ea ea2)

	       | jmp l => emit ("\tjmp " ^ pr_lab l)
	       | jl l => emit ("\tjl " ^ pr_lab l)
	       | jg l => emit ("\tjg " ^ pr_lab l)
	       | jle l => emit ("\tjle " ^ pr_lab l)
	       | jge l => emit ("\tjge " ^ pr_lab l)
	       | je l => emit ("\tje " ^ pr_lab l)
	       | jne l => emit ("\tjne " ^ pr_lab l)

	       | call l => emit("\tcall " ^ pr_lab l)
	       | ret => emit ("\tret")
	       | leave => emit ("\tleave")
	       | dot_align i => emit(".align " ^ Int.toString i)
	       | dot_globl l => emit(".globl " ^ pr_lab l)
	       | dot_text => emit(".text")
	       | dot_section s => emit(".section " ^ s)
	       | lab l => emit(pr_lab l ^ ":")
	       | comment s => emit ("; " ^ s) 
      in app emit_inst insts
      end

    fun emit_topdecl os t =
      case t
	of FUN (l, insts) => emit_insts(os, lab (MLFunLab l)::insts)
	 | FN (l, insts) =>  emit_insts(os, lab (MLFunLab l)::insts)

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)
    type lvar = Lvars.lvar
    local
      structure LvarFinMap = Lvars.Map

      val regs = [0,1,2,3,4,5,6,7]
      val reg_lvs = map (fn i => Lvars.new_named_lvar ("ph"^Int.toString i)) regs
      val map_lvs_to_reg = LvarFinMap.fromList(ListPair.zip(reg_lvs,regs))
      val map_reg_to_lvs = Vector.fromList reg_lvs
    in
      val all_regs_as_lvs = reg_lvs 

      fun is_reg lv = 
	(case LvarFinMap.lookup map_lvs_to_reg lv of
	   SOME reg => true
	 | NONE  => false)

      fun lv_to_reg lv = 
	(case LvarFinMap.lookup map_lvs_to_reg lv of
	   NONE => die "lv_to_phreg: lv not a register"
	 | SOME i => i)

      fun reg_to_lv(i) = Vector.sub(map_reg_to_lvs,i)

      val reg_args = [0,1,2]
      val reg_args_as_lvs = map reg_to_lv reg_args
      val reg_res = [2,1,0] 
      val reg_res_as_lvs = map reg_to_lv reg_res

      val reg_args_ccall = []
      val reg_args_ccall_as_lvs = map reg_to_lv reg_args_ccall
      val reg_res_ccall = [1] 
      val reg_res_ccall_as_lvs = map reg_to_lv reg_res_ccall

      val callee_save_regs_mlkit = []
      val callee_save_regs_mlkit_as_lvs = map reg_to_lv callee_save_regs_mlkit

      val caller_save_regs_mlkit = [0,1,2,3,4,5,6]
      val caller_save_regs_mlkit_as_lvs = map reg_to_lv caller_save_regs_mlkit

      val callee_save_regs_ccall = []
      val callee_save_regs_ccall_as_lvs = map reg_to_lv callee_save_regs_ccall

      val caller_save_regs_ccall = [0,1,2,3,4,5,6]
      val caller_save_regs_ccall_as_lvs = map reg_to_lv caller_save_regs_ccall
    end

    fun emit ({top_decls: top_decl list,
	       init_code: inst list,
	       exit_code: inst list,
	       static_data: inst list}, filename) =
      let val os : TextIO.outstream = TextIO.openOut filename
      in (emit_insts (os, init_code);
	  app (emit_topdecl os) top_decls;
	  emit_insts (os, static_data);
	  emit_insts (os, exit_code);
	  TextIO.closeOut os) handle E => (TextIO.closeOut os; raise E)
      end
    type StringTree = PP.StringTree
    fun layout _ = PP.LEAF "not implemented"
  end
