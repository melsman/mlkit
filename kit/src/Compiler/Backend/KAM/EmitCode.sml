functor EmitCode (structure Labels : ADDRESS_LABELS
		  structure CG : CODE_GEN_KAM
		  structure Opcodes : OPCODES_KAM
		  structure BC : BUFF_CODE
		  structure RLL : RESOLVE_LOCAL_LABELS
                    sharing type Labels.label = RLL.label
		  structure Kam : KAM
		    sharing type Kam.AsmPrg = CG.AsmPrg
                    sharing type Kam.label = Labels.label
		  structure BI : BACKEND_INFO
		  structure Flags : FLAGS
		  structure Crash : CRASH) : EMIT_CODE =
  struct
    fun msg s = TextIO.output(TextIO.stdOut, s)
    fun chat(s: string) = if !Flags.chat then msg (s) else ()
    fun die s  = Crash.impossible ("EmitCode." ^ s)

    type target = CG.AsmPrg
    type label = Labels.label

    local
      open BC
      val out_opcode = out_long_i
      val out_int = out_long_i
      val out_word32 = out_long_w32
      val out_byte = out_i
      open Opcodes
      open Kam
    in
      fun emit_kam_inst inst =
	case inst of
	Alloc(n) => (out_opcode ALLOC_N; out_int n)
      | AllocIfInf(n)  => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | AllocSatInf(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | AllocSatIfInf(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | AllocAtbot(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | BlockAlloc(n) => (out_opcode BLOCK_ALLOC_N; out_int n)
      | BlockAllocIfInf(n)  => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | BlockAllocSatInf(n) => (out_opcode BLOCK_ALLOC_SAT_INF_N; out_int n)
      | Block(n) => (out_opcode BLOCK_N; out_int n)
      | BlockAllocSatIfInf(n) => (out_opcode BLOCK_ALLOC_SAT_IF_INF_N; out_int n)
      | BlockAllocAtbot(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | ClearAtbotBit => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | SetAtbotBit => out_opcode SET_ATBOT_BIT

      | SetBit30 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | SetBit31 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ClearBit30And31 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | UbTagCon => out_opcode UB_TAG_CON
	
      | SelectStack(off,s) => (out_opcode SELECT_STACK_N; out_int off)
      | SelectEnv(off,s) => (out_opcode SELECT_ENV_N; out_int off)
      | Select(off) => (out_opcode SELECT_N; out_int off)
      | Store(off) => (out_opcode STORE_N; out_int off)

      | StackAddrInfBit(off,s) => (out_opcode STACK_ADDR_INF_BIT; out_int off)
      | StackAddr(off,s) => (out_opcode STACK_ADDR; out_int off)
      | EnvToAcc => out_opcode ENV_TO_ACC

      |	ImmedInt(i) => (out_opcode IMMED_INT; out_int i)
      | ImmedString(str) => 
	  let
	    val str_size = String.size str
	    fun gen_alignment 0 = ()
	      | gen_alignment n = (out_byte 0; gen_alignment (n-1))
	    val align = if Int.mod(str_size, 4) = 0 then 0 else (4-Int.mod(str_size, 4))
	  in
	    (out_opcode IMMED_STRING;
	     out_word32 (BI.tag_string(true,str_size));
	     out_int str_size;
	     out_int 0; (* NULL pointer to next fragment. *)
	     List.app (fn c => out_byte (Char.ord c)) (String.explode str); (* The actual string *)
	     gen_alignment align) (* obtain word alignment! *)
	  end
      | ImmedReal(r) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | Push => (out_opcode PUSH)
      | PushLbl(lab) => (out_opcode PUSH_LBL; RLL.out_label lab)
      | Pop(n) => (out_opcode POP_N; out_int n)
	
      | ApplyFnCall(n) => (out_opcode APPLY_FN_CALL; out_int n)
      | ApplyFnJmp(n1,n2) => (out_opcode APPLY_FN_JMP; out_int n1; out_int n2)
      | ApplyFunCall(lab,n) => (out_opcode APPLY_FUN_CALL; RLL.out_label lab; out_int n)
      | ApplyFunCallNoClos(lab,n) => (out_opcode APPLY_FUN_CALL_NO_CLOS; RLL.out_label lab; out_int n)
      | ApplyFunJmp(lab,n1,n2) => (out_opcode APPLY_FUN_JMP; RLL.out_label lab; out_int n1; out_int n2)
      | ApplyFunJmpNoClos(lab,n1,n2) => (out_opcode APPLY_FUN_JMP_NO_CLOS; RLL.out_label lab; out_int n1; out_int n2)
      | Return(n1,n2) => 
	  (out_opcode RETURN;
	   out_int n1;
	   out_int n2)
      | ReturnNoClos(n1,n2) => 
	  (out_opcode RETURN_NO_CLOS;
	   out_int n1;
	   out_int n2)

      | Ccall(idx,1) => (out_opcode C_CALL1; out_int idx)
      | Ccall(idx,2) => (out_opcode C_CALL2; out_int idx)
      | Ccall(idx,3) => (out_opcode C_CALL3; out_int idx)
      | Ccall(idx,n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | Label(lab) => RLL.define_label lab
      | JmpRel(lab) => (out_opcode JMP_REL; RLL.out_label lab)
      | IfNotEqJmpRel(lab) => (out_opcode IF_NOT_EQ_JMP_REL; RLL.out_label lab)
      | IfLessThanJmpRel(lab) => (out_opcode IF_LESS_THAN_JMP_REL; RLL.out_label lab)
      | IfGreaterThanJmpRel(lab) => (out_opcode IF_GREATER_THAN_JMP_REL; RLL.out_label lab)
      | DotLabel(lab) => RLL.out_label lab
      | JmpVector(lab,first_sel) => (out_opcode JMP_VECTOR; RLL.out_label lab; out_int first_sel)

      | Raise => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PushExnPtr => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PopExnPtr => die ("inst " ^ (pr_inst inst) ^ " not emitted")
	  
      | LetregionFin(n) => (out_opcode LETREGION_FIN; out_int n)
      | LetregionInf => (out_opcode LETREGION_INF)
      | EndregionInf => (out_opcode ENDREGION_INF)
      | ResetRegion => (out_opcode RESET_REGION)
      | MaybeResetRegion => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ResetRegionIfInf => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | FetchGlobal(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | StoreGlobal(lab) => () (*die ("inst " ^ (pr_inst inst) ^ " not emitted") not implemented yet *)

      | Comment(s) => ()
      | Nop => ()

      | PrimEquali => out_opcode PRIM_EQUAL_I
      | PrimSubi => out_opcode PRIM_SUB_I
      | PrimAddi => out_opcode PRIM_ADD_I
      | PrimNegi => out_opcode PRIM_NEG_I
      | PrimAbsi => out_opcode PRIM_ABS_I

      | PrimAddf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimSubf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimMulf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimNegf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimAbsf => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | PrimLessThan => (out_opcode PRIM_LESS_THAN)
      | PrimLessEqual => (out_opcode PRIM_LESS_EQUAL)
      | PrimGreaterThan => (out_opcode PRIM_GREATER_THAN)
      | PrimGreaterEqual => (out_opcode PRIM_GREATER_EQUAL)
					                              
      | PrimLessThanUnsigned => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimGreaterThanUnsigned	=> die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimLessEqualUnsigned => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimGreaterEqualUnsigned => die ("inst " ^ (pr_inst inst) ^ " not emitted")
					                              
      | PrimAddw8 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimSubw8 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
					                              
      | PrimAndi => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimOri => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimXori => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimShiftLefti => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimShiftRightSignedi => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimShiftRightUnsignedi	=> die ("inst " ^ (pr_inst inst) ^ " not emitted")
					                              
      | PrimAddw => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimSubw => die ("inst " ^ (pr_inst inst) ^ " not emitted")
					                              
      | PrimFreshExname => die ("inst " ^ (pr_inst inst) ^ " not emitted")
    end

    fun emit_kam_insts insts = List.app emit_kam_inst insts

    fun emit_top_decl top_decl =
      let
	fun emit_decl (lab,kam_insts) = (RLL.define_label lab;
					 emit_kam_insts kam_insts)
      in
	case top_decl of
	  Kam.FUN(lab,kam_insts) => emit_decl(lab,kam_insts)
	| Kam.FN(lab,kam_insts) => emit_decl(lab,kam_insts)
      end
    
    fun emit {target as {top_decls: Kam.TopDecl list,
			 init_code: Kam.KamInst list,
			 exit_code: Kam.KamInst list,
			 static_data: Kam.KamInst list}, filename:string} : unit = 
      (chat ("[Emitting KAM code in file " ^ filename ^ "...");
       BC.init_out_code();
       RLL.reset_label_table();
       List.app emit_top_decl top_decls;
       BC.dump_buffer filename;
       print ("[wrote KAM code file:\t" ^ filename ^ " of size " ^ (Int.toString (!BC.out_position)) ^ " bytes]\n");
       chat "]\n") handle IO.Io {name,...} => Crash.impossible ("EmitCode.emit:\nI cannot open \""
								^ filename ^ "\":\n" ^ name)

  end
