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

    fun mapi f i nil = nil
      | mapi f i (x::xs) = f (x,i) :: mapi f (i+1) xs

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

      | Raise => out_opcode RAISE
      | PushExnPtr => out_opcode PUSH_EXN_PTR
      | PopExnPtr => out_opcode POP_EXN_PTR
	  
      | LetregionFin(n) => (out_opcode LETREGION_FIN; out_int n)
      | LetregionInf => (out_opcode LETREGION_INF)
      | EndregionInf => (out_opcode ENDREGION_INF)
      | ResetRegion => (out_opcode RESET_REGION)
      | MaybeResetRegion => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ResetRegionIfInf => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | FetchData(lab) => (out_opcode FETCH_DATA; RLL.out_label lab)   (* fetch from data segment *)
      | StoreData(lab) => (out_opcode STORE_DATA; RLL.out_label lab)   (* store in data segment *)

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
					                              
      | PrimFreshExname => out_opcode PRIM_FRESH_EXNAME

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
			 main_lab_opt,
			 imports_code: label list,
			 imports_data: label list,
			 exports_code: label list,
			 exports_data: label list}, filename:string} : unit = 
      let val _ = chat ("[Emitting KAM code in file " ^ filename ^ "...")
	  val _ = (BC.init_out_code();
		   RLL.reset_label_table();
		   List.app emit_top_decl top_decls)

	    (* to find out where in the code there are references to external
	     * labels, we look in the environment maintained by RLL, which
	     * maps labels to either 1) a known position in the bytecode or 2) a list
	     * of those places that need be updated once the label position is known. *)

	  val map_import_code = map (fn (i,l) => (i, Labels.key l)) (RLL.imports imports_code)
	  val map_import_data = map (fn (i,l) => (i, Labels.key l)) (RLL.imports imports_data)
	  val map_export_code = map (fn (l,i) => (Labels.key l, i)) (RLL.exports exports_code)
	  val map_export_data = map (fn (i,l) => (Labels.key l, i)) (RLL.imports exports_data)

      (* Here is the story about data-segment exports: each unit can allocate data in the
       * data segment. In the non-loaded bytecode the instruction `StoreData lab' stores the
       * accumulator in the data slot determined by the label lab. The map_export_data-part
       * of the bytecode file, determines the bytecode-addresses of the label-arguments to 
       * each of the StoreData instructions. 
       *
       * At load time, each of the labels are associated with
       * new slots relative to the data-segment pointer, then the
       * StoreData instructions are modified to take offsets
       * from the data-segment pointer as arguments, and finally the
       * labels are associated with these offsets in the hash table that
       * maps labels to offsets. 
       *)

      in
	(BC.dump_buffer {filename=filename, 
			 main_lab_opt=Option.map Labels.key main_lab_opt, 
			 map_import_code=map_import_code, 
			 map_import_data=map_import_data, 
			 map_export_code=map_export_code, 
			 map_export_data=map_export_data};
	 print ("[wrote KAM code file:\t" ^ filename ^ "]\n");
	 chat "]\n") handle IO.Io {name,...} => Crash.impossible ("EmitCode.emit:\nI cannot open \""
								  ^ filename ^ "\":\n" ^ name)
      end
  end
