functor EmitCode (structure Labels : ADDRESS_LABELS
		  structure CG : CODE_GEN_KAM
		  structure Opcodes : OPCODES_KAM
		  structure BC : BUFF_CODE
		  structure RLL : RESOLVE_LOCAL_LABELS
		  structure Kam : KAM
		    sharing type Kam.AsmPrg = CG.AsmPrg
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

      | BlockAlloc(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | BlockAllocIfInf(n)  => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | BlockAllocSatInf(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | Block(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | BlockAllocSatIfInf(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | BlockAllocAtbot(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | ClearAtbotBit => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | SetAtbotBit => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | SetBit30 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | SetBit31 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ClearBit30And31 => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | UbTagCon => die ("inst " ^ (pr_inst inst) ^ " not emitted")
	
      | SelectStack(off,s) => (out_opcode SELECT_STACK_N; out_int off)
      | SelectEnv(off,s) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | Select(off) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | Store(off) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | StackAddrInfBit(off,s) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | StackAddr(off,s) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | EnvToAcc => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      |	ImmedInt(i) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
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
      | PushLbl(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | Pop(n) => (out_opcode POP_N; out_int n)
	
      | ApplyFnCall(n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ApplyFnJmp(n1,n2) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ApplyFunCall(lab,n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ApplyFunCallNoClos(lab,n) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ApplyFunJmp(lab,n1,n2) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ApplyFunJmpNoClos(lab,n1,n2) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | Return(n1,n2) => 
	  (out_opcode RETURN;
	   out_int n1;
	   out_int n2)

      | Ccall(idx,arity) => 
	  (out_opcode C_CALL;
	   out_int idx;
	   out_int arity)

      | Label(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | JmpRel(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | IfNotEqJmpRel(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | IfLessThanJmpRel(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | IfGreaterThanJmpRel(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | DotLabel(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | JmpVector(lab,first_sel) => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | Raise => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PushExnPtr => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PopExnPtr => die ("inst " ^ (pr_inst inst) ^ " not emitted")
	  
      | LetregionFin(n) => (out_opcode LETREGION_FIN; out_int n)
      | LetregionInf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | EndregionInf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ResetRegion => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | MaybeResetRegion => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | ResetRegionIfInf => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | FetchGlobal(lab) => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | StoreGlobal(lab) => () (*die ("inst " ^ (pr_inst inst) ^ " not emitted") not implemented yet *)

      | Comment(s) => ()
      | Nop => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | PrimEqual => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimSubi => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimAddi => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimNegi => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimAbsi => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | PrimAddf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimSubf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimMulf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimNegf => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimAbsf => die ("inst " ^ (pr_inst inst) ^ " not emitted")

      | PrimLessThan => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimLessEqual => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimGreaterThan => die ("inst " ^ (pr_inst inst) ^ " not emitted")
      | PrimGreaterEqual => die ("inst " ^ (pr_inst inst) ^ " not emitted")
					                              
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
	fun emit_decl (lab,kam_insts) = 
	  (BC.init_out_code ();
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
       List.app emit_top_decl top_decls;
       BC.dump_buffer filename;
       print ("[wrote KAM code file:\t" ^ filename ^ "]\n");
       chat "]\n") handle IO.Io {name,...} => Crash.impossible ("EmitCode.emit:\nI cannot open \""
								^ filename ^ "\":\n" ^ name)

  end
