functor HpPaDelaySlotOptimization(structure HpPaRisc : HP_PA_RISC
				  structure Flags : FLAGS
				  structure Crash : CRASH) : HP_PA_DELAY_SLOT_OPTIMIZATION =
  struct
    val debug = true

    (* ----------------------------------------------------------------------
     * Delay slot optimization for HP Precision Architecture code. 
     * ---------------------------------------------------------------------- *)
    open HpPaRisc

    (* -----------------------------
     * Some Basic Tools
     * ----------------------------- *)

    fun msg(s: string) = (TextIO.output(TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut)
    fun chat s = if !Flags.chat then msg(s ^ " ...\n") else ()
    fun die s = Crash.impossible ("HpPaDelaySlotOptimization." ^ s)

    (* Function does_inst_nullify is in module HpPaRisc *)
    fun doesFirstInstNullify ([]) = false
      | doesFirstInstNullify (i::C) = does_inst_nullify(i)

    fun fold f [] a = a
      | fold f (x::xs) a = fold f xs (f(x,a))

    (* Returns true if lists l1 and l2 have no same elements. *)
    fun check(l1, l2) =
      fold (fn (r1, akk1) => (fold (fn (r2, akk2) => akk2 andalso (r1 <> r2)) l2 akk1)) l1 true;
      
    (* Returns true if i1 does not define registers used by i2 and *)
    (* if i1 does not use registers defined by i2.                 *)
    fun checkDefUse(i1, i2) = check(regs_defd(i1), regs_used(i2)) andalso check(regs_used(i1), regs_defd(i2))

    (* Checks that inst1 can be put in the delay slot of inst2, and that there are *) 
    (* no def/use problems between the two instructions.                           *)
    (* When we check def/use dependencies, we do not check for flag registers, so  *)
    (* therefore some instructions returns false even though they can be put in a  *)
    (* delay slot.                                                                 *)
    (* We would have to update the functions regs_defd and regs_used in HpPaRisc   *)
    (* with status registers.                                                      *)
    fun instOkInDelaySlot (inst1, inst2) =
      let
	fun chk(i1, i2) = checkDefUse(i1, i2) andalso (not (does_inst_nullify(i1)))
      in
	case inst1 of
	  ADD  _   =>  chk(inst1, inst2)
	| ADDO _   =>  chk(inst1, inst2)
	| ADDI _   =>  chk(inst1, inst2)
	| ADDIO _  =>  chk(inst1, inst2)
	| ADDIL _  =>  chk(inst1, inst2)
	| ADDIL' _ =>  chk(inst1, inst2)
	| AND _    =>  false 
	| ANDCM _  =>  false
		   
	| B _      => false
	| BB _     => false
	| BL _     => false
	| BLE _    => false
	| BV _     => false
		   
	| COMB _   => false
	| COMCLR _ => false
	| COPY _   => chk(inst1, inst2)
	    
	| DEPI _   => chk(inst1, inst2)
	  
	| FABS _   => false
	| FADD _   => false
	| FCMP _   => false
	| FLDDS _  => false
	| FMPY _   => false
	| FSTDS _  => false
	| FSUB _   => false
	| FTEST    => false
	| XMPYU _  => false
	  
	| LDI _    => chk(inst1, inst2)
	| LDIL _   => chk(inst1, inst2)
	| LDO _    => chk(inst1, inst2)
	| LDO' _   => chk(inst1, inst2)
	| LDW _    => chk(inst1, inst2)
	| LDW' _   => chk(inst1, inst2)
	| LDWS _   => chk(inst1, inst2)
	| LDWM _   => chk(inst1, inst2)
		   
	| NOP      => false

	| OR _     => false
	| XOR _    => false
	| SH1ADD _ => false
	| SH2ADD _ => false
		   
	| SHD _    => chk(inst1, inst2)
	| SUB  _   => chk(inst1, inst2)
	| SUBO _   => chk(inst1, inst2)
	| SUBI _   => chk(inst1, inst2)
	| STW  _   => chk(inst1, inst2)
	| STW' _   => chk(inst1, inst2)
	| STWS _   => chk(inst1, inst2)
	| STWM _   => chk(inst1, inst2)

	| ZVDEP _  => false
	| MTSAR _  => false
	| VEXTRS _ => false
	| VSHD _   => false
	    
	| LABEL _        => false
	| COMMENT _      => false
	| NOT_IMPL _     => die "instOkInDelaySlot - NOT_IMPL" 
	| DOT_ALIGN _    => false
	| DOT_BLOCKZ _   => false
	| DOT_CALL _     => false
	| DOT_CALLINFO _ => false
	| DOT_CODE       => false
	| DOT_DATA       => false
	| DOT_DOUBLE _   => false
	| DOT_END        => false
	| DOT_ENTER      => false
	| DOT_ENTRY      => false
	| DOT_EQU _      => false
	| DOT_EXPORT _   => false
	| DOT_IMPORT _   => false
	| DOT_LEAVE      => false
	| DOT_EXIT       => false
	| DOT_PROC       => false
	| DOT_PROCEND    => false
	| DOT_STRINGZ _  => false
	| DOT_WORD _     => false
	| DOT_BYTE _     => false
	  
	| META_IF _     => die "instOkInDelaySlot - META_IF" 
	| META_BL _     => die "instOkInDelaySlot - META_BL" 
	| META_BV _     => die "instOkInDelaySlot - META_BV" 
	| META_IF_BIT _ => die "instOkInDelaySlot - META_IF_BIT" 
	| META_B _      => die "instOkInDelaySlot - META_B" 
      end

    (* It is only assembler directives that we do not have to stop for. *)
    val haveToStop =
      fn ADD _      => true
       | ADDO _     => true 
       | ADDI _     => true
       | ADDIO _    => true
       | ADDIL _    => true
       | ADDIL' _   => true
       | AND _      => true
       | ANDCM _    => true
		   
       | B _        => true
       | BB _       => true
       | BL _       => true
       | BLE _      => true
       | BV _       => true
		   
       | COMB _     => true
       | COMCLR _   => true
       | COPY _     => true
		   
       | DEPI _     => true

       | FABS _     => true
       | FADD _     => true
       | FCMP _     => true
       | FLDDS _    => true
       | FMPY  _    => true
       | FSTDS _    => true
       | FSUB _     => true
       | FTEST      => true
       | XMPYU _    => true
		   
       | LDI _      => true
       | LDIL _     => true
       | LDO _      => true
       | LDO' _     => true
       | LDW _      => true
       | LDW' _     => true
       | LDWS _     => true
       | LDWM _     => true
		   
       | NOP        => true

       | OR _       => true
       | XOR _      => true
       | SH1ADD _   => true
       | SH2ADD _   => true
		   
       | SHD _      => true
       | SUB _      => true
       | SUBO _     => true
       | SUBI _     => true
       | STW _      => true
       | STW' _     => true
       | STWS _     => true
       | STWM _     => true

       | ZVDEP _    => true
       | MTSAR _    => true
       | VEXTRS _   => true
       | VSHD _     => true

       | LABEL _        => true
       | COMMENT _      => false
       | NOT_IMPL _     => die "haveToStop - NOT_IMPL" 
       | DOT_ALIGN _    => false
       | DOT_BLOCKZ _   => false
       | DOT_CALL _     => false
       | DOT_CALLINFO _ => false
       | DOT_CODE       => false
       | DOT_DATA       => false
       | DOT_DOUBLE _   => false
       | DOT_END        => false
       | DOT_ENTER      => false
       | DOT_ENTRY      => false
       | DOT_EQU _      => false
       | DOT_EXPORT _   => false
       | DOT_IMPORT _   => false
       | DOT_LEAVE      => false
       | DOT_EXIT       => false
       | DOT_PROC       => false
       | DOT_PROCEND    => false
       | DOT_STRINGZ _  => false
       | DOT_WORD _     => false
       | DOT_BYTE _     => false

       | META_IF _      => die "haveToStop - META_IF" 
       | META_BL _      => die "haveToStop - META_BL" 
       | META_BV _      => die "haveToStop - META_BV" 
       | META_IF_BIT _  => die "haveToStop - META_IF_BIT" 
       | META_B _       => die "haveToStop - META_B" 
      
    fun DSO(prg as {top_decls: TopDecl list,
		    init_code: RiscInst list,
		    exit_code: RiscInst list,
		    static_data: RiscInst list}) =
      (* Don't remove init_code - it has to come first *)
      (* Don't remove exit_code - it has to come last *)
      let
	(* Some statistics *)
	val numberOfNOP      = ref 0
	val numberOfBopt     = ref 0
	val numberOfBnoOpt   = ref 0
	val numberOfBLopt    = ref 0
	val numberOfBLnoOpt  = ref 0
	val numberOfBVopt    = ref 0
	val numberOfBVnoOpt  = ref 0
	val numberOfBLEopt   = ref 0
	val numberOfBLEnoOpt = ref 0
	val numberOfBBopt    = ref 0
	val numberOfBBnoOpt  = ref 0

	fun findSubInst ([], bInst) = (NOP, [])
	  | findSubInst (inst::C, bInst) =
	  if instOkInDelaySlot(inst, bInst) andalso not (doesFirstInstNullify C) then
	    (inst, C)
	  else
	    (if haveToStop inst then
	       (NOP, inst::C)
	     else
	       let
		 val (sub_inst,C_res) = findSubInst(C, bInst)
	       in
		 (sub_inst,inst::C_res)
	       end)

	fun isNOP NOP = true
	  | isNOP _ = false

	fun delaySlotOptimizationList(C: RiscInst list,sinceLastLab: RiscInst list -> RiscInst list,result: RiscInst list) =
	  case C of
	    []      => sinceLastLab result
	  | NOP::C' =>
	      let
		val _ = numberOfNOP := !numberOfNOP + 1
	      in
		(case C' of
		   BL{n,target,t} :: C'' =>
		     let
		       val (subInst, C_next) = findSubInst(C'',BL{n=n,target=target,t=t})
		       val _ = (if isNOP subInst then
				  numberOfBLnoOpt := !numberOfBLnoOpt + 1
				else
				  numberOfBLopt := !numberOfBLopt + 1)
		     in
		       if doesFirstInstNullify(C'') then
			 delaySlotOptimizationList(C_next, fn C => BL{n=false,target=target,t=t}::subInst::(sinceLastLab C), result)
		       else
			 delaySlotOptimizationList(C_next, fn C => BL{n=false,target=target,t=t}::subInst::C, result)
		     end
		 | BV{n=n,x=x,b=base} :: C'' => 
		     let
		       val (subInst, C_next) = findSubInst(C'',BV{n=n,x=x,b=base})
		       val _ = (if isNOP subInst then
				  numberOfBVnoOpt := !numberOfBVnoOpt + 1
				else
				  numberOfBVopt := !numberOfBVopt + 1)
		     in
		       if doesFirstInstNullify C'' then
			 delaySlotOptimizationList(C_next, fn C => BV{n=false,x=x,b=base}::subInst::(sinceLastLab C), result)
		       else
			 delaySlotOptimizationList(C_next, fn C => BV{n=false,x=x,b=base}::subInst::C, result)
		     end
		 | B{n,target} :: C'' => 
		     let
		       val (subInst, C_next) = findSubInst(C'',B{n=n,target=target})
		       val _ = (if isNOP subInst then
				  numberOfBnoOpt := !numberOfBnoOpt + 1
				else
				  numberOfBopt := !numberOfBopt + 1)
		     in
		       if doesFirstInstNullify C'' then
			 delaySlotOptimizationList(C_next, fn C => B{n=false,target=target}::subInst::(sinceLastLab C), result)
		       else
			 delaySlotOptimizationList(C_next, fn C => B{n=false,target=target}::subInst:: C, result)
		     end
		 | BLE{n,wd,sr,b=b'} :: C'' => 
		     let
		       val (subInst, C_next) = findSubInst(C'', BLE{n=n,wd=wd,sr=sr,b=b'})
		       val _ = (if isNOP subInst then
				  numberOfBLEnoOpt := !numberOfBLEnoOpt + 1
				else
				  numberOfBLEopt := !numberOfBLEopt + 1)
		     in
		       if doesFirstInstNullify C'' then
			 delaySlotOptimizationList(C_next, fn C => BLE{n=false,wd=wd,sr=sr,b=b'}::subInst::(sinceLastLab C), result)
		       else
			 delaySlotOptimizationList(C_next, fn C => BLE{n=false,wd=wd,sr=sr,b=b'}::subInst::C, result)
		     end
		 | BB{n,cond,r,p,target} :: C'' => 
		     let
		       val (subInst, C_next) = findSubInst(C'',BB{n=n,cond=cond,r=r,p=p,target=target})
		       val _ = (if isNOP subInst then
				  numberOfBBnoOpt := !numberOfBBnoOpt + 1
				else
				  numberOfBBopt := !numberOfBBopt + 1)
		     in
		       if doesFirstInstNullify C'' then
			 delaySlotOptimizationList(C_next, fn C => BB{n=false,cond=cond,r=r,p=p,target=target}::subInst::(sinceLastLab C), result)
		       else
			 delaySlotOptimizationList(C_next, fn C => BB{n=false,cond=cond,r=r,p=p,target=target}::subInst::C, result)
		     end
		 | _ => delaySlotOptimizationList(C', fn C => NOP::(sinceLastLab C), result))
	      end
	  | LABEL lab :: C' => 
	      let
		fun keep_asm_directives([], C) = ([], C)
		  | keep_asm_directives(i::is,C) = if is_asm_directive i then keep_asm_directives(is,i::C) else (i::is,C)
		val (C'',res) = keep_asm_directives(C',LABEL lab::(sinceLastLab result))
	      in
		delaySlotOptimizationList(C'', fn C => C, res)
	      end
	  | inst :: C' => delaySlotOptimizationList(C', fn C => inst::(sinceLastLab C), result)

	fun do_top_decl(FUN(lab,inst_list)) = FUN(lab,delaySlotOptimizationList(List.rev inst_list,fn C => C, []))
	  | do_top_decl(FN(lab,inst_list))  = FN(lab,delaySlotOptimizationList(List.rev inst_list,fn C => C, []))

	val init_code' = delaySlotOptimizationList(List.rev init_code, fn C => C, [])
	val top_decls' = List.map do_top_decl top_decls
	val exit_code' = delaySlotOptimizationList(List.rev exit_code, fn C => C, [])

	val _ = 
	  if debug then
	        (chat ("Number of NOPs              : " ^ (Int.toString (!numberOfNOP)));
		 chat ("Number of B optimized       : " ^ (Int.toString (!numberOfBopt)));
		 chat ("Number of B not optimized   : " ^ (Int.toString (!numberOfBnoOpt)));
		 chat ("Number of BL optimized      : " ^ (Int.toString (!numberOfBLopt)));
		 chat ("Number of BL not optimized  : " ^ (Int.toString (!numberOfBLnoOpt)));
		 chat ("Number of BV optimized      : " ^ (Int.toString (!numberOfBVopt)));
		 chat ("Number of BV not optimized  : " ^ (Int.toString (!numberOfBVnoOpt)));
		 chat ("Number of BLE optimized     : " ^ (Int.toString (!numberOfBLEopt)));
		 chat ("Number of BLE not optimized : " ^ (Int.toString (!numberOfBLEnoOpt)));
		 chat ("Number of BB optimized      : " ^ (Int.toString (!numberOfBBopt)));
		 chat ("Number of BB not optimized  : " ^ (Int.toString (!numberOfBBnoOpt))))
	  else ()
      in
	{top_decls = top_decls',
	 init_code = init_code',
	 exit_code = exit_code',
	 static_data = static_data}
      end  
  end



