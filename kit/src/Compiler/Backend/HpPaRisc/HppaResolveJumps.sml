
functor HppaResolveJumps(structure HpPaRisc : HP_PA_RISC
			 structure Labels : ADDRESS_LABELS
			   sharing type Labels.label = HpPaRisc.label
			 structure IntFinMap : MONO_FINMAP where type dom = int
		         structure Crash : CRASH) : HPPA_RESOLVE_JUMPS =
  struct

  (* ----------------------------------------------------------------------
   * Resolvation of jumps for HP Precision Architecture code. 
   * ---------------------------------------------------------------------- *)

    open HpPaRisc

  (* -----------------------------
   * Some Basic Tools
   * ----------------------------- *)

    fun die s = Crash.impossible ("HppaResolveJumps." ^ s)

    (* instSize inst:  For all meta instructions we see below and count the max. number of
     * instructions that the meta instruction may be expanded into. The pseudo instruction 
     * loadlabel generates 2 instructions (see the HpPaRisc functor.) I guess we could 
     * return zero for all data space pseudo instructions since we only resolve program 
     * space distances. 12/11/97-Martin: I  *)

    val instSize =
      fn META_IF _ => 4         
       | META_BL _ => 4
       | META_BV _ => 2
       | META_IF_BIT _ => 5 (* was 3 *)
       | META_B _ => 4      (* was 3 *)
       | COMMENT _ => 0
       | DOT_ALIGN i => (i div 4) + 1   (* was i div 4 *)
       | DOT_CALL _ => 10
       | DOT_CALLINFO _ => 40
       | DOT_ENTER => 40
       | DOT_LEAVE => 40
       | DOT_PROC => 0
       | DOT_PROCEND => 0
       | LABEL _ => 0
       | _ => 1

    fun genOffsetMaps {top_decls: TopDecl list,
		       init_code: RiscInst list,
		       exit_code: RiscInst list,
		       static_data: RiscInst list} =
      let
   	                                         (* mlm : ML Fun label map *)
	fun addMap (i, (mlm,lm,offset)) = case i (* lm  : Local label map *)
					    of LABEL(MLFunLab label) => (IntFinMap.add (Labels.key label, offset, mlm),lm,offset)
					     | LABEL(LocalLab label) => (mlm, IntFinMap.add (Labels.key label,offset,lm),offset)
					     | LABEL _ => (mlm,lm,offset)
					     | _ => (mlm,lm,offset+instSize i)
	fun genOffsetMapRiscInstList([], maps) = maps
	  | genOffsetMapRiscInstList(inst::inst_list,maps) = genOffsetMapRiscInstList(inst_list,addMap(inst,maps))
	fun genOffsetMapTopDecl(FUN(_,inst_list),maps) = genOffsetMapRiscInstList(inst_list,maps)
	  | genOffsetMapTopDecl(FN(_,inst_list),maps) = genOffsetMapRiscInstList(inst_list,maps)
	val initMaps = genOffsetMapRiscInstList(init_code,(IntFinMap.empty, IntFinMap.empty,0))
	fun genOffsetMapTopDecls([],maps) = maps
	  | genOffsetMapTopDecls(top_decl::top_decls,maps) = genOffsetMapTopDecls(top_decls,genOffsetMapTopDecl(top_decl,maps))
	val funMaps = genOffsetMapTopDecls(top_decls,initMaps)
	val (mlm,lm,offset) = genOffsetMapRiscInstList(exit_code,funMaps)
      in
	(mlm,lm)
      end

    (* Note, that (only) tmp_reg0 and Gen 1 is used as temporary registers below. *)
    fun RJ (prg as {top_decls: TopDecl list,
		    init_code: RiscInst list,
		    exit_code: RiscInst list,
		    static_data: RiscInst list}) =
      (* Don't remove init_code - it has to come first *)
      (* Don't remove exit_code - it has to come last *)
      let
        val (blockMap,localMap) = genOffsetMaps prg
	fun lookup m n = case IntFinMap.lookup m n
			   of SOME i => i
			    | NONE => die "lookup"

	val longjump = 3000000
	val _ = if is_im19 longjump then die "longjump not long enough" else ()

	fun jumpSize (MLFunLab label,offset) = (case IntFinMap.lookup blockMap (Labels.key label)
						  of SOME n => (n - offset) * 4
						   | NONE => longjump)
	  | jumpSize (LocalLab label,offset) = (lookup localMap (Labels.key label) - offset) * 4
	  | jumpSize (NameLab labStr,_) = longjump
	  | jumpSize _ = die "jumpSize"

	fun loadLabel(lab,destReg,C) = 
	  ADDIL'{pr_i=fn() => "L'" ^ pp_lab lab ^ "-$global$", r=dp} ::
	  LDO'{pr_d=fn() => "R'" ^ pp_lab lab ^ "-$global$", b=Gen 1, t=destReg} :: C

	fun resolveInst(inst,offset,C) =
	  case inst
	    of META_IF {cond: cond, r1: reg, r2: reg, target: lab} =>
	      let
		val js = jumpSize(target,offset)
	      in
		if is_im14 js then
		  COMB {cond=revCond cond, n=true, r1=r1, r2=r2, target=target} :: C
		else 
		  if is_im19 js then
		    COMCLR {cond=cond, r1=r1, r2=r2, t=Gen 1} ::
		    B {n=true, target=target} :: C
		  else 
		    loadLabel(target,tmp_reg0,    (* 2 insts *)
			      COMCLR{cond=cond,r1=r1,r2=r2,t=Gen 1} ::
			      BV{n=true,x=Gen 0, b=tmp_reg0} :: C)
	      end
	     | META_BL {n: bool, target: lab, rpLink: reg, callStr : string} =>
	      let
		val js = jumpSize(target,offset)
	      in
		if is_im19 js then
		  DOT_CALL callStr ::
		  BL{n=false, target=target, t=rpLink} ::
		  NOP :: C
		else
		  DOT_CALL callStr ::
		  LDIL {i="L'" ^ pp_lab target, t=Gen 1} ::
		  BLE {n=false, wd="R'" ^ pp_lab target, sr=Space 4, b=Gen 1} ::
		  COPY {r=Gen 31, t=rpLink} :: C
	      end
	     | META_BV {n: bool, x: reg, b: reg} =>
		 (* This may only take up one instruction *)
		  BV {n=false,x=x,b=b} ::
		  NOP :: C
	     | META_IF_BIT {r: reg, bitNo: int, target: lab} =>
		  let
		    val js = jumpSize(target,offset)
		  in
		    if is_im14 js then
		      BB {n=true, cond=GREATEREQUAL, r=r, p=bitNo, target=target} :: C
		    else 
		      if is_im19 js then
			if bitNo < 31 then
			  SHD{cond=NEVER, r1=Gen 0, r2=r, p=Int.toString (31-bitNo), t=Gen 1} ::
			  AND {cond=ODD, r1=Gen 1, r2=Gen 1, t=Gen 0} ::
			  B {n=true, target=target} :: C
			else
			  AND {cond=ODD, r1=r, r2=r, t=Gen 0} ::
			  B {n=true, target=target} :: C
		      else
			if bitNo < 31 then
			  loadLabel(target,tmp_reg0,  (* 2 insts *)
				    SHD{cond=NEVER, r1=Gen 0, r2=r, p=Int.toString (31-bitNo), t=Gen 1} ::
				    AND{cond=ODD, r1=Gen 1, r2=Gen 1, t=Gen 0} ::
				    BV{n=true,x=Gen 0, b=tmp_reg0} :: C)
			else
			  loadLabel(target,tmp_reg0,   (* 2 insts *)
				    AND{cond=ODD, r1=r, r2=r, t=Gen 0} ::
				    BV{n=true,x=Gen 0, b=tmp_reg0} :: C)
		  end
	     | META_B {n: bool, target: lab} =>
		   let
		     val js = jumpSize(target,offset)
		   in
		     if is_im19 js then
		       B{n=false, target=target} ::
		       NOP :: C
		     else
		       loadLabel(target, tmp_reg0,
				 BV{n=false,x=Gen 0, b=tmp_reg0} ::
				 NOP :: C)
		   end
	     | _ => inst :: C
		
	fun resolveRiscInstList(inst_list,offset) =
	  let
	    fun fold ([],offset) = ([],offset)
	      | fold (inst::insts,offset) = 
	      let
		val offset' = offset + instSize inst
		val (C',offset'') = fold(insts,offset')
	      in
		(resolveInst(inst,offset,C'),offset'')
	      end
	  in
	    fold(inst_list,offset)
	  end
	fun do_top_decl(FUN(lab,inst_list),offset) = 
	  let
	    val (inst_list',offset') = resolveRiscInstList(inst_list,offset)
	  in
	    (FUN(lab,inst_list'),offset')
	  end
	  | do_top_decl(FN(lab,inst_list),offset) = 
	  let
	    val (inst_list',offset') = resolveRiscInstList(inst_list,offset)
	  in
	    (FN(lab,inst_list'),offset')
	  end
	val (init_code',offset_init) = resolveRiscInstList(init_code,0)
	fun do_top_decls([],offset) = ([],offset)
	  | do_top_decls(top_decl::top_decls,offset) =
	  let
	    val (top_decl',offset') = do_top_decl(top_decl,offset)
	    val (C',offset'') = do_top_decls(top_decls,offset')
	  in
	    (top_decl'::C',offset'')
	  end
	val (top_decls',offset_top_decls) = do_top_decls(top_decls,offset_init)
	val (exit_code',_) = resolveRiscInstList(exit_code,offset_top_decls)
      in
	{top_decls = top_decls',
	 init_code = init_code',
	 exit_code = exit_code',
	 static_data = static_data}
      end  
  end



