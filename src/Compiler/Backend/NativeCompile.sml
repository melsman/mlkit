(*

   The NativeCompile functor builds a backend appropriate for machine
   code generation; the backend provides, among other things, register
   allocation. This functor is independent of the exact machine
   architecture and is parameterized over a structure RegisterInfo,
   which provides information about machine registers, and a structure
   BackendInfo, which provides information about tagging,
   stack-properties, and so on.

   After program code is compiled into the LinePrg, which this module
   does, the LinePrg is emitted by a machine-dependant code generator,
   see for instance Backend/X86/ExecutionX86.sml. -- mael 2000-10-03

*)

signature NATIVE_COMPILE =
  sig
    structure CallConv : CALL_CONV
    structure LineStmt : LINE_STMT
    structure ClosExp : CLOS_EXP
    structure SubstAndSimplify : SUBST_AND_SIMPLIFY

    type BackendEnv
    type place
    type pp
    type 'a at
    type phsize
    type ('a,'b,'c) LambdaPgm

    type label
    type ('sty,'offset,'aty) LinePrg
    type offset = int
    type StoreTypeCO
    type Aty


    val compile : BackendEnv * ((place*pp)at,place*phsize,unit) LambdaPgm * bool * string(*vcg_file*) ->
      BackendEnv * {main_lab:label,
		    code:(StoreTypeCO,offset,Aty) LinePrg,
		    imports:label list * label list,
		    exports:label list * label list,
		    safe:bool}
  end


functor NativeCompile (structure BackendInfo : BACKEND_INFO
			 where type label = AddressLabels.label
		       structure RegisterInfo : REGISTER_INFO
			 where type lvar = Lvars.lvar
			 where type lvarset = Lvarset.lvarset
		       ) : NATIVE_COMPILE =
  struct
    structure RegionExp = MulExp.RegionExp
    structure PP = PrettyPrint

    structure ClosConvEnv = ClosConvEnv(BackendInfo)

    structure CallConv = CallConv(BackendInfo)

    structure ClosExp = ClosExp(structure ClosConvEnv = ClosConvEnv
				structure BI = BackendInfo
				structure CallConv = CallConv)

    structure LineStmt = LineStmt(structure CallConv = CallConv
				  structure ClosExp = ClosExp
				  structure RI = RegisterInfo
				  structure BI = BackendInfo)

    structure RegAlloc = RegAlloc(structure CallConv = CallConv
				  structure LineStmt = LineStmt
				  structure RI = RegisterInfo)

    structure FetchAndFlush = FetchAndFlush(structure CallConv = CallConv
					    structure LineStmt = LineStmt
					    structure RegAlloc = RegAlloc
					    structure RI = RegisterInfo)

    structure CalcOffset = CalcOffset(structure CallConv = CallConv
				      structure LineStmt = LineStmt
				      structure FetchAndFlush = FetchAndFlush
				      structure BI = BackendInfo)

    structure SubstAndSimplify = SubstAndSimplify(structure CallConv = CallConv
						  structure LineStmt = LineStmt
						  structure CalcOffset = CalcOffset
						  structure RI = RegisterInfo)

    type BackendEnv = ClosExp.env
    type place = PhysSizeInf.place
    type pp = PhysSizeInf.pp
    type 'a at = 'a PhysSizeInf.at
    type phsize = PhysSizeInf.phsize
    type ('a, 'b, 'c) LambdaPgm = ('a, 'b, 'c) PhysSizeInf.LambdaPgm
    type label = SubstAndSimplify.label
    type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
    type offset = SubstAndSimplify.offset
    type StoreTypeCO = SubstAndSimplify.StoreTypeCO
    type Aty = SubstAndSimplify.Aty

    val gc_p = Flags.is_on0 "garbage_collection"
    val print_region_flow_graph = Flags.is_on0 "print_region_flow_graph"

    fun fast_pr stringtree =
           (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
            TextIO.output(!Flags.log, "\n\n"))

    fun display(title, tree) =
        fast_pr(PP.NODE{start=title ^ ": ",
                   finish="",
                   indent=3,
                   children=[tree],
                   childsep=PP.NOSEP
                  }
          )

    fun chat s = if !Flags.chat then print (s ^ "\n") else ()

    (* the boolean `safe' is true if the fragment has no side-effects;
     * for dead code elimination. *)
    fun compile (clos_env: ClosExp.env, app_conv_psi_pgm, safe: bool, vcg_file:string)
      : ClosExp.env * {main_lab: label,
		       code: (StoreTypeCO,offset,Aty) LinePrg,
		       imports: label list * label list,
		       exports: label list * label list,
		       safe:bool}  =
      let

	val _ = RegionFlowGraphProfiling.reset_graph ()

	val {main_lab,code,imports,exports,env=clos_env1} =
	  Timing.timing "ClosConv" ClosExp.cc (clos_env, app_conv_psi_pgm)

	(* Show region flow graph and generate .vcg file *)
	val _ =
	  if print_region_flow_graph() then
	    (display("Report: REGION FLOW GRAPH FOR PROFILING:",
		     RegionFlowGraphProfiling.layout_graph());
	     let val outStreamVCG = TextIO.openOut vcg_file
	     in RegionFlowGraphProfiling.export_graph outStreamVCG;
	       TextIO.closeOut(outStreamVCG);
	       chat ("[Wrote region flow graph for profiling to file " ^ vcg_file ^ "]")
	     end)
	  else ()

	val all_line_stmt = Timing.timing "LineStmt" LineStmt.L {main_lab=main_lab,
								 code=code,imports=imports,
								 exports=exports}
	val all_reg_alloc = Timing.timing "RegAlloc"
	  (if Flags.is_on "register_allocation" then RegAlloc.ra
	   else RegAlloc.ra_dummy) all_line_stmt

	val all_fetch_flush = Timing.timing "FetchFlush" FetchAndFlush.IFF all_reg_alloc
	val all_calc_offset = Timing.timing "CalcOffset" CalcOffset.CO all_fetch_flush

	val all_calc_offset_with_bv =
	  if gc_p() then Timing.timing "CBV" CalcOffset.CBV all_calc_offset
	  else all_calc_offset

	val {main_lab, code, imports, exports, ...} =
	  Timing.timing "SS" SubstAndSimplify.SS all_calc_offset_with_bv
      in (clos_env1,
	 {main_lab=main_lab, code=code, imports=imports, exports=exports,
	  safe=safe})
      end

  end
