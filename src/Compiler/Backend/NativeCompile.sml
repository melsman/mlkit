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


    val compile : BackendEnv * ((place*pp)at,place*phsize,unit) LambdaPgm * bool -> 
      BackendEnv * {main_lab:label,
		    code:(StoreTypeCO,offset,Aty) LinePrg,
		    imports:label list * label list,
		    exports:label list * label list,
		    safe:bool}
  end


functor NativeCompile (include EXECUTION_ARGS
                       structure Effect : EFFECT
		       structure AtInf : AT_INF
		       structure PhysSizeInf : PHYS_SIZE_INF
		       structure MulExp : MUL_EXP
		       structure EffVarEnv : MONO_FINMAP
		       structure RType : RTYPE
		       structure Mul : MUL
			 sharing type Effect.place = AtInf.place = PhysSizeInf.place = MulExp.place = RType.place = EffVarEnv.dom = Mul.place
			 sharing type Effect.effect = MulExp.effect = Mul.effectvar
			 sharing type Lvars.lvar = MulExp.lvar = Mul.lvar
			 sharing type Excon.excon = MulExp.excon
			 sharing type Con.con = MulExp.con
			 sharing type Elaboration.Basics.TyName.TyName = MulExp.TyName = RType.tyname
			 sharing type Elaboration.Basics.Tools.PrettyPrint.StringTree = Mul.StringTree = 
			   RType.StringTree = MulExp.StringTree = PhysSizeInf.StringTree =
			   Effect.StringTree = AtInf.StringTree = EffVarEnv.StringTree
			 sharing type PhysSizeInf.at = AtInf.at
			 sharing type MulExp.mulef = Mul.mulef
			 sharing type MulExp.qmularefset = Mul.qmularefset
			 sharing type RType.Type = MulExp.Type
			 sharing type MulExp.LambdaPgm = PhysSizeInf.LambdaPgm
		         sharing type MulExp.il = RType.il
		       structure BackendInfo : BACKEND_INFO
			 sharing type BackendInfo.label = Labels.label
		       structure RegisterInfo : REGISTER_INFO
			 sharing type RegisterInfo.lvar = Lvars.lvar
			 sharing type RegisterInfo.lvarset = Lvarset.lvarset
		       ) : NATIVE_COMPILE =
  struct
    structure RegionExp = MulExp.RegionExp
    structure Basics = Elaboration.Basics
    structure Tools = Basics.Tools
    structure PP = Tools.PrettyPrint
    structure Crash = Tools.Crash
    structure Flags = Tools.Flags
    structure Report = Tools.Report
    structure IntFinMap = Tools.IntFinMap
    structure Timing = Tools.Timing

    structure TyName = Basics.TyName

    structure RegionFlowGraphProfiling =
      RegionFlowGraphProfiling(structure Effect = Effect
			       structure AtInf = AtInf
			       structure PhySizeInf = PhysSizeInf
			       structure PP = PP
			       structure Flags = Flags
			       structure Crash = Crash
			       structure Report = Report)

    structure ClosConvEnv = ClosConvEnv(structure Lvars = Lvars
					structure Con = Con
					structure Excon = Excon
					structure Effect = Effect
					structure MulExp = MulExp
					structure RegvarFinMap = EffVarEnv
					structure PhysSizeInf = PhysSizeInf
					structure Labels = Labels
					structure BI = BackendInfo
					structure PP = PP
					structure Crash = Crash)

    structure CallConv = CallConv(structure Lvars = Lvars
				  structure BI = BackendInfo
				  structure PP = PP
				  structure Flags = Flags
				  structure Report = Report
				  structure Crash = Crash)

    structure ClosExp = ClosExp(structure Con = Con
				structure Excon = Excon
				structure Lvars = Lvars
				structure TyName = TyName
				structure Effect = Effect
				structure RType = RType
				structure MulExp = MulExp
				structure Mul = Mul
				structure RegionExp = RegionExp
				structure AtInf = AtInf
				structure PhysSizeInf = PhysSizeInf
				structure Labels = Labels
				structure ClosConvEnv = ClosConvEnv
				structure BI = BackendInfo
				structure CallConv = CallConv
				structure PP = PP
				structure Flags = Flags
				structure Report = Report
				structure Crash = Crash)

    structure LineStmt = LineStmt(structure PhysSizeInf = PhysSizeInf
				  structure Con = Con
				  structure Excon = Excon
				  structure Lvars = Lvars
				  structure Effect = Effect
				  structure Labels = Labels
				  structure CallConv = CallConv
				  structure ClosExp = ClosExp
				  structure RI = RegisterInfo
				  structure BI = BackendInfo
				  structure Lvarset = Lvarset
				  structure PP = PP
				  structure Flags = Flags
				  structure Report = Report
				  structure Crash = Crash)

    structure RegAlloc = RegAlloc(structure PhysSizeInf = PhysSizeInf
				  structure Con = Con
				  structure Excon = Excon
				  structure Lvars = Lvars
				  structure Effect = Effect
				  structure Lvarset = Lvarset
				  structure Labels = Labels
				  structure CallConv = CallConv
				  structure LineStmt = LineStmt
				  structure RI = RegisterInfo
				  structure PP = PP
				  structure Flags = Flags
				  structure Report = Report
				  structure Crash = Crash)

    structure FetchAndFlush = FetchAndFlush(structure PhysSizeInf = PhysSizeInf
					    structure Con = Con
					    structure Excon = Excon
					    structure Lvars = Lvars
					    structure Effect = Effect
					    structure Labels = Labels
					    structure CallConv = CallConv
					    structure LineStmt = LineStmt
					    structure RegAlloc = RegAlloc
					    structure RI = RegisterInfo
					    structure Lvarset = Lvarset
					    structure PP = PP
					    structure Flags = Flags
					    structure Report = Report
					    structure Crash = Crash)

    structure CalcOffset = CalcOffset(structure PhysSizeInf = PhysSizeInf
				      structure Con = Con
				      structure Excon = Excon
				      structure Lvars = Lvars
				      structure Effect = Effect
				      structure Labels = Labels
				      structure CallConv = CallConv
				      structure LineStmt = LineStmt
				      structure FetchAndFlush = FetchAndFlush
				      structure BI = BackendInfo
				      structure IntSet = IntSet(structure PP = PP)
				      structure PP = PP
				      structure Flags = Flags
				      structure Report = Report
				      structure Crash = Crash)

    structure SubstAndSimplify = SubstAndSimplify(structure PhysSizeInf = PhysSizeInf
						  structure Con = Con
						  structure Excon = Excon
						  structure Lvars = Lvars
						  structure Effect = Effect
						  structure RegvarFinMap = EffVarEnv
						  structure Labels = Labels
						  structure CallConv = CallConv
						  structure LineStmt = LineStmt
						  structure CalcOffset = CalcOffset
						  structure RI = RegisterInfo
						  structure PP = PP
						  structure Flags = Flags
						  structure Report = Report
						  structure Crash = Crash)

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

    (* the boolean `safe' is true if the fragment has no side-effects;
     * for dead code elimination. *)
    fun compile (clos_env: ClosExp.env, app_conv_psi_pgm, safe: bool) 
      : ClosExp.env * {main_lab: label, 
		       code: (StoreTypeCO,offset,Aty) LinePrg,
		       imports: label list * label list, 
		       exports: label list * label list, 
		       safe:bool}  =
      let
	val {main_lab,code,imports,exports,env=clos_env1} = 
	  Timing.timing "ClosConv" ClosExp.cc (clos_env, app_conv_psi_pgm)
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
