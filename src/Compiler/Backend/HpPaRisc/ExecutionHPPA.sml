
functor ExecutionHPPA(structure TopdecGrammar : TOPDEC_GRAMMAR
		      structure FreeIds : FREE_IDS
		      structure Basics : BASICS
		      sharing TopdecGrammar.StrId = Basics.StrId
		      sharing TopdecGrammar.SigId = Basics.SigId
		      sharing TopdecGrammar.FunId = Basics.FunId
		      sharing type TopdecGrammar.DecGrammar.lab = Basics.Lab.lab
		      sharing type TopdecGrammar.DecGrammar.scon = Basics.SCon.scon
		      sharing type TopdecGrammar.DecGrammar.tycon = Basics.TyCon.tycon
		      sharing type TopdecGrammar.DecGrammar.longtycon = Basics.TyCon.longtycon
		      sharing type TopdecGrammar.DecGrammar.tyvar = Basics.TyVar.SyntaxTyVar
		      sharing type TopdecGrammar.DecGrammar.id = Basics.Ident.id
		      sharing type TopdecGrammar.DecGrammar.longid = Basics.Ident.longid = 
			Basics.ModuleEnvironments.longid
		      sharing type TopdecGrammar.DecGrammar.info = Basics.AllInfo.ElabInfo.ElabInfo
		      sharing type TopdecGrammar.DecGrammar.StringTree = Basics.Tools.PrettyPrint.StringTree
		      structure Lvars : LVARS
			sharing type Lvars.name = Basics.Name.name
			sharing type Lvars.Map.StringTree = Basics.Tools.PrettyPrint.StringTree
		      structure Lvarset : LVARSET
			sharing type Lvarset.lvar = Lvars.lvar
		      structure Labels : ADDRESS_LABELS
			sharing type Labels.name = Basics.Name.name
		      structure Con : CON
			sharing type Con.name = Basics.Name.name
			sharing type Con.Map.StringTree = Basics.Tools.PrettyPrint.StringTree
		      structure Excon : EXCON
			sharing type Excon.name = Basics.Name.name
			sharing type Excon.Map.StringTree = Basics.Tools.PrettyPrint.StringTree
			) : EXECUTION =
  struct
    structure Tools       = Basics.Tools
    structure AllInfo     = Basics.AllInfo
    structure PP          = Tools.PrettyPrint
    structure Name        = Basics.Name
    structure IntFinMap   = Tools.IntFinMap
    structure Flags       = Tools.Flags
    structure Report      = Tools.Report
    structure Crash       = Tools.Crash

    structure HpPaRisc = HpPaRISC(structure Labels = Labels
				  structure Lvars = Lvars
				  structure Crash = Crash
				  structure PP = PP)

    structure BackendInfo = BackendInfo(structure Labels = Labels
					structure Lvars = Lvars
					structure Lvarset = Lvarset
					structure HpPaRisc = HpPaRisc
					structure PP = PP
					structure Flags = Flags
					structure Report = Report
					structure Crash = Crash)

    structure BuildCompile = BuildCompile
      (structure TyName = Basics.TyName
       structure BackendInfo = BackendInfo
       structure Name = Name
       structure Lvars = Lvars
       structure Lvarset = Lvarset
       structure Labels = Labels
       structure Con = Con
       structure Excon = Excon
       structure FreeIds = FreeIds
       structure TopdecGrammar = TopdecGrammar
       structure ElabInfo = AllInfo.ElabInfo
       structure StatObject = Basics.StatObject
       structure Environments = Basics.Environments
       structure FinMap = Tools.FinMap
       structure IntFinMap = IntFinMap
       structure WordFinMap = Tools.WordFinMap
       structure FinMapEq = Tools.FinMapEq
       structure BasicIO = Tools.BasicIO
       structure Report = Report
       structure Flags = Flags
       structure PP = PP
       structure Crash = Crash
       structure Timing = Tools.Timing)

    structure Compile = BuildCompile.Compile
    structure CompileBasis = BuildCompile.CompileBasis
    structure CompilerEnv = BuildCompile.CompilerEnv

    structure JumpTables = JumpTables(structure BI = BackendInfo
				      structure Crash = Crash)

    structure HppaResolveJumps =
      HppaResolveJumps(structure HpPaRisc = HpPaRisc
		       structure Labels = Labels
   		       structure Crash = Crash
		       structure IntFinMap = IntFinMap)

    structure HpPaDelaySlotOptimization =
      HpPaDelaySlotOptimization(structure HpPaRisc = HpPaRisc
				structure Flags = Tools.Flags
				structure Crash = Tools.Crash)

    structure CodeGen = CodeGen(structure BI = BackendInfo
				structure HpPaRisc = HpPaRisc
				structure JumpTables = JumpTables
				structure HppaResolveJumps = HppaResolveJumps
				structure Con = Con
				structure Excon = Excon
				structure Lvars = Lvars
				structure Lvarset = Lvarset
				structure Labels = Labels
				structure CallConv = BuildCompile.CallConv
				structure LineStmt = BuildCompile.LineStmt
				structure SubstAndSimplify = BuildCompile.SubstAndSimplify
				structure PP = PP
				structure Flags = Tools.Flags
				structure Report = Tools.Report
				structure Crash = Tools.Crash)

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv

    type strdec = TopdecGrammar.strdec

    datatype target = OLDtarget of Compile.target
                    | NEWtarget of CodeGen.AsmPrg

    type linkinfo = Compile.linkinfo

    type label = Compile.label

    val code_label_of_linkinfo : linkinfo -> label = Compile.code_label_of_linkinfo
    val imports_of_linkinfo : linkinfo -> label list = Compile.imports_of_linkinfo
    val exports_of_linkinfo : linkinfo -> label list = Compile.exports_of_linkinfo
    val unsafe_linkinfo : linkinfo -> bool = Compile.unsafe_linkinfo

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    (****************************************************************)
    (* Add Dynamic Flags                                            *)
    (****************************************************************)
    val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Control","Lambda Backend"],x,y,r))
      [("delay_slot_optimization", "Delay Slot Optimization", ref true)]
    val dso_flag = Flags.lookup_flag_entry "delay_slot_optimization"
    val enable_lambda_backend = Flags.lookup_flag_entry "enable_lambda_backend"

    fun compile a =
      case Compile.compile a
	of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
	 | Compile.CodeRes(ce,cb,target,linkinfo,target_new) => 
	  if !enable_lambda_backend then
	    let 
	      val {main_lab, code, imports, exports, safe} = target_new
	      val _ = Tools.Timing.timing_begin()
	      val asm_prg = Tools.Timing.timing_end_res("CG",CodeGen.CG target_new)
	      val asm_prg_dso =
		if !dso_flag then
		  (Tools.Timing.timing_begin();
		   Tools.Timing.timing_end_res("DSO",HpPaDelaySlotOptimization.DSO asm_prg))
		else
		  asm_prg
	      val linkinfo = Compile.mk_linkinfo {code_label=main_lab,
						  imports=(#1 imports) @ (#2 imports), (* Merge MLFunLab and DatLab *)
						  exports=(#1 exports) @ (#2 exports), (* Merge MLFunLab and DatLab *)
						  unsafe=not(safe)}
	    in 
	      CodeRes(ce,cb,NEWtarget asm_prg_dso,linkinfo)
	    end
	  else 
	    CodeRes(ce,cb,OLDtarget target,linkinfo)

    fun generate_link_code (labs : label list) : target =
      if !enable_lambda_backend then 
	if !dso_flag then
	  NEWtarget(HpPaDelaySlotOptimization.DSO (CodeGen.generate_link_code labs))
	else
	  NEWtarget(CodeGen.generate_link_code labs)
      else 
	OLDtarget(Compile.generate_link_code labs)

    fun emit {target:target, filename:string} : unit =
      case target
	of OLDtarget t => Compile.emit {target=t, filename=filename}
	 | NEWtarget t => CodeGen.emit (t, filename)
  end;
