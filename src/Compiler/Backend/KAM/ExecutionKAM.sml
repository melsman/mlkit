
functor Execution(structure TopdecGrammar : TOPDEC_GRAMMAR
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

(*
    structure InstsX86 = InstsX86(structure Labels = Labels
				  structure Lvars = Lvars
				  structure Crash = Crash
				  structure PP = PP)
*)

  structure BackendInfo = BackendInfoKAM(structure Labels = Labels
					   structure Lvars = Lvars
					   structure Lvarset = Lvarset
					   structure InstsX86 = InstsX86
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

(*
    structure JumpTables = JumpTables(structure BI = BackendInfo
				      structure Crash = Crash)
*)

(*
    structure CodeGen = CodeGenX86(structure BackendInfo = BackendInfo
				   structure InstsX86 = InstsX86
				   structure Con = Con
				   structure Excon = Excon
				   structure Lvars = Lvars
				   structure Lvarset = Lvarset
				   structure Labels = Labels
				   structure JumpTables = JumpTables
				   structure CallConv = BuildCompile.CallConv
				   structure LineStmt = BuildCompile.LineStmt
				   structure SubstAndSimplify = BuildCompile.SubstAndSimplify
				   structure PP = PP
				   structure Flags = Tools.Flags
				   structure Report = Tools.Report
				   structure Crash = Tools.Crash)
*)
    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type strdec = TopdecGrammar.strdec
(*    datatype target = NEWtarget of CodeGen.AsmPrg*)
    type target = unit
    type label = Compile.label

(*    type linkinfo = {code_label:label, imports: label list, exports : label list, unsafe:bool}*)
    type linkinfo = unit

(*    fun code_label_of_linkinfo (li:linkinfo) = #code_label li
    fun exports_of_linkinfo (li:linkinfo) = #exports li
    fun imports_of_linkinfo (li:linkinfo) = #imports li
    fun unsafe_linkinfo (li:linkinfo) = #unsafe li*)

    val code_label_of_linkinfo : linkinfo -> label = fn _ => Labels.new()
    val imports_of_linkinfo : linkinfo -> label list = fn _ => nil
    val exports_of_linkinfo : linkinfo -> label list = fn _ => nil
    val unsafe_linkinfo : linkinfo -> bool = fn _ => false


    fun mk_linkinfo a : linkinfo = a

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

(*    fun compile a =
      case Compile.compile a
	of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
	 | Compile.CodeRes(ce,cb,target_new) => 
	    let 
	      val {main_lab, code, imports, exports, safe} = target_new
	      val _ = Tools.Timing.timing_begin()
	      val asm_prg = Tools.Timing.timing_end_res("CG",CodeGen.CG target_new)
	      val linkinfo = mk_linkinfo {code_label=main_lab,
					  imports=(#1 imports) @ (#2 imports), (* Merge MLFunLab and DatLab *)
					  exports=(#1 exports) @ (#2 exports), (* Merge MLFunLab and DatLab *)
					  unsafe=not(safe)}
	    in 
	      CodeRes(ce,cb,NEWtarget asm_prg,linkinfo)
	    end*)
    fun compile _ = CodeRes (CompilerEnv.emptyCEnv, CompileBasis.empty, (), ())

(*    fun generate_link_code (labs : label list) : target =
      NEWtarget(CodeGen.generate_link_code labs)*)

(*    fun emit {target=NEWtarget t, filename:string} : unit =
      CodeGen.emit (t, filename)*)

    fun generate_link_code _ = ()
    fun emit _ = ()

  end;
