
signature EXECUTION =
  sig
    structure CompilerEnv: COMPILER_ENV
    structure CompileBasis: COMPILE_BASIS
    structure Compile: COMPILE
      sharing type Compile.CompileBasis = CompileBasis.CompileBasis
      sharing type Compile.CEnv = CompilerEnv.CEnv
  end;


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

    structure InstsX86 = InstsX86(structure Labels = Labels
				  structure Lvars = Lvars
				  structure Crash = Crash
				  structure PP = PP)

    structure BackendInfo = BackendInfoX86(structure Labels = Labels
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

    structure CodeGen = CodeGenX86(structure BackendInfo = BackendInfo
				   structure InstsX86 = InstsX86
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

  end;
