
(* Build-file for the compiler. It returns a compile basis structure
 * and a compile structure. 
 *)

signature BUILD_COMPILE_BARRY = 
  sig
    structure ExecutionArgs : EXECUTION_ARGS
    structure CompilerEnv : COMPILER_ENV
      sharing type CompilerEnv.ElabEnv = ExecutionArgs.Elaboration.Basics.Environments.Env
      sharing type CompilerEnv.id = ExecutionArgs.Elaboration.Basics.ModuleEnvironments.id
      sharing type CompilerEnv.longid = ExecutionArgs.Elaboration.PostElabTopdecGrammar.DecGrammar.longid
      sharing type CompilerEnv.strid = ExecutionArgs.Elaboration.Basics.StrId.strid
      sharing type CompilerEnv.longstrid = ExecutionArgs.Elaboration.Basics.StrId.longstrid
      sharing type CompilerEnv.tycon = ExecutionArgs.Elaboration.Basics.TyCon.tycon
      sharing type CompilerEnv.longtycon = ExecutionArgs.Elaboration.Basics.TyCon.longtycon
    structure CompBasis: COMP_BASIS_BARRY
    structure Compile: COMPILE_BARRY
      sharing type Compile.CompBasis = CompBasis.CompBasis
      sharing type Compile.CEnv = CompilerEnv.CEnv
      sharing type Compile.strdec = ExecutionArgs.Elaboration.PostElabTopdecGrammar.strdec
      sharing type Compile.strexp = ExecutionArgs.Elaboration.PostElabTopdecGrammar.strexp
      sharing type Compile.funid = ExecutionArgs.Elaboration.PostElabTopdecGrammar.funid
      sharing type Compile.strid = ExecutionArgs.Elaboration.PostElabTopdecGrammar.strid
      sharing type Compile.Env = ExecutionArgs.Elaboration.Basics.Environments.Env

      sharing type ExecutionArgs.Con.con = CompBasis.con = CompilerEnv.con
      sharing type ExecutionArgs.Excon.excon = CompBasis.excon = CompilerEnv.excon
      sharing type ExecutionArgs.Elaboration.Basics.TyName.TyName 
	= CompBasis.TyName = CompilerEnv.TyName
      sharing type ExecutionArgs.Elaboration.Basics.Tools.PrettyPrint.StringTree
	= CompBasis.StringTree = CompilerEnv.StringTree
      sharing type ExecutionArgs.Lvars.lvar = CompBasis.lvar = CompilerEnv.lvar
  end

functor BuildCompileBarry (ExecutionArgs : EXECUTION_ARGS) : BUILD_COMPILE_BARRY =
  struct
    structure ExecutionArgs = ExecutionArgs
    open ExecutionArgs
    structure Basics = Elaboration.Basics
    structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
    structure Tools = Basics.Tools
    structure PP = Tools.PrettyPrint
    structure Crash = Tools.Crash
    structure Flags = Tools.Flags
    structure FinMap = Tools.FinMap
    structure FinMapEq = Tools.FinMapEq
    structure Name = Basics.Name
    structure Report = Tools.Report
    structure IntFinMap = Tools.IntFinMap

    structure TyName = Basics.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure SCon = DecGrammar.SCon
    structure Lab = DecGrammar.Lab
    structure TyVar = DecGrammar.TyVar
    structure Ident = DecGrammar.Ident
    structure StrId = DecGrammar.StrId
    structure TyCon = DecGrammar.TyCon

    structure LambdaExp =
      LambdaExp(structure Lvars = Lvars
                structure Con = Con
                structure Excon = Excon
                structure TyName = TyName
                structure PP = PP
                structure Crash = Crash
                structure Flags = Flags)

    structure LambdaBasics = 
      LambdaBasics(structure Lvars = Lvars
		   structure Excon = Excon		     
		   structure TyName = TyName
		   structure PP = PP
		   structure TLE = LambdaExp
		   structure FinMap = FinMap
		   structure FinMapEq = FinMapEq
		   structure Crash = Crash
		   structure Flags = Flags)
      
    structure NatSet = NatSet(structure PP = PP)

    structure LambdaStatSem =
      LambdaStatSem(structure Lvars = Lvars
		    structure Con = Con
		    structure Excon = Excon
		    structure TyName = TyName
		    structure Name = Name
		    structure NatSet = NatSet
		    structure LambdaExp = LambdaExp
		    structure LambdaBasics = LambdaBasics
		    structure Crash = Crash
		    structure PP = PP
		    structure Flags = Flags)

    structure EliminateEq =
      EliminateEq(structure Lvars = Lvars
		  structure Con = Con
		  structure Name = Name
		  structure TyName = TyName
		  structure LambdaExp = LambdaExp
		  structure Crash = Crash
		  structure Flags = Flags
		  structure PP = PP
		  structure TyVarMap =
		    OrderFinMap(structure Order = struct type T = LambdaExp.tyvar
							 fun lt (a:T) b = LambdaExp.lt_tyvar(a,b)
						  end
				structure PP = PP
				structure Report = Report
				structure Crash = Crash))

    structure Stack = Stack()

    structure UnionFindPoly = UF_with_path_halving_and_union_by_rank()

    structure DiGraph = DiGraph(structure UF = UnionFindPoly
                                structure Stack = Stack
                                structure PP = PP
                                structure Flags = Flags
                                structure Crash = Crash)

    structure CompilerEnv =
      CompilerEnv(structure Ident = Ident
		  structure StrId = StrId
                  structure Con = Con
                  structure Excon = Excon
		  structure Environments = Basics.Environments
		  structure LambdaBasics = LambdaBasics
                  structure TyCon = TyCon
                  structure TyVar = TyVar
                  structure TyName = TyName
                  structure LambdaExp = LambdaExp
                  structure Lvars = Lvars
                  structure FinMap = FinMap
		  structure FinMapEq = FinMapEq
                  structure PP = PP
		  structure Flags = Flags
                  structure Crash = Crash
		  structure Report = Report)

    structure LvarDiGraphScc = 
      DiGraphScc(structure InfoDiGraph = 
		   struct
		     type nodeId = Lvars.lvar
		     type info = Lvars.lvar
		     type edgeInfo = unit
		     val lt = fn a => fn b => Lvars.lt(a,b)
		     fun getId lv = lv
		     val pu = Lvars.pu
		   end
		 structure PP = PP
		 structure Flags = Flags
		 structure Crash = Crash
		 structure Report = Report)

    structure OptLambda = 
      OptLambda(structure Lvars = Lvars
		structure LambdaExp = LambdaExp
		structure Name = Name
		structure IntFinMap = IntFinMap
		structure LvarDiGraphScc = LvarDiGraphScc
		structure LambdaBasics = LambdaBasics
		structure FinMap = FinMap
		structure BasicIO = Tools.BasicIO
		structure Con = Con
		structure Excon = Excon
		structure TyName = TyName
		structure Flags = Flags
		structure Crash = Crash
		structure PP = PP)

    structure CompBasis = CompBasisBarry
               (structure CompilerEnv = CompilerEnv
		structure Con = Con
		structure Lvars = Lvars
		structure Excon = Excon
		structure TyName = TyName
		structure LambdaStatSem = LambdaStatSem
		structure EliminateEq = EliminateEq
		structure OptLambda = OptLambda
		structure Report = Report
		structure PP = PP
		structure Flags = Flags)

    structure CompileDec = CompileDec(
			structure Ident = Ident
                        structure Lab = Lab
                        structure SCon = SCon
                        structure Con = Con
                        structure Excon = Excon
                        structure TyCon = TyCon
                        structure TyName = TyName
                        structure TyVar = TyVar
                        structure Grammar = DecGrammar
			structure TopdecGrammar = TopdecGrammar
                        structure StatObject = Basics.StatObject
			structure Environments = Basics.Environments
                        structure Lvars = Lvars
                        structure LambdaExp = LambdaExp
                        structure LambdaBasics = LambdaBasics
                        structure CompilerEnv = CompilerEnv
			structure ElabInfo = Basics.AllInfo.ElabInfo
                        structure FinMap = FinMap
                        structure FinMapEq = FinMapEq
                        structure PrettyPrint = PP
                        structure Report = Report
                        structure Flags = Flags
                        structure Crash = Crash)

    structure Compile = CompileBarry
             (structure Excon = Excon
	      structure FinMap = FinMap
	      structure Lvars = Lvars
	      structure Name = Name
	      structure LambdaExp = LambdaExp
	      structure LambdaStatSem = LambdaStatSem
	      structure EliminateEq = EliminateEq
	      structure CompilerEnv = CompilerEnv
	      structure CompileDec = CompileDec
	      structure OptLambda = OptLambda
	      structure CompBasis = CompBasis
	      structure Report = Report
	      structure Flags = Flags
	      structure PP = PP
	      structure Crash = Crash
	      structure Timing = Tools.Timing)
  end
