
signature EXECUTION =
  sig
    structure Elaboration: ELABORATION
    structure CompilerEnv: COMPILER_ENV
    structure CompileBasis: COMPILE_BASIS
    structure FreeIds : FREE_IDS
      sharing type FreeIds.topdec = Elaboration.ElabTopdec.PostElabTopdec
    structure Compile: COMPILE
      sharing type Compile.CompileBasis = CompileBasis.CompileBasis
      sharing type Compile.CEnv = CompilerEnv.CEnv
  end;


functor Execution(structure Elaboration : ELABORATION) : EXECUTION =
  struct
    structure Elaboration = Elaboration
    structure Basics      = Elaboration.Basics
    structure Tools       = Basics.Tools
    structure AllInfo     = Basics.AllInfo

    structure FreeIds = FreeIds
      (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure Environments = Basics.Environments
       structure ModuleEnvironments = Basics.ModuleEnvironments
       structure ElabInfo = AllInfo.ElabInfo
       structure Crash = Tools.Crash
       structure PP = Tools.PrettyPrint)

    structure BuildCompile = BuildCompile
      (structure TyName = Basics.TyName
       structure Name = Basics.Name
       structure FreeIds = FreeIds
       structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ElabInfo = AllInfo.ElabInfo
       structure StatObject = Basics.StatObject
       structure Environments = Elaboration.Basics.Environments
       structure FinMap = Tools.FinMap
       structure IntFinMap = Tools.IntFinMap
       structure WordFinMap = Tools.WordFinMap
       structure FinMapEq = Tools.FinMapEq
       structure BasicIO = Tools.BasicIO
       structure Report = Tools.Report
       structure Flags = Tools.Flags
       structure PP = Tools.PrettyPrint
       structure Crash = Tools.Crash
       structure Timing = Tools.Timing)

    structure Compile = BuildCompile.Compile
    structure CompileBasis = BuildCompile.CompileBasis
    structure CompilerEnv = BuildCompile.CompilerEnv

  end;
