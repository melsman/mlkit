
functor ExecutionDummy(ExecutionArgs : EXECUTION_ARGS) : EXECUTION =
  struct
    open ExecutionArgs

    structure Basics = Elaboration.Basics
    structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
    structure Tools = Basics.Tools
    structure PP = Tools.PrettyPrint
    structure Crash = Tools.Crash

    structure TyName = Basics.ModuleEnvironments.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure TyVar = DecGrammar.TyVar
    structure Ident = DecGrammar.Ident
    structure StrId = DecGrammar.StrId
    structure TyCon = DecGrammar.TyCon

    structure CompilerEnv = CompilerEnvDummy(structure Ident = Ident
					     structure StrId = StrId
					     structure Environments = Basics.Environments
					     structure TyCon = TyCon
					     structure Flags = Tools.Flags
					     structure TyVar = TyVar
					     structure TyName = TyName
					     structure PP = PP
					     structure Crash = Crash)

    structure CompileBasis = CompileBasisDummy(structure TyName = TyName
					       structure PP = PP)

    val backend_name = "Dummy"
    val backend_longname = "Dummy (no output generated)"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type strdec = TopdecGrammar.strdec
    type label = Labels.label
    type linkinfo = unit
    type target = unit

    val code_label_of_linkinfo : linkinfo -> label = fn _ => Labels.new()
    val imports_of_linkinfo : linkinfo -> (label list * label list) = fn _ => (nil,nil)
    val exports_of_linkinfo : linkinfo -> (label list * label list) = fn _ => (nil,nil)
    val unsafe_linkinfo : linkinfo -> bool = fn _ => false

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv
    fun compile _ = CodeRes (CompilerEnv.emptyCEnv, CompileBasis.empty, (), ())
    val generate_link_code = NONE
    fun emit _ = ""
    fun link_files_with_runtime_system _ _ _ = ()

  end

