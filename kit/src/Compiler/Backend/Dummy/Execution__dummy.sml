
functor Execution(structure TopdecGrammar : TOPDEC_GRAMMAR
		  structure Labels : ADDRESS_LABELS
		  structure Basics : BASICS
		  sharing TopdecGrammar.StrId = Basics.StrId
		  sharing TopdecGrammar.SigId = Basics.SigId
		  sharing TopdecGrammar.FunId = Basics.FunId
                  sharing type TopdecGrammar.DecGrammar.tycon = Basics.TyCon.tycon
                  sharing type TopdecGrammar.DecGrammar.longtycon = Basics.TyCon.longtycon
                  sharing type TopdecGrammar.DecGrammar.tyvar = Basics.TyVar.SyntaxTyVar
                  sharing type TopdecGrammar.DecGrammar.id = Basics.Ident.id
                  sharing type TopdecGrammar.DecGrammar.longid = Basics.Ident.longid = 
		    Basics.ModuleEnvironments.longid
                  sharing type TopdecGrammar.DecGrammar.info = Basics.AllInfo.ElabInfo.ElabInfo
                  sharing type TopdecGrammar.DecGrammar.StringTree = Basics.Tools.PrettyPrint.StringTree
		    ) : EXECUTION =
  struct
    structure Tools       = Basics.Tools
    structure PP          = Tools.PrettyPrint
    structure Crash       = Tools.Crash

    structure TyName = Basics.ModuleEnvironments.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure TyVar = DecGrammar.TyVar
    structure Ident = DecGrammar.Ident
    structure StrId = DecGrammar.StrId
    structure TyCon = DecGrammar.TyCon

    structure CompilerEnv =
      CompilerEnv(structure Ident = Ident
		  structure StrId = StrId
		  structure Environments = Basics.Environments
                  structure TyCon = TyCon
		  structure Flags = Tools.Flags
                  structure TyVar = TyVar
                  structure TyName = TyName
                  structure PP = PP
                  structure Crash = Crash
                 )

    structure CompileBasis =
      CompileBasis(structure TyName = TyName
		   structure PP = PP)

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type strdec = TopdecGrammar.strdec
    type label = Labels.label
    type linkinfo = unit
    type target = unit

    val code_label_of_linkinfo : linkinfo -> label = fn _ => Labels.new()
    val imports_of_linkinfo : linkinfo -> label list = fn _ => nil
    val exports_of_linkinfo : linkinfo -> label list = fn _ => nil
    val unsafe_linkinfo : linkinfo -> bool = fn _ => false

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv
    fun compile _ = CodeRes (CompilerEnv.emptyCEnv, CompileBasis.empty, (), ())
    fun generate_link_code _ = ()
    fun emit _ = ()

  end;

