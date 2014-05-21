
(* Build-file for the dummy compiler. It returns a compile basis structure
 * and a compile structure. 
 *)

signature BUILD_COMPILE = 
  sig
    structure CompilerEnv : COMPILER_ENV
    structure CompileBasis: COMPILE_BASIS
    structure Compile: COMPILE
  end  

functor BuildCompile (
		      structure Labels : ADDRESS_LABELS
		      structure Con : CON
		      structure Excon : EXCON
		      structure TopdecGrammar: TOPDEC_GRAMMAR
		      structure Environments : ENVIRONMENTS
			sharing type Environments.id = TopdecGrammar.id
			sharing type Environments.longid = TopdecGrammar.DecGrammar.longid
			sharing type Environments.longtycon = TopdecGrammar.longtycon
			sharing type Environments.longstrid = TopdecGrammar.longstrid
			sharing type Environments.strid = TopdecGrammar.strid
		      structure PP: PRETTYPRINT
			sharing type PP.StringTree = TopdecGrammar.StringTree
		      structure Crash: CRASH
                  )  (* : BUILD_COMPILE *) = 
  struct

    structure TyName = StatObject.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure SCon = DecGrammar.SCon
    structure Lab = DecGrammar.Lab
    structure TyVar = DecGrammar.TyVar
    structure Ident = DecGrammar.Ident
    structure StrId = DecGrammar.StrId
    structure TyCon = DecGrammar.TyCon


    structure CompilerEnv =
      CompilerEnv(structure Ident = Ident
		  structure StrId = StrId
		  structure Environments = Environments
                  structure TyCon = TyCon
                  structure TyVar = TyVar
                  structure TyName = TyName
                  structure PP = PP
                  structure Crash = Crash
                 )

    structure CompileBasis =
      CompileBasis(structure TyName = TyName
		   structure PP = PP)


    structure Compile =
      Compile(type strdec = TopdecGrammar.strdec
	      structure CompilerEnv = CompilerEnv
	      structure CompileBasis = CompileBasis
	      structure PP = PP)
  end
