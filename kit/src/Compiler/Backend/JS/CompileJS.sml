(* Standard ML to JavaScript Compiler *)
      
signature COMPILE_JS =
  sig
    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    include COMPILE_GEN
    where type target = ExpToJs.Js

    (* emit: returns the filename for the generated .js file *)
    val emit : {target: target, filename: string} -> string  
  end 


structure CompileJS: COMPILE_JS =
  struct
    structure CompBasis = CompBasisToLamb
    structure PP = PrettyPrint

    structure CE = CompilerEnv

    type CompBasis = CompBasis.CompBasis
    type CEnv = CompileToLamb.CEnv
    type strdec = CompileToLamb.strdec
    type funid = CompileToLamb.funid
    type strid = CompileToLamb.strid
    type Env = CompileToLamb.Env
    type strexp = CompileToLamb.strexp
    type LambdaPgm = CompileToLamb.target
    type target = ExpToJs.Js

    fun die s = Crash.impossible ("CompileBarry." ^ s)

    val preHook = CompileToLamb.preHook
    val postHook = CompileToLamb.postHook

    (*****************************)
    (* This is the main function *)
    (*****************************)

    datatype res = CodeRes of CEnv * CompBasis * target * bool
                 | CEnvOnlyRes of CEnv

    fun compile fe (CEnv, Basis, strdecs) : res =
        case CompileToLamb.compile fe (CEnv,Basis,strdecs) 
         of CompileToLamb.CEnvOnlyRes CEnv1 => CEnvOnlyRes CEnv1
          | CompileToLamb.CodeRes (CEnv1, Basis1, lamb_opt, safe) =>
            CodeRes (CEnv1, Basis1, ExpToJs.toJs lamb_opt, safe)
          
    fun emit {target: target, filename} : string =
	let val filename = filename ^ ".js"
        in ExpToJs.toFile (filename, target)
         ; print ("[wrote JavaScript file:\t" ^ filename ^ "]\n")
         ; filename
	end

  end
