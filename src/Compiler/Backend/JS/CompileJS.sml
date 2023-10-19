(* Standard ML to JavaScript Compiler *)

signature COMPILE_JS =
  sig
    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    include COMPILE_GEN
    where type target =
               ExpToJs.Js * {exports: string list,
                             imports: string list}

    (* emit: returns the filename for the generated .js file *)
    val emit : {target: target, filename: string} -> string
  end


structure CompileJS: COMPILE_JS =
  struct
    structure CompBasis = CompBasisToLamb
    structure PP = PrettyPrint

    structure CE = CompilerEnv
    type lvar = CE.lvar

    type jse = ExpToJs.Env.t

    type CompBasis = CompBasis.CompBasis * jse
    type CEnv = CompileToLamb.CEnv
    type strdec = CompileToLamb.strdec
    type funid = CompileToLamb.funid
    type strid = CompileToLamb.strid
    type longid = CompileToLamb.longid
    type Env = CompileToLamb.Env
    type strexp = CompileToLamb.strexp
    type LambdaPgm = CompileToLamb.target
    type target = ExpToJs.Js * {imports:string list, exports:string list}

    val preHook = CompileToLamb.preHook
    val postHook = CompileToLamb.postHook

    (*****************************)
    (* This is the main function *)
    (*****************************)

    datatype res = CodeRes of CEnv * CompBasis * target * bool
                 | CEnvOnlyRes of CEnv

    fun compile fe (CEnv, (Basis, jse), strdecs) : res =
        case CompileToLamb.compile fe (CEnv,Basis,strdecs)
         of CompileToLamb.CEnvOnlyRes CEnv1 => CEnvOnlyRes CEnv1
          | CompileToLamb.CodeRes (CEnv1, Basis1, lamb_opt, safe) =>
            let val exports = ExpToJs.exports lamb_opt
                val imports = ExpToJs.imports lamb_opt
                val (js,jse1) = ExpToJs.toJs (jse,lamb_opt)
            in CodeRes (CEnv1, (Basis1, jse1), (js, {imports=imports,exports=exports}), safe)
            end

    fun emit {target: target, filename} : string =
	let fun repair f =
                let val {dir,file} = OS.Path.splitDirFile f
                    val file = String.translate (fn #"." => "-" | c => Char.toString c) file
                in OS.Path.joinDirFile{dir=dir,file=file}
                end
            val filename = repair filename ^ ".js"
        in ExpToJs.toFile (filename, #1 target)
         ; print ("[wrote JavaScript file:\t" ^ filename ^ "]\n")
         ; filename
	end

    datatype cval = datatype CompileToLamb.cval
    fun retrieve_longid (CE:CEnv) (B',_) (longid:longid) : lvar cval =
        let (*val (B',_) = CompileBasisJS.de_CompileBasis B *)
        in CompileToLamb.retrieve_longid CE B' longid
        end

  end
