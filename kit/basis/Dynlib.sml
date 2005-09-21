
structure Dynlib :> DYNLIB = 
  struct
    datatype flag = NOW | LAZY
    type ForeignLib = foreignptr

    local 
    fun isNull(s : string) : bool = prim("__is_null", s)
    fun isNullFP(s : foreignptr) : bool = prim("__is_null", s)

    in
    fun dlopen (lib : string option, flag : flag, global : bool) = 
            let val b0 = case flag 
                         of NOW => Word.fromInt 0
                          | LAZY => Word.fromInt 1
                val b1 = if global then Word.fromInt 2 else Word.fromInt 0
                val (b2,lib') = case lib of NONE => (Word.fromInt 4, "")
                                         | SOME l => (Word.fromInt 0, l)
                val (b : foreignptr, s: string) = 
                         prim("sml_dlopen", (lib', Word.toInt (Word.orb(Word.orb(b0,b1),b2))))
            in if isNullFP b then (if isNull s then raise Fail "Unknown error" else raise Fail s) 
               else b
            end
    fun dlsym (primname : string, cname : string, lib : foreignptr) = 
            let val a : string = prim("resolveFun", (primname, cname, lib))
            in if isNull a then () else raise Fail a
            end
    fun isLinked (primname : string) = 
            let val a : int = prim ("@isResolvedFun", primname)
            in a = 1
            end
    end
  end
