
functor Dynlib(Cache : WEB_CACHE) :> WEB_DYNLIB = 
  struct
    datatype flag = NOW | LAZY
    type ForeignLib = int

    local 
    fun isNull(s : string) : bool = prim("__is_null", s)

    val myLibCache = Cache.get(Cache.Option Cache.String, Cache.Int, "__WEBDYNLIB_CACHE1",
                               Cache.WhileUsed (NONE,NONE))
    val mySymCache = Cache.get(Cache.String, Cache.Unit, "__WEBDYNLIB_CACHE2",
                               Cache.WhileUsed (NONE,NONE))
    in
    fun dlopen (lib : string option, flag : flag, global : bool) = 
            case Cache.lookup myLibCache lib 
            of SOME i => i
             | NONE =>
            let val b0 = case flag 
                         of NOW => Word.fromInt 0
                          | LAZY => Word.fromInt 1
                val b1 = if global then Word.fromInt 2 else Word.fromInt 0
                val (b2,lib') = case lib of NONE => (Word.fromInt 4, "")
                                         | SOME l => (Word.fromInt 0, l)
                val (b : int, s: string) = 
                         prim("sml_dlopen", (lib', Word.toInt (Word.orb(Word.orb(b0,b1),b2))))
            in if b=0 then (if isNull s then raise Fail "Unknown error" else raise Fail s) 
               else (Cache.insert (myLibCache, lib, b, NONE) ; b)
            end
    fun dlsym (primname : string, cname : string, lib : int) = 
            case Cache.lookup mySymCache primname
            of SOME _ => ()
             | NONE => 
            let val a : string = prim("resolveFun", (primname, cname, lib))
            in if isNull a then (Cache.insert (mySymCache, primname, (), NONE); ()) else raise Fail a
            end
    fun isLinked (name : string) = 
            let val a : int = prim("@isResolvedFun", name)
            in a = 1
            end
    end
  end
