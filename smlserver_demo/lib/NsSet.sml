structure NsSet : NS_SET =
  struct
    type set = int

    fun isNull(s : string) : bool = prim("nssml_isNullString", s)

    fun get (s :set, key: string): string option =
      let val res : string = prim("nssml_SetGet", (s,key))
      in if isNull res then NONE
	 else SOME res
      end
    fun getOpt (s:set, key:string, dflt:string): string =
      Option.getOpt(get (s, key), dflt)
    fun put (s: set, key: string, value: string) : unit = 
      prim("nssml_SetPut", (s,key,value))
    fun free (s: set) : unit =
      prim("Ns_SetFree", s)
    fun create (name: string) : set =
      prim("nssml_SetCreate", name)
    fun size (s: set) : int =
      prim("nssml_SetSize", s)
    fun unique (s: set, key: string) : bool =
      prim("nssml_SetUnique", (s,key))

    fun key (s: set, index: int) : string option =
      let val res : string = prim("nssml_SetKey", (s, index))
      in if isNull res then NONE
	 else SOME res
      end

    fun value (s: set, index: int) : string option =
      let val res : string = prim("nssml_SetValue", (s, index))
      in if isNull res then NONE
	 else SOME res
      end

    fun getPair(s,n) = 
      case (key(s,n), value(s,n))
	of (SOME k,SOME v) => (k,v)
      | _ => raise Fail "Ns.getPair"
    fun foldr (f:(string * string) * 'a -> 'a) (b:'a) (s:set) : 'a =
      let fun loop (n,acc) = if n < 0 then acc
			     else loop (n-1, f(getPair(s,n),acc))
      in loop (size s - 1, b)
      end
    fun foldl (f:(string * string) * 'a -> 'a) (b:'a) (s:set) : 'a =
      let fun loop (n,acc) = if n < 0 then acc
			     else f(getPair(s,n), loop(n-1,acc))
      in loop (size s - 1, b)
      end
    fun getAll (s:set, key:string): string list = 
      foldl (fn ((k,v),a) => if k = key then v :: a else a) nil s
    fun list s = foldl (op ::) nil s
    fun filter p s = foldl (fn (pair,a) => if p pair then pair :: a else a) nil s
  end
