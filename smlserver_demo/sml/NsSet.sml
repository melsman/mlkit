signature NS_SET =
  sig
    type set

    (* get the first value associated with a key, if present *)
    val get : set * string -> string option
    val getOpt : set * string * string -> string

    (* Return the current size of a set *)
    val size : set -> int

    (* Check if a key in a set is unique, case sensitive *)
    val unique : set * string -> bool       

    (* Return the key name of a field *)
    val key : set * int -> string option    

    (* Return the value of a field *)
    val value : set * int -> string option

    (* Return the list representation of a set *)
    val list : set -> (string * string) list

    (* Return the elements that satisfy the property *)
    val filter : (string * string -> bool) -> set -> (string * string) list

    (* Fold over a set *)
    val foldl : ((string * string) * 'a -> 'a) -> 'a -> set -> 'a
    val foldr : ((string * string) * 'a -> 'a) -> 'a -> set -> 'a
  end

structure NsSet : NS_SET =
  struct
    type set = int

    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)

    fun get (s :set, key: string): string option =
      let val res : string = prim("nssml_SetGet", "nssml_SetGet", (s,key))
      in if isNull res then NONE
	 else SOME res
      end
    fun getOpt (s:set, key:string, dflt:string): string =
      Option.getOpt(get (s, key), dflt)
    fun put (s: set, key: string, value: string) : unit = 
      prim("nssml_SetPut", "nssml_SetPut", (s,key,value))
    fun free (s: set) : unit =
      prim("Ns_SetFree", "Ns_SetFree", s)
    fun create (name: string) : set =
      prim("nssml_SetCreate", "nssml_SetCreate", name)
    fun size (s: set) : int =
      prim("nssml_SetSize", "nssml_SetSize", s)
    fun unique (s: set, key: string) : bool =
      prim("nssml_SetUnique", "nssml_SetUnique", (s,key))

    fun key (s: set, index: int) : string option =
      let val res : string = prim("nssml_SetKey", "nssml_SetKey", (s, index))
      in if isNull res then NONE
	 else SOME res
      end

    fun value (s: set, index: int) : string option =
      let val res : string = prim("nssml_SetValue", "nssml_SetValue", (s, index))
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
    fun list s = foldl (op ::) nil s
    fun filter p s = foldl (fn (pair,a) => if p pair then pair :: a else a) nil s
  end
