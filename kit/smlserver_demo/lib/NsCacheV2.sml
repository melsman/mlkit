signature NS_CACHE_V2 = 
  sig
    (* Cache kinds *)
    datatype kind =
      WhileUsed of int
    | ForAWhile of int
    | Size of int

    (* Cache Type *)
    type ('a,'b) cache
    type 'a Type
    type name = string

    (* Get or create a cache *)
    val get : name * kind * 'a Type * 'b Type -> ('a,'b) cache

    (* Entries in a cache *)
    val lookup : ('a,'b) cache -> 'a -> 'b option
    val insert : ('a,'b) cache * 'a * 'b -> bool
    val flush  : ('a,'b) cache -> unit

    (* Memorization *)
    val cache  : ('a,'b) cache -> ('a -> 'b) -> 'a -> 'b

    (* Build cache types out of pre defined cache types *)
    val pair   : 'a Type -> 'b Type -> ('a*'b) Type
    val triple : 'a Type -> 'b Type -> 'c Type -> (('a*'b)*'c) Type

    (* Cache info *)
    val pp_type  : 'a Type -> string
    val pp_cache : ('a,'b) cache -> string

    (* Pre defined cache types *)
    val Int    : int Type
    val Real   : real Type
    val Bool   : bool Type
    val Char   : char Type
    val String : string Type
  end

(* 
 [kind] abstract type for cache kind. A cache kind describes the
 strategy used by the cache to insert and emit cache entries. The
 following strategies are supported:

     * WhileUsed t : elements are emitted from the cache after
       approximately t seconds after the last use.

     * ForAWhile t : elements are emitted from the cache after
       approximately t seconds after they were inserted.

     * Size n : the cache has a maximum size of n bytes. Elements are
       emitted as needed in order to store new elements. The size n
       may not be too small, a minimum size of 1 Kb seems to work fine
       for small caches; however it may also be much larger.

    [('a,'b) cache] abstract type of cache. A cache is a mapping from keys of
    type 'a to elements of type 'b. Only values of type 'a Type and 'b
    Type can be used as keys and elements

    ['a Type] abstract type of either a key or element that can be
    used in a cache.

    [name] abstract type of the name of a cache.

    [get (cn,ck,aType,bType)] returns a cache which is named
    cn. The cache will be a mapping from keys of type aType into
    elements of type bType. The cache strategy is described by ck. 

      * If no cache exists with name cn, then a new cache is created.

      * If a cache c exists with name cn, then there are two
        possibilities to consider:

          1) If c is a mapping from aType to bType, then c is
             returned.
          
          2) If c is not a mapping from aType to bType, then a new
             cache c' is created and returned.

     It is possible to create two caches with the same name, but only
     if they describe mappings of different type.

    [lookup c k] returns value associated with key k in cache c;
    returns NONE if key is not in cache.

    [insert (c,k,v)] associates a key k with a value v in the cache c;
    overwrites existing entry in cache if k is present, in which case
    the function returns false. If no previous entry for the key is
    present in the cache, the function returns true.

    [flush c] deletes all entries in cache c.

    [cache c f] implements memorisation on the function f. The
    function f must be a mapping of keys and elements that can be
    stored in a cache, that is, of type 'a Type.

    [pair aType bType] returns the pair type representing the pairs
    (a,b) where a is of type aType and b is of type bType.

    [triple aType bType cType] similar to pair except that the triple
    is represented with as one pair embedded in another pair:
    ((a,b),c) where a is of type aType, b is of type bType and c is of
    type cType.

    [pp_type aType] pretty prints the type aType.

    [pp_cache c] pretty prints the cache.

    (* Pre defined cache types *)
    [Int] predefined cache type representing integers.
    [Real] predefined cache type representing reals.
    [Bool] predefined cache type representing booleans.
    [Char] predefined cache type representing characters.
    [String] predefined cache type representing strings.

*)

structure NsCacheV2 :> NS_CACHE_V2 =
  struct
    datatype kind =
      WhileUsed of int
    | ForAWhile of int
    | Size of int
    type name = string
    type 'a Type = {name: string,
		    to_string: 'a -> string,
		    from_string: string -> 'a}

    type ('a,'b) cache = {name: string,
			  kind: kind,
			  domType: 'a Type,
			  rangeType: 'b Type,
			  cache: Ns.Cache.cache}

    (* Cache info *)
    fun pp_kind kind =
      case kind of
	WhileUsed t => "WhileUsed(" ^ (Int.toString t) ^ ")"
      | ForAWhile t => "ForAWhile(" ^ (Int.toString t) ^ ")"
      | Size n => "Size(" ^ (Int.toString n) ^ ")"

    fun pp_type (t: 'a Type) = #name t
    fun pp_cache (c: ('a,'b)cache) = 
      "[name:" ^ (#name c) ^ ",kind:" ^ (pp_kind(#kind c)) ^ 
      ",domType: " ^ (pp_type (#domType c)) ^ 
      ",rangeType: " ^ (pp_type (#rangeType c)) ^ "]"

    fun get (name,kind,domType:'a Type,rangeType: 'b Type) =
      let
	fun pp_kind kind =
	  case kind of
	    WhileUsed t => "WhileUsed"
	  | ForAWhile t => "ForAWhile"
	  | Size n => "Size"
	val c_name = name ^ (pp_kind kind) ^ #name(domType) ^ #name(rangeType)
	val cache = 
	  case kind of
	    Size n => Ns.Cache.findSz(c_name,n)
	  | WhileUsed t => Ns.Cache.findTm(c_name,t)
	  | ForAWhile t => Ns.Cache.findTm(c_name,t)
      in
	{name=c_name,
	 kind=kind,
	 domType=domType,
	 rangeType=rangeType,
	 cache=cache}
      end

    local
      open Time
      fun getWhileUsed (c: ('a,'b) cache) k =
	Ns.Cache.get(#cache c,#to_string(#domType c) k)
      fun getForAWhile (c: ('a,'b) cache) k t =
	case Ns.Cache.get(#cache c,#to_string(#domType c) k) of
	  NONE => NONE
	| SOME t0_v => 
	    (case scan Substring.getc (Substring.all t0_v)
	       of SOME (t0,s) => 
		 (case Substring.getc s
		    of SOME (#":",v) => 
		      if now() > t0 + (fromSeconds t)
			then NONE 
		      else SOME (Substring.string v)
		  | _ => NONE)
	     | NONE => NONE)
    in
      fun lookup (c:('a,'b) cache) (k: 'a) =
	let
	  val v = 
	    case #kind c of
	      Size n => Ns.Cache.get(#cache c,#to_string(#domType c) k)
	    | WhileUsed t => getWhileUsed c k 
	    | ForAWhile t => getForAWhile c k t
	in
	  case v of
	    NONE => NONE
	  | SOME s => SOME ((#from_string (#rangeType c)) s)
	end
    end

    fun insert (c: ('a,'b) cache, k: 'a, v: 'b) =
      case #kind c of
	Size n => Ns.Cache.set(#cache c,
			       #to_string (#domType c) k,
			       #to_string (#rangeType c) v)
      | WhileUsed t => Ns.Cache.set(#cache c,
				    #to_string (#domType c) k,
				    #to_string (#rangeType c) v)
      | ForAWhile t => Ns.Cache.set(#cache c,
				    #to_string(#domType c) k,
				    Time.toString (Time.now()) ^ ":" ^ ((#to_string (#rangeType c)) v))

    fun flush (c: ('a,'b) cache) = Ns.Cache.flush (#cache c)

    fun cache (c: ('a,'b) cache) (f:('a -> 'b)) =
      (fn k =>
       (case lookup c k of 
	  NONE => let val v = f k in (insert (c,k,v);v) end 
	| SOME v => v))

    fun pair (t1 : 'a Type) (t2: 'b Type) =
      let
	val name = "(" ^ (#name t1) ^ "," ^ (#name t2) ^ ")"
	fun to_string (a,b) = 
	  let
	    val a_s = (#to_string t1) a
	    val a_sz = Int.toString (String.size a_s)
	    val b_s = (#to_string t2) b
	  in
	    a_sz ^ ":" ^ a_s ^ b_s
	  end
	fun from_string s =
	  let
	    val s' = Substring.all s
	    val (a_sz,rest) = 
	      Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
	    val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
	    val (a_s,b_s) = (Substring.slice(rest,0,SOME a_sz),Substring.slice(rest,a_sz,NONE))
	    val a = (#from_string t1) (Substring.string a_s)
	    val b = (#from_string t2) (Substring.string b_s)
	  in
	    (a,b)
	  end
      in
	{name=name,
	 to_string=to_string,
	 from_string=from_string}
      end

    fun triple (t1 : 'a Type) (t2: 'b Type) (t3: 'c Type) = pair (pair t1 t2) t3

    (* Pre defined cache types *)
    val Int    = {name="Int",to_string=Int.toString,from_string=Option.valOf o Int.fromString}
    val Real   = {name="Real",to_string=Real.toString,from_string=Option.valOf o Real.fromString}
    val Bool   = {name="Bool",to_string=Bool.toString,from_string=Option.valOf o Bool.fromString}
    val Char   = {name="Char",to_string=Char.toString,from_string=Option.valOf o Char.fromString}
    val String = {name="String",to_string=(fn s => s),from_string=(fn s => s)}
  end