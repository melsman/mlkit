signature NS_CACHE = 
  sig
    (* Cache kinds *)
    datatype kind =
      WhileUsed of int
    | TimeOut of int
    | Size of int

    (* Cache Type *)
    type ('a,'b) cache
    type 'a Type
    type name = string

    (* Get or create a cache *)
    val get : 'a Type * 'b Type * name * kind -> ('a,'b) cache

    (* Entries in a cache *)
    val lookup : ('a,'b) cache -> 'a -> 'b option
    val insert : ('a,'b) cache * 'a * 'b -> bool
    val flush  : ('a,'b) cache -> unit

    (* Memoization *)
    val memoize  : ('a,'b) cache -> ('a -> 'b) -> 'a -> 'b

    (* Build cache types out of pre defined cache types *)
    val Pair   : 'a Type -> 'b Type -> ('a*'b) Type
    val Option : 'a Type -> 'a option Type
    val List   : 'a Type -> 'a list Type
    val Triple : 'a Type -> 'b Type -> 'c Type -> ('a*'b*'c) Type

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

     * TimeOut t : elements are emitted from the cache after
       approximately t seconds after they were inserted.

     * Size n : the cache has a maximum size of n bytes. Elements are
       emitted as needed in order to store new elements. The size n
       may not be too small, a minimum size of 1 Kb seems to work fine
       for small caches; however it may also be much larger.

    [('a,'b) cache] abstract type of cache. A cache is a mapping from
    keys of type 'a to elements of type 'b. Only values of type 'a
    Type and 'b Type can be used as keys and elements

    ['a Type] abstract type of either a key or element that can be
    used in a cache.

    [name] abstract type of the name of a cache.

    [get (cn,ck,aType,bType)] returns a cache which is named cn. The
    cache will be a mapping from keys of type aType into elements of
    type bType. The cache strategy is described by ck.

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

    [memoize c f] implements memoization on the function f. The
    function f must be a mapping of keys and elements that can be
    stored in a cache, that is, f is of type 'a Type -> 'b Type.

    [Pair aType bType] returns the pair type representing the pairs
    (a,b) where a is of type aType and b is of type bType.

    [Option aType] returns the type aType option, representing a
    option where a is of type aType.

    [List aType] returns the list type representing the list of
    elements of type aType.

    [Triple aType bType cType] similar to Pair except that the triple
    is represented with as one Pair embedded in another Pair:
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
