signature NS_CACHE =
  sig
    type cache
      
    (* Create a cache, given a cache name and a 
     * timeout value in seconds. *)
    val createTm : string * int -> cache
      
    (* Create a cache, given a cache name and a 
     * maximum cache size in bytes. *)
    val createSz : string * int -> cache  
	  
    (* Find a cache, given a cache name. Returns 
     * NONE if cache does not exist. *)
    val find : string -> cache option

    (* findTm (cn,t) : As find, except that the cache 
       with name cn is created if it does not already
       exist. If the cache is created then t is used 
       as timeout value in seconds. *)
    val findTm : string * int -> cache

    (* findSz (cn,s) : As find, except that the cache 
       with name cn is created if it does not already
       exist. If the cache is created then s is used 
       as size in bytes. *)
    val findSz : string * int -> cache

    (* Deletes all entries in cache. *)
    val flush : cache -> unit

    (* Associate a key with a value in the cache; 
     * Overwrites existing entry in cache if 
     * entry is present, in which case the 
     * function returns `false'. If no previous 
     * entry for the key is present in the cache, 
     * the function returns `true'. *)
    val set : cache * string * string -> bool

    (* Returns value associated with key in cache; 
     * returns NONE if key does not exist in 
     * cache. *)
    val get : cache * string -> string option

    (* cacheForAwhile (f,cn,t): given a function f that maps a string
       to a string, a cache name cn and a timeout value in seconds t,
       a new function f' is returned. f' is equal to f except that the
       results are cached and only recalculated when the cached
       results are older than the timeout value.  This can for
       instance be used to cache fetched HTML pages from the
       Internet. The timestamp is not renewed when items are
       accessed. This is not what you get with createTm, and is
       therefore simulated explicitly (i.e., this is a little slower
       than cacheWhileUsed). *)
    val cacheForAwhile : 
      (string -> string) * string * int -> string -> string

    (* cacheWhileUsed (f,cn,t): as casheForAwhile, except
       that the timestamp is renewed at each access. An item
       is removed from the cache if t seconds have passed after
       the last access. This is what you get with createTm. *)
    val cacheWhileUsed : 
      (string -> string) * string * int -> string -> string
  end

