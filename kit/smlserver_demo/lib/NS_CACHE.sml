signature NS_CACHE =
  sig
    (* [cache] type of cache. *)
    type cache
      
    (* [createTm (n, t)] creates a cache, given a cache name
     * n and a timeout value t in seconds. *)
    val createTm : string * int -> cache
      
    (* [createSz (n, sz)] creates a cache, given a cache name
     * n and a maximum cache size sz in bytes. *)
    val createSz : string * int -> cache  
	  
    (* [find n] returns a cache, given a cache name n. Returns 
     * NONE if no cache with the given name exists. *)
    val find : string -> cache option

    (* [findTm (cn,t)] as find, except that the cache with 
     * name cn is created if it does not already exist. If 
     * the cache is created then t is used as timeout value 
     * in seconds. *)
    val findTm : string * int -> cache

    (* [findSz (cn,s)] as find, except that the cache with 
     * name cn is created if it does not already exist. If 
     * the cache is created then s is used as size in 
     * bytes. *)
    val findSz : string * int -> cache

    (* [flush c] deletes all entries in cache c. *)
    val flush : cache -> unit

    (* [set (c,k,v)] associates a key k with a value v in 
     * the cache c; overwrites existing entry in cache if 
     * entry is present, in which case the function 
     * returns `false'. If no previous entry for the key 
     * is present in the cache, the function returns 
     * `true'. *)
    val set : cache * string * string -> bool

    (* [get (c,k)] returns value associated with key k in 
     * cache c; returns NONE if key is not in cache. *)
    val get : cache * string -> string option

    (* [cacheForAwhile (f,cn,t)] given a function f that 
     * maps a string to a string, a cache name cn and a 
     * timeout value in seconds t, a new function f' is 
     * returned. f' is equal to f except that the results 
     * are cached and only recalculated when the cached
     * results are older than the timeout value.  This can
     * for instance be used to cache fetched HTML pages 
     * from the Internet. The timestamp is not renewed 
     * when items are accessed. This is not what you get 
     * with createTm, and is therefore simulated 
     * explicitly (i.e., this is a little slower than 
     * cacheWhileUsed). *)
    val cacheForAwhile : 
      (string -> string) * string * int -> string -> string

    (* [cacheWhileUsed (f,cn,t)] as casheForAwhile, except
     * that the timestamp is renewed at each access. An item
     * is removed from the cache if t seconds have passed 
     * after the last access. This is what you get with 
     * createTm. *)
    val cacheWhileUsed : 
      (string -> string) * string * int -> string -> string
  end

