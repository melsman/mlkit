signature NS_STRING_CACHE = sig
  type cache = int
  val createTm  : string * int -> cache    
  val createSz  : string * int -> cache  
  val find      : string -> cache option
  val findTm    : string * int -> cache
  val findSz    : string * int -> cache
  val flush     : cache -> unit
  val set       : cache * string * string -> bool
  val get       : cache * string -> string option

  val cacheForAwhile : (string -> string) * string * int 
                       -> string -> string
  val cacheWhileUsed : (string -> string) * string * int 
                       -> string -> string
end

(*
 [cache] abstract type of cache.

 [createTm (n, t)] creates a cache, given a cache name n and
 a cache timeout value t in seconds.

 [createSz (n, sz)] creates a cache, given a cache name n 
 and a maximum cache size sz in bytes.

 [find n] returns a cache, given a cache name n. Returns 
 NONE if no cache with the given name exists.

 [findTm (cn,t)] as find, except that the cache with name cn 
 is created if it does not already exist. If the cache is 
 created then t is used as cache timeout value in seconds.

 [findSz (cn,s)] as find, except that the cache with name cn 
 is created if it does not already exist. If the cache is 
 created then s is used as cache size in bytes.

 [flush c] deletes all entries in cache c.

 [set (c,k,v)] associates a key k with a value v in the 
 cache c; overwrites existing entry in cache if k is 
 present, in which case the function returns false. If no 
 previous entry for the key is present in the cache, the 
 function returns true.

 [get (c,k)] returns value associated with key k in cache c; 
 returns NONE if key is not in cache.

 [cacheForAwhile (f,cn,t)] where f is a function, cn is a 
 cache name, and t a cache timeout value in seconds. Returns
 a new function f' equal to f except that its results are 
 cached and only recalculated when the cached results are 
 older than the timeout value. This function can be used, for 
 instance, to cache fetched HTML pages from the Internet. The 
 timestamp is not renewed when items are accessed.

 [cacheWhileUsed (f,cn,t)] as casheForAwhile, except that 
 the timestamp is renewed at each access. An item is removed 
 from the cache if t seconds have passed after the last 
 access.
*)