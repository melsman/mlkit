val n = FormVar.wrapExn FormVar.getIntErr "n"
  handle _ => 10

fun fib 0 = 1
  | fib 1 = 1
  | fib n = n + fib (n-1)

val cache = 
  NsCacheV2.get ("fib",
		 NsCacheV2.WhileUsed 20,
		 NsCacheV2.Int, 
		 NsCacheV2.Int)

(* Memorisation *)
fun fib_m 0 = 1 
  | fib_m 1 = 1
  | fib_m n = n + fib' (n-1)
and fib' n = (NsCacheV2.memoize cache fib_m) n

val _ = Page.return "Caching Demonstration - Memorisation - V2 " `

  Result of fib ^(Int.toString n) is ^(Int.toString (fib n)).<p>

  Result of memorized fib ^(Int.toString n) is ^(Int.toString (fib_m n)).<p>

  Pretty printing the cache: 
  <pre>
  ^(NsCacheV2.pp_cache cache)
  </pre><p>

  Go back to <a href=cache_v2.sml>Cache Demo Home Page</a>.`
