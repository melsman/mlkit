val n = FormVar.wrapExn FormVar.getIntErr "n"
  handle _ => 10

fun fib 0 = 1
  | fib 1 = 1
  | fib n = n + fib (n-1)

val cache = 
  Ns.Cache.get (Ns.Cache.Int, 
		Ns.Cache.Int,
		"fib",
		Ns.Cache.WhileUsed 20)

(* Memorisation *)
fun fib_m 0 = 1 
  | fib_m 1 = 1
  | fib_m n = n + fib' (n-1)
and fib' n = (Ns.Cache.memoize cache fib_m) n

val _ = Page.return "Caching Demonstration - Memorisation" `

  Result of fib ^(Int.toString n) is ^(Int.toString (fib n)).<p>

  Result of memorized fib ^(Int.toString n) is ^(Int.toString (fib_m n)).<p>

  Pretty printing the cache: 
  <pre>
  ^(Ns.Cache.pp_cache cache)
  </pre><p>

  Go back to <a href=cache.sml>Cache Demo Home Page</a>.`
