val n = FormVar.wrapExn FormVar.getIntErr "n"
  handle _ => 10

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib(n-2)

val cache = 
  Web.Cache.get (Web.Cache.Int, 
		Web.Cache.Int,
		"fib",
		Web.Cache.WhileUsed (SOME(Time.fromSeconds 20), SOME(10000)))

(* Memorisation *)
fun fib_m 0 = 1 
  | fib_m 1 = 1
  | fib_m n = fib' (n-1) + fib' (n-2)
and fib' n = (Web.Cache.memoize cache fib_m) n

val _ = Page.return "Caching Demonstration - Memorisation" (`

  Result of fib ^(Int.toString n) is ^(Int.toString (fib n)).<p>

  Result of memorized fib ^(Int.toString n) is ^(Int.toString (fib_m n)).<p>

` ^^ (*`
  Pretty printing the cache: 
  <pre>
  ^(Web.Cache.pp_cache cache)
  </pre><p> ` ^^*) `

  Go back to <a href=cache.sml>Cache Demo Home Page</a>.`)
