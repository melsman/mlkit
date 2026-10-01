fun block () : int = prim("rp_foreign_block", ())
fun ready () : int = prim("rp_foreign_ready", ())
fun iterations () : int = prim("rp_iterations", ())
fun wait () = if ready() = 1 then () else wait()
fun loop (0,a) = a | loop (n,a) = loop(n-1,a+1)
val n = iterations()
val result = Thread.spawn block (fn t => (wait(); loop(n,0)+Thread.get t))
val _ = if result = n+1 then print "foreign wait ok\n" else raise Fail "foreign result"
