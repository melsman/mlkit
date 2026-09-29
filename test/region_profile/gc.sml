val keep = Array.tabulate(10000, fn i => i)
fun churn 0 = ()
  | churn n = let val xs = List.tabulate(1000, fn i => i+n)
                  val _ = if List.length xs = 1000 then () else raise Fail "length"
              in churn(n-1) end
val _ = RegionProfile.sample()
val _ = churn 2000
val _ = RegionProfile.sample()
val _ = if Array.sub(keep,9999) = 9999 then print "gc profile ok\n" else raise Fail "lost array"
