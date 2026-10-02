fun sample () : unit = prim ("mlkit_rp_sample", ())
val shared = Array.array(4,0)
fun work i =
    let fun loop 0 = i
          | loop n = let val a = Array.array(3,n)
                         val _ = if n = 1 then sample () else ()
                     in Array.update(shared,i,Array.sub(a,0)); loop(n-1) end
    in loop 1000 end
fun launch 0 = 0
  | launch n = Thread.spawn (fn () => work(n-1)) (fn t => launch(n-1)+Thread.get t)
val _ = if launch 4 = 6 then print "parallel allocation ok\n" else raise Fail "parallel"
