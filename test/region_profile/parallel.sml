val shared = Array.array(16,0)
fun work i =
    let fun loop 0 = i
          | loop n = let val a = Array.tabulate(3000, fn j => j+n)
                         val _ = Array.update(shared,i,Array.sub(a,2999))
                         val _ = if n mod 10 = 0 then RegionProfile.sample() else ()
                     in loop(n-1) end
    in loop 50 end
fun launch 0 = (RegionProfile.sample(); 0)
  | launch n = Thread.spawn (fn () => work(n-1)) (fn t => launch(n-1)+Thread.get t)
val result = launch 12
val _ = if result = 66 andalso Array.sub(shared,0) = 3000 then print "stress ok\n" else raise Fail "stress"
