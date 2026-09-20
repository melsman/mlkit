(* Timer.checkCPUTimes splits the CPU time into collector and
 * non-collector parts that add up to checkCPUTimer; checkGCTime is the
 * collector's user time. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
fun burn 0 acc = acc
  | burn n acc = burn (n - 1) (acc + n mod 7)
val c = Timer.startCPUTimer ()
val _ = burn 3000000 0
val {nongc, gc} = Timer.checkCPUTimes c
val {usr, sys} = Timer.checkCPUTimer c
val direct = Timer.checkGCTime c
val nonneg = Time.>= (#usr nongc, Time.zeroTime) andalso Time.>= (#sys nongc, Time.zeroTime)
             andalso Time.>= (#usr gc, Time.zeroTime) andalso Time.>= (#sys gc, Time.zeroTime)
val () = p ("non-negative: " ^ b nonneg)
val () = p ("total covers the parts: " ^ b (Time.>= (usr, Time.+ (#usr nongc, #usr gc)) andalso Time.>= (sys, Time.+ (#sys nongc, #sys gc))))
val () = p ("gc time within total: " ^ b (Time.<= (#usr gc, Time.+ (usr, sys))))
val () = p ("checkGCTime later is not smaller: " ^ b (Time.>= (direct, #usr gc)))
val () = p ("real timer runs: " ^ b (Time.>= (Timer.checkRealTimer (Timer.startRealTimer ()), Time.zeroTime)))
