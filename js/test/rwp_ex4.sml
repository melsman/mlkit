
local
open RWP
infix *** &&& >>>

val _ = print ("<html></head></head><body><h1>Reactive Web Programming Examples</h1>" ^
               "<h2>Seconds since start (s)</h2>" ^
               "<span id='s'>?</span>" ^
               "<h2>(s + 1) + (s + 1 + 1)</h2>" ^
               "<span id='p'>?</span>" ^
               "<h2>Changes in the above (should equal 's' according to semantics)</h2>" ^
               "<span id='d'>?</span>" ^
               "</body></html>")

val t0 = Time.toSeconds(Time.now())

val timer_b = timer 100

val s = arr (fn t => Time.toSeconds t - t0) timer_b

val _ = insertDOM "s" (arr IntInf.toString s)

val p = arr (op +) (pair(arr (fn s => s + 1) s,(arr (fn s => s + 1) >>> arr (fn s => s + 1)) s))

val _ = insertDOM "p" (arr IntInf.toString p)

val d = hold 0 (fold (fn (_,n) => n+1) 0 (changes p))

val _ = insertDOM "d" (arr Int.toString d)

in
end
