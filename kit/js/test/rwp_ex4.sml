
local
open RWP
infix *** &&& >>>

val _ = print ("<html></head></head><body><h1>Reactive Web Programming Examples</h1>" ^
               "<h2>Time</h2>" ^
               "The time is <span id='timer'>not initialized</span>" ^
               "<h2>Text Input</h2>" ^
               "<input id='a' value='0'/> + <input id='b' value='0'/> = <span id='c'>?</span>" ^
               "<h2>Seconds since start</h2>" ^
               "<span id='s'>?</span>" ^
               "<h2>Seconds since start + Seconds since start</h2>" ^
               "<span id='p'>?</span>" ^
               "<h2>Changes in the above</h2>" ^
               "<span id='d'>?</span>" ^
               "</body></html>")

val t0 = Time.toSeconds(Time.now())

val timer_b = timer 100

val bt = arr (Date.toString o Date.fromTimeLocal)

val _ = insertDOM "timer" (bt timer_b)

val si_T : (string,int,B)arr = arr (Option.valOf o Int.fromString)

val form = pair(textField "a",textField "b")

val T = (si_T *** si_T) >>> (arr op +) >>> (arr Int.toString)

val _ = insertDOM "c" (T form)

val s = arr (fn t => Time.toSeconds t - t0) timer_b

val _ = insertDOM "s" (arr IntInf.toString s)

val p = arr (op +) (pair((arr (fn s => s + 1) >>> arr (fn s => s - 1)) s,s))

val _ = insertDOM "p" (arr IntInf.toString p)

val d = hold 0 (fold (fn (_,n) => n+1) 0 (changes p))

val _ = insertDOM "d" (arr Int.toString d)



in
end
