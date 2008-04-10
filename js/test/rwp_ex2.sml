
local
open RWP
infix *** &&& >>>

val _ = print ("<html></head></head><body><h1>Reactive Web Programming Example: Sine</h1>" ^
               "<div id='box'>Sine</div>" ^
               "</body></html>")

val elemB = 
    case Js.getElementById Js.document "box" of 
      SOME e => e
    | NONE => raise Fail "no box element in DOM"

val() = Js.setStyle elemB ("position", "absolute")
val() = Js.setStyle elemB ("border", "solid black 1px")


val t = timer 20

val x : int b = 
    arr (fn x => 
            let val s = 2000
                val t = IntInf.toInt (Time.toMilliseconds x mod (IntInf.fromInt s))
                val r = real t / real s
            in round (200.0 * Math.pi * r)
            end) t

val y : int b = 
    arr (fn x => round (300.0 + 200.0 * Math.sin (real x / 100.0))) x

fun setPos s (bp:(int*int)b) : unit =
    let fun toPx x = Int.toString x ^ "px"
    in setStyle s ("left", arr (toPx o #1) bp)
     ; setStyle s ("top", arr (toPx o #2) bp)
    end

val _ = setPos "box" (pair(x,y))

in
end
