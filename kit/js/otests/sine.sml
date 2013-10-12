val w = Js.openWindow "" "my window" 
  ("location=1,status=1,scrollbars=1,resizable=no,width=200,height=200," ^
   "menubar=no,toolbar=no")

val doc = Js.windowDocument w

val e = Js.documentElement doc

val () = Js.innerHTML e "<html></head></head><body id='body'></body></html>"

local
open Rwp
infix *** &&& >>>

val elemBody = 
    case Js.getElementById doc "body" of 
      SOME e => e
    | NONE => raise Fail "no body element in DOM"

fun newBox s =
    let val e = Js.createElement "div"
        val c = Js.createTextNode s
        val () = Js.appendChild e c
        val () = Js.setStyle e ("position", "absolute")
        val () = Js.setStyle e ("border", "solid blue 2px")
        val () = Js.appendChild elemBody e
    in e
    end

fun newSineBox A str =
    let val elemB = newBox str
        val t = timer 10
        val x : int b = 
            arr (fn x => 
                    let val s = 40 * A
                        val t = IntInf.toInt (Time.toMilliseconds x mod (IntInf.fromInt s))
                        val r = real t / real s
                    in round (400.0 * Math.pi * r)
                    end) t

        val y : int b = 
            arr (fn x => round (300.0 + real A * Math.sin (real x / 100.0))) x
            
        fun setPos e (bp:(int*int)b) : unit =
            let fun toPx x = Int.toString x ^ "px"
            in setStyle_elem e ("left", arr (toPx o #1) bp)
             ; setStyle_elem e ("top", arr (toPx o #2) bp)
            end
    in
      setPos elemB (pair(x,y))
    end

val () = newSineBox 100 "Kasper"
val () = newSineBox 200 "Rasmus"
val () = newSineBox 300 "Martin"
in
end
