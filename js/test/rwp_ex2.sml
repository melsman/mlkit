
local
open Rwp
infix *** &&& >>>

val _ = print ("<html></head></head><body id='body'></body></html>")

val elemBody = 
    case Js.getElementById Js.document "body" of 
      SOME e => e
    | NONE => raise Fail "no body element in DOM"

fun newBox id s =
    let val e = Js.createElement "div"
        val () = Js.setAttribute e "id" id
        val c = Js.createTextNode s
        val () = Js.appendChild e c
        val () = Js.setStyle e ("position", "absolute")
        val () = Js.setStyle e ("border", "solid blue 2px")
        val () = Js.appendChild elemBody e
    in e
    end

fun newSineBox A id str =
    let val elemB = newBox id str
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
            
        fun setPos s (bp:(int*int)b) : unit =
            let fun toPx x = Int.toString x ^ "px"
            in setStyle s ("left", arr (toPx o #1) bp)
             ; setStyle s ("top", arr (toPx o #2) bp)
            end
    in
      setPos id (pair(x,y))
    end

val () = newSineBox 100 "b1" "Kasper"
val () = newSineBox 200 "b2" "Rasmus"
val () = newSineBox 300 "b3" "Martin"
in
end
