val w = Js.openWindow "" "my window" 
  ("location=1,status=1,scrollbars=1,resizable=no,width=200,height=200," ^
   "menubar=no,toolbar=no")

val doc = Js.windowDocument w

val e = Js.documentElement doc

val () = Js.innerHTML e 
  ("<html></head></head><body id='top'><h1 align=center>RWP Example: Sheeps in a Box</h1>" ^
   "<h2 align=center>Score: <span id='score'>0</span> &nbsp; Time: <span id='time'>0</span></h2>" ^
   "<h2 align=center><span id='msg'>?</span></h2>" ^
   "</body></html>")

fun getElem s =
    case Js.getElementById doc s of 
      SOME e => e
    | NONE => raise Fail ("no " ^ s ^ " element in DOM")

val scoreElem = getElem "score"
val msgElem = getElem "msg"
val timeElem = getElem "time"
val body = getElem "top"

local
open Rwp
infix *** &&& >>>

val time : int b = 
    let val t0 = Time.now()
    in arr (fn t => IntInf.toInt(Time.toMilliseconds(Time.-(t,t0)))) 
           (timer 20)
    end

fun mkBox e0 border =
    let val e = Js.createElement "div"
      val () = Js.appendChild e e0
      val () = Js.setStyle e ("position", "absolute")
      val () = if border then Js.setStyle e ("border", "solid black 3px")
               else ()
    in Js.appendChild body e; e
    end

local
  fun toPx x = Int.toString x ^ "px"
in
  fun setPos e (bp:(int*int)b) : unit =
      ( setStyle_elem e ("left", arr (toPx o #1) bp)
      ; setStyle_elem e ("top", arr (toPx o #2) bp)
      )
  fun setSize e (w,h) : unit =
      ( Js.setStyle e ("width", toPx w)
      ; Js.setStyle e ("height", toPx h)
      )
end

local
  fun newSheep ((xm,ym),a as (xs,ys)) =
    let val dx = real (xs - xm)
        val dy = real (ys - ym)
        val dm = 80.0
        val d = Math.sqrt(dx*dx + dy*dy)
    in if d < dm then
          let val f = dm / d
            val x = xm + round(dx * f)
            val y = ym + round(dy * f)
          in (abs x,abs y)
          end
       else a
    end

  val m = delay 200 (mouse_doc doc)
in

  (* mkSheep returns a sheep behavior and a DOM-installer, to be
   * called later with a stopable version of the sheep behavior. *)

  fun mkSheep n : (int*int)b * ((int*int)b -> unit) =
      let val e = Js.createElement "img"
          val () = Js.setAttribute e "src" "sheep.png"
          val e' = mkBox e false
          val sheepPos0 = (200 + 70 * n, 200 + 70 * n)
          val s = hold sheepPos0 (fold newSheep sheepPos0 (changes m))
      in (s, setPos e')
      end
end

val sheeps : ((int*int)b * ((int*int)b -> unit)) list = 
    List.tabulate (3, mkSheep)

val boxPos = (400,200)
val boxDim = (150,150)
val sheepDim = (50,50)

val sheepInBox : (int*int)b -> bool b =
    arr (fn (x,y) => x > #1 boxPos andalso x + #1 sheepDim < #1 boxPos + #1 boxDim
                     andalso 
                     y > #2 boxPos andalso y + #2 sheepDim < #2 boxPos + #2 boxDim) 

local
  val sheepsBox : bool list b = 
      list(map (sheepInBox o #1) sheeps)

  val sheepsInBox : bool b =
      arr (List.all (fn x => x)) sheepsBox

  val stop = sheepsInBox  (* Stop the game when all sheeps 
                           * are in the box. *)
in
  val sheepsInBox : bool b = 
      until (stop, sheepsInBox)

  val score : int b = 
      until (stop, arr (List.length o List.filter (fn x => x)) sheepsBox)

  val () = (* apply DOM-installers to stopable versions of the sheeps *) 
      List.app (fn (x,f) => 
                   let val x = until(stop,x)
                   in f x
                   end) sheeps
  val time = until (stop, time)
end

val boxElem = mkBox (Js.createTextNode "") true
val _ = setPos boxElem (const boxPos)
val _ = setSize boxElem boxDim

val msg = 
    iff (sheepsInBox,
         const "Good - <a href='rwp_ex3.html'>play again?</a>",
         const "Use your mouse to put the sheeps in the box!")

val boxColor =
    iff (sheepsInBox,
         const "green",
         const "white")

val () = insertDOM_elem scoreElem (arr Int.toString score)

val () = insertDOM_elem timeElem (arr Int.toString time)

val () = insertDOM_elem msgElem msg

val () = setStyle_elem boxElem ("backgroundColor", boxColor)
in
end
