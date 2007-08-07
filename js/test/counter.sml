
val _ = print "<html><body><h1>Counter</h1><h2><div id='counter'>Start Counter</div></h2></body></html>"

local 
  val c = ref 0
  val status : Js.intervalId option ref = ref NONE
in 

fun get id =
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("Missing id in document: " ^ id)

val e = get "counter"

fun up () = 
    let val c = !c before c := !c + 1
    in Js.innerHTML e (Int.toString c)
    end

fun toggle () =
    case !status of
      SOME id =>
      (Js.clearInterval id; 
       status := NONE; true)
    | NONE => 
      (status := (SOME(Js.setInterval 500 up)); true)
end

val () = Js.installEventHandler e Js.onclick toggle
    

