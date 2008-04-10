
val _ = print ("<html><body><h1>Temperature Conversion</h1>" ^ 
               "<table border='1'><tr><th align='left'>Temperature in Celcius:</th><td><input type='text' id='tC'></td></tr>" ^
               "<tr><th align='left'>Temperature in Fahrenheit:</th><td><div id='tF'>?</div></td></tr></table></body></html>")

fun get id = 
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("Missing id in document: " ^ id)

val elemC = get "tC"

val () = Js.setStyle elemC ("backgroundColor", "green")

fun comp () = 
    let val v = Js.value elemC
        val res = 
            case Int.fromString v of
              NONE => "Err"
            | SOME i => Int.toString(9 * i div 5 + 32)
        val elemF = get "tF"
    in Js.innerHTML elemF res; false
    end

val () = Js.installEventHandler elemC Js.onchange comp
    

