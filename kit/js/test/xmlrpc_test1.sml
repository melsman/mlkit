
val _ = print ("<html><body><h1>XMLrpc test</h1>" ^ 
               "<table border='1'><tr><th align='left'>Number:</th><td><input type='text' id='n'></td></tr>" ^
               "<tr><th align='left'>Neg(number):</th><td><div id='res'>?</div></td></tr></table></body></html>")

fun get id = 
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("Missing id in document: " ^ id)

val elemN = get "n"
        
fun comp () = 
    let val v = Js.value elemN
        val res = 
            case Int.fromString v of
              NONE => "Err"
            | SOME i => 
              let 
                open XMLrpc
                val r = rpc int int {url="http://www.smlserver.org/demo/xmlrpc_test_server.sml",
                                     method="neg"} i
              in Int.toString r
              end handle XMLrpc.ServerConnection s => ("SC:" ^ s)
                       | XMLrpc.MethodInvocation(i,s) => ("MI:" ^ s)
                       | XMLrpc.TypeConversion => "TC"
                       | Fail s => ("Fail: " ^ s)
                       | _ => "Exn"
        val elemR = get "res"
    in Js.innerHTML elemR res; false
    end

val () = Js.installEventHandler elemN Js.onchange comp
    

