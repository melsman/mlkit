
val _ = print ("<html><body><h1>XMLrpc test</h1>" ^ 
               "<table border='1'><tr><th align='left'>Number:</th><td><input type='text' id='n'></td></tr>" ^
               "<tr><th align='left'>Neg(number):</th><td><div id='res'>?</div></td></tr></table><h2><div id='h'>Guest Comments</div></h2><div id='g'>?</div></body></html>")

fun call Ta Tr m a =
    XMLrpc.rpc Ta Tr {url="/demo/xmlrpc_test_server.sml",  (* http://www.smlserver.org *)
                      method=m} a

fun get id = 
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("Missing id in document: " ^ id)

val elemN = get "n"
        
val handler = 
 fn XMLrpc.ServerConnection s => ("SC:" ^ s)
  | XMLrpc.MethodInvocation(i,s) => ("MI:" ^ s)
  | XMLrpc.TypeConversion => "TC"
  | Fail s => ("Fail: " ^ s)
  | _ => "Exn"

fun comp () = 
    let val v = Js.value elemN
        val res = 
            case Int.fromString v of
              NONE => "Err"
            | SOME i => 
              let open XMLrpc
                  val r = call int int "neg" i
              in Int.toString r
              end handle e => handler e
        val elemR = get "res"
    in Js.innerHTML elemR res; false
    end

val () = Js.installEventHandler elemN Js.onchange comp
    
fun tag t s = "<" ^ t ^ ">" ^ s ^ "</" ^ t ^ ">"

val tr = tag "tr"
val td = tag "td"

fun m s = (Js.innerHTML (get "g") s; false)

fun guest_del(gid) =
    let open XMLrpc
    in call int bool "guest_del" gid
    end

fun redraw() =
     let open XMLrpc 
         val r = call int (list(pair(pair(int,string),pair(string,string)))) "guests" 0
         val (a,fs) = List.foldl (fn (((gid,n),(e,c)),(a,fs)) => 
                                     let val g = td (Int.toString gid)
                                         val gId = "b" ^ Int.toString gid
                                         val link = td ("<a href='mailto:" ^ e ^ "'>"^n^"</a>")
                                         val c = td c
                                         val act = td ("<input type='button' value='Delete' id='" ^ gId ^ "'>")
                                         fun f() = Js.installEventHandler (get gId) Js.onclick (fn() => (guest_del gid; redraw())) 
                                     in (tr (g ^ link ^ c ^ act) ^ a, f::fs)
                                     end)
                                 ("",nil) r
         val a = "<table border=1><tr><th>Id</th><th>Name</th><th>Comment</th><th>Action</th></tr>" ^ a ^ "</table>"
     in m a before List.app (fn f => f()) fs
     end handle e => m(handler e)

val () = Js.installEventHandler (get "h") Js.onmouseover redraw
