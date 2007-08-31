
structure J = JsCore

val _ = print "<h2>File exec.sml: Testing structure JsCore</h2>"


fun now() = 
    J.exec1 {stmt="return alert(a);", arg1=("a",J.string), res=J.string} s

fun alert (s:string) =
    J.call1 ("al" ^ "ert", J.string, J.string) s

fun now () =
    J.call1 ("Date.props", J.unit, J.string) ()

fun concat (x:string) (y:string) : string =
    J.exec2 {stmt="return a+b;", arg1=("a",J.string), arg2=("b",J.string), res=J.string} (x,y)

fun dplus (x:string) (y:string) : string =
    let val js = "return '' + (parseInt(a)+parseInt(b));"
    in J.exec2 {stmt=if true then js else "", arg1=("a",J.string),arg2=("b",J.string),res=J.string} (x,y)
    end

fun iplus (x:int) (y:int) : int =
    let val js = "return a+b;"
    in J.exec2 {stmt=if true then js else "", arg1=("a", J.int), arg2=("b", J.int), res=J.int} (x,y)
    end

fun println t s = print (t ^ ": " ^ s ^ "<br>")

val _ = println "test1" (now())

val _ = print "Test ended"


