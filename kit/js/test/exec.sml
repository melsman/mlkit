
structure J = JsCore

val _ = print "<h2>File exec.sml: Testing structure JsCore</h2>"


fun alertWithExec (s:string) = 
    J.exec1 {stmt="return alert(a);", arg1=("a",J.string), res=J.string} s

fun alert (s:string) =
    J.call1 ("al" ^ "ert", J.string, J.string) s

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

val _ = println "test1" (if concat "5" "4" = "54" then "OK" else "ERR")
val _ = println "test2" (if dplus "5" "4" = "9" then "OK" else "ERR")
val _ = println "test3" (if iplus 5 4 = 9 then "OK" else "ERR")

val _ = print "Test ended"

val _ = alert "Hello"

val _ = alert "Hello there!!"

