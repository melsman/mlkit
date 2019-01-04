infix ^
fun implode (chars : char list) : string = prim ("implodeCharsML", chars)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun print (x : string) : unit = prim ("printStringML", x)

val _ = print ("Hello " ^ " You world " ^ "1" ^ "2\n")
val _ = print (implode [#"a",#"b",#"e"] ^ "\n")
