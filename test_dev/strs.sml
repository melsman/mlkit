infix ^
fun implode (chars : char list) : string = prim ("implodeChars", "implodeCharsProfiling", chars)
fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))
fun print (x:string):unit = prim("printString","printString",x)

val _ = print ("Hello " ^ " You world " ^ "1" ^ "2\n")
val _ = print (implode [#"a",#"b",#"e"] ^ "\n")