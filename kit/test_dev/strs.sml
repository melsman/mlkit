infix ^
fun implode (chars : char list) : string = prim ("implodeCharsML", "implodeCharsProfilingML", chars)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", "concatStringProfilingML", (s, s'))
fun print (x:string):unit = prim("printStringML","printStringML",x)

val _ = print ("Hello " ^ " You world " ^ "1" ^ "2\n")
val _ = print (implode [#"a",#"b",#"e"] ^ "\n")