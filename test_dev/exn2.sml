exception E

fun print (s:string) : unit = prim("printStringML", "printStringML", s)

val a : string = ("**5**"; raise E) handle _ => "**9**"

val _ = print a