exception K
exception E
exception B

fun print (s:string) : unit = prim("printStringML", "printStringML", s)

val a : string = ("**wrong**"; raise E) handle K => "**also wrong**"
                                             | E => "**ok**"
                                             | B => "**also also wrong**"

val _ = print a