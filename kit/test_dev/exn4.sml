
fun print (s:string) : unit = prim("printString", "printString", s)
fun exnName (e: exn) : string = prim("exnName", "exnNameProfiling", e)   (* exomorphic by copying *)

exception ThisIsAnException
infix ::
fun app f nil = nil
  | app f (x::xs) = (f x; app f xs)

fun pr_exn e = (print(exnName e); print "\n")

val _ = app pr_exn [Div,Match,Bind,Overflow,ThisIsAnException]

val _ = (raise Bind) 
  handle Match => print "***Error***\n" | Bind => print "***Success***\n" 

val _ = raise Div