val _ =
let
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o

  fun print (s:string) : unit = prim("printStringML", s)

  fun printReal (n:real):unit = prim("printReal",n)

  val a = 1.0
  val b = 0.8
in
(*  val _ = printReal a
  val _ = printReal b*)
  printReal a
; printReal b
; if a>b then print "OK:gt\n" else print "ERR:gt\n"
; if a>a then print "ERR:gt2\n" else print "OK:gt2\n"
; if a<b then print "ERR:lt\n" else print "OK:lt\n"
; if a<a then print "ERR:lt2\n" else print "OK:lt2\n"
; if a<=b then print "ERR:lte\n" else print "OK:lte\n"
; if a<=a then print "OK:lte2\n" else print "ERR:lte2\n"
; if a>=b then print "OK:gte\n" else print "ERR:gte\n"
; if a>=a then print "OK:gte2\n" else print "ERR:gte2\n"
end
