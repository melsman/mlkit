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
  val c = ~2.3
  val d = ~0.4
in
(*  val _ = printReal a
  val _ = printReal b*)
  printReal a
; printReal b
; printReal c
; printReal d
; if ~d > 0.0 then print "OK:neg1\n" else print "ERR:neg1\n"
; if ~b < 0.0 then print "OK:neg2\n" else print "ERR:neg2\n"
; if ~b > ~0.9 then print "OK:neg3\n" else print "ERR:neg3\n"
; if ~b < ~0.7 then print "OK:neg4\n" else print "ERR:neg4\n"
; if abs c > 2.0 then print "OK:abs1\n" else print "ERR:abs1\n"
; if abs c < 2.5 then print "OK:abs2\n" else print "ERR:abs2\n"
; if abs b > 0.7 then print "OK:abs3\n" else print "ERR:abs3\n"
; if abs b < 0.9 then print "OK:abs4\n" else print "ERR:abs4\n"
end
