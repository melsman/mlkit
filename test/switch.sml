
let
infix <=
infix <
infix >
infix >=
fun print (s:string) : unit = prim("printString", "printString", s)
val y = 2
in
  case y of
    0 => print "It does not work 0"
  | 1 => print "It does not work 1"
  | 2 => print "It works!"
  | 3 => print "It does not work 3"
  | 4 => print "It does not work 4"
  | 5 => print "It does not work 5"
  | 6 => print "It does not work 6"

  | 30 => print "It does not work 30"
  | 31 => print "It does not work 31"
  | 32 => print "It does not work 32"
  | 33 => print "It does not work 33"
  | 34 => print "It does not work 34"
  | 35 => print "It does not work 35"
  | 36 => print "It does not work 36"

  | 130 => print "It does not work 130"
  | 131 => print "It does not work 131"
  | 132 => print "It does not work 132"
  | 133 => print "It does not work 133"
  | 134 => print "It does not work 134"
  | 135 => print "It does not work 135"
  | 136 => print "It does not work 136"

  | _ => print "It does not work default"
end
