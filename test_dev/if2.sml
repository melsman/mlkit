
let
infix <=
infix <
infix >
infix >=
fun print (s:string) : unit = prim("printStringML", "printStringML", s)
in
  if 3<4 then
    print "It works!"
  else
    print "It does not work"
end
