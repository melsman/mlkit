
let
infix <=
infix <
infix >
infix >=
fun print (s:string) : unit = prim("printStringML", s)
in
  if 3<=3 andalso 2<4 andalso 2>1 andalso 4>=4 then
    print "It works!"
  else
    print "It does not work"
end
