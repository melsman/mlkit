
exception Hd

fun hd [] = raise Hd
  | hd (x::_) = x

exception Tl

fun tl [] = raise Tl
  | tl (_ ::xs) = xs

exception Error of string 

local 
  val error_f = Error "f"
in
  fun f(l) = 
      hd(tl(tl l)) handle _ => raise error_f
end
     
val r = f[1,2,3,4];