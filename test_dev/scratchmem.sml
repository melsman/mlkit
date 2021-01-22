local

infix *

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (s:int):unit = prim ("printNum",s)
fun printReal (n:real):unit = prim("printReal",n)

structure B = struct
type t = string
fun alloc (i:int) : t = prim("allocStringML", (8*i))
fun scratch4 () : t = prim("__scratchmem",32)
fun update (t:t,i:int,v:real) : unit =
    prim("__blockf64_update_real", (t,i,v))
fun sub (t:t,i:int) : real =
    prim("__blockf64_sub_real", (t,i))
fun size (t:t) : int =
    prim ("__blockf64_size", t)
val maxLen = 12345678 (* arbitrarily chosen *)

fun pack (r0:real,r1:real,r2:real,r3:real) : t = prim ("__blockf64", (r0,r1,r2,r3))

end

in

fun try s x =
    ( print s
    ; print ":\n"
(*
    ; print "Expect 4: "  (* when tagging is diabled, the tags contain garbage... *)
    ; printNum (B.size x)
*)
    ; B.update(x,2,4.3)
    ; print "Expect 4.3: "
    ; printReal(B.sub(x,2)))

val () = try "pack" (B.pack(1.0,2.0,3.0,4.0))

val () = try "alloc" (B.alloc 4)

val () = try "scratch4" (B.scratch4())


end
