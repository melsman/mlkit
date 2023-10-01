local
  fun print (s:string) : unit = prim("printStringML", s)
  fun !(x: 'a ref): 'a = prim ("!", x)
  infix 3 :=
  fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
  val r = ref "Hello\n"
in
fun f() =
    ( print (!r)
    ; r := "Hello again\n"
    ; print (!r)
    )

val rec g `e : (unit #e -> unit) while nomut e =
 fn ()  =>
    let val r2 = ref "hi"
    in r2 := "hi there\n" ; r := "yes"
     ; print (!r2)
    end
(*
val () = f()
val () = g()
*)
end
