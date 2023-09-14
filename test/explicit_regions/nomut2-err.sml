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
(*
fun g `[e e0] (k: (unit #e0 -> unit) while nomut e0) : (unit #e -> unit) while nomut e =
 fn ()  =>
    let val r2 = ref "hi"
    in r2 := "hi there\n" ; k()
     ; print (!r2)
    end
*)
val rec g2 `[eee ee0] : ((unit #ee0 -> unit) while nomut ee0) -> (unit #eee -> unit) while nomut eee =
 fn k => fn ()  =>
    let val r2 = ref "hi"
    in r2 := "hi there\n" ; k()
     ; print (!r2)
    end

val rec g3 `[eee ee0] : ((unit #ee0 -> unit) -> (unit #eee -> unit) while nomut ee0) while nomut eee =
 fn k => fn ()  =>
    let val r2 = ref "hi"
    in r2 := "hi there\n" ; k()
     ; print (!r2)
    end

val () =
  let val x = g3 f
  in x ()
  end

end
