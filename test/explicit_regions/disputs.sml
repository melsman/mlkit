local
  fun print (s:string) : unit = prim("printStringML", s)
in

val rec par `[e1 e2] : (unit #e1 -> 'a) -> (unit #e2 -> 'b) -> ('a * 'b) while e1 ## e2 =
  fn f => fn g => (f(),g())

(*
fun par2 `[e1 e2] (f: unit #e1 -> 'a) (g: unit #e2 -> 'b) : ('a * 'b) while e1 ## e2 =
  (f(),g())
*)

val () =
    let val (a,b) = par (fn () => (1,2)) (fn () => (3,4))
        val c = if true then a else b
    in ()
    end
(*
fun f() =
    ( print (!r)
    ; r := "Hello again\n"
    ; print (!r)
    )

fun g `[e e0] (k: (unit #e0 -> unit) while nomut e0) : (unit #e -> unit) while nomut e =
 fn ()  =>
    let val r2 = ref "hi"
    in r2 := "hi there\n" ; k()
     ; print (!r2)
    end

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
*)
end
