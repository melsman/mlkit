(* Compile with mlkit -Pcee -no_cfold -maximum_inline_size 0 -no_gc *)

local
fun pr s = (print s; print "\n")

fun f (g: unit -> unit) : string =
    let val y : string = implode [#"H", #"i"]
    in g()
     ; y
    end

fun run () : unit =
  pr let val x : string = implode [#"H", #"e", #"j"]
     in if false then x else f (fn () => pr x)
     end
in
val () = run()
end
