
fun 'a withEscape (f: ('a -> 'b) -> 'a): 'a =
   let
      exception E of 'a
   in
      f (fn x => raise E x) handle E x => x
   end

val b = ref false
val s : string =
  withEscape
    (fn escapeStr =>
         Int.toString (withEscape (fn escapeInt =>
                                      if !b then escapeInt 4 else escapeStr "hello")))

val () = print (s ^ "\n")
