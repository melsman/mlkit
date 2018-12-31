val r = ref 5
val () =
    case r of
        ref 4 => raise Div
      | _ => ()
