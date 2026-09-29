(* Reject sat allocation while an earlier value is live. *)
infix +
fun f `r () : unit =
    let val x = 5.4`r
        val y = 6.4`sat r
    (* Keep both values boxed and live; compilation must fail before linking. *)
    in prim("storage_mode_consume",(x,y)) : unit
    end
