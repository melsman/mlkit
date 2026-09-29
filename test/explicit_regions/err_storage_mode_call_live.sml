(* Reject atbot call while an earlier value is live. *)
infix +
fun f `r () : real = 5.4`r
fun g () =
    let with r
        val x = 5.4`r
        val y = f `atbot r ()
    (* Keep both values boxed and live; compilation must fail before linking. *)
    in prim("storage_mode_consume",(x,y)) : unit
    end
