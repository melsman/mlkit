(* Reject atbot allocation while an earlier value is live. *)
infix +
fun f () =
    let with r
        val x = 5.4`r
        val y = 6.4`atbot r
    (* Keep both values boxed and live; compilation must fail before linking. *)
    in prim("storage_mode_consume",(x,y)) : unit
    end
