(* Storage mode words remain valid explicit region names. *)
fun print (s:string) : unit = prim("printStringML",s)
fun f `atbot () : string`atbot = "OK"`sat atbot
fun g () = let with sat in print (f `atbot sat ()) end
fun triple `[atbot sat attop] () : string * string * string =
    ("A"`atbot,"B"`sat,"C"`attop)
fun bare () =
    let with atbot sat attop
        val (a,b,c) = triple `[atbot,sat,attop] ()
    in print a; print b; print c
    end
val _ = (g (); bare ())
