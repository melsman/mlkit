(* Storage mode words remain ordinary SML identifiers. *)
infix +
infix 4 >
fun print (s:string) : unit = prim("printStringML",s)
type atbot = int
structure Constructors = struct datatype sat = attop of atbot end
structure atbot = struct val sat = 7 end
fun sat (Constructors.attop atbot) = atbot
fun example () =
    let val atbot = 34
        val sat = sat (Constructors.attop atbot)
        val attop = {atbot=sat, sat=atbot.sat, attop=1}
    in #atbot attop + #sat attop + #attop attop
    end
val _ = if example () > 41 then
            (if 43 > example () then print "OK" else print "FAIL")
        else print "FAIL"
