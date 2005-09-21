
fun out s = (TextIO.output(TextIO.stdOut, s ^ "\n"); TextIO.flushOut TextIO.stdOut)
fun isNullFP(s : foreignptr) : bool = prim("__is_null", s)

val b = Dynlib.dlopen (SOME "libcrack.so", Dynlib.NOW, false)
val _ = Dynlib.dlsym ("testdyn","FascistCheck",b)

fun fascistCheck a : string option = 
             let val b : foreignptr = prim("@:", ("testdyn", a : string, "/usr/lib/cracklib_dict"))
             in if isNullFP b then NONE else SOME(prim ("fromCtoMLstring", b))
             end

(*
fun fascistCheck a : string option = 
             let val b : foreignptr = prim(":", ("testdyn", a : string))
             in if isNullFP b then NONE else SOME (prim("fromCtoMLstring", b))
             end*)

fun removeLastChar s = implode(List.rev(List.drop((List.rev(explode s)),1) handle _ => []))


fun loop () = let val input = Option.map removeLastChar (TextIO.inputLine TextIO.stdIn)
         in case input of NONE => ()
          | SOME "" => ()
          | SOME i =>
            ((case fascistCheck i of NONE => out "PassWord OK"
                                   | SOME s => out s);loop())
         end

val _ = loop ()
