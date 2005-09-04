
fun out s = (TextIO.output(TextIO.stdOut, s ^ "\n"); TextIO.flushOut TextIO.stdOut)
fun isNullFP(s : foreignptr) : bool = prim("__is_null", s)
fun isNull(s : string) : bool = prim("__is_null", s)

(* opening library libcrack.so 
*)
val (b : foreignptr,s:string) = prim("sml_dlopen", ("libcrack.so", 0))
val _ = if isNullFP b then (if isNull s then out "bad" else out s ; raise Fail "bad") else ()
val a : string = prim("resolveFun",("testdyn","FascistCheck",b)) 
val _ = if isNull a then () else (out a; Process.exit(Process.failure))

(*
val (b : foreignptr,s : string) = prim("sml_dlopen", ("/home/varming/smlserver/smlserver-apache-develop/mlkit/kit/src/Runtime/liba.so", 0))
val _ = if isNullFP b then (if isNull s then out "bad" else out s ; raise Fail "bad") else ()
val a : string = prim("resolveFun",("testdyn","testS",b)) 
val _ = if isNull a then () else (out a; Process.exit(Process.failure))*)

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
