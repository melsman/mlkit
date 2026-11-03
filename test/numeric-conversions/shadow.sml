(* The exported wrapper must retain the original implementation binding. *)
fun narrowShadow (x : IntInf.int) = Int32.fromLarge x
fun rawByte (x : Word8.word) : IntInf.int = prim("__word8_to_intinf", x)
fun rawNarrow (x : IntInf.int) : Int32.int = prim("__intinf_to_int32", x)
structure IntInfRep = struct end
val () = if narrowShadow (IntInf.fromInt 17) = Int32.fromInt 17 then ()
         else raise Fail "same-unit conversion shadowing"
val () = if rawNarrow (rawByte 0w255) = Int32.fromInt 255 then ()
         else raise Fail "unoptimized semantic conversion lowering"
