(* Transparent signature matching: It is checked that the type of an implementation
 * is at least as general as the specified type. In this test, the implementation
 * is more general than the specification, also from an ML-type perspective. *)

signature X = sig
  val f : string`r1 -> string`r2
end

structure K1 = struct
  fun f a = a
end

structure K2 = K1 : X
