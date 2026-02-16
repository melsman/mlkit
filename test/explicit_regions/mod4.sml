(* Transparent signature matching: It is an error if the implementation type is
   less general than the specified type. In this test, from an ML-type
   perspective, the implementation is more general than the specification, but,
   from a region-and-effect perspective, the instantiated implementation type
   scheme (\/r.string`r -> string`r) is less general than the specified
   type (\/r1,r2.string`r1 -> string`r2). *)

signature X = sig
  val f : string`r1 -> string`r2
end

structure K1 = struct
  fun f a = a
end

structure K2 = K1 : X
