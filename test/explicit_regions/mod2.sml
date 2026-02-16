(* Transparent signature matching: It is an error if the specified
   type is less general than the implementation type. *)

signature X = sig
  val f : string`r1 -> string`r1
end

structure K1 = struct
  fun f `[r1 r2] (a:string`r1) : string`r2 = a ^ ""
end

structure K2 = K1 : X
