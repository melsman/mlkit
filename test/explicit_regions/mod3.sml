(* Transparent signature matching: It is an error if the implementation type
   is less general than the specified type. *)

signature X = sig
  val f : string`r1 -> string`r2
end

structure K1 = struct
  fun f (a:string) = a
end

structure K2 = K1 : X
