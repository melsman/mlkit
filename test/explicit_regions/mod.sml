(* Transparent signature matching: It is checked that the type of an implementation
 * is at least as general as the specified type. *)

signature X = sig
  val f : string`r1 -> string`r2
end

structure K1 = struct
  fun f `[r1] (a:string`r1) : string`r1 = a ^ ""
end

structure K2 = K1 : X
