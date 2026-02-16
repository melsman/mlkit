(* Transparent signature matching: It is checked that the type of an implementation
 * is at least as general as the specified type. In this test, the implementation
 * satisfies the specification. *)

signature X = sig
  val f : string`r1 -> string`r1
end

structure K1 = struct
  fun f (a:string) = a ^ ""
end

structure K2 = K1 : X

val () = print (K2.f "Hello" ^ " World\n")
