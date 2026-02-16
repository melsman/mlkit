(* Transparent signature matching: It is an error if the specification type is less
 * general than the implementation type. *)

signature X = sig
  val concat : string`r1 * string`r1 -> string`r1
end

structure K1 = struct
  fun concat (s1,s2) = s1 ^ s2
end

structure K2 = K1 : X

val () = print (K2.concat("Hello"," World\n"))
