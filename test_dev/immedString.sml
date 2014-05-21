(* This example shows that some region variables containing constant strings *)
(* are not dropped, that is, they are passed to the region polymorphic       *)
(* function and not used there.                                              *)
(* Works only if inline functions are disabled.                              *)

local
  fun print (s:string) : unit = prim("printStringML", "printStringML", s)
  fun f () = "It works"

  val _ = print (f ())
in
end
