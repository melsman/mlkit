(* Explicit primitives keep this test independent of backend-specific Basis
   inlining, including Barry's use of the host SML Basis. *)
val () =
  let
    fun deref (r:'a ref) : 'a = prim("!", r)
    fun assign (r:'a ref, x:'a) : unit = prim(":=", (r,x))
    fun read false (p:'a ref * int) = deref (#1 p)
      | read true p = read false p
    fun inspect false (p:int * (int * int)) = #2 (#2 p)
      | inspect true p = inspect false p
    val p = (ref (fn x:int => x), 7)
    val alias = p
    val () = assign (#1 p, fn _ => 42)
    val strings = (ref "before", 8)
    val () = assign (#1 strings, "after")
    val result = (read true alias 0, inspect true (1,(2,42)), read true strings)
  in case result of
       (42,42,"after") => print "portable data flattening passed\n"
     | _ => print "portable data flattening FAILED\n"
  end
