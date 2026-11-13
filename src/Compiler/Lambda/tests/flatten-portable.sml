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
    (* Appending tuple fields must not move their evaluation after later fields.
       Both references are updated through the original receiver paths. *)
    val effect = ref 0
    fun mark n = (assign(effect,n); n)
    fun batch__noinline (p:int * (int * int) * int ref * int ref * int) =
        (assign(#3 p,42); assign(#4 p,43);
         (#1 p,#1(#2 p),#2(#2 p),deref(#3 p),deref(#4 p),#5 p))
    val batched = batch__noinline (mark 1,(mark 2,mark 3),ref(mark 4),ref(mark 5),mark 6)
    exception Stop
    val caught = (batch__noinline (mark 7,(mark 8,raise Stop),ref(mark 9),ref(mark 10),mark 11); false)
                 handle Stop => true
    val batchOK = case (batched,deref effect,caught) of ((1,2,3,42,43,6),8,true) => true | _ => false
    val result = (read true alias 0, inspect true (1,(2,42)), read true strings,batchOK)
  in case result of
       (42,42,"after",true) => print "portable data flattening passed\n"
     | _ => print "portable data flattening FAILED\n"
  end
