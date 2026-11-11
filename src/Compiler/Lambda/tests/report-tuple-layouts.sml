(* A polymorphic constructor retains its declaration while its instance's
   tuple argument changes layout. Keep the receiver opaque to tuple selection. *)
val () =
  let
    fun add (x:real,y:real) : real = prim("__plus_real", (x,y))
    fun value__noinline (p:string * real) = add(#2 p,1.0)
    fun consume__noinline NONE = 0.0
      | consume__noinline (SOME p) = value__noinline p
    val result = consume__noinline (SOME ("payload",add(1.0,2.0)))
  in
    if prim("__lesseq_real", (result,4.0)) andalso
       prim("__lesseq_real", (4.0,result))
    then print "tuple layout reporting passed\n"
    else raise Fail "tuple layout reporting"
  end
