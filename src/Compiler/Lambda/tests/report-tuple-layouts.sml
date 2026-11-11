(* A polymorphic constructor retains its declaration while its instance's
   tuple argument changes layout. Keep the receiver opaque to tuple selection. *)
val () =
  let
    datatype marker = Marker of int | Empty
    datatype 'a wrapper = Wrapped of 'a
    fun marker__noinline n = Marker n
    fun unmark__noinline (Marker n) = n
      | unmark__noinline Empty = 0
    fun wrap__noinline x = Wrapped x
    fun unwrap__noinline (Wrapped x) = x
    val markersOK = unmark__noinline (marker__noinline 7) = 7
    val wrappersOK = unwrap__noinline (wrap__noinline 3) = 3 andalso
                     unwrap__noinline (wrap__noinline "ok") = "ok"
    fun add (x:real,y:real) : real = prim("__plus_real", (x,y))
    fun value__noinline (p:string * real) = add(#2 p,1.0)
    fun consume__noinline NONE = 0.0
      | consume__noinline (SOME p) = value__noinline p
    val result = consume__noinline (SOME ("payload",add(1.0,2.0)))
  in
    if markersOK andalso wrappersOK andalso prim("__lesseq_real", (result,4.0)) andalso
       prim("__lesseq_real", (4.0,result))
    then print "tuple layout reporting passed\n"
    else raise Fail "tuple layout reporting"
  end
