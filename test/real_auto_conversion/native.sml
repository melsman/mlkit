infix 6 + -
infix 4 < =
infixr 5 ::
fun op = (x:int, y:int) : bool = prim ("=", (x,y))
fun check (name:string, b:bool) =
    if b then ()
    else (prim ("printStringML", name) : unit; prim ("@exit", (1:int)) : unit)
fun equal (x:real, y:real) = prim ("__lesseq_real", (x,y)) : bool
                           andalso prim ("__greatereq_real", (x,y)) : bool
fun identity__noinline (x:real) : real = prim ("@real_auto_identity", x)
fun nine__noinline (a:real) : real =
    prim ("@real_auto_nine", (a,a+1.0,a+2.0,a+3.0,a+4.0,a+5.0,a+6.0,a+7.0,a+8.0))
val () = check ("identity", equal (identity__noinline 2.5, 2.5))
val sum : real = prim ("@real_auto_sum", (~1.5,2.25))
val () = check ("two floats", equal (sum,0.75))
val () = check ("nine floats", equal (nine__noinline 1.0, 285.0))
val mixed : real = prim ("@real_auto_mixed",
    (~10,0.25,11,1.25,12,2.25,13,3.25,14,4.25,15,5.25,16,6.25,17,7.25,18,8.25,19,9.25))
val () = check ("mixed stack", equal (mixed,28.25))
val other : real = prim ("@real_auto_other", (1.25,true,"abc"))
val () = check ("other conversions", equal (other,5.25))
val boxed : int64 = prim ("@real_auto_boxed", 2.5)
val n : int = prim ("@labs", boxed)
val () = check ("boxed integer result", n = 42)
val () : unit = prim ("@real_auto_void", 2.5)
val seen : bool = prim ("@real_auto_void_seen", ())
val () = check ("unit result", seen)
val ordered : real = prim ("@real_auto_ordered",
    (prim ("@real_auto_left", ()) : real, prim ("@real_auto_right", ()) : int))
val () = check ("argument order", equal (ordered,3.25))
exception Stop
val prevented = ((prim ("@real_auto_never", ((raise Stop):real,1)) : unit; false)
                 handle Stop => true)
val () = check ("argument exception", prevented)
fun specials__noinline (n:int, xs:real list) =
    if n < 0 then xs
    else specials__noinline (n-1, identity__noinline (prim ("@real_auto_special", n)) :: xs)
fun bits__noinline (nil:real list, n:int) = check ("special count", n = 6)
  | bits__noinline (x::xs, n:int) =
    (check ("special bits", prim ("@real_auto_bits", (x,n)) : bool);
     bits__noinline (xs,n+1))
val () = bits__noinline (specials__noinline (5,nil),0)
(* These arithmetic values become float64s under optimisation. They must
 * survive a C call that overwrites every caller-saved FP register. *)
fun live__noinline (a:real) =
    let
      val v0 = a+0.0
      val v1 = a+1.0
      val v2 = a+2.0
      val v3 = a+3.0
      val v4 = a+4.0
      val v5 = a+5.0
      val v6 = a+6.0
      val v7 = a+7.0
      val v8 = a+8.0
      val n : int = prim ("@real_auto_clobber", (0:int))
      val r : real = prim ("@real_auto_nine", (v0,v1,v2,v3,v4,v5,v6,v7,v8))
    in
      check ("live integer", n = 7);
      check ("live floats", equal (r,285.0))
    end
val () = live__noinline 1.0
(* Keeping results in a list forces boxing; request collection in C before
 * returning each double, then retain and inspect all previous ML roots. *)
fun collect__noinline (n:int, xs:real list) =
    if n < 1 then xs
    else collect__noinline (n-1, (prim ("@real_auto_gc_result", n) : real) :: xs)
fun roots__noinline (nil, n:int) = check ("root count", n = 501)
  | roots__noinline (x::xs, n:int) =
    (check ("real root", equal (x, (prim ("realInt", n) : real)+0.25));
     roots__noinline (xs,n+1))
val retained_results = collect__noinline (500,nil)
val () = roots__noinline (retained_results,1)
val () = check ("collection after result", prim ("@real_auto_gc_verify", ()) : bool)
val () : unit = prim ("printStringML", "real auto-conversion passed\n")
