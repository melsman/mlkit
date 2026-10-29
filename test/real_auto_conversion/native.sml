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
val word_mixed : real = prim ("@real_auto_word", (2.5,0wx7FFFFFFFFFFFFFFF:word))
val () = check ("unsigned word argument", equal (word_mixed,2.5))
val word_result : word = prim ("@real_auto_word_result", 2.5)
val () = check ("unsigned word result", prim ("@real_auto_word_check", word_result) : bool)
(* Arithmetic produces narrow register values, testing sign extension as well
 * as literals. Each C function checks its input before returning it to ML. *)
fun add31__noinline (x:int31,y:int31) : int31 = prim ("__plus_int31", (x,y))
val precise_int31_0 : int31 = add31__noinline (~1073741823,~1)
val result_int31_0 : int31 = prim ("@real_auto_int31", (2.5,0:int,precise_int31_0))
val () = check ("int31 boundary 0", prim ("__equal_int31", (precise_int31_0,result_int31_0)))
val precise_int31_1 : int31 = add31__noinline (~2,1)
val result_int31_1 : int31 = prim ("@real_auto_int31", (2.5,1:int,precise_int31_1))
val () = check ("int31 boundary 1", prim ("__equal_int31", (precise_int31_1,result_int31_1)))
val precise_int31_2 : int31 = 0
val result_int31_2 : int31 = prim ("@real_auto_int31", (2.5,2:int,precise_int31_2))
val () = check ("int31 boundary 2", prim ("__equal_int31", (precise_int31_2,result_int31_2)))
val precise_int31_3 : int31 = 1073741823
val result_int31_3 : int31 = prim ("@real_auto_int31", (2.5,3:int,precise_int31_3))
val () = check ("int31 boundary 3", prim ("__equal_int31", (precise_int31_3,result_int31_3)))
val precise_int63_0 : int63 = ~4611686018427387904
val result_int63_0 : int63 = prim ("@real_auto_int63", (2.5,0:int,precise_int63_0))
val () = check ("int63 boundary 0", prim ("__equal_int63", (precise_int63_0,result_int63_0)))
val precise_int63_1 : int63 = ~1
val result_int63_1 : int63 = prim ("@real_auto_int63", (2.5,1:int,precise_int63_1))
val () = check ("int63 boundary 1", prim ("__equal_int63", (precise_int63_1,result_int63_1)))
val precise_int63_2 : int63 = 0
val result_int63_2 : int63 = prim ("@real_auto_int63", (2.5,2:int,precise_int63_2))
val () = check ("int63 boundary 2", prim ("__equal_int63", (precise_int63_2,result_int63_2)))
val precise_int63_3 : int63 = 4611686018427387903
val result_int63_3 : int63 = prim ("@real_auto_int63", (2.5,3:int,precise_int63_3))
val () = check ("int63 boundary 3", prim ("__equal_int63", (precise_int63_3,result_int63_3)))
val precise_word31_0 : word31 = 0w0
val result_word31_0 : word31 = prim ("@real_auto_word31", (2.5,0:int,precise_word31_0))
val () = check ("word31 boundary 0", prim ("__equal_word31", (precise_word31_0,result_word31_0)))
val precise_word31_1 : word31 = 0w1
val result_word31_1 : word31 = prim ("@real_auto_word31", (2.5,1:int,precise_word31_1))
val () = check ("word31 boundary 1", prim ("__equal_word31", (precise_word31_1,result_word31_1)))
val precise_word31_2 : word31 = 0w1073741824
val result_word31_2 : word31 = prim ("@real_auto_word31", (2.5,2:int,precise_word31_2))
val () = check ("word31 boundary 2", prim ("__equal_word31", (precise_word31_2,result_word31_2)))
val precise_word31_3 : word31 = 0w2147483647
val result_word31_3 : word31 = prim ("@real_auto_word31", (2.5,3:int,precise_word31_3))
val () = check ("word31 boundary 3", prim ("__equal_word31", (precise_word31_3,result_word31_3)))
val precise_word63_0 : word63 = 0w0
val result_word63_0 : word63 = prim ("@real_auto_word63", (2.5,0:int,precise_word63_0))
val () = check ("word63 boundary 0", prim ("__equal_word63", (precise_word63_0,result_word63_0)))
val precise_word63_1 : word63 = 0w1
val result_word63_1 : word63 = prim ("@real_auto_word63", (2.5,1:int,precise_word63_1))
val () = check ("word63 boundary 1", prim ("__equal_word63", (precise_word63_1,result_word63_1)))
val precise_word63_2 : word63 = 0w4611686018427387904
val result_word63_2 : word63 = prim ("@real_auto_word63", (2.5,2:int,precise_word63_2))
val () = check ("word63 boundary 2", prim ("__equal_word63", (precise_word63_2,result_word63_2)))
val precise_word63_3 : word63 = 0w9223372036854775807
val result_word63_3 : word63 = prim ("@real_auto_word63", (2.5,3:int,precise_word63_3))
val () = check ("word63 boundary 3", prim ("__equal_word63", (precise_word63_3,result_word63_3)))
val precise_stack : int31 = prim ("@real_auto_precise_stack",
    (2.5,precise_int31_1,precise_word31_3,precise_int63_0,precise_word63_3,
     precise_int31_1,precise_word31_3,precise_int63_0,precise_word63_3,
     precise_int31_1,precise_word31_3))
val () = check ("explicit integer stack arguments", prim ("__equal_int31", (precise_stack,precise_int31_0)))
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
(* Preserve the sign and NaN payload across float registers and stack slots. *)
fun single_bits (x:real32) : word = prim ("id", x)
fun single_from_bits (x:word) : real32 = prim ("id", x)
fun single__noinline (last:real32) : real32 =
    prim ("@real_auto_single_mixed",
          (123:int,2.5:real,1.0:real32,2.0:real32,3.0:real32,4.0:real32,
           5.0:real32,6.0:real32,7.0:real32,8.0:real32,9.0:real32,last))
val single_nan = single_from_bits 0wxFFC01234
val single_nan_result = single__noinline single_nan
val () = check ("single NaN payload",
                prim ("=", (single_bits single_nan_result, single_bits single_nan)))
val single_zero_result = single__noinline (~0.0:real32)
val () = check ("single negative zero",
                prim ("=", (single_bits single_zero_result, 0wx80000000:word)))
val () : unit = prim ("printStringML", "real auto-conversion passed\n")
