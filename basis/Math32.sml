(** SigDoc *)
structure Math32 : MATH =
struct
  type real = real32
  val pi : real = 3.141592653589793238462643383279502884
  val e : real = 2.718281828459045235360287471352662498
  fun sqrt (x:real) : real = prim ("__sqrt_real32", x)
  fun sin (x:real) : real = prim ("@sinf", x)
  fun cos (x:real) : real = prim ("@cosf", x)
  fun tan (x:real) : real = prim ("@tanf", x)
  fun asin (x:real) : real = prim ("@asinf", x)
  fun acos (x:real) : real = prim ("@acosf", x)
  fun atan (x:real) : real = prim ("@atanf", x)
  fun atan2 (y:real, x:real) : real = prim ("@atan2f", (y,x))
  fun exp (x:real) : real = prim ("@expf", x)
  fun ln (x:real) : real = prim ("@logf", x)
  fun log10 (x:real) : real = prim ("@log10f", x)
  fun sinh (x:real) : real = prim ("@sinhf", x)
  fun cosh (x:real) : real = prim ("@coshf", x)
  fun tanh (x:real) : real = prim ("@tanhf", x)
  fun pow_ (x:real, y:real) : real = prim ("@powf", (x,y))
  fun pow (x:real, y:real) : real =
      if (x >= 1.0 andalso x <= 1.0 orelse x >= ~1.0 andalso x <= ~1.0)
         andalso (y > 3.40282346638528859811704183484516925e38 orelse
                  y < ~3.40282346638528859811704183484516925e38)
      then sqrt ~1.0
      else pow_ (x,y)
end
