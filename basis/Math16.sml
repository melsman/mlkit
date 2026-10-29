(** SigDoc *)
structure Math16 : MATH =
struct
  type real = real16
  val pi : real = 3.141592653589793238462643383279502884
  val e : real = 2.718281828459045235360287471352662498
  fun sqrt (x:real) : real = prim ("__sqrt_real16", x)
  fun widen (x:real) : LargeReal.real =
      prim ("@mlkit_real16_to_double", prim ("id", x) : word)
  fun narrow (x:LargeReal.real) : real =
      prim ("id", prim ("@mlkit_double_to_real16", x) : word)
  (* Transcendentals use libm at binary64 precision, then narrow once.
   * Core arithmetic and sqrt use the exact binary16 runtime fallback. *)
  fun sin (x:real) : real = narrow (prim ("@sin", widen x) : LargeReal.real)
  fun cos (x:real) : real = narrow (prim ("@cos", widen x) : LargeReal.real)
  fun tan (x:real) : real = narrow (prim ("@tan", widen x) : LargeReal.real)
  fun asin (x:real) : real = narrow (prim ("@asin", widen x) : LargeReal.real)
  fun acos (x:real) : real = narrow (prim ("@acos", widen x) : LargeReal.real)
  fun atan (x:real) : real = narrow (prim ("@atan", widen x) : LargeReal.real)
  fun exp (x:real) : real = narrow (prim ("@exp", widen x) : LargeReal.real)
  fun ln (x:real) : real = narrow (prim ("@log", widen x) : LargeReal.real)
  fun log10 (x:real) : real = narrow (prim ("@log10", widen x) : LargeReal.real)
  fun sinh (x:real) : real = narrow (prim ("@sinh", widen x) : LargeReal.real)
  fun cosh (x:real) : real = narrow (prim ("@cosh", widen x) : LargeReal.real)
  fun tanh (x:real) : real = narrow (prim ("@tanh", widen x) : LargeReal.real)
  fun atan2 (y:real,x:real) : real =
      narrow (prim ("@atan2", (widen y,widen x)) : LargeReal.real)
  fun pow (x:real,y:real) : real =
      if (x >= 1.0 andalso x <= 1.0 orelse x >= ~1.0 andalso x <= ~1.0)
         andalso (y > 65504.0 orelse y < ~65504.0)
      then sqrt ~1.0
      else narrow (prim ("@pow", (widen x,widen y)) : LargeReal.real)
end
