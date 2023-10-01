
fun (a:real) / (b:real) : real = prim ("divFloat", (a,b))

(** SigDoc *)
structure Math : MATH = struct
  type real = real

  fun isNan (x : real) : bool = prim ("isnanFloat", x)

  val posInf = Initial.posInf
  val negInf = Initial.negInf

  fun compare (x, y: real) =
    if x<y then LESS else if x>y then GREATER else EQUAL

  infix ==
  fun op == (x, y) = case compare (x,y)
		       of EQUAL => true
			| _ => false

  val pi = 3.14159265358979323846
  val e  = 2.7182818284590452354

  fun sqrt (r : real) : real = prim ("sqrtFloat", r)
  fun sin (r : real) : real = prim ("sinFloat", r)
  fun cos (r : real) : real = prim ("cosFloat", r)
  fun tan r = sin r / cos r
  fun atan (r : real) : real = prim ("atanFloat", r)
  fun asin (a : real) : real = prim ("asinFloat", a)
  fun acos (a : real) : real = prim ("acosFloat", a)
  fun atan2 (y : real, x : real) : real = prim ("atan2Float", (y,x))
  fun exp (r : real) : real = prim ("expFloat", r)
  fun pow (x : real, y : real) : real = prim ("powFloat", (x,y))
  local
    fun mkPosInf () : real = prim ("posInfFloat", ())
    fun mkNegInf () : real = prim ("negInfFloat", ())
    fun ln' (r : real) : real = prim ("lnFloat", r)
    fun sqrt (r : real) : real = prim ("sqrtFloat", r)
    fun mkNaN () = sqrt ~1.0
  in
     fun ln r = if r == 0.0 then mkNegInf()
		else if r == mkPosInf() then mkPosInf()
		     else let val r = ln' r
			  in if r == mkNegInf() then mkNaN()
			     else r
			  end
     fun log10 r = ln r / ln' 10.0
  end
  fun sinh (a : real) : real = prim ("sinhFloat", a)
  fun cosh (a : real) : real = prim ("coshFloat", a)
  fun tanh (a : real) : real = prim ("tanhFloat", a)

end
