(* vector3.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * 3 dimensional vector arithmetic.
 *)

structure Vector3 : VECTOR =
  struct

    type 'a vec = {x : 'a, y : 'a, z : 'a}
    type realvec = real vec

    val dim = 3

    fun tabulate f = {x = f 0, y = f 1, z = f 2}

    val zerov = {x = 0.0, y = 0.0, z = 0.0}
    fun equal({x, y, z}, {x=x1, y=y1, z=z1}) =
      Real.==(x, x1) andalso Real.==(y, y1) andalso Real.==(z, z1)

    fun addv ({x=x1, y=y1, z=z1} : realvec, {x=x2, y=y2, z=z2}) =
	  {x=x1+x2, y=y1+y2, z=z1+z2}

    fun subv ({x=x1, y=y1, z=z1} : realvec, {x=x2, y=y2, z=z2}) =
	  {x=x1-x2, y=y1-y2, z=z1-z2}

    fun dotvp ({x=x1, y=y1, z=z1} : realvec, {x=x2, y=y2, z=z2}) =
	  x1*x2 + y1*y2 + z1*z2

    fun crossvp ({x=x1, y=y1, z=z1} : realvec, {x=x2, y=y2, z=z2}) =
	  {x = y1*z2 - z1*y2, y = x1*z2 - z1*x2, z = x1*y2 - y1*x2}

    fun addvs ({x, y, z} : realvec, s) = {x=x+s, y=y+s, z=z+s}

    fun mulvs ({x, y, z} : realvec, s) = {x=x*s, y=y*s, z=z*s}

    fun divvs ({x, y, z} : realvec, s) = {x=x/s, y=y/s, z=z/s}

    fun mapv f {x, y, z} = {x = f x, y = f y, z = f z}

    fun map3v f ({x=x1, y=y1, z=z1}, {x=x2, y=y2, z=z2}, {x=x3, y=y3, z=z3}) =
	  {x = f(x1, x2, x3), y = f(y1, y2, y3), z = f(z1, z2, z3)}

    fun foldv f {x, y, z} init = f(z, f(y, f(x, init)))

    fun format {lp, rp, sep, cvt} {x, y, z} = String.concat[
	    lp, cvt x, sep, cvt y, sep, cvt z, rp
	  ]

    fun explode {x, y, z} = [x, y, z]

    fun implode [x, y, z] = {x=x, y=y, z=z}
      | implode _ = raise Fail "implode: bad dimension"

    type matrix = {
	    m00 : real, m01 : real, m02 : real,
	    m10 : real, m11 : real, m12 : real,
	    m20 : real, m21 : real, m22 : real
	  }

    val zerom = {
	    m00 = 0.0, m01 = 0.0, m02 = 0.0,
	    m10 = 0.0, m11 = 0.0, m12 = 0.0,
	    m20 = 0.0, m21 = 0.0, m22 = 0.0
	  }

    fun addm (a : matrix, b : matrix) = {
	    m00=(#m00 a + #m00 b), m01=(#m01 a + #m01 b), m02=(#m02 a + #m02 b),
	    m10=(#m10 a + #m10 b), m11=(#m11 a + #m11 b), m12=(#m12 a + #m12 b),
	    m20=(#m20 a + #m20 b), m21=(#m21 a + #m21 b), m22=(#m22 a + #m22 b)
	  }

    fun outvp ({x=a0, y=a1, z=a2} : realvec, {x=b0, y=b1, z=b2}) = {
	    m00=(a0*b0), m01=(a0*b1), m02=(a0*b2),
	    m10=(a1*b0), m11=(a1*b1), m12=(a1*b2),
	    m20=(a2*b0), m21=(a2*b1), m22=(a2*b2)
	  }

  end (* VectMath *)

