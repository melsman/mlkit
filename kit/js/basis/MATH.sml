signature MATH =
  sig
    type real
    val pi : real
    val e : real
    val sqrt : real -> real
    val sin : real -> real
    val cos : real -> real
    val tan : real -> real
    val asin : real -> real
    val acos : real -> real
    val atan : real -> real
    val atan2 : real * real -> real
    val exp : real -> real
    val pow : real * real -> real
    val ln    : real -> real
    val log10 : real -> real
    val sinh : real -> real
    val cosh : real -> real
    val tanh : real -> real
  end

(*
Description

val pi : real

    The constant pi (3.141592653...).

val e : real

    The base e (2.718281828...) of the natural logarithm.

sqrt x

    returns the square root of x. sqrt (~0.0) = ~0.0. If x < 0, it
    returns NaN.

sin x
cos x
tan x

    These return the sine, cosine, and tangent, respectively, of x,
    measured in radians. If x is an infinity, these functions return
    NaN. Note that tan will produce infinities at various finite
    values, roughly corresponding to the singularities of the tangent
    function.

asin x
acos x

    These return the arc sine and arc cosine, respectively, of x. asin
    is the inverse of sin. Its result is guaranteed to be in the
    closed interval [-pi/2,pi/2]. acos is the inverse of cos. Its
    result is guaranteed to be in the closed interval [0,pi]. If the
    magnitude of x exceeds 1.0, they return NaN.

atan x

    returns the arc tangent of x. atan is the inverse of tan. For
    finite arguments, the result is guaranteed to be in the open
    interval (-pi/2,pi/2). If x is +infinity, it returns pi/2; if x is
    -infinity, it returns -pi/2.

atan2 (y, x)

    returns the arc tangent of (y/x) in the closed interval [-pi,pi],
    corresponding to angles within +-180 degrees. The quadrant of the
    resulting angle is determined using the signs of both x and y, and
    is the same as the quadrant of the point (x,y). When x = 0, this
    corresponds to an angle of 90 degrees, and the result is (real
    (sign y)) * pi/2.0. It holds that

        sign ( cos ( atan2 (y,x))) = sign(x) 

    and

        sign ( sin ( atan2 (y,x))) = sign(y) 

    except for inaccuracies incurred by the finite precision of real
    and the approximation algorithms used to compute the mathematical
    functions.

    Rules for exceptional cases are specified in the following table.
      y 	x 	atan2(y,x)
      +-0 	0 < x 	+-0
      +-0 	+0 	+-0
      +-0 	x < 0 	+-pi
      +-0 	-0 	+-pi
      y, 0 < y 	+-0 	pi/2
      y, y < 0 	+-0 	-pi/2
      +-y, finite y > 0 	+infinity 	+-0
      +-y, finite y > 0 	-infinity 	+-pi
      +-infinity 	finite x 	+-pi/2
      +-infinity 	+infinity 	+-pi/4
      +-infinity 	-infinity 	+-3pi/4

exp x

    returns e(x), i.e., e raised to the x(th) power. If x is
    +infinity, it returns +infinity; if x is -infinity, it returns 0.

pow (x, y)

    returns x(y), i.e., x raised to the y(th) power. For finite x and
    y, this is well-defined when x > 0, or when x < 0 and y is
    integral. Rules for exceptional cases are specified below.

      x 	y 	pow(x,y)
      x, including NaN 	0 	1
      |x| > 1 	+infinity 	+infinity
      |x| < 1 	+infinity 	+0
      |x| > 1 	-infinity 	+0
      |x| < 1 	-infinity 	+infinity
      +infinity 	y > 0 	+infinity
      +infinity 	y < 0 	+0
      -infinity 	y > 0, odd integer 	-infinity
      -infinity 	y > 0, not odd integer 	+infinity
      -infinity 	y < 0, odd integer 	-0
      -infinity 	y < 0, not odd integer 	+0
      x 	NaN 	NaN
      NaN 	y <> 0 	NaN
      +-1 	+-infinity 	NaN
      finite x < 0 	finite non-integer y 	NaN
      +-0 	y < 0, odd integer 	+-infinity
      +-0 	finite y < 0, not odd integer 	+infinity
      +-0 	y > 0, odd integer 	+-0
      +-0 	y > 0, not odd integer 	+0

ln x
log10 r

    These return the natural logarithm (base e) and decimal logarithm
    (base 10), respectively, of x. If x < 0, they return NaN; if x =
    0, they return -infinity; if x is infinity, they return infinity.

sinh x
cosh x
tanh x

    These return the hyperbolic sine, hyperbolic cosine, and
    hyperbolic tangent, respectively, of x, that is, the values (e(x)
    - e(-x)) / 2, (e(x) + e(-x)) / 2, and (sinh x)/(cosh x).

    These functions have the following properties:
    sinh +-0 	= 	+-0
    sinh +-infinity 	= 	+-infinity
    cosh +-0 	= 	1
    cosh +-infinity 	= 	+-infinity
    tanh +-0 	= 	+-0
    tanh +-infinity 	= 	+-1
*)
