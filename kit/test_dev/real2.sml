(*
  fun printReal (n:real):unit = prim("printReal","printReal",n)

  infix ==
  val epsilon = 0.000666 
  fun r1 == r2 = 
    let val _ = printReal r1 
      val _ = printReal r2
	val r = (r1 - r2)
	val _ = printReal r
	val r_abs = abs r
	val _ = printReal r_abs
    in r_abs < epsilon (*no perfect world*)
    end
  fun error b s = print ((if b then "Ok - " else "Error - ") ^ s ^ "...\n")
  val b = (4.0 + 3.0 == 7.0)
  val _ = if b then print "True" else print "False"
  val _ = error (4.0 + 3.0 == 7.0) "+"
*)
  val b = 4.0 < 3.0

  val _ = if b then print "ERROR" else print "OK"
