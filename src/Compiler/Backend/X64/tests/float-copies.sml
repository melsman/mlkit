(* Keep the fixed coordinates live across iterations, and exercise copies for
   arithmetic, branches, tail-call arguments, and absolute value. *)
fun distance__noinline (x:real,y:real,points) =
    let
      fun loop ([],sum) = sum
        | loop ((a,b)::rest,sum) =
          let val dx = x-a
              val dy = y-b
          in loop(rest,sum + Math.sqrt(dx*dx + dy*dy))
          end
    in loop(points,0.0)
    end

fun rotate__noinline (n,a:real,b:real,c:real) =
    if n = 0 then (a,b,c)
    else rotate__noinline(n-1,b,c,a)

fun magnitude__noinline (x:real) = Real.abs x
fun choose__noinline (b,x:real,y:real) = if b then x else y

val points = List.tabulate(100,fn _ => (3.0,4.0))
val (a,b,c) = rotate__noinline(100,1.0,2.0,3.0)
val nan = 0.0/0.0
val () =
    if Real.==(distance__noinline(0.0,0.0,points),500.0)
       andalso Real.==(a,2.0) andalso Real.==(b,3.0) andalso Real.==(c,1.0)
       andalso Real.==(magnitude__noinline(~3.0),3.0)
       andalso not(Real.signBit(magnitude__noinline(~0.0)))
       andalso Real.signBit(choose__noinline(true,~0.0,0.0))
       andalso Real.isNan(choose__noinline(false,0.0,nan))
    then print "X64 floating-point copies passed\n"
    else raise Fail "X64 floating-point copies"
