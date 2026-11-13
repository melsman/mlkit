(* Exercise both a materialised Boolean and a conditional use. *)
fun equal__noinline (x:real,y:real) = Real.==(x,y)
fun branch__noinline (x:real,y:real) = if Real.==(x,y) then 17 else 23
fun unequal__noinline (x:real,y:real) = Real.!=(x,y)
fun unequalBranch__noinline (x:real,y:real) = if Real.!=(x,y) then 17 else 23
fun divide__noinline (x:real,y:real) = x/y
val nan = divide__noinline(0.0,0.0)
val inf = divide__noinline(1.0,0.0)
val values = [(0,nan),(1,~nan),(2,~inf),(3,~1.0),
              (4,~0.0),(5,0.0),(6,1.0),(7,inf)]
val ok = List.all (fn (i,x) => List.all (fn (j,y) =>
    let val expected = i >= 2 andalso j >= 2 andalso
                      (i = j orelse ((i = 4 orelse i = 5) andalso (j = 4 orelse j = 5)))
    in equal__noinline(x,y) = expected andalso
       branch__noinline(x,y) = (if expected then 17 else 23) andalso
       unequal__noinline(x,y) = not expected andalso
       unequalBranch__noinline(x,y) = (if expected then 23 else 17)
    end) values) values
val literals = Real.==(0.0,~0.0) andalso not (Real.!=(0.0,~0.0)) andalso
               not (Real.==(1.0,2.0)) andalso Real.!=(1.0,2.0)
val _ = print (if ok andalso literals andalso Real.signBit(~0.0) andalso not(Real.signBit 0.0)
               then "real equality: OK\n" else "real equality: WRONG\n")
