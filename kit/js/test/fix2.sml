val () = print "<h2>Testing mutual recursion</h2>\n"

fun error b s = 
    print ((if b then "Ok - " else "Error - ") ^ s ^ "...<br>")

val () = print "<h4>Testing mutual recursion toplevel</h4>\n"

fun even n = n=0 orelse odd(n-1)
and odd  n = not(even n)
val () = error (even 34) "even"
val () = error (not(even 21)) "not even"
val () = error (odd 3) "odd"
val () = error (not(odd 22)) "not odd"

val () = print "<h4>Testing mutual recursion local</h4>\n"
local
  fun even n = n=0 orelse odd(n-1)
  and odd  n = not(even n)
in
  val () = error (even 34) "even"
  val () = error (not(even 21)) "not even"
  val () = error (odd 3) "odd"
  val () = error (not(odd 22)) "not odd"
end

val () = print "<br>End of test.\n"

