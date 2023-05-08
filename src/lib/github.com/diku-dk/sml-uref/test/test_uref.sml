fun pr s = print ("URef." ^ s ^ "\n")

fun test s b =
    if b then pr (s ^ ": OK")
    else pr (s ^ ": ERR")

fun itest s expected n =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ Int.toString expected ^ " but got " ^ Int.toString n)

open URef infix ::=

val () = print "Testing URef\n"

val a = uref 8

val b = uref 9

val c = uref 10

val () = test "eq.not" (not(eq(a,b)))

val () = test "eq.1" (eq(a,a))

val () = unify (fn (x,y) => x+y) (b,c)

val () = test "eq.1" (eq(b,c))

val () = test "find.1" (!!b = !!c)

val () = itest "find.1" (!!b) 19

val () = itest "update.1" (b ::= 18; !!c) 18

val () = test "compare" (compare Int.compare (a,b) = LESS)
