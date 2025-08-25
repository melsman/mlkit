datatype t =
         A | B | C | D | E | F

fun test s e a b =
    print(if (a = b) = e then "OK\n" else "ERR\n")

val () = test "t1" false A B
val () = test "t2" true B B
val () = test "t3" true E E

datatype u = X of int
           | Y of string
           | N0 | N1 | N2

val () = test "u1" false (X 3) N0
val () = test "u2" true (X 3) (X 3)

val a = !(ref (X 3))
val b = !(ref N0)

val () = test "u3" false a b

datatype s = S of string

val s1 = !(ref (S "h"))
val s2 = !(ref (S "h"))
val () = test "s1" true s1 s2

datatype h = H1 of int*int
           | H2 of string
           | HN0 | HN1 | HN2

val ha = !(ref (H1 (3,2)))
val hb = !(ref HN0)

val () = test "h1" false ha hb
