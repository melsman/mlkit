val a = "OK\n"
val a = "ERROR\n"
and rec f = fn x => g x
and g = fn y => (a, y)

val _ = print (#1(f 23))