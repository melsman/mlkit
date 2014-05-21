val _ = if (0 : IntInf.int) = (1 : IntInf.int) then print "Err\n" else print "Ok\n"


fun is0 (0 : IntInf.int) = true
  | is0 400000000000000000000000000000000000000 = true
  | is0 400000000000000000000000000000000000001 = false
  | is0 3 = true
  | is0 _                = false

fun chk (i:IntInf.int) t =
    if is0 i = t then print "Ok\n" else print "Err\n"

val _ = chk 0 true

val _ = chk 400000000000000000000000000000000000000 true

val _ = chk 400000000000000000000000000000000000001 false

val _ = chk 3 true

val _ = chk 5 false
