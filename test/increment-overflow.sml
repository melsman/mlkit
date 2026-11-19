val maximum : Int63.int = 4611686018427387903
val minimum : Int63.int = ~4611686018427387904

fun strict n = if n < maximum then SOME (n + 1) else NONE
fun inclusive n = if n <= maximum then n + 1 else 0
fun reversed n = if maximum > n then SOME (n + 1) else NONE
fun outside n = if n < maximum then 0 else n + 1

val atMax =
    case CommandLine.arguments () of
        s :: _ => valOf (Int63.fromString s)
      | [] => maximum
val belowMax = atMax - 1

fun expectOverflow f =
    (ignore (f atMax); raise Fail "missing Overflow")
    handle Overflow => ()

val () =
    if strict belowMax = SOME atMax andalso strict atMax = NONE
       andalso strict minimum = SOME (minimum + 1)
       andalso reversed belowMax = SOME atMax andalso reversed atMax = NONE
       andalso inclusive belowMax = atMax
    then ()
    else raise Fail "incorrect increment"

val () = expectOverflow inclusive
val () = expectOverflow outside
val () = print "OK\n"
