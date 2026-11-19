fun endpoint n =
    let
      val m = if n >= 256 then raise Subscript else n + 1
    in (n < 255, 255 > n, n <= 255, m)
    end

val n =
    case CommandLine.arguments () of
        s :: _ => valOf (Int.fromString s)
      | [] => 255

val () =
    if endpoint n = (false, false, true, 256) then print "OK\n"
    else raise Fail "range comparison at inclusive endpoint"

val c = Char.chr n
val k = Char.ord c
val full = Array.array (256, 0)
val short = Array.array (255, 0)
val () = Array.update (full, k, 42)
val () = if Array.sub (full, k) = 42 then () else raise Fail "valid endpoint"
val () =
    (Array.update (short, k, 42); raise Fail "update accepted invalid endpoint")
    handle Subscript => ()
val () =
    (ignore (Array.sub (short, k)); raise Fail "read accepted invalid endpoint")
    handle Subscript => ()
