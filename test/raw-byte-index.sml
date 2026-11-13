val table = Array.array (256, 1)
val () = Array.update (table, 255, 511)

(* The table covers every byte and the tests use valid string offsets. *)
fun byteAt (text : string, i : int) : char = prim ("__bytetable_sub", (text, i))
fun sub (i : int) : int = prim ("word_sub0", (table, i))
fun store (i : int, value : int) : unit = prim ("word_update0", (table, i, value))

fun read__noinline (text, i, n) =
    let
      val byte = Char.ord (byteAt (text, i))
      val product = n * 10
    in product + sub byte
    end

fun reuse__noinline (text, i) =
    let
      val byte = Char.ord (byteAt (text, i))
    in (sub byte, byte)
    end

fun update__noinline (text, i, value) =
    store (Char.ord (byteAt (text, i)), value)

val text = "\000\255"
val () =
    if read__noinline (text, 0, 7) = 71
       andalso read__noinline (text, 1, 7) = 581
       andalso reuse__noinline (text, 1) = (511, 255)
    then ()
    else raise Fail "byte-indexed read"

val () = update__noinline (text, 1, 42)
val () =
    if reuse__noinline (text, 1) = (42, 255) then print "OK\n"
    else raise Fail "byte-indexed update"
