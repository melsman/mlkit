fun right63__noinline (x : Int63.int) = x * 10
fun left63__noinline (x : Int63.int) = 10 * x
fun right64__noinline (x : Int64.int) = x * 10
fun left64__noinline (x : Int64.int) = 10 * x

fun verify multiply (minimum : IntInf.int, maximum : IntInf.int) =
    let
      val low = ~(~minimum div 10)
      val high = maximum div 10
      fun check n =
          let
            val product = n * 10
            val expected =
                if product < minimum orelse product > maximum then NONE
                else SOME product
            val actual = SOME (multiply n) handle Overflow => NONE
          in
            if actual = expected then ()
            else raise Fail ("checked multiply by ten at " ^ IntInf.toString n)
          end
    in
      List.app check [minimum, low - 1, low, low + 1, ~1, 0, 1,
                      high - 1, high, high + 1, maximum]
    end

val () =
    List.app (fn multiply =>
                 verify (Int63.toLarge o multiply o Int63.fromLarge)
                        (~4611686018427387904, 4611686018427387903))
             [right63__noinline, left63__noinline]
val () =
    List.app (fn multiply =>
                 verify (Int64.toLarge o multiply o Int64.fromLarge)
                        (~9223372036854775808, 9223372036854775807))
             [right64__noinline, left64__noinline]
val () = print "OK\n"
