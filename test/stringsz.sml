
fun test sz f =
    (if size (f()) = sz then print "ok\n"
     else print "err\n"
    ) handle Size => print "exn\n"

fun try sz =
    let val () = print ("Trying with " ^ Int.toString sz ^ ": ")
        val s = CharVector.tabulate(sz, fn _ => #"a")
    in if size s = sz then (print "ok\n"; s)
       else (print "err\n"; "")
    end handle Size => (print "exn\n"; "")

fun pow k n = if n <= 0 then 1
              else k * pow k (n-1)

val k1 = pow 2 10

val m1 = k1 * k1

val g1 = k1 * m1

val _ = try k1
val _ = try m1
val _ = try g1

val s1 = try (10 * m1)

val () = ( print "Trying concat (^): "
         ; test (2*10*m1) (fn () => s1 ^ s1)
         )

val s2 = try (2 * 10 * m1)
