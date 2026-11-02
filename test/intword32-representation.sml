(* Values must retain all 32 bits through polymorphic containers and calls.
 * Exercise these paths before and after changing the native representation. *)
fun check (name, b) =
    print (name ^ (if b then ": OK\n" else ": WRONG\n"))

val words : Word32.word list = [0w0, 0w1, 0wx7fffffff, 0wx80000000, 0wxffffffff]
val ints : Int32.int list = [~2147483648, ~1, 0, 1, 2147483647]
fun roundtrip xs =
    let val a = Array.fromList xs
        val r = ref (Vector.fromList xs)
    in List.tabulate (Array.length a,
                      fn i => (Array.sub (a, i), Vector.sub (!r, i)))
    end
val _ = check ("word containers", List.all (fn (x, y) => x = y) (roundtrip words))
val _ = check ("int containers", List.all (fn (x, y) => x = y) (roundtrip ints))
val _ = check ("word wrap", Word32.+ (0wxffffffff, 0w1) = 0w0)
val _ = check ("word high bit", Word32.>> (0wx80000000, 0w31) = 0w1)
val _ = check ("word signed shift", Word32.~>> (0wx80000000, 0w31) = 0wxffffffff)
val _ = check ("word signed conversion", Word32.toIntX 0wxffffffff = ~1)
val _ = check ("int overflow",
               ((Int32.+ (2147483647, 1); false) handle Overflow => true))
val _ = check ("int negate minimum",
               ((Int32.~ (~2147483648); false) handle Overflow => true))
val _ = check ("int quotient overflow",
               ((Int32.quot (~2147483648, ~1); false) handle Overflow => true))
val _ = check ("int division", Int32.div (~7, 3) = ~3 andalso Int32.mod (~7, 3) = 2)
