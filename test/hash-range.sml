(* The modulo recurrence bounds the hash, but must not justify overflowing
 * operations before a modulo, unknown entry values, or unbounded back edges. *)
functor HashRangeTest (I : INTEGER) =
struct
    val zero = I.fromInt 0
    val one = I.fromInt 1
    val factor = I.fromInt 32
    val divisor = I.fromInt 2357
    fun hash__noinline bytes =
        let
            fun loop ([], h) = h
              | loop (c::cs, h) =
                loop (cs, I.mod (I.+ (I.* (factor, h), I.fromInt (Char.ord c)), divisor))
        in loop (bytes, zero)
        end
    fun reference bytes =
        List.foldl (fn (c, h : IntInf.int) => (32 * h + IntInf.fromInt (Char.ord c)) mod 2357)
                   0 bytes
    val bytes = List.tabulate (4096, fn i => Char.chr (i mod 256))
    val () = List.app
        (fn xs => if I.toLarge (hash__noinline xs) = reference xs then ()
                  else raise Fail "bounded hash")
        [[], [Char.chr 255], bytes]

    val maximum = valOf I.maxInt
    fun seeded__noinline (seed, bytes) =
        let
            fun loop ([], h) = h
              | loop (c::cs, h) =
                loop (cs, I.mod (I.+ (I.* (factor, h), I.fromInt (Char.ord c)), divisor))
        in loop (bytes, seed)
        end
    fun mustOverflow f =
        (f (); raise Fail "lost Overflow") handle Overflow => ()
    val () = mustOverflow (fn () => ignore (seeded__noinline (maximum, [Char.chr 0])))
    val () = mustOverflow (fn () => ignore (seeded__noinline (valOf I.minInt, [Char.chr 0])))

    (* A large factor can overflow even with a modulo-bounded accumulator. *)
    fun largeFactor__noinline bytes =
        let
            fun loop ([], h) = h
              | loop (_::cs, h) = loop (cs, I.mod (I.* (maximum, h), divisor))
        in loop (bytes, I.fromInt 2)
        end
    val () = mustOverflow (fn () => ignore (largeFactor__noinline [Char.chr 0]))

    (* Increment at maxInt must still raise, even with a modulo afterwards. *)
    fun endpoint__noinline n =
        let
            fun loop (0, h) = h
              | loop (k, h) = loop (k - 1, I.mod (I.+ (h, one), divisor))
        in loop (n, maximum)
        end
    val () = mustOverflow (fn () => ignore (endpoint__noinline 1))

    (* Every back edge participates: one unbounded reset defeats the small
     * accumulator invariant supplied by the other, modulo-bounded edge. *)
    fun reset__noinline n =
        let
            fun loop (0, h) = h
              | loop (k, h) =
                if k = 2 then loop (k - 1, maximum)
                else loop (k - 1, I.mod (I.+ (h, one), divisor))
        in loop (n, zero)
        end
    val () = mustOverflow (fn () => ignore (reset__noinline 2))
end
structure Test63 = HashRangeTest (Int63)
structure Test64 = HashRangeTest (Int64)
val () = print "OK\n"
