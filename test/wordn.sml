(* Constructing an unused wrapper must not call its host's word operations.
 * The browser compiler instantiates WordN with the dummy Word64 backend. *)
structure Unused = WordN(struct
  open Word64
  val wordSize = 63
  fun fromInt _ = raise Fail "eager WordN initialization"
end)

val calls = ref 0
structure W63 = WordN(struct
  open Word64
  val wordSize = 63
  fun fromInt i = (calls := Int.+ (!calls,1); Word64.fromInt i)
end)

fun check name b = if b then () else raise Fail name
val () = check "lazy masks" (!calls = 0)
val all = W63.fromLargeInt 9223372036854775807
val () = check "mask initialization" (!calls = 2)
val () = check "unsigned conversion" (W63.toLargeInt all = 9223372036854775807)
val () = check "signed conversion" (W63.toLargeIntX all = ~1)
val () = check "addition wraps" (W63.toLargeInt (W63.+ (all,W63.fromLargeInt 1)) = 0)
val top = W63.<< (W63.fromLargeInt 1,0w62)
val () = check "arithmetic shift" (W63.toLargeInt (W63.~>> (top,0w62)) = 9223372036854775807)
val () = check "logical shift" (W63.toLargeInt (W63.>> (top,0w62)) = 1)
val () = check "left shift wraps" (W63.toLargeInt (W63.<< (top,0w1)) = 0)
val beforeMask = !calls
val () = ignore (W63.fromLargeInt 0)
val () = ignore (W63.fromLargeInt 1)
val () = check "cached masks" (!calls = beforeMask)
val () = check "scan range" ((ignore (W63.fromString "8000000000000000"); false) handle Overflow => true)
val () = print "WordN: OK\n"
