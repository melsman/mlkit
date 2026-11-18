(* Bounds checks must preserve exceptions and evaluation order, including at
 * the signed extremes. Test arrays and vectors with both dynamic and fixed
 * lengths, and guards whose two branches establish different ranges. *)
fun assert true = ()
  | assert false = raise Fail "bounds"
fun subscript f = (f (); false) handle Subscript => true
fun check n =
  let
    val a = Array.tabulate (n, fn i => i + 17)
    val v = Vector.tabulate (n, fn i => i + 17)
    val extremes =
      (case Int.minInt of SOME i => [i] | NONE => []) @
      (case Int.maxInt of SOME i => [i] | NONE => [])
    (* Keep an explicit check in this unit: imported Array.sub may already
     * contain an unsigned comparison in a prebuilt Basis cache. *)
    fun inBounds i = 0 <= i andalso i < Array.length a
    fun test i =
      if i >= 0 andalso i < n then
        (assert (inBounds i);
         assert (Array.sub (a,i) = i + 17);
         assert (Vector.sub (v,i) = i + 17);
         Array.update (a,i,42);
         assert (Array.sub (a,i) = 42);
         Array.update (a,i,i + 17))
      else
        (assert (not (inBounds i));
         assert (subscript (fn () => (Array.sub (a,i); ())));
         assert (subscript (fn () => (Vector.sub (v,i); ())));
         assert (subscript (fn () => Array.update (a,i,42))))
    fun scan i = if i < n then (test i; scan (i+1)) else ()
  in
    List.app test (~1 :: n :: extremes);
    scan 0
  end
val () = List.app check [0,1,2,17,129]

(* An unknown or negative upper bound must not justify unsigned comparison. *)
fun interval (i,n) = if 0 <= i andalso i < n then 1 else 0
val () = List.app (fn (i,n,want) => assert (interval(i,n) = want))
                 [(~2,~1,0),(~1,0,0),(0,0,0),(0,1,1),(1,1,0),(7,9,1)]
fun ranges x =
  (if x < 0 then x < 1 else x >= 0) andalso
  (if x <= 3 then x < 4 else x > 3) andalso
  (if x > 4 then x >= 5 else x <= 4) andalso
  (if x >= 7 then x > 6 else x < 7) andalso
  (if 0 > x then 1 > x else 0 <= x) andalso
  (if 3 >= x then 4 > x else 3 < x) andalso
  (if 4 < x then 5 <= x else 4 >= x) andalso
  (if 7 <= x then 6 < x else 7 > x)
val () = List.app (fn i => assert (ranges i)) [~99,~1,0,1,3,4,5,6,7,99]
val calls = ref 0
val a = Array.array (1,0)
fun bump x = (calls := !calls + 1; x)
val () = assert (subscript (fn () => Array.update (bump a,bump ~1,bump 5)))
val () = assert (!calls = 3 andalso Array.sub(a,0) = 0)
val () = calls := 0
val () = assert (subscript (fn () => (Vector.sub (bump (Vector.fromList []),bump 0); ())))
val () = assert (!calls = 2)
(* A conditional upper operand must stay conditional. *)
fun guarded i = 0 <= i andalso i < bump 3
val () = calls := 0
val () = assert (not (guarded ~1) andalso !calls = 0)
val () = assert (guarded 1 andalso !calls = 1)
exception Upper
fun raises i = 0 <= i andalso i < (raise Upper)
val () = assert (not (raises ~1))
val () = assert ((raises 0; false) handle Upper => true)

(* Strict and inclusive bounds, including the rejection form used by byte
 * tables. Negative upper bounds must not be interpreted as unsigned. *)
fun accepted (i,n) = 0 <= i andalso i <= n
fun rejected (i,n) = i < 0 orelse i >= n
fun rejectedInclusive (i,n) = i < 0 orelse i > n
fun fixedAccepted i = i >= 0 andalso i <= 7
fun fixedRejected i = 0 > i orelse i >= 7
fun fixedRejectedInclusive i = i < 0 orelse i > 7
val edgeIndexes = [~2,~1,0,1,6,7,8] @
    (case Int.minInt of SOME i => [i] | NONE => []) @
    (case Int.maxInt of SOME i => [i] | NONE => [])
fun oracle (i,n) =
  let val j = IntInf.fromInt i
      val k = IntInf.fromInt n
  in (j >= 0 andalso j < k, j >= 0 andalso j <= k)
  end
val () = List.app (fn i =>
  let val (strict,inclusive) = oracle(i,7)
  in assert(fixedAccepted i = inclusive);
     assert(fixedRejected i = not strict);
     assert(fixedRejectedInclusive i = not inclusive);
     List.app (fn n =>
       let val (strict,inclusive) = oracle(i,n)
       in assert(accepted(i,n) = inclusive);
          assert(rejected(i,n) = not strict);
          assert(rejectedInclusive(i,n) = not inclusive)
       end) [~2,~1,0,1,7]
  end) edgeIndexes

fun rejectEffect i = i < 0 orelse i >= bump 3
fun rejectInclusiveEffect i = i < 0 orelse i > bump 3
fun acceptInclusiveEffect i = 0 <= i andalso i <= bump 3
fun rejectRaises i = i < 0 orelse i > (raise Upper)
fun rejectStringEffect i =
  i < 0 orelse i >= String.size (bump "abc")
val () = calls := 0
val () = assert(rejectEffect ~1 andalso rejectInclusiveEffect ~1)
val () = assert(not(acceptInclusiveEffect ~1) andalso rejectStringEffect ~1)
val () = assert(!calls = 0 andalso rejectRaises ~1)
val () = assert((rejectRaises 0; false) handle Upper => true)
val () = assert(not(rejectEffect 1) andalso not(rejectInclusiveEffect 3))
val () = assert(acceptInclusiveEffect 3 andalso not(rejectStringEffect 1))
val () = assert(!calls = 4)

fun checkBytes n =
  let val s = String.implode(List.tabulate(n,fn _ => #"x"))
      val a = Word8Array.array(n,0w42)
      val v = Word8Vector.tabulate(n,fn _ => 0w42)
      fun test i =
        if #1(oracle(i,n)) then
          (assert(String.sub(s,i) = #"x");
           assert(Word8Array.sub(a,i) = 0w42);
           assert(Word8Vector.sub(v,i) = 0w42);
           assert(Word8Vector.sub(Word8Vector.update(v,i,0w43),i) = 0w43);
           assert(Word8Vector.sub(v,i) = 0w42);
           Word8Array.update(a,i,0w43);
           assert(Word8Array.sub(a,i) = 0w43);
           Word8Array.update(a,i,0w42))
        else
          (assert(subscript(fn () => (String.sub(s,i); ())));
           assert(subscript(fn () => (Word8Array.sub(a,i); ())));
           assert(subscript(fn () => (Word8Vector.sub(v,i); ())));
           assert(subscript(fn () => (Word8Vector.update(v,i,0w0); ())));
           assert(subscript(fn () => Word8Array.update(a,i,0w0))))
  in List.app test (n :: edgeIndexes)
  end
val () = List.app checkBytes [0,1,7,8,129]
fun sizeException f = (f (); false) handle Size => true
val () = assert(sizeException(fn () => (Word8Array.array(~1,0w0); ())))
val () = assert(sizeException(fn () => (Word8Array.array(Word8Array.maxLen+1,0w0); ())))
val () = assert(sizeException(fn () => (Array.array(Array.maxLen+1,0); ())))

(* The loop-specific bounds eliminator must still understand the new check. *)
fun byteSum s =
  let val n = String.size s
      fun loop (i,sum) =
        if i < n then loop(i+1,sum+Char.ord(String.sub(s,i))) else sum
  in loop(0,0)
  end
val () = assert(byteSum "" = 0 andalso byteSum "abc" = 294)

val () = print "bounds checks passed\n"
