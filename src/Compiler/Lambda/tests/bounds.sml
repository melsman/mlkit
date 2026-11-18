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
val () = print "bounds checks passed\n"
