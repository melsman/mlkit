(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  

val _ = print "Testing datatypes and unboxing schemes...\n"

(* Some auxiliary functions *)
fun sum l = List.foldl (op +) 0 l


(* Check that t1 is not unboxed, which would be unsafe *)
datatype t1 = T1_1 of int list | T1_2

fun sum_t1 T1_2 = ~1
  | sum_t1 (T1_1 l) = sum l

fun chk_t1_a T1_2 = "2a"
  | chk_t1_a (T1_1 _) = "1a"

fun chk_t1_b (T1_1 _) = "1b"
  | chk_t1_b T1_2 = "2b"

val a_t1 = T1_1 [1,2,3,4,5]

val _ = tst "test1a" (sum_t1 (T1_1 [1,2,3,4,5]) = 15)
val _ = tst "test1b" (sum_t1 (T1_1 []) = 0)
val _ = tst "test1c" (sum_t1 T1_2 = ~1)
val _ = tst "test1d" (chk_t1_a T1_2 = "2a" andalso chk_t1_a (T1_1[1]) = "1a")
val _ = tst "test1e" (chk_t1_b T1_2 = "2b" andalso chk_t1_b (T1_1[1]) = "1b")



datatype t2 = T2_1 | T2_2 | T2_3

fun chk_t2_a T2_1 = "1a"
  | chk_t2_a T2_2 = "2a"
  | chk_t2_a T2_3 = "3a"

fun chk_t2_1 T2_1 = true
  | chk_t2_1 _ = false

fun chk_t2_2 T2_2 = true
  | chk_t2_2 _ = false

fun chk_t2_3 T2_3 = true
  | chk_t2_3 _ = false

val _ = tst "test2a" (chk_t2_a T2_1 = "1a" andalso 
		      chk_t2_a T2_2 = "2a" andalso
		      chk_t2_a T2_3 = "3a")

val _ = tst "test2b" (chk_t2_1 T2_1 andalso chk_t2_2 T2_2 andalso chk_t2_3 T2_3)

val _ = tst "test2c" (not(chk_t2_1 T2_2) andalso not(chk_t2_1 T2_3) andalso 
		      not(chk_t2_2 T2_1) andalso not(chk_t2_2 T2_3) andalso 
		      not(chk_t2_3 T2_1) andalso not(chk_t2_3 T2_2))

datatype t3 = T3_1 of t2 | T3_2

fun chk_t3_a (T3_1 T2_1) = true
  | chk_t3_a _ = false

fun chk_t3_b (T3_1 T2_2) = true
  | chk_t3_b _ = false

fun chk_t3_c (T3_1 T2_3) = true
  | chk_t3_c _ = false

fun chk_t3_d T3_2 = true
  | chk_t3_d _ = false

val _ = tst "test3a" (chk_t3_a (T3_1 T2_1) andalso 
		      chk_t3_b (T3_1 T2_2) andalso 
		      chk_t3_c (T3_1 T2_3) andalso 
		      chk_t3_d T3_2)

val _ = tst "test3b" (not(chk_t3_a (T3_1 T2_2)) andalso 
		      not(chk_t3_a (T3_1 T2_3)) andalso 
		      not(chk_t3_a T3_2))

val _ = tst "test3c" (not(chk_t3_b (T3_1 T2_1)) andalso 
		      not(chk_t3_b (T3_1 T2_3)) andalso 
		      not(chk_t3_b T3_2))

val _ = tst "test3d" (not(chk_t3_c (T3_1 T2_1)) andalso 
		      not(chk_t3_c (T3_1 T2_2)) andalso 
		      not(chk_t3_c T3_2))

val _ = tst "test3e" (not(chk_t3_d (T3_1 T2_1)) andalso 
		      not(chk_t3_d (T3_1 T2_2)) andalso 
		      not(chk_t3_d (T3_1 T2_3)))
