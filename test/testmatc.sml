(* testmatc.sml --- test the match compiler 1996-07-10, 1997-02-03 *)

(* This test file is stolen from MosML. mael 2001-04-26 *)

val _ = print "Testing match compiler\n"

fun check b = if b then "OK" else "WRONG"

fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun checkres1 f argres = 
    check'(fn _ => List.all (fn (arg, res) => f arg = res) argres)

fun checkres2 f argres = 
    check'(fn _ => List.all (fn (arg1, arg2, res) => f arg1 arg2 = res) argres)


fun prln t s = print(t ^ ": " ^ s ^ "\n")

(* Inexhaustive: *)

fun f1 ([], [])       = 111 
  | f1 (x::xr, y::yr) = 222;        

fun f1c []      []      = 111 
  | f1c (x::xr) (y::yr) = 222;        

val test1a = checkres1 f1 [(([], []), 111), (([7], [8]), 222)];
val _ = prln "test1a" test1a
val test1b = (f1 ([], [1]); "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test1b" test1b
val test1c = (f1 ([2], []); "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test1c" test1c

val test2a = checkres2 f1c [([], [], 111), ([7], [8], 222)];
val _ = prln "test2a" test2a
val test2b = (f1c [] [1]; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test2b" test2b
val test2c = (f1c [2] []; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test2c" test2c

(* Inexhaustive, with unused rules: *)

fun f2 ([], [])       = 111 
  | f2 (x::xr, y::yr) = 222
  | f2 ([], [])       = 333;

fun f2c []      []      = 111 
  | f2c (x::xr) (y::yr) = 222
  | f2c []      []      = 333;

val test3a = checkres1 f2 [(([], []), 111), (([7], [8]), 222)];
val _ = prln "test3a" test3a
val test3b = (f2 ([], [1]); "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test3b" test3b
val test3c = (f2 ([2], []); "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test3c" test3c

val test4a = checkres2 f2c [([], [], 111), ([7], [8], 222)];
val _ = prln "test4a" test4a
val test4b = (f2c [] [1]; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test4b" test4b
val test4c = (f2c [2] []; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test4c" test4c


(* Constructors with span 1 *)

datatype 'a t = Uniq of 'a

fun fc1 (Uniq "slam") = "en"
  | fc1 (Uniq "glyf") = "to"
  | fc1 (Uniq x)      = x

val test5 = checkres1 fc1 [(Uniq "slam", "en"), 
			   (Uniq "glyf", "to"), 
			   (Uniq "mock", "mock")]
val _ = prln "test5" test5


(* Constructors with arity = 0 *)

fun berry (true,  false, _    ) = 111
  | berry (false, _,     true ) = 222
  | berry (_,     true,  false) = 333
  | berry (false, false, false) = 444
  | berry (true,  true,  true ) = 555;

val testberry = checkres1 berry
    [((true, true, true), 555),
     ((false, false, false), 444),
     ((false, false, true), 222),
     ((false, true, false), 333),
     ((true, false, false), 111)];
val _ = prln "testberry" testberry


(* Constructors with arity > 0; see also lists *)

datatype t = A | B | C of int list

fun fcon (A,   B, C [] ) = 111
  | fcon (A,   B, C [1]) = 222
  | fcon (B,   B, _    ) = 333
  | fcon (A,   A, A    ) = 444
  | fcon (C[], A, A    ) = 555;

val test6a = 
    checkres1 fcon [((C[], A, A), 555), 
		    ((A, A, A), 444), 
		    ((A, B, C[]), 111), 
		    ((A, B, C[1]), 222), 
		    ((B, B, C[]), 333), 
		    ((B, B, C[1]), 333), 
		    ((B, B, A), 333)];
val _ = prln "test6a" test6a

val test6b = (fcon (C[1], A, A); "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test6b" test6b
val test6c = (fcon (C[],  B, A); "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test6c" test6c
val test6d = (fcon (C[],  A, B); "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test6d" test6d

(* Non-greedy constructors *)

(* ? *)


(* Special constants: int, string, char, word, real *)

fun fi 101  = 111
  | fi 102  = 222
  | fi 101  = 333
  | fi 104  = 444
  | fi ~101 = 555;

val test10a = checkres1 fi [(104, 444), (102, 222), (101, 111), (~101, 555)];
val _ = prln "test10a" test10a
val test10b = (fi 100; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test10b" test10b
val test10c = (fi 103; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test10c" test10c
val test10d = (fi 105; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test10d" test10d

fun fs "first"  = 111
  | fs "second" = 222
  | fs "first"  = 333
  | fs "fourth" = 444;

val test11a = checkres1 fs [("fourth", 444), ("second", 222), ("first", 111)];
val _ = prln "test11a" test11a
val test11b = (fs ""; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test11b" test11b
val test11c = (fs "Fourth"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test11c" test11c

fun fc #"A" = 111
  | fc #"B" = 222
  | fc #"A" = 333
  | fc #"D" = 444;

val test12a = checkres1 fc [(#"D", 444), (#"B", 222), (#"A", 111)];
val _ = prln "test12a" test12a
val test12b = (fc #"@"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test12b" test12b
val test12c = (fc #"C"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test12c" test12c
val test12d = (fc #"E"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test12d" test12d
val test12e = (fc #"d"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test12e" test12e

fun fw 0wx101 = 111
  | fw 0wx102 = 222
  | fw 0wx101 = 333
  | fw 0wx104 = 444;

val test13a = checkres1 fw [(0wx104, 444), (0wx102, 222), (0wx101, 111)];
val _ = prln "test13a" test13a
val test13b = (fw 0wx100; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test13b" test13b
val test13c = (fw 0wx103; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test13c" test13c
val test13d = (fw 0wx105; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test13d" test13d

fun fr "101.0"  = 111
  | fr "102.5"  = 222
  | fr "101.0"  = 333
  | fr "104.8"  = 444
  | fr "~101.0" = 555;

val test14a = checkres1 fr [("104.8", 444), ("102.5", 222), 
			    ("101.0", 111), ("~101.0", 555)];
val _ = prln "test14a" test14a
val test14b = (fr "100.1"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test14b" test14b
val test14c = (fr "103.0"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test14c" test14c
val test14d = (fr "104.9"; "WRONG") handle Match => "OK" | _ => "WRONG"
val _ = prln "test14d" test14d


(* Tuples --- unused rules *)

fun funit1 () = 111
  | funit1 x  = 222;

fun funit2 {} = 111
  | funit2 x  = 222;

val test20 = checkres1 funit1 [((), 111), ({}, 111)];
val _ = prln "test20" test20
val test21 = checkres1 funit2 [((), 111), ({}, 111)];
val _ = prln "test21" test21


(* Vectors *)
(*
fun berryvec #[true,  false, _    ] = 111
  | berryvec #[false, _,     true ] = 222
  | berryvec #[_,     true,  false] = 333
  | berryvec #[false, false, false] = 444
  | berryvec #[]                    = 666
  | berryvec #[true]                = 777
  | berryvec #[true, true]          = 888
  | berryvec #[true, true, true, true] = 999
  | berryvec #[true,  true,  true ] = 555

val testberryvec = checkres1 berryvec
    [(#[true, true, true], 555),
     (#[false, false, false], 444),
     (#[false, false, true], 222),
     (#[false, true, false], 333),
     (#[true, false, false], 111),
     (#[], 666),
     (#[true], 777),
     (#[true, true], 888),
     (#[true, true, true, true], 999)];     
*)

(* Ref patterns *)

fun fref1 (ref ()) = 111
  | fref1 (ref x)  = 222;

val test30 = checkres1 fref1 [(ref (), 111), (ref {}, 111)];
val _ = prln "test30" test30

fun fref2 (ref [117]) = 111
  | fref2 (ref [x])   = x
  | fref2 (ref _)     = 222;

val test31 = 
    checkres1 fref2 [(ref [], 222), (ref [999], 999), (ref [117], 111)];
val _ = prln "test31" test31


(* Static exception constructors *)

(* Make a dynamic excon for testing purposes *)

local 
    fun fabricate () = 
	let exception A in A end
in
    val dynExcon = fabricate ()
end

exception A and C and D;
exception B = A;

fun fexc1 A = 1
  | fexc1 B = 2
  | fexc1 A = 3
  | fexc1 C = 4;

val test40a = 
    checkres1 fexc1 [(C, 4), (B, 1), (A, 1)];
val _ = prln "test40a" test40a

val test40b = (fexc1 Div; "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test40b" test40b
val test40c = (fexc1 D; "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test40c" test40c
val test40d = (fexc1 (Fail "blah"); "WRONG") 
              handle Match => "OK" | _ => "WRONG";
val _ = prln "test40d" test40d
val test40e = (fexc1 dynExcon; "WRONG") handle Match => "OK" | _ => "WRONG";
val _ = prln "test40e" test40e


exception I of int and R of string 
exception Z = I;

fun fexc2 (I 7)    = 111
  | fexc2 (R "1.2")  = 222
  | fexc2 (I 8)    = 333
  | fexc2 (R "~1.2") = 444
  | fexc2 (Z 9)    = 555
  | fexc2 (Fail s) = 666
  | fexc2 (R "~1.2") = 777
  | fexc2 (Z 8)    = 888
  | fexc2 _        = 999;

val test41a = checkres1 fexc2 
    [(I 7, 111), (I 6, 999), (Z 8, 333), (R "1.2", 222), (I 8, 333), 
     (R "~1.2", 444), (I 9, 555), (Z 9, 555), (Z 0, 999), (Fail "baf", 666),
     (A, 999), (Div, 999), (dynExcon, 999)];
val _ = prln "test41a" test41a

(* Dynamic exception constructors, nullary *)

fun enclose42 () =
    let 
	exception A and C and D;
	exception B = A;
	
	fun fexc11 A = 1
	  | fexc11 B = 2
	  | fexc11 A = 3
	  | fexc11 C = 4;
	    
	val test42a = checkres1 fexc11 [(C, 4), (B, 1), (A, 1)];   
	val _ = prln "test42a" test42a
	val test42b = (fexc11 Div; "WRONG") 
	              handle Match => "OK" | _ => "WRONG";
	val _ = prln "test42b" test42b
	val test42c = (fexc11 D; "WRONG") 
                      handle Match => "OK" | _ => "WRONG";
	val _ = prln "test42c" test42c
	val test42d = (fexc11 (Fail "blah"); "WRONG") 
	              handle Match => "OK" | _ => "WRONG";
	val _ = prln "test42d" test42d
	val test42e = (fexc11 dynExcon; "WRONG") 
                      handle Match => "OK" | _ => "WRONG";
	val _ = prln "test42e" test42e
    in [test42a, test42b, test42c, test42d, test42e] end;

val test42 = enclose42 ();


fun enclose43 () =
    let 
	exception I of int and R of string 
	exception Z = I;
	
	fun fexc22 (I 7)    = 111
	  | fexc22 (R "1.2")  = 222
	  | fexc22 (I 8)    = 333
	  | fexc22 (R "~1.2") = 444
	  | fexc22 (Z 9)    = 555
	  | fexc22 (Fail s) = 666
	  | fexc22 (R "~1.2") = 777
	  | fexc22 (Z 8)    = 888
	  | fexc22 _        = 999;
	    
	val test43a = checkres1 fexc22 
	    [(I 7, 111), (I 6, 999), (Z 8, 333), (R "1.2", 222), (I 8, 333), 
	     (R "~1.2", 444), (I 9, 555), (Z 9, 555), (Z 0, 999), 
	     (Fail "baf", 666), (A, 999), (Div, 999), (dynExcon, 999)];
    in test43a end;

val test43 = enclose43();
val _ = prln "test43" test43


(* Raising Bind *)

fun fbind xs =
    let val [x] = xs in x end

val test50a = check'(fn _ => 117 = fbind [117]);
val _ = prln "test50a" test50a
val test50b = (fbind []; "WRONG") handle Bind => "OK" | _ => "WRONG";
val _ = prln "test50b" test50b
val test50c = (fbind [1,2]; "WRONG") handle Bind => "OK" | _ => "WRONG";
val _ = prln "test50c" test50c


(* And a user test *)

fun esc s =
    case explode s of
	[]                  => 100
      | #"\\" :: #"n"  :: _ => 101
      | #"\\" :: #"t"  :: _ => 102
      | #"\\" :: #"v"  :: _ => 103
      | #"\\" :: #"b"  :: _ => 104
      | #"\\" :: #"r"  :: _ => 105
      | #"\\" :: #"f"  :: _ => 106
      | #"\\" :: #"\\" :: _ => 107
      | #"\\" :: #"?"  :: _ => 108
      | #"\\" :: #"'"  :: _ => 109
      | #"\\" :: #"\"" :: _ => 110
      | #"\\" :: #"x"  :: _ => 111
      | #"\\" :: _          => 112
      | _                   => 113;

val test60a = checkres1 esc 
    [("", 100), ("\\nbaf", 101), ("\\\"klam", 110), ("\\yrg", 112), 
     ("abc", 113), ("\nbaf", 113)]
val _ = prln "test60a" test60a

(* Proper compilation of irrefutable subpatterns *)

fun irr1 ((), 1) = 1 
  | irr1 ((), _) = 2;

fun irr2 (1, 1) = 3
  | irr2 (1, _) = 4
  | irr2 (_, _) = 5;

fun irr3 (_, 1) = 6
  | irr3 (_, _) = 7;

val test70 = check 
    (irr1((), 1) = 1 andalso irr1((), 7) = 2
     andalso irr2(1, 1) = 3 andalso irr2(1, 7) = 4 andalso irr2(2, 1) = 5
     andalso irr3(68, 1) = 6 andalso irr3(78, 7) = 7)
val _ = prln "test70" test70

(* Excons defined by equality *)

exception A71;				(* "static" exception *)

val test71 = check
    let exception B71 = A71 
    in (case A71 of B71 => true | A71 => false | _ => false)
       andalso
       (case B71 of A71 => true | B71 => false | _ => false)
    end
val _ = prln "test71" test71

exception A72 of int;			(* "static" exception *)

val test72 = check
    let exception B72 = A72
    in (case A72 3 of B72 3 => true | B72 2 => false 
                    | A72 _ => false | _ => false)
       andalso
       (case A72 3 of B72 2 => false | B72 3 => true
                    | B72 _ => false | _ => false)
       andalso
       (case B72 3 of B72 3 => true | B72 2 => false 
                    | A72 _ => false | _ => false)
       andalso
       (case B72 3 of B72 2 => false | B72 3 => true
                    | B72 _ => false | _ => false)
    end
val _ = prln "test72" test72
