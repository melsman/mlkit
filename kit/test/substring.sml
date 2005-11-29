(* test/substring.sml 1995-04-27, 1997-06-03, 1999-03-02, 2000-10-17 

   - modified for the MLKit; mael 2005-11-28 *)

fun ptest t s = print(t ^ ": " ^ s ^ "\n")
infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds p = check'(fn _ => range bounds p)

local 
    open Char Substring
    fun base2 (a, b) = (base a, base b)

    val s1 = ""				(* String.size s1 =  0 *)
    and s2 = "ABCDE\tFGHI";		(* String.size s2 = 10 *)
    val ss1 = full s1			(* size s1 =  0 *)
    and ss2 = full s2;			(* size s2 = 10 *)

    val sa = "AAAAaAbAABBBB";		(* String.size sa = 14 *)
    (*            45678      *)

    val ssa1 = extract(sa, 4, SOME 0)	(* size ssa1 = 0 *)
    val ssa2 = extract(sa, 4, SOME 5)	(* size ssa2 = 5 *)

    val ss3 = extract("junk this is a   (clear)textjunk", 4, SOME 24);
    (*                       456789012345678901234567        *)

in


val test1a = 
    check'(fn _ => 
	   (s2, 10, 0) = base(extract(s2, 10, SOME 0))
	   andalso (s2, 10, 0) = base(extract(s2, 10, NONE))
	   andalso (s2, 0,  0) = base(extract(s2, 0, SOME 0))
	   andalso (s2, 4,  3) = base(extract(s2, 4, SOME 3))
	   andalso (s2, 4,  6) = base(extract(s2, 4, SOME 6))
	   andalso (s2, 4,  6) = base(extract(s2, 4, NONE))
	   andalso (s2, 0, 10) = base(extract(s2, 0, SOME 10))
	   andalso (s2, 0, 10) = base(extract(s2, 0, NONE)));
val _ = ptest "test1a" test1a

val test1b = (extract(s2, ~1, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1b" test1b
val test1c = (extract(s2, 11, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1c" test1c
val test1d = (extract(s2, 0, SOME 11) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1d" test1d
val test1e = (extract(s2, 10, SOME 1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1e" test1e
val test1f = (extract(s2, ~1, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1f" test1f
val test1g = (extract(s2, 11, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test1g" test1g

val test1h =
    check'(fn _ =>
	   string ssa1 = ""
	   andalso string ssa2 = "aAbAA"
	   andalso s1 = string (full s1) 
	   andalso s2 = string (full s2));
val _ = ptest "test1h" test1h

val test2a = 
    check'(fn _ => 
	   string(triml 6 ss2) = "FGHI"
	   andalso s2 = string(triml 0 ss2)
	   andalso s1 = string(triml 0 ss1)
	   andalso (s2, 10, 0) = base(triml 10 ss2)
	   andalso (s2, 10, 0) = base(triml 11 ss2)
	   andalso (sa, 6, 3) = base(triml 2 ssa2)
	   andalso (sa, 9, 0) = base(triml 5 ssa2)
	   andalso (sa, 9, 0) = base(triml 6 ssa2));
val _ = ptest "test2a" test2a

val test2b = (triml ~1 seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2b" test2b
val test2c = (triml ~1 seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2c" test2c

val test3a = 
    check'(fn _ => 
	   string(trimr 6 ss2) = "ABCD"
	   andalso s2 = string(trimr 0 ss2)
	   andalso s1 = string(trimr 0 ss1)
	   andalso (s2, 0, 0) = base(trimr 10 ss2)
	   andalso (s2, 0, 0) = base(trimr 11 ss2)
	   andalso (sa, 4, 3) = base(trimr 2 ssa2)
	   andalso (sa, 4, 0) = base(trimr 5 ssa2)
	   andalso (sa, 4, 0) = base(trimr 6 ssa2));
val _ = ptest "test3a" test3a

val test3b = (trimr ~1 seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test3b" test3b
val test3c = (trimr ~1 seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test3c" test3c

val test4 = 
    check'(fn _ => 
	   isEmpty ss1 
	   andalso not (isEmpty ss2)
	   andalso isEmpty ssa1
	   andalso not (isEmpty ssa2));
val _ = ptest "test4" test4

val test5a = 
    check'(fn _ =>
	   case getc ssa1 of NONE => true | _ => false);
val _ = ptest "test5a" test5a

val test5b = 
    check'(fn _ =>
	   case getc ssa2 of
	       NONE             => false 
	     | SOME(#"a", rest) => "AbAA" = string rest
	     | _                => false);
val _ = ptest "test5b" test5b

val test6 = 
    check'(fn _ =>
	   first ssa1 = NONE
	   andalso first ssa2 = SOME #"a")
val _ = ptest "test6" test6

val test7 = 
    check'(fn _ => (size ss1 = 0 andalso size ss2 = 10
		    andalso size ssa1 = 0 andalso size ssa2 = 5));
val _ = ptest "test7" test7

val test8a = 
    check'(fn _ => (sub(ss2,6) = chr 70 andalso sub(ss2,9) = chr 73
		    andalso sub(ssa2, 1) = chr 65));
val _ = ptest "test8a" test8a
val test8b = 
    (sub(ss1, 0)  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8b" test8b
val test8c = 
    (sub(ss2, ~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8c" test8c
val test8d = 
    (sub(ss2, 10) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8d" test8d
val test8e = 
    (sub(ssa2, ~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8e" test8e
val test8f = 
    (sub(ssa2, 5) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8f" test8f

val test9a = 
    check'(fn _ => 
	   base ss2 = base(slice(ss2, 0, SOME (size ss2)))
	   andalso base ss2 = base(slice(ss2, 0, NONE))
	   andalso (s2, 10, 0) = base(slice(ss2, size ss2, SOME 0))
	   andalso (s2, 10, 0) = base(slice(ss2, size ss2, NONE))
	   andalso base ss1 = base(slice(ss1, 0, SOME 0))
	   andalso base ss1 = base(slice(ss1, 0, NONE)));
val _ = ptest "test9a" test9a

val test9b = 
    check'(fn _ => 
	           (sa, 4, 5) = base(slice(ssa2, 0, SOME 5))
	   andalso (sa, 4, 5) = base(slice(ssa2, 0, NONE))
	   andalso (sa, 4, 0) = base(slice(ssa2, 0, SOME 0))
	   andalso (sa, 9, 0) = base(slice(ssa2, 5, SOME 0))
	   andalso (sa, 9, 0) = base(slice(ssa2, 5, NONE))
	   andalso (sa, 5, 3) = base(slice(ssa2, 1, SOME 3))
	   andalso (sa, 5, 4) = base(slice(ssa2, 1, SOME 4))
	   andalso (sa, 5, 4) = base(slice(ssa2, 1, NONE)));
val _ = ptest "test9b" test9b

val test9c = (slice(ssa2, ~1, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9c" test9c
val test9d = (slice(ssa2, 6, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9d" test9d
val test9e = (slice(ssa2, 0, SOME 6) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9e" test9e
val test9f = (slice(ssa2, 5, SOME 1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9f" test9f
val test9g = (slice(ssa2, ~1, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9g" test9g
val test9h = (slice(ssa2, 6, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test9h" test9h

val test12 =
    check'(fn _ => 
	   concat [] = ""
	   andalso concat [ssa1, ssa1, ssa1] = ""
	   andalso concat [ssa2, ssa2, ssa2] = "aAbAAaAbAAaAbAA"
	   andalso concat [ssa2, ssa1, ss2, ss1] = "aAbAAABCDE\tFGHI");
val _ = ptest "test12" test12

val test13 = 
    check'(fn _ => 
	   explode ss1 = []
	   andalso explode ssa1 = []
	   andalso explode ssa2 = [#"a", #"A", #"b", #"A", #"A"]);
val _ = ptest "test13" test13

val test14 = 
    check'(fn _ => 
	   EQUAL = compare(ssa1,ssa1) andalso EQUAL = compare(ssa2,ssa2)
	   andalso LESS = compare(triml 1 ssa2, ssa2)
	   andalso GREATER = compare(ssa2, triml 1 ssa2)
	   andalso LESS = compare(trimr 1 ssa2, ssa2)
	   andalso GREATER = compare(ssa2, trimr 1 ssa2)
	   andalso LESS = compare(full "AB", ssa2)
	   andalso GREATER = compare(ssa2, full "AB"));
val _ = ptest "test14" test14

fun finda c = c <> #"A";
fun findb c = c <> #"B";

val test15 = 
    check'(fn _ =>
	   (sa, 5, 4) = base(dropl finda ssa2)
	   andalso (sa, 9, 0) = base(dropl findb ssa2)
	   andalso base ssa1 = base(dropl finda ssa1));
val _ = ptest "test15" test15

val test16 = 
    check'(fn _ =>
	   (sa, 4, 5) = base(dropr finda ssa2)
	   andalso (sa, 4, 0) = base(dropr findb ssa2)
	   andalso base ssa1 = base(dropr finda ssa1));
val _ = ptest "test16" test16

val test17 = 
    check'(fn _ =>
	   (sa, 4, 1) = base(takel finda ssa2)
	   andalso (sa, 4, 5) = base(takel findb ssa2)
	   andalso base ssa1 = base(takel finda ssa1));
val _ = ptest "test17" test17
    
val test18 = 
    check'(fn _ =>
	   (sa, 9, 0) = base(taker finda ssa2)
	   andalso (sa, 4, 5) = base(taker findb ssa2)
	   andalso base ssa1 = base(taker finda ssa1));
val _ = ptest "test18" test18

val test19 =
    check'(fn _ => 
	   ((sa, 4, 1), (sa, 5, 4)) = base2(splitl finda ssa2)
	   andalso ((sa, 4, 5), (sa, 9, 0)) = base2(splitl findb ssa2)
	   andalso base2(ssa1, ssa1) = base2(splitl finda ssa1));
val _ = ptest "test19" test19

val test20 =
    check'(fn _ => 
	   ((sa, 4, 5), (sa, 9, 0)) = base2(splitr finda ssa2)
	   andalso ((sa, 4, 0), (sa, 4, 5)) = base2(splitr findb ssa2)
	   andalso base2(ssa1, ssa1) = base2 (splitr finda ssa1));
val _ = ptest "test20" test20

val test21 = 
    check'(fn _ => 
	   ((sa, 4, 0), (sa, 4, 5)) = base2(position "" ssa2)
	   andalso ((sa, 4, 1), (sa, 5, 4)) = base2(position "Ab" ssa2)
	   andalso ((sa, 4, 5), (sa, 9, 0)) = base2(position "B" ssa2)
	   andalso ((sa, 4, 5), (sa, 9, 0)) = base2(position "AAB" ssa2)
	   andalso ((sa, 4, 0), (sa, 4, 5)) = base2(position "aA" ssa2)
	   andalso ((sa, 4, 2), (sa, 6, 3)) = base2(position "bAA" ssa2)
	   andalso (base ssa1, base ssa1) = base2(position "A" ssa1)
	   andalso (base ssa1, base ssa1) = base2(position "" ssa1));
val _ = ptest "test21" test21

(* For the pre-November 1995 version of position: 
val test21 = 
    check'(fn _ => 
	   (sa, 4, 5) = base(position "" ssa2)
	   andalso (sa, 5, 4) = base(position "Ab" ssa2)
	   andalso (sa, 9, 0) = base(position "B" ssa2)
	   andalso (sa, 9, 0) = base(position "AAB" ssa2)
	   andalso (sa, 4, 5) = base(position "aA" ssa2)
	   andalso (sa, 6, 3) = base(position "bAA" ssa2)
	   andalso base ssa1 = base(position "A" ssa1)
	   andalso base ssa1 = base(position "" ssa1));
*)

val test22a = 
    check'(fn _ => 
	   (translate (fn _ => "") ssa2 = ""
	    andalso translate (fn x => str x) ssa1 = ""
	    andalso translate (fn x => str x) ssa2 = string ssa2));
val _ = ptest "test22a" test22a

val test22b = 
    check'(fn _ => 
	   (translate (fn c => if c = #"b" then "XYZ " else str c) ssa2
	              = "aAXYZ AA"));
val _ = ptest "test22b" test22b

val test23 = 
    check'(fn _ => 
	   (null(tokens isSpace ssa1)
	    andalso null(tokens (Char.contains "Aab") ssa2)
	    andalso map string (tokens (fn c => c = #"A") ssa2) = ["a","b"]));
val _ = ptest "test23" test23

val test24 = 
    check'(fn _ => 
	   (map base (fields isSpace ssa1) = [base ssa1]
	    andalso map base (fields (contains "Aab") ssa2)
	            = [(sa,4,0),(sa,5,0),(sa,6,0),(sa,7,0),(sa,8,0),(sa,9,0)]
	    andalso map string (fields (fn c => c = #"A") ssa2) 
	            = ["a","b","",""]));
val _ = ptest "test24" test24

val test25 = 
    check'(fn _ => 
	   null(tokens (fn _ => true) ss3)
	   andalso null(tokens (fn _ => false) (full ""))
	   andalso null(tokens (contains " ()") (full "(()())(( ()"))
	   andalso ["this","is","a","clear","text"] = 
                           map string (tokens (contains " ()") ss3));
val _ = ptest "test25" test25

local 
    val v = ref 0
    fun setv c = v := ord c;
in 
    
val test26a = 
    check'(fn _ => 
	   (v := 0;
	    foldl (fn (x, _) => setv x) () ssa2;
	    !v = 65));
val _ = ptest "test26a" test26a

val test26b = 
    check'(fn _ => 
	   implode(foldl (op ::) [] ssa2) = "AAbAa");
val _ = ptest "test26b" test26b

val test27a = 
    check'(fn _ => 
	   (v := 0;
	   foldr (fn (x, _) => setv x) () ssa2;
	   !v = 97));
val _ = ptest "test27a" test27a


val test27b = 
    check'(fn _ => 
	   implode(foldr (op ::) [] ssa2) = "aAbAA");
val _ = ptest "test27b" test27b

val test28 = 
    check'(fn _ => 
	   (v := 0;
	    app setv ssa2;
	    !v = 65));
val _ = ptest "test28" test28
end

val test29a = 
    check'(fn _ =>
	   base2(splitAt(ssa1, 0)) = ((sa, 4, 0), (sa, 4, 0))
	   andalso base2(splitAt(ssa2, 0)) = ((sa, 4, 0), (sa, 4, 5))
	   andalso base2(splitAt(ssa2, 1)) = ((sa, 4, 1), (sa, 5, 4))
	   andalso base2(splitAt(ssa2, 4)) = ((sa, 4, 4), (sa, 8, 1))
	   andalso base2(splitAt(ssa2, 5)) = ((sa, 4, 5), (sa, 9, 0)));
val _ = ptest "test29a" test29a
val test29b = (splitAt(ssa2, ~1) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test29b" test29b
val test29c = (splitAt(ssa2, 6) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test29c" test29c

val test30a = 
    check'(fn _ => 
	   (s2, 10, 0) = base(substring(s2, 10, 0))
	   andalso (s2, 0,  0) = base(substring(s2, 0, 0))
	   andalso (s2, 4,  3) = base(substring(s2, 4, 3))
	   andalso (s2, 4,  6) = base(substring(s2, 4, 6))
	   andalso (s2, 0, 10) = base(substring(s2, 0, 10)));
val _ = ptest "test30a" test30a

val test30b = (substring(s2, ~1, 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test30b" test30b
val test30c = (substring(s2, 11, 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test30c" test30c
val test30d = (substring(s2, 0, 11) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test30d" test30d
val test30e = (substring(s2, 10, 1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test30e" test30e
val test30f = 
    case (Int.minInt, Int.maxInt) of
	(SOME min, SOME max) => 
	    ((substring("", max, max); "WRONG")
	     handle Subscript => 
		 ((substring("", min, min); "WRONG")
		  handle Subscript => "OK" | _ => "WRONG"))
       | _ => "OK";
val _ = ptest "test30f" test30f

(* val sa = "AAAAaAbAABBBB"; *)
val test31a = 
    check'(fn _ => 
	   isPrefix "" (substring(sa, 0, 0))
	   andalso isPrefix "" (substring(sa, 13, 0))
	   andalso isPrefix "" ssa1
	   andalso isPrefix "aAbAA" ssa2
	   andalso isPrefix "aAbA" ssa2
	   andalso not (isPrefix "aAbAAB" ssa2)
	   andalso not (isPrefix "aAbAAB" ssa1)
	   andalso not (isPrefix "AAA" ssa2)
	   andalso not (isPrefix "AaA" ssa2)
	   andalso not (isPrefix "AAB" ssa2))
val _ = ptest "test31a" test31a

(* val sa = "AAAAaAbAABBBB"; *)
val test31b = 
    check'(fn _ => 
	   isSuffix "" (substring(sa, 0, 0))
	   andalso isSuffix "" (substring(sa, 13, 0))
	   andalso isSuffix "" ssa1
	   andalso isSuffix "aAbAA" ssa2
	   andalso isSuffix "AbAA" ssa2
	   andalso not (isSuffix "baAbAA" ssa2)
	   andalso not (isSuffix "baAbAA" ssa1)
	   andalso not (isSuffix "AAA" ssa2)
	   andalso not (isSuffix "AaA" ssa2)
	   andalso not (isSuffix "AAB" ssa2))
val _ = ptest "test31b" test31b

(* val sa = "AAAAaAbAABBBB"; *)
val test31c = 
    check'(fn _ => 
	   isSubstring "" (substring(sa, 0, 0))
	   andalso isSubstring "" (substring(sa, 13, 0))
	   andalso isSubstring "" ssa1
	   andalso isSubstring "aAbAA" ssa2
	   andalso isSubstring "AbAA" ssa2
	   andalso isSubstring "aAbA" ssa2
	   andalso not (isSubstring "baAbAA" ssa2)
	   andalso not (isSubstring "baAbAA" ssa1)
	   andalso not (isSubstring "AAA" ssa2)
	   andalso not (isSubstring "AaA" ssa2)
	   andalso not (isSubstring "AAB" ssa2))
val _ = ptest "test31c" test31c

fun eqspan(sus1, sus2, res) = base(span(sus1, sus2)) = base res

val test32a = check'(fn _ =>
   eqspan(substring(sa, 0, 0), substring(sa, 0, 13), full sa)
   andalso eqspan(substring(sa, 0, 13), substring(sa, 13, 0), full sa)
   andalso eqspan(substring(sa, 5, 0), substring(sa, 5, 0), substring(sa, 5,0))
   andalso eqspan(substring(sa, 0, 5), substring(sa, 5, 8), full sa)
   andalso eqspan(substring(sa, 0, 13), substring(sa, 0, 13), full sa)
   andalso eqspan(substring(sa, 5, 4), substring(sa, 2, 4), substring(sa,5,1))
   andalso eqspan(substring(sa, 2, 5), substring(sa, 6, 3), substring(sa, 2,7))
   andalso eqspan(substring("abcd", 1, 0), substring("abcd", 1, 2), 
		  substring("abcd", 1, 2))
   andalso eqspan(substring("", 0, 0), substring("", 0, 0), full ""))
val _ = ptest "test32a" test32a

val test32b = (span(substring("a", 0, 0), substring("b", 0, 0)) seq "WRONG") 
              handle Span => "OK" | _ => "WRONG";
val _ = ptest "test32b" test32b

val test32c = (span(substring(sa, 1, 0), substring(sa, 0, 0)) seq "WRONG") 
              handle Span => "OK" | _ => "WRONG";
val _ = ptest "test32c" test32c

val test32d = (span(substring(sa, 3, 2), substring("abcd", 2, 1)) seq "WRONG") 
              handle Span => "OK" | _ => "WRONG";
val _ = ptest "test32d" test32d

val test32e = (span(substring("a", 0, 0), substring("b", 0, 0)) seq "WRONG") 
              handle Span => "OK" | _ => "WRONG";
val _ = ptest "test32e" test32e

val test33a = 
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Char.compare (c2, c1) 
	       fun coll s1 s2 = collate invcompare (full s1, full s2)
	   in 
	       coll "" "" = EQUAL
	       andalso coll "" " " = LESS
	       andalso coll " " "" = GREATER
	       andalso coll "ABCD" "ABCD" = EQUAL
	       andalso coll "ABCD" "ABCD " = LESS
	       andalso coll "ABCD " "ABCD" = GREATER
	       andalso coll "B" "ABCD" = LESS
	       andalso coll "ABCD" "B" = GREATER
	       andalso coll "CCCB" "CCCABCD" = LESS
	       andalso coll "CCCABCD" "CCCB" = GREATER
	       andalso coll "CCCB" "CCCA" = LESS
	       andalso coll "CCCA" "CCCB" = GREATER
	   end)
val _ = ptest "test33a" test33a

(* val sa = "AAAAaAbAABBBB"; *)
(*           0123456789012   *)
val test33b = 
    check'(fn _ =>
  let fun invcompare (c1, c2) = Char.compare (c2, c1) 
      fun coll s1 s2 = collate invcompare (s1, s2)
  in 
      coll (full sa) (substring(sa, 0, 13)) = EQUAL
      andalso coll (substring(sa, 0, 0)) (substring(sa, 13, 0)) = EQUAL
      andalso coll (substring(sa, 0, 0)) (substring(sa, 0, 13)) = LESS
      andalso coll (substring(sa, 0, 13)) (substring(sa, 0, 0)) = GREATER
      andalso coll (substring(sa, 0, 3)) (substring(sa, 1, 3)) = EQUAL
      andalso coll (substring(sa, 0, 4)) (substring(sa, 1, 4)) = GREATER
      andalso coll (substring(sa, 1, 4)) (substring(sa, 0, 4)) = LESS
  end)
val _ = ptest "test33b" test33b

val test34 = 
    check'(fn _ =>
	   concatWith "+" [] = ""
	   andalso concatWith "" [] = ""
	   andalso concatWith "+" [full "abc"] = "abc"
	   andalso concatWith "+" [full "h3", full "h2", full "h1"] = "h3+h2+h1"
	   andalso concatWith "+-" [full "h3", full "h2", full "h1"]="h3+-h2+-h1"
	   andalso concatWith "+-" [full "", full "", full ""]="+-+-"
	   andalso concatWith "" [full "h3", full "h2", full "h1"] = "h3h2h1"
	   andalso concatWith "678" [ssa1, ssa1, ssa1]="678678"
	   andalso concatWith "678" [ssa2, ssa2, ssa2]="aAbAA678aAbAA678aAbAA"
	   andalso concatWith "678" [ssa2, ssa1, ss2, ss1] = 
	   "aAbAA678678ABCDE\tFGHI678")
val _ = ptest "test34" test34

end; 
