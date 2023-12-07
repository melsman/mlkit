datatype t = Var of string | Add of t * t | Mul of t * t | R of real | Zero
fun mkt i = if i < 0 then R (real (~i))
            else if i mod 2 = 0 then Mul(mkt(i-3),mkt(i-1))
            else if i = 3 then Var "a" else if i = 1 then Zero
            else Add(mkt(i-2),mkt(i-3))
val a = mkt 8
val b = mkt 9;
:set pretty_depth 6;
datatype s = L | B of s * s
fun mks i = if i < 0 then L else B(mks(i-1),mks(i-2))
val c = mks 5;
datatype e = S | K | D | A | U
val e = [S,A,U,D,D,S,K];
datatype single = Single of int
val s = Single 322;
datatype ('a,'b)t = A of 'a * 'b;
val x = A(4,2.3);
:quit;
