datatype t = Var of string | Add of t * t | Mul of t * t | R of real
fun mkt i = if i < 0 then R (real (~i))
            else if i mod 2 = 0 then Mul(mkt(i-3),mkt(i-1))
            else if i = 3 then Var "a"
            else Add(mkt(i-2),mkt(i-4))
val a = mkt 8
val b = mkt 9;
:set pretty_depth 6;
datatype s = L | B of s * s
fun mks i = if i < 0 then L else B(mks(i-1),mks(i-2))
val c = mks 5;
:quit;
