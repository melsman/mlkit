(* Compare constant operations with arbitrary-precision arithmetic, including
 * unsigned values above maxInt and shifts at and beyond the word width. *)
functor CheckWordConstant (W : WORD) =
struct
    val bits = W.wordSize
    fun pow n = IntInf.<<(1,Word.fromInt n)
    val modulus = pow bits
    val top = IntInf.-(modulus,1)
    fun check (actual,expected) =
        if W.toLargeInt actual = expected then () else raise Fail "word constant"
    val values = List.map W.fromLargeInt
        [0,1,2,3,7,2356,2357,2358,75647,IntInf.div(modulus,2),top-1,top]
    val remainders : (IntInf.int * (W.word -> W.word)) list =
        [(1,fn x => W.mod(x,(W.fromInt 1))),(2,fn x => W.mod(x,(W.fromInt 2))),
         (3,fn x => W.mod(x,(W.fromInt 3))),(7,fn x => W.mod(x,(W.fromInt 7))),
         (2357,fn x => W.mod(x,(W.fromInt 2357)))]
    fun remainder x = List.app
        (fn (d,f) => check(f x,IntInf.mod(W.toLargeInt x,d))) remainders
    val shifts =
        [(0,fn x => W.<<(x,0w0),fn x => W.>>(x,0w0),fn x => W.~>>(x,0w0)),
         (1,fn x => W.<<(x,0w1),fn x => W.>>(x,0w1),fn x => W.~>>(x,0w1)),
         (31,fn x => W.<<(x,0w31),fn x => W.>>(x,0w31),fn x => W.~>>(x,0w31)),
         (32,fn x => W.<<(x,0w32),fn x => W.>>(x,0w32),fn x => W.~>>(x,0w32)),
         (62,fn x => W.<<(x,0w62),fn x => W.>>(x,0w62),fn x => W.~>>(x,0w62)),
         (63,fn x => W.<<(x,0w63),fn x => W.>>(x,0w63),fn x => W.~>>(x,0w63)),
         (64,fn x => W.<<(x,0w64),fn x => W.>>(x,0w64),fn x => W.~>>(x,0w64)),
         (65,fn x => W.<<(x,0w65),fn x => W.>>(x,0w65),fn x => W.~>>(x,0w65))]
    fun shift x = List.app
        (fn (n,left,right,signed) =>
            let val u = W.toLargeInt x
                val s = if u >= IntInf.div(modulus,2) then u-modulus else u
                val n = Int.min(n,bits)
            in
                check(left x,IntInf.mod(u * pow n,modulus));
                check(right x,IntInf.div(u,pow n));
                check(signed x,IntInf.mod(IntInf.div(s,pow n),modulus))
            end) shifts
    fun random (0,_) = ()
      | random (n,x) =
        (remainder x; shift x;
         random(n-1,W.+(W.*(x,(W.fromInt 1664525)),(W.fromInt 1013904223))))
    val () = List.app (fn x => (remainder x; shift x)) values
    val () = random(1000,(W.fromInt 12345))
    fun zero x = W.mod(x,(W.fromInt 0))
    val () = List.app (fn x =>
        if ((zero x; false) handle Div => true) then () else raise Fail "Div") values
end
structure Default = CheckWordConstant(Word)
structure W32 = CheckWordConstant(Word32)
structure W64 = CheckWordConstant(Word64)
fun high64 x = Word64.mod(x,0wx8000000000000001)
fun max64 x = Word64.mod(x,0wxFFFFFFFFFFFFFFFF)
val () = List.app (fn x =>
    if Word64.toLargeInt(high64 x) = IntInf.mod(Word64.toLargeInt x,9223372036854775809)
       andalso Word64.toLargeInt(max64 x) = IntInf.mod(Word64.toLargeInt x,18446744073709551615)
    then () else raise Fail "high divisor")
    [0w0,0wx8000000000000000,0wx8000000000000001,0wxFFFFFFFFFFFFFFFE,0wxFFFFFFFFFFFFFFFF]
val () = print "OK\n"
