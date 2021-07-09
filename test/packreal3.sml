(* Test case from Moscow ML, by KFL *)

fun check' f = (if f () then "OK" else "WRONG") handle ? => "EXN ("^(exnName ?)^")";

functor Test(P : sig
                   val subVec : Word8Vector.vector * int -> real
                   val subArr : Word8Array.array * int -> real
                   val update : Word8Array.array * int * real -> unit
                 end) =
struct

val encoded : word8 list =
    [0wx40, 0wx09, 0wx21, 0wxFB, 0wx54, 0wx44, 0wx2D, 0wx18,
     0wx40, 0wx09, 0wx21, 0wxFB, 0wx54, 0wx44, 0wx2D, 0wx18]

val test_subvec =
    let val encoded = Word8Vector.fromList encoded
    in  check'(fn _ => Real.== (P.subVec(encoded, 0),
                                P.subVec(encoded, 1)))
    end;

val test_subarr =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ => Real.== (P.subArr(encoded, 0),
                                P.subArr(encoded, 1)))
    end;

val test_update0 =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ =>
                  ( P.update(encoded, 0, 0.0)
                  ; Real.== (P.subArr(encoded, 0),
                             0.0)
                  ))
    end;

val test_update1 =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ =>
                  ( P.update(encoded, 1, 1.0)
                  ; Real.== (P.subArr(encoded, 1),
                             1.0)
                  ))
    end;
end

structure TestBig = Test(PackRealBig)
structure TestLittle = Test(PackRealLittle)

val _ = app (fn s => print(s^"\n"))
            [ "Testing PackRealBig"
            , TestBig.test_subvec
            , TestBig.test_subarr
            , TestBig.test_update0
            , TestBig.test_update1
            , "Testing PackRealLittle"
            , TestLittle.test_subvec
            , TestLittle.test_subarr
            , TestLittle.test_update0
            , TestLittle.test_update1
            ];
