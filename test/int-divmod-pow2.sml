(* Unknown dividends exercise strength reduction; IntInf supplies the oracle. *)
fun test (divisor:IntInf.int) split toLarge inputs =
    List.app (fn x =>
      let val (q,r) = split x
          val n = toLarge x
      in if toLarge q = IntInf.div(n,divisor) andalso
            toLarge r = IntInf.mod(n,divisor) then ()
         else raise Fail "integer division/modulo by power of two"
      end) inputs
fun inputs (fromInt:int -> 'a) (minimum:'a option) (maximum:'a option) =
    valOf minimum :: valOf maximum ::
    map fromInt [~9,~8,~7,~1,0,1,7,8,9]
fun p__noinline (x:int) = (x div 8,x mod 8)
fun p8__noinline (x:Int8.int) =
    (Int8.div(x,Int8.fromInt 8),Int8.mod(x,Int8.fromInt 8))
fun p16__noinline (x:Int16.int) =
    (Int16.div(x,Int16.fromInt 8),Int16.mod(x,Int16.fromInt 8))
fun p31__noinline (x:Int31.int) = (x div 8,x mod 8)
fun p32__noinline (x:Int32.int) = (x div 8,x mod 8)
fun p63__noinline (x:Int63.int) = (x div 8,x mod 8)
fun p64__noinline (x:Int64.int) = (x div 8,x mod 8)
val () = test 8 p__noinline Int.toLarge (inputs Int.fromInt Int.minInt Int.maxInt)
val () = test 8 p8__noinline Int8.toLarge (inputs Int8.fromInt Int8.minInt Int8.maxInt)
val () = test 8 p16__noinline Int16.toLarge (inputs Int16.fromInt Int16.minInt Int16.maxInt)
val () = test 8 p31__noinline Int31.toLarge (inputs Int31.fromInt Int31.minInt Int31.maxInt)
val () = test 8 p32__noinline Int32.toLarge (inputs Int32.fromInt Int32.minInt Int32.maxInt)
val () = test 8 p63__noinline Int63.toLarge (inputs Int63.fromInt Int63.minInt Int63.maxInt)
val () = test 8 p64__noinline Int64.toLarge (inputs Int64.fromInt Int64.minInt Int64.maxInt)
(* Highest representable positive powers of two exercise width and boxing. *)
fun high31__noinline (x:Int31.int) = (x div 536870912,x mod 536870912)
fun high32__noinline (x:Int32.int) = (x div 1073741824,x mod 1073741824)
fun high63__noinline (x:Int63.int) = (x div 2305843009213693952,x mod 2305843009213693952)
fun high64__noinline (x:Int64.int) = (x div 4611686018427387904,x mod 4611686018427387904)
val () = test 536870912 high31__noinline Int31.toLarge
              (inputs Int31.fromInt Int31.minInt Int31.maxInt)
val () = test 1073741824 high32__noinline Int32.toLarge
              (inputs Int32.fromInt Int32.minInt Int32.maxInt)
val () = test 2305843009213693952 high63__noinline Int63.toLarge
              (inputs Int63.fromInt Int63.minInt Int63.maxInt)
val () = test 4611686018427387904 high64__noinline Int64.toLarge
              (inputs Int64.fromInt Int64.minInt Int64.maxInt)

(* Unsupported divisors, exceptions, and evaluating the dividend still matter. *)
fun check b = if b then () else raise Fail "integer div/mod evaluation"
val () = check (~7 div 4 = ~2 andalso ~7 mod 4 = 1)
val () = check (7 div 8 = 0 andalso 7 mod 8 = 7)
fun zeroDiv__noinline (x:int) = x div 0
fun zeroMod__noinline (x:int) = x mod 0
fun negativeDiv__noinline (x:int) = x div ~1
fun negativeMod__noinline (x:int) = x mod ~1
fun thirdDiv__noinline (x:int) = x div 3
fun thirdMod__noinline (x:int) = x mod 3
val () = check ((zeroDiv__noinline 7; false) handle Div => true)
val () = check ((zeroMod__noinline 7; false) handle Div => true)
val () = check ((negativeDiv__noinline(valOf Int.minInt); false) handle Overflow => true)
val () = check (negativeMod__noinline(valOf Int.minInt) = 0)
val () = check (thirdDiv__noinline ~7 = ~3 andalso thirdMod__noinline ~7 = 2)
val calls = ref 0
fun dividend__noinline () = (calls := !calls+1; ~7)
val () = check (dividend__noinline() div 1 = ~7 andalso !calls = 1)
val () = check (dividend__noinline() mod 1 = 0 andalso !calls = 2)
val () = check (dividend__noinline() div 4 = ~2 andalso !calls = 3)
val () = check (dividend__noinline() mod 4 = 1 andalso !calls = 4)
exception Dividend
val () = check ((((raise Dividend) : int) mod 1; false) handle Dividend => true)
val () = check ((((raise Dividend) : int) div 1; false) handle Dividend => true)
val () = print "OK\n"
