fun check (name, true) = ()
  | check (name, false) = raise Fail name
fun word__noinline (x:Word32.word) = x
fun int__noinline (x:Int32.int) = x
fun plus__noinline (x:Word32.word,y) = x + y
fun mul__noinline (x:Word32.word,y) = x * y
fun sub__noinline (x:Word32.word,y) = x - y
fun shift__noinline (x:Word32.word,y) = Word32.<< (x,y)
fun signedShift__noinline (x:Word32.word,y) = Word32.~>> (x,y)
fun addInt__noinline (x:Int32.int,y) = x + y
fun divInt__noinline (x:Int32.int,y) = x div y
fun remInt__noinline (x:Int32.int,y) = Int32.rem (x,y)
fun cast__noinline (x:Int32.int) : Word32.word = prim ("id", x)
fun words9__noinline (a:Word32.word,b,c,d,e,f,g,h,i) =
    List.foldl Word32.xorb a [b,c,d,e,f,g,h,i]
fun switchInt__noinline (x:Int32.int) =
    case x of ~2147483648 => 0 | ~2147483647 => 1 | ~2147483646 => 2
            | ~2147483645 => 3 | ~2147483644 => 4 | _ => 99
fun switchWord__noinline (x:Word32.word) =
    case x of 0wx7ffffffe => 0 | 0wx7fffffff => 1 | 0wx80000000 => 2
            | 0wx80000001 => 3 | 0wx80000002 => 4 | _ => 99
fun ffiWord__noinline (x:Word32.word) : Word32.word = prim ("@mlkit_word32_identity", x)
fun ffiInt__noinline (x:Int32.int) : Int32.int = prim ("@mlkit_int32_identity", x)
val w = word__noinline 0wxffffffff
val i = int__noinline (~2147483648)
val () = check ("word encoding", (prim ("mlkit_check_word32", w) : int) = 1)
val () = check ("int encoding", (prim ("mlkit_check_int32", i) : int) = 1)
val () = check ("unboxed cast", cast__noinline (~1) = w)
val () = check ("stack word arguments", words9__noinline
                (0wx7fffffff,0wx7fffffff,0wx7fffffff,0wx7fffffff,0wx7fffffff,
                 0wx7fffffff,0wx7fffffff,0wx7fffffff,0wx7fffffff) = 0wx7fffffff)
val () = check ("dense integer switch", switchInt__noinline (~2147483648) = 0 andalso
                switchInt__noinline (~2147483644) = 4 andalso switchInt__noinline (~2147483643) = 99)
val () = check ("dense word switch", switchWord__noinline 0wx7fffffff = 1 andalso
                switchWord__noinline 0wx80000002 = 4 andalso switchWord__noinline 0wx80000003 = 99)
val () = check ("word wrap", plus__noinline (w,0w1) = 0w0)
val () = check ("word multiply", mul__noinline (w,w) = 0w1)
val () = check ("word subtract", sub__noinline (0w0,0w1) = w)
val () = check ("large shift", shift__noinline (w,0w64) = 0w0)
val () = check ("signed shift", signedShift__noinline (w,0w64) = w)
val computedMin = addInt__noinline (int__noinline (~2147483647),~1)
val () = check ("computed int encoding", (prim ("mlkit_check_int32", computedMin) : int) = 1)
val dividedMin = divInt__noinline (i,1)
val () = check ("runtime int encoding", (prim ("mlkit_check_int32", dividedMin) : int) = 1)
val () = check ("int overflow", ((addInt__noinline (i,~1); false) handle Overflow => true))
val () = check ("division overflow", ((divInt__noinline (i,~1); false) handle Overflow => true))
val () = check ("minimum remainder", remInt__noinline (i,~1) = 0)
val () = check ("word FFI", ffiWord__noinline w = w)
val () = check ("int FFI", List.all
                (fn x => ffiInt__noinline x = x) [i,~1,0,1,2147483647])
val () = check ("word FFI boundaries", List.all
                (fn x => ffiWord__noinline x = x) [0w0,0w1,0wx80000000,w])
val () = check ("unsigned widening", Word32.toLargeWord w = 0wxffffffff)
val () = check ("signed widening", Word32.toLargeWordX w = 0wxffffffffffffffff)
val () = check ("signed large", Int32.fromLarge (Int32.toLarge i) = i)
val () = check ("unsigned large", Word32.fromLargeInt (Word32.toLargeInt w) = w)
val packed = Word32Array.fromList [w,0w0,0wx80000000]
val () = Word32Array.update (packed,1,w)
val () = check ("packed words", Word32Array.sub(packed,0) = w andalso
                Word32Array.sub(packed,1) = w andalso
                Word32Array.sub(packed,2) = 0wx80000000)
val bytes = Word8Array.array (4,0w0)
val () = PackWord32Little.update (bytes,0,Word32.toLargeWord w)
val () = check ("packed bytes", PackWord32Little.subArr(bytes,0) = Word32.toLargeWord w)
datatype mixedPayload = Signed of Int32.int | Unsigned of Word32.word | Empty
fun match__noinline (Signed x) = x = i
  | match__noinline (Unsigned x) = x = w
  | match__noinline Empty = false
val () = check ("mixed datatype signed", match__noinline (Signed i))
val () = check ("mixed datatype unsigned", match__noinline (Unsigned w))
val () = check ("mixed arithmetic payload", match__noinline (Signed computedMin))
val () = check ("mixed runtime payload", match__noinline (Signed dividedMin))
val () = check ("mixed FFI payload", match__noinline (Signed (ffiInt__noinline i)))
(* Keep polymorphic structures and captured values live across allocation/GC. *)
fun roots__noinline (w:Word32.word,i:Int32.int) =
    let val r = ref [(w,i)]
        val a = Array.fromList [w,0w0]
        val v = Vector.fromList [i,0]
        fun captured__noinline () = (w,i)
        fun allocate (0, xs) = xs
          | allocate (n, xs) = allocate (n-1, (Word32.fromInt n,Int32.fromInt n)::xs)
        val xs = allocate (100000, [])
    in check ("live roots", List.length xs = 100000 andalso !r = [captured__noinline ()]
              andalso Array.sub(a,0) = w andalso Vector.sub(v,0) = i)
    end
val () = roots__noinline (w,i)
val () = print "intword32 native: OK\n"
