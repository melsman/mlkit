infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before
fun !(x: 'a ref): 'a = prim ("!", x)
fun printNum (i:int) : unit = prim("printNum", i)

val r00 = ref 0w0
val r01 = ref 0w1
val r02 = ref 0w2
val r03 = ref 0w3
val r04 = ref 0w4
val r05 = ref 0w5
val r06 = ref 0w6
val r07 = ref 0w7
val r08 = ref 0w8
val r09 = ref 0w9

val r10 = ref 0w10
val r11 = ref 0w11
val r12 = ref 0w12
val r13 = ref 0w13
val r14 = ref 0w14
val r15 = ref 0w15
val r16 = ref 0w16
val r17 = ref 0w17
val r18 = ref 0w18
val r19 = ref 0w19

val r20 = ref 0w20
val r21 = ref 0w21
val r22 = ref 0w22
val r23 = ref 0w23
val r24 = ref 0w24
val r25 = ref 0w25
val r26 = ref 0w26
val r27 = ref 0w27
val r28 = ref 0w28
val r29 = ref 0w29

val r30 = ref 0w30
val r31 = ref 0w31
val r32 = ref 0w32
val r33 = ref 0w33
val r34 = ref 0w34
val r35 = ref 0w35
val r36 = ref 0w36
val r37 = ref 0w37
val r38 = ref 0w38
val r39 = ref 0w39

fun max n m =
    if n > m then n else m
fun mk (n,acc) =
    if n <= 0 then acc
    else mk(n-1,n::acc)

fun sumit (nil,acc) = acc
  | sumit (x::xs,acc) = sumit(xs,max x acc)

fun alloc () =
    let val xs = mk(500000,nil)
    in sumit(xs,0)
    end

fun toWord (i:int):word = prim("id",i)

fun sum () =
    let
      val x00 = !r00
      val x01 = !r01
      val x02 = !r02
      val x03 = !r03
      val x04 = !r04
      val x05 = !r05
      val x06 = !r06
      val x07 = !r07
      val x08 = !r08
      val x09 = !r09

      val x10 = !r10
      val x11 = !r11
      val x12 = !r12
      val x13 = !r13
      val x14 = !r14
      val x15 = !r15
      val x16 = !r16
      val x17 = !r17
      val x18 = !r18
      val x19 = !r19

      val x20 = !r20
      val x21 = !r21
      val x22 = !r22
      val x23 = !r23
      val x24 = !r24
      val x25 = !r25
      val x26 = !r26
      val x27 = !r27
      val x28 = !r28
      val x29 = !r29

      val x30 = !r30
      val x31 = !r31
      val x32 = !r32
      val x33 = !r33
      val x34 = !r34
      val x35 = !r35
      val x36 = !r36
      val x37 = !r37
      val x38 = !r38
      val x39 = !r39

      val x0 = x00 + x01 + x02 + x03 + x04 + x05 + x06 + x07 + x08 + x09
      val x1 = x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19
      val x2 = x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29
      val x3 = x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39
      val y = x0 + x1 + x2 + x3
      val v = alloc()
(*      val () = printNum 0 *)
(*      val () = printNum v *)
      val x0 = x00 + x01 + x02 + x03 + x04 + x05 + x06 + x07 + x08 + x09
      val x1 = x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19
      val x2 = x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29
      val x3 = x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39
      val y = x0 + x1 + x2 + x3
    in toWord v + y + y
    end

val x = sum()
fun toIntX (w : word) : int = prim("id", w)

val () = printNum (toIntX x)
