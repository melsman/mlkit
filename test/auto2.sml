

functor TestAuto (eqtype t
                  val toInt : t -> int
                  val t : t*t*t
                  val e : t) : sig end =
struct

fun runtime_test0 (a1:t,a2:t,a3:t) : t =
    prim("@runtime_test0", (a1,a2,a3))

val x : t = runtime_test0 t

val () = if e = x then print "ok\n" else print "err\n"
end

structure T_Int = TestAuto (type t = int
                            val toInt = fn x => x
                            val t = (100,10,1)
                            val e = 135)

structure T_Word8 = TestAuto (type t = Word8.word
                              val toInt = Word8.toInt
                              val t = (0w100:t,0w10:t,0w1:t)
                              val e = 0w135:t)

structure T_Word32 = TestAuto (type t = Word32.word
                              val toInt = Word32.toInt
                              val t = (0w100:t,0w10:t,0w1:t)
                              val e = 0w135:t)

structure T_Char = TestAuto (type t = Char.char
                              val toInt = Char.ord
                              val t = (chr 100:t,chr 10:t,chr 1:t)
                              val e = chr 135:t)

structure T_Word64 = TestAuto (type t = Word64.word
                              val toInt = Word64.toInt
                              val t = (0w100:t,0w10:t,0w1:t)
                              val e = 0w135:t)

structure T_Int64 = TestAuto (type t = Int64.int
                              val toInt = Int64.toInt
                              val t = (100:t,10:t,1:t)
                              val e = 135:t)

structure T_Int16 = TestAuto (type t = Int16.int
                              val toInt = Int16.toInt
                              val t = (Int16.fromInt 100,Int16.fromInt 10,Int16.fromInt 1)
                              val e = Int16.fromInt 135)

structure T_Int8 = TestAuto (type t = Int8.int
                             val toInt = Int8.toInt
                             val t = (Int8.fromInt 10,Int8.fromInt 10,Int8.fromInt 1)
                             val e = Int8.fromInt 45)


functor TestAutoW8Array (type t eqtype e
                         val fromInt : int -> e) = struct
  fun alloc0 (n:int) : t = prim("allocStringML", n)
  fun runtime_test_cstring0 (a:t,idx:int,e:e) : unit =
      prim("@runtime_test_cstring0", (a,idx,e))

  fun sub (t:t,i:int) : e = prim ("__bytetable_sub", (t,i))

  val a = alloc0 6
  val l = [0,1,2,3,4,5,6]
  val l = List.map (fn i => (i,fromInt i)) l
  val () = List.app (fn (i,e) => runtime_test_cstring0(a,i,e)) l
  val () = if List.all (fn (i,r) => sub(a,i) = r) l
           then print "ok - array\n"
           else print "err - array\n"
end


structure T1 = TestAutoW8Array(type t = chararray type e = char
                               val fromInt = Char.chr)

structure T2 = TestAutoW8Array(type t = Word8Array.array type e = Word8.word
                               val fromInt = Word8.fromInt)

structure T2 = TestAutoW8Array(type t = Int8Array.array type e = Int8.int
                               val fromInt = Int8.fromInt)
