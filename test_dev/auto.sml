local
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int) : unit = prim("printNum", n)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun printCharArray (s:chararray) : unit = prim("printStringML", s)
fun chr (i:int) : char = if i>=0 andalso i<256 then prim ("id", i)
			 else raise Div
in

(* Auto-conversion for type int: convert to uintptr_t *)
local
  fun runtime_test0 (a1:int,a2:int,a3:int) : int =
      prim("@runtime_test0", (a1,a2,a3))
in
  val x = 20 + runtime_test0 (1,2,3)
  val () = printNum x
end

(* Auto-conversion with boxed int64 arguments (when gc is enabled and unboxed otherwise)
 * (convert to uintptr_t) *)
local
  fun runtime_test0 (a1:int64,a2:int64,a3:int64) : int =
      prim("@runtime_test0", (a1,a2,a3))
in
  val x = 20 + runtime_test0 (1,2,3)
  val () = printNum x
  val a1 = 9223372036854775806 + (1:int64)
  val y = runtime_test0(a1,~1537228672809129301,~28)
  val () = if y = 4611686018427387764 then print "ok: i64\n" else print "err: i64\n"
end

(* Auto-conversion with boxed int32 arguments (when gc is enabled and unboxed otherwise)
 * (convert to uintptr_t) *)
local
  fun runtime_test0 (a1:int32,a2:int32,a3:int32) : int =
      prim("@runtime_test0", (a1,a2,a3))
in
  val x = 20 + runtime_test0 (1,2,3)
  val () = printNum x
  val a1 = 2000 + (1:int32)
  val y = runtime_test0(a1,100,10)
  val () = if y = 2351 then print "ok: i32\n" else print "err: i32\n"
end

(* Auto-conversion with boxed i64 result (when gc is enabled and unboxed otherwise)
 * (converts from uintptr_t) *)
local
  fun runtime_test0 (a1:int,a2:int,a3:int) : int64 =
      prim("@runtime_test0", (a1,a2,a3))
in
  val x = runtime_test0 (1,2,3)
  val () = if x = (22:int64) then print "ok: i64\n"
           else print "err: i64\n"
  val r = runtime_test0(4611686018427387903,55,10000)
  val () = if r = (4611686018427438068:int64) then print "ok: i64(2)\n"
           else print "err: i64(2)\n"
end

(* Auto-conversion with boxed i32 result (when gc is enabled and unboxed otherwise)
 * (converts from uintptr_t) *)
local
  fun runtime_test0 (a1:int,a2:int,a3:int) : int32 =
      prim("@runtime_test0", (a1,a2,a3))
in
  val x = runtime_test0 (1,2,3)
  val () = if x = (22:int32) then print "ok: i32 res\n"
           else print "err: i32 res\n"
  val r = runtime_test0(1000,100,10)
  val () = if r = (1350:int32) then print "ok: i32 res2\n"
           else print "err: i32 res2\n"
end

(* Auto-conversion with char arrays *)
local
  fun alloc0 (n:int) : chararray = prim("allocStringML", n)
  fun runtime_test_cstring0 (a:chararray,idx:int,c:char) : unit =
      prim("@runtime_test_cstring0", (a,idx,c))
  fun app f nil = ()
    | app f (x::xs) = (f x ; app f xs)
  fun sub (t:chararray,i:int) : char = prim ("__bytetable_sub", (t,i))
in
val a = alloc0 6
val l = [(0,#"h"), (1,#"e"), (2,#"l"), (3,#"l"), (4,#"o"), (5,chr 0)]
val () = app (fn (i,c) => runtime_test_cstring0(a,i,c)) l

val () = printCharArray a
val () = print "\n"
val () = if sub(a,1) = #"e" then print "ok: chararray\n" else print "err: chararray\n"
end

end
