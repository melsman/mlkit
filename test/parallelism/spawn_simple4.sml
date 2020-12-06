signature THREAD = sig
  type 'a t
  val spawn : (unit->'a) -> ('a t->'b) -> 'b
  val get   : 'a t -> 'a
end

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

local
fun exnName (e: exn) : string = prim("exnNameML", e)   (* exomorphic by copying *)

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun printNum (i:int) : unit = prim("printNum", i)

in
structure T :> THREAD = struct
  type thread = foreignptr
  type 'a t = (unit->'a) * thread
  fun get ((_,t0:thread):'a t) : 'a = prim("thread_get", t0)
  fun spawn (f: unit->'a) (k: 'a t -> 'b) : 'b =
      let val rf = ref f
          val fp_f : foreignptr = prim("pointer", !rf) (* very unsafe *)
          (*val () = prim("function_test", fp_f) *)
          val t0 : thread = prim("spawnone", fp_f)
          val t = (f,t0)
          val res = k t
      in get t; res
      end
end
(*
structure T :> THREAD = struct
  type 'a t = 'a
  fun spawn f k = k(f())
  fun get x = x
end
*)
fun fib x = if x < 2 then 1 else fib(x-1)+fib(x-2)

fun pair (f,g) (x,y) =
    T.spawn (fn () => f x)
            (fn t1 =>
                T.spawn (fn () => g y)
                        (fn t2 => (T.get t1,T.get t2)))

val () = print "starting...\n"
val (x,(y,z)) = pair (fib,pair(fib,fib)) (21,(22,23))
val () = printNum x
val () = printNum y
val () = printNum z
val () = print "goodbye.\n"

end
