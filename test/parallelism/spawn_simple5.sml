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
  type 'a t = ((unit->'a) * thread) ref
  fun get__noinline ((ref (f,t0)): 'a t) : 'a = prim("thread_get", t0)
  fun spawn__noinline (f: unit->'a) (k: 'a t -> 'b) : 'b =
      let val rf : (unit -> 'a) ref = ref f
          val fp_f : foreignptr = prim("pointer", !rf)

          (* From a region inference perspective, coercing the type of
           * a function to a pointer type is very unsafe, as the
           * pointer type contains no information about which regions
           * need to be kept alive for the pointer value to be
           * valid. By including the type of the function in the type
           * of the thread value, however, we keep all necessary
           * regions alive.
           *
           * Moreover, we encapsulate the function in a ref to avoid
           * that the closure is inlined into the "pointer" prim
           * argument. If it is inlined, the closure will be allocated
           * in a local stack-allocated region inside the "pointer"
           * prim value, which cause the program to segfault.. *)

          (* val () = prim("function_test", fp_f) *)
          val t0 : thread = prim("spawnone", fp_f)
          val t: 'a t = ref (f,t0)
          val res = k t
          val _ = if !(ref true)          (* make sure the thread has terminated before returning *)
                  then get__noinline t    (* and mimic that, from a type perspective, spawn has *)
                  else !rf()              (* the effect of calling f *)

          (* Notice that it is not safe to call `thread_free t0` here
           * as t0 may be live through later calls to `get t`.
           *
           * What is needed is for the ThreadInfo structs to be region allocated and to
           * add finalisers (thread_free) to objects in these regions. I wonder whether
           * we can detach the thread already in the thread_get function (the mutex and
           * the ThreadInfo struct must be kept live of course.
           *)

          (* val () = prim("thread_free", t0) *)
      in res
      end

  fun spawn x = spawn__noinline x
  fun get x = get__noinline x
end
(*
structure T :> THREAD = struct
  type 'a t = 'a
  fun spawn f k = k(f())
  fun get x = x
end
*)

fun iota n =
    let fun loop (n,acc) =
            if n < 0 then acc
            else loop (n-1,n::acc)
    in loop (n-1,nil)
    end

fun map f nil = nil
  | map f (x::xs) = f x :: map f xs

fun foldl f acc nil = acc
  | foldl f acc (x::xs) = foldl f (f(x,acc)) xs

fun fib x = if x < 2 then 1 else fib(x-1)+fib(x-2)

fun pair (f,g) (x,y) =
    T.spawn (fn () => f x)
            (fn t1 =>
                T.spawn (fn () => g y)
                        (fn t2 => (T.get t1,T.get t2)))

fun calc n = foldl (op +) 0 (map fib (iota n))

local
val () = print "starting...\n"
val (x,(y,z)) = pair (calc,pair(fib,calc)) (24,(23,20))
val () = printNum x
val () = printNum y
val () = printNum z
val () = print "goodbye.\n"
in
end
end
