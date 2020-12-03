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

  (* From a type perspective, we include the type of the function in
   * the type of the thread, which means that regions in the effect of
   * the function will occur in the type of the thread. *)

  type 'a t = (unit->'a) * thread
  fun get ((_,t0): 'a t) : 'a = prim("thread_get", t0)
  fun spawn (f: unit->'a) (k: 'a t -> 'b) : 'b =
      let val rf = ref f
          val fp_f : foreignptr = prim("pointer", !rf)

          (* From a region inference perspective, coercing the type of
           * a function to a pointer type is very unsafe, as the
           * pointer type contains no information about which regions
           * need to be kept alive for the pointer value to be
           * valid. By including the type of the function in the type
           * of the thread value, however, we keep all necessary
           * regions alive. *)

          (* val () = prim("function_test", fp_f) *)
          val t0 : thread = prim("spawnone", fp_f)
          val t: 'a t = (f,t0)
          val res = k t
          val _ = if !(ref true) then get t else f()   (* make sure the thread has terminated before returning *)
                                                       (* and mimic that, from a type perspective, spawn has *)
                                                       (* the effect of calling f *)

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
end

(*
structure T :> THREAD = struct
  type 'a t = 'a
  fun spawn f k = k(f())
  fun get x = x
end
*)

fun test () : int =
    let val f = T.spawn (fn () => 8 + 3) (fn t => fn () => T.get t)
    in f ()
    end

fun iota n =
    let fun loop (n,acc) =
            if n < 0 then acc
            else loop (n-1,n::acc)
    in loop (n-1,nil)
    end

fun repl n v =
    let fun loop (n,acc) =
            if n < 0 then acc
            else loop (n-1,v::acc)
    in loop (n-1,nil)
    end

fun rev xs =
    let fun loop (nil,acc) = acc
          | loop (x::xs,acc) = loop (xs,x::acc)
    in loop (xs,nil)
    end

fun map f xs =
    let fun loop (nil,acc) = rev acc
          | loop (x::xs,acc) = loop(xs,f x::acc)
    in loop (xs,nil)
    end

fun foldl f acc nil = acc
  | foldl f acc (x::xs) = foldl f (f(x,acc)) xs

fun fib x = if x < 2 then 1 else fib(x-1)+fib(x-2)

fun pair (f,g) (x,y) =
    T.spawn (fn () => f x)
            (fn t1 =>
                T.spawn (fn () => g y)
                        (fn t2 => (T.get t1,T.get t2)))

fun pmap f xs =
    let fun loop nil = nil
          | loop (x::xs) =
            T.spawn (fn () => f x)
                    (fn t =>
                        let val xs' = loop xs
                        in T.get t :: xs'
                        end)
    in loop xs
    end

fun pmapMerge (f:'a->'c)
              (merge: 'c * 'b list->'b list)
              (xs:'a list) : 'b list =
  let fun g nil k = k()
        | g (x::xs) k =
          T.spawn (fn() => f x) (fn t =>
          g xs (fn() => merge(T.get t,k())))
  in  g xs (fn () => nil)
  end

fun sum xs = foldl (op +) 0 xs

fun f (a,b) =
    let val xs = map (fn x => (a+x) mod 10) (iota(b-a))
    in (sum xs, b-a)
    end

fun merge ((s,n),res) = (s,n)::res

fun myfun (xs:(int*int)list):(int*int)list =
    pmapMerge f merge xs

fun prList xs =
    (map (fn (x,_) => printNum x) xs
    ; ())

fun chk (x::(ys as y::xs)) =
    if x <= y then chk ys
    else "err\n"
  | chk _ = "ok\n"

val () =
    let val P = 4
        val () = print "generating input...\n"
        val is = repl P (10,1000000)
        val () = print "computations...\n"
        val rs = myfun is
        val v = foldl (fn ((s,n),a) => s div n + a) 0 rs
        val () = print "printing results...\n"
    in prList rs;
       print "goodbye.\n"
    end
end
