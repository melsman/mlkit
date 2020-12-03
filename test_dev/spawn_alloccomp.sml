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
  fun get ((_,t0): 'a t) : 'a = prim("thread_get", t0)
  fun spawn (f: unit->'a) (k: 'a t -> 'b) : 'b =
      let val rf = ref f
          val fp_f : foreignptr = prim("pointer", !rf) (* very unsafe *)
          (*val () = prim("function_test", fp_f) *)
          val t0 : thread = prim("spawnone", fp_f)
          val t: 'a t = (f,t0)
          val res = k t
(*          val () = if false then (f();()) else () *)
          val _ = if true then get t else f()   (* make sure the thread has terminated before returning *)
                                                (* and mimic that, from a type perspective, spawn has *)
                                                (* the effect of calling f *)
          (* Notice that it is not safe to call `thread_free t0` here
           * as t0 may be live through later calls to `get t` *)

          (* What is needed is for the ThreadInfo structs to be region allocated and to
           * add finalisers (thread_free) to objects in these regions. *)
(*          val () = prim("thread_free", t0) *)
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

fun calc n = foldl (op +) 0 (map fib (iota n))

fun myf n = map (fn x => x+1) (iota n)

fun cp nil = nil
  | cp (x::xs) = x :: cp xs

fun length xs =
    let fun len (nil,n) = n
          | len (x::xs,n) = len(xs,n+1)
    in len(xs,0)
    end

fun split vs =
    let fun sp (x::y::vs,xs,ys) = sp (vs,x::xs,y::ys)
          | sp ([x],xs,ys) = (x::xs,ys)
          | sp (nil,xs,ys) = (xs,ys)
    in sp (vs,nil,nil)
    end

fun nil @ ys = ys
  | (x::xs) @ ys = x :: (xs@ys)

fun merge (a,b) =
    let fun m (xs,ys,acc) =
            case (xs,ys) of
                (nil,_) => rev ys @ acc
              | (_,nil) => rev xs @ acc
              | (x::xs',y::ys') =>
                if x < y then m (xs',ys, x::acc)
                else m (xs,ys',y::acc)
    in rev(m(a,b,nil))
    end

fun smsort nil = nil
  | smsort [x] = [x]
  | smsort xs =
    let val (l,r) = split xs
    in merge (smsort l, smsort r)
    end

fun flatten nil = nil
  | flatten ((x,y)::ps) = x :: y :: flatten ps

fun rand (a,b) p = a + ((p+13) * 16807) mod (b-a)

fun randlist (a,b) n p =
    let fun gen (0,p,acc) = acc
          | gen (n,p,acc) =
            let val r = rand (a,b) p
            in gen (n-1,r,r::acc)
            end
    in gen (n,p,nil)
    end

fun comp (n:int) : int =
    let val xs = randlist (1,20000) n 0
        val s = foldl (op +) 0 xs
    in s div n
    end

fun compk k n =
    if k <= 1 then comp n
    else comp n + compk (k-1) n

fun prList xs =
    (map (fn x => printNum x) xs
    ; ())

fun chk (x::(ys as y::xs)) =
    if x <= y then chk ys
    else "err\n"
  | chk _ = "ok\n"

val () =
    let val P = 8
        val N = 200000
        val () = print "generating input...\n"
        val is = repl P N
        val () = print "computations...\n"
        val rs = pmap (compk 100) is
        val () = print "printing results...\n"
    in prList rs;
       print "goodbye.\n"
    end
end
