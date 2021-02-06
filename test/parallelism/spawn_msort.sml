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

fun myfpmap nil = nil
  | myfpmap (x::xs) =
    T.spawn (fn () => myf x)
            (fn t =>
                let val xs' = myfpmap xs
                in cp(T.get t) :: xs'
                end)
fun length xs =
    let fun len (nil,n) = n
          | len (x::xs,n) = len(xs,n+1)
    in len(xs,0)
    end

(*
fun split xs =
    let val h = length xs div 2
        fun tk (nil,n,acc) = (rev acc,nil)
          | tk (x::xs,n,acc) =
            if n < 1 then (rev acc,x::cp xs)
            else tk (xs,n-1,x::acc)
    in tk (xs,h,nil)
    end
*)

fun split vs =
    let fun sp (x::y::vs,xs,ys) = sp (vs,x::xs,y::ys)
          | sp ([x],xs,ys) = (x::xs,ys)
          | sp (nil,xs,ys) = (xs,ys)
    in sp (vs,nil,nil)
    end

(*
fun merge (xs,ys) =
    case (xs,ys) of
        (nil,_) => cp ys
      | (_,nil) => cp xs
      | (x::xs',y::ys') =>
        if x < y then x :: merge (xs',ys)
        else y :: merge (xs,ys')
*)

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

fun pmsort [] : int list = []
  | pmsort [x] = [x]
  | pmsort xs =
    let val (l,r) = split xs
    in T.spawn (fn() => pmsort l) (fn l =>
       T.spawn (fn() => pmsort r) (fn r =>
       merge (T.get l, T.get r)))
    end

fun pdmsort p [] : int list = []
  | pdmsort p [x] = [x]
  | pdmsort p xs =
    let val (l,r) = split xs
    in if p <= 1 then merge (pdmsort p l, pdmsort p r)
       else T.spawn (fn() => pdmsort (p div 2) l) (fn l =>
            T.spawn (fn() => pdmsort (p div 2) r) (fn r =>
            merge (T.get l, T.get r)))
    end

fun pdmsort2 p [] : int list = []
  | pdmsort2 p [x] = [x]
  | pdmsort2 p xs =
    let val (l,r) = split xs
    in if p <= 1 then merge (pdmsort2 p l, pdmsort2 p r)
       else T.spawn (fn() => pdmsort2 (p div 2) l) (fn l => let val r = pdmsort2 (p div 2) r
                                                            in merge (T.get l, r)
                                                            end)
    end

fun flatten nil = nil
  | flatten ((x,y)::ps) = x :: y :: flatten ps

fun splitd d xs =
    let fun spl d xss =
            if d < 1 then xss
            else let val ps = map split xss
                 in spl (d-1) (flatten ps)
                 end
    in spl d [xs]
    end

fun merges nil = nil
  | merges [x] = x
  | merges (x::xs) = merge(x,merges xs)

fun psort d xs =
    let val xss = splitd d xs
        val () = printNum (length xss)
        val xss = pmap smsort xss
    in merges xss
    end

fun ppsort xs =
    let val (xs1,xs2) = split xs
        val (xs11,xs12) = split xs1
        val (xs21,xs22) = split xs2
        val [xs11,xs12,xs21,xs22] = pmap smsort [xs11,xs12,xs21,xs22]
    in merge(merge(xs11,xs12),merge(xs21,xs22))
    end

fun rand (a,b) p = a + ((p+13) * 16807) mod (b-a)

fun randlist (a,b) n p =
    let fun gen (0,p,acc) = acc
          | gen (n,p,acc) =
            let val r = rand (a,b) p
            in gen (n-1,r,r::acc)
            end
    in gen (n,p,nil)
    end

fun spawns_pdmsort2 p = if p <= 1 then 0
                        else 1 + 2 * spawns_pdmsort2 (p div 2)

fun prList xs =
    (map (fn x => printNum x) xs
    ; ())

fun chk (x::(ys as y::xs)) =
    if x <= y then chk ys
    else "err\n"
  | chk _ = "ok\n"

val () =
    let val P = 8
        val () = print "generating input...\n"
        val xs = randlist (10,100) 500000 0
        val () = print "starting merge sort...\n"
        val res = pdmsort2 P xs
        val () = print "checking result...\n"
        val () = print (chk res);
    in (*prList xs;
       print "Sorted:\n";
       prList res;*)
       print "goodbye.\n";
       printNum (spawns_pdmsort2 P)
    end
end
