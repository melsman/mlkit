structure ForkJoin :>
          sig include FORK_JOIN
(*              val parfor'__noinline : (int*int->unit) -> gcs -> int*int -> unit *)
          end = struct

fun alloc (n:int) (v:'a) : 'a array =
    prim ("word_table0", n)

structure T = Thread

fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i =
            if i >= hi then ()
            else (f i; loop (i+1))
    in loop lo
    end

fun parfor X (lo,hi) (f:int->unit) : unit =
    if lo >= hi then ()
    else let val m = lo+X
         in if m >= hi (* last bucket *)
            then for (lo,hi) f
            else T.spawn (fn () => for (lo,m) f)
                         (fn _ => parfor X (m,hi) f)
         end

fun pmap (f:'a->'b) (xs:'a list):'b list =
    let fun g nil k = k()
          | g (x::xs) k =
            T.spawn (fn() => f x) (fn t =>
            g xs (fn() => T.get t :: k()))
    in  g xs (fn () => nil)
    end


fun par (f,g) =
    T.spawn g (fn t => (f(), T.get t))
(*
fun par (f,g) =
    T.spawn f (fn t1 =>
    T.spawn g (fn t2 =>
    (T.get t1, T.get t2)))
*)

fun pair (f,g) (x,y) = par (fn () => f x, fn () => g y)

type gcs = int * int (* max parallelism, min sequential work *)

fun parfor'__noinline (doit:int*int->unit) (P,G) (lo,hi) : unit =
    if hi-lo <= 0 then () else
    let val n = hi-lo
        val m = Int.min(P+1, 1 + (n-1) div G) (* blocks *)
        val k = n div m                   (* block size *)
        fun loop (lo,hi) =
            if lo >= hi then ()
            else let val m = lo+k
                 in if m >= hi (* last block *)
                    then doit (lo,hi)
                    else T.spawn (fn () => doit (lo,m))
                                 (fn _ => loop (m,hi))
                 end
    in loop (lo,hi)
    end


fun parfor'__inline gcs (lo,hi) (f:int->unit) : unit =
    let fun doit (lo,hi) = for (lo,hi) f
    in parfor'__noinline doit gcs (lo,hi)
    end

val parfor' = parfor'__inline
end
