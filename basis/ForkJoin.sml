structure ForkJoin :> FORK_JOIN = struct

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

fun parfor' (P,G) (lo,hi) (f:int->unit) : unit =
    let val n = hi-lo
        val m = Int.min(P+1, 1 + (n-1) div G) (* groups *)
        val k = n div m                   (* group size *)
        fun loop (lo,hi) =
            if lo >= hi then ()
            else let val m = lo+k
                 in if m >= hi (* last bucket *)
                    then for (lo,hi) f
                    else T.spawn (fn () => for (lo,m) f)
                                 (fn _ => loop (m,hi))
                 end
    in loop (lo,hi)
    end

end
