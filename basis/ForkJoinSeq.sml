structure ForkJoinSeq : FORK_JOIN = struct

fun par (f,g) = (f(),g())

fun parfor X (lo, hi) (f:int->unit) : unit =
    let fun loop lo =
            if lo >= hi then ()
            else (f lo; loop (lo+1))
    in loop lo
    end

fun pair (f,g) (x,y) = par (fn () => f x, fn () => g y)

fun pmap f xs = List.map f xs

fun alloc n v = Array.tabulate(n,fn _ => v)

type pspec = int * int (* max parallelism, min sequential work *)
fun parfor' (_,G) = parfor G

end

structure ForkJoin = ForkJoinSeq
