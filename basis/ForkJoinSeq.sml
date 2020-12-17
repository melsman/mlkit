structure ForkJoinSeq : FORK_JOIN = struct

fun par (f,g) = (f(),g())

fun parfor X (lo, hi) (f:int->unit) : unit =
    if lo >= hi then () else (f lo; parfor X (lo+1, hi) f)

fun pair (f,g) (x,y) = par (fn () => f x, fn () => g y)

fun pmap f xs = List.map f xs

fun alloc n v = Array.tabulate(n,fn _ => v)

end
