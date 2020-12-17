structure ForkJoin :> FORK_JOIN = struct
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

fun pair (f,g) (x,y) = par (fn () => f x, fn () => g y)

fun alloc n v = Array.tabulate(n,fn _ => v)

end
