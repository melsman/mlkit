structure ForkJoin :> FORK_JOIN = struct

fun alloc (n:int) (v:'a) : 'a array =
    prim ("word_table0", n)

local

fun alloc_unsafe (n:int) : 'a array =
    prim ("word_table0", n)

structure T = Thread

fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i =
            if i >= hi then ()
            else (f i; loop (i+1))
    in loop lo
    end
in
fun parfor `e (X:int) (lo:int,hi:int) (f: int #e -> unit) : unit while noput e =
    if lo >= hi then ()
    else let val m = lo+X
         in if m >= hi (* last bucket *)
            then for (lo,hi) f
            else T.spawn (fn () => for (lo,m) f)
                         (fn _ => parfor X (m,hi) f)
         end

fun pmap `[e] (f:'a #e ->'b) : ('a list -> 'b list) while noput e =
    fn (xs:'a list) =>
       let val v = Vector.fromList xs
           val sz = Vector.length v
           val a : 'b Array.array = alloc_unsafe sz
       in parfor 1000 (0,sz) (fn i => Array.update(a,i,f(Vector.sub(v,i))))
        ; Array.foldr (op ::) nil a
       end

fun par `[e1 e2] (f: unit #e1 -> 'a, g: unit #e2 -> 'b) : 'a * 'b while e1 ## e2 =
    let val a1 : 'a array = alloc_unsafe 1
        val b1 : 'b array = alloc_unsafe 1
    in T.spawn (fn () => Array.update(b1,0,g())) (fn _ => (Array.update(a1,0,f())))
     ; (Array.sub(a1,0), Array.sub(b1,0))
    end

fun pair `[e1 e2] (f: 'a #e1 -> 'b, g: 'c #e2 -> 'd) (x:'a,y:'c) : 'b * 'd while e1 ## e2 =
    par (fn () => f x, fn () => g y)

type gcs = int * int (* max parallelism, min sequential work *)

val rec parfor'__noinline `[e] : (int*int #e-> unit) -> gcs -> int*int -> unit while noput e =
  fn doit => fn (P,G) => fn (lo,hi) =>
    if hi-lo <= 0 then () else
    let val n = hi-lo
        val m = Int.min(P+1, 1 + (n-1) div G) (* blocks *)
        val k = n div m                   (* block size *)
        val v = Vector.tabulate(m, fn i =>
                                      let val first = i*k
                                          val next = first+k
                                      in (first,
                                          if next > hi then hi else next)
                                      end)
        fun loop n =
            if n >= m-1 then doit (Vector.sub(v,n))
            else T.spawn (fn () => doit (Vector.sub(v,n)))
                         (fn _ => loop (n+1))
    in loop 0
    end

val rec parfor' `[e] : gcs -> int*int -> (int #e -> unit) -> unit while noput e =
 fn gcs => fn (lo,hi) => fn f =>
    let fun doit (lo,hi) = for (lo,hi) f
    in parfor'__noinline doit gcs (lo,hi)
    end

val parfor' = parfor'

end
end
