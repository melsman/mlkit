(* Exercise register-only calls and odd/even stack argument counts.
 * Runtime values force frame-relative argument loads across the padding. *)
infix 6 +
infix 4 =
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun check b = prim("printStringML", if b then "OK\n" else "BAD\n"):unit
val seed:int = prim("@alignment_seed", ())
fun run__noinline (x:int) =
    (
     (prim("alignment0", ()):unit);
     check true;
     check ((prim("@alignment0", ()):int) = 1);
     check ((prim("alignment6", (x, x+11, x+22, x+33, x+44, x+55)):int) = x+55);
     check ((prim("@alignment6", (x, x+11, x+22, x+33, x+44, x+55)):int) = x+55);
     check ((prim("@alignment6", (x, x+11, x+22, x+33, x+44, x+55)):int64) = (66:int64));
     check ((prim("alignment7", (x, x+11, x+22, x+33, x+44, x+55, x+66)):int) = x+66);
     check ((prim("@alignment7", (x, x+11, x+22, x+33, x+44, x+55, x+66)):int) = x+66);
     check ((prim("@alignment7", (x, x+11, x+22, x+33, x+44, x+55, x+66)):int64) = (77:int64));
     check ((prim("alignment8", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77)):int) = x+77);
     check ((prim("@alignment8", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77)):int) = x+77);
     check ((prim("@alignment8", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77)):int64) = (88:int64));
     check ((prim("alignment9", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88)):int) = x+88);
     check ((prim("@alignment9", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88)):int) = x+88);
     check ((prim("@alignment9", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88)):int64) = (99:int64));
     check ((prim("alignment10", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88, x+99)):int) = x+99);
     check ((prim("@alignment10", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88, x+99)):int) = x+99);
     check ((prim("@alignment10", (x, x+11, x+22, x+33, x+44, x+55, x+66, x+77, x+88, x+99)):int64) = (110:int64));
     ())
val () = run__noinline seed
