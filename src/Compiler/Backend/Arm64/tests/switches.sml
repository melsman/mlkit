(* Dense tables, holes, sparse trees, and constructor selectors. *)
infix 6 + -
infix 4 < ==
fun (x:int) == (y:int) = if x < y then false else if y < x then false else true
fun dense__noinline (x:int) =
  case x of ~4 => 11 | ~3 => 12 | ~1 => 14 | 0 => 15 | 1 => 16 | 3 => 18 | _ => 99
fun sparse__noinline (x:int) =
  case x of ~100000 => 1 | ~100 => 2 | 0 => 3 | 100 => 4 | 100000 => 5 | _ => 99
fun small__noinline (x:int) = case x of ~1 => 1 | 1 => 2 | _ => 99
datatype choice = A | B | C | D | E | F | G | H
fun enum__noinline x =
  case x of A => 1 | B => 2 | C => 3 | D => 4 | E => 5 | F => 6 | G => 7 | H => 8
datatype box = P of int | Q of int | R of int | S of int | T of int | U of int
fun boxed__noinline x =
  case x of P n => n+1 | Q n => n+2 | R n => n+3 | S n => n+4 | T n => n+5 | U n => n+6
fun check true = () | check false = raise Overflow
val () = check (dense__noinline (~5) == 99 andalso dense__noinline (~4) == 11 andalso
                dense__noinline (~3) == 12 andalso dense__noinline (~2) == 99 andalso
                dense__noinline (~1) == 14 andalso dense__noinline 0 == 15 andalso
                dense__noinline 1 == 16 andalso dense__noinline 2 == 99 andalso
                dense__noinline 3 == 18 andalso dense__noinline 4 == 99)
val () = check (sparse__noinline (~100001) == 99 andalso sparse__noinline (~100000) == 1 andalso
                sparse__noinline (~100) == 2 andalso sparse__noinline 0 == 3 andalso
                sparse__noinline 100 == 4 andalso sparse__noinline 100000 == 5 andalso
                sparse__noinline 100001 == 99 andalso sparse__noinline 1 == 99)
val () = check (small__noinline (~1) == 1 andalso small__noinline 1 == 2 andalso small__noinline 0 == 99)
val () = check (enum__noinline A == 1 andalso enum__noinline D == 4 andalso enum__noinline H == 8)
val () = check (boxed__noinline (P 10) == 11 andalso boxed__noinline (S 10) == 14 andalso boxed__noinline (U 10) == 16)
val () = prim("printStringML","OK\n")
