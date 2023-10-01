(* Invalid constraint *)

infix ^
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

fun f `[r1 r2] (a: string`r1) (b: string`r2) : string while r1 # r2 =
    (a ^ b) (*: string while r2 # r1*)

val a = "hi"`r0string
val b = "there"`r0string

val c = f a b
