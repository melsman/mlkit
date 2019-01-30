infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int):unit = prim("printNum",n)
fun toInt (w : word32) : int = prim("__word32_to_int", w)
fun toIntX (w : word32) : int = prim("__word32_to_int_X", w)
fun cast_iw (a: int) : word = prim("id", a)
fun fromInt (i : int) : word32 = prim("__word_to_word32", cast_iw i)
fun rshiftsig_ (w : word32, k : word) : word32 =
    prim("__shift_right_signed_word32", (w,k))

fun printWord a = printNum (toIntX a)

fun ~>> (w:word,k:word) = rshiftsig_(w, k)


fun doit n =
    let val a = fromInt n
        val () = printWord a
        val b = ~>> (a, 0w1)
    in printWord b
    end

val () = doit ~64

val () = doit ~1

val maxInt = 2147483647
val minInt = ~2147483648

fun tst b =
    if b then print "Ok\n"
    else print "Err\n"

val r = ref false
val () = tst (if !r then true
              else toIntX 0wx7FFFFFFF = maxInt)

                                                                     (*
val test10c = checkrange (~513, 513)
    (fn i => i div 2 = Int32.fromLarge(toLargeIntX (~>> (i2w i, 0w1))));
*)
