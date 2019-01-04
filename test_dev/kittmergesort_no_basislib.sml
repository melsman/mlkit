(*kittmergesort.sml*)

(* This is tmergesort taken from Paulson's book , page 99 *)

(* The merge function has been modified slightly, to
   traverse and rebuild both arguments fully, even when
   the one argument is empty. This ensures that both
   recursive calls of tmergesort in itself can put their
   results in regions local to the body of tmergesort.

   One can show that the maximum number of live list elements
   is 3n, where n is the number of elements to be sorted.
   For n=50000 this should give an approximate memory usage of
   3 * 50.000 list elements * 5 words/list element * 4 bytes/word=
   3Mb. The actual memory usage (run24d) is 4.5Mb. The remaining
   1.5Mb is probably largely due to the fact that merge puts
   stuff on the stack (as it is not tail recursive).

*)



infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before


    type unit = unit
    type exn = exn
    type 'a ref = 'a ref
    type real = real

    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string

fun (a:real) / (b:real) : real = prim ("divFloat", (a,b))
fun implode (chars : char list) : string = prim ("implodeCharsML", chars)
fun concat (ss : string list) : string = prim ("implodeStringML", ss)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun str (c : char) : string = implode [c]
fun size (s : string) : int = prim ("__bytetable_size", s)
fun chr (i : int) : char = prim ("chrCharML", (i, Chr))
fun ord (c : char) : int = prim ("id", c)
fun print (x:string):unit = prim("printStringML", x)

fun append [] ys = ys
  | append (x::xs) ys = x :: append xs ys
fun xs @ ys = append xs ys

fun real (x : int) : real = prim ("realInt", x)
fun floor (x : real) : int = prim ("floorFloat", x)    (* may raise Overflow *)

fun not true = false
    | not false = true
fun (f o g) x = f(g x)
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

exception Take and Drop

fun take(0, _ ) = []
  | take(n, x::xs) = x::take(n-1, xs)
  | take(n, []) = raise Take

fun drop(0, l) = l
  | drop(n, x::xs) = drop(n-1, xs)
  | drop(n, []) = raise Drop

fun digit n = chr(ord #"0" + n)

fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)
fun int_to_string(n) = implode(digits(n,[#"\n"]))

fun snd(x,y) = y

val a = 167
val m = 2147
fun nextrand(seed) =
   let val t = a*seed
   in t - (m*(t div m))
   end

fun randlist(n,seed,tail)=
    if n=0 then (seed,tail)
    else randlist(n-1, nextrand seed, seed::tail)


fun length [] = 0
  | length (_::xs) = 1+length xs

fun merge([], ys) = (ys:int list)@[]
  | merge(xs, []) = xs @[]
  | merge(l as x::xs, r as y:: ys) =
      if x<= y then x::merge(xs, r)
      else y:: merge(l, ys)

fun tmergesort [] = []
  | tmergesort [x] = [x]
  | tmergesort xs =
      let val k = length xs div 2
      in merge(tmergesort(take(k, xs)),
               tmergesort(drop(k, xs)))
      end

val result =
let
  val n = 25000
  val xs = snd(randlist(n,1,[]))
  val _ = print "\n List generated\n"
  fun report msg = print(msg^"\n")
in
  report "Doing tmergesort...";
  tmergesort xs;
  report("Sorted " ^ int_to_string n ^ " numbers\n")
end
