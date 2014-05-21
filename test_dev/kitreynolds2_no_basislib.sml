(*kitreynolds2.sml*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before


    type unit = unit
    type exn = exn
    type 'a ref = 'a ref

    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string 

fun implode (chars : char list) : string = prim ("implodeCharsML", "implodeCharsProfilingML", chars)
fun concat (ss : string list) : string = prim ("implodeStringML", "implodeStringProfilingML", ss)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", "concatStringProfilingML", (s, s'))
fun str (c : char) : string = implode [c]
fun size (s:string): int = prim ("sizeStringML", "sizeStringML", s)
fun chr (i : int) : char = prim ("chrCharML", "chrCharML", (i, Chr))
fun ord (c : char) : int = prim ("id", "id", c)
fun print (x:string):unit = prim("printStringML","printStringML",x)
      fun append [] ys = ys
	| append (x::xs) ys = x :: append xs ys
      fun xs @ ys = append xs ys

fun not true = false
    | not false = true
fun (f o g) x = f(g x)
      fun op = (x: ''a, y: ''a): bool = prim ("=", "=", (x, y))
fun eq_integer (x: int, y: int): bool = prim ("=", "=", (x, y)) 
fun eq_string  (x: string, y: string): bool = prim("=", "=", (x, y)) 

fun map f [] = []
    | map f (x::xs) = f x :: map f xs

fun revAcc [] ys = ys
        | revAcc (x::xs) ys = revAcc xs (x::ys)
fun rev xs = revAcc xs []


fun digit n = chr(ord #"0" + n)
fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)

fun int_to_string(n) = implode(digits(n,[]))

fun rev l = (* linear-time reversal of lists! *)
  let fun loop([], acc) = acc
        | loop(x::xs, acc) = loop(xs, x::acc)
  in
     loop(l, [])
  end

fun foldR f b [] = b
  | foldR f b (x::xs) = f x (foldR f b xs)

fun curry f x y = f(x,y)

datatype 'a Option = None | Some of 'a 


datatype 'a tree = 
    Lf 
  | Br of 'a * 'a tree * 'a tree

fun max(i:int, j) = if i>j then i else j
fun search p Lf = false
  | search p (Br(x,t1,t2)) = 
      if p x then true
      else search (fn y => y=x orelse p y) t1 orelse 
           search (fn y => y=x orelse p y) t2

fun mk_tree 0 = Lf
  | mk_tree n = let val t = mk_tree(n-1)
                in Br(n,t,t)
                end
val it = if search (fn _ => false) (mk_tree 20) then print "true\n"
	 else print "false\n"

