infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun print (s:string) : unit = prim("printStringML",s)
val r : int ref = ref 45
fun incr () : int =
    let val x = !r + 1
    in r := x ; x
    end

(* example demonstrating higher-order partial inlining *)

fun map f nil = nil
  | map f (x::xs) = f x :: map f xs

fun nil @ ys = ys
  | (x::xs) @ ys = x :: (xs @ ys)

local

fun length nil = 0
  | length (_ :: xs) = 1 + length xs

fun split xs =
    let fun td n xs =
            if n <= 0 then ([],xs)
            else case xs of
                     nil => ([],[])
                   | x::xs =>
                     let val (xs,ys) = td (n-1) xs
                     in (x::xs,ys)
                     end
    in td (length xs div 2) xs
    end

fun last a xs =
    case xs of
        nil => a
      | [x] => x
      | _ :: xs => last a xs

fun par2 f (a,b) = (f a, f b)

in

fun scanmain__noinline distribute seqscan a xs =
    let val (b1,b2) = par2 (seqscan a) (split xs)   (* simulate parallelism *)
        val d = last a b1
        val b2 = distribute d b2
    in b1 @ b2
    end

fun scan (f:'a*'a->'a) (ne:'a) (xs:'a list) : 'a list =
    let fun seq_scan a xs =
            case xs of
                nil => nil
              | x::xs => let val y = f(a,x)
                         in y::seq_scan y xs
                         end
        fun distribute a xs = map (fn x => f (a,x)) xs
    in scanmain__noinline distribute seq_scan ne xs
    end
end
