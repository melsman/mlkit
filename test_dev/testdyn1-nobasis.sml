(*testdyn1-nobasis.sml*)

(* ------------------------------------------------------------------- *)
(*   testdyn1, 08/02/1995 19:17, Martin                                *)
(*   Dynamic test of primitives... except for input/output             *)
(* ------------------------------------------------------------------- *)

(*
  MEMO : 'sin', 'cos', 'arctan', 'ln' and 'exp' are not checked yet.

*)

(* structure General *)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

exception Fail of string
exception Subscript
exception Span
exception Size
exception Domain
exception Chr

datatype order = LESS | EQUAL | GREATER

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun (f o g) x = f(g x)
fun a before () = a
fun ignore (a) = ()

fun exnName (e: exn) : string = prim("exnNameML", e)   (* exomorphic by copying *)
fun exnMessage (e: exn) : string = exnName e

datatype 'a option = NONE | SOME of 'a
exception Option
fun getOpt (NONE, a) = a
  | getOpt (SOME a, b) = a
fun isSome NONE = false
  | isSome _ = true
fun valOf (SOME a) = a
  | valOf _ = raise Option

fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

fun not true = false
  | not false = true

fun a <> b = not (a = b)

fun print (s:string) : unit = prim("printStringML", s)
fun implode (chars : char list) : string = prim ("implodeCharsML", chars)
fun concat (ss : string list) : string = prim ("implodeStringML", ss)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun str (c : char) : string = implode [c]
fun size (s:string): int = prim ("__bytetable_size", s)
fun ord (c : char) : int = prim ("id", c)
fun chr (i:int) : char = if i>=0 andalso i<256 then prim ("id", i)
			else raise Chr

local
   fun sub_unsafe (s:string,i:int) : char = prim ("__bytetable_sub", (s,i))
in fun explode s =
     let fun h (j, res) = if j<0 then res
			  else h (j-1, sub_unsafe (s, j) :: res)
     in h (size s - 1, nil)
     end
end

(* structure List *)

  exception Empty

  fun null [] = true
    | null _  = false

  fun hd []      = raise Empty
    | hd (x::xr) = x

  fun tl []      = raise Empty
    | tl (x::xr) = xr

  fun last []      = raise Empty
    | last [x]     = x
    | last (x::xr) = last xr

  fun length xs =
    let fun acc []      k = k
	    | acc (x::xr) k = acc xr (k+1)
    in acc xs 0
    end

  local
    fun revAcc [] ys = ys
	| revAcc (x::xs) ys = revAcc xs (x::ys)
  in
    fun rev xs = revAcc xs []
  end

  fun nil @ ys     = ys
    | (x::xs) @ ys = x :: (xs @ ys)

  fun app f []      = ()
    | app f (x::xr) = (f x; app f xr)

  fun map f [] = []
    | map f (x::xs) = f x :: map f xs

  fun foldr f e []      = e
    | foldr f e (x::xr) = f(x, foldr f e xr)

  fun foldl f e []      = e
    | foldl f e (x::xr) = foldl f (f(x, e)) xr

  (* structure Real *)

  fun real (x : int) : real = prim ("realInt", x)
  fun floor (x : real) : int = prim ("floorFloat", x)    (* may raise Overflow *)
  fun ceil (x : real) : int = prim ("ceilFloat", x)      (* may raise Overflow *)
  fun trunc (x : real) : int = prim ("truncFloat", x)    (* may raise Overflow *)

  fun (x: real) / (y: real): real = prim ("divFloat", (x, y))

  (* structure Math *)
  fun sqrt (r : real) : real = prim ("sqrtFloat", r)

  (* The tests *)

  infix ==
  val epsilon = 0.000666
  fun r1 == r2 = abs (r1 - r2) < epsilon (*no perfect world*)

  fun digit n = chr(ord #"0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)

  fun int_to_string(n) = if n >= 0 then implode(digits(n,[]))
			 else "~" ^ int_to_string(~n)

  fun error b s = print ((if b then "Ok - " else "Error - ") ^ s ^ "...\n")

  (* testing stuff *)
  val _ =
    let
      val _ = print "Testing list operations:\n\
      \  [rev, @, map]...\n"
    in
      error (rev [3,34,2,23] = [23,2,34,3]) "rev";
      error (map (fn a:int => 2 * a) [3,34,2] = [6,68,4]) "map";
      error ([34,56] @ [12,67] = [34,56,12,67]) "@"
    end

  val _  =
    let
      val _ = print "Testing string operations:\n\
       \  [implode, explode, chr, ord, size]...\n"
      fun hds [] = #"-"
	| hds (x::_) = x
    in
      error (int_to_string 232 = "232") "int_to_string";
      error (implode [#"h", #"e", #"l", #"l", #" "] = "hell ") "implode";
      error (hds (explode "hello") = #"h") "explode";
      error (chr 66 = #"B") "chr";
      error (ord #"B" = 66) "ord";
      error (((chr 1000) handle Chr => #"h") = #"h") "Chr";
      error (((chr 1000) handle Div => #"h"
                              | Chr => #"k") = #"k") "Chr2";
      error (size "hello I'm 19 long.." = 19) "size"
    end

  val _ =
    let
      val _ = print "Testing ref [ref, :=, !]...\n"
      val a = ref "hello"
      val g = ref 45
    in
      error (!a = "hello") "!1";
      error ((a := "hej") = ()) ":=1";
      error (!a = "hej") "!2";
      error ((g := !g + 1) = ()) ":=2";
      error (!g = 46) "!3"
    end

  val _ =
    let
      val _ = print "Testing polymorphic equality...\n"
      val a = [(34,"hejsa"), (4, "bw")]
      val b = [[3,23], [~34,23]]
      val c = (56, ref "hello")
      val d = ref "hej"
      datatype k = A of int * string | B | C of k * k
      val k1 = C (A(5,"hello"), B)
      val k2 = C (A(5,"hello2"), B)
      val k3 = C (A(5,"hello2"), B)
    in
      error (a = [(34,"hejsa"), (4, "bw")]) "equal";
      error ((a = [(34,"hejsa"), (4, "cw")]) = false) "equal2";
      error (b = [[3,23], [~34,23]]) "equal3";
      error ((b = [[3,23], [~34,21]]) = false) "equal4";
      error ((c = (56, ref "hello")) = false) "equal5 (ref1)";
      error ((34,d) = (34,d)) "equal5 (ref2)";
      error (k1 <> k2) "equal6 (dat k)";
      error (k2 = k3) "equal7 (dat k)"
    end

  val _ =
    let
      val _ = print "Testing arithmetic integer operations:\n\
       \  [~, abs, floor, +, -, *, div, mod, <, >, <=, >=] ...\n"
      fun checkdivmod (i, d) =
	let
	  val (r, q) = (i mod d, i div d)
	  val gt_zero = fn a => a > 0
	in
	  error (gt_zero r = gt_zero d andalso d * q + r = i)
	  ("intdivmod - " ^ int_to_string i ^ " mod " ^ int_to_string d ^
	   " = " ^ int_to_string r ^ ", " ^  int_to_string i ^ " div "
	   ^ int_to_string d ^ " = " ^ int_to_string q)
	end
    in
      error (~ 5 = ~5) "~1";
      error (~ (~2) = 2) "~2";
      error (abs 5 = 5) "abs1";
      error (abs (~23) = 23) "abs2";
      error (floor (23.23) = 23) "floor1";
      error (floor (~23.23) = ~24) "floor2";
      error (((floor (23.0E23)) handle Overflow => 4) = 4) "floor3";
      error (23 + 12 = 35 andalso ~4 + 5 = 1) "+";
      error (34 - 12 = 22 andalso ~23 - 15 = ~38) "-";
      error (12 * 3 = 36 andalso ~23 * 2 = ~46) "*";
      map checkdivmod [(2,3), (34, ~3), (5, ~2), (~7, 3)];
      error (((3 div 0) handle Div => 60) = 60) "Div1";
      error (((3 mod 0) handle Div => 45) = 45) "Div2";
      error ((23 < 40) = true) "<1";
      error ((54 < 40) = false) "<2";
      error ((40 < 40) = false) "<3";
      error ((23 > 40) = false) ">1";
      error ((54 > 40) = true) ">2";
      error ((40 > 40) = false) ">3";
      error ((23 <= 40) = true) "<=1";
      error ((54 <= 40) = false) "<=2";
      error ((40 <= 40) = true) "<=3";
      error ((23 >= 40) = false) ">=1";
      error ((54 >= 40) = true) ">=2";
      error ((40 >= 40) = true) ">=3"
    end

  val _ =
    let
      val _ = print "Testing arithmetic real operations:\n\
       \   [+, -, *, /, ~, abs, real, sqrt, <, >, <=, >=] ...\n"
    in
      error (4.0 + 3.0 == 7.0) "+";
      error (4.0 - 1.0 == 3.0) "-";
      error (4.0 * 3.0 == 12.0) "*";
      error (9.0 / 2.0 == 4.5) "/";
      error (~ 5.3 == ~5.3) "~1";
      error (~ (~2.23) == 2.23) "~2";
      error (abs 5.23 == 5.23) "abs1";
      error (abs (~23.12) == 23.12) "abs2";
      error (real 5 == 5.0) "real1";
      error (real ~5 == ~5.0) "real2";
      error (sqrt 0.0 == 0.0) "sqrt1";
      error (sqrt 2.0 > 1.4) "sqrt2";
      error (sqrt 2.0 < 1.5) "sqrt3";

      error ((23.34 < 40.23) = true) "<1";
      error ((54.12 < 40.45) = false) "<2";
      error ((40.12 < 40.12) = false) "<3";
      error ((23.34 > 40.12) = false) ">1";
      error ((54.45 > 40.23) = true) ">2";
      error ((40.23 > 40.23) = false) ">3";
      error ((23.12 <= 40.34) = true) "<=1";
      error ((54.23 <= 40.23) = false) "<=2";
      error ((40.23 <= 40.23) = true) "<=3";
      error ((23.75 >= 40.75) = false) ">=1";
      error ((54.57 >= 40.57) = true) ">=2";
      error ((40.23 >= 40.23) = true) ">=3"
    end

  val _ =
    let
      val _ = print "Testing composition o:\n"
      fun f x = 3 + x
      fun g y = (y, 2*y)
    in
      error ((g o f) 7 = (10,20)) "o"
    end

  val _ =
    let
      val _ = print "Testing generative exceptions:\n"
      fun g a =
	let
	  fun f x =
	    let
	      exception E
	    in
	      if x < 1 then raise E
	      else ((f (x-1)) handle E => 7) (* should not handle this.. *)
	    end
	in
	  (f a) handle _ => a
	end (* a *)
    in
      error (g 10 = 10) "exn - generative"
    end

  fun etst b s = if b then () else print ("Error - " ^ s ^ "...\n");

  val _ = etst ("\u0041\u000a\\u0041\n" = "A\n\092" ^ "u0041\010")
            "backslash u does not work or somepin";

  val _ = etst (map ord [#"a", #"A", #" ", chr 42, #"\117"] =
		[97, 65, 32, 42, 117]) "char problem, maybe #"

  val _ = print "End of test.\n"
