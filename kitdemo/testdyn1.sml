(*testdyn1.sml*)

(* ------------------------------------------------------------------- *)
(*   testdyn1, 08/02/1995 19:17, Martin                                *)
(*   Dynamic test of primitives... except for input/output             *)
(* ------------------------------------------------------------------- *)

(*
  MEMO : 'sin', 'cos', 'arctan', 'ln' and 'exp' are not checked yet. 

*)

local 
(* ==================================================
 * Prelude for the ML Kit with Regions   ME 17.12.96
 * Changed CCALL primitive (31)          NH 10.02.97
 * ================================================== *)

infix  3 := o
infix  4 = <> < > <= >= 
infix  5 @
infixr 5 ::
infix  6 + - ^
infix  7 div mod / * 

exception Ord and Chr and Div and Mod and Quot and Floor and Sqrt and
      Exp and Ln and Io of string


(* ============================
 * BUILT-IN PRIMITIVES
 * ============================ *)

fun op = (x: ''a, y: ''a): bool =           prim(0, (x, y))
fun (x: 'a ref) := (y: 'a): unit =          prim(17, (x, y)) 
fun !(x: 'a ref): 'a =                      prim(18, x) 


(* ======================
 * IMPORTED PRIMITIVES
 * ====================== *)

fun ord (c:string): int =                   prim(31, ("ordString", "ordString", c, Ord))
fun chr (i:int): string =                   prim(31, ("chrString", "chrStringProfiling", i, Chr))
fun size (s:string): int =                  prim(31, ("sizeString", "sizeString", s))
fun explode (str: string): string list =    prim(31, ("explodeString", "explodeStringProfiling", str))
fun implode (strs: string list): string =   prim(31, ("implodeString", "implodeStringProfiling", strs))
fun op ^ (s1:string, s2:string): string =   prim(31, ("concatString", "concatStringProfiling", s1, s2)) 
fun (x: int) div (y: int): int =            prim(31, ("divInt", "divInt", x, y, Div))
fun (x: int) mod (y: int): int =            prim(31, ("modInt", "modInt", x, y, Mod)) 
fun real(x: int): real =                    prim(31, ("realInt", "realInt", x))
fun floor(x:real):int =                     prim(31, ("floorFloat", "floorFloat", x, Floor))
fun (x: real) / (y: real): real =           prim(31, ("divFloat", "divFloat", x, y, Quot))
fun sqrt(x: real): real =                   prim(31, ("sqrtFloat", "sqrtFloat", x, Sqrt)) 
fun exp(x: real): real =                    prim(31, ("expFloat", "expFloat", x, Exp)) 
fun ln(x: real): real =                     prim(31, ("lnFloat", "lnFloat", x, Ln)) 
fun sin(x: real): real =                    prim(31, ("sinFloat", "sinFloat", x)) 
fun cos(x: real): real =                    prim(31, ("cosFloat", "cosFloat", x)) 
fun arctan(x: real): real =                 prim(31, ("arctanFloat", "arctanFloat", x)) 


abstype instream = INS of int                 (* Streams ala the old Def. *)
and outstream = OUTS of int
with
  val std_in : instream =                     INS(prim(31, ("stdInStream", "stdInStream", 0)))
  val std_out : outstream =                   OUTS(prim(31, ("stdOutStream", "stdOutStream", 0)))

  exception CANNOT_OPEN
  fun open_in(f: string): instream =          INS(prim(31, ("openInStream", "openInStream", f, CANNOT_OPEN)))
                                              handle CANNOT_OPEN => raise Io("Cannot open " ^ f)
  fun open_out(f: string): outstream =        OUTS(prim(31, ("openOutStream", "openOutStream", f, CANNOT_OPEN)))
                                              handle CANNOT_OPEN => raise Io("Cannot open " ^ f)
  fun input(INS i, n: int): string =          prim(31, ("inputStream", "inputStreamProfiling", i, n))
  fun lookahead(INS i): string =              prim(31, ("lookaheadStream", "lookaheadStreamProfiling", i)) 
  fun close_in(INS i): unit =                 prim(31, ("closeStream", "closeStream", i))
  fun end_of_stream(INS i): bool =            prim(31, ("endOfStream", "endOfStream", i))
  val output_exval =                          Io "Output stream is closed"
  fun output(OUTS i, str: string): unit =     prim(31, ("outputStream", "outputStream", i, str, output_exval))
  fun close_out(OUTS i): unit =               prim(31, ("closeStream", "closeStream", i))
  fun flush_out(OUTS i): unit =               prim(31, ("flushStream", "flushStream", i))    
end


(* ======================
 * DERIVED PRIMITIVES
 * ====================== *)

val not = fn true => false | false => true
fun (f o g) x = f(g x)
fun op <> (a,b) = not(a=b)
fun nil @ M = M
  | (x :: L) @ M = x :: (L @ M)
fun map f nil = nil
  | map f (x :: L) = f x :: map f L
fun rev l =
  let fun rev_rec(p as ([], acc)) = p
        | rev_rec(x::xs, acc) = rev_rec(xs, x::acc)
  in #2 (rev_rec(l,nil))
  end
fun length [] = 0
  | length (x::xs) = 1 + length xs
fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)

in

  fun print s = output (std_out, s)
  fun digit n = chr(ord "0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)

  fun int_to_string(n) = if n >= 0 then implode(digits(n,[]))
			 else "~" ^ int_to_string(~n)

  fun error b s = if b then print ("Ok - " ^ s ^ "...\n") else print ("Error - " ^ s ^ "...\n")

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
      fun hds [] = "-"
	| hds (x::_) = x
    in
      error (int_to_string 232 = "232") "int_to_string";
      error (implode ["hello", " ", "world\n"] = "hello world\n") "implode";
      error (hds (explode "hello") = "h") "explode";
      error (chr 66 = "B") "chr";
      error (ord "B" = 66) "ord";
      error (((chr 1000) handle Chr => "hej") = "hej") "Chr";
      error (((chr 1000) handle Div => "h"
                              | Chr => "kurt") = "kurt") "Chr2";
      error (((ord "") handle Ord => 5) = 5) "Ord";
      error (((ord "") handle Div => 34
                              | Ord => 5) = 5) "Ord2";
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
      val b = [[3.23], [~34.23]]
      val c = (56, ref "hello")
      val d = ref "hej"
      datatype k = A of int * string | B | C of k * k
      val k1 = C (A(5,"hello"), B)
      val k2 = C (A(5,"hello2"), B)
      val k3 = C (A(5,"hello2"), B)
    in
      error (a = [(34,"hejsa"), (4, "bw")]) "equal";
      error ((a = [(34,"hejsa"), (4, "cw")]) = false) "equal2";
      error (b = [[3.23], [~34.23]]) "equal3";
      error ((b = [[3.23], [~34.21]]) = false) "equal4";
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
      error ((exp 640000000000000.0 handle Exp => 4.0) = 4.0) "Exp";
      error ((ln 0.0 handle Ln => 4.0) = 4.0) "Ln";
      error (floor (23.23) = 23) "floor1";
      error (floor (~23.23) = ~24) "floor2";
      error (((floor (23.0E23)) handle Floor => 4) = 4) "Floor";
      error (23 + 12 = 35 andalso ~4 + 5 = 1) "+";
      error (34 - 12 = 22 andalso ~23 - 15 = ~38) "-";
      error (12 * 3 = 36 andalso ~23 * 2 = ~46) "*";
      map checkdivmod [(2,3), (34, ~3), (5, ~2), (~7, 3)];
      error (((3 div 0) handle Div => 60) = 60) "Div";
      error (((3 mod 0) handle Mod => 45) = 45) "Mod";
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
      error (4.0 + 3.0 = 7.0) "+";
      error (4.0 - 1.0 = 3.0) "-";
      error (4.0 * 3.0 = 12.0) "*";
(*      print "* ok.\n"; *)
      error (9.0 / 2.0 = 4.5) "/";
(*      print "/ ok.\n"; *)
      error (((4.0 / 0.0) handle Quot => 23.12) = 23.12) "Quot"; 
(*      print "Quot ok.\n"; *)
      error (~ 5.3 = ~5.3) "~1";
      error (~ (~2.23) = 2.23) "~2";
      error (abs 5.23 = 5.23) "abs1";
      error (abs (~23.12) = 23.12) "abs2";
      error (real 5 = 5.0) "real1";
      error (real ~5 = ~5.0) "real2";
      error (sqrt 0.0 = 0.0) "sqrt1";
      error (sqrt 2.0 > 1.4) "sqrt2";
      error (sqrt 2.0 < 1.5) "sqrt3";
      error (((sqrt ~4.0) handle Sqrt => 3.0) = 3.0) "Sqrt";
(*      print "Sqrt ok.\n"; *)

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

  val _ = print "End of test.\n"

end



