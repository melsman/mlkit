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

exception Chr and Quot and Floor and Sqrt and
      Exp and Ln and Io of string


(* ============================
 * BUILT-IN PRIMITIVES
 * ============================ *)

fun op = (x: ''a, y: ''a): bool =           prim ("=", "=", (x, y))
fun (x: 'a ref) := (y: 'a): unit =          prim (":=", ":=", (x, y))
fun !(x: 'a ref): 'a =                      prim ("!", "!", x)
fun ord (c : char) : int =                  prim ("id", "id", c)



(* ======================
 * IMPORTED PRIMITIVES
 * ====================== *)

fun chr (i:int): char =                     prim ("chrChar", "chrChar", (i, Chr))
fun size (s:string): int =                  prim ("sizeString", "sizeString", s)
fun explode (str: string): char list =      prim ("explodeString", "explodeStringProfiling", str)
fun implode (chars: char list): string =    prim ("implodeChars", "implodeCharsProfiling", chars)
fun op ^ (s1:string, s2:string): string =   prim ("concatString", "concatStringProfiling", (s1, s2))
fun real(x: int): real =                    prim ("realInt", "realInt", x)
fun floor(x:real):int =                     prim ("floorFloat", "floorFloat", x)
fun (x: real) / (y: real): real =           prim ("divFloat", "divFloat", (x, y))
fun sqrt(x: real): real =                   prim ("sqrtFloat", "sqrtFloat", x)
fun exp(x: real): real =                    prim ("expFloat", "expFloat", x)
fun ln(x: real): real =                     prim ("lnFloat", "lnFloat", x)
fun sin(x: real): real =                    prim ("sinFloat", "sinFloat", x) 
fun cos(x: real): real =                    prim ("cosFloat", "cosFloat", x) 
fun atan(x: real): real =                   prim ("atanFloat", "atanFloat", x) 


abstype instream = INS of int                 (* Streams ala the old Def. *)
and outstream = OUTS of int
with
  val std_in : instream =                     INS(prim ("stdInStream", "stdInStream", 0))
  val std_out : outstream =                   OUTS(prim ("stdOutStream", "stdOutStream", 0))

  exception CANNOT_OPEN
  fun open_in(f: string): instream =          INS(prim ("openInStream", "openInStream", (f, CANNOT_OPEN)))
                                              handle CANNOT_OPEN => raise Io("Cannot open " ^ f)
  fun open_out(f: string): outstream =        OUTS(prim ("openOutStream", "openOutStream", (f, CANNOT_OPEN)))
                                              handle CANNOT_OPEN => raise Io("Cannot open " ^ f)
  fun input(INS i, n: int): string =          prim ("inputStream", "inputStreamProfiling", (i, n))
  fun lookahead(INS i): string =              prim ("lookaheadStream", "lookaheadStreamProfiling", i) 
  fun close_in(INS i): unit =                 prim ("closeStream", "closeStream", i)
  fun end_of_stream(INS i): bool =            prim ("endOfStream", "endOfStream", i)
  val output_exval =                          Io "Output stream is closed"
  fun output(OUTS i, str: string): unit =     prim ("outputStream", "outputStream", (i, str, output_exval))
  fun close_out(OUTS i): unit =               prim ("closeStream", "closeStream", i)
  fun flush_out(OUTS i): unit =               prim ("flushStream", "flushStream", i)
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

fun say s = output(std_out, s)
