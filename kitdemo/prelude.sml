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

fun op = (x: ''a, y: ''a): bool =           prim(0, (x, y))
fun (x: 'a ref) := (y: 'a): unit =          prim(17, (x, y)) 
fun !(x: 'a ref): 'a =                      prim(18, x) 
fun ord (c:char): int =                     prim(11, c)


(* ======================
 * IMPORTED PRIMITIVES
 * ====================== *)

fun chr (i:int): char =                     prim(31, ("chrChar", "chrChar", i, Chr))
fun size (s:string): int =                  prim(31, ("sizeString", "sizeString", s))
fun explode (str: string): char list =      prim(31, ("explodeString", "explodeStringProfiling", str))
fun implode (chars: char list): string =    prim(31, ("implodeChars", "implodeCharsProfiling", chars))
fun op ^ (s1:string, s2:string): string =   prim(31, ("concatString", "concatStringProfiling", s1, s2)) 
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

fun say s = output(std_out, s)
