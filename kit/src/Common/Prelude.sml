(* Prelude for the Kit. *)

infix  3 := o
infix  4 = <> < > <= >= 
infix  5 @
infixr 5 ::
infix  6 + - ^
infix  7 div mod / * 


exception 
      Ord
      and Chr
      and Div
      and Mod
      and Quot
      and Floor
      and Sqrt
      and Exp
      and Ln
      and Io of string
      and Match
      and Bind
      and Interrupt

type unit = { }					

val not = fn true => false | false => true

datatype 'a list = nil | op :: of 'a * 'a list

fun op = (x: ''a, y: ''a): bool = prim(0, (x, y))
fun floor(x: real): int = prim(1, (x, Floor))
fun real(x: int): real = prim(2, x)
fun sqrt(x: real): real = prim(3, (x, Sqrt))
fun sin(x: real): real = prim(4, x)
fun cos(x: real): real = prim(5, x)
fun arctan(x: real): real = prim(6, x)
fun exp(x: real): real = prim(7, (x, Exp))
fun ln(x: real): real = prim(8, (x, Ln))
fun size(x: string): int = prim(9, x)
fun chr(x: int): string = prim(10, (x, Chr))
fun ord(x: string): int = prim(11, (x, Ord))
fun explode(x: string): string list = prim(12, x)
fun implode(x: string list): string = prim(13, x)
fun (x: real) / (y: real): real = prim(14, (x, y, Quot))
fun (x: int) div (y: int): int = prim(15, (x, y, Div))
fun (x: int) mod (y: int): int = prim(16, (x, y, Mod))
fun (x: 'a ref) := (y: 'a): unit = prim(17, (x, y))
fun !(x: 'a ref): 'a = prim(18, x)

	(* `!' is primitive to make the type non-weak (the Definition
	   is inconsistent here).
	*)

fun (f o g) x = f(g x)

val op <> = not o (op =)

fun nil @ M = M
  | (x :: L) @ M = x :: (L @ M)

fun map F nil = nil
  | map F (x :: L) = F x :: map F L

fun rev nil = nil
  | rev (x :: L) = rev L @ [x]

fun s ^ s' = implode(explode s @ explode s')

(* Our I/O streams are just numbers, so that we don't have to implement
   any primitive type in the interpreter. *)

abstype instream = INSTREAM of int
    and outstream = OUTSTREAM of int
with
  val std_in = INSTREAM 0
  val std_out = OUTSTREAM 0

  fun open_in(f: string) = INSTREAM(prim(19, (f, Io("Cannot open " ^ f))))

  fun open_out(f: string) = OUTSTREAM(prim(20, (f, Io("Cannot open " ^ f))))

  fun input(INSTREAM i, n: int): string = prim(21, (i, n))
    
  fun lookahead(INSTREAM i): string = prim(22, i)

  fun close_in(INSTREAM i): unit = prim(23, i)

  fun end_of_stream(INSTREAM i): bool = prim(24, i)
    
  fun output(OUTSTREAM i, str: string): unit =
    prim(25, (i, str, Io "Output stream is closed"))

  fun close_out(OUTSTREAM i): unit = prim(26, i)

  fun flush_out(OUTSTREAM i): unit = prim(28,i) (* NOT Standard ML *)
end



(*
structure Kit =
  struct
    exception Use
    fun use f: unit = prim(27, (f, Io f, Use))
    val flush_out = flush_out                   (* NOT Standard ML *)
  end;
*)
(* To be used in stead of the structure above if compiler  *)

    exception Use
    fun use f: unit = prim(27, (f, Io f, Use));







