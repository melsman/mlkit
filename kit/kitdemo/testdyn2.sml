(* ------------------------------------------------------------------- *
 *   testdyn2, 13/02/1995 20:53, Martin Elsman                         *
 *             15/04/1997, Niels Hallenberg                            *
 *   Dynamic test of input/output primitives...                        *
 *   Run testdyn1 prior to this test...                                *
 * ------------------------------------------------------------------- *)

let 
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

  (* ------------------------
   * Misc.
   *-------------------------*)
  fun digit n = chr(ord "0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)
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

  fun int_to_string(n) = if n >= 0 then implode(digits(n,[]))
			 else "~" ^ int_to_string(~n)

  fun print s = output (std_out, s)
  fun print_int_tuple (a,b) = 
    print ("("^ int_to_string a ^ "," ^ int_to_string b ^ ")\n")
  fun error b s = if b then () else print ("Error - " ^ s ^ "...\n")

  (*---------------*)
  (* testing stuff *)
  (*---------------*)
  val _ =
    let
      val _ = print "Testing [open_out, output, close_out]\n"
      val os = open_out ("/tmp/test.txt");
    in
      error (output (os, "hello\n") = ()) "output1";
      error (close_out os = ()) "close_out";
      error ((output (os, "wow"); false) handle (Io s) =>
	     s = "Output stream is closed") "output2"
    end;

  val is =
    let
      val _ = print "Testing [open_in, input, lookahead, end_of_stream, close_in]\n"
      val is = open_in ("/tmp/test.txt");
      val k = input(is,3)
    in
      error (k = "hel") ("input1, k=[" ^ k ^ "]");
      error (not (end_of_stream is)) "end_of_stream1";
      error (lookahead is = "l") "lookahead1"; is
    end

  val _ =
    let
      val j = input(is,3)
    in
      error (j = "lo\n") ("input2, j=[" ^ j ^ "]");
      error (end_of_stream is) "end_of_stream2";
      error (lookahead is = "") "lookahead2";
      error (close_in is = ()) "close_in";
      error ((open_in("I_do_not_exist");false) handle (Io s) =>
	     s = "Cannot open I_do_not_exist") "open_in"
    end

  val _ =
    let
      val _ = print "Testing [std_in, input] (interactively)\n"
      val _ = print "\nWrite the word 'Potato' (and return)\n\n"
      val s = input(std_in, 7)
      val _ = print "\nNow, write the word 'hello' (and return)\n\n"
      val s2 = input(std_in, 6)

    in
      error (s = "Potato\n") "std_in, input";
      error (s2 = "hello\n") "std_in, input"
    end

in
  print "End of test.\n"
end



