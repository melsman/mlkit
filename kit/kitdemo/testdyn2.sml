(* ------------------------------------------------------------------- *
 *   testdyn2, 13/02/1995 20:53, Martin Elsman                         *
 *             15/04/1997, Niels Hallenberg                            *
 *   Dynamic test of input/output primitives...                        *
 *   Run testdyn1 prior to this test...                                *
 * ------------------------------------------------------------------- *)

let 
  fun digit n = chr(ord #"0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)

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



