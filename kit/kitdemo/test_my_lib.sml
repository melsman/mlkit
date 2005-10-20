(* ----------------------------------------------------------
 *   Test of my_lib.c
 * ---------------------------------------------------------- *)

fun power_test () =
  let
    val base = 10
    val n = 4
  in
    print ("Test of the power function...\n");
    print ("Result of power(" ^ Int.toString base ^ "," ^ Int.toString n ^ 
	   ")=" ^ Int.toString(power(base,n)) ^ ".\n\n")
  end

fun power_auto_test () =
  let
    val base = 10
    val n = 4
  in
    print ("Test of the power_auto function...\n");
    print ("Result of power_auto(" ^ Int.toString(base) ^ "," ^ Int.toString(n) ^ 
	   ")=" ^ Int.toString(power_auto(base,n)) ^ ".\n\n")
  end

fun power_real_test () =
  let
    val base = 10.5
    val n = 4
  in
    print ("Test of the power_real function...\n");
    print ("Result of power_real(10.5" ^ "," ^ Int.toString(n) ^ ")=" ^ 
	   Real.toString(power_real(base,n)) ^ ".\n\n")
  end

fun power_exn_test () =
  let
    val base = 10.5
    val n = 4
  in
    print ("Test of the power_exn function...\n");
    print ("Result of power_exn(10.5" ^ "," ^ Int.toString(n) ^ ")=" ^ 
	   Real.toString(power_exn(base,n)) ^ ".\n\n");
    print ("Exception Power should be raised, because the power_exn function");
    print ("\nis called with base = ~10.5.\n");
    print ("Result of power_exn(~10.5" ^ "," ^ Int.toString(n) ^ ")=" ^ 
	   Real.toString(power_exn(~base,n)) ^ ".\n\n")
    handle Power => print ("Ok, now handling exception Power\n\n")
  end

fun print_string_list_test () =
  let
    val strs = ["The", " ", "ML", " ", "Kit", "\n"]
  in
    print ("Test of the print_string_list function.\n");
    print ("The string The MLKit should be written.\n");
    print_string_list strs;
    print ("\n")
  end

fun dir_test () =
  let
    val filename1 = "../kitdemo"
    val filename2 = "../itdemo"
    fun print_list ss = app (fn s => print (s^"\n")) ss
  in
    print ("Test of the dir function\n");
    print ("We first try directory " ^ filename1 ^ "\n");
    print ("Exception DIR should not be raised\n");
    print_list (dir filename1)
    handle DIR => print ("\nError, now handling exception DIR\n\n");
    print ("\nWe then try directory " ^ filename2 ^ "\n");
    print ("Exception DIR should be raised\n");
    print_list (dir filename2)
    handle DIR => print ("\nOk, now handling exception DIR\n\n")
  end

fun real_list_test() =
  let
    val res = real_list()
    fun pp_real r = print(Real.toString(r*100.0) ^ " ")
  in
    print("Test of the real_list function.\n");
    app pp_real res;
    print("\n\n")
  end

fun change_elem_test () =
  let
    val p = (42, "The MLKit")
    val (s,i) = change_elem p
  in
    print("Test of change_elem.\n");
    print("The pair (42,The MLKit) is changed into ");
    print("("^s^","^Int.toString(i)^").\n")
  end
    
val _ = power_test()
val _ = power_auto_test ()
val _ = power_real_test()
val _ = power_exn_test()
val _ = print_string_list_test()
val _ = dir_test()
val _ = real_list_test()
val _ = change_elem_test()


