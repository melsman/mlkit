(******************************************************************)
(* This file contains examples of declarations of C-interface     *)
(* functions, plus Standard ML code which uses these functions.   *)
(*                                                                *)
(******************************************************************)

 (* auxiliary functions *)

fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)

fun print_string x = output(std_out,x)

fun digit n = chr(ord #"0" + n)
fun digits(n,acc) =
  if n >=0 andalso n<=9 then digit n:: acc
  else digits (n div 10, digit(n mod 10) :: acc)
    
fun string(n) = implode(digits(n,[]))
fun print_num n = print_string(string n)

(*---------------------------------------------------*)
(*      Interface functions that call prim           *)
(*---------------------------------------------------*)

(* The file my_lib.c contains declarations matching the following *)
(* prim calls.                                                    *)

(* The function power calculates base^n. The result type int *)
(* is unboxed, i.e. no regions are passed to the C function. *)
(* We therefore use the same C function when profiling is    *)
(* enabled.                                                  *)

fun power(base : int, n : int) : int = prim ("power", "power", (base, n))

(* We also have a version using auto conversion.             *)

fun power_auto(base : int, n : int) : int = 
      prim ("@power_auto", "@power_auto", (base, n))

(* The function power_real is similar to power, except that  *)
(* the base is now a real. The result type is also a real so *)
(* an address to pre-allocated space for the result is       *)
(* passed to the C function. The function does not allocate  *)
(* into an infinite region so the same C function can be     *)
(* used when profiling is enabled.                           *)

fun power_real (base : real, n : int) : real = 
      prim ("power_real", "power_real", (base, n))

(* The function print_string_list prints an ML Kit list of   *)
(* ML Kit strings on stdout. The result value is unit so the *)
(* same C function may be used when profiling is enabled.    *)

fun print_string_list (string_list : string list) : unit = 
      prim ("print_string_list", "print_string_list", string_list)

(* The function power_exn shows how an exception may be      *)
(* passed and raised in the C function. The result value is  *)
(* a real so the same function may be used when profiling is *)
(* enabled.                                                  *)

exception Power of string
fun power_exn (base : real, n : int) : real = 
      prim ("power_exn", "power_exn", (base, n, Power "This is power"))

(* The function dir shows how we can use UNIX system calls   *)
(* to get the contents of a directory. We also show how the  *)
(* ML exception mechanism may be used to handle errors when  *)
(* trying to find and read the directory.                    *)
(* It is necessary to make a special version of this C       *)
(* function because we return a list of strings, and they    *)
(* are allocated in three infinite regions.                  *)
(* The directory list is returned in reverse order because   *)
(* the C function builds the string list backwards.          *)

exception DIR of string
fun dir (directory : string) : string list = 
      prim ("dir", "dirProf", (directory, DIR "Cannot open directory"))

(* The function real_list shows, that an infinite region is  *)
(* passed to the C function to hold the reals. In function   *)
(* power_real an address to pre_allocated space was passed   *)
(* but that is not possible when the reals are in a list.    *)
(* The C function shows how a list can be constructed        *)
(* forwards.                                                 *)
(* A special C function for profiling is necessary because   *)
(* we allocate in three infinite regions.                    *)

fun real_list () : real list = prim ("real_list", "real_listProf", ())

(* The function change_elem exchanges the elements in a pair.*)
fun change_elem (p : int*string) : string*int =
      prim ("change_elem", "change_elem", p)

(*************************************************************)
(* Use of interface functions:                               *)
(*************************************************************)

fun power_test () =
  let
    val base = 10
    val n = 4
  in
    print_string ("Test of the power function...\n");
    print_string ("Result of power(" ^ string(base) ^ "," ^ string(n) ^ ")=" ^ string(power(base,n)) ^ ".\n\n")
  end

fun power_auto_test () =
  let
    val base = 10
    val n = 4
  in
    print_string ("Test of the power_auto function...\n");
    print_string ("Result of power_auto(" ^ string(base) ^ "," ^ string(n) ^ ")=" ^ string(power_auto(base,n)) ^ ".\n\n")
  end

fun power_real_test () =
  let
    val base = 10.5
    val n = 4
  in
    print_string ("Test of the power_real function...\n");
    print_string ("The result is printed without digits.\n");
    print_string ("Result of power_real(10.5" ^ "," ^ string(n) ^ ")=" ^ string(floor(power_real(base,n))) ^ ".\n\n")
  end

fun power_exn_test () =
  let
    val base = 10.5
    val n = 4
  in
    print_string ("Test of the power_exn function...\n");
    print_string ("The result is printed without digits.\n");
    print_string ("Result of power_exn(10.5" ^ "," ^ string(n) ^ ")=" ^ string(floor(power_exn(base,n))) ^ ".\n\n");
    print_string ("Exception Power should be raised, because the power_exn function");
    print_string ("\nis called with base = -10.5.\n");
    print_string ("Result of power_exn(-10.5" ^ "," ^ string(n) ^ ")=" ^ string(floor(power_exn(0.0-base,n))) ^ ".\n\n")
    handle Power s => print_string ("Ok, now handling exception Power " ^ s ^ "\n\n")
  end

fun print_string_list_test () =
  let
    val strs = ["The", " ", "ML", " ", "Kit", "\n"]
  in
    print_string ("Test of the print_string_list function.\n");
    print_string ("The string The ML Kit should be written.\n");
    print_string_list strs;
    print_string ("\n")
  end

fun dir_test () =
  let
    val filename1 = "../../kitdemo/"
    val filename2 = "../../itdemo/"
  in
    print_string ("Test of the dir function\n");
    print_string ("We first try directory: " ^ filename1 ^ "\n");
    app (fn s => print_string ("\n"^s)) (dir(filename1))
    handle DIR s => print_string ("\nOk, now handling exception DIR " ^ s ^ "\n\n");
    print_string ("\nWe then try directory: " ^ filename2 ^ "\n");
    print_string ("Exception DIR should be raised\n");
    app (fn s => print_string ("\n"^s)) (dir("../../itDemo/"))
    handle DIR s => print_string ("\nOk, now handling exception DIR " ^ s ^ "\n\n")
  end

fun real_list_test() =
  let
    val res = real_list()
    fun pp_real r = print_string(string(floor(r*100.0)) ^ " ")
  in
    print_string("Test of the real_list function.\n");
    app pp_real res;
    print_string("\n\n")
  end

fun change_elem_test () =
  let
    val p = (42, "The ML Kit")
    val (s,i) = change_elem p
  in
    print_string("Test of change_elem.\n");
    print_string("The pair (42,The ML Kit) is changed into\n");
    print_string("     ("^s^","^string(i)^").\n")
  end
    
val _ = power_test()
val _ = power_auto_test ()
val _ = power_real_test()
val _ = power_exn_test()
val _ = print_string_list_test()
val _ = dir_test()
val _ = real_list_test()
val _ = change_elem_test()


