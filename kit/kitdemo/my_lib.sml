
(*---------------------------------------------------
 *  SML interface to the C functions in my_lib.c
 *---------------------------------------------------*)

(* The function power calculates base^n. The result type int is
 * unboxed, i.e. no regions are passed to the C function. We therefore
 * use the same C function when profiling is enabled. *)

fun power(base : int, n : int) : int = 
  prim ("power", (base, n))

(* We also have a version using auto conversion: *)

fun power_auto(base : int, n : int) : int = 
  prim ("@power_auto", (base, n))

(* The function power_real is similar to power, except that the base
 * is now a real. The result type is also a real so an address to
 * pre-allocated space for the result is passed to the C function. The
 * function does not allocate into an infinite region so the same C
 * function can be used when profiling is enabled. *)

fun power_real (base : real, n : int) : real = 
  prim ("power_real", (base, n))
      
(* The function print_string_list prints a list of strings on
 * stdout. The result value is unit so the same C function may be used
 * when profiling is enabled. *)

fun print_string_list (string_list : string list) : unit = 
  prim ("print_string_list", string_list)

(* The function power_exn shows how an exception may be passed and
 * raised in the C function. The result value is a real so the same
 * function may be used when profiling is enabled. *)

exception Power
fun power_exn (base : real, n : int) : real = 
  prim ("power_exn", (base, n, Power))

(* The function dir shows how we can use UNIX system calls to get the
 * contents of a directory. We also show how the ML exception
 * mechanism may be used to handle errors when trying to find and read
 * the directory. It is necessary to make a special version of this C
 * function because we return a list of strings, and they are
 * allocated in two infinite regions. The directory list is returned
 * in reverse order because the C function builds the string list
 * backwards. *)

exception DIR
fun dir (directory : string) : string list = 
  prim ("dir", (directory, DIR))

(* The function real_list shows, that an infinite region is passed to
 * the C function to hold the reals. In function power_real an address
 * to pre_allocated space was passed but that is not possible when the
 * reals are in a list.  The C function shows how a list can be
 * constructed forwards.  A special C function for profiling is
 * necessary because we allocate in three infinite regions. *)

fun real_list () : real list = 
  prim ("real_list", ())

(* The function change_elem exchanges the elements in a pair (no 
 * sharing). *)
fun change_elem (p : int*string) : string*int =
  prim ("change_elem", p)
