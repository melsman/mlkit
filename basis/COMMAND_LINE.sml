(* CommandLine -- SML Basis Library *)

signature COMMAND_LINE =
  sig
    val name      : unit -> string 
    val arguments : unit -> string list
  end

(* 
   [name ()] returns the name used to start the current process.

   [arguments ()] returns the command line arguments of the current process.
   Hence List.nth(arguments (), 0) is the first argument.
*)
