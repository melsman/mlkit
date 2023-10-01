(** Command-line name and arguments. *)

signature COMMAND_LINE =
  sig
    val name : unit -> string
    val arguments : unit -> string list
  end

(**

[name()] returns the name used to invoke the current program.

[arguments()] returns the argument list used to invoke the current
program.

[Discussion] The precise semantics of the above operations are
operating system and implementation-specific. For example, name might
return a full pathname or just the base name. See also the comment
under arguments.

*)
