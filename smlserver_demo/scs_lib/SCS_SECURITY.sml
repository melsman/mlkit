(* The SCS Security module is basically a port of the ACS security *
 * module found in ACS version 3.4.4 (http://www.arsdigita.com)    *
 * Ported by Niels Hallenberg, nh@it.edu 2001-10-21.               *)

signature SCS_SECURITY =
  sig
    val randomChar : unit -> char
  end

(*
  [randomChar] returns a random character which can be used for a password or token.
*)

