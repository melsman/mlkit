signature NON_STANDARD =

(* NON-STANDARD FUNCTIONS REQUIRED BY THE LIBRARY.

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   These functions are needed to load the library, but are not defined
   in the Definition of Standard ML.  The load file for each compiler
   must define a structure to match this signature.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:19  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/01/25  11:29:16  11:29:16  db (Dave Berry)
Initial revision


*)

sig
(*
  val use: string -> unit
   (* use file; load the SML code in named file into the current top-level
      environment. *)
*)

  val flush_out: TextIO.outstream -> unit
   (* flush_out os; flush any buffered characters on os. *)
end
