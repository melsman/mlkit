(*$SYSTEM *)

signature SYSTEM =
sig

(* SYSTEM FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:	        14 Nov 1989

Maintenance:	Author


DESCRIPTION

   Functions that interact with the operating system.

   These functions raise General.NotImplemented if the implementation doesn't
   provide them.  The values are extremely conservative; they should be
   replaced with the correct values in any implementation of ther library.
   
RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:33  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.5  91/04/10  16:56:49  16:56:49  db (Dave Berry)
Added polymorphic hash function.

Revision 1.4  91/01/25  16:57:43  16:57:43  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.3  91/01/25  16:02:27  16:02:27  db (Dave Berry)
Added isDir function.

Revision 1.2  91/01/24  17:08:49  17:08:49  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:57:57  16:57:57  db (Dave Berry)
Initial revision


*)


(* ML SYSTEM *)

  val collect: unit -> unit
   (* collect (); an identity function that gives a hint to the system that
      now would be a good time to perform garbage collection.  This function
      never raises General.NotImplemented, since it can't affect the
      computation. *)

  val eq: 'a -> 'a -> bool
   (* eq x y; returns false if x and y are not located at the same hardware
      address.  May return either true or false otherwise. *)

  val hash: ''a -> int
   (* hash x; applies a has function to x and returns the result. *)

  val quit: unit -> unit (* never returns. *)
   (* quit (); exit ML. *)


(* INTERFACE TO OPERATING SYSTEM *)

  exception NoFile of string * string
   (* NoFile (fn, file); raised if the function called fn tries to access
      a file f that doesn't exist. *)

  exception Permission of string * string
   (* Permission (fn, file); raised if the function called fn tries to access
      a protected file f. *)

  val use: string -> unit
   (* use "f"; read f as if it was typed at top level.  This function is not
      defined unless it is called from top-level or from top-level in a file
      that is itself being read as a result of a call to "use".  The function
      should check for files that recursively "use" each other. *)

  val cd: string -> unit
   (* cd "d"; change working directory to d.  Raise (NoFile ("cd", "d"))
      if d doesn't exist.  Raise (Permission ("cd", "d")) if d exists
      but doesn't allow the user access.  *)

  val isDir: string -> bool
   (* isDir "d"; returns true if d is a directory. *)

  val pwd: unit -> string
   (* pwd (); return the full name of the working directory. *)

  val dir: string -> string list
   (* dir "d"; returns a list of the names of the files in directory d.
      Raise (NoFile ("dir", "d")) if d doesn't exist.
      Raise (Permission ("cd", "d")) if d exists and is unreadable. *)

  val delete: string -> unit
   (* delete "f"; delete the file f, if possible.
      Raise (NoFile ("delete", "f")) if f doesn't exist.
      Raise (Permission ("delete", "f")) if f exists but can't be deleted. *)

  val system: string -> string
   (* system "c"; run the system command c, with input from std_in, and
      return the output of c. *)

  val getenv: string -> string
   (* getenv var; return the string associated with var in the system
      environment. *)
end
