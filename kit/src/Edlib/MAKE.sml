(* There's no point putting a tag on this signature! *)

signature MAKE =
sig

(* A PORTABLE MAKE SYSTEM

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

Maintenance:    Author


DESCRIPTION

   Each piece of code should be preceded by a tag declaration in a special
   comment.  Tag declaration comments begin with the string "(*$" instead of
   the usual "(*", and must start at the beginning of a line.  Formatting
   characters are forbidden before and after the initial "(*$" string, but
   are permitted between tags and dependencies.

   The following tag declaration associates the tag "Foo" with the code
   that follows it:

   (*$Foo *)

   The following tag declaration associates the tag "Bar" with the code that
   follows it, and states that this code depends on the code associated
   with the tags "Foo1" and "Foo2":

   (*$Bar: Foo1 Foo2 *)

   The code associated with a tag is terminated by the next tag declaration
   or the end of a file.  Keeping the dependency information with each
   piece of code makes it easier to keep the two in sync.  Tag declaration
   comments are a subset of ordinary comments, so files containing them can
   be compiled without using make.

   The easiest way to consult the dependency information is to list
   the files containing the code in a file (called a tag file), and to call 
   loadFrom with the name of that tag file.  This loads all the dependency
   information into the make system.  If you change the dependency
   information, use consultDecl or consultFile to update the stored
   dependencies.

   Once the dependency information has been loaded, call (make tag) to compile
   the code associated with the tag and all the code that it depends on.
   The system will write all the relevant code into a temporary file (called
   "%Make.tmp%" by default) and call use on that file.

   Once a piece of code has been read from a file, it is stored in a cache.
   When the code associated with a tag is changed, call (touch tag) to
   tell the make system that the current entry in the cache is obselete.
   consultDecl and consultFile do this automatically for the tags that they
   read.  If a piece of code fails to compile, then that entry in the cache
   is automatically touched as well.
   The system does not check the modification dates of files.
   
   Once all the changed pieces of code have been touched, call (again ()) to
   re-make your program.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:18  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.4  91/04/11  11:13:53  11:13:53  db (Dave Berry)
Fixed minor bugs in description.

Revision 1.3  91/02/20  16:42:54  16:42:54  db (Dave Berry)
Added uncompiled function.

Revision 1.2  91/02/11  18:44:54  18:44:54  db (Dave Berry)
Removed version value.

Revision 1.1  91/01/23  16:40:37  16:40:37  db (Dave Berry)
Initial revision


(The following end of comment symbols really are needed.) *) *) *)

*)

(* MANIPULATORS *)

  val resides: string * string -> unit
   (* resides (tag, filename) says that the item identified by tag lives in
      the named file.  Be sure to use the same filename string for each
      occurrence of a file.  *)

  val standsAlone: string -> unit
   (* standsAlone tag; declares tag to be independent of anything, i.e.
      clears any dependency information.  *)

  val depends: string * string -> unit
   (* depends (tag1, tag2); expresses a dependency.  Circularity not checked
      or catered for.  *)

  val consultDecl: string -> unit
   (* consultDecl tag; scans the tag declaration, extracting it from the
      appropriate file, and replaces the stored information with that found
      in the file. *)

  val consultFile: string -> unit
   (* consultFile filename; scans the named fildeclaration, examining all the tag
      declarations, and replace the dependency information currently stored
      about those tags with the information found in the file.  Useful when
      the dependency information changes for a number of items in a file. *)

  val whereIs: string -> string
   (* whereIs tag; returns the name of the file that Make thinks tag is in. *)

  val loadFrom: string -> unit
   (* loadFrom tagFile; reads in a list of filenames from the named tag file,
      and does a consultFile on each one.  *)

  val reload: unit -> unit
   (* reload (); reloads from the last used tag file. *)

  val wipeCache: unit -> unit
   (* wipeCache (); throw away the source text in the cache (to reclaim
      some heap). *)

  val forgetAll: unit -> unit
   (* forgetAll (); forget the compilation state of all objects. *)

  val touch: string -> unit
   (* touch tag; manually say that tag has been changed. *)

  val compiling_: string -> unit
   (* compiling msg; puts out a compiling message, turns on TraceML, hides
      the object in the cache.  *)

  val OK_: string -> unit
   (* OK_ tag; says that tag has been recompiled.  Make puts these
      OK calls actually within the temp file containing Foo, just
      after its occurrence.  Turns off TraceML.  *)

  val allOK: string list -> unit
   (* allOK l; like OK_, but marks all dependents as O.K. a well. It also
      takes a list, since List.map is most useful. Slower than OK_, but
      thorough.  *)

  val make: string -> unit
   (* make tag; gathers the tags of all the dependencies of tag
      which have changed, extracts the text into a temporary file,
      and does a `use' on this.  Also puts occurrences of (OK_ ("..."))
      within the temp file to tell the Make system that various objects
      are becoming up-to-date.  (make "Foo") also assumes that Foo
      has been altered.  *)

  val again: unit -> unit
   (* again (); performs a "make" of previous tag. *)

  val makeTask: string * string -> unit
   (* makeTask (Tag, file); extracts the sources as does make, but puts
      them into the named file, without the "compiling_" and "OK_" calls.  *)

  val setTempfile: string -> unit
   (* setTempfile file; use the named file as the temporary file. *)

  val uncompiled: unit -> unit
   (* Print any tags which have never been compiled (or which have
      subsequently been touched). *)
end;
