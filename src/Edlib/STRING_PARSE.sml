(*$STRING_PARSE: InstreamType GeneralTypes *)

signature STRING_PARSE =
sig

(* STRING CONVERTERS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        14 Feb 1990

Maintenance:	Author


DESCRIPTION

   Standard conversion functions on the built-in type "string".
   These expect strings to be surrounded by quotes and with appropriate
   characters escaped.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:31  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.9  91/02/22  14:46:45  14:46:45  db (Dave Berry)
Added words function, based on the words, words', wordSingles and
wordSingles' functions that I've just removed from STRING.sml.

Revision 1.8  91/02/12  14:41:23  14:41:23  db (Dave Berry)
Moved file and fromFile functions here from STRING.sml.

Revision 1.7  91/02/12  12:19:17  12:19:17  db (Dave Berry)
Added type synonym eqtype T.

Revision 1.6  91/02/11  19:31:49  19:31:49  db (Dave Berry)
Added fixedWidth value.

Revision 1.5  91/02/04  15:38:57  15:38:57  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.4  91/01/30  18:08:34  18:08:34  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.3  91/01/25  19:10:25  19:10:25  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.2  91/01/25  16:57:38  16:57:38  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:57:29  16:57:29  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type T = string


(* CONVERTERS *)

  val parse:  string -> (string * string, string EdlibGeneral.Option * string) EdlibGeneral.Result

  val read: TextIO.instream -> (string, string EdlibGeneral.Option) EdlibGeneral.Result

  val fromFile: string -> string
   (* fromFile name; read the contents of name into a string.
      Raises the pervasive exception Io if something goes wrong. *)

  val file: string -> string -> unit
   (* file name s; write s to the new file called name.
      Raises the pervasive exception Io if something goes wrong. *)


(* OBSERVERS *)

  val fixedWidth: bool
   (* fixedWidth = false. *)


(* MANIPULATORS *)

  val words: {groups: string, singles: string, 
	      preserveGroups: bool, preserveSingles: bool} ->
	     string -> string list
   (* words {groups, singles, preserveGroups, preserveSingles} s;
      splits s into words.  Word boundaries are defined by each occurrence of
      a character in singles and by each consecutive sequence of characters
      in groups.  If preserveSingles or preserveGroups is true, then
      occurrences of the corresponding separator characters are included in
      the result.  If a character appears in both sep and singles, its
      presence in singles is ignored. *)

end
