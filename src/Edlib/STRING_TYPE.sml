(*$STRING_TYPE *)

signature STRING_TYPE =
sig

(* CHARACTER CLASSES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author


DESCRIPTION

   Functions to find the types of the first character in a string.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:32  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.4  91/02/11  19:32:22  19:32:22  db (Dave Berry)
Moved the names of ASCII control characters to ASCII.sml as part of
the major reorganisation of the library.

Revision 1.3  91/01/25  16:57:40  16:57:40  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:44  17:08:44  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:57:47  16:57:47  db (Dave Berry)
Initial revision


*)


(* CONSTANTS *)

  val digits: string
   (* digits = "1234567890"; *)
  val hexes: string
   (* hexes = "1234567890abcdefABCDEF"; *)
  val formats: string
   (* formats; contains space, tab, newline, carriage return, backspace,
      and formfeed *)
  val uppers: string
   (* uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; *)
  val lowers: string
   (* lowers = "abcdefghijklmnopqrstuvwxyz"; *)
  val letters: string
   (* letters = uppers ^ lowers; *)
  val alNums: string
   (* alNums = letters ^ digits; *)
  val ids: string
   (* ids = alNums ^ "_'"; *)
  val symbols: string
   (* symbols = "!%&$#+-/:<=>?@\\~`^|*"; *)
  val puncts: string
   (* puncts = symbols ^ "'_(),.;[]{}"; *)
  val visibles: string
   (* visibles = letters ^ digits ^ puncts *)
  val prints: string
   (* prints = visibles ^ formats *)
  val controls: string
   (* controls; the control characters (Ascii numbers 0 - 31 and 127) *)
  val asciis: string
   (* asciis = controls ^ visibles *)


(* OBSERVERS *)

  (* These functions test the first character of a string for membership
     of the above sets.  They raise Empty if the string is empty. *)

  exception Empty of string
   (* Empty fn; raised if the function called fn is applied to an empty
      string. *)

  val isDigit: string -> bool
  val isHex: string -> bool
  val isFormat: string -> bool
  val isPrint: string -> bool
  val isVisible: string -> bool
  val isLetter: string -> bool
  val isUpper: string -> bool
  val isLower: string -> bool
  val isPunct: string -> bool
  val isControl: string -> bool
  val isAlNum: string -> bool
  val isId: string -> bool
  val isSymbol: string -> bool
  val isAscii: string -> bool

end
