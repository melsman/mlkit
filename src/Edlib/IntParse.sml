(*$IntParse: PARSE String StringType Instream *)

structure IntParse: PARSE =

(* INTEGERS

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:12  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.7  91/02/11  20:03:30  20:03:30  db (Dave Berry)
Changed the name of this signature from IntObject to IntParse.
Moved the comparison and string functions to Int.sml.
This forms part of the major reorganisation of the library.

Revision 1.6  91/02/04  15:10:45  15:10:45  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.5  91/01/30  17:48:01  17:48:01  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.4  91/01/25  20:22:36  20:22:36  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.3  91/01/25  15:43:58  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.2  91/01/24  17:21:23  17:21:23  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:54:11  14:54:11  db (Dave Berry)
Initial revision


*)

struct

  open EdlibGeneral
  open OldString

(* TYPES *)

  type T = int


(* OBSERVERS *)

  val fixedWidth = false


(* CONVERTERS *)

  local
    infix 9 sub
    val op sub = String.sub

    val zero = ord "0"

    fun parseInt' ("", n) = OK (n, "")
    |   parseInt' (s, n) =
	  if StringType.isDigit s
	  then parseInt' (String.extract 1 (size s) s,
			  n * 10 + ord (s sub 0) - zero)
	  else OK (n, s)

    fun parseInt "" = Fail (None, "")
    |   parseInt s =
	  if StringType.isDigit s
	  then parseInt' (String.extract 1 (size s) s, ord (s sub 0) - zero)
	       handle _ => Fail (None, s)
	  else Fail (None, s)
  in
    fun parse s =
	  case String.skipSpaces s of
	    "" => Fail (None, s)
	  | s' =>
	      if s' sub 0 = "~" then
	       case parseInt (String.extract 1 (size s') s') of
	         OK (n, s'') => OK (~n, s'')
	       | Fail x => Fail x
	     else parseInt s'
  end

  local
    val zero = ord "0"

    fun readInt' (i, n) =
          let val s = Instream.lookahead i
          in if s = "" then OK n
	     else if not (StringType.isDigit s) then OK n
             else (Instream.input1 i; readInt' (i, n * 10 + ord s - zero))
          end

    fun readInt s i =
	  if not (StringType.isDigit s) then Fail None
	  else (Instream.input1 i;
		readInt' (i, ord s - zero))
	       handle _ => Fail None

  in
    fun read i =
        ( Instream.skip (not o StringType.isVisible) i;
	  if Instream.eof i then Fail None
	  else if Instream.lookahead i = "~"
	  then let val s = (Instream.input1 i; Instream.lookahead i)
	       in case readInt s i of
		    OK n => OK (~n)
		  | Fail x => Fail x
	       end
	  else readInt (Instream.lookahead i) i
        )
  end

end
