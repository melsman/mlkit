(*$BoolParse: PARSE Instream String *)

structure BoolParse: PARSE =

(* BOOLEANS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:00:59  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.8  91/02/11  19:56:44  19:56:44  db (Dave Berry)
Changed the name of this structure from BoolObject to BoolParse.  Moved
equality functions and string function to Bool.sml.  Removed ordering
functions altogether.  This forms part of the major reorganisation of
the library.

Revision 1.7  91/02/05  11:06:15  11:06:15  db (Dave Berry)
Changed read functions slightly to use new definition of Instream.eof.

Revision 1.6  91/02/04  15:10:40  15:10:40  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.5  91/01/30  17:42:51  17:42:51  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.4  91/01/25  20:22:31  20:22:31  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.3  91/01/25  15:43:47  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.2  91/01/24  17:20:52  17:20:52  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:49:15  14:49:15  db (Dave Berry)
Initial revision


*)

struct

  open EdlibGeneral

(* TYPES *)

  type T = bool


(* CONVERTERS *)

  fun parse s =
	let val s' = String.skipSpaces s
	in
          if String.extract 0 4 s' = "true"
	  then OK (true, String.extract 4 (size s') s')
	  else if String.extract 0 5 s' = "false"
	  then OK (false, String.extract 5 (size s') s')
	  else Fail (None, s')
	  handle String.Extract _ => Fail (None, s')
	end

  fun read i =
	(Instream.skip (not o StringType.isVisible) i;
         case Instream.lookahead i of
           "t" =>
	   ( case Instream.readString i "true" of
	       OK () =>
		 if (not o StringType.isId) (Instream.lookahead i)
		 then OK true
		 else Fail None
	     | Fail _ => Fail None
	   )
         | "f" =>
	   ( case Instream.readString i "false" of
	       OK () =>
		 if (not o StringType.isId) (Instream.lookahead i)
		 then OK false
		 else Fail None
	     | Fail _ => Fail None
	   )
         |  _  => Fail None
	)


(* OBSERVERS *)

  val fixedWidth = false

end
