(*$StringParse: STRING_PARSE List StringType Instream Outstream *)

structure StringParse: STRING_PARSE =

(* STRING CONVERTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        14 Feb 1990

Maintenance:	Author

DESCRIPTION

   Standard conversion functions on the built-in type "string".


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:36  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.10  91/02/22  14:49:45  14:49:45  db (Dave Berry)
Added words function, based on the words, words', wordSingles and
wordSingles' functions that I've just removed from String.sml.

Revision 1.9  91/02/20  16:01:58  16:01:58  db (Dave Berry)
Added type synonym T.

Revision 1.8  91/02/12  14:42:22  14:42:22  db (Dave Berry)
Moved file and fromFile functions here from String.sml.
Added dependency on Outstream.

Revision 1.7  91/02/11  20:52:34  20:52:34  db (Dave Berry)
Moved the string function to String.sml.
Changed references to the List' structure to its new name: List.
This forms part of the major reorganisation of the library.

Revision 1.6  91/02/05  11:06:10  11:06:10  db (Dave Berry)
Changed read functions slightly to use new definition of Instream.eof.

Revision 1.5  91/02/04  15:10:56  15:10:56  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.4  91/01/30  17:43:11  17:43:11  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.3  91/01/25  20:22:13  20:22:13  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.2  91/01/25  15:44:08  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.1  90/12/20  15:52:16  15:52:16  db (Dave Berry)
Initial revision


*)

struct

  open EdlibGeneral
  open OldIO
  open OldString

(* TYPES *)

  type T = string


(* OBSERVERS *)

  val fixedWidth = false


(* CONVERTORS *)

  local
    (* Implement parse in terms of lists.  We use this structure to build the
       String structure, so the String operations aren't available. *)
    fun parseString [] = Fail ([], [])
    |   parseString ["\\"] = Fail (["\\"], [])
    |   parseString ["\\^"] = Fail (["\\^"], [])
    |   parseString (l as "\\" :: "^" :: c :: t) =
	  if 64 <= ord c andalso ord c <= 95 then
            case parseString t of
	      OK (s, l) => OK (chr (ord c - 64) :: s, l)
            | Fail (s, l) => Fail ("\\" :: "^" :: c :: s, l)
	  else Fail ([], l)
    |   parseString ("\\" :: c :: t) =
	  if StringType.isDigit c then parseDigits c t
          else
	    let val tmp =
                  if StringType.isFormat c
		  then parseFormat t
		  else parseString t
            in case tmp of
                 OK (s, l) =>
		   (case c of
		      "n" => OK ("\n" :: s, l)
		    | "t" => OK ("\t" :: s, l)
		    | "\"" => OK ("\"" :: s, l)
		    | "\\" => OK ("\\" :: s, l)
		    | c => Fail ("\\" :: c :: s, l)
		   )
               | Fail (s, l) => Fail ("\\" :: c :: s, l)
            end
    |   parseString ("\"" :: t) = OK ([], t)
    |   parseString (c :: t) =
          case parseString t of
            OK (s, l) => OK (c :: s, l)
          | Fail (s, l) => Fail (c :: s, l)

    and parseFormat [] = Fail ([], [])
    |   parseFormat ("\\" :: t) =
        ( case parseString t of
            OK (s, l) => OK ("\\" :: s, l)
          | Fail (s, l) => Fail ("\\" :: s, l)
        )
    |   parseFormat (c :: t) =
          if StringType.isFormat c
          then
            case parseFormat t of
              OK (s, l) => OK (c :: s, l)
            | Fail (s, l) => Fail (c :: s, l)
          else Fail ([], t)

    and parseDigits d1 [] = Fail ([], ["\\", d1])
    |   parseDigits d1 [d2] = Fail ([], ["\\", d1, d2])
    |   parseDigits d1 (d2 :: d3 :: t) =
	  if not (StringType.isDigit d2) orelse
	     not (StringType.isDigit d3)
	  then Fail (["\\", d1, d2, d3], t)
	  else
	    let fun digit d = ord d - ord "0"
	        val c = chr (digit d1 * 188 + digit d2 * 10 + digit d3)
	    in
	      case parseString t of
	        OK (s, l) => OK (c :: s, l)
	      | Fail (s, l) => Fail (c :: s, l)
	    end
  in
    fun parse s =
          let val ("\"" :: t) = List.dropPrefix (not o StringType.isVisible)
						 (explode s)
          in case parseString t of
               OK (res, l) => OK (implode res, implode l)
             | Fail (res, l) => Fail (Some (implode res), implode l)
          end
          handle Bind => Fail (Some "", s)
  end

  local
    fun readString i =
          if Instream.eof i then Fail [] else
          case Instream.input1 i of
            "\"" => OK []
          | "\\" =>
             ( if Instream.eof i then Fail ["\\"] else
	       case Instream.input1 i of
                 "^" =>
		  ( if Instream.eof i then Fail ["\\", "^"] else
		    let val c = Instream.input1 i
		    in
	  	       if 64 <= ord c andalso ord c <= 95 then
            	         case readString i of
	      	           OK s => OK (chr (ord c - 64) :: s)
            	         | Fail s => Fail ("\\" :: "^" :: c :: s)
		       else Fail (["\\", "^", c])
		    end
		  )
               | c  =>
		  if StringType.isDigit c then readDigits c i else
                  let val tmp =
                       if StringType.isFormat c
                       then readFormat i
                       else readString i
                  in case tmp of
                       (OK s) =>
			 (case c of
			    "n" => OK ("\n" :: s)
			  | "t" => OK ("\t" :: s)
			  | "\"" => OK ("\"" :: s)
			  | "\\" => OK ("\\" :: s)
			  | c => Fail ("\\" :: c :: s)
			 )
                     | (Fail s) => Fail ("\\" :: c :: s)
                  end
             )
          | c => case readString i of
                   (OK s) => OK (c :: s)
                 | (Fail s) => Fail (c :: s)

    and readFormat i =
      if Instream.eof i then Fail [] else
      case Instream.input1 i of
        "\\" => readString i
      | c => if StringType.isFormat c then readFormat i else Fail []

    and readDigits d1 i =
      if Instream.eof i then Fail ["\\", d1] else
      let val d2 = Instream.input1 i
      in
         if Instream.eof i then Fail ["\\", d1, d2] else
	 let val d3 = Instream.input1 i
	 in
	    if not (StringType.isDigit d2) orelse
	       not (StringType.isDigit d3)
	    then Fail (["\\", d1, d2, d3])
	    else
	      let fun digit d = ord d - ord "0"
	          val c = chr (digit d1 * 188 + digit d2 * 10 + digit d3)
	      in case readString i of
	           OK s => OK (c :: s)
	         | Fail s => Fail (c :: s)
	      end
         end
      end
  in
    fun read i =
        ( Instream.skip (not o StringType.isVisible) i;
          if Instream.eof i orelse Instream.lookahead i <> "\""
          then Fail (Some "")
          else
            case readString i of
              OK s => OK (implode s)
            | Fail s => Fail (Some (implode s))
        )
  end

  fun fromFile name =
        let fun fromFile' i =
                  let val s = Instream.input (i, 256)
                  in if size s < 256 then (Instream.closeIn i; s)
                     else s ^ fromFile' i
                  end
        in fromFile' (Instream.openIn name)
        end

  fun file name s =
        let val os = TextIO.openOut name
        in TextIO.output (os, s);
           TextIO.closeOut os
        end


(* MANIPULATORS *)

  fun words {groups, singles, preserveGroups, preserveSingles} s =
        let val groups' = explode groups
            val singles' = explode singles

            (* takeWord l; parses a word. *)
            fun takeWord [] = ("", [])
            |   takeWord (l as h::t) =
                  if List.member h groups' then ("", l)
                  else if List.member h singles' then ("", l)
		  else
                    let val (s, l) = takeWord t
                    in (h^s, l)
                    end

            (* takeGroup l; parses a group of separators. *)
            fun takeGroup [] = ("", [])
            |   takeGroup (l as h::t) =
                  if List.member h groups' then
                    let val (s, l) = takeGroup t
                    in if preserveGroups then (h^s, l)
		       else ("", l)
                    end
                  else ("", l)

            (* takeWord l; parses a singleton. group or word. *)
            fun takeElement [] = ("", [])
            |   takeElement (l as h::t) =
                  if List.member h groups' then
		    let val result as (_, l') = takeGroup l
		    in if preserveGroups then result
		       else takeElement l'
		    end
                  else if List.member h singles' then
		    if preserveSingles then (h, t)
		    else takeElement t
                  else
                    let val (s, l) = takeWord t
                    in (h^s, l)
                    end

            fun words' l =
                  case takeElement l of
		    (w, []) => [w]
                  | (w, t)  => w :: words' t

        in case explode s of
	     [] => []
           | (l as h::t) =>
                 if List.member h groups' orelse
		    List.member h singles'
		 then "" :: words' l
		 else words' l
        end

end
