(*$ListParse : SEQ_PARSE StringType String Instream Outstream *)

structure ListParse: SEQ_PARSE =

(* PARSE AND READ FUNCTIONS FOR LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author



RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:17  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.11  91/03/08  16:59:58  16:59:58  db (Dave Berry)
Fixed a couple of bugs in parseEntries' and readEntries'.

Revision 1.10  91/02/22  20:18:29  20:18:29  db (Dave Berry)
Added Outstream to dependencies list.
Fixed some minor bugs in parseEntries.

Revision 1.9  91/02/22  19:11:04  19:11:04  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.

Revision 1.8  91/02/20  16:05:44  16:05:44  db (Dave Berry)
Added file and fromFile functions.

Revision 1.7  91/02/11  20:09:44  20:09:44  db (Dave Berry)
Changed the name of this structure from List to ListParse.
Removed the inclusion of List'.
This forms part of the major reorganisation of the library.

Revision 1.6  91/02/05  11:05:41  11:05:41  db (Dave Berry)
Changed read functions slightly to use new definition of Instream.eof.

Revision 1.5  91/02/04  15:10:48  15:10:48  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.4  91/01/30  17:42:55  17:42:55  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.3  91/01/25  20:17:23  20:17:23  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:25  17:21:25  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:58:32  14:58:32  db (Dave Berry)
Initial revision


*)

struct

  open EdlibGeneral

  type 'a T = 'a list

  exception Size of string * int

  exception Sep of string * string * string * string

  local
    (* checkSep checks that the starting, finishing and separating symbols
       are either the empty string or a single visible character. *)

    fun checkSep' "" = true
    |   checkSep' s = (String.size s = 1 andalso StringType.isVisible s)

    fun checkSep start finish sep error =
	  if checkSep' start andalso
	     checkSep' finish andalso
	     (checkSep' sep orelse sep = " ")
	  then ()
	  else raise Sep (error, start, finish, sep)
	 
    (* dec is used to decrement the count of the number of elements to be
       parsed.  When ~1 the count is to be ignored; dec ~1 = ~1 to
       avoid any possibility of overflow. *)
    fun dec ~1 = ~1
    |   dec  n = n - 1

    infix 9 sub
    val op sub = String.sub

    (* parseEntries parses the elements of a list in the case that
       finish <> sep and finish <> "".  It assumes that the starting symbol
       (if any) has been parsed and parseFirst has checked for an empty list.
       The list must end with a finishing symbol.  *)
    fun parseEntries 0 _ _ _ s = Fail ([], s)
    |   parseEntries n finish sep p s =
	  if sep = "" orelse sep = " " then
	    ( case p s of
	        Fail _ => Fail ([], s)
	      | OK (x, "") => Fail ([x], "")
	      | OK (x, s') =>
	          if s' sub 0 = finish then
		    if n = 1 orelse n = ~1
		    then OK ([x], String.extract 1 (String.size s') s')
		    else Fail ([x], String.extract 1 (String.size s') s')
	          else
	            case parseEntries (dec n) finish sep p s' of
		      OK (x', s'') => OK (x :: x', s'')
		    | Fail (x', s'') => Fail (x :: x', s'')
	     )
	  else
	    ( case p s of 
	        Fail _ => Fail ([], s)
	      | OK (x, "") => Fail ([x], "")
	      | OK (x, s') =>
		  case String.skipSpaces s' of
		    "" => Fail ([x], "")
		  | s'' =>
		      if s'' sub 0 = sep then
		        case parseEntries (dec n) finish sep p
					  (String.extract 1 (String.size s'') s'') of
		          OK (x', s3) => OK (x :: x', s3)
		        | Fail (x', s3) => Fail (x :: x', s3)
		      else if s'' sub 0 = finish andalso
		              (n = 1 orelse n = ~1)
		      then OK ([x], String.extract 1 (String.size s'') s'')
		      else Fail ([x], s'')
	     )

    (* parseEntries' parses elements in the cases that finish = sep or
       finish = "".  If the first argument is ~1, it parses until the end
       of the input; otherwise it parses that number of elements.  It assumes
       that if sep <> "" then leading whitespace has been skipped before the
       call. *)
    fun parseEntries' 0 _ _ _ s = OK ([], s)
    |   parseEntries' n "" "" p "" =
	  if n = ~1 then OK ([], "") else Fail ([], "")
    |   parseEntries' n "" "" p s =
	  ( case p s of
	      Fail _ => Fail ([], s)
	    | OK (x, "") =>
	        if n = ~1 orelse n = 1 then OK ([x], "")
	        else Fail ([x], "")
	    | OK (x, s') =>
		  case parseEntries' (dec n) "" "" p s' of
		    Fail (x', s'') => Fail (x :: x', s'')
	          | OK (x', s'') => OK (x :: x', s'')
	  )
    |   parseEntries' n "" " " p s =
	  ( case p s of
	      Fail _ => Fail ([], s)
	    | OK (x, s') =>
		case String.skipSpaces s' of
		  "" =>
	            if (n = ~1 orelse n = 1)
	              then OK ([x], "")
	              else Fail ([x], "")
		| s'' =>
	            case parseEntries' (dec n) "" " " p s'' of
	              Fail (x', s3) => Fail (x :: x', s3)
		    | OK (x', s3) => OK (x :: x', s3)
	  )
    |   parseEntries' n finish sep p s =
	 (case p s of
	    Fail _ => Fail ([], s)
	  | OK (x, s') =>
	      case String.skipSpaces s' of
		"" =>
	          if finish = "" andalso (n = ~1 orelse n = 1)
	          then OK ([x], "")
	          else Fail ([x], "")
	      | s'' =>
		  if n = 1 andalso finish = "" then OK ([x], s'')
		  else if s'' sub 0 = sep then
		    let val s3 = (String.extract 1 (String.size s'') s'')
		    in
		      case String.skipSpaces s3 of
			"" => OK ([x], s3)
		      | s4 =>
		          case parseEntries' (dec n) finish sep p s4 of
		            Fail (x', s5) => Fail (x :: x', s5)
		          | OK (x', s5) => OK (x :: x', s5)
		    end
		  else Fail ([x], s'')
	 )
	   
    (* parseFirst is called to parse the first element in the list.  It is
       needed because the list might be empty, so it has to check for a
       finishing symbol.  parseFirst itself just skips leading whitespace
       if neccessary and calls parseFirst'. *)
    fun parseFirst' n finish sep p "" =
	  if finish = "" then
	    if n > 0 then Fail (Some [], "") else OK ([], "")
	  else
	    Fail (Some [], "")
    |   parseFirst' n finish sep p s =
	  if s sub 0 = finish then
	    if n > 0 then Fail (Some [], s)
	    else OK ([], String.extract 1 (String.size s) s)
	  else if n = 0 then Fail (Some [], s)
	  else
	    let val res =
		  if sep = finish orelse finish = ""
		  then parseEntries' n finish sep p s
	  	  else parseEntries n finish sep p s
	    in case res of
		 Fail (l, s') => Fail (Some l, s')
	       | OK x => OK x
	    end

    fun parseFirst 0 "" _ _ s = OK ([], s)
    |   parseFirst n finish "" p s =
	  parseFirst' n finish "" p s
    |   parseFirst n finish sep p s =
	  let val s' = String.skipSpaces s
	  in parseFirst' n finish sep p s'
	  end
    
    (* parseStart is called to parse a starting symbol after leading whitespace
       has been skipped.  The start symbol is known not to be "". *)
    fun parseStart n start finish sep p "" = Fail (Some [], "")
    |   parseStart n start finish sep p s =
          if s sub 0 <> start then Fail (Some [], s)
	  else parseFirst n finish sep p (String.extract 1 (String.size s) s)

    (* parseList is the main parse function that the user visible functions
       call.  It takes the following arguments:
	 n - the integer parameter to parseSepN or parseSepN'.  This is ~1
	     	when parseList is called from parse, parseSep, parse' etc.
	 start - the starting symbol.
	 finish - the finishing symbol.
	 sep - the separating symbol.
		These symbols may be "" or a single printable character.
	 p - the function to parse an element.
	 s - the string to be parsed.
       
       parseList matches cases of the start symbol.  If start = "", it calls
       parseFirst, as if a start symbol had been parsed.  Otherwise it skips
       leading whitespace and calls parseStart to parse the starting symbol. *)

    fun parseList n "" finish sep p s =
	  parseFirst n finish sep p s
    |   parseList n start finish sep p s =
	  let val s' = String.skipSpaces s
	  in parseStart n start finish sep p s'
	  end

    (* readEntries reads the elements of a list in the case that finish <> sep
       and finish <> "".  It assumes that the starting symbol (if any) have
       been read and that readFirst has checked for EOF.  The list must
       end with a finishing symbol. *)
    fun readEntries 0 _ _ _ _ = Fail []
    |   readEntries n finish sep p i =
	  if sep = "" orelse sep = " " then
	  ( case p i of
	      Fail _ => Fail []
	    | OK x =>
		if Instream.eof i then Fail [x]
	        else if Instream.lookahead i = finish then 
		  if n = 1 orelse n = ~1
		  then (Instream.input1 i; OK [x])
		  else Fail [x]
	        else
		  case readEntries (dec n) finish "" p i of
		    OK l => OK (x :: l)
		  | Fail l => Fail (x :: l)
	  )
	  else
	    case p i of
	      Fail _ => Fail []
	    | OK x =>
	        if Instream.eof i then Fail []
	        else
	        ( Instream.skip (not o StringType.isVisible) i;
		  if Instream.eof i then Fail [x]
	          else
		    let val c = Instream.input1 i
		    in if c = sep then
		         case readEntries (dec n) finish sep p i of
		           OK l => OK (x :: l)
		         | Fail l => Fail (x :: l)
	              else if c = finish andalso (n = 1 orelse n = ~1)
		      then OK [x]
	              else Fail [x]
		    end
	        )


    (* readEntries' reads elements of a list in the cases that finish = sep or
       finish = "".  If the first argument is ~1, it parses until the end
       of the input; otherwise it reads that number of elements.  It assumes
       that leading whitespace has been skipped before the call. *)
    fun readEntries' 0 _ _ _ _ = OK []
    |   readEntries' n "" "" p i =
	( if Instream.eof i then
	    if n = ~1 then OK [] else Fail []
	  else
	    case p i of
	      Fail _ =>
		if Instream.eof i andalso n = ~1 then OK [] else Fail []
	    | OK x =>
		if Instream.eof i then
		  if n = 1 orelse n = ~1 then OK [x] else Fail [x]
		else
		  case readEntries' (dec n) "" "" p i of
		    OK l => OK (x :: l)
		  | Fail l => Fail (x :: l)
	)
    |   readEntries' n "" " " p i =
	( if Instream.eof i then
	    if n = ~1 then OK [] else Fail []
	  else
	    case p i of
	      Fail _ =>
		if Instream.eof i andalso n = ~1 then OK [] else Fail []
	    | OK x =>
	        ( Instream.skip (not o StringType.isVisible) i;
		  if Instream.eof i then
		    if n = 1 orelse n = ~1 then OK [x] else Fail [x]
		  else
		    case readEntries' (dec n) "" "" p i of
		      OK l => OK (x :: l)
		    | Fail l => Fail (x :: l)
		)
	)
    |   readEntries' n finish sep p i =
	  if Instream.eof i then
	    if n = ~1 then OK [] else Fail []
	  else
	    case p i of
	      Fail _ =>
		if Instream.eof i andalso n = ~1 then OK [] else Fail []
	    | OK x =>
		if Instream.eof i then
		  if finish = "" andalso (n = 1 orelse n = ~1)
		  then OK [x]
		  else Fail [x]
		else if n = 1 andalso finish = "" then OK [x]
		else
	        ( Instream.skip (not o StringType.isVisible) i;
		  if not (Instream.eof i) then
	            if Instream.lookahead i = sep then
		      if n = 1 then (Instream.input1 i; OK [x])
		      else
	              ( Instream.input1 i;
			Instream.skip (not o StringType.isVisible) i;
			if Instream.eof i then
	    		  if n = ~1 orelse n = 1 then OK [x] else Fail [x]
			else
		          case readEntries' (dec n) finish sep p i of
		            OK l => OK (x :: l)
		          | Fail l => Fail (x :: l)
		      )
	            else Fail [x]
		  else if finish = "" andalso (n = 1 orelse n = ~1)
		  then OK [x]
		  else Fail [x]
		)

    (* readFirst is called to read the first element in the list.  It is
       needed because the list might be empty, so it has to check for a
       finishing symbol.  readFirst itself just skips leading whitespace
       if neccessary and calls readFirst'. *)
    fun readFirst' n finish sep p i =
	( if Instream.lookahead i = finish then
	    if n < 1 then
	      ( if finish <> "" then Instream.input1 i else "";
	        OK []
	      )
	    else Fail (Some [])
	  else if n = 0 then Fail (Some [])
	  else let
	    val res = if finish = "" orelse finish = sep
	  	      then readEntries' n finish sep p i
	  	      else readEntries n finish sep p i
	  in case res of
	       Fail x => Fail (Some x)
	     | OK x => OK x
	  end
	)

    fun readFirst 0 "" _ _ _ = OK []
    |   readFirst n finish "" p i =
          readFirst' n finish "" p i
    |   readFirst n finish sep p i =
	( Instream.skip (not o StringType.isVisible) i;
	  if Instream.eof i then
	    if finish = "" andalso n < 1 then OK [] else Fail (Some [])
	  else
            readFirst' n finish sep p i
	)

    (* start <> "" in readStart *)
    fun readStart n start finish sep p i =
	( Instream.skip (not o StringType.isVisible) i;
	  if Instream.eof i then
	    if finish = "" andalso n < 1 then OK [] else Fail (Some [])
	  else if Instream.lookahead i = start
    	  then (Instream.input1 i; readFirst n finish sep p i)
	  else Fail (Some [])
        )

    fun readList n "" finish sep p i =
	  readFirst n finish sep p i
    |   readList n start finish sep p i =
	  readStart n start finish sep p i
  in
    fun parseSep start finish sep p s =
	( checkSep start finish sep "parseSep";
	  parseList ~1 start finish sep p s
	)

    fun parseSepN start finish sep p n s =
	  if n < 0 then raise Size ("parseSepN", n)
	  else
	  ( checkSep start finish sep "parseSepN";
	    parseList n start finish sep p s
	  )

    fun parse p s = parseSep ("[") ("]") (",") p s

    fun parseN p n s = parseSepN ("[") ("]") (",") p n s

    fun readSep start finish sep p i =
	( checkSep start finish sep "readSep";
	  readList ~1 start finish sep p i
	)

    fun readSepN start finish sep p n i =
	  if n < 0 then raise Size ("readSepN", n)
	  else
	  ( checkSep start finish sep "readSepN";
	    readList n start finish sep p i
	  )
	  
    fun read p i = readSep ("[") ("]") (",") p i
	  
    fun readN p n i = readSepN ("[") ("]") (",") p n i
  end

  fun fromFile p name =
        let fun readList i =
                  case p i
                  of Fail _ => (Instream.closeIn i; [])
                  |  OK x => x :: readList i
        in readList (Instream.openIn name)
        end

  fun file p v name =
        let val os = TextIO.openOut name
        in TextIO.output (os, List.string p v);
           TextIO.closeOut os
        end

end
