(*$PairParse : PAIR_PARSE String Instream *)

structure PairParse: PAIR_PARSE =


(* FUNCTIONS FOR PARSING PAIRS.

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk

Date:		6 Feb 1991

Maintenance:	Author


NOTES

   These functions were taken from the Pair structure.


SEE ALSO

   Pair.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:27  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  20:32:49  20:32:49  db (Dave Berry)
Initial revision


*)

struct
  
  open EdlibGeneral
  open OldString
  open OldIO


(* CONVERTERS *)

  fun parse p1 p2 s =
	  let val s' = String.skipSpaces s
	  in if "(" <> String.sub (s', 0)
	     then Fail (None, s')
	     else
	       case p1 (String.extract 1 (size s') s') of
	         Fail (_, s'') => Fail (None, s'')
	       | OK (x, s'') =>
		   let val s3 = String.skipSpaces s''
		   in if "," <> String.sub (s3, 0)
		      then Fail (None, s3)
		      else
		        case p2 (String.extract 1 (size s3) s3) of
			  Fail (_, s4) => Fail (None, s4)
	                | OK (y, s4) =>
			    let val s5 = String.skipSpaces s4
			    in if ")" = String.sub (s5, 0)
			       then OK ((x, y), String.extract 1 (size s5) s5)
			       else Fail (None, s5)
			    end 
		   end
	  end


  fun read p1 p2 i =
	( Instream.skip (not o StringType.isVisible) i;
	  if Instream.eof i then Fail () else
	  if Instream.input1 i <> "(" then Fail () else
	  case p1 i of
	    Fail _ => Fail ()
	  | OK x =>
	    ( Instream.skip (not o StringType.isVisible) i;
	      if Instream.eof i then Fail () else
	      if Instream.input1 i <> "," then Fail () else
	      case p2 i of
	        Fail _ => Fail ()
	      | OK y =>
	        ( Instream.skip (not o StringType.isVisible) i;
	          if Instream.eof i then Fail () else
	          if Instream.input1 i <> ")" then Fail () else
		  OK (x, y)
		)
	    )
	)
end

