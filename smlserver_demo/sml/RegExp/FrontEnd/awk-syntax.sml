(* awk-syntax.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This module implements the AWK syntax for regular expressions.  The
 * syntax is defined on pp. 28-30 of "The AWK Programming Language,"
 * by Aho, Kernighan and Weinberger.
 *
 * The meta characters are:
 *	"\" "^" "$" "." "[" "]" "|" "(" ")" "*" "+" "?"
 *    Atomic REs:
 *      c	matches the character c (for non-metacharacters c)
 *      "^"	matches the empty string at the beginning of a line
 *	"$"	matches the empty string at the end of a line
 *      "."	matches any single character (except \000 and \n)
 *
 *    Escape sequences:
 *	"\b"	matches backspace
 *	"\f"	matches formfeed
 *	"\n"	matches newline (linefeed)
 *	"\r"	matches carriage return
 *	"\t"	matches tab
 *	"\"ddd	matches the character with octal code ddd.
 *	"\"c	matches the character c (e.g., \\ for \, \" for ")
 *      "\x"dd  matches the character with hex code dd.
 *
 *    Character classes:
 *
 *    Compound regular expressions:
 *	A"|"B	matches A or B
 *	AB	matches A followed by B
 *	A"?"	matches zero or one As
 *	A"*"	matches zero or more As
 *	A"+"	matches one or more As
 *	"("A")"	matches A
 *)

structure AwkSyntax : REGEXP_PARSER =
  struct

    structure R = RegExpSyntax

    structure SC = StringCvt
    structure W8 = Word8
    structure C = Char

    val isMeta = C.contains "\\^$.[]|()*+?"

    exception Error

    val dotMatch = R.NonmatchSet (R.CharSet.addList (R.CharSet.empty,explode "\000\n"))

    fun scan getc cs = let
	  fun getc' cs = (case (getc cs)
			      of NONE => raise Error
			    | (SOME arg) => arg)
	  (* end case *)
	  fun isOctDigit c = (#"0" <= c) andalso (c <= #"7")
	  fun returnVal (v,cl,cs) = 
	      let val (n,_) = valOf (Int.scan v List.getItem cl)
	      in
		  (C.chr n, cs) handle _ => raise Error
	      (* SC.scanString (Int.scan SC.OCT) (implode [c1,c2,c3]) *)
	      end
	  fun getHexChar (c,cs) = (case (getc cs)
                 of NONE => returnVal (SC.HEX,[c],cs)
	       | SOME (c',cs') => 
		   if not (C.isHexDigit c') then returnVal (SC.HEX,[c],cs)
		   else returnVal (SC.HEX,[c,c'],cs'))
	  fun getOctalChar (c,cs) = (case (getc cs)
		 of NONE => returnVal (SC.OCT,[c],cs)
	          | SOME (c',cs') => 
		      if not (isOctDigit c') then returnVal (SC.OCT,[c],cs)
		      else (case (getc cs')
			of NONE => returnVal (SC.OCT,[c,c'],cs')
		         | SOME (c'',cs'') => 
			     if not (isOctDigit c'') then returnVal (SC.OCT,[c,c'],cs')
			     else returnVal (SC.OCT,[c,c',c''],cs'')))
	  fun getEscapeChar cs = (case (getc' cs)
		 of (#"b", cs) => (#"\008", cs)
		  | (#"f", cs) => (#"\012", cs)
		  | (#"n", cs) => (#"\n", cs)
		  | (#"r", cs) => (#"\013", cs)
		  | (#"t", cs) => (#"\t", cs)
		  | (#"x", cs) => let val (c1,cs) = getc' cs
				  in
				      if (C.isHexDigit c1) then getHexChar (c1,cs) else raise Error
				  end
		  | (c1, cs) =>
       		      if (isOctDigit c1) then getOctalChar (c1,cs) else (c1, cs))
	  fun scanAlt (stk, cs) = let
		val (re, cs') = scanSeq([], cs)
		in
		  case (stk, getc cs')
		   of ([], NONE) => (re, cs')
		    | (_, SOME(#"|", cs'')) => scanAlt(re::stk, cs'')
		    | _ => (R.Alt(rev(re::stk)), cs')
		  (* end case *)
		end
	  and scanSeq (stk, cs) = let
		fun continue (re, cs') = scanSeq(re::stk, cs')
		fun done () = (R.Concat(rev stk), cs)
		in
		  case (stk, getc cs)
		   of ([], NONE) => raise Error
		    | ([re], NONE) => (re, cs)
		    | (_, NONE) => done()
		    | (re::r, SOME(#"?", cs')) => scanSeq (R.Option re :: r, cs')
		    | (re::r, SOME(#"*", cs')) => scanSeq (R.Star re :: r, cs')
		    | (re::r, SOME(#"+", cs')) => scanSeq (R.Plus re :: r, cs')
		    | (_, SOME(#"|", _)) => done()
		    | (_, SOME(#")", _)) => done()
		    | (_, SOME(#"(", cs')) => continue(scanGrp cs')
		    | (_, SOME(#".", cs')) => continue(dotMatch, cs')
		    | (_, SOME(#"^", cs')) => continue(R.Begin, cs')
		    | (_,SOME(#"$",cs')) => continue(R.End, cs')
		    | (_, SOME(#"[", cs')) => continue(scanClass cs')
		    | (_, SOME(#"\\", cs')) => continue(scanEscape cs')
		    | (_, SOME(c, cs')) => if (isMeta c)
			then raise Error
			else scanSeq((R.Char c)::stk, cs')
		  (* end case *)
		end
	  and scanGrp cs = let
		val (re, cs') = scanAlt ([], cs)
		in
		  case (getc' cs')
		   of (#")", cs'') => (R.Group re, cs'')
		    | _ => raise Error
		  (* end case *)
		end
	  and scanClass cs = let
		fun scanClass' cs = let
		      fun scanRange1 (set, cs) = (case (getc' cs)
			     of (#"]", cs) => (set,cs)
			      | (#"\\", cs) => let
				  val (c, cs) = getEscapeChar cs
				  in
				    scanRange2 (set, c, cs)
				  end
			      | (c, cs) => scanRange2 (set, c, cs)
			    (* end case *))
		      and scanRange2 (set, c, cs) = (case (getc' cs)
			     of (#"]", cs) => (R.CharSet.add(set, c), cs)
			      | (#"\\", cs) => let
				  val (c', cs) = getEscapeChar cs
				  in
				    scanRange2 (R.CharSet.add(set, c), c', cs)
				  end
			      | (#"-", cs) => scanRange3 (set, c, cs)
			      | (c', cs) =>
				  scanRange2 (R.CharSet.add(set, c), c', cs)
			    (* end case *))
		      and scanRange3 (set, minC, cs) = (case (getc' cs)
			     of (#"]", cs) =>
				  (R.CharSet.add(R.CharSet.add(set, minC), #"-"), cs)
			      | (#"\\", cs) => let
				  val (c, cs) = getEscapeChar cs
				  in
				    chkRange(set, minC, c, cs)
				  end
			      | (c, cs) => chkRange(set, minC, c, cs)
			    (* end case *))
		      and chkRange (set, minC, maxC, cs) =
			    if (minC > maxC)
			      then scanRange1 (set,cs )  (*raise Error *) (* as per bwk test suite *)
			      else scanRange1 (R.addRange (set,minC,maxC),cs) (*R.CharSet.addList (set,List.tabulate (ord(maxC)-ord(minC)+1,fn v => chr (v+ord(minC)))), cs) *)
		      in
			case (getc' cs)
			 of (#"-", cs) =>
			      scanRange1(R.CharSet.add(R.CharSet.empty, #"-"), cs)
		          | (#"]", cs) => 
			      scanRange2(R.CharSet.empty, #"]", cs)  (* as per bwk test suite *)
			  | _ => scanRange1(R.CharSet.empty, cs)
			(* end case *)
		      end
		in
		  case (getc' cs)
		   of (#"^", cs) => let
			val (set, cs) = scanClass' cs
			in
			  (R.NonmatchSet set, cs)
			end
		    | _ => let
			val (set, cs) = scanClass' cs
			in
			  (R.MatchSet set, cs)
			end
		  (* end case *)
		end
	  and scanEscape cs = let val (c, cs) = getEscapeChar cs
		in
		  (R.Char c, cs)
		end
	  in
	    SOME(scanAlt([], cs)) handle Error => NONE
	  end


  end (* AWK_RE_Syntax *)
