(*$LexBasics: BASIC_IO REPORT PRETTYPRINT FLAGS CRASH LEX_BASICS*)
functor LexBasics(structure BasicIO: BASIC_IO
		  structure Report: REPORT
		  structure PP: PRETTYPRINT
		    sharing type PP.Report = Report.Report
		  structure Flags: FLAGS
		  structure Crash: CRASH
		 ): LEX_BASICS =
  struct

    open Edlib OldIO OldString
    open General 

    datatype pos = POSITION of unit -> {file: string, line: int, column: int,
					getLine: int -> string
				       }
		 | DUMMY
    

    datatype SourceReader =
	SOURCE_READER of {name: string,
			  clearFn: unit -> unit,
			  lexingFn: int -> string,
			  positionFn: int -> pos
			 }

   (* We store an entire file (or the entire source text string) as a list
      of lines, paired with the absolute character position of the first
      character of the line (counting from 0). All the lines have a "\n" at
      the end, except the last. For ordinary text files, the last line will
      be "". For weirder files (with EOF in the middle of the line), it
      will be some text without a "\n". Oh: we must remove tabs in here
      (see also lexFromStdIn). *)

    datatype SourceText =
      SOURCE_TEXT of {filename: string,
		      lines: {absCharacter: int, line: string} list
		     }
		      
   (* Reports, diags etc. *)

    type Report = Report.Report

    infix //
    val op // = Report.//

    local
      fun spaces n =
	if n<0 then (output(std_out,
			    "\n\n       ************ LexBasics.spaces: n<0  ************  \n\n\n");
		     raise String.Size ("Alleluja, the Lord is come", 42))
	else String.create n " "

      fun untabifyL(col, "\t" :: rest) =
	    let
	      val gap = 8 - col mod 8
	    in
	      spaces gap :: untabifyL(col + gap, rest)
	    end

	| untabifyL(col, x :: rest) =
	    x :: untabifyL(col + 1, rest)

	| untabifyL(_, nil) = nil
    in
      fun untabify indent line = implode(untabifyL(indent, explode line))
    end

    fun stringToSourceText(filename, string) =
      let
	fun process "" = [""]
	  | process string =
	      case String.search String.MatchCase "\n" string 0
		of OK n =>
		     String.extract 0 (n+1) string (* Include `\n'. *)
		     :: process(String.extract (n+1) (size string) string)

		 | Fail() =>
		     [string]

	val lines = map (untabify 0) (process string)

	fun withAbsCharacter(n, (x :: xs)) =
	      {absCharacter=n, line=x} :: withAbsCharacter(n+size x, xs)
	  | withAbsCharacter(_, nil) = nil
      in
	SOURCE_TEXT{filename=filename, lines=withAbsCharacter(0, lines)}
      end


   (* I suspect doing that for an entire file might be rather painful. So,
      we read a file a chunk at a time and build a sequence of SourceTexts.
      Therefore, we need a SourceText concatenation function. *)

    fun bumpChars(n: int, {absCharacter, line} :: rest) =
          {absCharacter=absCharacter+n, line=line} :: bumpChars(n, rest)
      | bumpChars(_, nil) = nil

    fun concat(SOURCE_TEXT{filename, lines=list1},
	       SOURCE_TEXT{lines=list2, ...}
	      ) =
      case (rev list1, list2)
	of ({absCharacter=lastChar, line=lastLine} :: revFirstLines,
	    {line=firstLine2, ...} :: restLines
	   ) =>
	     let
	       val joinedLine =
		 {absCharacter=lastChar, line=lastLine ^ firstLine2}
	     in
	       SOURCE_TEXT{
		 filename=filename,
		 lines=rev revFirstLines
		       @ (joinedLine
			  :: bumpChars(lastChar + size lastLine, restLines)
			 )
	       }
	     end

	 | _ => Crash.impossible "LexBasics.concat"

    val BUFFER_SIZE = 1024		(* Say... *)

    fun fileToSourceText file =
      let
	val stream = BasicIO.open_in file	(* Io raised here gets dealt
						   with in any calling
						   context properly. *)

	val _ = BasicIO.println("[reading source file:\t" ^ file ^ "]")

	fun read() =
	  let
	    val text = BasicIO.input(stream, BUFFER_SIZE)
	    val st = stringToSourceText(file, text)
	  in
	    if size text = BUFFER_SIZE
	    then concat(st, read())
	    else st
	  end

	val st = read()
      in
	BasicIO.close_in stream; st
      end

   (* `positionFn' is suspended, for efficiency. It's used in
      lexFromSourceText (to turn a completed SourceText into a position
      function), and we also use it when reading interactively and
      making up a SourceText as we go. *)

    fun positionFn(sourceTextRef, absPos) =
					(* Must start from very top... *)
      let
	fun pr n =
	  let
	    val ref(SOURCE_TEXT{lines, ...}) = sourceTextRef
	  in
	    Report.print(
	      Report.decorate("getLine " ^ Int.string n ^ ": ",
			      List.foldR (fn {line, ...} =>
					    fn rep =>
					      Report.line line // rep
					 ) Report.null lines
			     )
	    )
	  end

	fun getLine n =
	  let
	    val ref(SOURCE_TEXT{lines, ...}) = sourceTextRef
	  in
	    if !Flags.DEBUG_ERRORPRINT then pr n else ();
	    #line(List.nth (n-1) lines)
	  end
      in
	POSITION(fn () =>
	  let
	    val _ =
	      if !Flags.DEBUG_ERRORPRINT then
		 BasicIO.println("positionFn(absPos=" ^ Int.string absPos ^ ")")
	      else ()

	    val ref(SOURCE_TEXT{filename, lines}) = sourceTextRef

	    fun search(n, previousLine, (L as {absCharacter, line}) :: rest) =
		  if absPos < absCharacter
				(* The next line in the list is beyond
				   this character position, so we must want
				   the previous line. *)
		  then
		    case previousLine
		      of SOME{absCharacter, line} =>
			   {file=filename, line=n,
			    column=absPos-absCharacter,
			    getLine=getLine
			   }

		       | NONE =>
			   Crash.impossible "LexBasics.search(previous/1)"
		  else
		    search(n+1, SOME L, rest)

	      | search(n, previousLine, nil) =
				(* No more lines, so we must want the
				   previous line. *)
		  (case previousLine
		     of SOME{absCharacter, line} =>
			  {file=filename, line=n,
			   column=absPos-absCharacter,
			   getLine=getLine
			  }

		      | NONE =>
			  Crash.impossible "LexBasics.search(previous/2)"
		  )
	  in
	    search(0, NONE, lines)
	  end
	)
      end

    fun lexFromSourceText(source as SOURCE_TEXT{filename, lines=allLines}) =
      let
	val theLines = ref allLines

	fun lexingFn _ =
	  case !theLines
	    of nil => ""
	     | {line, ...} :: rest => (theLines := rest; line)
      in
	SOURCE_READER{name=filename,
		      clearFn=fn () => (),
		      lexingFn=lexingFn,
		      positionFn=fn absPos => positionFn(ref source, absPos)
		     }	(* When we're lexing from a source file, we don't have
			   the `clearFn' functionality to reset the list of
			   input lines - it's too much hassle to implement.
			   However, errors during file read are fatal anyway,
			   so it's not important. Not that `clearFn' mustn't
			   panic: it gets called right at the start of any
			   parse, even for file/string input.
			     The ref here is superfluous; positionFn takes
			   a ref for handling incremental input properly. *)
      end

    val lexFromString =
      lexFromSourceText o (fn text => stringToSourceText("<string>", text))

    val lexFromFile =
      lexFromSourceText o fileToSourceText


   (* lexFromStdIn is more complex. (Yes: even more complex.) We are lexing as
      we go along and building a SourceText line by line. Oh, it's here that
      we get to eliminate tabs as well. They're nuked as we read the lines of
      source, which saves us the hassle of dealing with them later. Don't you
      dare use other than 8-character tabs. *)

    val prompt = ". "

    fun lexFromStdIn() =
      let
	val name = "std_in"
	val theSource = ref(SOURCE_TEXT{filename=name, lines=nil})

	fun getLine() =
	  let
	   (* Prompt and get the line: *)
	    val _ = BasicIO.print prompt

	    val line =
	      untabify (String.size prompt) (TextIO.inputLine std_in)

	   (* The lines (and character positions) that we've got so far,
	      in reverse order (latest at front of list): *)
	    val oldLines =
	      case !theSource of SOURCE_TEXT{lines, ...} => rev lines

	   (* Absolute position of the new line: *)
	    val absPos =
	      case oldLines
		of nil => 0
		 | {absCharacter, line} :: _ => absCharacter + size line

	    val _ =
	      if !Flags.DEBUG_ERRORPRINT then
		BasicIO.println("absPos(" ^ String.string line ^ ") = "
				^ Int.string absPos
			       )
	      else ()

	   (* The new list of lines: *)
	    val newLines = rev({absCharacter=absPos, line=line} :: oldLines)

	   (* Update the SourceText: *)
	    val _ = (theSource := SOURCE_TEXT{filename=name, lines=newLines})
	  in
	    line
	  end
      in
	SOURCE_READER{
	  name=name,
	  clearFn=fn () => (theSource := SOURCE_TEXT{filename=name, lines=nil}),
	  lexingFn=fn _ => getLine(),
	  positionFn=fn absPos => positionFn(theSource, absPos)
	}
      end

   (* highlight underlines part of a line. Oh, the line will probably
      still carry the trailing `\n'. *)

    fun highlight(line, column, width) =
      let
	val _ =
	  if !Flags.DEBUG_ERRORPRINT then
	    BasicIO.println("highlight " ^ String.string line
			    ^ ", " ^ Int.string column
			    ^ " =-> " ^ Int.string width
			   )
	  else
	    ()

	val line = String.dropR "\n" line
	val width = if width <= 0 then 1 else width
				(* Eliminate any 0-width error fields. *)
      in
	Report.indent(
	  String.size prompt,
	     Report.line line
	  // Report.line(String.create column " " ^ String.create width "^")
	)
      end handle String.Size _ =>
	Crash.impossible "LexBasics.highlight: width or column <0"


    type StringTree = PP.StringTree

    fun layoutPos (POSITION f) =
	  let
	    val {file, line, column, getLine} = f ()
	  in
	    PP.NODE {start="POSITION(", finish=")",
		     indent=3, childsep=PP.RIGHT ",",
		     children=[PP.LEAF("\"" ^ file ^ "\""),
			       PP.LEAF("line " ^ Int.string line),
			       PP.LEAF("column " ^ Int.string column),
			       PP.LEAF("\"" ^ getLine line ^ "\"")]}
	  end
      | layoutPos DUMMY = PP.LEAF "DUMMY"

    fun reportPosition {left=POSITION posLfn, right=POSITION posRfn} =
          let
	    val {file, line=line1, column=column1, getLine} = posLfn()
	    val {line=line2, column=column2, ...} = posRfn()
	          (*`file' and `getLine' had better match.*)
	    val theLine1 = getLine line1
	    val theLine2 = getLine line2
	    val len1 = String.size theLine1 - 1	(* remove `\n'. *)
	    val len2 = String.size theLine2 - 1

	    fun highlightAll(fromL, toL) =
		  if fromL > toL then Report.null
		  else let val line = getLine fromL
			   val width = String.size line - 1
		       in
		         highlight (line, 0, width)
			 // highlightAll (fromL + 1, toL)
		       end
	  in
	    Report.line (file ^ ", line " ^ Int.string line1
			 ^ ", column " ^ Int.string column1 ^ ":")
	    // (if line1 = line2 then
		  highlight (theLine1, column1, column2-column1)
		else
		  highlight (theLine1, column1, len1-column1)
		  // (*old:  highlightAll(line1+1, line2-1) *)
		  let val lower = line1+1  (* omit lengthy underlinings *)
		      val upper = line2-1
		  in
		    if upper - lower > 5 (*was: 3*)
		    then Report.line "      (text omitted)"
		    else highlightAll (lower,upper)
		  end
		// highlight (theLine2, 0, column2))
	  end
      | reportPosition {left=pos1, right=pos2} =
	  Report.line "(position unknown (*4*)): "
	  // PP.reportStringTree (layoutPos pos1)
	  // PP.reportStringTree (layoutPos pos2)

    fun output_source {os: outstream, left=POSITION posLfn, right=POSITION posRfn} : unit =
      let val {file, line=line1:int,column=column1:int, getLine} = posLfn()
	  val {line=line2:int, column=column2:int, ...} = posRfn()
	    
	    (* If the first character is not `:' or `=' (it shows!!) then we emit `='. *)
	  fun patch s = case explode s
			     of [] => Crash.impossible "LexBasics.output_source"
			      | ":" :: _ => s
	                      | "=" :: _ => s
			      | _ => " = " ^ s

      in if line1 = line2 then 
	   let val line = getLine line1
	   in output(os,patch(String.extract column1 column2 line))
	   end
	 else 
	   let val first = getLine line1
	       val first = patch(String.extract column1 (size first) first)
	       val last = getLine line2
	       val last = String.extract 0 column2 last
	       fun get_lines (l, lines) = 
		 if l = line1 then first :: lines
		 else get_lines(l-1,getLine l :: lines)
	   in List.apply (fn s => output(os,s)) (get_lines(line2-1,[last]))
	   end
      end
      | output_source {os,...} = output(os,"*** POSITION UNKNOWN ***")

    val FIELD_WIDTH = 18

    fun pad str = if size str mod 18 <> 0 then pad(str ^ " ") else str

    fun thinking x = () (*BasicIO.print(pad(x ^ " "))*)
    fun shifting x = () (*BasicIO.print(pad("S." ^ x ^ " "))*)
    fun reducing x = () (*BasicIO.print(pad("R." ^ x ^ " "))*)

    exception LEXICAL_ERROR of pos * string
			(* Used to signal lexical errors to the parser. *)
  end;
