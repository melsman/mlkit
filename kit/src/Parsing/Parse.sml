(* The interface to the parser that the rest of the Kit Compiler sees. We hide
   all the ugly MLLex and MLYacc implementation details here. Parse() depends
   on the base support modules for MLYacc (MyBase.sml), and my support functors
   - these are the functors that are referred to freely. *)

functor Parse (structure TopdecGrammar : TOPDEC_GRAMMAR
	       structure LexBasics : LEX_BASICS
	       structure ParseInfo : PARSE_INFO
	       sharing type ParseInfo.SourceInfo.pos = LexBasics.pos
	       sharing type TopdecGrammar.info = ParseInfo.ParseInfo
	       structure InfixBasis : INFIX_BASIS
               sharing type InfixBasis.id = TopdecGrammar.DecGrammar.Ident.id
	       sharing type InfixBasis.Basis = ParseInfo.DFInfo.InfixBasis
               structure Report: REPORT
               sharing type LexBasics.Report
		           = ParseInfo.SourceInfo.Report
		           = InfixBasis.Report
		           = Report.Report
               structure PrettyPrint: PRETTYPRINT
               sharing type TopdecGrammar.StringTree = PrettyPrint.StringTree
		  = LexBasics.StringTree
	       sharing type PrettyPrint.Report = Report.Report
               structure FinMap: FINMAP
               structure BasicIO: BASIC_IO
               structure Flags: FLAGS
               structure Crash: CRASH
		) : PARSE =
  struct
    structure Stream = Stream()

    structure LrParser = ParserGen(structure LrTable = LrTable()
                                   structure Stream = Stream
                                  )

    structure GrammarUtils =
      GrammarUtils (structure TopdecGrammar = TopdecGrammar
		    structure LexBasics = LexBasics
		    structure ParseInfo = ParseInfo
		    structure Report = Report
		    structure PrettyPrint = PrettyPrint
		    structure Crash = Crash
		      )

    structure TopdecLrVals =
      TopdecLrVals(structure Token = LrParser.Token
                   structure LexBasics = LexBasics
                   structure GrammarUtils = GrammarUtils
                  )

    structure LexUtils = LexUtils(structure LexBasics = LexBasics
                                  structure Token = TopdecLrVals.Tokens
                                  structure BasicIO = BasicIO
                                  structure Flags = Flags
                                  structure Crash = Crash
                                 )

    structure Infixing = Infixing(structure InfixBasis = InfixBasis
                                  structure GrammarUtils = GrammarUtils
				  structure ParseInfo = ParseInfo
				  structure Report = Report
                                  structure PP = PrettyPrint
                                  structure Crash = Crash
                                 )

    structure TopdecLex =
      TopdecLex(structure Tokens = TopdecLrVals.Tokens
                structure LexBasics = LexBasics
                structure LexUtils = LexUtils
               )

    structure TopdecParser =
      JoinWithArg(structure ParserData = TopdecLrVals.ParserData
                  structure Lex = TopdecLex
                  structure LrParser = LrParser
                 )


    val eof = TopdecLrVals.Tokens.EOF(LexBasics.DUMMY, LexBasics.DUMMY)
    val sc = TopdecLrVals.Tokens.SEMICOLON(LexBasics.DUMMY, LexBasics.DUMMY)

    type Report = Report.Report
    infix //
    val op // = Report.//

    exception ESCAPE of string * (LexBasics.pos * LexBasics.pos)

   (* The state of a parser, holding the current token stream between
      top-level phrases (needed to handle "val x = 1; val y = 2;" without
      losing the end of the line. *)

    local
      open TopdecLrVals
    in
      datatype State =
        STATE of (Tokens.svalue, LexBasics.pos) Tokens.token Stream.stream
                        (* God I HATE that parser generator... *)
    end

   (* result type for `parseStream' (as opposed to `parse'): *)

    datatype PSResult = PS_SUCCESS of TopdecGrammar.topdec * State
                      | PS_ERROR of Report
                      | PS_EOF

    (* For profiling *)
    fun sameToken a = LrParser.Token.sameToken a
    fun Stream_get a = Stream.get a
    fun Stream_cons a = Stream.cons a
    fun TopdecParser_parse a = TopdecParser.parse a

    fun parseStream(STATE lazyStream) =
      let
        val (firstToken, rest) = Stream_get lazyStream
        val lazyStream = Stream_cons(firstToken, rest)
                                      (* Streams side-effect (yuck). *)
      in
        if sameToken(firstToken, eof)
        then
          (if !Flags.DEBUG_PARSING then BasicIO.println "**EOF**" else ();
           PS_EOF
          )
        else if sameToken(firstToken, sc)
        then
          (if !Flags.DEBUG_PARSING then BasicIO.println "**SC**" else ();
           parseStream(STATE rest)
          )
        else
          (let
             val (topdec, lazyStream') =
               TopdecParser_parse(0, lazyStream,
                                  fn (x, l, r) => raise ESCAPE (x, (l, r)),
                                  ()
                                 )
               handle LexBasics.LEXICAL_ERROR (pos, msg) =>
                        raise ESCAPE (msg, (pos, pos))
                    | GrammarUtils.LAYERPAT_ERROR positions =>
                        raise ESCAPE ("Bad layered pattern", positions)
           in
             if !Flags.DEBUG_PARSING
             then BasicIO.println "**SUCCESS**"
             else ();
             PS_SUCCESS(topdec, STATE lazyStream')
           end
          ) handle ESCAPE (text, (lPos, rPos)) =>
                     PS_ERROR (LexBasics.reportPosition {left=lPos, right=rPos}
			       // Report.line text)
      end

    type topdec = TopdecGrammar.topdec
    type InfixBasis = Infixing.InfixBasis
    type SourceReader = LexBasics.SourceReader

    val sourceFromStdIn = LexBasics.lexFromStdIn
    val sourceFromFile = LexBasics.lexFromFile (*may raise Io s*)
    val sourceFromString = LexBasics.lexFromString

    fun nameOf(LexBasics.SOURCE_READER{name, ...}) = name

    datatype Result = SUCCESS of InfixBasis * topdec * State
                    | ERROR of Report
                    | LEGAL_EOF

    fun begin sourceReader =
      let
        val LexBasics.SOURCE_READER{clearFn, lexingFn, ...} = sourceReader
        val _ = clearFn()               (* Forget any stored lines *)

        val lex_fn =
          TopdecLex.makeLexer lexingFn (LexUtils.initArg sourceReader)

        val stream = Stream.streamify lex_fn
      in
        STATE stream
      end

    fun parse(ib, state) =
      (case parseStream state of
	 PS_SUCCESS(topdec, state') =>
	   (case Infixing.resolve(ib, topdec) of
	      Infixing.SUCCESS (ib', topdec') => SUCCESS(ib', topdec', state')
	    | Infixing.FAILURE report => ERROR report)
       | PS_ERROR report => ERROR report
       | PS_EOF => LEGAL_EOF)
  end;
