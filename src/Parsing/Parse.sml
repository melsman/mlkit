(* The interface to the parser that the rest of the Kit Compiler sees. We hide
   all the ugly MLLex and MLYacc implementation details here. Parse() depends
   on the base support modules for MLYacc (MyBase.sml), and my support functors
   - these are the functors that are referred to freely. *)

structure Parse: PARSE =
  struct
    structure Stream = Stream()

    structure LrParser = ParserGen(structure LrTable = LrTable()
                                   structure Stream = Stream)

    structure TopdecLrVals = TopdecLrVals(LrParser.Token)

    structure LexUtils = LexUtils(TopdecLrVals.Tokens)

    structure Infixing = Infixing

    structure TopdecLex =
      TopdecLex(structure Tokens = TopdecLrVals.Tokens
                structure LexUtils = LexUtils)

    structure TopdecParser =
      JoinWithArg(structure ParserData = TopdecLrVals.ParserData
                  structure Lex = TopdecLex
                  structure LrParser = LrParser)

    structure TopdecGrammar = PreElabTopdecGrammar

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
          (PS_EOF
          )
        else if sameToken(firstToken, sc)
        then
          (parseStream(STATE rest)
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
             PS_SUCCESS(topdec, STATE lazyStream')
           end
          ) handle ESCAPE (text, (lPos, rPos)) =>
                     PS_ERROR (LexBasics.reportPosition {left=lPos, right=rPos}
			       // Report.line text)
      end

    type topdec = TopdecGrammar.topdec
    type InfixBasis = Infixing.InfixBasis
    type SourceReader = LexBasics.SourceReader

(*    val sourceFromStdIn = LexBasics.lexFromStdIn *)
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

    val explicit_regions = LexUtils.explicit_regions

    fun parse(ib, state) =
      (case parseStream state of
	 PS_SUCCESS(topdec, state') =>
	   (case Infixing.resolve(ib, topdec) of
	      Infixing.SUCCESS (ib', topdec') => SUCCESS(ib', topdec', state')
	    | Infixing.FAILURE report => ERROR report)
       | PS_ERROR report => ERROR report
       | PS_EOF => LEGAL_EOF)
  end;
