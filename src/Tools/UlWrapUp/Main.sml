
structure ULLrVals = ULLrValsFun(structure Token = LrParser.Token)
structure ULLex = UlLexFun(structure Tokens = ULLrVals.Tokens)

structure UlParser = Join(structure ParserData = ULLrVals.ParserData
                          structure Lex = ULLex
                          structure LrParser = LrParser);

fun createLexerStream (is : TextIO.instream) =
  LilyGrammarParser.makeLexer ((fn _ => TextIO.input is) handle 
                    IO.Io {cause = OS.SysErr(c,_), name = name, ...} => 
                        raise Fail (String.concat
                                     ["\n! IO Error, tried reading file: ",
                                      name, ",\n! but got the error: ", c, "\n"]))

fun parse file =
    let val is     = TextIO.openIn file
        val lexbuf = createLexerStream is
        val (expr,lb) =
                   (LilyGrammarParser.parse (0,lexbuf, fn (s,p1,p2) =>
                        List.app print [s, " ", Int.toString p1,
                                        ",", Int.toString p2], ()))
                   handle exn => (TextIO.closeIn is; raise exn)
    in
      TextIO.closeIn is; expr
    end

