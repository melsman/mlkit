(* General lexing utilities. *)

signature LEX_UTILS =
  sig
    type LexArgument
    type pos
    type svalue
    type ('a, 'b) token

   (* The stream type from LexBasics is carried around in the lexing
      argument. This is because the SourceReader is the thing which can
      map absolute positions (as delivered by yypos) into line/column
      information, plus filename and source text. *)

    type SourceReader
    val sourceReaderOf: LexArgument -> SourceReader

    val isQualStar: string -> bool
    val asQualId: string -> string list
    val asDigit: string -> int
    val asInteger: string -> Int32.int option
    val asWord: string -> Word32.word option
    val asReal: string -> string option

    val initArg: SourceReader -> LexArgument
    val clearString: LexArgument -> LexArgument
    val newComment: LexArgument -> LexArgument

    val addChars: string -> LexArgument -> LexArgument
    val addControlChar: string -> LexArgument -> LexArgument
    val addAsciiChar: (pos * string) -> LexArgument -> LexArgument
    val addUnicodeChar: (pos * string) -> LexArgument -> LexArgument

    val asString: LexArgument -> string

    val identifier: string * pos * pos -> (svalue, pos) token

    val incComment: LexArgument -> LexArgument
    val decComment: LexArgument -> int * LexArgument
  end;
