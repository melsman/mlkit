(* Lexing basics. *)

signature LEX_BASICS =
  sig
   (* Loads of horrible side-effecting stuff to keep track of the source
      position. Don't blame me. It's all the fault of ML-Lex and ML-Yacc. *)

   (* OK, let's try to be structured here. The type `pos' (required to
      be that identifier by JoinWithArg()) contains filename, line number,
      column number and a function for getting lines of text (which we need
      because we might want to print out some surrounding context, for example).
      Whenever we start reading a file or string, we build a `SourceReader'
      which contains the function required by ML-Lex, and also contains an
      enquiry function. *)

    datatype pos = POSITION of unit -> {file: string, line: int, column: int,
					getLine: int -> string
				       }
		 | DUMMY	(* The suspension in POSITION is to avoid a
				   lot of calculation for every token lexed.
				   We don't mind paying the price when the
				   position needs to be printed. *)


    datatype SourceReader =
      SOURCE_READER of {name: string,
			clearFn: unit -> unit,
				(* Clear the remembered lines. Needed if we
				   rebuild the lexing function since yypos
				   will then start again from 0. *)
			lexingFn: int -> string,
			positionFn: int -> pos
		       }	(* positionFn takes yypos which is the
				   absolute character position. Unless we
				   pass characters one at a time, we don't
				   actually know the real lexing position. *)

    val lexFromFile: string -> SourceReader (*may raise Io s*)
    val lexFromString: string -> SourceReader
    val lexFromStdIn: unit -> SourceReader

    exception LEXICAL_ERROR of pos * string
				(* The (generated) lexer raises this on a
				   lex error. We must catch it in Parse(). *)

    type Report
    val reportPosition: {left: pos, right: pos} -> Report
    val output_source : {os: TextIO.outstream, left: pos, right: pos} -> unit
    val get_source : {left: pos, right: pos} -> string

    type StringTree
    val layoutPos: pos -> StringTree
  end;
