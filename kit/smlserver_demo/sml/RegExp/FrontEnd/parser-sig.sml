(* parser-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature REGEXP_PARSER =
  sig

    val scan : (char, 'a) StringCvt.reader
	          -> (RegExpSyntax.syntax, 'a) StringCvt.reader
	(* read an external representation of a regular expression
	 * from the stream and return an abstract syntax representation
	 *)

  end

