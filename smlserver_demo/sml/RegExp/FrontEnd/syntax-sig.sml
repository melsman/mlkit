(* syntax-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the abstract syntax tree used to represent regular expressions.
 * It serves as the glue between different front-ends (implementing
 * different RE specification languages), and different back-ends (implementing
 * different compilation/searching algorithms).
 *)

signature REGEXP_SYNTAX =
  sig

      exception RegExp of string
      exception CannotCompile

      structure CharSet : ORD_SET where type Key.ord_key = char

      datatype syntax
        = Group of syntax
	| Alt of syntax list
	| Concat of syntax list
	| Interval of (syntax * int * int option)
	| Option of syntax	(* == Interval(re, 0, SOME 1) *)
	| Star of syntax	(* == Interval(re, 0, NONE) *)
	| Plus of syntax	(* == Interval(re, 1, NONE) *)
	| MatchSet of CharSet.set
	| NonmatchSet of CharSet.set
	| Char of char
	| Begin                   (* Matches beginning of stream *)
	| End                     (* Matches end of stream *)

      val addRange : CharSet.set * char * char -> CharSet.set
      val allChars : CharSet.set
	  
  end;
