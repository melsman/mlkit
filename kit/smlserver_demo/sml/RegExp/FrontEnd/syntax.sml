(* syntax.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the abstract syntax tree used to represent regular expressions.
 * It serves as the glue between different front-ends (implementing
 * different RE specification languages), and different back-ends (implementing
 * different compilation/searching algorithms).
 *)

structure RegExpSyntax : REGEXP_SYNTAX =
  struct
 
      exception RegExp of string
      exception CannotCompile

      structure CharSet = ListSetFn (struct type ord_key = char val compare = Char.compare end)

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
	| Begin
	| End

      fun addRange (s,minC,maxC) = CharSet.addList (s,List.tabulate (ord(maxC)-ord(minC)+1,fn v => chr (v+ord(minC))))

      val allChars = addRange (CharSet.empty, Char.minChar, Char.maxChar)
	  
  end;
