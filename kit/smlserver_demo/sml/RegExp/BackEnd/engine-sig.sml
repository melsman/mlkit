(* engine-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *)

signature REGEXP_ENGINE =
    sig

	type regexp
	    (* the type of a compiled regular expression
	     *)

	val compile : RegExpSyntax.syntax -> regexp
	    (* compile a regular expression from the abstract syntax
	     *)

	val find : regexp -> 
	              (char,'a) StringCvt.reader ->
		      ({pos : 'a, len : int} option MatchTree.match_tree,'a) StringCvt.reader
	    (* scan the stream for the first occurence of the regular expression
	     *)
	    
	val prefix : regexp ->
	                (char,'a) StringCvt.reader ->
			({pos : 'a, len : int} option MatchTree.match_tree,'a) StringCvt.reader
	    (* attempt to match the stream at the current position with the
	     * regular expression
	     *)

	val match : (RegExpSyntax.syntax * 
                     ({pos: 'a, len:int} option MatchTree.match_tree -> 'b)) list -> 
	                 (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
            (* attempt to the match the stream at the current position with one of
	     * the abstract syntax representations of regular expressions and trigger
	     * the corresponding action
	     *)
    end
