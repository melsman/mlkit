(* regexp-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Main signature for regular expressions.
 *)

signature REGEXP = 
    sig

	type regexp
	    (* the type of a compiled regular expression
	     *)

	val compile : (char,'a) StringCvt.reader -> (regexp, 'a) StringCvt.reader
	    (* read an external representation of a regular expression from a stream 
	     *)

	val compileString : string -> regexp
	    (* read an external representation of a regular expression from a string
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

	val match : (string * ({pos: 'a, len:int} option MatchTree.match_tree -> 'b)) list -> 
	              (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
            (* attempt to match the stream at the current position with one 
	     * of the external representations of regular expressions and trigger
	     * the corresponding action 
	     *)

    end
