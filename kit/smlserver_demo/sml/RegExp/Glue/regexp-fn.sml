(* regexp-fn.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Functor that implements a regular expressions matcher by combining
 * a surface syntax and a matching engine.
 *)

functor RegExpFn (structure P : REGEXP_PARSER 
		  structure E : REGEXP_ENGINE) : REGEXP =
    struct
	
	structure M = MatchTree

	type regexp = E.regexp
	    
	fun compile reader s = (case (P.scan reader s) 
				  of NONE => NONE
				   | SOME (syntax,s') => let val v = E.compile syntax
							 in
							     SOME (v,s')
							 end)
	    
	fun compileString str = (case (StringCvt.scanString P.scan str)
				   of SOME r => E.compile (r) 
				    | NONE => raise RegExpSyntax.RegExp str)
	    
	val prefix = E.prefix
	val find = E.find
	fun match l =
	    let val l' = map (fn (s,f) => ((case (StringCvt.scanString P.scan s)
					      of SOME r => r
					       | NONE => raise RegExpSyntax.RegExp s),
					   f)) l
		val m = E.match l'
	    in
		fn getc => fn stream => m getc stream
	    end

    end
