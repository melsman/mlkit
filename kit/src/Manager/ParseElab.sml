(*$ParseElab: PARSE ELABTOPDEC TOPDEC_GRAMMAR ERROR_TRAVERSE
	INFIX_BASIS TOP_LEVEL_REPORT BASIC_IO REPORT PRETTYPRINT FLAGS
	CRASH PARSE_ELAB *)

functor ParseElab(structure Parse: PARSE

		  structure ElabTopdec: ELABTOPDEC
		    sharing type ElabTopdec.PreElabTopdec = Parse.topdec
			  
		  structure PreElabTopdecGrammar: TOPDEC_GRAMMAR
		    sharing type PreElabTopdecGrammar.topdec
				      = ElabTopdec.PreElabTopdec
		  structure PostElabTopdecGrammar: TOPDEC_GRAMMAR
		    sharing type PostElabTopdecGrammar.topdec
				      = ElabTopdec.PostElabTopdec

		  structure ErrorTraverse: ERROR_TRAVERSE
		    sharing type ErrorTraverse.topdec
					= ElabTopdec.PostElabTopdec

		  structure InfixBasis: INFIX_BASIS
		    sharing type InfixBasis.Basis = Parse.InfixBasis

		  structure TopLevelReport: TOP_LEVEL_REPORT
		    sharing type TopLevelReport.ElabBasis = ElabTopdec.StaticBasis
		        and type TopLevelReport.InfixBasis = InfixBasis.Basis

		  structure BasicIO: BASIC_IO

		  structure Report: REPORT
		    sharing type InfixBasis.Report
					= Parse.Report
					= ErrorTraverse.Report
					= TopLevelReport.Report
					= Report.Report

		  structure PP: PRETTYPRINT
		    sharing type PP.Report = Report.Report
		        and type InfixBasis.StringTree
					= PreElabTopdecGrammar.StringTree
					= PostElabTopdecGrammar.StringTree
					= ElabTopdec.StringTree
					= PP.StringTree

		  structure Flags: FLAGS
		  structure Crash: CRASH
		    ): PARSE_ELAB =
  struct
    type Report = Report.Report
    type topdec = PostElabTopdecGrammar.topdec

    infix //
    val op // = Report.//

    type InfixBasis = InfixBasis.Basis
    type ElabBasis = ElabTopdec.StaticBasis
    datatype Result =
        SUCCESS of {report: Report, infB: InfixBasis, elabB: ElabBasis, topdec: topdec}
      | FAILURE of Report

    fun elab (infB, elabB, topdec) : Result =
          let val debugParse =
	            if !Flags.DEBUG_PARSING then
		      PP.reportStringTree(PreElabTopdecGrammar.layoutTopdec topdec)
		      // PP.reportStringTree(InfixBasis.layoutBasis infB)
		    else Report.null
	      val (elabB', topdec') = ElabTopdec.elab_topdec (elabB, topdec)
	  in
	    (case ErrorTraverse.traverse topdec' of
	       ErrorTraverse.SUCCESS warnings =>
		 let val debugElab =
		           if !Flags.DEBUG_ELABTOPDEC then
			     PP.reportStringTree(ElabTopdec.layoutStaticBasis elabB')
			   else Report.null
		     val report = TopLevelReport.report {infB=infB, elabB=elabB', bindings=false}
		 in
		   SUCCESS {report = debugParse // debugElab // report // warnings,
			    infB = infB, elabB = elabB', topdec = topdec'}
		 end
	     | ErrorTraverse.FAILURE errors => FAILURE (debugParse // errors))
	  end

    exception Parse of Report.Report
    local
      (*append_topdec topdec topdec_opt = the topdec formed by putting
       topdec after topdec_opt.  Linear in the number of nested topdecs in
       the first argument.*)
      open PreElabTopdecGrammar
      fun append_topdec (STRtopdec (info, strdec, None)) topdec_opt =
	    STRtopdec (info, strdec, topdec_opt)
	| append_topdec (STRtopdec (info, strdec, Some topdec_between)) topdec_opt =
	    STRtopdec (info, strdec, Some (append_topdec topdec_between topdec_opt))
	| append_topdec (SIGtopdec (info, sigdec, None)) topdec_opt =
	    SIGtopdec (info, sigdec, topdec_opt)
	| append_topdec (SIGtopdec (info, sigdec, Some topdec_between)) topdec_opt =
	    SIGtopdec (info, sigdec, Some (append_topdec topdec_between topdec_opt))
	| append_topdec (FUNtopdec (info, fundec, None)) topdec_opt = 
	    FUNtopdec (info, fundec, topdec_opt)
	| append_topdec (FUNtopdec (info, fundec, Some topdec_between)) topdec_opt = 
	    FUNtopdec (info, fundec, Some (append_topdec topdec_between topdec_opt))

      fun parse0 (infB, state, topdecs) =
	    (case Parse.parse (infB, state) of
	       Parse.SUCCESS (infB', topdec, state') =>
		 parse0 (InfixBasis.compose (infB, infB'),
			 state', topdec :: topdecs)
	     | Parse.ERROR report => raise Parse report
	     (*Parse ought to not return an ERROR but instead simply raise
	      an exception, such that this checking for ERROR and raising here
	      could be avoided.  26/03/1997 22:38. tho.*)
	     | Parse.LEGAL_EOF => (infB, topdecs))
    in
      (*parse may raise Parse*)

      fun parse (infB : InfixBasis, file_name : string)
	    : InfixBasis * PreElabTopdecGrammar.topdec Option =
	    let val state = Parse.begin (Parse.sourceFromFile file_name
					 (*may raise Io s*))
	        val (infB, topdecs) = parse0 (infB, state, [])
	    in
	      (infB,
	       List.foldL
	         (fn topdec => fn topdec_opt =>
		  Some (append_topdec topdec topdec_opt))
		     None topdecs)
	    end handle Io s => raise Parse (Report.line s)
    end (*local*)

    fun parse_elab {infB: InfixBasis, elabB: ElabBasis, file : string} : Result =
          (case parse (*may raise Parse*) (infB, file)
	     of (infB, Some topdec) => elab (infB, elabB, topdec)
	      | (infB, None) => FAILURE (Report.line "empty file"))
	     (* TODO 26/03/1997 23:10. tho.:  an empty file oughtn't
	      * be an error, just an empty topdec:
	      *
	      *  STRtopdec (PP defaultPos defaultPos,
	      *             EMPTYstrdec(PP defaultPos defaultPos), None)
	      *
	      * but I don't know what position info to insert
	      * (the "PP defaultPos"'es).
	      *)
	     handle Parse report => FAILURE report
  end;
