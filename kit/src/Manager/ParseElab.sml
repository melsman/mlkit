(*$ParseElab: BASIS PARSE ELABTOPDEC TOPDEC_GRAMMAR ERROR_TRAVERSE
	INFIX_BASIS TOP_LEVEL_REPORT BASIC_IO REPORT PRETTYPRINT FLAGS
	CRASH PARSE_ELAB *)

functor ParseElab(structure Basis: BASIS 

		  structure Parse: PARSE
		    sharing type Parse.InfixBasis = Basis.InfixBasis

		  structure ElabTopdec: ELABTOPDEC
		    sharing type ElabTopdec.PreElabTopdec = Parse.topdec
			and type ElabTopdec.StaticBasis = Basis.StaticBasis
			  
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
		    sharing type InfixBasis.Basis = Basis.InfixBasis

		  structure TopLevelReport: TOP_LEVEL_REPORT
		    sharing type TopLevelReport.Basis = Basis.Basis

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
    type Basis = Basis.Basis
    type topdec = PostElabTopdecGrammar.topdec

    infix //
    val op // = Report.//

    datatype Result =
        SUCCESS of {report: Report, basis: Basis, topdec: topdec}
      | FAILURE of Report

    fun elab (oldBasis, infix_basis, topdec) : Result =
          let val debugParse =
	            if !Flags.DEBUG_PARSING then
		      PP.reportStringTree(PreElabTopdecGrammar.layoutTopdec topdec)
		      // PP.reportStringTree(InfixBasis.layoutBasis infix_basis)
		    else Report.null
	      val (sBas, topdec') = ElabTopdec.elab_topdec
		                      (Basis.Stat_of_B oldBasis, topdec)
	  in
	    (case ErrorTraverse.traverse topdec' of
	       ErrorTraverse.SUCCESS warnings =>
		 let val debugElab =
		           if !Flags.DEBUG_ELABTOPDEC then
			     PP.reportStringTree(ElabTopdec.layoutStaticBasis sBas)
			   else Report.null
		     val elabBasis = Basis.B_plus_B
			               (Basis.Inf_in_B infix_basis, Basis.Stat_in_B sBas)
		     val report = TopLevelReport.report {basis=elabBasis, bindings=false}
		 in
		   SUCCESS {report = debugParse // debugElab // report // warnings,
			    basis = elabBasis, topdec = topdec'}
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

      fun parse0 (infix_basis, state, topdecs) =
	    (case Parse.parse (infix_basis, state) of
	       Parse.SUCCESS (infix_basis', topdec, state') =>
		 parse0 (InfixBasis.compose (infix_basis, infix_basis'),
			 state', topdec :: topdecs)
	     | Parse.ERROR report => raise Parse report
	     (*Parse ought to not return an ERROR but instead simply raise
	      an exception, such that this checking for ERROR and raising here
	      could be avoided.  26/03/1997 22:38. tho.*)
	     | Parse.LEGAL_EOF => (infix_basis, topdecs))
    in
      (*parse may raise Parse*)

      fun parse (infix_basis : Basis.InfixBasis, file_name : string)
	    : Basis.InfixBasis * PreElabTopdecGrammar.topdec Option =
	    let val state = Parse.begin (Parse.sourceFromFile file_name
					 (*may raise Io s*))
	        val (infix_basis, topdecs) = parse0 (infix_basis, state, [])
	    in
	      (infix_basis,
	       List.foldL
	         (fn topdec => fn topdec_opt =>
		  Some (append_topdec topdec topdec_opt))
		     None topdecs)
	    end handle Io s => raise Parse (Report.line s)
    end (*local*)

    fun parse_elab {Basis, file : string} : Result =
          (case parse (*may raise Parse*) (Basis.Inf_of_B Basis, file) of
	     (infix_basis, Some topdec) => elab (Basis, infix_basis, topdec)
	   | (infix_basis, None) => FAILURE (Report.line "empty file"))
	       (*TODO 26/03/1997 23:10. tho.:  an empty file oughtn't
		be an error, just an empty topdec:

		  STRtopdec (PP defaultPos defaultPos,
		             EMPTYstrdec(PP defaultPos defaultPos), None)

		but I don't know what position info to insert
		(the "PP defaultPos"'es).*)
	     handle Parse report => FAILURE report
  end;
