(*$ParseElab: PARSE ELABTOPDEC MODULE_ENVIRONMENTS TOPDEC_GRAMMAR ERROR_TRAVERSE
	INFIX_BASIS TOP_LEVEL_REPORT BASIC_IO REPORT PRETTYPRINT FLAGS
	CRASH PARSE_ELAB *)

functor ParseElab(structure Parse: PARSE

		  structure ElabTopdec: ELABTOPDEC
		    sharing type ElabTopdec.PreElabTopdec = Parse.topdec

 	          structure ModuleEnvironments : MODULE_ENVIRONMENTS
		    sharing type ElabTopdec.StaticBasis = ModuleEnvironments.Basis
			  
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

    structure ErrorCode = ErrorTraverse.ErrorCode

    type Report = Report.Report
    type topdec = PostElabTopdecGrammar.topdec

    fun log s = output(!Flags.log, s)
    fun chat s = if !Flags.chat then log s else ()

    (* -----------------------------------------------------------------
     * Dynamic flags
     * ----------------------------------------------------------------- *)

    val report_file_sig = ref false
    val _ = Flags.add_flag_to_menu
          (["Control"], "report_file_sig",
	   "report program unit signatures", report_file_sig)


    infix //
    val op // = Report.//

    type InfixBasis = InfixBasis.Basis
    type ElabBasis = ElabTopdec.StaticBasis
    datatype Result =
        SUCCESS of {report: Report, infB: InfixBasis, elabB: ElabBasis, topdec: topdec}
      | FAILURE of Report * ErrorCode.ErrorCode list

    fun elab (infB, elabB, topdec) : Result =
          let val debugParse =
	            if !Flags.DEBUG_PARSING then
		      PP.reportStringTree(PreElabTopdecGrammar.layoutTopdec topdec)
		      // PP.reportStringTree(InfixBasis.layoutBasis infB)
		    else Report.null
	      val (elabB', topdec') = ElabTopdec.elab_topdec (elabB, topdec)
	  in
	    (case ErrorTraverse.traverse topdec' of
	       ErrorTraverse.SUCCESS =>
		 let val debugElab =
		           if !Flags.DEBUG_ELABTOPDEC then
			     ((PP.reportStringTree(ElabTopdec.layoutStaticBasis elabB'))
			      // (PP.reportStringTree(PostElabTopdecGrammar.layoutTopdec topdec')))
			   else Report.null
		     val report = if !report_file_sig then
			             TopLevelReport.report {infB=infB, elabB=elabB', bindings=false}
				  else Report.null
		 in
		   SUCCESS {report = debugParse // debugElab // report,
			    infB = infB, elabB = elabB', topdec = topdec'}
		 end
	     | ErrorTraverse.FAILURE (error_report, error_codes) => FAILURE (debugParse // error_report, error_codes))
	  end

    exception Parse of Report.Report
    local
      (*append_topdec topdec topdec_opt = the topdec formed by putting
       topdec after topdec_opt.  Linear in the number of nested topdecs in
       the first argument.*)
      open PreElabTopdecGrammar
      fun append_topdecs [] = None
	| append_topdecs (topdec::topdecs) =
	Some(case topdec
	       of STRtopdec (i, strdec, None) => STRtopdec(i, strdec, append_topdecs topdecs)
		| STRtopdec (i, strdec, Some topdec') => STRtopdec(i, strdec, append_topdecs (topdec'::topdecs))
		| SIGtopdec (i, sigdec, None) => SIGtopdec(i, sigdec, append_topdecs topdecs)
		| SIGtopdec (i, sigdec, Some topdec') => SIGtopdec(i, sigdec, append_topdecs (topdec'::topdecs))
		| FUNtopdec (i, fundec, None) => FUNtopdec(i, fundec, append_topdecs topdecs)
		| FUNtopdec (i, fundec, Some topdec') => FUNtopdec(i, fundec, append_topdecs (topdec'::topdecs)))

      fun parse0 (infB, state) =
	case Parse.parse (infB, state) 
	  of Parse.SUCCESS (infB', topdec, state') =>
	    let val (infB'', topdecs) = parse0(InfixBasis.compose (infB, infB'), state')
	    in (InfixBasis.compose(infB', infB''), topdec::topdecs)
	    end
	   | Parse.ERROR report => raise Parse report
	   (* Parse ought to not return an ERROR but instead simply raise
	    * an exception, such that this checking for ERROR and raising here
	    * could be avoided.  26/03/1997 22:38. tho.*)
	   | Parse.LEGAL_EOF => (InfixBasis.emptyB, [])
    in
      (*parse may raise Parse*)

      fun parse (infB : InfixBasis, file_name : string)
	    : InfixBasis * PreElabTopdecGrammar.topdec Option =
	    let val state = Parse.begin (Parse.sourceFromFile file_name
					 (*may raise Io s*))
	        val (infB', topdecs) = parse0 (infB, state)
	    in (infB', append_topdecs topdecs)
	    end handle Io s => raise Parse (Report.line s)

    end (*local*)

    val empty_success = SUCCESS{report=Report.null, infB=InfixBasis.emptyB,
				elabB=ModuleEnvironments.B.empty, topdec=PostElabTopdecGrammar.empty_topdec}

    fun parse_elab {infB: InfixBasis, elabB: ElabBasis, file : string} : Result =
      let val _ = chat "[parsing begin...]\n"
	  val parse_res = parse (infB, file)  (*may raise Parse*) 
	  val _ = chat "[parsing end...]\n"
	  val _ = chat "[elaboration begin...]\n"
	  val elab_res = case parse_res 
			   of (infB, Some topdec) => elab (infB, elabB, topdec)
			    | (infB, None) => empty_success
	  val _ = chat "[elaboration end...]\n"
      in elab_res
      end handle Parse report => (chat "[parsing end...]\n"; FAILURE (report, [ErrorCode.error_code_parse]))

  end
