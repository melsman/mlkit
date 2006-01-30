
structure ParseElab: PARSE_ELAB =
  struct
    structure PP = PrettyPrint
    structure ErrorCode = ErrorTraverse.ErrorCode

    type Report = Report.Report
    type topdec = PostElabTopdecGrammar.topdec

    type absprjid = ModuleEnvironments.absprjid

    fun log s = TextIO.output(!Flags.log, s)
    fun chat s = if !Flags.chat then log s else ()

    (* -----------------------------------------------------------------
     * Dynamic flags
     * ----------------------------------------------------------------- *)

    val report_file_sig = Flags.lookup_flag_entry "report_file_sig" 
    val debug = Flags.is_on0 "debug_compiler"

    infix //
    val op // = Report.//

    type InfixBasis = InfixBasis.Basis
    type ElabBasis = ElabTopdec.StaticBasis
    datatype Result =
        SUCCESS of {report: Report, infB: InfixBasis, elabB: ElabBasis, topdec: topdec}
      | FAILURE of Report * ErrorCode.ErrorCode list

    fun elab (absprjid : absprjid, infB, elabB, topdec) : Result =
          let
	      val (elabB', topdec') = ElabTopdec.elab_topdec (absprjid, elabB, topdec)
	  in
	    (case ErrorTraverse.traverse topdec' of
	       ErrorTraverse.SUCCESS =>
		 let
		     val report = if !report_file_sig then
			             TopLevelReport.report {infB=infB, elabB=elabB', bindings=false}
				  else Report.null
		 in
		   SUCCESS {report =report,
			    infB = infB, elabB = elabB', topdec = topdec'}
		 end
	     | ErrorTraverse.FAILURE (error_report, error_codes) => FAILURE (error_report, error_codes))
	  end

    exception Parse of Report.Report
    local
      (*append_topdec topdec topdec_opt = the topdec formed by putting
       topdec after topdec_opt.  Linear in the number of nested topdecs in
       the first argument.*)
      open PreElabTopdecGrammar
      fun append_topdecs [] = NONE
	| append_topdecs (topdec::topdecs) =
	SOME(case topdec
	       of STRtopdec (i, strdec, NONE) => STRtopdec(i, strdec, append_topdecs topdecs)
		| STRtopdec (i, strdec, SOME topdec') => STRtopdec(i, strdec, append_topdecs (topdec'::topdecs))
		| SIGtopdec (i, sigdec, NONE) => SIGtopdec(i, sigdec, append_topdecs topdecs)
		| SIGtopdec (i, sigdec, SOME topdec') => SIGtopdec(i, sigdec, append_topdecs (topdec'::topdecs))
		| FUNtopdec (i, fundec, NONE) => FUNtopdec(i, fundec, append_topdecs topdecs)
		| FUNtopdec (i, fundec, SOME topdec') => FUNtopdec(i, fundec, append_topdecs (topdec'::topdecs)))

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
	    : InfixBasis * PreElabTopdecGrammar.topdec option =
	    let val state = Parse.begin (Parse.sourceFromFile file_name
					 (*may raise Io s*))
	        val (infB', topdecs) = parse0 (infB, state)
	    in (infB', append_topdecs topdecs)
	    end handle IO.Io {name,cause = OS.SysErr(err,se),...} => 
           raise Parse (Report.line ((case se of NONE => err | SOME m => OS.errorMsg m) ^ ": " ^ name))
               | IO.Io {name,...} => 
		       raise Parse (Report.line ("Failed to read file: " ^ name))

    end (*local*)

    fun maybe_print_topdec topdec =
	if debug() then
	    let val _ = print "AST before elaboration:\n"
		val st = PreElabTopdecGrammar.layoutTopdec topdec
	    in PP.printTree st
	    end
	else ()

    val empty_success = SUCCESS{report=Report.null, infB=InfixBasis.emptyB,
				elabB=ModuleEnvironments.B.empty, topdec=PostElabTopdecGrammar.empty_topdec}

    fun parse_elab {infB: InfixBasis, elabB: ElabBasis, absprjid: absprjid, file : string} : Result =
      let val _ = chat "[parsing..."
	  val _ = Timing.timing_begin()
	  val parse_res = (parse (infB, file)  (*may raise Parse*) 
			   handle E => (Timing.timing_end "Parse" ; raise E))
	  val _ = Timing.timing_end "Parse" 
	  val _ = chat "]\n"
	  val _ = chat "[elaboration..."
          (*val _ = Compiler.Profile.reset(); mads*)
          (*val _ = Compiler.Profile.setTimingMode true; mads*)
	  val _ = Timing.timing_begin()
	  val elab_res = case parse_res 
			   of (infB, SOME topdec) => (maybe_print_topdec topdec;
						      elab (absprjid, infB, elabB, topdec) 
						      handle E => (Timing.timing_end "Elab" ; raise E))
			    | (infB, NONE) => empty_success
	  val _ = Timing.timing_end "Elab" 
      (*  val _ = Compiler.Profile.setTimingMode false; 
          val _ = if Flags.is_on0 "compiler_timings" ()
                  then let val os = TextIO.openOut "elabprofile"
                       in Compiler.Profile.report os;
                          TextIO.closeOut os
                       end
                   else ()
      mads*)
	  val _ = chat "]\n"
      in elab_res
      end handle Parse report => (chat "[parsing end...]\n"; FAILURE (report, [ErrorCode.error_code_parse]))

  end
