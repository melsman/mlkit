
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

    type renderer = TopLevelReport.renderer

    datatype Result =
        SUCCESS of {doreport: renderer option -> Report, infB: InfixBasis, elabB: ElabBasis, topdec: topdec}
      | FAILURE of Report * ErrorCode.ErrorCode list

    fun elab (absprjid : absprjid, infB', elabB, topdec) : Result =
          let
	      val (elabB', topdec') =
                  ElabTopdec.elab_topdec (absprjid, elabB, topdec)
	  in
	    (case ErrorTraverse.traverse topdec' of
	       ErrorTraverse.SUCCESS =>
		 let
		     val doreport =
                         if !report_file_sig then
			   fn render => TopLevelReport.report {infB=infB', elabB=elabB', render=render}
			 else fn _ => Report.null
		 in
		   SUCCESS {doreport = doreport,
			    infB = infB', elabB = elabB', topdec = topdec'}
		 end
	     | ErrorTraverse.FAILURE (error_report, error_codes) =>
               FAILURE (error_report, error_codes))
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

      fun parse_all (infB, state) =
	case Parse.parse (infB, state)
	  of Parse.SUCCESS (infB', topdec, state') =>
	    let val (infB'', topdecs) = parse_all(InfixBasis.compose (infB, infB'), state')
	    in (InfixBasis.compose(infB', infB''), topdec::topdecs)
	    end
	   | Parse.ERROR report => raise Parse report
	   (* Parse ought to not return an ERROR but instead simply raise
	    * an exception, such that this checking for ERROR and raising here
	    * could be avoided.  26/03/1997 22:38. tho.*)
	   | Parse.LEGAL_EOF => (InfixBasis.emptyB, [])

    in
      (*parse may raise Parse*)

      datatype src = SrcFile of string
                   | SrcString of string

      fun parse (infB : InfixBasis, src : src)
	    : InfixBasis * PreElabTopdecGrammar.topdec option =
	    let val src =
                    case src of
                      SrcFile s => Parse.sourceFromFile s (*may raise Io s*)
                    | SrcString s => Parse.sourceFromString s
                val state = Parse.begin src
	        val (infB', topdecs) = parse_all (infB, state)
	    in (infB', append_topdecs topdecs)
	    end
            handle IO.Io {name,cause = OS.SysErr(err,se),...} =>
                   raise Parse (Report.line ((case se of NONE => err | SOME m => OS.errorMsg m) ^ ": " ^ name))
                 | IO.Io {name,...} =>
		   raise Parse (Report.line ("Failed to read file: " ^ name))

      fun parse_one (infB, state) =
	  case Parse.parse (infB, state) of
              Parse.SUCCESS (infB', topdec, state') => SOME (infB', topdec, state')
	    | Parse.ERROR report => raise Parse report
	    | Parse.LEGAL_EOF => NONE

    end (*local*)

    fun maybe_print_topdec topdec =
	if debug() then
	    let val _ = print "AST before elaboration:\n"
		val st = PreElabTopdecGrammar.layoutTopdec topdec
	    in PP.printTree st
             ; print "\n"
	    end
	else ()

    fun empty_success () =
        SUCCESS{doreport=fn _ => Report.null, infB=InfixBasis.emptyB,
		elabB=ModuleEnvironments.B.empty,
                topdec=PostElabTopdecGrammar.empty_topdec}

    fun parse_elab {infB: InfixBasis, elabB: ElabBasis, absprjid: absprjid, src : src} : Result =
      let val _ = chat "[parsing..."
	  val _ = Timing.timing_begin()
	  val parse_res = (parse (infB, src)  (*may raise Parse*)
			   handle E => (Timing.timing_end "Parse" ; raise E))
	  val _ = Timing.timing_end "Parse"
	  val _ = chat "]\n"
	  val _ = chat "[elaboration..."
	  val _ = Timing.timing_begin()
	  val elab_res = case parse_res
			  of (infB', SOME topdec) =>
                             (maybe_print_topdec topdec;
			      elab (absprjid, infB', elabB, topdec)
			      handle E => (Timing.timing_end "Elab" ; raise E))
			   | (_, NONE) => empty_success()
	  val _ = Timing.timing_end "Elab"
	  val _ = chat "]\n"
      in elab_res
      end handle Parse report =>
                 (chat "[parsing end...]\n";
                  FAILURE (report, [ErrorCode.error_code_parse]))


    type State = Parse.State
    fun begin_stdin () : State =
        Parse.begin(Parse.sourceFromStdIn())

    val colonLine = Parse.colonLine
    val stripSemiColons = Parse.stripSemiColons

    fun parse_elab_stdin {infB, elabB, absprjid, state: State} : State option * Result =
      let val _ = chat "[parsing..."
	  val _ = Timing.timing_begin()
	  val parse_res = (parse_one (infB, state)  (*may raise Parse*)
			   handle E => (Timing.timing_end "Parse" ; raise E))
	  val _ = Timing.timing_end "Parse"
	  val _ = chat "]\n"
      in case parse_res of
             SOME (infB',topdec,state') =>
             let
                 val () = maybe_print_topdec topdec
                 val _ = chat "[elaboration..."
	         val _ = Timing.timing_begin()
	         val elab_res = elab (absprjid, infB', elabB, topdec)
			        handle E => (Timing.timing_end "Elab" ; raise E)
                 val _ = Timing.timing_end "Elab"
	         val _ = chat "]\n"
             in (SOME state', elab_res)
             end
           | NONE =>
             (chat "[parsing end...]\n";
              (NONE, FAILURE (Report.line ("Parse error - no input!"),
                              [ErrorCode.error_code_eof])))
      end handle Parse report =>
                 (chat "[parsing end...]\n";
                  (NONE, FAILURE (report, [ErrorCode.error_code_parse])))

  end
