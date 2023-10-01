(* Topdec error traversal. NICK, 16/Jan/92. *)

structure ErrorTraverse : ERROR_TRAVERSE =
  struct
    structure ElabInfo = AllInfo.ElabInfo
    structure TopdecGrammar = PostElabTopdecGrammar

    open TopdecGrammar TopdecGrammar.DecGrammar
    structure TypeInfo = ElabInfo.TypeInfo

    (* Support for error testing. *)
    structure ErrorCode = ElabInfo.ErrorInfo.ErrorCode
    type ErrorCode = ErrorCode.ErrorCode

   (* Simple-minded first attempt: walk over a topdec, accumulating any
      error nodes we encounter. *)

    val ok = Report.null
    infix //
    val op // = Report.//

   (* Yup, side-effect time; this is the best way to take note when an
      error has been spotted. *)

    local
      val errors = ref ([]:ErrorCode list)
      val error_counter = ref 0
    in
      fun report_more_errors() : bool = !error_counter < 50
      fun spot (ec : ErrorCode) = (errors := ec :: !errors;
                                   error_counter := !error_counter + 1)
      fun get_errors () = !errors
      fun reset () = (errors := []; error_counter := 0)
    end

    val report_SourceInfo_in_ElabInfo =
            ElabInfo.ParseInfo.SourceInfo.report
          o ElabInfo.ParseInfo.to_SourceInfo
          o ElabInfo.to_ParseInfo

    fun check i =
      if report_more_errors() then
          (case ElabInfo.to_ErrorInfo i of
             SOME ei =>
               (spot (ErrorCode.from_ErrorInfo ei);
                Report.line ""
                // report_SourceInfo_in_ElabInfo i
                // ElabInfo.ErrorInfo.report ei)
           | NONE => Report.null)
      else Report.null

    fun report_escaping (i, msg : string) =
          report_SourceInfo_in_ElabInfo i
          // Report.line ("escaping type variable(s): " ^ msg)

    fun walk_opt _ NONE = ok
      | walk_opt walk (SOME obj) = walk obj

    fun walk_IdInfoList list =
      case list
        of nil => ok
         | WITH_INFO(i, _) :: rest => check i // walk_IdInfoList rest

    fun walk_Topdec topdec =
      case topdec
        of STRtopdec(i, strdec, topdec_opt) =>
             check i // walk_Strdec strdec // walk_opt walk_Topdec topdec_opt

         | SIGtopdec(i, sigdec, topdec_opt) =>
             check i // walk_Sigdec sigdec // walk_opt walk_Topdec topdec_opt

         | FUNtopdec(i, fundec, topdec_opt) =>
             check i // walk_Fundec fundec // walk_opt walk_Topdec topdec_opt

   (* MODULES: *)
    and walk_Strdec strdec =
      case strdec
        of DECstrdec(i, dec) =>
             check i // walk_Dec dec

         | STRUCTUREstrdec(i, strbind) =>
             check i // walk_Strbind strbind

         | LOCALstrdec(i, strdec1, strdec2) =>
             check i // walk_Strdec strdec1 // walk_Strdec strdec2

         | EMPTYstrdec i =>
             check i

         | SEQstrdec(i, strdec1, strdec2) =>
             check i // walk_Strdec strdec1 // walk_Strdec strdec2

    and walk_Strbind (STRBIND(i, _, strexp, strbind_opt)) =
          check i // walk_Strexp strexp // walk_opt walk_Strbind strbind_opt

    and walk_Sigexp sigexp =
      case sigexp
        of SIGsigexp(i, spec) =>
             check i // walk_Spec spec

         | SIGIDsigexp(i, _) =>
             check i

         | WHERE_TYPEsigexp(i, sigexp, tyvars, longtycon, ty) =>
             check i // walk_Sigexp sigexp // walk_Ty ty

    and walk_Strexp strexp =
      case strexp
        of STRUCTstrexp(i, strdec) =>
             check i // walk_Strdec strdec

         | LONGSTRIDstrexp(i, _) =>
             check i

         | TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
             check i // walk_Strexp strexp // walk_Sigexp sigexp

         | OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) =>
             check i // walk_Strexp strexp // walk_Sigexp sigexp

         | APPstrexp(i, _, strexp) =>
             check i // walk_Strexp strexp

         | LETstrexp(i, strdec, strexp) =>
             check i // walk_Strdec strdec // walk_Strexp strexp

    and walk_Sigdec (SIGNATUREsigdec(i, sigbind)) =
          check i // walk_Sigbind sigbind

    and walk_Sigbind (SIGBIND(i, _, sigexp, sigbind_opt)) =
          check i // walk_Sigexp sigexp // walk_opt walk_Sigbind sigbind_opt

    and walk_Fundec (FUNCTORfundec(i, funbind)) =
          check i // walk_Funbind funbind

    and walk_Funbind (FUNBIND(i, _, _, sigexp, strexp, funbind_opt)) =
          check i
          // walk_Sigexp sigexp
          // walk_Strexp strexp
          // walk_opt walk_Funbind funbind_opt

    and walk_Spec spec =
      case spec
        of VALspec(i, valdesc) =>       check i // walk_Valdesc valdesc
         | TYPEspec(i, typdesc) =>      check i // walk_Typdesc typdesc
         | EQTYPEspec(i, typdesc) =>    check i // walk_Typdesc typdesc
         | DATATYPEspec(i, datdesc) =>  check i // walk_Datdesc datdesc
         | DATATYPE_REPLICATIONspec(i, tycon, longtycon) =>
             check i
         | EXCEPTIONspec(i, exdesc) =>  check i // walk_Exdesc exdesc
         | STRUCTUREspec(i, strdesc) => check i // walk_Strdesc strdesc
         | INCLUDEspec(i, sigexp) => check i // walk_Sigexp sigexp
         | SHARING_TYPEspec(i, spec, longtycon_withinfo_list) =>
             check i
             // walk_Spec spec
             // walk_IdInfoList longtycon_withinfo_list
         | SHARINGspec(i, spec, longstrid_withinfo_list) =>
             check i
             // walk_Spec spec
             // walk_IdInfoList longstrid_withinfo_list
         | EMPTYspec i => check i
         | SEQspec(i, spec1, spec2) =>
             check i // walk_Spec spec1 // walk_Spec spec2

    and walk_Valdesc valdesc =
      case valdesc
        of VALDESC(i, _, ty, valdesc_opt) =>
             check i // walk_Ty ty // walk_opt walk_Valdesc valdesc_opt

    and walk_Typdesc typdesc =
      case typdesc
        of TYPDESC(i, _, _, typdesc_opt) =>
          check i // walk_opt walk_Typdesc typdesc_opt

    and walk_Datdesc datdesc =
      case datdesc
        of DATDESC(i, _, _, condesc, datdesc_opt) =>
             check i
             // walk_Condesc condesc
             // walk_opt walk_Datdesc datdesc_opt

    and walk_Condesc condesc =
      case condesc
        of CONDESC(i, _, ty_opt, condesc_opt) =>
             check i
             // walk_opt walk_Ty ty_opt
             // walk_opt walk_Condesc condesc_opt

    and walk_Exdesc exdesc =
      case exdesc
        of EXDESC(i, _, ty_opt, exdesc_opt) =>
             check i
             // walk_opt walk_Ty ty_opt
             // walk_opt walk_Exdesc exdesc_opt

    and walk_Strdesc strdesc =
      case strdesc
        of STRDESC(i, _, sigexp, strdesc_opt) =>
             check i
             // walk_Sigexp sigexp
             // walk_opt walk_Strdesc strdesc_opt

   (* CORE: *)
    and walk_Dec dec =
      case dec
        of VALdec(i, tyvars, valbind) =>
             check i // walk_Valbind valbind

         | UNRES_FUNdec _ =>
             Crash.impossible "ErrorTraverse.walk_Dec(UNRES_FUN)"

         | TYPEdec(i, typbind) =>
             check i // walk_Typbind typbind

         | DATATYPEdec(i, datbind) =>
             check i // walk_Datbind datbind

         | DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
             check i

         | ABSTYPEdec(i, datbind, dec) =>
             check i // walk_Datbind datbind // walk_Dec dec

         | EXCEPTIONdec(i, exbind) =>
             check i // walk_Exbind exbind

         | LOCALdec(i, dec1, dec2) =>
             check i // walk_Dec dec1 // walk_Dec dec2

         | OPENdec(i, list) =>
             check i // walk_IdInfoList list

         | SEQdec(i, dec1, dec2) =>
             check i // walk_Dec dec1 // walk_Dec dec2

         | INFIXdec(i, _, _) =>  check i
         | INFIXRdec(i, _, _) => check i
         | NONFIXdec(i, _) =>    check i
         | EMPTYdec i =>         check i
         | WITHdec(i, _) =>      check i

    and walk_Valbind valbind =
      case valbind
        of PLAINvalbind(i, pat, exp, valbind_opt) =>
             check i // walk_Pat pat
                    // walk_Exp exp
                   // walk_opt walk_Valbind valbind_opt


         | RECvalbind(i, valbind) =>
             check i // walk_Valbind valbind

    and walk_Typbind typbind =
      case typbind
        of TYPBIND(i, _, _, ty, typbind_opt) =>
             check i // walk_Ty ty // walk_opt walk_Typbind typbind_opt

    and walk_Datbind datbind =
      case datbind
        of DATBIND(i, _, _, conbind, datbind_opt) =>
             check i
             // walk_Conbind conbind
             // walk_opt walk_Datbind datbind_opt

    and walk_Conbind conbind =
      case conbind
        of CONBIND(i, _, ty_opt, conbind_opt) =>
             check i
             // walk_opt walk_Ty ty_opt
             // walk_opt walk_Conbind conbind_opt

    and walk_Exbind exbind =
      case exbind
        of EXBIND(i, _, ty_opt, exbind_opt) =>
             check i
             // walk_opt walk_Ty ty_opt
             // walk_opt walk_Exbind exbind_opt

         | EXEQUAL(i, _, _, exbind_opt) =>
             check i // walk_opt walk_Exbind exbind_opt

    and walk_Pat pat =
      case pat
        of ATPATpat(i, atpat) =>
             check i // walk_Atpat atpat

         | CONSpat(i, _, atpat) =>
             check i // walk_Atpat atpat

         | TYPEDpat(i, pat, ty) =>
             check i // walk_Pat pat // walk_Ty ty

         | LAYEREDpat(i, _, ty_opt, pat) =>
             check i // walk_opt walk_Ty ty_opt // walk_Pat pat

         | UNRES_INFIXpat _ =>
             Crash.impossible "ErrorTraverse.walk_Pat(UNRES_INFIX)"

    and walk_Atpat atpat =
      case atpat
        of WILDCARDatpat i => check i
         | SCONatpat(i, _) => check i
         | LONGIDatpat(i, _, _) => check i

         | RECORDatpat(i, patrow_opt) =>
             check i // walk_opt walk_Patrow patrow_opt

         | PARatpat(i, pat) =>
             check i // walk_Pat pat

    and walk_Patrow patrow =
      case patrow
        of DOTDOTDOT i =>
             check i

         | PATROW(i, _, pat, patrow_opt) =>
             check i // walk_Pat pat // walk_opt walk_Patrow patrow_opt

    and walk_Exp exp =
      case exp
        of ATEXPexp(i, atexp) =>
             check i // walk_Atexp atexp

         | APPexp(i, exp, atexp) =>
             check i // walk_Exp exp // walk_Atexp atexp

         | TYPEDexp(i, exp, ty) =>
             check i // walk_Exp exp // walk_Ty ty

         | HANDLEexp(i, exp, match) =>
             check i // walk_Exp exp // walk_Match match

         | RAISEexp(i, exp) =>
             check i // walk_Exp exp

         | FNexp(i, match) =>
             check i // walk_Match match

         | UNRES_INFIXexp _ =>
             Crash.impossible "ErrorTraverse.walk_Exp(UNRES_INFIX)"

    and walk_Atexp atexp =
      case atexp
        of SCONatexp(i, _, _) =>  check i
         | IDENTatexp(i, _, _) => check i

         | RECORDatexp(i, exprow_opt, _) =>
             check i // walk_opt walk_Exprow exprow_opt

         | LETatexp(i, dec, exp) =>
             check i // walk_Dec dec // walk_Exp exp

         | PARatexp(i, exp) =>
             check i // walk_Exp exp

    and walk_Exprow exprow =
      case exprow
        of EXPROW(i, _, exp, exprow_opt) =>
             check i // walk_Exp exp // walk_opt walk_Exprow exprow_opt

    and walk_Match match =
      case match
        of MATCH(i, mrule, match_opt) =>
             check i // walk_Mrule mrule // walk_opt walk_Match match_opt

    and walk_Mrule mrule =
      case mrule
        of MRULE(i, pat, exp) =>
             check i // walk_Pat pat // walk_Exp exp

    and walk_Ty ty =
      case ty
        of TYVARty(i, _) =>
             check i

         | RECORDty(i, tyrow_opt, NONE) =>
             check i // walk_opt walk_Tyrow tyrow_opt

         | RECORDty(i, tyrow_opt, SOME(i2,_)) =>
             check i // walk_opt walk_Tyrow tyrow_opt // check i2

         | CONty(i, tys, _) =>
             check i // List.foldr (fn (a,b) => walk_Ty a // b) ok tys

         | FNty(i, ty1, rvopt, ty2) =>
             check i // walk_Ty ty1 // walk_Ty ty2 // walk_rvopt rvopt

         | PARty(i, ty, rvopts) =>
             check i // walk_Ty ty // walk_rvopts rvopts

         | WITHty(i, ty, c) =>
             check i // walk_Ty ty  (* memo: maybe walk the constraints if info attached to
                                       regvars may contain errors! *)
    and walk_rvopt NONE = Report.null
      | walk_rvopt (SOME(i,_)) = check i

    and walk_rvopts NONE = Report.null
      | walk_rvopts (SOME(i,xs)) = List.foldl (fn ((i,_),acc) => check i // acc) (check i) xs

    and walk_Tyrow tyrow =
      case tyrow
        of TYROW(i, _, ty, tyrow_opt) =>
             check i // walk_Ty ty // walk_opt walk_Tyrow tyrow_opt


    type Report = Report.Report

    datatype result = SUCCESS
                    | FAILURE of Report * ErrorCode list

    fun traverse topdec =
      let
        val _ = reset()
        val report = walk_Topdec topdec
      in
        case get_errors()
          of [] => SUCCESS
           | error_codes => FAILURE (report, error_codes)
      end
  end
