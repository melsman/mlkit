
functor GrammarUtils (structure TopdecGrammar : TOPDEC_GRAMMAR
		      structure LexBasics : LEX_BASICS
		      structure ParseInfo : PARSE_INFO
		      sharing type ParseInfo.SourceInfo.pos = LexBasics.pos
		      sharing type ParseInfo.ParseInfo = TopdecGrammar.info			
		      structure Report : REPORT 
		      sharing type LexBasics.Report
			         = Report.Report
			         = ParseInfo.SourceInfo.Report
		      structure PrettyPrint : PRETTYPRINT
		      sharing type PrettyPrint.StringTree
			= TopdecGrammar.StringTree
			= LexBasics.StringTree
		      sharing type PrettyPrint.Report = Report.Report
		      structure Crash : CRASH
			) : GRAMMAR_UTILS =
  struct

    fun impossible s = Crash.impossible ("GrammarUtils." ^ s)

    structure TopdecGrammar = TopdecGrammar
    structure SourceInfo = ParseInfo.SourceInfo
    type pos = LexBasics.pos
    type Report = Report.Report

    structure M = TopdecGrammar
    structure C = TopdecGrammar.DecGrammar

    open TopdecGrammar TopdecGrammar.DecGrammar

    fun inventId_from_atpat atpat = 
         case C.find_topmost_id_in_atpat atpat of
           SOME string => Ident.invent_named_id string
         | NONE =>        Ident.inventId()

   (* The simple constructing functions first. *)

    val mk_IdentLab = Lab.mk_IdentLab
    val mk_IntegerLab = Lab.mk_IntegerLab
    val mk_Id = Ident.mk_Id
    val mk_LongId = Ident.mk_LongId
    val mk_FunId = FunId.mk_FunId
    val mk_StrId = StrId.mk_StrId
    val mk_LongStrId = StrId.mk_LongStrId
    val longStrIdOfStrId = StrId.longStrIdOfStrId
    val mk_SigId = SigId.mk_SigId
    val mk_TyVar = TyVar.mk_TyVar
    val mk_TyCon = TyCon.mk_TyCon
    val mk_LongTyCon = TyCon.mk_LongTyCon
    val mk_IntSCon = SCon.INTEGER
    val mk_WordSCon = SCon.WORD
    val mk_StringSCon = SCon.STRING
    val mk_CharSCon = SCon.CHAR
    val mk_RealSCon = SCon.REAL

    fun PP pos_l pos_r = ParseInfo.from_SourceInfo
                           (SourceInfo.from_positions pos_l pos_r)

    val un_PP = SourceInfo.to_positions o ParseInfo.to_SourceInfo
    val right = #2 o un_PP
    val left = #1 o un_PP

    local 
      fun info_before_or_after (side : ParseInfo.ParseInfo -> pos)
	    (info : ParseInfo.ParseInfo) : ParseInfo.ParseInfo =
	    let val pos = side info in PP pos pos end
    in
      val info_before = info_before_or_after left
      val info_after = info_before_or_after right
    end (*local*)

    (*span_info (i1, i2) combines the ParseInfos i1 and i2 to a
     single ParseInfo, whose position information spans from the
     left of i1 to the right of i2.  The resulting ParseInfo has
     no derived form information.*)

    fun span_info (ParseInfo1, ParseInfo2) =
          PP (left ParseInfo1) (right ParseInfo2)

    local
      (*val rightmost0 :  ('a -> 'info) -> 'a ->
                          ('b -> 'info) -> 'b option -> 'info
        val rightmost0' : ('a -> 'info) -> 'a ->
                          ('b -> 'info) -> 'b option ->
                          ('c -> 'info) -> 'c option -> 'info*)
      fun rightmost0 r1 n1 r2 (SOME n2) = r2 n2
	| rightmost0 r1 n1 r2 NONE = r1 n1
    in
      fun rightmost r1 n1 r2 (SOME n2) = right (r2 n2)
	| rightmost r1 n1 r2 NONE = right (r1 n1)
      fun rightmost' pos1 r2 (SOME n2) = right (r2 n2)
	| rightmost' pos1 r2 NONE = pos1
      fun rightmost_of_three pos1 r2     _     r3 (SOME n3) = right (r3 n3)
	| rightmost_of_three pos1 r2 (SOME n2) r3     _     = right (r2 n2)
	| rightmost_of_three pos1 r2     _     r3     _     = pos1
      fun rightmost_of_four pos1 r2     _     r3     _     r4 (SOME n4) = right (r4 n4)
        | rightmost_of_four pos1 r2     _     r3 (SOME n3) r4     _     = right (r3 n3)
	| rightmost_of_four pos1 r2 (SOME n2) r3     _     r4     _     = right (r2 n2)
	| rightmost_of_four pos1 r2     _     r3     _     r4     _     = pos1
    end (*local*)

    fun wi_Convert f list =
      case list
        of WITH_INFO(i, x) :: rest =>
             WITH_INFO(i, f x) :: wi_Convert f rest

         | nil => nil


   (* Complex constructor functions. *)

    fun expOfAtexp atexp = ATEXPexp (get_info_atexp atexp, atexp)
    fun patOfAtpat atpat = ATPATpat (get_info_atpat atpat, atpat)

    fun atexpOfIdent info id = 
          IDENTatexp (info, OP_OPT (Ident.idToLongId id, false))
    fun expOfIdent info id = expOfAtexp (atexpOfIdent info id)
    fun patOfIdent info (id, withOp) =
          patOfAtpat (LONGIDatpat (info, OP_OPT (Ident.idToLongId id, withOp)))

    fun topdecOfExp exp =       (* Convert `exp' to `val it = exp'. *)
          let
	    val info = get_info_exp exp
	    val pat = patOfIdent info (Ident.id_IT, false)
	    val valbind = PLAINvalbind (info, pat, exp, NONE)
	    val valdec = VALdec (info, [], valbind)
	    val strdec = DECstrdec (info, valdec)
	  in
	    STRtopdec (info, strdec, NONE)
	  end

    fun composeStrDec(i, strdec1, strdec2) =
      case (strdec1, strdec2)
        of (EMPTYstrdec _, _) => strdec2
         | (_, EMPTYstrdec _) => strdec1
         | _ => SEQstrdec(i, strdec1, strdec2)

    fun composeSpec(i, spec1, spec2) =
      case (spec1, spec2)
        of (EMPTYspec _, _) => spec2
         | (_, EMPTYspec _) => spec1
         | _ => SEQspec(i, spec1, spec2)

    val inventStrId = StrId.inventStrId
    val inventId = Ident.inventId

    fun composeDec(i, dec1, dec2) =
      case (dec1, dec2)
        of (EMPTYdec _, _) => dec2
         | (_, EMPTYdec _) => dec1
         | _ => SEQdec(i, dec1, dec2)

    fun tuple_atexp_with_info info exps = (* `(A, B, C)' -> `{1=A, 2=B, 3=C}'. *)
          let
	    fun f (n, e :: exps) =
	          SOME (EXPROW (info, mk_IntegerLab n, e, f (n+1, exps)))
	      | f (_, []) = NONE
	  in
	    RECORDatexp (info, f (1, exps))
	  end

    fun tuple_atexp (exp1::exp2::exps) =
          tuple_atexp_with_info
	    (span_info (get_info_exp exp1,
			get_info_exp (List.last (exp2::exps))
			handle _ => impossible "tuple_atexp: last"))
	       (exp1::exp2::exps)
      | tuple_atexp _ = impossible "tuple_atexp _"

    fun case_exp info (exp, match) =
          APPexp (info, FNexp (get_info_match match, match),
		  PARatexp (get_info_exp exp, exp)) 

    fun sequenceExp exps =
          let
	    fun wildMatch exp =
                  let val info_exp = get_info_exp exp
		  in
		    MATCH (info_exp,
			   MRULE
			     (info_exp, ATPATpat (info_exp, WILDCARDatpat info_exp), exp),
			   NONE)
		  end
		
	    fun f (exp :: exps, context) =
	          case_exp (get_info_exp exp) (exp, wildMatch (f (exps, context)))
	      | f ([], context) = context
	  in
	    case rev exps of
	      last :: rest => f (rev rest, last)
	    | [] => impossible "sequenceExp"
	  end

    fun list_atexp info exps =
          let
	    val nilExp = expOfIdent info Ident.id_NIL
	    val consExp = expOfIdent info Ident.id_CONS

	    fun f (exp :: exps) = 
	          APPexp (get_info_exp exp, consExp, tuple_atexp [exp, f exps])
	      | f [] = nilExp
	  in
	    PARatexp (info, f exps)
	  end

    fun hash info lab =
          let
	    val id = Ident.inventId ()
	    val row = PATROW (info, lab, patOfIdent info (id, false),
			      SOME (DOTDOTDOT info))
	    val pat = ATPATpat (info, RECORDatpat (info, SOME row))
	    val mrule = MRULE (info, pat, expOfIdent info id)
	    val match = MATCH (info, mrule, NONE)
	  in
	    PARatexp (info, FNexp (info, match))
	  end

    fun exp_true info = expOfIdent info Ident.id_TRUE

    fun exp_false info = expOfIdent info Ident.id_FALSE

    fun exp_quote (info:info) (s:string) : exp =
      let val quoteExp = expOfIdent info Ident.id_QUOTE
      in APPexp(info, quoteExp, SCONatexp(info, SCon.STRING s))
      end

    fun exp_antiquote (info:info) (atexp: atexp) : exp =
      let val antiquoteExp = expOfIdent info Ident.id_ANTIQUOTE
      in APPexp(info, antiquoteExp, atexp)
      end

    fun if_then_else_exp info (ifExp, thenExp, elseExp) =
          let
	    val mruleT = MRULE (info, patOfIdent info (Ident.id_TRUE, false), thenExp)
	    val mruleF = MRULE (info, patOfIdent info (Ident.id_FALSE, false), elseExp)
	  in
	    case_exp info
	      (ifExp, MATCH (info, mruleT, SOME (MATCH (info, mruleF, NONE))))
	  end

    fun while_exp info (whileExp, doExp) =
          let
	    val var = Ident.inventLongId ()
	    val varExp = ATEXPexp (info, IDENTatexp (info, OP_OPT (var, false)))
	    val unitAtExp = RECORDatexp (info, NONE)
	    val unitExp = ATEXPexp (info, unitAtExp)
	    val varPat = ATPATpat (info, LONGIDatpat (info, OP_OPT (var, false)))
	    val unitPat = ATPATpat (info, RECORDatpat (info, NONE))
	    val callVar = APPexp (info, varExp, unitAtExp)

	    val fnBody = if_then_else_exp info
	                   (whileExp, sequenceExp [doExp, callVar], unitExp)

	    val mrule = MRULE (info, unitPat, fnBody)
	    val match = MATCH (info, mrule, NONE)
	    val fnExp = FNexp (info, match)
	    val bind = RECvalbind (info, PLAINvalbind (info, varPat, fnExp, NONE))
	    val dec = VALdec (info, [], bind)
	  in
	    ATEXPexp (info, LETatexp (info, dec, callVar))
	  end

    local
      fun count _ [] _ = NONE
	| count p (x::xs) n = if p x then SOME n
			      else count p xs (n + 1)
    in
      fun index p l = count p l 0
    end


    fun rewriteDatBind(datbind, typbind) =
        (* XXX no check for different identifiers to be bound in datbind 
           and typbind (as required, Def. p. 66) *)
      let

        (* replace --- replaces typevariables in ty, that occur in
           tyvarseq with the corresonding types in tyseq, assumes size
           of tyvarseq = size of tyseq ; that is, this one implements
           substitution. *)

        fun replaceTy tyvarseq tyseq ty = 
          case ty of 
            TYVARty(i, tv) => 
              let 
                val i = 
                  case index (fn a => a=tv) tyvarseq
		    of SOME i => i
		     | NONE => Crash.unimplemented
                      "No check for tyvar on rsh in lhs of withtype defined type"
              in
                (List.nth (tyseq,i)) 
                handle _ => impossible "rewriteDatBind---replaceTy"
              end
          | RECORDty(i, NONE) =>
              ty
          | RECORDty(i, SOME tyrow) => 
              RECORDty(i, SOME (replaceTyrow tyvarseq tyseq tyrow))
          | CONty(i, tylist, tycon) => 
              CONty(i, map (replaceTy tyvarseq tyseq) tylist, tycon)
          | FNty(i, ty1, ty2) => 
              FNty(i, replaceTy tyvarseq tyseq ty1,
                   replaceTy tyvarseq tyseq ty2)
          | PARty(i, ty) =>
              PARty(i, replaceTy tyvarseq tyseq ty)

        and replaceTyrow tyvarseq tyseq tyrow =
          case tyrow of
            TYROW(i, lab, ty, NONE) =>
              TYROW(i, lab, replaceTy tyvarseq tyseq ty, NONE)
          | TYROW(i, lab, ty, SOME tyrow) =>
              TYROW(i, lab, replaceTy tyvarseq tyseq ty, 
                   SOME (replaceTyrow tyvarseq tyseq tyrow))
          
        exception Lookup_tycon
        fun lookup_tycon tycon typbind =
          case typbind of 
            TYPBIND(_, tyvarseq, tycon', ty, NONE) =>
              if tycon' = tycon then (tyvarseq, ty)
              else raise Lookup_tycon
          | TYPBIND(_, tyvarseq, tycon', ty, SOME typbind) =>
              if tycon' = tycon then (tyvarseq, ty)
              else lookup_tycon tycon typbind

        fun rewriteTy ty =
          case ty of 
            TYVARty _ => 
              ty
          | RECORDty(i, NONE) => 
              ty
          | RECORDty(i, SOME tyrow) => 
              RECORDty(i, SOME (rewriteTyrow tyrow))
          | CONty(i, tyseq', longtycon') =>
              let 
                val (strid_list, tycon') = TyCon.explode_LongTyCon longtycon'
              in
                if strid_list = nil then
                  (let 
                     val (tyvarseq1, ty1) = lookup_tycon tycon' typbind
                     val _ = 
                       if (List.length tyseq') <> (List.length tyvarseq1) then
                         Crash.unimplemented
			   "GrammarUtils.rewriteDatBind< insert error info into i >"
                       else ()
                   in
                     replaceTy tyvarseq1 tyseq' ty1
                   end)
                     handle Lookup_tycon => 
                     (* keep type constructor, but traverse its
                        arguments*)
                     CONty(i, map rewriteTy tyseq', longtycon')
                else
		  (* Was: ty ; ME 2001-03-05 - bug reported by Stephen
		   * Weeks, Mon, 7 Aug 2000. *)
		  (* keep type constructor, but traverse its
		   arguments*)
		  CONty(i, map rewriteTy tyseq', longtycon')
              end
          | FNty(i, ty1, ty2) => 
              FNty(i, rewriteTy ty1, rewriteTy ty2)
          | PARty(i, ty) =>
              PARty(i, rewriteTy ty)

        and rewriteTyrow tyrow =
          case tyrow of
            TYROW(i, lab, ty, NONE) =>
              TYROW(i, lab, rewriteTy ty, NONE)
          | TYROW(i, lab, ty, SOME tyrow) =>
              TYROW(i, lab, rewriteTy ty, SOME (rewriteTyrow tyrow))

        fun rewriteConBind (CONBIND(i, con, NONE, NONE)) =
              CONBIND(i, con, NONE, NONE)
          | rewriteConBind (CONBIND(i, con, SOME ty, NONE)) =
              CONBIND(i, con, SOME (rewriteTy ty), NONE)
          | rewriteConBind (CONBIND(i, con, NONE, SOME conbind)) =
              CONBIND(i, con, NONE, SOME (rewriteConBind conbind))
          | rewriteConBind (CONBIND(i, con, SOME ty, SOME conbind)) =
              CONBIND(i, con, SOME (rewriteTy ty), SOME (rewriteConBind conbind))

	fun in_dom tc tb = (lookup_tycon tc tb; true)
	  handle Lookup_tycon => false

	fun check i tycon =		    
	  if in_dom tycon typbind then
	    let val pos = left i
	    in raise LexBasics.LEXICAL_ERROR (pos, "type constructor " ^ TyCon.pr_TyCon tycon 
					      ^ " is bound by both datatype binding and withtype binding")
	    end
	  else ()

      in
        case datbind of 
          DATBIND(i, tyvarlist, tycon, conbind, NONE) =>
	    (check i tycon;
	     DATBIND(i, tyvarlist, tycon, rewriteConBind conbind, NONE))
        | DATBIND(i, tyvarlist, tycon, conbind, SOME datbind) =>
            (check i tycon;
	     DATBIND(i, tyvarlist, tycon, 
		     rewriteConBind conbind, SOME (rewriteDatBind(datbind, typbind))))
      end


    fun tuple_atpat_with_info info pats =       (* `(A, B, C)' -> `{1=A, 2=B, 3=C}'. *)
          let
	    fun f (n, pat::pats) =
                  SOME (PATROW (info, mk_IntegerLab n, pat, f (n+1, pats)))
	      | f (_, []) = NONE
	  in
	    RECORDatpat (info, f (1, pats))
	  end

    fun tuple_atpat (pat1::pat2::pats) = 
          tuple_atpat_with_info
	    (span_info (get_info_pat pat1,
			get_info_pat (List.last (pat2::pats))
			handle _ => impossible "tuple_atpat: last"))
	       (pat1::pat2::pats)
      | tuple_atpat _ = impossible "tuple_atpat"

    fun list_atpat info pats =
          let
	    fun f (pat :: pats) =
                  CONSpat (get_info_pat pat,
			   OP_OPT (Ident.idToLongId Ident.id_CONS, false),
			   tuple_atpat [pat, f pats])
	      | f [] = patOfIdent info (Ident.id_NIL, false)
	  in
	    PARatpat (info, f pats)
	  end

    (*layeredPat: analyse "Pat AS Pat", which is what comes out
     of the parser, and turn it into a valid "as"-pattern. NB:
     either Pat might be an UNRES_INFIX (and will be for the
     isolated identifiers) since we haven't done the post-pass
     yet.  MEMO - preserve the "op" for the identifier.*)

    exception LAYERPAT_ERROR of (pos * pos)
    fun layeredPat (i, idPat, asPat) =
          let
	    fun longIdToId longid =
	          (case Ident.decompose longid of
		     (nil, id) => id
		   | ( _ , id) => raise LAYERPAT_ERROR (un_PP i))
	  in
	    case idPat of
	      TYPEDpat (_, ATPATpat (_, LONGIDatpat (_, OP_OPT (id, withOp))), ty) =>
		LAYEREDpat (i, OP_OPT (longIdToId id, withOp), SOME ty, asPat)
	    | ATPATpat (_, LONGIDatpat (_, OP_OPT (id, withOp))) =>
		LAYEREDpat (i, OP_OPT (longIdToId id, withOp), NONE, asPat)
	    | _ => raise LAYERPAT_ERROR (un_PP i)
	  end

    fun tuple_type info tys =
          let
	    fun f (n, ty :: tys) =
                  SOME (TYROW (info, mk_IntegerLab n, ty, f (n+1, tys)))
	      | f (_, []) = NONE
	  in
	    RECORDty (info, f (1, tys))
	  end

    (*rewrite_type_abbreviation_spec: rewrite a derived form of spec,
     Definition, fig. 19:*)

    fun rewrite_type_abbreviation_spec
          (tyvars, tycon, ty, i_tyvars_ty, i_tyvars_tycon) =
	  (i_tyvars_ty, (*info spanning the whole thing*)
	   INCLUDEspec (i_tyvars_ty,
	     WHERE_TYPEsigexp (i_tyvars_ty,
	       SIGsigexp (i_tyvars_tycon,
		 TYPEspec (i_tyvars_tycon,
		   TYPDESC (i_tyvars_tycon,
		     tyvars, tycon, NONE))),
	       tyvars, TyCon.implode_LongTyCon ([],tycon), ty)))

  (*fold_specs_to_spec [(i1,spec1),(i2,spec2),(i3,spec3)] =
   SEQspec(i13, SEQspec(i12, spec1, spec2), spec3)*)

    fun foldl' f [] = NONE
      | foldl' f (h::t) = SOME(foldl f h t)

    fun fold_specs_to_spec i_spec_s =
      case foldl' (fn ((i2, spec2), (i1, spec1)) =>
		   let val i12 = span_info (i1, i2)
		   in (i12, SEQspec (i12, spec1, spec2))
		   end) i_spec_s
	of SOME res => #2 res
	 | NONE => impossible "fold_specs_to_spec"

    fun raise_lexical_error_if_none pos NONE =
          raise LexBasics.LEXICAL_ERROR (pos, "constant too big")
      | raise_lexical_error_if_none pos (SOME a) = a

    val reportPosition_from_info = SourceInfo.report o ParseInfo.to_SourceInfo
  end;
