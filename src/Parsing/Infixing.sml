
functor Infixing(structure InfixBasis: INFIX_BASIS
                 structure GrammarUtils: GRAMMAR_UTILS
                   sharing type GrammarUtils.TopdecGrammar.DecGrammar.id = InfixBasis.id

		 structure Report: REPORT
		 sharing type InfixBasis.Report
		   = GrammarUtils.Report 
		   = Report.Report 

		 structure ParseInfo : PARSE_INFO
		   sharing type ParseInfo.ParseInfo = GrammarUtils.TopdecGrammar.info
		   sharing type ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis

                 structure PP: PRETTYPRINT
                   sharing type GrammarUtils.TopdecGrammar.DecGrammar.StringTree = PP.StringTree
		   sharing type PP.Report = Report.Report

                 structure Crash: CRASH
		   ) : INFIXING =
  struct

    structure List = Edlib.List

    fun impossible s = Crash.impossible ("Infixing." ^ s)
    open GrammarUtils.TopdecGrammar
    open GrammarUtils.TopdecGrammar.DecGrammar

    type Report = Report.Report
    val // = Report.//
    infix //
    exception Error of Report
    exception InfixStack_error of string

    fun error_report info report =
      raise (Error (GrammarUtils.reportPosition_from_info info // report))

    fun error_string info s =
      error_report info (Report.line s)

   (* Two operator precedence stack modules - one for expressions and
      another for patterns. *)

    structure ExpStack =
      InfixStack(structure InfixBasis = InfixBasis

                 type FullObject = exp
                 type AtomObject = atexp

                 type id = Ident.id
                 val pr_id = Ident.pr_id

                 fun atomToFull atexp = ATEXPexp (get_info_atexp atexp, atexp)
                 fun fullToAtom exp = PARatexp (get_info_exp exp, exp)

                 fun pair (exp1, exp2) = GrammarUtils.tuple_atexp [exp1, exp2]

                 fun asId atexp =
                   case atexp
                     of IDENTatexp(_, OP_OPT(longid, withOp)) =>
                          if Ident.unqualified longid andalso not withOp then
                            SOME(Ident.decompose0 longid)
                          else
                            NONE

                      | _ => NONE

                 fun applyId(id, atexp) =
                   let
                     val i = get_info_atexp atexp
                     (* still not quite right: we lack
                        the position information of i *)
                     val identExp =
                       ATEXPexp(
                         i,
                         IDENTatexp(i, OP_OPT(Ident.idToLongId id, false))
                       )
                   in
                     APPexp(i, identExp, atexp)
                   end


                 fun applyObj(exp, atexp) = 
		       APPexp (GrammarUtils.span_info (get_info_exp exp,
						       get_info_atexp atexp),
		               exp, atexp)

		 exception InfixStack_error = InfixStack_error
                 structure Crash = Crash
                )

    structure PatStack =
      InfixStack(structure InfixBasis = InfixBasis

                 type FullObject = pat
                 type AtomObject = atpat

                 type id = Ident.id
                 val pr_id = Ident.pr_id

                 fun atomToFull atpat = ATPATpat(get_info_atpat atpat, atpat)
                 fun fullToAtom pat = PARatpat(get_info_pat pat, pat)

                 fun pair (pat1, pat2) = GrammarUtils.tuple_atpat [pat1, pat2]

                 fun asId atpat =
                   case atpat
                     of LONGIDatpat(_, OP_OPT(longid, withOp)) =>
                          if Ident.unqualified longid andalso not withOp then
                            SOME(Ident.decompose0 longid)
                          else
                            NONE

                      | _ => NONE

                 fun applyId(id, atpat) =
                   (* still not quite right; we should get the position info
                      of id *)
                   let val i = get_info_atpat atpat
                   in CONSpat(i, OP_OPT(Ident.idToLongId id, false), atpat)
                   end

                 fun applyObj(pat, atpat) =
                   (case pat
		      of ATPATpat(_, LONGIDatpat(_, OP_OPT(longid, withOp))) =>
                        let val i = GrammarUtils.span_info (get_info_pat pat,
							    get_info_atpat atpat)
                        in
                          CONSpat(i, OP_OPT(longid, withOp), atpat)
                        end 
		       | _ =>
			error_string (get_info_pat pat) "must be an identifier.")
		 exception InfixStack_error = InfixStack_error
                 structure Crash = Crash
                )

    open InfixBasis
    type InfixBasis = Basis

    infix ++
    val op ++ = compose

    fun isInfix(iBas, id) =
      let
        open InfixBasis
      in
        case lookup iBas id
          of NONFIX => false
           | INFIX _ => true
           | INFIXR _ => true
      end

    fun checkNoInfixes iBas atpat =
      (case atpat
	 of LONGIDatpat(info, OP_OPT(longid, false)) =>
	   let
	     open InfixBasis
	   in
	     (case Ident.decompose longid
		of (nil, id) =>
		  (case lookup iBas id
		     of NONFIX => ()
		      | _ => error_string info ("put `op' in front.")
		       )
		 | _ => ())
	   end
	  | _ => ())
	 
   (* `fun' bindings are a pain in the posterior. The definition (V4, Apdx. B,
      Fig. 20) gives the syntax rules as a footnote (sigh). I've formalised
      them as below. The parser delivers a FUN binding as a sequence (>= 1)
      of atomic patterns, followed by an optional `: <ty>', and `=' and so on.
      Of that general syntax, I permit the following:

        (1) "fun" NonfixID NonfixAP+ (":" Ty)? "=" ...

        (2) "fun" "op" ID NonfixAP+ (":" Ty)? "=" ...

        (3) "fun" "(" NonfixAP InfixID NonfixAP ")" NonfixAP* (":" Ty)? "=" ...

        (4) "fun" NonfixAP InfixID NonfixAP (":" Ty)? "=" ...

      NonfixID is any identifier which isn't an infix. InfixID is an identifier
      with infix status. NonfixAP is any atomic pattern other than an isolated
      identifier which has infix status (that's legal in our parser). ID is
      any identifier (except "=" - look to Topdec_GRAM.src for the gory details
      of what we actually consider an identifier).
    *)

   (* resolveFClauseArgs - takes an infix basis and a list of atomic patterns,
                           and returns the identifier plus its atpat arguments.
                           (We're throwing away the infix status at present,
                           since each branch might have different infix use
                           anyway...) Note that
                           it isn't responsible for resolving the individual
                           atpats. resolveFClauseArgs can fail for an
                           ineligible list of patterns. *)

    fun resolveFClauseArgs(iBas, atpats): id * atpat list =
      let
        datatype Category = INFIXED of id | OTHER

        fun categorise atpat: Category =
          let
            open InfixBasis
          in
            case atpat
              of LONGIDatpat(_, OP_OPT(_, true)) => OTHER       (* has `op'. *)
               | LONGIDatpat(_, OP_OPT(longid, false)) =>
                   (case Ident.decompose longid
                      of (nil, id) => if isInfix(iBas, id)
                                      then INFIXED id else OTHER
                       | _ => OTHER                     (* qualified. *)
                   )

               | _ => OTHER                             (* complex pattern. *)
          end

        fun mustDecompose longid info =
          (case Ident.decompose longid
	     of (nil, id) => id
	      | _ => error_string info "you cannot have a long identifier after `fun'.")

        fun pair (atpat1, atpat2) =
	      GrammarUtils.tuple_atpat
	        (map GrammarUtils.patOfAtpat [atpat1, atpat2])

	val info_of_first_atpat =
	      (case atpats of
		 atpat::atpats => get_info_atpat atpat
	       | [] => impossible "resolveFClauseArgs")
      in
        (case map categorise atpats
          of [OTHER, INFIXED id, OTHER] =>      (* SUCCESS: matches (4). *)
               (case atpats
                  of [ap1, _, ap2] => (id, [pair(ap1, ap2)])

                   | _ => impossible "resolveClauseArgs")

           | OTHER :: _ =>                      (* Try for case (1)/(2)/(3): *)
               (case atpats
                  of PARatpat(info, UNRES_INFIXpat(_, [ap1, ap2, ap3])) :: rest =>
                                                (* Try for case (3)... *)
                       (case categorise ap2
                          of INFIXED id =>      (* SUCCESS: matches (3). *)
                               (id, pair(ap1, ap3) :: rest)

                           | OTHER =>           (* `fun (<ap1> <junk> <ap3>)' *)
			       error_string (get_info_atpat ap2)
			       "that is not an infix identifier.")

                   | fst :: snd :: rest =>      (* Try for case (1)/(2)... *)
                       (case fst
                          of LONGIDatpat(info, OP_OPT(longid, withOp)) =>
                               (mustDecompose longid info, snd :: rest)
                                                (* `longid' can't be an infix
                                                   because it matches OTHER. *)

                           | _ =>               (* `fun <junk> <junk> ...' *)
			       error_string info_of_first_atpat
			       "I expect a function declaration.")

                   | _ =>                       (* `fun <ap> = ...' *)
			  error_string info_of_first_atpat "where are the arguments?")

           | INFIXED id :: _ =>                 (* `fun +' or something. *)
		  error_string info_of_first_atpat
		    "put `op' in front."
	   | _ => impossible "resolveFClauseArgs")
      end

   (* resolveFClause - turn an FClause into an id plus list of argument
                       lists, RHS's and type constraints. All the id's must
                       agree, of course. It's here that we also run through
                       the arguments, resolving them as well, and resolve the
                       RHS exp. *)

    and resolveFClause (iBas, fclause)
        : id * (atpat list * exp * ty option) list =
      case fclause
        of FCLAUSE(info, atpats, ty_opt, exp, fclause_opt) =>
             let
               val (id, args) = resolveFClauseArgs(iBas, atpats)
               val _ = List.apply (checkNoInfixes iBas) args
               val args' = map (fn a => resolveAtpat(iBas, a)) args
               val exp' = resolveExp(iBas, exp)
             in
               case fclause_opt
                 of SOME fclause' =>
                      let
                        val (id', rest) = resolveFClause (iBas, fclause')
                      in
                        if id = id' then
                          (id, (args', exp', ty_opt) :: rest)
                        else
			  error_string info (Ident.pr_id id ^ " and "
					     ^ Ident.pr_id id' ^ " should be the same.")
                      end
                  | NONE =>
                      (id, [(args', exp', ty_opt)])
             end


   (* resolveFValBind - turn the form `FUN <FValBind>' into a dec. There's
                        an enclosing `VAL REC', and then each FValBind
                        gives rise to a single fn, whose branches are built from
                        FClauses (which must have identical identifiers). *)

    and resolveFValBind (iBas, fvalbind) =
      let
        fun probableTupleAtExp [] = impossible "probableTupleAtExp []"
	  | probableTupleAtExp [atexp] = atexp
	  | probableTupleAtExp atexps =
	      GrammarUtils.tuple_atexp (map GrammarUtils.expOfAtexp atexps)

        fun probableTuplePat atpats =
              GrammarUtils.patOfAtpat
	        (case atpats of
		   [] => impossible "probableTuplePat []"
		 | [atpat] => atpat
		 | atpats =>
		     GrammarUtils.tuple_atpat (map GrammarUtils.patOfAtpat atpats))

        exception NOT_SIMPLE

        (* a fvalbind of the special form

                vid atpat_1 ... atpat_n <:ty> = exp

           in which every atpat_i is certain to be matched is transformed into

               vid = fn atpat_1 => ... fn atpat_n = exp <:ty>

        *)

        fun try_simple_fvalbindToValbind(info_fvalbind,id,rhsList: (atpat list * exp * ty option) list) = 
          let
            fun certainMatch_atpat (LONGIDatpat _) = true
              | certainMatch_atpat (PARatpat(_,pat)) = certainMatch_pat pat
              | certainMatch_atpat (WILDCARDatpat _) = true
              | certainMatch_atpat (SCONatpat  _) = false
              | certainMatch_atpat (RECORDatpat(_,NONE)) = true
              | certainMatch_atpat (RECORDatpat(_,SOME patrow)) = certainMatch_patrow patrow

            and certainMatch_patrow (DOTDOTDOT _) = true
              | certainMatch_patrow (PATROW(_,_,pat,NONE)) =certainMatch_pat pat
              | certainMatch_patrow (PATROW(_,_,pat,SOME patrow)) =
              certainMatch_pat pat andalso certainMatch_patrow patrow
         
            and certainMatch_pat(ATPATpat(_,atpat)) = certainMatch_atpat atpat
              | certainMatch_pat(CONSpat(_,atpat,_)) = false
              | certainMatch_pat(TYPEDpat(_,pat,ty)) = certainMatch_pat pat
              | certainMatch_pat(LAYEREDpat(_,_,_,pat)) = certainMatch_pat pat
              | certainMatch_pat(UNRES_INFIXpat  _) = false

          in
            case rhsList of
              [(atpats,exp,ty_opt)] => 
                if List.forAll certainMatch_atpat atpats
                  then
                    let
                        val exp' =
                          case ty_opt
                            of SOME ty => TYPEDexp (GrammarUtils.span_info
						      (get_info_ty ty,get_info_exp exp),
                                                    exp, ty)
                             | NONE => exp

                        fun curry atpat exp =
                          let val info_exp = get_info_exp exp
                              val info_atpat = get_info_atpat atpat
                              val info_fn = GrammarUtils.span_info(info_atpat, info_exp)
                          in
                            FNexp(info_fn,
                                  MATCH(info_fn,
                                        MRULE(info_fn,ATPATpat(info_atpat,atpat),exp),NONE))
                          end
                        val rhs = List.foldR curry exp' atpats
                    in
                      PLAINvalbind
                      (info_fvalbind, GrammarUtils.patOfIdent info_fvalbind 
                       (id, isInfix (iBas, id)),
                       rhs,
                       NONE)
                    end
                else raise NOT_SIMPLE
            | _ => raise NOT_SIMPLE
          end

        fun fvalbindToValbind (fvalbind as FVALBIND(info, fclause, _)) =
            let 
              val (id, rhsList: (atpat list * exp * ty option) list) = 
                  resolveFClause (iBas, fclause)
            in
              try_simple_fvalbindToValbind(info,id,rhsList) 
              handle NOT_SIMPLE =>
              let
(*                val _ = output(std_out, Ident.pr_id id ^ " is not simple\n")*)
                val numArgs = (* changed back, 31/3/97, mads*)
                  case rhsList
                    of (atpats, _, _) :: _ => List.size atpats
                     | _ => impossible "Infixing.fvalbindToValbind"

                val _ = 
                  (* Lars: check that the number of atpats is the same in every rhs 
                   * (c.f. Def. p. 72)
                   *)
                  let 
                    val rhsListNums = map (List.size o #1) rhsList
                  in
                    List.apply (fn n => 
                                if n = numArgs then () 
                                else
				  error_string info "the clauses must have the same\
				                    \ number of arguments.")
                    rhsListNums
                  end

                fun inventVars ([(atpats,_,_)]) = (* only one clause *)
                      map (fn atpat =>
			   (GrammarUtils.inventId_from_atpat atpat,
			    get_info_atpat atpat))
		        atpats
                  | inventVars ((atpats,_,_)::_) = (* more than one clause *)
                      map (fn atpat =>
			   (GrammarUtils.inventId (), get_info_atpat atpat))
		        atpats
                  | inventVars [] = Crash.impossible "Infixing.inventVars: no\
                       \ clauses in fvalbind"

                  
                val vars = inventVars rhsList
		  (*the newly invented vars are paired with the info from the atpats
		   they stand for to give better infos where they are used*)

                val varTuple =
                      probableTupleAtExp
		        (map (fn (id, i_id) => GrammarUtils.atexpOfIdent i_id id) vars)
(*KILL 06/01/1998 20:49. tho.:
		  (*Using `info' here is somewhat arbitrary; before, an empty info
		   was used.  It is not possible to put any really meaningful info
		   on these newly invented atexps, dixi.*)
*)

                fun mkMatch((atpats, exp, ty_opt) :: rest) =
                      let
                        val exp' =
                          case ty_opt
                            of SOME ty => TYPEDexp (GrammarUtils.span_info
						      (get_info_exp exp, get_info_ty ty),
                                                    exp, ty)
                             | NONE => exp

                        val pat' = probableTuplePat atpats
                        val i_mrule = GrammarUtils.span_info (get_info_pat pat',
							      get_info_exp exp')
                        val rest' = mkMatch rest
                        val i_m = 
                               case rest' of
                                  NONE => i_mrule
                               | SOME m => GrammarUtils.span_info (i_mrule, get_info_match m)
                      in
                        SOME(MATCH(i_m,
                                   MRULE(i_mrule, pat', exp'),
                                   rest'
                                  )
                            )
                      end
                  | mkMatch [] = NONE

                val innerApp =
                  case mkMatch rhsList
                    of SOME m => 
                          let val i_match = get_info_match m
                              val i_app = GrammarUtils.span_info
				            (i_match, get_info_atexp varTuple)
                          in
                              APPexp(i_app, FNexp(i_match, m), varTuple)
                          end
                     | NONE => impossible "fvalbindToValbind(innerApp)"

                fun curry (id, i_id) exp =
                  (*The handling of the sourcinfo is not ideal, here.
		   The abstraction and other syntax nodes get the same source
		   info as the body*)
                  let val i_body = get_info_exp exp 
                  in 
		    FNexp (i_body,
		      MATCH (i_body,
			MRULE (GrammarUtils.span_info (i_id,
						       get_info_exp exp),
			       GrammarUtils.patOfIdent i_id (id, false),
			       exp),
			NONE))
                  end

                val curriedFn =
                  List.foldL curry innerApp (rev vars)
              in
                PLAINvalbind
		  (info, GrammarUtils.patOfIdent info 
		   (id, isInfix (iBas, id)),
		   curriedFn,
		   NONE)
              end 
            end (*fvalbindToValbind*)

        fun resolveAll (fvalbind as FVALBIND (_, _, rest)) =
	      (case fvalbindToValbind fvalbind of
		 PLAINvalbind (i, id, exp, NONE) =>
		   PLAINvalbind (i, id, exp,
				 (case rest of
				    SOME fvalbind' =>
				      SOME (resolveAll fvalbind')
				  | NONE => NONE))
	       | _ => impossible "resolveFValBind.resolveAll")

	val valbind = resolveAll fvalbind
      in
        RECvalbind (get_info_valbind valbind, valbind)
      end (*resolveFValBind, maybe*)

   (* resolveTopdec - walk over a topdec, resolving infix patterns, expressions
      and `FUN'-bindings. Infix/nonfix declarations are analysed and scoped as
      well. The result of resolveInfixes contains the new infix basis built
      from any infix/nonfix declarations at this scope. Note: the iBas argument
      to each resolveXXX function is the entire basis, but the iBas result
      returned by any resolveXXX function contains the fixity declarations
      established by that phrase only. Some resolveXXX functions don't make
      any alterations to the infix basis - in this case, we only return the
      re-written phrase. *)

    and resolveTopdec(iBas, topdec): Basis * topdec =
     (case topdec of
	STRtopdec(i, strdec, topdec_opt) =>
	  let
	    val (iBas', strdec') = resolveStrdec(iBas, strdec)
	    val (iBas'', topdec_opt') = resolveTopdec_opt (iBas ++ iBas', topdec_opt)
	  in
	    (iBas' ++ iBas'', STRtopdec(i, strdec', topdec_opt'))
	  end
	
      | SIGtopdec(i, sigdec, topdec_opt) =>
	  let
	    val (iBas', topdec_opt') = resolveTopdec_opt (iBas, topdec_opt)
	  in
	    (iBas', SIGtopdec(i, sigdec, topdec_opt'))
	  end
	
      | FUNtopdec(i, fundec, topdec_opt) =>
	  let
	    val fundec' = resolveFundec(iBas, fundec)
	    val (iBas', topdec_opt') = resolveTopdec_opt (iBas, topdec_opt)
	  in
	    (iBas', FUNtopdec(i, fundec', topdec_opt'))
	  end)

    and resolveTopdec_opt (iBas, SOME topdec) : Basis * topdec option =
          let
	    val (iBas', topdec') = resolveTopdec (iBas, topdec)
	  in
	    (iBas', SOME topdec')
	  end
      | resolveTopdec_opt (iBas, NONE) = (emptyB, NONE)


    and resolveStrdec(iBas, strdec) =
      case strdec
        of DECstrdec(i, dec) =>
             let
               val (iBas', dec') = resolveDec(iBas, dec)
             in
               (iBas', DECstrdec(i, dec'))
             end

         | STRUCTUREstrdec(i, strbind) =>
             (emptyB, STRUCTUREstrdec(i, resolveStrbind(iBas, strbind)))

         | LOCALstrdec(i, strdec1, strdec2) =>
             let
               val (iBas', strdec1') = resolveStrdec(iBas, strdec1)
               val (iBas'', strdec2') = resolveStrdec(iBas ++ iBas', strdec2)
             in
               (iBas'', LOCALstrdec(i, strdec1', strdec2'))
             end

         | EMPTYstrdec i =>
             (emptyB, EMPTYstrdec i)

         | SEQstrdec(i, strdec1, strdec2) =>
             let
               val (iBas', strdec1') = resolveStrdec(iBas, strdec1)
               val (iBas'', strdec2') = resolveStrdec(iBas ++ iBas', strdec2)
             in
               (iBas' ++ iBas'', SEQstrdec(i, strdec1', strdec2'))
             end

    and resolveFundec(iBas, FUNCTORfundec(i, funbind)) =
          FUNCTORfundec(i, resolveFunbind(iBas, funbind))

    and resolveFunbind(iBas, FUNBIND(i, funid, strid, sigexp,
				     strexp, funbind_opt)) =
      let

          (* We record the infix basis here, so that we can later
	   * fetch it and put it into a functor closure. The idea is
	   * that we can then parse, infix-resolve and elaborate the
	   * body of a functor again - and achieve the same
	   * result. This will then save space in the compiler. *)

	  val i' = ParseInfo.plus_DFInfo i (ParseInfo.DFInfo.INFIX_BASIS iBas)

      in FUNBIND(i', funid, strid, sigexp,
		 resolveStrexp(iBas, strexp),
		 (case funbind_opt of
		    SOME funbind => SOME(resolveFunbind(iBas, funbind))
		  | NONE => NONE))
      end

    and resolveStrbind(iBas, STRBIND(i, strid, strexp, strbind_opt)) =
          STRBIND(i, strid, resolveStrexp(iBas, strexp),
		  (case strbind_opt of
		     SOME strbind => SOME(resolveStrbind(iBas, strbind))
		   | NONE => NONE))

    and resolveStrexp(iBas, strexp) =
      (case strexp
        of STRUCTstrexp(i, strdec) =>
             let
               val (_, strdec') = resolveStrdec(iBas, strdec)
             in
               STRUCTstrexp(i, strdec')
             end

	 | TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	     TRANSPARENT_CONSTRAINTstrexp
	     (i, resolveStrexp(iBas, strexp), sigexp)
	     
	 | OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) =>
	     OPAQUE_CONSTRAINTstrexp(i, resolveStrexp(iBas, strexp), sigexp)

         | LONGSTRIDstrexp(i, longstrid) =>
             LONGSTRIDstrexp(i, longstrid)

         | APPstrexp(i, funid, strexp) =>
             APPstrexp(i, funid, resolveStrexp(iBas, strexp))

         | LETstrexp(i, strdec, strexp) =>
             let
               val (iBas', strdec') = resolveStrdec(iBas, strdec)
             in
               LETstrexp(i, strdec', resolveStrexp(iBas ++ iBas', strexp))
             end)


   (* Core level *)

    and resolveDec(iBas, dec): Basis * dec =
      case dec
        of VALdec(i, tyvars, valbind) =>
  	     (emptyB, VALdec(i, tyvars, resolveValbind(iBas, valbind)))

         | UNRES_FUNdec(i, tyvars, fvalbind) =>
             (emptyB, VALdec(i, tyvars, resolveFValBind(iBas, fvalbind)))

         | TYPEdec(i, typbind) =>
             (emptyB, TYPEdec(i, typbind))
                                (* typbinds don't need resolving -
                                   they don't know or care about infixing. *)

         | DATATYPEdec(i, datbind) =>
             (emptyB, DATATYPEdec(i, resolveDatbind(iBas, datbind)))

	 | DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
	     (emptyB, DATATYPE_REPLICATIONdec(i, tycon, longtycon))
	     
         | ABSTYPEdec(i, datbind, dec) =>
             let
               val datbind' = resolveDatbind(iBas, datbind)
               val (iBas', dec') = resolveDec(iBas, dec)
             in
               (iBas', ABSTYPEdec(i, datbind', dec'))
             end

         | EXCEPTIONdec(i, exbind) =>
             (emptyB, EXCEPTIONdec(i, resolveExbind(iBas, exbind)))

         | LOCALdec(i, dec1, dec2) =>
             let
               val (iBas', dec1') = resolveDec(iBas, dec1)
               val (iBas'', dec2') = resolveDec(iBas ++ iBas', dec2)
             in
               (iBas'', LOCALdec(i, dec1', dec2'))
             end

         | OPENdec(i, list) =>
             (emptyB, OPENdec(i, list))

         | SEQdec(i, dec1, dec2) =>
             let
               val (iBas', dec1') = resolveDec(iBas, dec1)
               val (iBas'', dec2') = resolveDec(iBas ++ iBas', dec2)
             in
               (iBas' ++ iBas'', SEQdec(i, dec1', dec2'))
             end

         | INFIXdec(i, int_opt, ids) =>
             let
               val newBas =
                 new(ids, INFIX(case int_opt of SOME n => n | NONE => 0))
             in
               (newBas, INFIXdec(i, int_opt, ids))
             end

         | INFIXRdec(i, int_opt, ids) =>
             let
               val newBas =
                 new(ids, INFIXR(case int_opt of SOME n => n | NONE => 0))
             in
               (newBas, INFIXRdec(i, int_opt, ids))
             end

         | NONFIXdec(i, ids) =>
             let
               val newBas = new(ids, NONFIX)
             in
               (newBas, NONFIXdec(i, ids))
             end

         | EMPTYdec i =>
             (emptyB, EMPTYdec i)

    and resolveValbind(iBas, valbind) =
      case valbind
        of PLAINvalbind(i, pat, exp, valbind_opt) =>
             PLAINvalbind(i, resolvePat(iBas, pat),
                             resolveExp(iBas, exp),
                             case valbind_opt
                               of SOME valbind =>
                                    SOME(resolveValbind(iBas, valbind))
                                | NONE => NONE
                         )

         | RECvalbind(i, valbind) =>
             RECvalbind(i, resolveValbind(iBas, valbind))

   (* resolveDatbind - we don't actually need to resolve it at all - it's
      unambiguous - but we are required to check occurrences of `op' against
      infixed operators. *)

    and resolveDatbind(iBas, datbind) =
      datbind                           (* MEMO: incomplete (no checks yet) *)

   (* resolveExbind - same treatment as resolveDatbind. *)

    and resolveExbind(iBas, exbind) =
      exbind                            (* MEMO: incomplete (no checks yet) *)

    and resolveExp(iBas, exp) =
      case exp
        of ATEXPexp(i, atexp) =>
             ATEXPexp(i, resolveAtexp(iBas, atexp))

         | APPexp(i,  exp, atexp) =>
             APPexp(i, resolveExp(iBas, exp), resolveAtexp(iBas, atexp))

         | TYPEDexp(i, exp, ty) =>
             TYPEDexp(i, resolveExp(iBas, exp), ty)

         | HANDLEexp(i, exp, match) =>
             HANDLEexp(i, resolveExp(iBas, exp), resolveMatch(iBas, match))

         | RAISEexp(i, exp) =>
             RAISEexp(i, resolveExp(iBas, exp))

         | FNexp(i, match) =>
             FNexp(i, resolveMatch(iBas, match))

         | UNRES_INFIXexp(i, atexps) =>
             (ExpStack.resolveInfix(iBas,
				    map (fn a => resolveAtexp(iBas, a))
				      atexps
				    ))
	     handle InfixStack_error s => error_string i s

    and resolveMatch(iBas, match) =
      case match
        of MATCH(i, mrule, match_opt) =>
             MATCH(i, resolveMrule(iBas, mrule),
                      case match_opt
                        of SOME match => SOME(resolveMatch(iBas, match))
                         | NONE => NONE
                  )

    and resolveMrule(iBas, mrule) =
      case mrule
        of MRULE(i, pat, exp) =>
             MRULE(i, resolvePat(iBas, pat), resolveExp(iBas, exp))

    and resolvePat(iBas, pat) =
      case pat
        of ATPATpat(i, atpat) =>
             ATPATpat(i, resolveAtpat(iBas, atpat))

         | CONSpat(i, longidOp_opt, atpat) =>
             CONSpat(i, longidOp_opt, resolveAtpat(iBas, atpat))

         | TYPEDpat(i, pat, ty) =>
             TYPEDpat(i, resolvePat(iBas, pat), ty)

         | LAYEREDpat(i, idOp_opt, ty_opt, pat) =>
             LAYEREDpat(i, idOp_opt, ty_opt, resolvePat(iBas, pat))

         | UNRES_INFIXpat(i, atpats) =>
             (PatStack.resolveInfix(iBas,
				    map (fn a => resolveAtpat(iBas, a))
                                        atpats
				   ))
	     handle InfixStack_error s => error_string i s

    and resolveAtexp(iBas, atexp) =
      case atexp
        of SCONatexp _ => atexp
         | IDENTatexp _ => atexp

         | RECORDatexp(i, exprow_opt) =>
             RECORDatexp(i,
                         case exprow_opt
                           of SOME exprow => SOME(resolveExprow(iBas, exprow))
                            | NONE => NONE
                        )

         | LETatexp(i, dec, exp) =>
             let
               val (iBas', dec') = resolveDec(iBas, dec)
             in
               LETatexp(i, dec', resolveExp(iBas ++ iBas', exp))
             end

         | PARatexp(i, exp) =>
             PARatexp(i, resolveExp(iBas, exp))

    and resolveExprow(iBas, exprow) =
      case exprow
        of EXPROW(i, lab, exp, exprow_opt) =>
             EXPROW(i, lab, resolveExp(iBas, exp),
                    case exprow_opt
                      of SOME exprow => SOME(resolveExprow(iBas, exprow))
                       | NONE => NONE
                   )

    and resolveAtpat(iBas, atpat) =
      case atpat
        of WILDCARDatpat _ => atpat
         | SCONatpat _ => atpat
         | LONGIDatpat _ => atpat

         | RECORDatpat(i, patrow_opt) =>
             RECORDatpat(i,
                         case patrow_opt
                           of SOME patrow => SOME(resolvePatrow(iBas, patrow))
                            | NONE => NONE
                        )

         | PARatpat(i, pat) =>
             PARatpat(i, resolvePat(iBas, pat))

    and resolvePatrow(iBas, patrow) =
      case patrow
        of DOTDOTDOT _ =>
             patrow

         | PATROW(i, lab, pat, patrow_opt) =>
             PATROW(i, lab, resolvePat(iBas, pat),
                    case patrow_opt
                      of SOME patrow => SOME(resolvePatrow(iBas, patrow))
                       | NONE => NONE
                   )

    datatype 'a result = SUCCESS of 'a | FAILURE of Report

    fun resolve ((iBas, topdec):(InfixBasis * topdec))
         : (InfixBasis * topdec) result =
      (SUCCESS (resolveTopdec (iBas, topdec)))
      handle Error report => FAILURE report
  end;
