(* Elaborator for Core Language Declarations*)

structure ElabDec: ELABDEC =
  struct
    structure IG = PreElabDecGrammar
    structure OG = PostElabDecGrammar
    structure PP = PrettyPrint
    structure ParseInfo = AllInfo.ParseInfo
    structure ElabInfo = AllInfo.ElabInfo

    structure ListHacks =
      struct
        fun member x [] = false
          | member x (y::ys) = x=y orelse member x ys

        fun union (set1, set2) =
          set1 @ List.filter (fn x => not(member x set1)) set2

        fun intersect (set1, set2) =
          List.filter (fn x => member x set1) set2

        fun minus (set1, set2) =
          List.filter (fn x => not(member x set2)) set1
      end

    fun impossible s = Crash.impossible ("ElabDec." ^ s)
    fun noSome NONE s = impossible s
      | noSome (SOME x) s = x

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
         type Type         = StatObject.Type
         type TyVar        = StatObject.TyVar
         type RecType      = StatObject.RecType
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
         type Substitution = StatObject.Substitution
    structure Substitution = StatObject.Substitution
    structure TypeFcn      = StatObject.TypeFcn
    structure Realisation  = StatObject.Realisation

    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure constructor_map = Environments.constructor_map

    (*import from other modules:*)
    structure ErrorInfo = ElabInfo.ErrorInfo
    structure OverloadingInfo = ElabInfo.OverloadingInfo
    structure TypeInfo = ElabInfo.TypeInfo
    type id = Ident.id
    type ParseInfo  = ParseInfo.ParseInfo
    type ElabInfo = ElabInfo.ElabInfo
    type TyName = TyName.TyName

    val values_64bit = Flags.is_on0 "values_64bit"

    (*info*)

    (*okConv ParseInfo = the ParseInfo converted to ElabInfo with
     empty Error and Type fields:*)

    val okConv = ElabInfo.from_ParseInfo

    (*errorConv (ParseInfo, ErrorInfo) = the ParseInfo converted
     to ElabInfo with the error info ErrorInfo. The assumption
     (forced by this type conversion) is that there can only be
     one error tag per info:*)

    fun errorConv (i : ParseInfo, e : ErrorInfo.ErrorInfo) : ElabInfo =
          ElabInfo.plus_ErrorInfo (okConv i) e

    (*preOverloadingConv: as okConv, except it includes some
     OverloadingInfo:*)

    fun preOverloadingConv (i : ParseInfo, oi : OverloadingInfo.OverloadingInfo)
          : ElabInfo = ElabInfo.plus_OverloadingInfo (okConv i) oi

    fun lookupIdError (i : ParseInfo, longid : Ident.longid) : ElabInfo =
          errorConv (i, ErrorInfo.LOOKUP_LONGID longid)

    fun lookupTyConError (i : ParseInfo, longtycon : Environments.longtycon)
          : ElabInfo = errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon)

    fun repeatedIdsError (i : ParseInfo, rids: ErrorInfo.RepeatedId list)
          : ElabInfo = errorConv (i, ErrorInfo.REPEATED_IDS rids)

    (*infixes*)
    val on = Substitution.on     infixr on
    val onC = C.on     infixr onC
    val onVE = VE.on     infixr onVE
    infixr oo    val op oo = Substitution.oo

    fun C_plus_E a = C.plus_E a   infixr C_plus_E
    fun C_plus_TE a = C.plus_TE a  infixr C_plus_TE
    fun C_plus_VE_and_TE a = C.plus_VE_and_TE a   infixr C_plus_VE_and_TE

    (*types needed for the signature ELABDEC*)
    type PreElabDec  = IG.dec
    type PostElabDec = OG.dec
    type PreElabTy   = IG.ty
    type PostElabTy  = OG.ty

    fun pr (msg : string, t : PP.StringTree) : unit =
          Report.print (Report.decorate (msg, PP.reportStringTree t))

    fun pr_st st = (PP.outputTree(print,st,100); print "\n")

    fun getRepeatedElements equal ls =
          let
            fun NoOfOccurences x [] = 0
              | NoOfOccurences x (y::ys) =
                  if equal (x, y) then 1 + NoOfOccurences x ys
                  else NoOfOccurences x ys
          in
            List.filter (fn e => (NoOfOccurences e ls) > 1) ls
          end
    fun isEmptyTyVarList x = case x of nil => true | _ => false
    fun memberTyVarList x xs = List.exists (fn y => TyVar.eq (x,y)) xs

    local
      fun count _ [] _ = NONE
        | count p (x::xs) n = if p x then SOME n
                              else count p xs (n + 1)
    in
      fun index p l = count p l 0
    end

    fun where' list elem =
      case index (fn a => a=elem) list
        of SOME n => n
         | NONE => impossible "where'"

    (* Hooks needed by the compiler:

          o see signature TYPEINFO

     *)

    local open ElabInfo.TypeInfo
    in
     (* MEMO: no duplication checks here (or anywhere else!) *)
      fun addTypeInfo_CON (ElabInfo, C, instances, longid) =
            let
              val (_, con) = Ident.decompose longid
              val cons = C.lookup_fellow_constructors C longid
            in
              ElabInfo.plus_TypeInfo ElabInfo
                (CON_INFO {numCons=length cons,
                           index=where' cons con,instances=instances,
                           longid=longid})
            end

      fun addTypeInfo_EXCON (ElabInfo, tau, longid : Ident.longid) =
            (*The excon carries a value if the type tau is
             functional, and doesn't if it's `exn'.*)
            ElabInfo.plus_TypeInfo ElabInfo
              (EXCON_INFO {Type=tau, longid=longid})

      fun addTypeInfo_EXBIND (ElabInfo, typeOpt) = (*martin*)
            ElabInfo.plus_TypeInfo ElabInfo (EXBIND_INFO {TypeOpt=typeOpt})

      fun addTypeInfo_LAB (ElabInfo, index) =
            ElabInfo.plus_TypeInfo ElabInfo (LAB_INFO {index=index})

      fun addTypeInfo_RECORD_ATPAT (ElabInfo, Type) =
            ElabInfo.plus_TypeInfo ElabInfo (RECORD_ATPAT_INFO {Type=Type})

      fun addTypeInfo_EXP (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (EXP_INFO {Type=tau})

      fun addTypeInfo_MATCH (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (MATCH_INFO {Type=tau})

      fun addTypeInfo_VAR (ElabInfo, instances) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_INFO {instances=instances})

      fun addTypeInfo_VAR_PAT (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_PAT_INFO {tyvars=[], Type=tau})

      fun addTypeInfo_TYENV (ElabInfo, TE : TyEnv) =
            ElabInfo.plus_TypeInfo ElabInfo (TYENV_INFO TE)

      fun addTypeInfo_ABSTYPE (ElabInfo, (TE : TyEnv, rea : realisation)) =
            ElabInfo.plus_TypeInfo ElabInfo (ABSTYPE_INFO (TE,rea))

      fun addTypeInfo_PLAINvalbind (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo
              (PLAINvalbind_INFO {Type=tau, tyvars=[]})
    end


   (*Type_bogus () = when we get elaboration errors we often need to return a
    bogus type.  It's best if that's just a fresh type variable:*)

    val Type_bogus = Type.fresh_normal

    fun Unify (tau, tau', i): Substitution * ElabInfo =
      case Type.unify {unify_regvars=false} (tau, tau')
        of Type.UnifyOk => (Substitution.Id, okConv i)  (* substitutions are dummies *)
         | Type.UnifyFail _ =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION(tau, tau'))
             )
         | Type.UnifyRankError(tv,tn) =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_RANK(tau, tau', tv, tn))
             )

    fun UnifyWithTexts0 {unify_regvars:bool} (text,tau,text', tau', i): Substitution * ElabInfo =
      case Type.unify {unify_regvars=unify_regvars} (tau, tau')
        of Type.UnifyOk => (Substitution.Id, okConv i)
         | Type.UnifyFail _ =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_TEXT(text,tau,text', tau'))
             )
         | Type.UnifyRankError(tv,tn) =>
             (Substitution.Id,
              errorConv(i, ErrorInfo.UNIFICATION_RANK(tau, tau', tv, tn))
             )

    fun UnifyWithTexts a = UnifyWithTexts0 {unify_regvars=false} a
(*    fun UnifyWithTexts' a = UnifyWithTexts0 {unify_regvars=true} a *)


   (* Traversal of patterns to build a VE. We only do this for `rec'
      bindings. Arguably, we should reject records and so on right here,
      but the caller isn't set up to deal with that (and ignoring them
      would give unbound var errors for actual occuring vars), so we work for
      all patterns. We reject illegal pattern forms in the `rec' post-pass. *)

   (*MEMO: these should all be returning sets. *)

    (********
    Find the variables bound in a valbind
    *********
    There is no checking for multiply declared variables
    ********)

    fun dom_vb (C: Context, vb: IG.valbind): id list =
      case vb
        of IG.PLAINvalbind(_, pat, _, vb_opt) =>
             C.dom_pat (C, pat, true)
             @ (case vb_opt of
                  SOME vb => dom_vb (C, vb)
                | NONE => nil)

         | IG.RECvalbind(_, vb) =>
             dom_vb(C, vb)

    (*initial_TE datbind = the TE to be used initially in
     elaborating a datbind. We determine the correct equality
     attributes when we maximise equality:*)

    fun initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, NONE)) =
          TE.init explicittyvars tycon
      | initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, SOME datbind)) =
          TE.plus (TE.init explicittyvars tycon, initial_TE datbind)


    (* addLabelIndexInfo: given a Type and a PATROW, populate the info
     * fields with the index of each label (needed by the
     * compiler). The index info is recoreded during overloading
     * resolvation.  The reason that the correct indexes are recorded
     * later is that only when the type is resolved (so that record
     * variables have been turned into record types without record
     * variables) do we know the correct index: consider fn {2 = x,
     * ...} => x; here we do not know the index of 2 until we have
     * found out what ... stands for (e.g., if it stands for { 1 = _ }
     * the index of 2 should be 1 but if it stands for { 3 = _ } the
     * index of 2 should be 0). *)

    fun addLabelIndexInfo (Type,patrow) =
      let
        val recType = #1 (noSome (Type.to_RecType Type) "addLabelIndexInfo")

        val labtys = Type.RecType.to_list recType
        val sortedLabs = map (#1) labtys
        fun f(OG.PATROW(i, lab, pat, patrow_opt)) =
          (case ElabInfo.to_ErrorInfo i
             of NONE =>
               let val index = where' sortedLabs lab
               in OG.PATROW (ElabInfo.plus_TypeInfo i (TypeInfo.LAB_INFO {index=index}),
                             lab, pat, Option.map f patrow_opt)
               end
              | SOME _ => OG.PATROW(i, lab, pat, patrow_opt)
               )
          | f(OG.DOTDOTDOT i) = OG.DOTDOTDOT i
      in
        f patrow
      end


    (* Insert type information in valbind; generalised type variables
     * are collected from annotated type information in the patterns. *)

    fun insert_type_info_in_valbind (VE : VarEnv, valbind: OG.valbind) : OG.valbind =
      let
        val generic_tyvars_pat = ref nil
        fun add_tyvars tvs = generic_tyvars_pat := TyVar.unionTyVarSet (!generic_tyvars_pat, tvs)
        fun do_pat pat =
          case pat
            of OG.ATPATpat(i, atpat) => OG.ATPATpat(i,do_atpat atpat)
             | OG.CONSpat(i, longid_op, atpat) => OG.CONSpat(i, longid_op, do_atpat atpat)
             | OG.TYPEDpat(i, pat, ty) => OG.TYPEDpat(i, do_pat pat, ty)
             | OG.LAYEREDpat(i, id_op as OG.OP_OPT(id, withOp),ty_opt, pat) =>
              let val i' =
                    case VE.lookup VE id
                      of SOME(VE.LONGVAR sigma) =>
                        let val (tyvars, _, Type) = TypeScheme.to_TyVars_and_Type sigma
                            val _ = add_tyvars tyvars
                        in ElabInfo.plus_TypeInfo i (TypeInfo.VAR_PAT_INFO{tyvars=tyvars,Type=Type})
                        end
                       | _ => i
              in OG.LAYEREDpat(i', id_op, ty_opt, do_pat pat)
              end
             | OG.UNRES_INFIXpat _ => impossible "do_pat(UNRES_INFIX)"

        and do_atpat atpat =
          case atpat
            of OG.WILDCARDatpat _ => atpat
             | OG.SCONatpat _ => atpat
             | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid,withOp), regvars_opt) =>
              let val i' =
                    case Ident.decompose longid
                      of ([],id) =>
                        (case VE.lookup VE id
                           of SOME(VE.LONGVAR sigma) =>
                             let val (tyvars, _, Type) = TypeScheme.to_TyVars_and_Type sigma
                                 val _ = add_tyvars tyvars
                             in ElabInfo.plus_TypeInfo i (TypeInfo.VAR_PAT_INFO{tyvars=tyvars,Type=Type})
                             end
                            | _ => i)
                       | _ => i
              in OG.LONGIDatpat(i', longid_op, regvars_opt)
              end
             | OG.RECORDatpat(i, patrowOpt) => OG.RECORDatpat (i, Option.map do_patrow patrowOpt)
             | OG.PARatpat(i, pat) => OG.PARatpat(i, do_pat pat)

        and do_patrow patrow =
          case patrow
            of OG.DOTDOTDOT _ => patrow
             | OG.PATROW(i, l, pat, patrowOpt) => OG.PATROW(i, l, do_pat pat, Option.map do_patrow patrowOpt)

        fun do_valbind (vb : OG.valbind) : OG.valbind =
          case vb
            of OG.PLAINvalbind(i, pat, exp, vb_opt) =>
              let val _ = generic_tyvars_pat := nil
                  val pat = do_pat pat
                  val i' = case ElabInfo.to_TypeInfo i
                             of SOME (TypeInfo.PLAINvalbind_INFO{tyvars=[],Type}) =>
                               ElabInfo.plus_TypeInfo i
                               (TypeInfo.PLAINvalbind_INFO{tyvars= !generic_tyvars_pat, Type=Type})
                              | _ => (*impossible "ElabDec.do_valbind: wrong type info"*)
                               i (* in case of dublicated identifiers in bindings *)
                  val vb_opt' = case vb_opt
                                  of SOME vb => SOME(do_valbind vb)
                                   | NONE => NONE
              in OG.PLAINvalbind(i', pat, exp, vb_opt')
              end
             | OG.RECvalbind(i, vb) => OG.RECvalbind(i,do_valbind vb)
      in
        do_valbind valbind
      end


    (********************************************************)
    (*      Elaboration (type checking)                     *)
    (********************************************************)


    (****** atomic expressions ******)

    fun elab_atexp (C : Context, atexp : IG.atexp)
      : (Substitution * Type * OG.atexp) =

        case atexp of

          (* special constants *)                               (*rule 1*)
          IG.SCONatexp(i, scon, rv_opt) =>
            (* Some special constants are overloaded; thus, we must
             * record some overloading info in the case a special constant
             * can denote one of a set of type names.
             *)
            let val {type_scon, overloading} = Type.of_scon scon
                val i_out =
                  case overloading
                    of NONE => okConv i
                     | SOME tv => preOverloadingConv (i, OverloadingInfo.UNRESOLVED_IDENT tv)
                val i_out = addTypeInfo_EXP(i_out, type_scon)
                val rv_opt = Option.map (fn (i,rv) => (okConv i,rv)) rv_opt
            in (Substitution.Id, type_scon, OG.SCONatexp (i_out, scon, rv_opt))
            end


          (* identifiers - variables or constructors *)         (*rule 2*)
         | IG.IDENTatexp(i, IG.OP_OPT(longid, withOp), regvars_opt) =>
           let val regvars_opt = Option.map (fn(i,rvs) => (okConv i,rvs)) regvars_opt
           in case C.lookup_longid C longid of

               (* Variable *)
              SOME(VE.LONGVAR sigma) =>
                 let val (instance, instances) = TypeScheme.instance' sigma

                   (*if Type.overloaded_tyvars instances yields [], then there
                    are no overloaded tyvars in the type.  If there is exactly
                    one overloaded tyvar, longid may be a primitive
                    (e.g., + : 'a * 'a -> 'a, where 'a is overloaded), and this
                    tyvar must later be resolved.  If there is more than one
                    overloaded tyvar in the type, longid cannot be a primitive,
                    and the overloading info will never be needed (e.g.,
                    fun ~+ ((x,y),(v,w)) = (x+v,y+w))*)
                   val out_i =
                         addTypeInfo_VAR
                           ((case List.filter TyVar.is_overloaded (Type.tyvars instance) of
                               [tyvar] => preOverloadingConv
                                            (i, OverloadingInfo.UNRESOLVED_IDENT tyvar)
                             | _ => okConv i),
                            instances)
                 in
                   (Substitution.Id, instance,
                    OG.IDENTatexp (out_i, OG.OP_OPT (longid, withOp), regvars_opt))
                 end

              (* Constructor *)
            | SOME(VE.LONGCON sigma) =>
                let val (tau,instances) = (TypeScheme.instance' sigma)
                in
                  (Substitution.Id, tau,
                   OG.IDENTatexp(addTypeInfo_CON(okConv i, C, instances, longid),
                                 OG.OP_OPT(longid, withOp), regvars_opt
                                 )
                  )
                end

             (* Exception constructor *)
           | SOME(VE.LONGEXCON tau) =>
                (Substitution.Id,
                 tau,
                 OG.IDENTatexp(addTypeInfo_EXCON(okConv i, tau, longid),
                               OG.OP_OPT(longid, withOp), regvars_opt
                               )
                 )

             (* Not found in current context *)
           | NONE =>
               (Substitution.Id, Type_bogus (),
                OG.IDENTatexp(lookupIdError (i, longid),
                              OG.OP_OPT(Ident.bogus, withOp),
                              regvars_opt
                              )
                )
           end

          (* record expression *)                               (*rule 3*)
         | IG.RECORDatexp(i, NONE, rv_opt) =>
           let val rv_opt = Option.map (fn (i,rv) => (okConv i,rv)) rv_opt
           in (Substitution.Id, Type.Unit, OG.RECORDatexp(okConv i,NONE,rv_opt))
           end

          (* record expression *)
        | IG.RECORDatexp(i, SOME exprow, rv_opt) =>
          let val rv_opt' = Option.map (fn (i,rv) => (okConv i,rv)) rv_opt
              val (S, rho, out_exprow) = elab_exprow(C,exprow)
          in (S, Type.from_RecType (rho,rv_opt),
              OG.RECORDatexp (okConv i, SOME out_exprow, rv_opt'))
          end

          (* let expression *)                                  (*rule 4*)
        | IG.LETatexp(i, dec, exp) =>
            let
              val (S1, T, E, out_dec) = elab_dec(C,dec)
              val (S2, tau, out_exp) = elab_exp((S1 onC C) C_plus_E E, exp)
              val out_i = case TyName.Set.list
                             (TyName.Set.intersect
                              (Type.tynames tau) (TyName.Set.fromList T))
                             of [] => okConv i
                              | tynames => errorConv (i, ErrorInfo.DATATYPES_ESCAPE_SCOPE tynames)
              val R = E.to_R E
              val tau = Type.remove_regvars R tau
            in
              (S2 oo S1, tau, OG.LETatexp (out_i, out_dec, out_exp))
            end

          (* parenthesised expression *)
        | IG.PARatexp(i, exp) =>                                (*rule 5*)
            let val (S, tau, out_exp) = elab_exp(C,exp)
            in (S, tau, OG.PARatexp(okConv i,out_exp)) end

    (******** expression rows ********)

    and elab_exprow (C : Context, exprow : IG.exprow) :
        (Substitution * RecType * OG.exprow) =

        case exprow of

          (* Expression row *)                                  (*rule 6*)
          IG.EXPROW(i, lab, exp, NONE) =>
            let
              val (S, tau, out_exp) = elab_exp(C, exp)
              val rho = Type.RecType.add_field (lab,tau) Type.RecType.empty
            in
              (S, rho, OG.EXPROW(okConv i,lab,out_exp,NONE))
            end

          (* Expression row *)
        | IG.EXPROW(i, lab, exp, SOME exprow) =>
            let
              val (S1, tau, out_exp   ) = elab_exp(C, exp)
              val (S2, rho, out_exprow) = elab_exprow(S1 onC C,exprow)
            in
              if (ListHacks.member lab (Type.RecType.sorted_labs rho)) then
                (S2, rho,
                 OG.EXPROW(repeatedIdsError(i, [ErrorInfo.LAB_RID lab]),
                           lab, out_exp, SOME out_exprow))
              else
                (S2 oo S1, Type.RecType.add_field (lab,S2 on tau) rho,
                 OG.EXPROW(okConv i,lab,out_exp,SOME out_exprow))
            end

    (******** expressions ********)

    and elab_exp(C : Context, exp : IG.exp) :
        (Substitution * Type * OG.exp) =
      let
        val (S, ty, exp') = elab_exp'(C, exp)
      in
        (S, ty, exp')
      end

    and elab_exp'(C, exp) =
      case exp

           (* Atomic expression *)                              (*rule 7*)
        of IG.ATEXPexp(i, atexp) =>
             let
               val (S, tau, out_atexp) = elab_atexp(C, atexp)
             in
               (S, tau, OG.ATEXPexp(addTypeInfo_EXP(okConv i, tau),out_atexp))
             end

           (* Application expression *)                         (*rule 8*)
         | IG.APPexp(i, exp, atexp) =>
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_atexp) = elab_atexp(S1 onC C, atexp)
               val new   = Type.fresh_normal ()
               val arrow = Type.mk_Arrow(tau2,new,NONE)
               val (S3, i') = UnifyWithTexts ("operand suggests operator type",arrow,
                                              "but I found operator type",S2 on tau1, i)
               val tau = S3 on new
             in
               (S3 oo S2 oo S1, tau,
                OG.APPexp(addTypeInfo_EXP(i', tau), out_exp, out_atexp))
             end

           (* Typed expression *)                               (*rule 9*)
         | IG.TYPEDexp(i, exp, ty) =>
             let val (S1, tau, out_exp) = elab_exp(C, exp)
             in case elab_ty(S1 onC C, ty)
                  of (SOME tau', out_ty) =>
                     let val (S2, i') =
                             UnifyWithTexts ("type of expression",tau,"disagrees with your type constraint", tau', i)
                         val tau'' = S2 on tau'
                    in (S2 oo S1, tau'', OG.TYPEDexp(addTypeInfo_EXP(i',tau''), out_exp, out_ty))
                    end
                   | (NONE, out_ty) => (S1, tau, OG.TYPEDexp(okConv i, out_exp, out_ty))
             end

           (* Handle exception *)                               (*rule 10*)
         | IG.HANDLEexp(i, exp, match) =>
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_match) = elab_match(S1 onC C, match)
               val matchTy = Type.mk_Arrow(Type.Exn, tau1, NONE)
               val (S3, i') = UnifyWithTexts("handled expression suggests handler type", matchTy,
                                             "but I found handler type", tau2, i)
               val tau3 = (S3 oo S2) on tau1
             in
               (S3 oo S2 oo S1, tau3,
                OG.HANDLEexp(addTypeInfo_EXP(i',tau3), out_exp, out_match))
             end

           (* Raise exception *)                                (*rule 11*)
         | IG.RAISEexp(i, exp) =>
             let
               val (S1, tau1, out_exp) = elab_exp(C, exp)
               val exnType = Type.Exn
               val (S2, i')    = UnifyWithTexts("type of expression after 'raise' should be",
                                                exnType, "but I found type", tau1, i)
               val tau = Type.fresh_normal ()
             in
               (S2 oo S1, tau, OG.RAISEexp(addTypeInfo_EXP(i',tau), out_exp))
             end

           (* Function expression *)                            (*rule 12*)
         | IG.FNexp(i, match) =>
             let
               val (S, tau, out_match) = elab_match(C, match)
             in
               (S, tau, OG.FNexp(addTypeInfo_EXP(okConv i,tau), out_match))
             end

         | IG.UNRES_INFIXexp _ =>
             impossible "elab_exp(UNRES_INFIX)"


    (******** matches ********)

    and elab_match (C : Context, match : IG.match) :
        (Substitution * Type * OG.match) =

        case match of

          (* Match *)                                           (*rule 13*)
          IG.MATCH(i, mrule, NONE) =>
            let val (S, tau, out_mrule) = elab_mrule(C,mrule)
            in (S, tau, OG.MATCH(addTypeInfo_MATCH(okConv i,tau),out_mrule,NONE)) end

          (* Match *)
        | IG.MATCH(i, mrule, SOME match') =>
            let
              val (S ,tau ,out_mrule) = elab_mrule(C,mrule)
              val (S',tau',out_match) = elab_match(S onC C,match')
              val (S'', i') = UnifyWithTexts("type of match rule with wildcard", tau',
                                             "type of previous match rules", S' on tau,i)
              val tau'' = S'' on tau'
            in
              (S'' oo S' oo S, tau'',
               OG.MATCH(addTypeInfo_MATCH(i',tau''), out_mrule, SOME out_match))
            end

    (******** match rules ********)

    and elab_mrule (C : Context, mrule : IG.mrule) :
        (Substitution * Type * OG.mrule) =

        case mrule of

          (* Match rule *)                                      (*rule 14*)
          IG.MRULE(i, pat, exp) =>
            let
              val (S, (VE,tau,_), out_pat) = elab_pat(C,pat)
              val (S',tau', out_exp) =
                  elab_exp(C.plus_VE (S onC C, VE),exp)
              val S'' = S' oo S
              val out_i = okConv i
            in
              (S'', Type.mk_Arrow (S'' on tau,tau', NONE),
               OG.MRULE (out_i ,out_pat,out_exp))
            end

    (******** declarations ********)

    and elab_dec(C : Context, dec : IG.dec) :
        (Substitution * TyName list * Env * OG.dec) =
        (case dec of

           (* Value declaration *)                              (*rule 15*)

           IG.VALdec (i, ExplicitTyVars, valbind) =>
             let
               val U = ListHacks.union (ExplicitTyVars,
                       ListHacks.minus (Environments.unguarded_valbind valbind, C.to_U C))
               val _ = Level.push ()
               val (S, VE, out_valbind) =
                     elab_valbind (C.plus_U (C, U), valbind)    (* plus_U creates levels for the explicit tyvars in U *)
                     handle E => (Level.pop(); raise E)
               val _ = Level.pop()

               val VE' = C.close (S onC C, valbind, VE)

(*for debugging
               fun pr_id VE id =
                 case C.lookup_longid (C.plus_VE(C,VE)) (Ident.mk_LongId [id])
                   of SOME(VE.LONGVAR sigma) => print (id ^ ": " ^ TypeScheme.string sigma ^ "\n")
                    | _ => ()
 *)

               val out_i = case ListHacks.intersect (ExplicitTyVars, C.to_U C)
                             of [] => okConv i
                              | explicittyvars => errorConv
                               (i, ErrorInfo.TYVARS_SCOPED_TWICE
                                    (map TyVar.from_ExplicitTyVar explicittyvars))

               val out_valbind = insert_type_info_in_valbind (VE', out_valbind)

             (*The side condition ``U n tyvars VE' = {}'' is enforced partly
              by disallowing unification of free explicit tyvars (giving the
              error message

                      (fn x => let val y : 'a = x in y y end) 666 ;
                                       ^^^^^^^^^^
               Type clash,
                  type of left-hand side pattern:     'a
                  type of right-hand side expression: int).

              And partly the side condition is enforced by the restriction
              that the same explicit tyvar may not be scoped at two
              valbinds within each other (Definition 1997, sec. 2.9, last
              bullet).  The latter is checked above by checking whether
              any of `ExplicitTyVars' are in U of C (I hope that does it?)
              24/01/1997 15:38. tho.*)

             in
               (S, [], E.from_VE VE',
                OG.VALdec (out_i, ExplicitTyVars, out_valbind))
             end

           (* `fun'-declaration *)
         | IG.UNRES_FUNdec _ => impossible "elab_dec(UNRES_FUN)"

           (* Type declaration *)                               (*rule 16*)
         | IG.TYPEdec(i, typbind) =>
             let
               (* Note that no substitutions are produced *)
               val (TE, out_typbind) = elab_typbind(C, typbind)
             in
               (Substitution.Id, [], E.from_TE TE,
                OG.TYPEdec(addTypeInfo_TYENV (okConv i, TE), out_typbind))
             end

           (* Datatype declaration *)                           (*rule 17*)
         | IG.DATATYPEdec(i, datbind) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_plus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE (VE1, TE1)
               val T = (TE.fold (fn tystr => fn T => case TypeFcn.to_TyName(TyStr.to_theta tystr)
                                                       of SOME t => t::T
                                                        | NONE => impossible "elab_dec(DATATYPEdec)")
                        [] TE2)
             in
               (Substitution.Id, T, E.from_VE_and_TE (VE2, TE2),
                OG.DATATYPEdec(addTypeInfo_TYENV (okConv i, TE2),
                               out_datbind)) (*martin*)
             end

           (*datatype replication*)                             (*rule 18*)
         | IG.DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
             (case C.lookup_longtycon C longtycon of
                SOME tystr =>
                  let
                    val TE = TE.singleton (tycon, tystr)
                    val (theta, VE) = TyStr.to_theta_and_VE tystr
                  in
                    (Substitution.Id,[],
                     E.from_VE_and_TE (VE,TE),
                     OG.DATATYPE_REPLICATIONdec
                       (addTypeInfo_TYENV (okConv i, TE), tycon, longtycon))
                  end
              | NONE =>
                  (Substitution.Id,[],
                   E.bogus,
                   OG.DATATYPE_REPLICATIONdec
                     (lookupTyConError (i,longtycon), tycon, longtycon)))

           (* Abstype declaration *)                            (*rule 19*)
         | IG.ABSTYPEdec(i, datbind, dec) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_plus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE
                                  (VE1, TE1)
               val (S, T, E, out_dec) = elab_dec(C C_plus_VE_and_TE (VE2,TE2), dec)
               val (T',E',phi) = Environments.ABS (TE2, E)
                 (* the realisation returned maps abstract type
                  * names to type names for the datbind. *)
               val out_i = addTypeInfo_ABSTYPE (okConv i, (TE2, phi))
             in
               (S,T @ T',E',
                OG.ABSTYPEdec(out_i, out_datbind, out_dec))
             end

           (* Exception declaration *)                          (*rule 20*)
         | IG.EXCEPTIONdec(i, exbind) =>
             let
               val (VE, out_exbind) = elab_exbind (C, exbind)
             in
               (Substitution.Id, [],
                E.from_VE VE,
                OG.EXCEPTIONdec(okConv i, out_exbind))
             end

           (* Local declaration *)                              (*rule 21*)
         | IG.LOCALdec(i, dec1, dec2) =>
             let
               val (S1, T1, E1, out_dec1) = elab_dec(C,dec1)
               val (S2, T2, E2, out_dec2) = elab_dec((S1 onC C) C_plus_E E1,dec2)
             in
               (S2 oo S1, T1 @ T2, E2, OG.LOCALdec(okConv i,out_dec1,out_dec2))
             end

           (* Open declaration *)                               (*rule 22*)
         | IG.OPENdec(i, list) =>
             let
               fun process(E0, list)
                   : Env * Environments.longstrid OG.WithInfo list =
                 case list
                   of IG.WITH_INFO(i, longstrid) :: rest =>
                        (case C.lookup_longstrid C longstrid of
                           SOME E =>
                                let val (E', rest') = process(E0, rest)
                                in
                                  (E.plus (E, E'),
                                   OG.WITH_INFO (okConv i, longstrid) :: rest')
                                end

                            | NONE =>   (* Lookup failure: process rest of
                                           list and build env regardless. *)
                                let
                                  val (E', rest') = process(E0, rest)
                                  val ei = ErrorInfo.LOOKUP_LONGSTRID longstrid
                                in
                                  (E', OG.WITH_INFO(errorConv(i, ei), longstrid)
                                       :: rest'
                                  )
                                end
                        )

                    | nil => (E.empty, nil)

               val (E', list') = process(E.empty, list)
               val i' = ElabInfo.plus_TypeInfo (okConv i)
                 (TypeInfo.OPEN_INFO
                  let val (SE,TE,VE,_) = E.un E'
                  in (EqSet.list (SE.dom SE), EqSet.list (TE.dom TE), EqSet.list (VE.dom VE))
                  end)
             in
               (Substitution.Id, [], E', OG.OPENdec(i', list'))
             end

         | IG.INFIXdec(i, prec, ids) =>    (* infix -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.INFIXdec(okConv i, prec , ids))

         | IG.INFIXRdec(i, prec, ids) =>   (* infixr -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.INFIXRdec(okConv i, prec, ids))

         | IG.NONFIXdec(i, ids) =>         (* nonfix -- no rule in Definition *)
             (Substitution.Id, [],
              E.from_VE VE.empty,
              OG.NONFIXdec(okConv i, ids))

           (* Empty declaration *)                              (*rule 23*)
         | IG.EMPTYdec(i) =>
             (Substitution.Id, [], E.from_VE VE.empty, OG.EMPTYdec(okConv i))

           (* Sequential declaration *)                         (*rule 24*)
         | IG.SEQdec(i, dec1, dec2) =>
             let
               val (S,T,E,C',out_dec) = elab_decs(C,dec)
             in
               (S,T,E, out_dec)
             end

           (* Region declaration; added declaration *)
         | IG.REGIONdec(i, (i2,regvars)) =>
           let val i2' = okConv i2
               val i' =
                   case getRepeatedElements RegVar.eq regvars of
                       [] => okConv i
                     | repeated => repeatedIdsError (i2, map ErrorInfo.REGVAR_RID repeated)
               val i' =
                   let val rs_env = C.to_R C
                       val dups = List.foldl (fn (r,acc) => if List.exists (fn r' => RegVar.eq(r',r)) rs_env
                                                            then r::acc else acc) nil regvars
                   in case dups of
                          nil => i'
                        | _ => ElabInfo.plus_ErrorInfo i' (ErrorInfo.REGVARS_SCOPED_TWICE dups)
                   end
           in (Substitution.Id, [], E.from_R regvars, OG.REGIONdec(i',(i2',regvars)))
           end
        )
    and elab_decs(C : Context, dec : IG.dec) :  (* fast elaboration when SEQ associates to the left *)
        (Substitution * TyName list * Env * Context * OG.dec) =
        (case dec of
           IG.SEQdec(i, dec1, dec2) =>
             let
               val (S1, T1, E1, C1res, out_dec1) = elab_decs(C,dec1)
                                (*C1res= (S1 onC C) C_plus_E E1*)
               val (S2, T2, E2, out_dec2) = elab_dec(C1res,dec2)
               val E1' = E.on (S2, E1)
             in
               (S2 oo S1, T1 @ T2,
                E.plus (E1',E2),
                (S2 onC C1res) C_plus_E E2,
                OG.SEQdec(okConv i,out_dec1,out_dec2))
             end
         | dec1 =>
             let
               val (S1, T1, E1, out_dec1) = elab_dec(C,dec1)
             in
               (S1, T1, E1, (S1 onC C) C_plus_E E1, out_dec1)
             end
        )

    (****** value bindings - Definition, p. ? ******)

    and elab_valbind(C : Context, valbind : IG.valbind)
          : (Substitution * VarEnv * OG.valbind) =

        case valbind of

        (* Simple value binding *)                              (*rule 25*)
        IG.PLAINvalbind(i, pat, exp, valbind_opt) =>
          let
            val (S0, (VE,tau,R), out_pat) = elab_pat(C, pat)
(*            val () = print ("PLAINvalbind: " ^ Int.toString (length R) ^ "\n") *)
            val (S1, tau1, out_exp) = elab_exp(C.plus_E(S0 onC C,E.from_R R), exp)

            val (S2, i') = UnifyWithTexts ("type of left-hand side pattern", (S1 oo S0) on tau,
                                           "type of right-hand side expression", tau1, i)

            (* Here we modify the type schemes in VE to also abstract over the regvars R *)
            val VE = VE.close_regvars R VE

            (* if there was a unification error in the line above, change the right source
               info field of i' to become the right end of exp : *)
            val i' = case ElabInfo.to_ErrorInfo i' of
                         NONE => i'
                       | SOME _ => ElabInfo.retractRight (i', OG.get_info_exp out_exp)

            val (S3, VE', valbind_opt') =
              case valbind_opt
                of SOME valbind =>
                     let
                       val (S, VE, vb) =
                         elab_valbind((S2 oo S1 oo S0) onC C, valbind)
                     in
                       (S, VE, SOME vb)
                     end

                 | NONE =>
                     (Substitution.Id, VE.empty, NONE)
            val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
          in
            if EqSet.isEmpty intdom then
              (case List.filter IG.is_'true'_'nil'_etc
                      (EqSet.list (VE.dom VE)) of
                 [] =>
                   (S3 oo S2 oo S1 oo S0,
                    VE.plus ((S3 oo S2 oo S1 oo S0) onVE VE, VE'),
                    OG.PLAINvalbind
                    (addTypeInfo_PLAINvalbind(i', tau1),
                     out_pat, out_exp, valbind_opt'))
               | ids =>
                   (S3, VE',
                    OG.PLAINvalbind
                      (ElabInfo.plus_ErrorInfo i'
                         (ErrorInfo.REBINDING_TRUE_NIL_ETC ids),
                       out_pat, out_exp, valbind_opt')))
            else
              (S3, VE',
               OG.PLAINvalbind((case ElabInfo.to_ErrorInfo i' of
                                  NONE =>
                                   ElabInfo.plus_ErrorInfo i'
                                    (ErrorInfo.REPEATED_IDS
                                     (map ErrorInfo.ID_RID (EqSet.list intdom)))
                                | SOME _ => i'),
                               out_pat, out_exp, valbind_opt'))
          end
                                                                (*rule 26*)
                                (* Recursive value binding. Rather tricky
                                   because we have to plant error info after
                                   the second pass. Make that `very tricky.' *)
      | IG.RECvalbind(i, valbind) =>
          let
                                (* Function to unify the occurrence of a
                                   variable in two VE's. The result is a
                                   substitution and an ErrorInfo tag. *)

            fun processID (i, VE, VE', id): Substitution * ElabInfo =
                  (case (VE.lookup VE id, VE.lookup VE' id) of
                     (SOME (VE.LONGVAR sigma1), SOME (VE.LONGVAR sigma2)) =>
                       let val (_, _, tau1) = TypeScheme.to_TyVars_and_Type sigma1
                           val (_, _, tau2) = TypeScheme.to_TyVars_and_Type sigma2
                       in (case Type.unify {unify_regvars=false} (tau1, tau2) of
                             Type.UnifyOk => (Substitution.Id, i)   (* substitutions are dummies *)
                           | Type.UnifyFail _ => (Substitution.Id,
                                                ElabInfo.plus_ErrorInfo i
                                                (ErrorInfo.UNIFICATION(tau1, tau2)))
                           | Type.UnifyRankError(tv,tn) => (Substitution.Id,
                                                            ElabInfo.plus_ErrorInfo i
                                                            (ErrorInfo.UNIFICATION(tau1, tau2))))
                       end
                    | _ => impossible "processID")

                                (* Traverse the out_valbind, doing a
                                   unification (and adding ErrorInfo if reqd.)
                                   and giving a subst. at each stage. The
                                   ErrorInfo goes into the pattern...
                                   ...somewhere... *)

            fun traverseRecValbind (VE, VE', vb): Substitution * OG.valbind =
              case vb
                of OG.PLAINvalbind(i, pat, exp, vb_opt) =>
                     let val (S, pat') = traverseRecPat(VE, VE', pat)
                         val (S', vb_opt') =
                           case vb_opt
                             of SOME vb =>
                                let val (S', vb') = traverseRecValbind (S onVE VE, S onVE VE', vb)
                                in (S' oo S, SOME vb')
                                end
                              | NONE => (S, NONE)
                     in (S', OG.PLAINvalbind(i, pat', exp, vb_opt'))
                     end

                 | OG.RECvalbind(i, vb) =>
                     let
                       val (S, vb') = traverseRecValbind(VE, VE', vb)
                     in
                       (S, OG.RECvalbind(i, vb'))
                     end

            and traverseRecPat (VE, VE', pat): Substitution * OG.pat =
              case pat
                of OG.ATPATpat(i, atpat) =>
                     let
                       val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in
                       (S, OG.ATPATpat(i, atpat'))
                     end

                 | OG.CONSpat(i, id, atpat) =>
                     let val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in (S, OG.CONSpat(i, id, atpat'))
                     end

                 | OG.TYPEDpat(i, pat, ty) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.TYPEDpat(i, pat', ty))
                     end

                 | OG.LAYEREDpat(i, id as OG.OP_OPT(id', withOp),
                                 ty_opt, pat
                                ) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                       val (S', i') = processID(i, S onVE VE, S onVE VE', id')
                     in
                       (S oo S', OG.LAYEREDpat(i', id, ty_opt, pat'))
                     end

                 | OG.UNRES_INFIXpat _ =>
                     impossible "traverseRecPat(UNRES_INFIX)"

            and traverseRecAtpat (VE, VE', atpat): Substitution * OG.atpat =
              case atpat
                of OG.WILDCARDatpat _ => (Substitution.Id, atpat)

                 | OG.SCONatpat _ => (Substitution.Id, atpat)

                 | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid, withOp), regvars_opt) =>
                     (case C.lookup_longid C longid of
                        SOME (VE.LONGCON _) => (Substitution.Id, atpat)
                      | SOME (VE.LONGEXCON _) => (Substitution.Id, atpat)
                      | _ => (case Ident.decompose longid of
                                ([], id) =>
                                  let val (S, i') = processID(i, VE, VE', id)
                                  in (S, OG.LONGIDatpat(i', longid_op, regvars_opt))
                                  end
                              | _ => impossible "traverseRecAtpat(longid)"))

                 | OG.RECORDatpat(i, patrowOpt) =>
                     (case patrowOpt of
                        NONE => (Substitution.Id,atpat)
                      | SOME patrow =>
                          let
                            val (S, patrow') =
                              traverseRecPatrow (VE, VE', patrow)
                          in
                            (S, OG.RECORDatpat(i, SOME patrow'))
                          end)

                 | OG.PARatpat(i, pat) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.PARatpat(i, pat'))
                     end

            and traverseRecPatrow (VE, VE', patrow): Substitution * OG.patrow =
              case patrow of
                OG.DOTDOTDOT i => (Substitution.Id, patrow)
              | OG.PATROW(i, l, pat, patrowOpt) =>
                  let
                    val (S, pat') = traverseRecPat(VE, VE', pat)
                    val (S', patrowOpt') =
                      (case patrowOpt of
                        NONE => (Substitution.Id, NONE)
                      | SOME patrow =>
                          let
                            val (S'', patrow') = traverseRecPatrow(VE, VE', patrow)
                          in
                            (S'', SOME patrow')
                          end)
                  in
                    (S' oo S, OG.PATROW(i, l, pat', patrowOpt'))
                  end

            (* set up a value environment, VE, for the recursively declared values *)

            val domain_list = dom_vb(C, valbind)

            fun TypeScheme_fresh () = TypeScheme.from_Type (Type.fresh_normal ())

            fun setup (id, VE) =
                  VE.plus (VE, VE.singleton_var (id, TypeScheme_fresh ()))

            val VE = foldl setup VE.empty domain_list
                                (* VE now maps each rec identifier to 'a. *)

                                (* Proceed with type checking. The ErrorInfo
                                   tags for the rec identifiers will be
                                   untouched (I hope, since we might assign
                                   them further on). *)

            val (S, VE', valbind') =
                  elab_valbind (C.plus_VE (C, VE), valbind)

                                (* Post-pass, to patch up the rec identifiers
                                   and plant unification error tags: *)

            val (S', valbind'') =
                  traverseRecValbind (S onVE VE, VE', valbind')

            val VE'' = S' onVE VE'

            val out_i = okConv i
          in
            (S' oo S, VE'', OG.RECvalbind (out_i, valbind''))
          end


    (******* type bindings *******)

    and elab_typbind (C : Context, typbind : IG.typbind) : (TyEnv * OG.typbind) =

      case typbind of

        (* Type binding *)                                      (*rule 27*)
        IG.TYPBIND(i, ExplicitTyVars, tycon, ty, typbind_opt) =>
          let
            val _ = Level.push()

            val (TyVars,C') = C.plus_U'(C, ExplicitTyVars)

            val tyvarsRepeated = getRepeatedElements (op =) ExplicitTyVars
            val tyvarsNotInTyVarList =
              List.filter (fn tv => not (ListHacks.member tv ExplicitTyVars))
              (IG.getExplicitTyVarsTy ty)

          in case elab_ty(C', ty)
               of (SOME tau, out_ty) =>
                 let val _ = Level.pop()
                     val typeFcn = TypeFcn.from_TyVars_and_Type (TyVars, tau)
                     val tystr = TyStr.from_theta_and_VE(typeFcn, VE.empty)
                     val (TE, out_typbind_opt) = elab_typbind_opt(C, typbind_opt)
                 in
                   if not(isEmptyTyVarList(tyvarsNotInTyVarList)) then
                     (TE, OG.TYPBIND(errorConv(i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
                                               (map SyntaxTyVar.pr_tyvar tyvarsNotInTyVarList)),
                                     ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                   else
                     if (EqSet.member tycon (TE.dom TE)) then
                       (TE.plus (TE.singleton(tycon, tystr), TE),
                        OG.TYPBIND(repeatedIdsError(i, [ErrorInfo.TYCON_RID tycon]),
                                   ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                     else
                       case tyvarsRepeated
                         of [] =>
                           (TE.plus (TE.singleton(tycon, tystr), TE),
                            OG.TYPBIND(okConv i, ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                          | _ =>
                           (TE, OG.TYPBIND(repeatedIdsError(i, map ErrorInfo.TYVAR_RID
                                                            (map TyVar.from_ExplicitTyVar tyvarsRepeated)),
                                           ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                 end
                | (NONE, out_ty) =>
                 let val _ = Level.pop()
                     val (TE, out_typbind_opt) = elab_typbind_opt(C, typbind_opt)
                 in (TE, OG.TYPBIND(okConv i, ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                 end
          end

    and elab_typbind_opt (C : Context, typbind_opt : IG.typbind option)
      : (TyEnv * OG.typbind option) =

      case typbind_opt of

        SOME(typbind) =>
          let
            val (TE, out_typbind) = elab_typbind(C, typbind)
          in
            (TE, SOME out_typbind)
          end

      | NONE =>
          (TE.empty, NONE)

    (******* datatype bindings *******)

    and elab_datbind (C : Context, datbind : IG.datbind)
      : ((VarEnv * TyEnv) * OG.datbind) =

      case datbind of

        (* Datatype binding *)                                  (*rule 28*)
        IG.DATBIND(i, ExplicitTyVars, tycon, conbind, datbind_opt) =>
          let
            val _ = Level.push()

            val (TyVars, C') = C.plus_U'(C, ExplicitTyVars)

            val tyvarsRepeated = getRepeatedElements (op =) ExplicitTyVars
            val tyvarsNotInTyVarList =
              List.filter
                (fn tv => not (ListHacks.member tv ExplicitTyVars))
                (IG.getExplicitTyVarsConbind conbind)

            val (_, C') = C.plus_U'(C', tyvarsNotInTyVarList)  (* if tyvarsNotInTyVarList
                                                                * is not empty, then there is
                                                                * a type error, which we catch
                                                                * below. *)
            val (typeFcn, _) =
              case C.lookup_tycon C tycon of
                SOME(tystr) => TyStr.to_theta_and_VE(tystr)
              | NONE => impossible "datbind(1)"

            val tyname =
              case TypeFcn.to_TyName typeFcn of
                SOME(tyname) => tyname
              | NONE => impossible "datbind(2)"

            val tau_list =
              map Type.from_TyVar TyVars

            val tau =
              Type.from_ConsType (Type.mk_ConsType (tau_list, tyname, NONE))

            val (constructor_map : constructor_map,
                 out_conbind) = elab_conbind (C', tau, conbind)
            val _ = Level.pop()
            val VE = constructor_map.to_VE constructor_map

            val VE_closed = VE.close VE

            val tystr = TyStr.from_theta_and_VE (TypeFcn.from_TyName tyname, VE_closed)

            val ((VE', TE'), out_datbind_opt) = elab_datbind_opt (C, datbind_opt)
            val out_i =
                  if TyCon.is_'true'_'nil'_etc tycon then
                    errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [])
                  else if TyCon.is_'it' tycon then
                    errorConv (i, ErrorInfo.REBINDING_IT)
                  else if not (isEmptyTyVarList tyvarsNotInTyVarList) then
                    errorConv (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
                               (map SyntaxTyVar.pr_tyvar
                                  tyvarsNotInTyVarList))
                  else
                    let val repeated_ids_errorinfos =
                      map ErrorInfo.TYVAR_RID (map TyVar.from_ExplicitTyVar tyvarsRepeated)
                      @ map ErrorInfo.ID_RID
                          (EqSet.list (EqSet.intersect (VE.dom VE') (VE.dom VE)))
                      @ (if EqSet.member tycon (TE.dom TE')
                         then [ErrorInfo.TYCON_RID tycon] else [])
                    in
                      case repeated_ids_errorinfos
                        of [] => okConv i
                         | _ => repeatedIdsError (i, repeated_ids_errorinfos)
                    end
          in
            ( (VE.plus  (VE_closed, VE'),
               TE.plus (TE.singleton (tycon, tystr), TE')),
              OG.DATBIND(out_i, ExplicitTyVars, tycon,
                         out_conbind, out_datbind_opt) )
          end

    and elab_datbind_opt (C : Context, datbind_opt : IG.datbind option)
      : ((VarEnv * TyEnv) * OG.datbind option) =

      case datbind_opt of

        SOME(datbind) =>
          let
            val ((VE, TE), out_datbind) = elab_datbind(C, datbind)
          in
            ((VE, TE), SOME out_datbind)
          end

       | NONE =>
          ((VE.empty, TE.empty), NONE)

    (****** constructor bindings *****)

    and elab_conbind (C : Context, tau : Type, conbind : IG.conbind)
      : (constructor_map * OG.conbind) =

      (*I deviate from the definition in letting elab_conbind return
       a mapping from id's (constructors) to TypeScheme's instead of
       a VE, because a VE also contains fellow constructors for each
       constructor, and the fellow constructors cannot be determined
       before the whole conbind has been processed. I let the caller
       of elab_conbind (elab_datbind) turn this map into a VE proper
       19/01/1997 17:02. tho.*)

      case conbind of

        (* Constructor binding *)                               (*rule 29*)
        IG.CONBIND(i, IG.OP_OPT(con, withOp), SOME ty, conbind_opt) =>
          let val (constructor_map, out_conbind_opt) = elab_conbind_opt (C, tau, conbind_opt)
              fun result out_ty = OG.CONBIND (out_i_for_conbind con constructor_map i,
                                              OG.OP_OPT (con, withOp),
                                              SOME out_ty, out_conbind_opt)
          in case elab_ty (C, ty)
               of (SOME tau', out_ty) =>
                 let val arrow = TypeScheme.from_Type (Type.mk_Arrow (tau', tau, NONE))
                 in (constructor_map.add con arrow constructor_map, result out_ty)
                 end
                | (NONE, out_ty) => (constructor_map, result out_ty)
          end

      | IG.CONBIND(i, IG.OP_OPT(con, withOp), NONE, conbind_opt) =>
          let val (constructor_map, out_conbind_opt) = elab_conbind_opt (C, tau, conbind_opt)
          in
            (constructor_map.add con (TypeScheme.from_Type tau) constructor_map,
             OG.CONBIND (out_i_for_conbind con constructor_map i,
                         OG.OP_OPT(con, withOp), NONE, out_conbind_opt))
          end

    and out_i_for_conbind con constructor_map i =
          if constructor_map.in_dom con constructor_map then
            repeatedIdsError (i, [ErrorInfo.CON_RID con])
          else if IG.is_'true'_'nil'_etc con then
            errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [con])
          else if IG.is_'it' con then errorConv (i, ErrorInfo.REBINDING_IT)
          else okConv i

    and elab_conbind_opt (C : Context, tau : Type, conbind_opt : IG.conbind option)
      : (constructor_map * OG.conbind option) =

      case conbind_opt

        of SOME conbind =>
          let val (constructor_map, out_conbind) = elab_conbind (C, tau, conbind)
          in (constructor_map, SOME out_conbind)
          end

         | NONE => (constructor_map.empty, NONE)

    (****** exception bindings *****)

    and elab_exbind (C : Context, exbind : IG.exbind)
      : (VarEnv * OG.exbind) =

      case exbind of

        (* Exception binding *)                                 (*rule 30*)
        IG.EXBIND(i, IG.OP_OPT(excon, withOp), SOME ty, rest) =>
          let val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in case elab_ty (C, ty)
               of (SOME tau, out_ty) =>
                 let val exnTy = Type.mk_Arrow (tau, Type.Exn, NONE)
                     val VE_this = VE.singleton_excon (excon, exnTy)
                 in
                   (VE.plus  (VE_this, VE_rest),
                    OG.EXBIND (out_i_for_exbind excon VE_rest i (SOME tau),
                               OG.OP_OPT(excon, withOp), SOME out_ty, out_rest))
                 end
                | (NONE, out_ty) =>
                 (VE_rest, OG.EXBIND(okConv i, OG.OP_OPT(excon, withOp), SOME out_ty, out_rest))
          end

      | IG.EXBIND(i, IG.OP_OPT(excon, withOp), NONE, rest) =>
          let
            val VE_this = VE.singleton_excon (excon, Type.Exn)
            val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in
            (VE.plus  (VE_this, VE_rest),
             OG.EXBIND (out_i_for_exbind excon VE_rest i NONE,
                        OG.OP_OPT(excon, withOp), NONE, out_rest))
          end

        (* Exception binding *)                                 (*rule 31*)
      | IG.EXEQUAL(i, IG.OP_OPT(excon, exconOp),
                      IG.OP_OPT(longid, longidOp), rest) =>
          (case C.lookup_longid C longid of
             SOME (VE.LONGEXCON tau) =>
               let
                 val VE_this = VE.singleton_excon (excon, tau)
                 val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in
                 (VE.plus  (VE_this, VE_rest),
                  OG.EXEQUAL (out_i_for_exbind excon VE_rest i NONE,
                              OG.OP_OPT(excon, exconOp),
                              OG.OP_OPT(longid, longidOp), out_rest))
               end
           | _ => (*Carry on, building an error node.*)
               let val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in
                 (VE_rest, OG.EXEQUAL(lookupIdError(i, longid),
                                      OG.OP_OPT(excon, exconOp),
                                      OG.OP_OPT(longid, longidOp),
                                      out_rest))
               end)

    and out_i_for_exbind excon VE_rest i tau_opt =
          if EqSet.member excon (VE.dom VE_rest)
          then repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
          else if IG.is_'true'_'nil'_etc excon
               then errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [excon])
               else if IG.is_'it' excon
                    then errorConv (i, ErrorInfo.REBINDING_IT)
                    else addTypeInfo_EXBIND (okConv i, tau_opt)

    and elab_exbind_opt (C, SOME exbind) =
          let val (VE, out_exbind) = elab_exbind (C, exbind)
          in
            (VE, SOME out_exbind)
          end

      | elab_exbind_opt(C, NONE) = (VE.empty, NONE)

    (****** atomic patterns ******)

    and elab_atpat (C : Context, atpat : IG.atpat) :
        (Substitution * (VarEnv * Type * RegVar.regvar list) * OG.atpat) =

        case atpat of

          (* Wildcard *)                                        (*rule 32*)
          IG.WILDCARDatpat i  =>
            (Substitution.Id,
             (VE.empty, Type.fresh_normal (), nil),
              OG.WILDCARDatpat(okConv i))

          (* Special constant *)                                (*rule 33*)
        | IG.SCONatpat(i,scon) =>
            (* Some special constants are overloaded; thus, we must
             * record some overloading info in the case a special constant
             * can denote one of a set of type names.
             *)
            let val {type_scon, overloading} = Type.of_scon scon
                val i_out =
                  case overloading
                    of NONE => (case scon
                                  of SCon.REAL _ => errorConv(i, ErrorInfo.REALSCON_ATPAT)
                                   | _ => okConv i)
                     | SOME tv => preOverloadingConv (i, OverloadingInfo.UNRESOLVED_IDENT tv)
                val i_out = addTypeInfo_MATCH(i_out, type_scon)
            in (Substitution.Id, (VE.empty, type_scon, nil), OG.SCONatpat (i_out, scon))
            end

          (* Long identifier *)                                 (*rule 34*)
        | IG.LONGIDatpat(i, IG.OP_OPT(longid, withOp), regvars_opt) =>
            (case C.lookup_longid C longid of
               SOME(VE.LONGCON sigma) =>          (* rule 36 *)
                    let
                      fun isConsType tau =
                            (case Type.to_ConsType tau of
                               SOME _ => true
                             | NONE => false)

                      val (tau,instances) = (TypeScheme.instance' sigma)
                      val (tau', i') =
                          if isConsType tau then
                            (tau, okConv i)
                          else
                            (Type_bogus (),
                             errorConv(i, ErrorInfo.NOTCONSTYPE tau)
                            )
                      val i' = if Option.isSome regvars_opt then
                                 ElabInfo.plus_ErrorInfo i' ErrorInfo.REGVARS_IDONLY
                               else i'
                      val R = case regvars_opt of SOME (_, rs) => rs
                                                | NONE => nil
                      val regvars_opt' = Option.map (fn(i,rvs) => (okConv i,rvs)) regvars_opt
                    in
                      (Substitution.Id,
                       (VE.empty, tau', R),
                       OG.LONGIDatpat(addTypeInfo_CON(i', C, instances, longid),
                                      OG.OP_OPT(longid, withOp),
                                      regvars_opt'))
                    end

                | SOME(VE.LONGEXCON tau) =>
                    let
                      val exnType = Type.Exn
                      val (_, i') = UnifyWithTexts
                                      ("expected long excon in pattern to have type",
                                       exnType, "but found it to have type", tau,i)
                      val i' = if Option.isSome regvars_opt then
                                 ElabInfo.plus_ErrorInfo i' ErrorInfo.REGVARS_IDONLY
                               else i'
                      val regvars_opt' = Option.map (fn(i,rvs) => (okConv i,rvs)) regvars_opt
                    in
                      (Substitution.Id,
                       (VE.empty, exnType, nil),
                       OG.LONGIDatpat(addTypeInfo_EXCON(i',exnType,longid),
                                      OG.OP_OPT(longid, withOp),
                                      regvars_opt'))
                    end

                | _ =>          (* make new variable environment *)
                                (* unbound long identifier *)
                    let
                      val tau = Type.fresh_normal ()
                      val tau_scheme = TypeScheme.from_Type tau
                      val regvars_opt' = Option.map (fn(i,rvs) => (okConv i,rvs)) regvars_opt
                    in
                      case Ident.decompose longid
                        of (nil, id) =>
                           let val (i',R) =
                                   case regvars_opt of
                                       NONE => (okConv i,nil)
                                     | SOME (i2,regvars) =>
                                       case getRepeatedElements RegVar.eq regvars of
                                           [] => (okConv i,regvars)
                                         | repeated =>
                                           (repeatedIdsError (i2, map ErrorInfo.REGVAR_RID repeated),regvars)
                           in (Substitution.Id,
                               (VE.singleton_var(id, tau_scheme), tau, R),
                               OG.LONGIDatpat(addTypeInfo_VAR_PAT(i',
                                                                  tau),
                                              OG.OP_OPT(longid, withOp),
                                              regvars_opt'
                                             )
                              )
                           end
                         | (_, _) =>
                             (Substitution.Id,
                              (VE.bogus, Type_bogus (), nil),
                              OG.LONGIDatpat(
                                errorConv(i, ErrorInfo.QUALIFIED_ID longid),
                                OG.OP_OPT(longid, withOp),
                                regvars_opt'
                              )
                             )
                    end
            )

          (* Record pattern *)                                  (*rule 36*)
        | IG.RECORDatpat(i, row_opt as NONE) =>
            (Substitution.Id,
             (VE.empty, Type.Unit, nil),
              OG.RECORDatpat(okConv i, NONE))

        | IG.RECORDatpat(i, row_opt as SOME patrow) =>
            let
              val (S, (VE, rho), out_patrow) = elab_patrow(C, patrow)
            in
              (S,
               (VE,Type.from_RecType (rho,NONE), nil),
                OG.RECORDatpat(addTypeInfo_RECORD_ATPAT(okConv i,
                                                        Type.from_RecType(rho,NONE)),
                               SOME(out_patrow)))
            end

          (* Parenthesised pattern *)                           (*rule 37*)
        | IG.PARatpat(i, pat) =>
            let val (S, (VE,tau,R), out_pat) = elab_pat(C, pat)
            in (S, (VE,tau,R), OG.PARatpat(okConv i,out_pat)) end

    (****** pattern rows ******)

    and elab_patrow (C : Context, patrow: IG.patrow)
          : (Substitution * (VarEnv * RecType) * OG.patrow) =
      case patrow of

           (* Pattern row *)                                    (*rule 39*)
           IG.PATROW(i, lab, pat, NONE) =>
             let
               val (S, (VE, tau, _), out_pat) = elab_pat(C, pat)
             in
               (S, (VE, Type.RecType.add_field (lab, tau) Type.RecType.empty),
                OG.PATROW(okConv i, lab, out_pat, NONE)
               )
             end

         | IG.PATROW(i, lab, pat, SOME patrow) =>
             let
               val (S, (VE, tau, _), out_pat) = elab_pat(C, pat)
               val (S', (VE', rho), out_patrow) = elab_patrow(C, patrow)
               val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
             in
               case (EqSet.isEmpty intdom,
                     ListHacks.member lab (Type.RecType.sorted_labs rho)) of
                 (true, false) =>
                   (S' oo S,
                    (VE.plus (VE, VE'), Type.RecType.add_field (lab, tau) rho
                     ), OG.PATROW(okConv i, lab, out_pat, SOME out_patrow))
               | (true, true) =>
                   (Substitution.Id,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i,[ErrorInfo.LAB_RID lab]),
                              lab, out_pat, SOME out_patrow))
               | (false, false) =>
                   (Substitution.Id,
                    (VE', Type.RecType.add_field (lab, tau) rho),
                    OG.PATROW(repeatedIdsError(i,
                                  map ErrorInfo.ID_RID (EqSet.list intdom)),
                              lab, out_pat, SOME out_patrow))
               | (false, true) =>
                   (Substitution.Id,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i,
                               (map ErrorInfo.ID_RID (EqSet.list intdom)) @
                               [ErrorInfo.LAB_RID lab]),
                              lab, out_pat, SOME out_patrow))
             end

        | IG.DOTDOTDOT i => (* Flexible record treatment... *)  (*rule 38*)
            let
              val rho = Type.RecType.dotdotdot ()
            in
              (Substitution.Id,
               (VE.empty, rho),
               OG.DOTDOTDOT(preOverloadingConv(i,
                   OverloadingInfo.UNRESOLVED_DOTDOTDOT rho)))
            end

    (****** patterns - Definition, p. ? ******)

    and elab_pat (C : Context, pat : IG.pat)
      : (Substitution * (VarEnv * Type * RegVar.regvar list) * OG.pat) =
      let
        val (S, (VE, ty, R), pat') = elab_pat'(C, pat)
      in
        (S, (VE, ty, R), pat')
      end

    and elab_pat' (C, pat) =
        case pat of

          (* Atomic pattern *)                                  (*rule 40*)
          IG.ATPATpat(i, atpat) =>
            let val (S, (VE,tau,R), out_atpat) = elab_atpat(C, atpat)
            in (S, (VE,tau,R), OG.ATPATpat(okConv i,out_atpat)) end

          (* Constructed pattern *)                             (*rule 41*)
        | IG.CONSpat(i, IG.OP_OPT(longid, withOp), atpat) =>
            let
              val (S, (VE,tau',R), out_atpat) = elab_atpat(C, atpat)
            in
              case C.lookup_longid C longid of

                SOME(VE.LONGCON sigma) =>
                  let
                    val new = Type.fresh_normal ()
                    val arrow = Type.mk_Arrow(tau', new, NONE)
                    val (tau1,instances) = (TypeScheme.instance' sigma)
                    val (S1, i') = UnifyWithTexts("argument to long value constructor \
                                                  \in pattern suggests constructor type",
                                                  arrow,
                                                  "but constructor has type", tau1, i)
                    val tau2 = S1 on new
                  in
                    (S1 oo S, (S1 onVE VE, tau2, R),
                     OG.CONSpat(addTypeInfo_CON(i', C, instances, longid),
                                OG.OP_OPT(longid, withOp),
                                out_atpat
                                )
                    )
                  end

              | SOME(VE.LONGEXCON tau) =>
                  let
                    val arrow = Type.mk_Arrow(tau',Type.Exn, NONE)
                    val (S1, i') = UnifyWithTexts("argument to long \
                          \exception constructor in pattern requires exception \
                          \constructor type ", arrow,
                          "but the exception constructor has type", tau, i)
                  in
                    (S1 oo S,
                     (S1 onVE VE,Type.Exn,nil),
                     OG.CONSpat(addTypeInfo_EXCON(i',S1 on arrow,longid),
                                OG.OP_OPT(longid, withOp),
                                    out_atpat
                               )
                    )
                  end

              | _ => (* Mark the error. *)
                  (Substitution.Id,
                   (VE, Type_bogus (),nil),
                   OG.CONSpat(lookupIdError(i, longid),
                              OG.OP_OPT(Ident.bogus, withOp),
                              out_atpat
                             )
                  )
            end

          (* Typed pattern *)                                   (*rule 42*)
        | IG.TYPEDpat(i, pat, ty) =>
            let val (S, (VE,tau,R), out_pat) = elab_pat(C, pat)
            in case elab_ty(C, ty)
                 of (SOME tau', out_ty) =>
                   let val (S', i') = UnifyWithTexts("pattern has type", tau, "which conflicts \
                                                     \with your type constraint", tau', i)
                       val S'' = S' oo S
                       val tau'' = S' on tau'
                   in (S'', (S'' onVE VE, S'' on tau, R),
                       OG.TYPEDpat(addTypeInfo_VAR_PAT(i',tau''), out_pat, out_ty))
                   end
                  | (NONE, out_ty) => (S, (VE, tau, R), OG.TYPEDpat(okConv i, out_pat, out_ty))
            end

          (* Layered pattern *)                                 (*rule 43*)
        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), NONE, pat) =>
            let
              val (S, (VE1, tau, _), out_pat) = elab_pat(C, pat)
              val VE2 = VE.singleton_var(id, TypeScheme.from_Type tau)
              val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
              val VE3 = VE.plus (VE1, VE2)
            in
              if EqSet.isEmpty intdom then
                (S, (VE3, tau, nil),
                 OG.LAYEREDpat(addTypeInfo_VAR_PAT(okConv i,tau),
                               OG.OP_OPT(id, withOp),
                               NONE, out_pat))
              else
                (S, (VE3, tau, nil),
                 OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID
                                                   (EqSet.list intdom)),
                               OG.OP_OPT(id, withOp),
                               NONE, out_pat))
            end

        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), SOME ty, pat) =>
            let val (S, (VE1, tau, _), out_pat) = elab_pat(C, pat)
            in case elab_ty(C, ty)
                 of (SOME tau', out_ty) =>
                   let val (S', i') = UnifyWithTexts("pattern has type", tau,
                                                     "which conflicts with your constraint", tau', i)
                       val i' = addTypeInfo_VAR_PAT(i', tau') (*added, mads*)
                       val S'' = S' oo S
                       val VE2 = VE.singleton_var (id, TypeScheme.from_Type tau)
                       val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
                       val VE3 = VE.plus (VE1, VE2)
                   in
                     if EqSet.isEmpty intdom then
                       (S'',
                        (S'' onVE VE3, S'' on tau, nil),
                        OG.LAYEREDpat(i', OG.OP_OPT(id, withOp), SOME out_ty, out_pat)
                        )
                     else
                       (S'',
                        (S'' onVE VE3, S'' on tau, nil),
                        OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID (EqSet.list intdom)),
                                      OG.OP_OPT(id, withOp), SOME out_ty, out_pat)
                        )
                   end
                  | (NONE, out_ty) =>
                   (S, (VE1, tau, nil), OG.LAYEREDpat(okConv i, OG.OP_OPT(id, withOp), SOME out_ty, out_pat))
            end

        | IG.UNRES_INFIXpat _ =>
            impossible "elab_pat(UNRES_INFIX)"

    (****** types  ******)

                (* elab_ty returns `NONE' if an error occurred when elborating the
                 * type expression. The reason we do things this way is that
                 * errors are dealt with in two different ways depending on the
                 * construct the type expression is part of. *)


    and elab_ty (C : Context, ty : IG.ty) : (Type option * OG.ty) =

        case ty of

          (* Explicit type variable *)                          (*rule 44*)
          IG.TYVARty(i, ExplicitTyVar) =>
            let val ty_opt = C.ExplicitTyVar_lookup C ExplicitTyVar
                val i =
                    case ty_opt of
                        SOME _ => okConv i
                      | NONE => errorConv (i,ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
                                           [SyntaxTyVar.pr_tyvar ExplicitTyVar])
            in (ty_opt, OG.TYVARty(i, ExplicitTyVar))
            end

          (* Record type *)                                     (*rule 45*)
         | IG.RECORDty(i, NONE, NONE) =>  (SOME Type.Unit,
                                           OG.RECORDty (okConv i, NONE, NONE))

         | IG.RECORDty(i, NONE, SOME (i2,rv)) =>
           (SOME Type.Unit,
            OG.RECORDty (okConv i, NONE,
                         SOME (errorConv (i2, ErrorInfo.REGVAR_TY_UNBOXED),rv)))

          (* Record type *)
        | IG.RECORDty(i, SOME tyrow, NONE) =>  (* The error has already been reported. *)
           (case elab_tyrow(C, tyrow)
              of (SOME rho, out_tyrow) => (SOME (Type.from_RecType (rho,NONE)), OG.RECORDty(okConv i, SOME out_tyrow, NONE))
               | (NONE, out_tyrow) => (NONE, OG.RECORDty(okConv i, SOME out_tyrow, NONE)))

        | IG.RECORDty(i, SOME tyrow, SOME(i2,rv)) =>  (* The error has already been reported. *)
          (case elab_tyrow(C, tyrow)
             of (SOME rho, out_tyrow) => (SOME (Type.from_RecType (rho,SOME(i2,rv))),
                                          OG.RECORDty(okConv i, SOME out_tyrow, SOME(okConv i2,rv)))
               | (NONE, out_tyrow) => (NONE, OG.RECORDty(okConv i, SOME out_tyrow, SOME(okConv i, rv))))


        (* Constructed type *)                                  (*rule 46*)
        | IG.CONty(i, ty_list, longtycon) =>
            let
              val res_list = map (fn ty => elab_ty (C, ty)) ty_list
              val tau_opt_list = map #1 res_list
              val out_ty_list = map #2 res_list
              fun unopt_list ([],a) = SOME (rev a)
                | unopt_list (NONE::rest,a) = NONE
                | unopt_list (SOME tau::rest, a) = unopt_list (rest, tau::a)
            in
              case unopt_list (tau_opt_list, [])
                of SOME tau_list =>
                  (case C.lookup_longtycon C longtycon of
                     SOME tystr =>
                       let
                         val (typeFcn, _) = TyStr.to_theta_and_VE tystr
                         val expectedArity = TypeFcn.arity typeFcn
                         val actualArity = length tau_list
                       in
                         if expectedArity = actualArity then
                           (SOME(TypeFcn.apply (typeFcn, tau_list)),
                            OG.CONty (okConv i, out_ty_list, longtycon))
                         else
                           (NONE,
                            OG.CONty (errorConv (i, ErrorInfo.WRONG_ARITY
                                                 {expected=expectedArity,
                                                  actual=actualArity}),
                                      out_ty_list, longtycon))
                       end
                   | NONE => (NONE, OG.CONty(lookupTyConError(i, longtycon), out_ty_list, longtycon)))
                 | NONE => (NONE, OG.CONty(okConv i, out_ty_list, longtycon))
            end

          (* Function type *)                                   (*rule 47*)
        | IG.FNty(i, ty, ty') =>
            (case (elab_ty(C, ty ), elab_ty(C, ty'))
               of ((SOME tau, out_ty), (SOME tau', out_ty')) =>
                 (SOME (Type.mk_Arrow (tau, tau', NONE)), OG.FNty(okConv i, out_ty, out_ty'))
                | ((_, out_ty), (_, out_ty')) => (NONE, OG.FNty(okConv i, out_ty, out_ty')))

          (* Parenthesised type *)                              (*rule 48*)
        | IG.PARty(i, ty, rvsopt) =>
            let val (tau_opt, out_ty) = elab_ty(C, ty)
                val (tau_opt', rvsopt') =
                    case rvsopt of
                        NONE => (tau_opt, NONE)
                      | SOME (i,rvs) =>
                        let val rvs' = map (fn (i,rv) => (okConv i,rv)) rvs
                            val rvsopt' = SOME (okConv i, rvs')
                        in case tau_opt of
                               NONE => (tau_opt, rvsopt')
                             | SOME tau =>
                               case rvs' of
                                   nil => (tau_opt, rvsopt')
                                 | _ =>
                                   let val tau' = Type.add_regvars (i,rvs) tau
                                   in (SOME tau', rvsopt')
                                   end handle Fail msg =>
                                              (SOME tau, SOME(errorConv (i, ErrorInfo.REGVAR_TY_ANNOTATE msg),rvs'))
                        end
            in
              (tau_opt', OG.PARty(okConv i, out_ty, rvsopt'))
            end

    (****** type rows ******)

    and elab_tyrow (C : Context, IG.TYROW(i, lab, ty, tyrow_opt)) : (RecType option * OG.tyrow) =
      case (elab_ty(C, ty), elab_tyrow_opt(C, tyrow_opt))
        of ((SOME tau, out_ty), (SOME rho', out_tyrow_opt)) =>
          let
            val (rho, i') =
              if (ListHacks.member lab (Type.RecType.sorted_labs rho')) then
                (rho', repeatedIdsError(i, [ErrorInfo.LAB_RID lab]))
              else (Type.RecType.add_field (lab,tau) rho', okConv i)
          in (SOME rho, OG.TYROW(i', lab, out_ty, out_tyrow_opt))
          end

         | ((NONE, out_ty), (SOME rho', out_tyrow_opt)) =>
          (SOME rho', OG.TYROW(okConv i, lab, out_ty, out_tyrow_opt))

         | ((_, out_ty), (_, out_tyrow_opt)) =>
          (NONE, OG.TYROW(okConv i, lab, out_ty, out_tyrow_opt))

      and elab_tyrow_opt(C, NONE) = (SOME Type.RecType.empty, NONE)
        | elab_tyrow_opt(C, SOME tyrow) =
        case elab_tyrow(C, tyrow)
          of (SOME rho, out_tyrow) => (SOME rho, SOME out_tyrow)
           | (NONE, out_tyrow) => (NONE, SOME out_tyrow)

(**** Overloading resolution ****)

fun resolve_overloading (S : Substitution, dec : OG.dec): OG.dec =

    (* resolves overloading in dec, by applying S on every recorded
       overloaded type variable --- if repeated application of S
       yields int or real, we record this information in the info
       field; otherwise overloading cannot be resolved
       and error-info is inserted in the info-field.
     *)

let
  open OG

  local
  val tau_to_overloadinginfo_alist =
        [(Type.Int31,  OverloadingInfo.RESOLVED_INT31),
         (Type.Int32,  OverloadingInfo.RESOLVED_INT32),
         (Type.IntInf, OverloadingInfo.RESOLVED_INTINF),
         (Type.Real,   OverloadingInfo.RESOLVED_REAL),
         (Type.String, OverloadingInfo.RESOLVED_STRING),
         (Type.Char,   OverloadingInfo.RESOLVED_CHAR),
         (Type.Word8,  OverloadingInfo.RESOLVED_WORD8),
         (Type.Word31, OverloadingInfo.RESOLVED_WORD31),
         (Type.Word32, OverloadingInfo.RESOLVED_WORD32)]
        @
        (if values_64bit() then
           [(Type.Int63,  OverloadingInfo.RESOLVED_INT63),
            (Type.Int64,  OverloadingInfo.RESOLVED_INT64),
            (Type.Word63, OverloadingInfo.RESOLVED_WORD63),
            (Type.Word64, OverloadingInfo.RESOLVED_WORD64)]
         else [])

  fun tau_to_overloadinginfo tau  =
    case List.find (fn (tau', oi) => Type.eq (tau, tau')) tau_to_overloadinginfo_alist
      of SOME res => #2 res
       | NONE => OverloadingInfo.resolvedIntDefault()   (* can happen only if type error *)

  fun resolve_tv (tv : TyVar) : Type =
    let val ts = StatObject.TyVar.resolve_overloaded tv
      open TyName
    in
      if Set.member tyName_INT32 ts then
          if Set.member tyName_INT31 ts then
            Type.IntDefault()
          else Type.Int32
      else
        if Set.member tyName_WORD32 ts then
          if Set.member tyName_WORD31 ts then
            Type.WordDefault()
          else Type.Word32
        else Crash.impossible ("resolve_tv.hmm; maybe insert cases for string, etc; ts = {"
                               ^ String.concatWith "," (map pr_TyName (Set.list ts)) ^ "}")
    end

  fun resolve_tau tau : OverloadingInfo.OverloadingInfo =
        let val tau' = S on tau
        in case Type.to_TyVar tau'
             of NONE => tau_to_overloadinginfo tau'
                 (*TODO 25/06/1997 10:11. tho.
                  I'd rather do an impossible here: If tau' is not a
                  tyvar, it must be one of {int31, int32, intinf, real, string,
                  char, word8, word31, word32}; everything else would
                  be a type error.  Well,
                  perhaps it can occur then, namely when there is a
                  type error (they do occur), and since type errors
                  should not make the compiler crash, it is probably
                  best to not do an impossible.  The only thing to do
                  is then to return `default', as unresolved
                  overloading should not result in an error message. *)
              | SOME tv' =>
               if Type.eq (tau', tau)
               then (*tau' is a tyvar, so the overloading is as yet
                     unresolved, & according to the Definition of SML '97, it
                     must be resolved to a default type.  And now someone has remembered
                     to put this resolve into work by unifying the tyvar with
                     the default type:*)
                 let val t = resolve_tv tv'
                   val _ = Type.unify {unify_regvars=false} (t, tau')
                 in tau_to_overloadinginfo tau'
                 end
               else (*repeat application of S:*) resolve_tau tau'
        end
  in

  (*resolve_tyvar gives `default' overloading info when overloading
   couldn't be resolved.  According to the definition (p. 72), int is
   the default type except for /, but / is not overloaded in this
   compiler; / always has type real * real -> real, as there is only
   one kind of real.  25/06/1997 10:30. tho. Another exception is when
   the choice is between word8, word31, and word32! mael 2001-05-15 *)

    fun resolve_tyvar (tv : TyVar) : OverloadingInfo.OverloadingInfo =
      (resolve_tau o Type.from_TyVar) tv

  end (*local*)

  datatype flexresResult = FLEX_RESOLVED | FLEX_NOTRESOLVED
  fun flexrecres (rho : RecType) : flexresResult =
        let
          fun loop typ =
                let val typ' = S on typ
                in
                  if Type.eq (typ',typ) then typ else loop typ'
                end
        in
          if Type.contains_row_variable (loop (Type.from_RecType (rho,NONE)))
          then FLEX_NOTRESOLVED else FLEX_RESOLVED
        end

  local
    open TypeInfo
    infixr on_TypeInfo

    fun S on_TypeInfo i =
      case i
        of LAB_INFO {index} => LAB_INFO {index=index}
         | RECORD_ATPAT_INFO {Type} => RECORD_ATPAT_INFO {Type=S on Type}
         | VAR_INFO {instances} => VAR_INFO {instances=map (fn tau => S on tau) instances}
         | VAR_PAT_INFO {tyvars,Type} => VAR_PAT_INFO {tyvars=tyvars,Type=S on Type}
         | CON_INFO {numCons,index,longid,instances} =>
          CON_INFO {numCons=numCons,index=index,longid=longid,
                    instances= map (fn tau => S on tau) instances}
         | EXCON_INFO {Type,longid} => EXCON_INFO {Type=S on Type,longid=longid}
         | EXBIND_INFO {TypeOpt=NONE} => EXBIND_INFO {TypeOpt=NONE}
         | EXBIND_INFO {TypeOpt=SOME Type} => EXBIND_INFO {TypeOpt=SOME (S on Type)}
         | TYENV_INFO TE => TYENV_INFO TE                  (*no free tyvars here*)
         | ABSTYPE_INFO (TE,rea) => ABSTYPE_INFO (TE,rea)  (*no free tyvars here*)
         | EXP_INFO {Type} => EXP_INFO {Type=S on Type}
         | MATCH_INFO {Type} => MATCH_INFO {Type=S on Type}
         | PLAINvalbind_INFO {tyvars,Type} =>
          PLAINvalbind_INFO {tyvars=tyvars, Type=S on Type}
         | OPEN_INFO i => OPEN_INFO i  (*only identifiers*)
         | INCLUDE_INFO i => INCLUDE_INFO i  (*only identifiers*)
         | FUNCTOR_APP_INFO rea => FUNCTOR_APP_INFO rea   (* type functions are closed *)
         | FUNBIND_INFO E => FUNBIND_INFO E (* signatures are closed *)
         | TRANS_CONSTRAINT_INFO E => TRANS_CONSTRAINT_INFO E (* signatures are closed *)
         | OPAQUE_CONSTRAINT_INFO E_and_phi =>
          OPAQUE_CONSTRAINT_INFO E_and_phi (* signatures and realisations are closed *)
         | SIGBIND_INFO _ => impossible "on_TypeInfo.SIGBIND_INFO"
         | DELAYED_REALISATION _ => impossible "on_TypeInfo.DELAYED_REALISATION"
  in
    fun resolve_i ElabInfo =
          (case ElabInfo.to_TypeInfo ElabInfo of
             SOME typeinfo =>
               ElabInfo.plus_TypeInfo ElabInfo (S on_TypeInfo typeinfo)
           | NONE => ElabInfo)

  end (*local open TypeInfo ...*)

  exception DDD_IS_EMPTY (* raised by handling of ... when ... stands for the empty set of labels *)

  (*resolve_X X: apply resolve_i to all info fields i in X and resolve_tyvar to
   all overloadinginfos on id's in X, and also do something about flex
   records.*)

  fun resolve_atexp (atexp : atexp) : atexp =
      case atexp of
          SCONatexp(i,scon,rv_opt) =>
            (case ElabInfo.to_OverloadingInfo i
               of NONE => SCONatexp(resolve_i i, scon, rv_opt)
                | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
                 SCONatexp (ElabInfo.plus_OverloadingInfo i (resolve_tyvar tyvar),
                            scon, rv_opt)
                | SOME _ => impossible "resolve_atexp.SCON")
        | IDENTatexp(i, op_opt, regvars_opt) =>
              (case ElabInfo.to_OverloadingInfo i of
                   NONE => IDENTatexp (resolve_i i, op_opt, regvars_opt)
                 | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
                     IDENTatexp (ElabInfo.plus_OverloadingInfo i (resolve_tyvar tyvar),
                                 op_opt, regvars_opt)
                 | SOME _ => impossible "resolve_atexp")
        | RECORDatexp(i, NONE, rv_opt) => RECORDatexp(resolve_i i,NONE, rv_opt)
        | RECORDatexp(i, SOME exprow, rv_opt) =>
              RECORDatexp(resolve_i i, SOME (resolve_exprow exprow), rv_opt)
        | LETatexp(i, dec, exp) =>
              LETatexp(resolve_i i, resolve_dec dec, resolve_exp exp)
        | PARatexp(i, exp) =>
              PARatexp(resolve_i i, resolve_exp exp)

  and resolve_exprow (exprow: exprow) : exprow =
    case exprow of
      EXPROW(i, l, exp, NONE) =>
        EXPROW(resolve_i i, l, resolve_exp exp, NONE)
    | EXPROW(i, l, exp, SOME exprow) =>
        EXPROW(resolve_i i, l, resolve_exp exp, SOME (resolve_exprow exprow))

  and resolve_exp (exp: exp) : exp =
    case exp of
      ATEXPexp(i, atexp) => ATEXPexp(resolve_i i, resolve_atexp atexp)
    | APPexp(i, exp, atexp) => APPexp(resolve_i i, resolve_exp exp, resolve_atexp atexp)
    | TYPEDexp(i, exp, ty) => TYPEDexp(resolve_i i, resolve_exp exp, ty)
    | HANDLEexp(i, exp, match) => HANDLEexp(resolve_i i, resolve_exp exp, resolve_match match)
    | RAISEexp(i, exp) => RAISEexp(resolve_i i, resolve_exp exp)
    | FNexp(i, match) => FNexp(resolve_i i, resolve_match match)
    | UNRES_INFIXexp _ => impossible "resolve_exp(UNRES_INFIX)"

  and resolve_match (match: match) : match =
    case match of
      MATCH(i, mrule, NONE) =>
        MATCH(resolve_i i, resolve_mrule mrule, NONE)
    | MATCH(i, mrule, SOME match) =>
        MATCH(resolve_i i, resolve_mrule mrule, SOME (resolve_match match))

  and resolve_mrule (MRULE(i, pat, exp) : mrule) : mrule =
    MRULE(resolve_i i, resolve_pat pat, resolve_exp exp)

  and resolve_dec (dec : dec) : dec =
    (case dec of
       VALdec(i, tyvars, valbind) =>
         VALdec(resolve_i i, tyvars, resolve_valbind valbind)
     | UNRES_FUNdec _ => impossible "resolve_dec(UNRES_FUNdec)"
     | TYPEdec _ => dec
     | DATATYPEdec(i,datbind) => DATATYPEdec(resolve_i i,datbind)
     | DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
         DATATYPE_REPLICATIONdec(resolve_i i, tycon, longtycon)
     | ABSTYPEdec(i, datbind, dec) =>
         ABSTYPEdec(resolve_i i, datbind, resolve_dec dec)
     | EXCEPTIONdec(i,exbind) => EXCEPTIONdec(resolve_i i, exbind)
     | LOCALdec(i, dec1, dec2) =>
         LOCALdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
     | OPENdec _ => dec
     | SEQdec(i, dec1, dec2) =>
         SEQdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
     | INFIXdec _ => dec
     | INFIXRdec _ => dec
     | NONFIXdec _ => dec
     | EMPTYdec _ => dec
     | REGIONdec _ => dec)

  and resolve_valbind (valbind : valbind) : valbind =
    case valbind of
      PLAINvalbind(i, pat, exp, NONE) =>
        PLAINvalbind(i, resolve_pat pat, resolve_exp exp, NONE)
    | PLAINvalbind(i, pat, exp, SOME valbind) =>
        PLAINvalbind(i, resolve_pat pat,
                     resolve_exp exp, SOME (resolve_valbind valbind))
    | RECvalbind(i, valbind) =>
        RECvalbind(i, resolve_valbind valbind)

  and resolve_atpat (atpat : atpat) : atpat =
    case atpat of
      WILDCARDatpat _ => atpat
    | SCONatpat(i,scon) =>
        (case ElabInfo.to_OverloadingInfo i
           of NONE => SCONatpat(resolve_i i, scon)
            | SOME (OverloadingInfo.UNRESOLVED_IDENT tyvar) =>
             SCONatpat (ElabInfo.plus_OverloadingInfo i (resolve_tyvar tyvar),
                        scon)
            | SOME _ => impossible "resolve_atpat.SCON")
    | LONGIDatpat(i,x,y) => LONGIDatpat(resolve_i i,x,y)
    | RECORDatpat(i, NONE) => RECORDatpat(resolve_i i,NONE)
    | RECORDatpat(i, SOME patrow) =>
        let
          val i' = resolve_i i
        in let  val patrow' = resolve_patrow patrow
           in
             case ElabInfo.to_TypeInfo i' of
               SOME typeinfo =>
                 (case typeinfo of
                    TypeInfo.RECORD_ATPAT_INFO{Type} =>
                      (* Type has been resolved, c.f. i' *)
                      RECORDatpat(i',SOME (addLabelIndexInfo(Type,patrow')))
                  | _ => impossible ("resolve_atpat(RECORDatpat): " ^
                                     "wrong typeinfo"))
             | NONE => impossible ("resolve_atpat(RECORDatpat): " ^
                                   "no typeinfo")
           end handle DDD_IS_EMPTY => RECORDatpat(i',NONE)
        end
    | PARatpat(i, pat) =>
        PARatpat(resolve_i i, resolve_pat pat)

  and resolve_patrow (patrow : patrow): patrow  =
    case patrow of
      DOTDOTDOT(i) =>
        (case (ElabInfo.to_OverloadingInfo i) of
           NONE => patrow
         | SOME (OverloadingInfo.UNRESOLVED_DOTDOTDOT rho) =>
             (case flexrecres rho of
                FLEX_RESOLVED => (* old: DOTDOTDOT (ElabInfo.remove_OverloadingInfo i)*)
                   (* expand "..." into a patrow with ordinary wildcards (_) *)
                   let val i0 = ElabInfo.from_ParseInfo(ElabInfo.to_ParseInfo i)
                       fun wild ty = ATPATpat(i0, WILDCARDatpat(ElabInfo.plus_TypeInfo i0 (TypeInfo.VAR_PAT_INFO{tyvars=[], Type= ty})))
                       val labs_and_types = Type.RecType.to_list rho
                       fun f ((lab,ty), acc) = SOME(PATROW(ElabInfo.plus_TypeInfo i0 (TypeInfo.LAB_INFO{index=0}),lab, wild ty, acc))
                   in case foldr f NONE labs_and_types of
                        SOME patrow' => patrow'
                      | NONE => raise DDD_IS_EMPTY (* caller of resolve_patrow should replace SOME(...) by NONE *)
                   end
              | FLEX_NOTRESOLVED =>
                  DOTDOTDOT(ElabInfo.plus_ErrorInfo i ErrorInfo.FLEX_REC_NOT_RESOLVED))
         | SOME _ => impossible "resolve_patrow")
    | PATROW(i, lab, pat, NONE) =>
        PATROW(resolve_i i, lab, resolve_pat pat, NONE)
    | PATROW(i, lab, pat, SOME patrow) =>
        PATROW(resolve_i i, lab, resolve_pat pat, SOME (resolve_patrow patrow)
                                                  handle DDD_IS_EMPTY => NONE)

  and resolve_pat (pat : pat) : pat =
    case pat of
      ATPATpat(i, atpat) => ATPATpat(resolve_i i, resolve_atpat atpat)
    | CONSpat(i, longidopt, atpat) => CONSpat(resolve_i i, longidopt, resolve_atpat atpat)
    | TYPEDpat(i, pat, ty) => TYPEDpat(resolve_i i, resolve_pat pat, ty)
    | LAYEREDpat(i, idopt, tyopt, pat) => LAYEREDpat(resolve_i i, idopt, tyopt, resolve_pat pat)
    | UNRES_INFIXpat _ => impossible "resolve_pat(UNRES_INFIX)"

in
  resolve_dec dec
end (*fun resolve_overloading (ugly)*)

    (****** Elaborate a declaration and resolve overloading ******)

    val elab_dec : (Context * IG.dec) -> (TyName list * Env * OG.dec) =

      fn (C, dec) =>
        let
          val (S, T, E, out_dec) = elab_dec(C, dec)
          val dec' = resolve_overloading (S, out_dec)
        in
          (T, E, dec')
        end

end
