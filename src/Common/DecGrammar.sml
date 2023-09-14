(* Bare language - Definition v3 pages 8,9,70,71 *)
(* modified to have ident in place of con and var *)

functor DecGrammar(GrammarInfo: sig type GrammarInfo
                                    val bogus_info : GrammarInfo
                                end): DEC_GRAMMAR =
struct

  structure Lab = Lab
  structure SCon = SCon
  structure TyVar = SyntaxTyVar
  structure TyCon = TyCon
  structure StrId = StrId
  structure Ident = Ident
  structure RegVar = RegVar

  type lab    = Lab.lab             (* labels *)
   and scon   = SCon.scon           (* special constants *)
   and id     = Ident.id            (* identifiers - variables or constructors *)
   and longid = Ident.longid        (* long identifiers - variables or constructors *)
   and tyvar  = TyVar.SyntaxTyVar   (* type variables *)
   and tycon  = TyCon.tycon         (* type constructors *)
   and longtycon = TyCon.longtycon  (* long type constructors *)
   and longstrid = StrId.longstrid  (* structure identifiers *)
   and regvar = RegVar.regvar       (* region variables *)

  type info = GrammarInfo.GrammarInfo
  val bogus_info = GrammarInfo.bogus_info

  datatype 'a op_opt = OP_OPT of 'a * bool
  datatype 'a WithInfo = WITH_INFO of info * 'a
  fun strip_info (WITH_INFO (info,a)) = a

  datatype atexp =
        SCONatexp of info * scon * (info*regvar) option |
        IDENTatexp of info * longid op_opt * (info*regvar list) option |
        RECORDatexp of info * exprow option * (info*regvar) option |
        LETatexp of info * dec * exp |
        PARatexp of info * exp

  and opid = OPID of longid * bool

  and exprow =
        EXPROW of info * lab * exp * exprow option

  and exp =
        ATEXPexp of info * atexp |
        APPexp of info * exp * atexp |
        TYPEDexp of info * exp * ty |
        HANDLEexp of info * exp * match |
        RAISEexp of info * exp |
        FNexp of info * match |
        UNRES_INFIXexp of info * atexp list

  and match =
        MATCH of info * mrule * match option

  and mrule =
        MRULE of info * pat * exp

  and dec =
        VALdec of info * tyvar list * valbind |
        UNRES_FUNdec of info * tyvar list * FValBind |
        TYPEdec of info * typbind |
        DATATYPEdec of info * datbind |
        DATATYPE_REPLICATIONdec of info * tycon * longtycon |
        ABSTYPEdec of info * datbind * dec |
        EXCEPTIONdec of info * exbind |
        LOCALdec of info * dec * dec |
        OPENdec of info * longstrid WithInfo list |
        SEQdec of info * dec * dec |
        INFIXdec of info * int option * id list |
        INFIXRdec of info * int option * id list |
        NONFIXdec of info * id list |
        EMPTYdec of info |
        REGIONdec of info * (info*regvar list)

  and valbind =
        PLAINvalbind of info * pat * exp * valbind option |
        RECvalbind of info * valbind

  and FValBind = FVALBIND of info * FClause * FValBind option
  and FClause = FCLAUSE of info * atpat list * ty option * exp * FClause option

  and typbind =
        TYPBIND of info * tyvar list * tycon * ty * typbind option

  and datbind =
        DATBIND of info * tyvar list * tycon * conbind * datbind option

  and conbind =
        CONBIND of info * id op_opt * ty option * conbind option

  and exbind =
        EXBIND of info * id op_opt * ty option * exbind option |
        EXEQUAL of info * id op_opt * longid op_opt * exbind option

  and atpat =
        WILDCARDatpat of info |
        SCONatpat of info * scon |
        LONGIDatpat of info * longid op_opt * (info*regvar list) option |
        RECORDatpat of info * patrow option |
        PARatpat of info * pat

  and patrow =
        DOTDOTDOT of info |
        PATROW of info * lab * pat * patrow option

  and pat =
        ATPATpat of info * atpat |
        CONSpat of info * longid op_opt * atpat |
        TYPEDpat of info * pat * ty |
        LAYEREDpat of info * id op_opt * ty option * pat |
        UNRES_INFIXpat of info * atpat list

  and ty =
        TYVARty of info * tyvar |
        RECORDty of info * tyrow option * (info*regvar) option |
        CONty of info * ty list * longtycon |
        FNty of info * ty * (info*regvar) option * ty |
        PARty of info * ty * (info*(info*regvar)list) option |
        WITHty of info * ty * constraint

  and constraint =
        DISJOINTconstraint of info * eff * eff * bool |   (* true if puts only *)
        INCLconstraint of info * (info*regvar) * eff |
        PROPconstraint of info * prop * eff

  and prop =
        NOMUTprop of info |
        NOPUTprop of info |
        NOEXNprop of info

  and eff =
        SETeff of info * ateff list |
        VAReff of info * regvar

  and ateff =
        VARateff of info * regvar |
        PUTateff of info * (info*regvar) |
        GETateff of info * (info*regvar)

  and tyrow =
        TYROW of info * lab * ty * tyrow option

  fun get_info_atexp obj =
    case obj of
      SCONatexp x => (#1) x
    | IDENTatexp x => (#1) x
    | RECORDatexp x => (#1) x
    | LETatexp x => (#1) x
    | PARatexp x => (#1) x

  and get_info_exprow obj =
    case obj of
      EXPROW x => (#1) x

  and get_info_exp obj =
    case obj of
      ATEXPexp x => (#1) x
    | APPexp x => (#1) x
    | TYPEDexp x => (#1) x
    | HANDLEexp x => (#1) x
    | RAISEexp x => (#1) x
    | FNexp x => (#1) x
    | UNRES_INFIXexp x => (#1 x)

  and get_info_match obj =
    case obj of
      MATCH x => (#1) x

  and get_info_mrule obj =
    case obj of
      MRULE x => (#1) x

  and get_info_dec obj =
    case obj of
      VALdec x => (#1) x
    | UNRES_FUNdec x => (#1 x)
    | TYPEdec x => (#1) x
    | DATATYPEdec x => (#1) x
    | DATATYPE_REPLICATIONdec x => (#1) x
    | ABSTYPEdec x => (#1) x
    | EXCEPTIONdec x => (#1) x
    | LOCALdec x => (#1) x
    | OPENdec x => (#1) x
    | SEQdec x => (#1) x
    | INFIXdec x => (#1) x
    | INFIXRdec x => (#1) x
    | NONFIXdec x => (#1) x
    | EMPTYdec x => x
    | REGIONdec x => (#1) x

  and get_info_valbind obj =
    case obj of
      PLAINvalbind x => (#1) x
    | RECvalbind x => (#1) x

  and get_info_datbind obj =
    case obj of
      DATBIND x => (#1) x

  and get_info_conbind obj =
    case obj of
      CONBIND x => (#1) x

  and get_info_pat obj =
    case obj of
      ATPATpat x => (#1) x
    | CONSpat x => #1 x
    | TYPEDpat x => #1 x
    | LAYEREDpat x => #1 x
    | UNRES_INFIXpat x => #1 x

  and get_info_atpat obj =
    case obj of
      WILDCARDatpat x => x
    | SCONatpat x => (#1) x
    | LONGIDatpat x => (#1) x
    | RECORDatpat x => (#1) x
    | PARatpat x => (#1) x

  and get_info_patrow obj =
    case obj of
      DOTDOTDOT x => x
    | PATROW x => (#1) x

  and get_info_ty obj =
    case obj of
      TYVARty x => #1 x
    | RECORDty x => #1 x
    | CONty x => #1 x
    | FNty x => #1 x
    | PARty x => #1 x
    | WITHty x => #1 x

  and get_info_constraint c =
      case c of
          DISJOINTconstraint x => #1 x
        | INCLconstraint x => #1 x
        | PROPconstraint x => #1 x

  fun get_info_eff e =
      case e of
          SETeff x => #1 x
        | VAReff x => #1 x

  fun get_info_ateff ae =
      case ae of
          VARateff x => #1 x
        | PUTateff x => #1 x
        | GETateff x => #1 x

  fun get_info_typbind (TYPBIND (info, tyvars, tycon, ty, typbind_opt)) = info

  fun get_info_tyrow (TYROW (info, lab, ty, tyrow_opt)) = info

  fun get_info_exbind (EXBIND (info, id_op_opt, ty_opt, exbind_opt)) = info
    | get_info_exbind (EXEQUAL (info, id_op_opt, longid_op_opt, exbind_opt)) = info

  fun get_info_FValBind (FVALBIND (info, FClause, FValBind_opt)) = info

  fun get_info_FClause (FCLAUSE (info, atpats, tyOpt, exp, FClause_opt)) = info

  local
    fun do_opt opt f =
      case opt of
        NONE => NONE
      | SOME x  => SOME(f x)
  in
    fun map_atexp_info f (atexp : atexp) : atexp =
      case atexp of
        SCONatexp(i, x, y) => SCONatexp(f i, x, y)
      | IDENTatexp(i, x, y) => IDENTatexp(f i, x, y)
      | RECORDatexp(i, NONE, y) => RECORDatexp(f i,NONE, y)
      | RECORDatexp(i, SOME exprow, y) =>
          RECORDatexp(f i, SOME (map_exprow_info f exprow), y)
      | LETatexp(i, dec, exp) => LETatexp(f i, map_dec_info f dec, map_exp_info f exp)
      | PARatexp(i, exp) => PARatexp(f i, map_exp_info f exp)

    and map_exprow_info f (exprow: exprow) : exprow =
      case exprow of
        EXPROW(i, l, exp, NONE) => EXPROW(f i, l, map_exp_info f exp, NONE)
      | EXPROW(i, l, exp, SOME exprow) =>
          EXPROW(f i, l, map_exp_info f exp, SOME (map_exprow_info f exprow))

    and map_exp_info f (exp: exp) : exp =
      case exp of
        ATEXPexp(i, atexp) => ATEXPexp(f i, map_atexp_info f atexp)
      | APPexp(i, exp, atexp) =>
          APPexp(f i, map_exp_info f exp, map_atexp_info f atexp)
      | TYPEDexp(i, exp, ty) =>
          TYPEDexp(f i, map_exp_info f exp, ty)
      | HANDLEexp(i, exp, match) =>
          HANDLEexp(f i, map_exp_info f exp, map_match_info f match)
      | RAISEexp(i, exp) =>
          RAISEexp(f i, map_exp_info f exp)
      | FNexp(i, match) =>
          FNexp(f i, map_match_info f match)
      | UNRES_INFIXexp(i,atexps) =>
          UNRES_INFIXexp(f i, map (map_atexp_info f) atexps)

    and map_match_info f (match: match) : match =
      case match of
        MATCH(i, mrule, NONE) =>
          MATCH(f i, map_mrule_info f mrule, NONE)
      | MATCH(i, mrule, SOME match) =>
          MATCH(f i, map_mrule_info f mrule, SOME (map_match_info f match))

    and map_mrule_info f (MRULE(i, pat, exp) : mrule) : mrule =
      MRULE(f i, map_pat_info f pat, map_exp_info f exp)

    and map_dec_info f (dec : dec) : dec =
      case dec of
        VALdec(i, tyvars, valbind) => VALdec(f i, tyvars, map_valbind_info f valbind)
      | UNRES_FUNdec(i, tyvars, FValBind) =>
          UNRES_FUNdec(f i, tyvars, map_FValBind_info f FValBind)
      | TYPEdec(i,typbind) =>
          TYPEdec(f i, map_typbind_info f typbind)
      | DATATYPEdec(i,datbind) =>
          DATATYPEdec(f i,map_datbind_info f datbind)
      | DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
          DATATYPE_REPLICATIONdec(f i, tycon, longtycon)
      | ABSTYPEdec(i, datbind, dec) =>
          ABSTYPEdec(f i, map_datbind_info f datbind, map_dec_info f dec)
      | EXCEPTIONdec(i,exbind) =>
          EXCEPTIONdec(f i, map_exbind_info f exbind)
      | LOCALdec(i, dec1, dec2) =>
          LOCALdec(f i, map_dec_info f dec1, map_dec_info f dec2)
      | OPENdec(i,ls) => OPENdec(f i, ls)
      | SEQdec(i, dec1, dec2) =>
          SEQdec(f i, map_dec_info f dec1, map_dec_info f dec2)
      | INFIXdec(i,x,y) => INFIXdec(f i,x,y)
      | INFIXRdec(i,x,y) => INFIXRdec(f i,x,y)
      | NONFIXdec(i,x) => NONFIXdec(f i,x)
      | EMPTYdec i => EMPTYdec (f i)
      | REGIONdec(i,x) => REGIONdec(f i,x)

    and map_FValBind_info f (FVALBIND(i,FClause,FValBind_opt)) : FValBind =
      FVALBIND(f i, map_FClause_info f FClause,
               case FValBind_opt of
                 NONE => NONE
               | SOME FValBind => SOME (map_FValBind_info f FValBind))

    and map_FClause_info f (FCLAUSE(i,atpats,tyOpt,exp,FClause_opt)) : FClause =
      FCLAUSE(f i, map (map_atpat_info f) atpats,
              case tyOpt of
                NONE => NONE
              | SOME ty => SOME(map_ty_info f ty),
                  map_exp_info f exp,
                  case FClause_opt of
                    NONE => NONE
                  | SOME FClause => SOME(map_FClause_info f FClause))

    and map_valbind_info f (valbind : valbind) : valbind =
      case valbind of
        PLAINvalbind(i, pat, exp, NONE) =>
          PLAINvalbind(f i, map_pat_info f pat, map_exp_info f exp, NONE)
      | PLAINvalbind(i, pat, exp, SOME valbind) =>
          PLAINvalbind(f i, map_pat_info f pat,
                       map_exp_info f exp, SOME (map_valbind_info f valbind))
      | RECvalbind(i, valbind) =>
          RECvalbind(f i, map_valbind_info f valbind)

    and map_typbind_info f (TYPBIND(i,tyvars,tycon,ty,typbind_opt)): typbind =
      TYPBIND(f i,tyvars,tycon,map_ty_info f ty,
              do_opt typbind_opt (map_typbind_info f))

    and map_datbind_info f (DATBIND(i,tyvars,tycon,conbind,datbind_opt)): datbind =
      DATBIND(f i,tyvars,tycon,map_conbind_info f conbind,
              do_opt datbind_opt (map_datbind_info f))

    and map_conbind_info f (CONBIND(i,id,ty_opt,conbind_opt)): conbind =
      CONBIND(f i, id, do_opt ty_opt (map_ty_info f),
              do_opt conbind_opt (map_conbind_info f))

    and map_exbind_info f (exbind: exbind): exbind =
      case exbind of
        EXBIND(i,id,ty_opt,exbind_opt) =>
          EXBIND(f i, id, do_opt ty_opt (map_ty_info f),
                 do_opt exbind_opt (map_exbind_info f))
      | EXEQUAL(i,id,longid,exbind_opt) =>
          EXEQUAL(f i,id,longid,do_opt exbind_opt (map_exbind_info f))

    and map_atpat_info f (atpat : atpat) : atpat =
      case atpat of
        WILDCARDatpat i => WILDCARDatpat (f i)
      | SCONatpat(i,scon) => SCONatpat(f i, scon)
      | LONGIDatpat(i,x,y) => LONGIDatpat(f i,x,y)
      | RECORDatpat(i, NONE) => RECORDatpat(f i,NONE)
      | RECORDatpat(i, SOME patrow) =>
          RECORDatpat(f i, SOME (map_patrow_info f patrow))
      | PARatpat(i, pat) => PARatpat(f i, map_pat_info f pat)

    and map_patrow_info f (patrow : patrow): patrow  =
      case patrow of
        DOTDOTDOT(i) => DOTDOTDOT (f i)
      | PATROW(i, lab, pat, NONE) =>
          PATROW(f i, lab, map_pat_info f pat, NONE)
      | PATROW(i, lab, pat, SOME patrow) =>
          PATROW(f i, lab, map_pat_info f pat, SOME (map_patrow_info f patrow))

    and map_pat_info f (pat : pat) : pat =
      case pat of
        ATPATpat(i, atpat) =>
          ATPATpat(f i, map_atpat_info f atpat)
      | CONSpat(i, longidopt, atpat) =>
          CONSpat(f i, longidopt, map_atpat_info f atpat)
      | TYPEDpat(i, pat, ty) =>
          TYPEDpat(f i, map_pat_info f pat, map_ty_info f ty)
      | LAYEREDpat(i, id, ty_opt, pat) =>
          LAYEREDpat(f i, id, do_opt ty_opt (map_ty_info f),
                     map_pat_info f pat)
      | UNRES_INFIXpat(i,atpats) =>
          UNRES_INFIXpat(f i, map (map_atpat_info f) atpats)

    and map_ty_info f (ty: ty) : ty =
      case ty of
        TYVARty(i,tyvar) => TYVARty(f i, tyvar)
      | RECORDty(i,tyrow_opt,opt) => RECORDty(f i, do_opt tyrow_opt (map_tyrow_info f),Option.map (fn (i,rv) => (f i,rv)) opt)
      | CONty(i,tys,longtycon) => CONty(f i, map (map_ty_info f) tys,longtycon)
      | FNty(i,ty,opt,ty') => FNty(f i, map_ty_info f ty, Option.map (fn (i,rv) => (f i,rv)) opt, map_ty_info f ty')
      | PARty(i,ty,opt) => PARty(f i, map_ty_info f ty, Option.map (fn (i,rvis) => (f i,List.map (fn (i,rv) => (f i,rv)) rvis)) opt)
      | WITHty (i,t,c) => WITHty (f i, map_ty_info f t, map_constraint_info f c)

    and map_tyrow_info f (TYROW(i,lab,ty,tyrow_opt)) : tyrow =
      TYROW(f i, lab, map_ty_info f ty, do_opt tyrow_opt (map_tyrow_info f))

    and map_constraint_info f c =
        case c of
            DISJOINTconstraint (i,e1,e2,p) => DISJOINTconstraint (f i,map_eff_info f e1,map_eff_info f e2,p)
          | INCLconstraint (i,(ir,r),e) => INCLconstraint (f i,(f ir,r),map_eff_info f e)
          | PROPconstraint (i,p,e) => PROPconstraint (f i,map_prop_info f p,map_eff_info f e)

    and map_prop_info f e =
        case e of
            NOMUTprop i => NOMUTprop (f i)
          | NOPUTprop i => NOPUTprop (f i)
          | NOEXNprop i => NOEXNprop (f i)

    and map_eff_info f e =
        case e of
            SETeff (i,aes) => SETeff (f i,List.map (map_ateff_info f) aes)
          | VAReff (i,r) => VAReff (f i,r)

    and map_ateff_info f ae =
        case ae of
            VARateff (i,r) => VARateff (f i,r)
          | PUTateff (i,(ir,r)) => PUTateff (f i,(f ir,r))
          | GETateff (i,(ir,r)) => GETateff (f i,(f ir,r))
  end

  fun expansive (harmless_con : longid -> bool) exp =
    let
      fun nexp_exp (ATEXPexp(info, atexp)) = nexp_atexp atexp
        | nexp_exp (APPexp(info, exp, atexp)) = conexp_exp exp andalso nexp_atexp atexp
        | nexp_exp (TYPEDexp(info, exp,  ty)) = nexp_exp exp
        | nexp_exp (FNexp(info, match)) = true
        | nexp_exp _ = false

      and nexp_atexp (SCONatexp(info, scon, _)) = true
        | nexp_atexp (IDENTatexp(info, longid_op_opt, _)) = true
        | nexp_atexp (RECORDatexp(info, exprow_opt, _)) = nexp_exprow_opt exprow_opt
        | nexp_atexp (PARatexp(info, exp)) = nexp_exp exp
        | nexp_atexp _ = false

      and nexp_exprow_opt (SOME(EXPROW(info, lab, exp, exprow_opt))) =
            nexp_exp exp andalso nexp_exprow_opt exprow_opt
        | nexp_exprow_opt NONE = true

      and conexp_exp (ATEXPexp(info, atexp)) = conexp_atexp atexp
        | conexp_exp (TYPEDexp(info, exp, ty)) = conexp_exp exp
        | conexp_exp _ = false

      and conexp_atexp (IDENTatexp(info, OP_OPT(longid, ?), _)) =
            harmless_con longid
        | conexp_atexp (PARatexp(info, exp)) = conexp_exp exp
        | conexp_atexp _ = false
    in
      not (nexp_exp exp)
    end


  local
    fun fTy ty res =
      case ty of
        TYVARty(_, tv) => tv::res
      | RECORDty(_, NONE, _) => res
      | RECORDty(_, SOME tyrow, _) => fTyrow tyrow res
      | CONty(_, tys, _) => foldl (fn (ty,res) => fTy ty res) res tys
      | FNty(_, ty1, _, ty2) => fTy ty1 (fTy ty2 res)
      | PARty(_, ty, _) => fTy ty res
      | WITHty(_, ty, _) => fTy ty res

    and fTyrow (TYROW(_, _, ty, tyrowopt)) res =
      case tyrowopt of
        NONE => fTy ty res
      | SOME tyrow => fTyrow tyrow (fTy ty res)

    and fConbind (CONBIND(_, _, tyopt, conopt)) res =
      let
        val res' = case tyopt of NONE => res | SOME ty => fTy ty res
      in
        case conopt of
          NONE => res'
        | SOME conbind => fConbind conbind res'
      end

  in
    fun getExplicitTyVarsTy ty =  fTy ty []
    fun getExplicitTyVarsConbind ty = fConbind ty []
  end

  (* finding the string name of a topmost value identifier in a pattern, if any exists: *)

  fun find_topmost_id_in_pat (ATPATpat(_, atpat)): string option = find_topmost_id_in_atpat atpat
    | find_topmost_id_in_pat (LAYEREDpat(_,OP_OPT(id, _),_,_)) = SOME(Ident.pr_id id)
    | find_topmost_id_in_pat (TYPEDpat(_,pat,_)) = find_topmost_id_in_pat pat
    | find_topmost_id_in_pat _ = NONE

  and find_topmost_id_in_atpat (LONGIDatpat(_,OP_OPT(longid,_),_)) = SOME(Ident.pr_longid longid)
    | find_topmost_id_in_atpat (PARatpat(_,pat)) = find_topmost_id_in_pat pat
    | find_topmost_id_in_atpat _ = NONE


  (*is_'true'_'nil'_etc & is_'it' are used to enforce SOME syntactic
   restrictions (Definition, §2.9 & §3.5).*)

  fun is_'true'_'nil'_etc id =
    id = Ident.id_TRUE orelse
    id = Ident.id_FALSE orelse
    id = Ident.id_NIL orelse
    id = Ident.id_CONS orelse
    id = Ident.id_REF

  fun is_'it' id = id = Ident.id_IT


  local open PrettyPrint
  in
    type StringTree = StringTree
    type minipage = minipage

    fun list_from_opt (SOME x) = [x]
      | list_from_opt NONE = []

    (* layoutXXX: convert grammar of declarations into a StringTree. *)

    val INDENT = 3                      (* Standard indentation level. *)

    fun layoutAtexp atexp : StringTree =
      case atexp
        of SCONatexp(_, scon, NONE) => LEAF(SCon.pr_scon scon)
         | SCONatexp(_, scon, SOME(_,rv)) => LEAF(SCon.pr_scon scon ^ "`" ^ RegVar.pr rv)
         | IDENTatexp(_, OP_OPT(longid, withOp), NONE) =>
             LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

         | IDENTatexp(_, OP_OPT(longid, withOp), SOME(_,regvars)) =>
           let val first = LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)
           in NODE{start="",finish="",indent=1,
                   children=[first,
                             NODE{start="[", finish="]", indent=1,
                                  children=map (LEAF o RegVar.pr) regvars,
                                  childsep=LEFT ","}],
                   childsep=LEFT " "}
           end
         | RECORDatexp(_, exprow_opt, rv_opt) =>
             (case exprow_opt
                of SOME exprow =>
                   let val finish = case rv_opt of SOME (_,rv) => "}`" ^ RegVar.pr rv
                                                 | NONE => "}"
                   in NODE{start="{", finish=finish, indent=1,
                           children=[layoutExprow exprow],
                           childsep=NOSEP
                          }
                   end
                 | NONE =>
                     LEAF "{}"  (* Keep this atomic... *)
             )

         | LETatexp(_, dec, exp) =>
             let
               val decT = layoutDec dec
               val expT = layoutExp exp
             in
               NODE{start="let ", finish=" end", indent=4,
                       children=[decT, expT],
                       childsep=LEFT " in "
                      }
             end

         | PARatexp(_, exp) =>
             NODE{start="(", finish=")", indent=1,
                     children=[layoutExp exp],
                     childsep=NOSEP
                    }

    and layoutExprow row: StringTree =
      let
        fun treesOfExprow(EXPROW(_, lab, exp, exprow_opt)): StringTree list =
          let
            val this =
              NODE{start="", finish="", indent=0,
                      children=[LEAF(Lab.pr_Lab lab), layoutExp exp],
                      childsep=RIGHT " = "
                     }
          in
            this :: (case exprow_opt
                       of SOME row => treesOfExprow row
                        | NONE => nil
                    )
          end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfExprow row,
                childsep=RIGHT ", "
               }
      end

    and layoutExp exp : StringTree =
      case exp
        of ATEXPexp(_, atexp) =>
             layoutAtexp atexp

         | APPexp(_, exp, atexp) =>
             let
               val expT = layoutExp exp
               val atexpT = layoutAtexp atexp
             in
               NODE{start="", finish="", indent=0,
                       children=[expT, atexpT],
                       childsep=RIGHT " "
                      }
             end

         | TYPEDexp(_, exp, ty) =>
             let
               val expT = layoutExp exp
               val tyT = layoutTy ty
             in
               NODE{start="", finish="", indent=0,
                       children=[expT, tyT],
                       childsep=LEFT " : "
                      }
             end

         | HANDLEexp(_, exp, match) =>
             let
               val expT = layoutExp exp
               val matchT = layoutMatch match
             in
               NODE{start="", finish="", indent=0,
                       children=[expT, matchT],
                       childsep=LEFT " handle "
                      }
             end

         | RAISEexp(_, exp) =>
             NODE{start="raise ", finish="", indent=6,
                     children=[layoutExp exp],
                     childsep=NOSEP
                    }

         | FNexp(_, match) =>
             NODE{start="fn ", finish="", indent=3,
                     children=[layoutMatch match],
                     childsep=NOSEP
                    }

         | UNRES_INFIXexp(_, atexps) =>
             NODE{start="<UNRES_INFIX ", finish=">", indent=3,
                     children=map layoutAtexp atexps, childsep=RIGHT " "
                    }

    and layoutMatch match : StringTree =
      let
        fun treesOfMatch(MATCH(_, mrule, match_opt)) : StringTree list =
          layoutMrule mrule
          :: (case match_opt
                of SOME match => treesOfMatch match
                 | NONE => nil
             )
      in
        NODE{start="", finish="", indent=0,
                children=treesOfMatch match,
                childsep=LEFT " | "
               }
      end

    and layoutMrule (MRULE(_, pat, exp)) : StringTree =
      let
        val patT = layoutPat pat
        val expT = layoutExp exp
      in
        NODE{start="", finish="", indent=0,
                children=[patT, expT],
                childsep=RIGHT " => "
               }
      end

    and layoutDec dec : StringTree =
      (case dec
        of VALdec(_, tyvars, valbind) =>
             NODE{start="val ", finish="", indent=4,
                     children = list_from_opt (layoutTyvarseq tyvars)
                                @ [layoutValbind valbind],
                     childsep=NOSEP
                     }


         | UNRES_FUNdec _ =>
             LEAF "<UNRES_FUN>"

         | TYPEdec(_, typbind) =>
             NODE{start="type ", finish="", indent=5,
                     children=[layoutTypbind typbind],
                     childsep=NOSEP
                    }

         | DATATYPEdec(_, datbind) =>
             NODE{start="datatype ", finish="", indent=INDENT,
                     children=[layoutDatbind datbind],
                     childsep=NOSEP
                    }

         | DATATYPE_REPLICATIONdec(i, tycon, longtycon) =>
             layout_datatype_replication(i, tycon, longtycon)

         | ABSTYPEdec(_, datbind, dec) =>
             let
               val datbindT = layoutDatbind datbind
               val decT = layoutDec dec
             in
               NODE{start="abstype ", finish=" end", indent=INDENT,
                       children=[datbindT, decT],
                       childsep=LEFT " with "
                      }
             end

         | EXCEPTIONdec(_, exbind) =>
             NODE{start="exception ", finish="", indent=INDENT,
                     children=[layoutExbind exbind],
                     childsep=NOSEP
                    }

         | LOCALdec(_, dec1, dec2) =>
             let
               val dec1T = layoutDec dec1
               val dec2T = layoutDec dec2
             in
               NODE{start="local ", finish=" end", indent=INDENT,
                       children=[dec1T, dec2T],
                       childsep=LEFT " in "
                      }
             end

         | OPENdec (_, list) =>
             NODE {start="open ", finish="", indent=5,
                   children = map (LEAF o StrId.pr_LongStrId o strip_info) list,
                   childsep = RIGHT " "}

         | SEQdec(_, dec1, dec2) =>
             let
               val dec1T = layoutDec dec1
               val dec2T = layoutDec dec2
             in
               NODE{start="(", finish=")", indent=0,
                       children=[dec1T, dec2T],
                       childsep=RIGHT "; "
                      }
             end

         | INFIXdec(_, prec, ids) =>
             NODE{start="infix ", finish="", indent=6,
                     children=(case prec
                                 of SOME p => [LEAF(Int.toString p)]
                                  | NONE => nil
                              ) @ map (LEAF o Ident.pr_id) ids,
                     childsep=RIGHT " "
                    }

         | INFIXRdec(_, prec, ids) =>
             NODE{start="infixr ", finish="", indent=7,
                     children=(case prec
                                 of SOME p => [LEAF(Int.toString p)]
                                  | NONE => nil
                              ) @ map (LEAF o Ident.pr_id) ids,
                     childsep=RIGHT " "
                    }

         | NONFIXdec(_, ids) =>
             NODE{start="nonfix ", finish="", indent=7,
                     children=map (LEAF o Ident.pr_id) ids,
                     childsep=RIGHT " "
                    }

         | EMPTYdec _ =>
             LEAF "(emptydec)"

         | REGIONdec(_, (_,rvs)) =>
             NODE{start="region ", finish="", indent=7,
                     children=map (LEAF o RegVar.pr) rvs,
                     childsep=RIGHT " "
                    })

    and layout_datatype_replication (i, tycon, longtycon) =
          LEAF ("datatype " ^ TyCon.pr_TyCon tycon ^ " = "
                ^ "datatype " ^ TyCon.pr_LongTyCon longtycon)

    and layoutValbind valbind : StringTree =
      let
        fun treesOfValbind valbind : StringTree list =
          case valbind
            of PLAINvalbind(_, pat, exp, valbind_opt) =>
              let
                val patT = layoutPat pat
                val expT = layoutExp exp
                val this =
                  NODE{start="", finish="", indent=0,
                          children=[patT, expT],
                          childsep=RIGHT " = "
                          }
              in
                this :: (case valbind_opt
                           of SOME valbind => treesOfValbind valbind
                            | NONE => nil
                        )
              end

          | RECvalbind(_, valbind) =>
              [NODE{start="rec ", finish="", indent=4,
                       children=treesOfValbind valbind,
                       childsep=LEFT " and "
                      }
              ]
      in
        NODE{start="", finish="", indent=0,
                children=treesOfValbind valbind,
                childsep=LEFT " and "
               }
      end

    and layoutTyvarseq tyvars =
      case tyvars
        of nil => NONE
         | [tv] => SOME(LEAF(TyVar.pr_tyvar tv))
         | tvs => SOME(NODE{start="(", finish=")", indent=1,
                               children=map (LEAF o TyVar.pr_tyvar) tvs,
                               childsep=RIGHT ", "
                              }
                      )

    and layoutTypbind typbind : StringTree =
      let
        fun treesOfTypbind(TYPBIND(_, tyvars, tycon, ty, typbind_opt))
          : StringTree list =
          let
            val tyvars_opt = layoutTyvarseq tyvars
            val tyconT = LEAF(TyCon.pr_TyCon tycon)
            val eqT = LEAF " = "
            val tyT = layoutTy ty

            val this =
              NODE{start="", finish="", indent=0,
                      children=(case tyvars_opt of SOME x => [x]
                                                 | NONE => nil
                               ) @  [tyconT, eqT, tyT],
                      childsep=NOSEP
                     }
          in
            this :: (case typbind_opt
                       of SOME typbind => treesOfTypbind typbind
                        | NONE => nil
                    )
          end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfTypbind typbind,
                childsep=LEFT " and "
               }
      end

    and layoutDatbind datbind : StringTree =
      let
        fun treesOfDatbind(DATBIND(_, tyvars, tycon, conbind, datbind_opt))
          : StringTree list =
          let
            val tyvarsT_opt = layoutTyvarseq tyvars
            val tyconT = LEAF(TyCon.pr_TyCon tycon)
            val tyBindingT =
              case tyvarsT_opt
                of NONE => tyconT
                 | SOME x => NODE{start="", finish="", indent=0,
                                     children=[x, tyconT],
                                     childsep=RIGHT " "
                                    }
            val eqT = LEAF " = "
            val conbindT = layoutConbind conbind

            val this =
              NODE{start="", finish="", indent=0,
                      children=[tyBindingT, eqT, conbindT],
                      childsep=NOSEP
                     }
          in
            this :: (case datbind_opt
                       of SOME datbind => treesOfDatbind datbind
                        | NONE => nil
                    )
          end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfDatbind datbind,
                childsep=LEFT " and "
               }
      end

    and layoutConbind conbind : StringTree =
      let
        fun treesOfConbind(CONBIND(_, OP_OPT(id, withOp), ty_opt, conbind_opt))
          : StringTree list =
          let
            val conT =
              LEAF((if withOp then "op " else "") ^ Ident.pr_id id)

            val this =
              case ty_opt
                of SOME ty => NODE{start="", finish="", indent=0,
                                      children=[conT, layoutTy ty],
                                      childsep=LEFT " of "
                                     }
              | NONE => conT
          in
            this :: (case conbind_opt
                       of SOME conbind => treesOfConbind conbind
                        | NONE => nil
                    )
          end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfConbind conbind,
                childsep=LEFT " | "
               }
      end

    and layoutExbind exbind : StringTree =
      let
        fun layoutIdSubTRest(id, withOp, sep, subT, rest) : StringTree list =
          let
            val this =
              NODE{start="", finish="", indent=0,
                      children=[LEAF((if withOp then "op " else "")
                                        ^ Ident.pr_id id
                                       ),
                                subT
                               ],
                      childsep=LEFT sep
                     }
          in
            this :: (case rest
                       of SOME exbind => treesOfExbind exbind
                        | NONE => nil
                    )
          end

        and treesOfExbind exbind : StringTree list =
          case exbind
            of EXBIND(_, OP_OPT(id, withOp), SOME ty, exbind_opt) =>
              layoutIdSubTRest(id, withOp, " of ", layoutTy ty, exbind_opt)

          | EXBIND(_, OP_OPT(id, withOp), NONE, exbind_opt) =>
              LEAF((if withOp then "op " else "") ^ Ident.pr_id id)
              :: (case exbind_opt
                    of SOME exbind => treesOfExbind exbind
                     | NONE => nil
                 )

          | EXEQUAL(_, OP_OPT(id, exconOp),
                       OP_OPT(longid, longidOp),
                       exbind_opt
                   ) =>
              layoutIdSubTRest(id, exconOp, " = ",
                               LEAF((if longidOp then "op " else "")
                                       ^ Ident.pr_longid longid
                                      ),
                               exbind_opt
                              )
      in
        NODE{start="", finish="", indent=0,
                children=treesOfExbind exbind,
                childsep=LEFT " and "
               }
      end

    and layoutAtpat atpat : StringTree =
      case atpat
        of WILDCARDatpat _ => LEAF "_"

         | SCONatpat(_, scon) => LEAF(SCon.pr_scon scon)

         | LONGIDatpat(_, OP_OPT(longid, withOp), NONE) =>
             LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)

         | LONGIDatpat(_, OP_OPT(longid, withOp), SOME(_,regvars)) =>
           let val first = LEAF((if withOp then "op " else "") ^ Ident.pr_longid longid)
           in NODE{start="",finish="",indent=1,
                   children=[first,
                             NODE{start="[", finish="]", indent=1,
                                  children=map (LEAF o RegVar.pr) regvars,
                                  childsep=LEFT ","}],
                   childsep=LEFT " "}
           end

         | RECORDatpat(_, patrow_opt) =>
             (case patrow_opt
                of SOME patrow =>
                     NODE{start="{", finish="}", indent=1,
                             children=[layoutPatrow patrow],
                             childsep=NOSEP
                             }

                 | NONE =>
                     LEAF "{}"
             )

         | PARatpat(_, pat) =>
             NODE{start="(", finish=")", indent=1,
                     children=[layoutPat pat],
                     childsep=NOSEP
                     }

    and layoutPatrow row : StringTree =
      let
        fun treesOfPatrow row : StringTree list =
          case row
            of DOTDOTDOT _ =>
                 [LEAF "..."]

             | PATROW(_, lab, pat, patrow_opt) =>
                 let
                   val this =
                     NODE{start="", finish="", indent=0,
                             children=[LEAF(Lab.pr_Lab lab), layoutPat pat],
                             childsep=RIGHT " = "
                             }
                 in
                   this :: (case patrow_opt
                              of SOME row => treesOfPatrow row
                               | NONE => nil
                            )
                 end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfPatrow row,
                childsep=RIGHT ", "
                }
      end

    and layoutPat pat : StringTree =
      case pat

        of ATPATpat(_, atpat) =>
             layoutAtpat atpat

         | CONSpat(_, OP_OPT(longid, withOp), atpat) =>
             NODE{start=(if withOp then "op " else "")
                           ^ Ident.pr_longid longid ^ " ",
                     finish="",
                     indent=INDENT,
                     children=[layoutAtpat atpat],
                     childsep=NOSEP
                    }

         | TYPEDpat(_, pat, ty) =>
             let
               val patT = layoutPat pat
               val tyT = layoutTy ty
             in
               NODE{start="", finish="", indent=0,
                       children=[patT, tyT],
                       childsep=LEFT " : "
                       }
             end

         | LAYEREDpat(_, OP_OPT(id, withOp), ty_opt, pat) =>
             let
               val idT = LEAF((if withOp then "op " else "") ^ Ident.pr_id id)

               val identColonTyT =
                 case ty_opt
                   of SOME ty =>
                        NODE{start="", finish="", indent=0,
                                children=[idT, layoutTy ty],
                                childsep=LEFT " : "
                                }
                    | NONE =>
                        idT

               val patT = layoutPat pat
             in
               NODE{start="", finish="", indent=0,
                       children=[identColonTyT, patT],
                       childsep=LEFT " as "
                       }
             end

         | UNRES_INFIXpat(_, atpats) =>
             NODE{start="<UNRES_INFIX ", finish=">", indent=3,
                     children=map layoutAtpat atpats, childsep=RIGHT " "
                    }

    and layoutTy ty : StringTree =
      case ty
        of TYVARty(_, tyvar) =>
             LEAF(TyVar.pr_tyvar tyvar)

         | RECORDty(_, tyrow_opt, regvar_opt) =>
           let val finish =
                   case regvar_opt of
                       SOME (_,rv) => "}`" ^ RegVar.pr rv
                     | NONE => "}"
           in case tyrow_opt
               of SOME tyrow =>
                  NODE{start="{", finish=finish, indent=1,
                       children=[layoutTyrow tyrow],
                       childsep=NOSEP
                      }
                 | NONE => LEAF "{}" (* "unit" ? *)
           end

         | CONty(_, tys, longtycon) =>
             let
               fun idTail t =
                 NODE{start="", finish=" " ^ TyCon.pr_LongTyCon longtycon,
                         indent=0, children=[t], childsep=NOSEP
                         }
             in
               case tys
                 of nil => LEAF(TyCon.pr_LongTyCon longtycon)

                  | [ty] => idTail(layoutTy ty)

                  | tys => idTail(NODE{start="(", finish=")", indent=1,
                                          children=map layoutTy tys,
                                          childsep=RIGHT ", "
                                         }
                                 )
             end

         | FNty(_, ty1, opt, ty2) =>
           let val arrow =
                   case opt of
                       SOME (i,r) => " -" ^ RegVar.pr r ^ "-> "
                     | NONE => " -> "
           in NODE{start="", finish="", indent=0,
                   children=[layoutTy ty1, layoutTy ty2],
                   childsep=LEFT arrow
                  }
           end
         | PARty(_, ty, regvar_opt) =>
           let val finish = case regvar_opt of
                                SOME (_,[(_,rv)]) => ")`" ^ RegVar.pr rv
                              | SOME (_,rvis) => ")`[" ^ String.concatWith "," (map (fn (_,rv) => RegVar.pr rv) rvis) ^ "]"
                              | NONE => ")"
           in NODE{start="(", finish=finish, indent=1,
                   children=[layoutTy ty],
                   childsep=NOSEP}
           end
         | WITHty(_, ty, c) =>
           NODE{start="(", finish=")", indent=1,
                children=[layoutTy ty, layoutConstraint c],
                childsep=LEFT " where "}

    and layoutConstraint c : StringTree =
        case c of
            DISJOINTconstraint (_,e1,e2,p) =>
            NODE{start="", finish="", indent=0,
                 children=[layoutEff e1, layoutEff e2],
                 childsep=LEFT (if p then " ## " else " # ")}
          | INCLconstraint (_, (_,r),e) =>
            NODE{start="", finish="", indent=0,
                 children=[LEAF (RegVar.pr r), layoutEff e],
                 childsep=LEFT " <= "}
          | PROPconstraint (_, p,e) =>
            NODE{start="", finish="", indent=0,
                 children=[LEAF (pr_prop p), layoutEff e],
                 childsep=LEFT " "}

    and pr_prop p : string =
        case p of
            NOMUTprop _ => "nomut"
          | NOPUTprop _ => "noput"
          | NOEXNprop _ => "noexn"

    and layoutEff e : StringTree =
        case e of
            SETeff (_,aes) =>
            NODE{start="{", finish="}", indent=1, children=map layoutAteff aes,
                 childsep=LEFT ","}
          | VAReff (_,r) => LEAF (RegVar.pr r)

    and layoutAteff ae : StringTree =
        case ae of
            VARateff (_,r) => LEAF (RegVar.pr r)
          | PUTateff (_,(_,r)) => LEAF ("put " ^ RegVar.pr r)
          | GETateff (_,(_,r)) => LEAF ("get " ^ RegVar.pr r)

    and layoutTyrow row : StringTree =
      let
        fun treesOfTyrow (TYROW(_, lab, ty, tyrow_opt)) =
          let
            val this =
              NODE{start="", finish="", indent=0,
                      children=[LEAF(Lab.pr_Lab lab), layoutTy ty],
                      childsep=LEFT " : "
                     }
          in
            this :: (case tyrow_opt
                       of SOME row => treesOfTyrow row
                        | NONE => nil
                    )
          end
      in
        NODE{start="", finish="", indent=0,
                children=treesOfTyrow row,
                childsep=RIGHT ", "
               }
      end
    end (*local*)
end
