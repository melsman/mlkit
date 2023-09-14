(*************************************************************)
(* Grammar for Bare language - Definition v3 pages 8,9,70,71 *)
(* modified to have ident in place of con, var and excon     *)
(* Added support for 'region' decs.                          *)
(*************************************************************)

signature DEC_GRAMMAR =
sig
  type lab = Lab.lab
  type scon = SCon.scon
  type id = Ident.id
  type longid = Ident.longid
  type tyvar = SyntaxTyVar.SyntaxTyVar (*very confusing*)
  type tycon = TyCon.tycon
  type longtycon = TyCon.longtycon
  type longstrid = StrId.longstrid
  type regvar = RegVar.regvar

  type info       (* info about the position in the source text, errors etc *)
  val bogus_info : info

  datatype 'a op_opt = OP_OPT of 'a * bool
  datatype 'a WithInfo = WITH_INFO of info * 'a
  val strip_info : 'a WithInfo -> 'a

  datatype atexp =
	SCONatexp of info * scon * (info*regvar) option |
	IDENTatexp of info * longid op_opt * (info*regvar list) option |
	RECORDatexp of info * exprow option * (info*regvar) option |
	LETatexp of info * dec * exp |
	PARatexp of info * exp

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
		(* TEMPORARY: removed when resolving infixes after parsing. *)
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
        REGIONdec of info * (info*regvar list)                      (* ReML *)

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
        WITHty of info * ty * constraint                               (* ReML *)

  and constraint =                                                     (* ReML *)
        DISJOINTconstraint of info * eff * eff * bool |                (* true if puts only *)
        INCLconstraint of info * (info*regvar) * eff |
        PROPconstraint of info * prop * eff

  and prop =
        NOMUTprop of info |
        NOPUTprop of info |
        NOEXNprop of info

  and eff =                                                            (* ReML *)
        SETeff of info * ateff list |
        VAReff of info * regvar

  and ateff =                                                          (* ReML *)
        VARateff of info * regvar |
        PUTateff of info * (info*regvar) |
        GETateff of info * (info*regvar)

  and tyrow =
        TYROW of info * lab * ty * tyrow option

  val get_info_atexp : atexp -> info
  val get_info_exprow : exprow -> info
  val get_info_exp : exp -> info
  val get_info_match : match -> info
  val get_info_mrule : mrule -> info
  val get_info_dec : dec -> info
  val get_info_valbind : valbind -> info
  val get_info_datbind : datbind -> info
  val get_info_conbind : conbind -> info
  val get_info_pat : pat -> info
  val get_info_atpat : atpat -> info
  val get_info_patrow : patrow -> info
  val get_info_ty : ty -> info
  val get_info_constraint : constraint -> info    (* ReML *)
  val get_info_eff : eff -> info                  (* ReML *)
  val get_info_ateff : ateff -> info              (* ReML *)
  val get_info_typbind : typbind -> info
  val get_info_tyrow : tyrow -> info
  val get_info_exbind : exbind -> info
  val get_info_FValBind : FValBind -> info
  val get_info_FClause : FClause -> info

  val map_atexp_info : (info -> info) -> atexp -> atexp
  val map_exprow_info : (info -> info) -> exprow -> exprow
  val map_exp_info : (info -> info) -> exp -> exp
  val map_match_info : (info -> info) -> match -> match
  val map_mrule_info : (info -> info) -> mrule -> mrule
  val map_dec_info : (info -> info) -> dec -> dec
  val map_valbind_info : (info -> info) -> valbind -> valbind
  val map_datbind_info : (info -> info) -> datbind -> datbind
  val map_conbind_info : (info -> info) -> conbind -> conbind
  val map_pat_info : (info -> info) -> pat -> pat
  val map_atpat_info : (info -> info) -> atpat -> atpat
  val map_patrow_info : (info -> info) -> patrow -> patrow
  val map_ty_info : (info -> info) -> ty -> ty
  val map_constraint_info : (info -> info) -> constraint -> constraint    (* ReML *)
  val map_eff_info : (info -> info) -> eff -> eff                         (* ReML *)
  val map_ateff_info : (info -> info) -> ateff -> ateff                   (* ReML *)


  val getExplicitTyVarsTy      : ty -> tyvar list
  and getExplicitTyVarsConbind : conbind -> tyvar list

  (*expansive harmless_con exp = true iff exp is expansive.
   harmless_con longid = true iff longid is an excon or a con different
   from id_REF.  To know this, the context is necessary; that is the
   reason you must provide harmless_con.*)

  val expansive : (longid -> bool) -> exp -> bool

  val find_topmost_id_in_pat : pat -> string option
  val find_topmost_id_in_atpat: atpat -> string option

  (*is_'true'_'nil'_etc & is_'it' are used to enforce some syntactic
   restrictions (Definition, §2.9 & §3.5).*)

  val is_'true'_'nil'_etc : id -> bool
  val is_'it' : id -> bool

  type StringTree

  val layoutTyvarseq : tyvar list -> StringTree option
  val layoutTy :       ty	  -> StringTree
  val layoutAtpat :    atpat	  -> StringTree
  val layoutPat :      pat	  -> StringTree
  val layoutExp :      exp	  -> StringTree
  val layoutDec :      dec	  -> StringTree
  val layout_datatype_replication : info * tycon * longtycon -> StringTree
end;
