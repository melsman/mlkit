(*$FreeIds : DEC_GRAMMAR TOPDEC_GRAMMAR CRASH PRETTYPRINT
    FREE_IDS ELAB_INFO OrderSet*)

functor FreeIds (structure TopdecGrammar : TOPDEC_GRAMMAR     (* Post elab *)
		 structure ElabInfo : ELAB_INFO
		   sharing type ElabInfo.ElabInfo = TopdecGrammar.info
		       and type ElabInfo.TypeInfo.strid = TopdecGrammar.strid
		       and type ElabInfo.TypeInfo.tycon = TopdecGrammar.tycon
		       and type ElabInfo.TypeInfo.id = TopdecGrammar.id
		 structure Crash : CRASH
		 structure PP : PRETTYPRINT
		  ) :  FREE_IDS =
  struct
    fun die s = Crash.impossible ("FreeIds."^s)

    open TopdecGrammar
    open TopdecGrammar.DecGrammar

    type id = Ident.id

    structure IdSet = OrderSet(structure Order = 
				 struct type T = Ident.id
				        fun lt x y = Ident.< (x,y)
				 end
                               structure PP = PP)

    structure TyConSet = OrderSet(structure Order = 
				    struct type T = TyCon.tycon
				           fun lt x y = TyCon.< (x,y)
				    end
                                  structure PP = PP)
 
    structure StrIdSet = OrderSet(structure Order = 
				    struct type T = StrId.strid
				           fun lt x y = StrId.< (x,y)
				    end
                                  structure PP = PP)

    structure FunIdSet = OrderSet(structure Order = 
			   	    struct type T = FunId.funid
				           fun lt x y = FunId.< (x,y)
				    end
                                  structure PP = PP)

    structure SigIdSet = OrderSet(structure Order = 
			 	    struct type T = SigId.sigid
				           fun lt x y = SigId.< (x,y)
				    end
                                  structure PP = PP)


    (* The way this works is by passing sets of those identifiers
     * being declared downwards in the syntax tree, and before an
     * identifier is added to a bucket it is checked if it is in a
     * `declared set'. -- Martin *)


    type ids = {vids: IdSet.Set,
		tycons: TyConSet.Set,
		strids: StrIdSet.Set,
		funids: FunIdSet.Set,
		sigids: SigIdSet.Set}

    val empty_ids = {vids=IdSet.empty, tycons=TyConSet.empty,
		     strids=StrIdSet.empty, funids=FunIdSet.empty, sigids=SigIdSet.empty}

    infix ++
    fun {vids,tycons,strids,funids,sigids} ++
        {vids=vids',tycons=tycons',strids=strids',funids=funids',sigids=sigids'} =
	{vids=IdSet.union vids vids',
	 tycons=TyConSet.union tycons tycons',
	 strids=StrIdSet.union strids strids',
	 funids=FunIdSet.union funids funids',
	 sigids=SigIdSet.union sigids sigids'}

    val vids_of_ids:     ids -> id list     = IdSet.list o #vids
    val tycons_of_ids:   ids -> tycon list  = TyConSet.list o #tycons
    val strids_of_ids:   ids -> strid list  = StrIdSet.list o #strids
    val funids_of_ids:   ids -> funid list  = FunIdSet.list o #funids
    val sigids_of_ids:   ids -> sigid list  = SigIdSet.list o #sigids

    (* -------------------------------------
     * Sets of ids
     * ------------------------------------- *)
    fun add_vid (id:id,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=IdSet.insert id vids,tycons=tycons,strids=strids,funids=funids,sigids=sigids}

    fun add_tycon (tycon:tycon,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=TyConSet.insert tycon tycons,strids=strids,funids=funids,sigids=sigids}

    fun add_strid (strid:strid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=StrIdSet.insert strid strids,funids=funids,sigids=sigids}

    fun add_funid (funid:funid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=strids,funids=FunIdSet.insert funid funids,sigids=sigids}

    fun add_sigid (sigid:sigid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=strids,funids=funids,sigids=SigIdSet.insert sigid sigids}

    (* -------------------------------------
     * Bucket for free ids
     * ------------------------------------- *)
    val free = ref empty_ids
    fun mk_free_vid (id:id) = free := add_vid(id,!free)
    fun mk_free_tycon (tycon:tycon) = free := add_tycon(tycon,!free)
    fun mk_free_strid (strid:strid) = free := add_strid(strid,!free)
    fun mk_free_funid (funid:funid) = free := add_funid(funid,!free)
    fun mk_free_sigid (sigid:sigid) = free := add_sigid(sigid,!free)

    (* -------------------------------------
     * Functions to apply on uses of ids
     * ------------------------------------- *)
    fun use_id({vids,...}:ids,id:id): unit =
      if IdSet.member id vids then () else mk_free_vid id
    fun use_strid({strids,...}:ids,strid:strid): unit =
      if StrIdSet.member strid strids then () else mk_free_strid strid

    fun use_longid(bound_ids:ids,longid:longid): unit =
      case Ident.decompose longid
	of ([],id) => use_id(bound_ids,id)
	 | (strid::_,_) => use_strid(bound_ids,strid) 

    fun use_tycon({tycons,...}:ids,tycon:tycon): unit =
      if TyConSet.member tycon tycons then () else mk_free_tycon tycon
    fun use_longtycon(bound_ids:ids,longtycon:longtycon) : unit =
      case TyCon.explode_LongTyCon longtycon
	of ([],tycon) => use_tycon(bound_ids,tycon)
	 | (strid::_,_) => use_strid(bound_ids,strid) 
    fun use_longstrid(bound_ids:ids,longstrid:longstrid) : unit =
      case StrId.explode_longstrid longstrid
	of ([],strid) => use_strid(bound_ids,strid) 
	 | (strid::_,_) => use_strid(bound_ids,strid) 
    fun use_funid({funids,...}:ids,funid:funid): unit =
      if FunIdSet.member funid funids then () else mk_free_funid funid
    fun use_sigid({sigids,...}:ids,sigid:sigid): unit =
      if SigIdSet.member sigid sigids then () else mk_free_sigid sigid

    (* --------------------------------------
     * We carry around a persistent set of
     * bound identifiers, I. 
     * -------------------------------------- *)

    (* 
     * CORE
     *)

    and free_atexp I =
      fn SCONatexp _ => ()
       | IDENTatexp(_, OP_OPT(longid,_)) => use_longid(I,longid)
       | RECORDatexp(_,exprow_opt) => free_exprow_opt I exprow_opt
       | LETatexp(_,dec,exp) => let val I' = free_dec I dec
				in free_exp (I ++ I') exp
				end
       | PARatexp(_,exp) => free_exp I exp

    and free_exprow I (EXPROW(_,_,exp,exprow_opt)) : unit = (free_exp I exp; free_exprow_opt I exprow_opt)
    and free_exprow_opt I None = ()
      | free_exprow_opt I (Some exprow) = free_exprow I exprow

    and free_exp I =
      fn ATEXPexp(_,atexp) => free_atexp I atexp
       | APPexp(_,exp,atexp) => (free_exp I exp; free_atexp I atexp)
       | TYPEDexp(_,exp,ty) => (free_exp I exp; free_ty I ty)
       | HANDLEexp(_,exp,match) => (free_exp I exp; free_match I match)
       | RAISEexp(_,exp) => free_exp I exp
       | FNexp(_,match) => free_match I match
       | UNRES_INFIXexp _ => die "free.UNRES_INFIXexp"
      
    and free_match I (MATCH(_,mrule,match_opt)) : unit =
      (free_mrule I mrule; free_match_opt I match_opt)
    and free_match_opt I None = ()
      | free_match_opt I (Some match) = free_match I match

    and free_mrule I (MRULE(_,pat,exp)) : unit =
      let val I' = free_pat I pat
      in free_exp (I ++ I') exp
      end

    and free_dec I =
      fn VALdec(_,_,valbind) => free_valbind I valbind
       | UNRES_FUNdec _ => die "free.UNRES_FUNdec"
       | TYPEdec(_,typbind) => free_typbind I typbind
       | DATATYPEdec(_,datbind) => free_datbind I datbind
       | DATATYPE_REPLICATIONdec(_,tycon,longtycon) => (use_longtycon(I,longtycon); add_tycon(tycon,empty_ids))
       | ABSTYPEdec(_,datbind,dec) => 
         let fun Abs ({tycons,...}:ids) =
	        {tycons=tycons, vids=IdSet.empty, strids=StrIdSet.empty,
		 funids=FunIdSet.empty, sigids=SigIdSet.empty}
	     val I1 = free_datbind I datbind
	     val I2 = free_dec (I ++ I1) dec
	 in Abs(I1) ++ I2                          (* Only tycons of I1 survives. -- Martin *)
	 end
       | EXCEPTIONdec(_,exbind) => free_exbind I exbind
       | LOCALdec(_,dec1,dec2) => let val I1 = free_dec I dec1
				  in free_dec (I ++ I1) dec2
				  end
       | OPENdec(info,longstrids_with_info) =>
	  let fun use_longstrids_with_info(I,longstrids_with_info) =
	         List.apply (fn WITH_INFO(_,longstrid) => use_longstrid(I,longstrid)) 
		 longstrids_with_info
	      val (strids, tycons, ids) = case ElabInfo.to_TypeInfo info
					    of Some (ElabInfo.TypeInfo.OPEN_INFO decls) => decls
					     | _ => die "OPENdec - no decl. info"
	      val decl_strids = List.foldL (fn strid => fn ids => add_strid(strid,ids))
	      val decl_tycons = List.foldL (fn tycon => fn ids => add_tycon(tycon,ids))
	      val decl_ids = List.foldL (fn id => fn ids => add_vid(id,ids))
	  in use_longstrids_with_info(I,longstrids_with_info);
	     decl_strids (decl_tycons (decl_ids empty_ids ids) tycons) strids
	  end
       | SEQdec(_,dec1,dec2) =>  let val I1 = free_dec I dec1
				     val I2 = free_dec (I ++ I1) dec2
				 in I1 ++ I2
				 end
       | INFIXdec _ => empty_ids
       | INFIXRdec _ => empty_ids
       | NONFIXdec _ => empty_ids
       | EMPTYdec _ => empty_ids

    and free_valbind I =
      fn PLAINvalbind(_,pat,exp,valbind_opt) =>
      (free_exp I exp; free_pat I pat ++ free_valbind_opt I valbind_opt)
       | RECvalbind(_,valbind) =>
      let fun f (PLAINvalbind(_,pat,_,None)) = free_pat I pat
	    | f (PLAINvalbind(_,pat,_,Some valbind)) = free_pat I pat ++ f valbind
	    | f (RECvalbind(_,valbind)) = f valbind
	  val I' = f valbind
      in free_valbind' (I ++ I') valbind; I'
      end
    and free_valbind_opt I None = empty_ids
      | free_valbind_opt I (Some valbind) = free_valbind I valbind
 
    and free_valbind' I =
      fn PLAINvalbind(_,_,exp,valbind_opt) => (free_exp I exp; free_valbind_opt' I valbind_opt)
       | RECvalbind(_,valbind) => free_valbind' I valbind
    and free_valbind_opt' I None = ()
      | free_valbind_opt' I (Some valbind) = free_valbind' I valbind

    and free_typbind I (TYPBIND(_,_,tycon,ty,typbind_opt)) : ids =
      (free_ty I ty; add_tycon(tycon,empty_ids) ++ free_typbind_opt I typbind_opt)
    and free_typbind_opt I None = empty_ids
      | free_typbind_opt I (Some typbind) = free_typbind I typbind

    and free_datbind I datbind : ids =
      let fun f (DATBIND(_,_,tycon,_,None)) = add_tycon(tycon,empty_ids)
	    | f (DATBIND(_,_,tycon,_,Some datbind)) = add_tycon(tycon,f datbind)
	  fun g I (DATBIND(_,_,_,conbind,None)) = free_conbind I conbind 
	    | g I (DATBIND(_,_,_,conbind,Some datbind)) = free_conbind I conbind ++ g I datbind
	  val I1 = f datbind
	  val I2 = g (I ++ I1) datbind
      in I1 ++ I2
      end

    and free_conbind I (CONBIND(_,OP_OPT(con,_),ty_opt,conbind_opt)) : ids =
      (free_ty_opt I ty_opt; add_vid(con,empty_ids) ++ free_conbind_opt I conbind_opt)
    and free_conbind_opt I None = empty_ids
      | free_conbind_opt I (Some conbind) = free_conbind I conbind

    and free_exbind I =
      fn EXBIND(_,OP_OPT(excon,_),ty_opt,exbind_opt) =>
      (free_ty_opt I ty_opt; add_vid(excon,empty_ids) ++ free_exbind_opt I exbind_opt)
       | EXEQUAL(_,OP_OPT(excon,_),OP_OPT(longid,_),exbind_opt) =>
      (use_longid(I,longid); add_vid(excon,empty_ids) ++ free_exbind_opt I exbind_opt)
    and free_exbind_opt I None = empty_ids
      | free_exbind_opt I (Some exbind) = free_exbind I exbind

    and free_atpat I =
      fn WILDCARDatpat _ => empty_ids
       | SCONatpat _ => empty_ids
       | LONGIDatpat(info,OP_OPT(longid,_)) => 
          (case ElabInfo.to_TypeInfo info
	     of Some (ElabInfo.TypeInfo.VAR_PAT_INFO _) =>
	       (case Ident.decompose longid
		  of ([],id) => add_vid(id, empty_ids)
		   | _ => die "free_atpat.longid in pattern.") 
              | Some (ElabInfo.TypeInfo.CON_INFO _) => (use_longid(I,longid); empty_ids)
	      | Some (ElabInfo.TypeInfo.EXCON_INFO _) => (use_longid(I,longid); empty_ids)
	      | _ => die "free_atpat.no type info")  
       | RECORDatpat(_,patrow_opt) => free_patrow_opt I patrow_opt
       | PARatpat(_,pat) => free_pat I pat

    and free_patrow I =
      fn DOTDOTDOT _ => empty_ids
       | PATROW(_,lab,pat,patrow_opt) => (free_pat I pat ++ free_patrow_opt I patrow_opt)
    and free_patrow_opt I None = empty_ids
      | free_patrow_opt I (Some patrow) = free_patrow I patrow

    and free_pat I =
      fn ATPATpat(_,atpat) => free_atpat I atpat
       | CONSpat(_,OP_OPT(longid,_),atpat) => (use_longid(I,longid); free_atpat I atpat)
       | TYPEDpat(_,pat,ty) => (free_ty I ty; free_pat I pat)
       | LAYEREDpat(_,OP_OPT(id,_),ty_opt,pat) => (free_ty_opt I ty_opt; add_vid(id,empty_ids) ++ free_pat I pat)
       | UNRES_INFIXpat _ => die "free.UNRES_INFIXpat"

    and free_ty I =
      fn TYVARty _ => ()
       | RECORDty(_,tyrow_opt) => free_tyrow_opt I tyrow_opt
       | CONty(_,tys,longtycon) => (List.apply (free_ty I) tys; use_longtycon(I,longtycon))
       | FNty(_,ty1,ty2) => (free_ty I ty1; free_ty I ty2)
       | PARty(_,ty) => free_ty I ty
    and free_ty_opt I None = ()
      | free_ty_opt I (Some ty) = free_ty I ty

    and free_tyrow I (TYROW(_,lab,ty,tyrow_opt)) : unit = (free_ty I ty; free_tyrow_opt I tyrow_opt)
    and free_tyrow_opt I None = ()
      | free_tyrow_opt I (Some tyrow) = free_tyrow I tyrow

 
    (*
     * MODULES
     *)

    fun free_strexp I =
      fn STRUCTstrexp(_,strdec) => (free_strdec I strdec; ())
       | LONGSTRIDstrexp(_,longstrid) => use_longstrid(I,longstrid)
       | TRANSPARENT_CONSTRAINTstrexp(_,strexp,sigexp) => 
          (free_strexp I strexp; free_sigexp I sigexp)
       | OPAQUE_CONSTRAINTstrexp(_,strexp,sigexp) => 
          (free_strexp I strexp; free_sigexp I sigexp)
       | APPstrexp(_,funid,strexp) => (use_funid(I,funid); free_strexp I strexp)
       | LETstrexp(_,strdec,strexp) =>
	  let val I' = free_strdec I strdec
	  in free_strexp (I ++ I') strexp
	  end

    and free_strdec I =
      fn DECstrdec(_,dec) => free_dec I dec
       | STRUCTUREstrdec(_,strbind) => free_strbind I strbind
       | LOCALstrdec(_,strdec1,strdec2) => let val I1 = free_strdec I strdec1
					   in free_strdec (I ++ I1) strdec2
					   end 
       | EMPTYstrdec _ => empty_ids
       | SEQstrdec(_,strdec1,strdec2) => let val I1 = free_strdec I strdec1
					     val I2 = free_strdec (I ++ I1) strdec2
					 in I1 ++ I2
					 end

    and free_strbind I (STRBIND(_,strid,strexp,strbind_opt)) : ids =
      (free_strexp I strexp; add_strid(strid,empty_ids) ++ free_strbind_opt I strbind_opt)
    and free_strbind_opt I None = empty_ids
      | free_strbind_opt I (Some strbind) = free_strbind I strbind

    and free_sigexp I =
      fn SIGsigexp(_,spec) => (free_spec I spec; ())
       | SIGIDsigexp(_,sigid) => use_sigid(I, sigid)
       | WHERE_TYPEsigexp(_,sigexp,_,longtycon,ty) => (free_sigexp I sigexp; free_ty I ty)
                                          (*longtycon local*)

    and free_sigdec I (SIGNATUREsigdec(_,sigbind)) : ids = free_sigbind I sigbind

    and free_sigbind I (SIGBIND(_,sigid,sigexp,sigbind_opt)) : ids =
      (free_sigexp I sigexp; add_sigid(sigid,empty_ids) ++ free_sigbind_opt I sigbind_opt)
    and free_sigbind_opt I None = empty_ids
      | free_sigbind_opt I (Some sigbind) = free_sigbind I sigbind

    and free_spec I =
      fn VALspec(_,valdesc) => (free_valdesc I valdesc; empty_ids)
       | TYPEspec(_,typdesc) => free_typdesc I typdesc
       | EQTYPEspec(_,typdesc) => free_typdesc I typdesc
       | DATATYPEspec(_,datdesc) => free_datdesc I datdesc
       | DATATYPE_REPLICATIONspec(_,tycon,longtycon) => 
          (use_longtycon(I, longtycon); add_tycon(tycon,empty_ids))
       | EXCEPTIONspec(_,exdesc) => (free_exdesc I exdesc; empty_ids)
       | STRUCTUREspec(_,strdesc) => free_strdesc I strdesc
       | INCLUDEspec(info,sigexp) =>
	  let val (strids, tycons) = case ElabInfo.to_TypeInfo info
				       of Some (ElabInfo.TypeInfo.INCLUDE_INFO specs) => specs
					| _ => die "INCLUDEspec - no specs info"
	      val decl_strids = List.foldL (fn strid => fn ids => add_strid(strid,ids))
	      val decl_tycons = List.foldL (fn tycon => fn ids => add_tycon(tycon,ids))
	  in free_sigexp I sigexp;
	     decl_strids (decl_tycons empty_ids tycons) strids
	  end
       | SHARING_TYPEspec(_,spec,_) => free_spec I spec  (* these are local *)
       | SHARINGspec(_,spec,_) => free_spec I spec       (* these are local *)
       | EMPTYspec _ => empty_ids
       | SEQspec(_,spec1,spec2) =>
	  let val I1 = free_spec I spec1
	      val I2 = free_spec (I ++ I1) spec2
	  in I1 ++ I2
	  end

    and free_valdesc I (VALDESC(_,id,ty,valdesc_opt)) : unit = (*no need to add id*)
      (free_ty I ty; free_valdesc_opt I valdesc_opt)
    and free_valdesc_opt I None = ()
      | free_valdesc_opt I (Some valdesc) = free_valdesc I valdesc

    and free_typdesc I (TYPDESC(_,_,tycon,typdesc_opt)) : ids =
      add_tycon(tycon,empty_ids) ++ free_typdesc_opt I typdesc_opt
    and free_typdesc_opt I None = empty_ids
      | free_typdesc_opt I (Some typdesc) = free_typdesc I typdesc 

    and free_datdesc I datdesc : ids =
      let fun f (DATDESC(_,_,tycon,_,None)) = add_tycon(tycon,empty_ids)
	    | f (DATDESC(_,_,tycon,_,Some datdesc)) = add_tycon(tycon,f datdesc)
	  val I' = f datdesc
      in free_datdesc' (I ++ I') datdesc; I'
      end
    and free_datdesc' I (DATDESC(_,_,_,condesc,datdesc_opt)) : unit =
      (free_condesc I condesc; free_datdesc_opt' I datdesc_opt)
    and free_datdesc_opt' I None = ()
      | free_datdesc_opt' I (Some datdesc) = free_datdesc' I datdesc

    and free_condesc I (CONDESC(_,con,ty_opt,condesc_opt)) : unit = (*no need to add con*)
      (free_ty_opt I ty_opt; free_condesc_opt I condesc_opt)
    and free_condesc_opt I None = ()
      | free_condesc_opt I (Some condesc) = free_condesc I condesc

    and free_exdesc I (EXDESC(_,excon,ty_opt,exdesc_opt)) : unit = (*no need to add excon*)
      (free_ty_opt I ty_opt; free_exdesc_opt I exdesc_opt)
    and free_exdesc_opt I None = ()
      | free_exdesc_opt I (Some exdesc) = free_exdesc I exdesc

    and free_strdesc I (STRDESC(_,strid,sigexp,strdesc_opt)) : ids =
      (free_sigexp I sigexp; (add_strid(strid,empty_ids)) ++ free_strdesc_opt I strdesc_opt)
    and free_strdesc_opt I None = empty_ids
      | free_strdesc_opt I (Some strdesc) = free_strdesc I strdesc

    fun free_fundec I (FUNCTORfundec(_,funbind)) : ids = free_funbind I funbind
    and free_funbind I (FUNBIND(_,funid,strid,sigexp,strexp,funbind_opt)) : ids =
      (free_sigexp I sigexp;
       free_strexp (add_strid(strid,I)) strexp;
       (add_funid(funid,empty_ids)) ++ free_funbind_opt I funbind_opt)
    and free_funbind_opt I None = empty_ids
      | free_funbind_opt I (Some funbind) = free_funbind I funbind

    fun free_topdec I =
      fn STRtopdec(_,strdec,topdec_opt) => let val I' = free_strdec I strdec
					   in free_topdec_opt (I ++ I') topdec_opt
					   end
       | SIGtopdec(_,sigdec,topdec_opt) => let val I' = free_sigdec I sigdec
					   in free_topdec_opt (I ++ I') topdec_opt
					   end
       | FUNtopdec(_,fundec,topdec_opt) => let val I' = free_fundec I fundec
					   in free_topdec_opt (I ++ I') topdec_opt
					   end
    and free_topdec_opt I None = empty_ids
      | free_topdec_opt I (Some topdec) = free_topdec I topdec


    (*
     * MAIN FUNCTIONS
     *)

    fun free_ids_any (free_any:ids->'a->'b) (any:'a) : ids =
      let val _ = free := empty_ids
	  val _ = free_any empty_ids any
	  val free_ids = !free
	  val _ = free := empty_ids
      in free_ids
      end

    val free_ids = free_ids_any free_topdec
    val free_ids_dec = free_ids_any free_dec
    val free_ids_strexp = free_ids_any free_strexp


    (* 
     * PRETTYPRINTING
     *)

    type StringTree = PP.StringTree
    val layout_vids = IdSet.layoutSet {start="vids=[",finish="]",sep=","} (PP.LEAF o Ident.pr_id)
    and layout_tycons = TyConSet.layoutSet {start="tycons=[",finish="]",sep=","} (PP.LEAF o TyCon.pr_TyCon)
    and layout_strids = StrIdSet.layoutSet {start="strids=[",finish="]",sep=","} (PP.LEAF o StrId.pr_StrId)
    and layout_funids = FunIdSet.layoutSet {start="funids=[",finish="]",sep=","} (PP.LEAF o FunId.pr_FunId)
    and layout_sigids = SigIdSet.layoutSet {start="sigids=[",finish="]",sep=","} (PP.LEAF o SigId.pr_SigId)

    fun layout_ids ({vids,tycons,strids,funids,sigids}:ids) =
      PP.NODE{start="{|", finish="|}", indent=2,childsep=PP.RIGHT ", ",
	      children=[layout_vids vids,
			layout_tycons tycons,
			layout_strids strids,
			layout_funids funids,
			layout_sigids sigids]}
  end