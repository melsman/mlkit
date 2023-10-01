
structure FreeIds:  FREE_IDS =
  struct
    structure PP = PrettyPrint
    structure TopdecGrammar = PostElabTopdecGrammar
    structure ElabInfo = AllInfo.ElabInfo

    fun die s = Crash.impossible ("FreeIds."^s)

    open TopdecGrammar
    open TopdecGrammar.DecGrammar

    type id    = Ident.id
    type tycon = TyCon.tycon
    type strid = StrId.strid
    type funid = FunId.funid
    type sigid = SigId.sigid
    type longid = Ident.longid
    type longtycon = TyCon.longtycon
    type longstrid = StrId.longstrid

    (* The way this works is by passing lists of those identifiers
     * being declared downwards in the syntax tree, and before an
     * identifier is added to a bucket (holding free identifiers) it
     * is checked if it is in a `declared set'. -- Martin *)

    type ids = {vids: id list, tycons: tycon list, strids: strid list,
                funids: funid list, sigids: sigid list}

    val empty_ids : ids = {vids=[], tycons=[], strids=[], funids=[], sigids=[]}

    infix ++
    fun ({vids,tycons,strids,funids,sigids} ++
	 {vids=vids',tycons=tycons',strids=strids',funids=funids',sigids=sigids'}) =
      {vids=vids' @ vids, tycons=tycons' @ tycons, strids=strids' @ strids,
       funids=funids' @ funids, sigids=sigids' @ sigids}

    (* -------------------------------------
     * Lists of ids
     * ------------------------------------- *)

    fun add_vid (id:id,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=id::vids,tycons=tycons,strids=strids,funids=funids,sigids=sigids}

    fun add_vids (vids':id list,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids'@vids,tycons=tycons,strids=strids,funids=funids,sigids=sigids}

    fun add_tycon (tycon:tycon,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycon::tycons,strids=strids,funids=funids,sigids=sigids}

    fun add_strid (strid:strid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=strid::strids,funids=funids,sigids=sigids}

    fun add_funid (funid:funid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=strids,funids=funid::funids,sigids=sigids}

    fun add_sigid (sigid:sigid,{vids,tycons,strids,funids,sigids}:ids) =
      {vids=vids,tycons=tycons,strids=strids,funids=funids,sigids=sigid::sigids}

    (* -------------------------------------
     * Buckets for free ids
     * ------------------------------------- *)

    type longids = {funids: funid list, sigids: sigid list,
		    longstrids: longstrid list, longtycons: longtycon list,
		    longvids: longid list}

    fun mem (y,[]) = false
      | mem (y,x::xs) = y=x orelse mem(y,xs)

    local
      val bucket_longvids = ref ([] : longid list)
      val bucket_longtycons = ref ([] : longtycon list)
      val bucket_longstrids = ref ([] : longstrid list)
      val bucket_funids = ref ([] : funid list)
      val bucket_sigids = ref ([] : sigid list)

      fun mk_free bucket x = if mem(x,!bucket) then ()
                             else bucket := x::(!bucket)
    in
      fun mk_free_longvid (longvid:longid) = mk_free bucket_longvids longvid
      fun mk_free_longtycon (longtycon:longtycon) = mk_free bucket_longtycons longtycon
      fun mk_free_longstrid (longstrid:longstrid) = mk_free bucket_longstrids longstrid
      fun mk_free_funid (funid:funid) = mk_free bucket_funids funid
      fun mk_free_sigid (sigid:sigid) = mk_free bucket_sigids sigid

      fun reset_buckets() = (bucket_longvids := []; bucket_longtycons := [];
                             bucket_longstrids := []; bucket_funids := []; bucket_sigids := [])
      fun get_free_longids() : longids =
	{longvids= !bucket_longvids, longtycons= !bucket_longtycons,
	 longstrids= !bucket_longstrids, funids= !bucket_funids, sigids= !bucket_sigids}
      fun install_longids ({funids, sigids, longstrids, longtycons, longvids} : longids) : unit =
	(bucket_funids:=funids;
	 bucket_sigids:=sigids;
	 bucket_longstrids:=longstrids;
	 bucket_longtycons:=longtycons;
	 bucket_longvids:=longvids)
    end

    (* -------------------------------------
     * Functions to apply on uses of ids
     * ------------------------------------- *)

    fun use_longvid ({vids,strids,...}:ids,longvid:longid): unit =
      case Ident.decompose longvid
	of ([], vid) => if mem(vid,vids) then () else mk_free_longvid longvid
	 | (strid::_,_) => if mem(strid,strids) then () else mk_free_longvid longvid

    fun use_longstrid ({strids,...}:ids,longstrid:longstrid): unit =
      case StrId.explode_longstrid longstrid
	of ([],strid) => if mem(strid,strids) then () else mk_free_longstrid longstrid
	 | (strid::_,_) => if mem(strid,strids) then () else mk_free_longstrid longstrid

    fun use_longtycon ({tycons,strids,...}:ids,longtycon:longtycon): unit =
      case TyCon.explode_LongTyCon longtycon
	of ([], tycon) => if mem(tycon,tycons) then () else mk_free_longtycon longtycon
	 | (strid::_,_) => if mem(strid,strids) then () else mk_free_longtycon longtycon

    fun use_funid ({funids,...}:ids,funid:funid): unit =
      if mem(funid,funids) then () else mk_free_funid funid

    fun use_sigid ({sigids,...}:ids,sigid:sigid): unit =
      if mem(sigid,sigids) then () else mk_free_sigid sigid

    fun use_longids (ids:ids, {longvids,longtycons,longstrids,funids,sigids}) : unit =
      (app (fn a => use_longvid (ids, a)) longvids;
       app (fn a => use_longtycon (ids, a)) longtycons;
       app (fn a => use_longstrid (ids, a)) longstrids;
       app (fn a => use_funid (ids, a)) funids;
       app (fn a => use_sigid (ids, a)) sigids)

    (* Get type info from info-nodes; we could do better here, because
     * we force applications of realisations without using the
     * annotated type info. *)

    local fun normalise ti = ElabInfo.TypeInfo.normalise ti
    in fun to_TypeInfo i =
         case ElabInfo.to_TypeInfo i
	   of SOME ti => SOME(normalise ti)
	    | NONE => NONE
    end

    (* --------------------------------------
     * We carry around a persistent set of
     * bound identifiers, I.
     * -------------------------------------- *)

    (*
     * CORE
     *)

    fun free_atexp I =
      fn SCONatexp _ => ()
       | IDENTatexp(_, OP_OPT(longvid,_), _) => use_longvid(I,longvid)
       | RECORDatexp(_,exprow_opt,_) => free_exprow_opt I exprow_opt
       | LETatexp(_,dec,exp) => let val I' = free_dec I dec
				in free_exp (I ++ I') exp
				end
       | PARatexp(_,exp) => free_exp I exp

    and free_exprow I (EXPROW(_,_,exp,exprow_opt)) : unit = (free_exp I exp; free_exprow_opt I exprow_opt)
    and free_exprow_opt I NONE = ()
      | free_exprow_opt I (SOME exprow) = free_exprow I exprow

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
    and free_match_opt I NONE = ()
      | free_match_opt I (SOME match) = free_match I match

    and free_mrule I (MRULE(_,pat,exp)) : unit =
      let val I' = free_pat I pat
      in free_exp (I ++ I') exp
      end

    and free_dec I =
      fn VALdec(_,_,valbind) => free_valbind I valbind
       | UNRES_FUNdec _ => die "free.UNRES_FUNdec"
       | TYPEdec(_,typbind) => free_typbind I typbind
       | DATATYPEdec(_,datbind) => free_datbind I datbind
       | DATATYPE_REPLICATIONdec(info,tycon,longtycon) =>
         let
	   val vids =
	     case to_TypeInfo info
	       of SOME (ElabInfo.TypeInfo.TYENV_INFO TE) =>
		 (case Environments.TE.lookup TE tycon
		    of SOME tystr => EqSet.list(Environments.VE.dom(Environments.TyStr.to_VE tystr))
		     | NONE => die "free_dec.DATATYPE_REPLICATIONdec: no tystr")
		| _ => die "free_dec.DATATYPE_REPLICATIONdec: no type info"
	   val strids = #1(TyCon.explode_LongTyCon longtycon)
	   val longvids = map (fn id => Ident.implode_LongId (strids,id)) vids
	 in use_longtycon(I,longtycon);
	   app (fn longvid => use_longvid(I,longvid)) longvids;
	   add_vids(vids, add_tycon(tycon,empty_ids))
	 end
       | ABSTYPEdec(_,datbind,dec) =>
         let fun Abs ({tycons,...}:ids) = {tycons=tycons, vids=[], strids=[], funids=[], sigids=[]}
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
	         List.app (fn WITH_INFO(_,longstrid) => use_longstrid(I,longstrid))
		 longstrids_with_info
	      val (strids, tycons, ids) = case to_TypeInfo info
					    of SOME (ElabInfo.TypeInfo.OPEN_INFO decls) => decls
					     | _ => die "OPENdec - no decl. info"
	      val decl_strids = foldl (fn (strid,ids) => add_strid(strid,ids))
	      val decl_tycons = foldl (fn (tycon,ids) => add_tycon(tycon,ids))
	      val decl_ids = foldl (fn (vid,ids) => add_vid(vid,ids))
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
       | WITHdec _ => empty_ids

    and free_valbind I =
      fn PLAINvalbind(_,pat,exp,valbind_opt) =>
      (free_exp I exp; free_pat I pat ++ free_valbind_opt I valbind_opt)
       | RECvalbind(_,valbind) =>
      let fun f (PLAINvalbind(_,pat,_,NONE)) = free_pat I pat
	    | f (PLAINvalbind(_,pat,_,SOME valbind)) = free_pat I pat ++ f valbind
	    | f (RECvalbind(_,valbind)) = f valbind
	  val I' = f valbind
      in free_valbind' (I ++ I') valbind; I'
      end
    and free_valbind_opt I NONE = empty_ids
      | free_valbind_opt I (SOME valbind) = free_valbind I valbind

    and free_valbind' I =
      fn PLAINvalbind(_,_,exp,valbind_opt) => (free_exp I exp; free_valbind_opt' I valbind_opt)
       | RECvalbind(_,valbind) => free_valbind' I valbind
    and free_valbind_opt' I NONE = ()
      | free_valbind_opt' I (SOME valbind) = free_valbind' I valbind

    and free_typbind I (TYPBIND(_,_,tycon,ty,typbind_opt)) : ids =
      (free_ty I ty; add_tycon(tycon,empty_ids) ++ free_typbind_opt I typbind_opt)
    and free_typbind_opt I NONE = empty_ids
      | free_typbind_opt I (SOME typbind) = free_typbind I typbind

    and free_datbind I datbind : ids =
      let fun f (DATBIND(_,_,tycon,_,NONE)) = add_tycon(tycon,empty_ids)
	    | f (DATBIND(_,_,tycon,_,SOME datbind)) = add_tycon(tycon,f datbind)
	  fun g I (DATBIND(_,_,_,conbind,NONE)) = free_conbind I conbind
	    | g I (DATBIND(_,_,_,conbind,SOME datbind)) = free_conbind I conbind ++ g I datbind
	  val I1 = f datbind
	  val I2 = g (I ++ I1) datbind
      in I1 ++ I2
      end

    and free_conbind I (CONBIND(_,OP_OPT(con,_),ty_opt,conbind_opt)) : ids =
      (free_ty_opt I ty_opt; add_vid(con,empty_ids) ++ free_conbind_opt I conbind_opt)
    and free_conbind_opt I NONE = empty_ids
      | free_conbind_opt I (SOME conbind) = free_conbind I conbind

    and free_exbind I =
      fn EXBIND(_,OP_OPT(excon,_),ty_opt,exbind_opt) =>
      (free_ty_opt I ty_opt; add_vid(excon,empty_ids) ++ free_exbind_opt I exbind_opt)
       | EXEQUAL(_,OP_OPT(excon,_),OP_OPT(longvid,_),exbind_opt) =>
      (use_longvid(I,longvid); add_vid(excon,empty_ids) ++ free_exbind_opt I exbind_opt)
    and free_exbind_opt I NONE = empty_ids
      | free_exbind_opt I (SOME exbind) = free_exbind I exbind

    and free_atpat I =
      fn WILDCARDatpat _ => empty_ids
       | SCONatpat _ => empty_ids
       | LONGIDatpat(info,OP_OPT(longvid,_),_) =>
          (case to_TypeInfo info
	     of SOME (ElabInfo.TypeInfo.VAR_PAT_INFO _) =>
	       (case Ident.decompose longvid
		  of ([],vid) => add_vid(vid, empty_ids)
		   | _ => die "free_atpat.longid in pattern.")
              | SOME (ElabInfo.TypeInfo.CON_INFO _) => (use_longvid(I,longvid); empty_ids)
	      | SOME (ElabInfo.TypeInfo.EXCON_INFO _) => (use_longvid(I,longvid); empty_ids)
	      | _ => die "free_atpat.no type info")
       | RECORDatpat(_,patrow_opt) => free_patrow_opt I patrow_opt
       | PARatpat(_,pat) => free_pat I pat

    and free_patrow I =
      fn DOTDOTDOT _ => empty_ids
       | PATROW(_,lab,pat,patrow_opt) => (free_pat I pat ++ free_patrow_opt I patrow_opt)
    and free_patrow_opt I NONE = empty_ids
      | free_patrow_opt I (SOME patrow) = free_patrow I patrow

    and free_pat I =
      fn ATPATpat(_,atpat) => free_atpat I atpat
       | CONSpat(_,OP_OPT(longvid,_),atpat) => (use_longvid(I,longvid); free_atpat I atpat)
       | TYPEDpat(_,pat,ty) => (free_ty I ty; free_pat I pat)
       | LAYEREDpat(_,OP_OPT(vid,_),ty_opt,pat) => (free_ty_opt I ty_opt; add_vid(vid,empty_ids) ++ free_pat I pat)
       | UNRES_INFIXpat _ => die "free.UNRES_INFIXpat"

    and free_ty I =
      fn TYVARty _ => ()
       | RECORDty(_,tyrow_opt,_) => free_tyrow_opt I tyrow_opt
       | CONty(_,tys,longtycon) => (List.app (free_ty I) tys; use_longtycon(I,longtycon))
       | FNty(_,ty1,_,ty2) => (free_ty I ty1; free_ty I ty2)
       | PARty(_,ty,_) => free_ty I ty
       | WITHty(_,ty,_) => free_ty I ty
    and free_ty_opt I NONE = ()
      | free_ty_opt I (SOME ty) = free_ty I ty

    and free_tyrow I (TYROW(_,lab,ty,tyrow_opt)) : unit = (free_ty I ty; free_tyrow_opt I tyrow_opt)
    and free_tyrow_opt I NONE = ()
      | free_tyrow_opt I (SOME tyrow) = free_tyrow I tyrow


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
    and free_strbind_opt I NONE = empty_ids
      | free_strbind_opt I (SOME strbind) = free_strbind I strbind

    and free_sigexp I =
      fn SIGsigexp(_,spec) => (free_spec I spec; ())
       | SIGIDsigexp(_,sigid) => use_sigid(I, sigid)
       | WHERE_TYPEsigexp(_,sigexp,_,longtycon,ty) => (free_sigexp I sigexp; free_ty I ty)
                                          (*longtycon local*)

    and free_sigdec I (SIGNATUREsigdec(_,sigbind)) : ids = free_sigbind I sigbind

    and free_sigbind I (SIGBIND(_,sigid,sigexp,sigbind_opt)) : ids =
      (free_sigexp I sigexp; add_sigid(sigid,empty_ids) ++ free_sigbind_opt I sigbind_opt)
    and free_sigbind_opt I NONE = empty_ids
      | free_sigbind_opt I (SOME sigbind) = free_sigbind I sigbind

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
	  let val (strids, tycons) = case to_TypeInfo info
				       of SOME (ElabInfo.TypeInfo.INCLUDE_INFO specs) => specs
					| _ => die "INCLUDEspec - no specs info"
	      val decl_strids = foldl (fn (strid,ids) => add_strid(strid,ids))
	      val decl_tycons = foldl (fn (tycon,ids) => add_tycon(tycon,ids))
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
    and free_valdesc_opt I NONE = ()
      | free_valdesc_opt I (SOME valdesc) = free_valdesc I valdesc

    and free_typdesc I (TYPDESC(_,_,tycon,typdesc_opt)) : ids =
      add_tycon(tycon,empty_ids) ++ free_typdesc_opt I typdesc_opt
    and free_typdesc_opt I NONE = empty_ids
      | free_typdesc_opt I (SOME typdesc) = free_typdesc I typdesc

    and free_datdesc I datdesc : ids =
      let fun f (DATDESC(_,_,tycon,_,NONE)) = add_tycon(tycon,empty_ids)
	    | f (DATDESC(_,_,tycon,_,SOME datdesc)) = add_tycon(tycon,f datdesc)
	  val I' = f datdesc
      in free_datdesc' (I ++ I') datdesc; I'
      end
    and free_datdesc' I (DATDESC(_,_,_,condesc,datdesc_opt)) : unit =
      (free_condesc I condesc; free_datdesc_opt' I datdesc_opt)
    and free_datdesc_opt' I NONE = ()
      | free_datdesc_opt' I (SOME datdesc) = free_datdesc' I datdesc

    and free_condesc I (CONDESC(_,con,ty_opt,condesc_opt)) : unit = (*no need to add con*)
      (free_ty_opt I ty_opt; free_condesc_opt I condesc_opt)
    and free_condesc_opt I NONE = ()
      | free_condesc_opt I (SOME condesc) = free_condesc I condesc

    and free_exdesc I (EXDESC(_,excon,ty_opt,exdesc_opt)) : unit = (*no need to add excon*)
      (free_ty_opt I ty_opt; free_exdesc_opt I exdesc_opt)
    and free_exdesc_opt I NONE = ()
      | free_exdesc_opt I (SOME exdesc) = free_exdesc I exdesc

    and free_strdesc I (STRDESC(_,strid,sigexp,strdesc_opt)) : ids =
      (free_sigexp I sigexp; (add_strid(strid,empty_ids)) ++ free_strdesc_opt I strdesc_opt)
    and free_strdesc_opt I NONE = empty_ids
      | free_strdesc_opt I (SOME strdesc) = free_strdesc I strdesc

    fun free_fundec I (FUNCTORfundec(_,funbind)) : ids = free_funbind I funbind
    and free_funbind I (FUNBIND(info,funid,strid,sigexp,strexp,funbind_opt)) : ids =
      let val _ = free_sigexp I sigexp
	  val longids = get_free_longids()
	  val _ = reset_buckets()
	  val _ = free_strexp empty_ids strexp   (* (add_strid(strid,I)) *)
	  val longids_body = get_free_longids()
	  val _ = install_longids longids
	  val _ = use_longids (add_strid(strid,I), longids_body)
	  val _ = case to_TypeInfo info
		    of SOME (ElabInfo.TypeInfo.FUNBIND_INFO {argE,elabBref,T,resE,opaq_env_opt}) =>
		      ((elabBref := ModuleEnvironments.B.restrict(!elabBref,longids_body))
		       handle _ => die "free_funbind.restrict failed")
		     | _ => die "free_funbind.no type info"
      in
	add_funid(funid,empty_ids) ++ free_funbind_opt I funbind_opt
      end
    and free_funbind_opt I NONE = empty_ids
      | free_funbind_opt I (SOME funbind) = free_funbind I funbind

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
    and free_topdec_opt I NONE = empty_ids
      | free_topdec_opt I (SOME topdec) = free_topdec I topdec


    (*
     * MAIN FUNCTIONS
     *)

    fun free_ids_any (ids:ids) (free_any:ids->'a->'b) (any:'a) : longids =
      let val _ = reset_buckets()
	  val _ = free_any ids any
	  val free_longids = get_free_longids()
	  val _ = reset_buckets()
      in free_longids
      end

    val fid_topdec = free_ids_any empty_ids free_topdec
    val fid_dec = free_ids_any empty_ids free_dec
    val fid_strexp = free_ids_any empty_ids free_strexp
    fun fid_strexp_sigexp strid strexp sigexp =
      let val _ = reset_buckets()
	  val _ = free_strexp {vids=[], tycons=[], strids=[strid], funids=[], sigids=[]} strexp
	  val _ = free_sigexp empty_ids sigexp
	  val free_longids = get_free_longids()
	  val _ = reset_buckets()
      in free_longids
      end

    (*
     * PRETTYPRINTING
     *)

    type StringTree = PP.StringTree
    fun layout_list s layout_elem l =
        PP.NODE{start=s ^ "=[",finish="]",indent=0,childsep=PP.RIGHT ",",children=map layout_elem l}

    val layout_longvids = layout_list "longvids" (PP.LEAF o Ident.pr_longid)
    and layout_longtycons = layout_list "longtycons" (PP.LEAF o TyCon.pr_LongTyCon)
    and layout_longstrids = layout_list "longstrids" (PP.LEAF o StrId.pr_LongStrId)
    and layout_funids = layout_list "funids" (PP.LEAF o FunId.pr_FunId)
    and layout_sigids = layout_list "sigids" (PP.LEAF o SigId.pr_SigId)

    fun layout_longids ({longvids,longtycons,longstrids,funids,sigids}:longids) =
      PP.NODE{start="{|", finish="|}", indent=2,childsep=PP.RIGHT ", ",
	      children=[layout_longvids longvids,
			layout_longtycons longtycons,
			layout_longstrids longstrids,
			layout_funids funids,
			layout_sigids sigids]}
  end
