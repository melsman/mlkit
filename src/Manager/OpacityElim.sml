(*$OpacityElim: CRASH ELAB_INFO ENVIRONMENTS STATOBJECT
                TOPDEC_GRAMMAR OPACITY_ELIM*)

functor OpacityElim(structure Crash : CRASH
		    structure ElabInfo : ELAB_INFO
		    structure Environments : ENVIRONMENTS
		      sharing type Environments.realisation = ElabInfo.TypeInfo.realisation
			  and type Environments.TyEnv = ElabInfo.TypeInfo.TyEnv
		    structure StatObject : STATOBJECT
		      sharing StatObject.TyName = Environments.TyName
			  and type StatObject.TypeFcn = Environments.TypeFcn
		    structure TopdecGrammar : TOPDEC_GRAMMAR
		      sharing type TopdecGrammar.info = ElabInfo.ElabInfo
			  and type TopdecGrammar.tycon = Environments.tycon) : OPACITY_ELIM =
  struct
    structure TyName = Environments.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure TyCon = DecGrammar.TyCon
    structure TypeInfo = ElabInfo.TypeInfo
    structure VE = Environments.VE
    structure TE = Environments.TE
    structure TypeFcn = StatObject.TypeFcn
    structure TyStr = Environments.TyStr

    fun die s = Crash.impossible ("OpacityElim." ^ s)

    type realisation = Environments.realisation
    type topdec = TopdecGrammar.topdec

    val plus = Environments.Realisation.oo
    val enrich = Environments.Realisation.enrich
    fun restrict (rea,set) = Environments.Realisation.restrict set rea
    val empty : realisation = Environments.Realisation.Id
    val initial : realisation = Environments.Realisation.Id  (* here we want to insert  char -> word8,  int -> word, etc. *)


    (* ---------------------------------------------------------------------
     * Eliminate opaque signature constraints by translating them into
     * transparent signature constraints; this is fine since we have
     * already done elaboration at this stage. One can prove that
     * opaque signature constraints only limit what programs
     * elaborate. The translation here also alter type information
     * recorded during elaboration. Martin Elsman 13/10/97 
     * --------------------------------------------------------------------- *)
     
    fun on_info(rea, elab_info) =
      case ElabInfo.to_TypeInfo elab_info
	of Some type_info => ElabInfo.plus_TypeInfo elab_info (ElabInfo.TypeInfo.on_TypeInfo(rea,type_info))
	 | None => elab_info          (* plus_TypeInfo is destructive as we want it to be.. *)

    fun elim_opt elim (rea, Some phrase) =
      let val (phrase', rea') = elim(rea,phrase)
      in (Some phrase', rea')
      end
      | elim_opt elim (rea, None) = (None, empty) 

    fun elim_opt' elim (rea, Some phrase) = Some(elim(rea,phrase))
      | elim_opt' elim (rea, None) = None

		     (* ------- *)
		     (*  Core   *)
		     (* ------- *)

   local open DecGrammar
   in
     
     fun elim_atexp(rea, atexp) =
       case atexp
	 of SCONatexp(i,scon) => (SCONatexp(on_info(rea,i),scon), empty)         
	  | IDENTatexp (i, longid_op_opt) => (IDENTatexp(on_info(rea,i), longid_op_opt), empty)
	  | RECORDatexp (i,exprowopt) => 
	   let val (exprowopt', rea') = elim_opt elim_exprow (rea,exprowopt)
	       val i' = on_info(rea,i)
	   in (RECORDatexp(i',exprowopt'), rea')
	   end
	  | LETatexp(i,dec,exp) =>
	   let val (dec', rea') = elim_dec(rea,dec)
	       val (exp', rea'') = elim_exp(plus(rea,rea'),exp)
	       val i' = on_info(rea,i)
	   in (LETatexp(i',dec',exp'),plus(rea',rea''))
	   end
          | PARatexp(i,exp) =>
	   let val (exp', rea') = elim_exp(rea,exp)
	       val i' = on_info(rea,i)
	   in (PARatexp(i',exp'), rea')
	   end

     and elim_exprow(rea, EXPROW(i,lab,exp,exprowopt)) =
       let val (exp',rea') = elim_exp(rea,exp)
	   val (exprowopt', rea'') = elim_opt elim_exprow (rea,exprowopt)
	   val i' = on_info(rea,i)
       in (EXPROW(i',lab,exp',exprowopt'), plus(rea',rea''))
       end

     and elim_exp(rea, exp) =
       case exp
	 of ATEXPexp(i,atexp) => 
           let val (atexp', rea') = elim_atexp(rea,atexp)
	       val i' = on_info(rea,i)
	   in (ATEXPexp(i',atexp'), rea')
	   end
	  | APPexp(i,exp,atexp) =>
	   let val (exp',rea') = elim_exp(rea,exp)
	       val (atexp',rea'') = elim_atexp(rea,atexp)
	       val i' = on_info(rea,i)
	   in (APPexp(i',exp',atexp'),plus(rea',rea''))
	   end
	  | TYPEDexp(i,exp,ty) =>
	   let val (exp',rea') = elim_exp(rea,exp)
	       val i' = on_info(rea,i)
	   in (TYPEDexp(i',exp',ty), rea')
	   end
	  | HANDLEexp(i,exp,match) =>
	   let val (exp',rea') = elim_exp(rea,exp)
	       val (match',rea'') = elim_match(rea,match)
	       val i' = on_info(rea,i)
	   in (HANDLEexp(i',exp',match'), plus(rea',rea''))
	   end	   
	  | RAISEexp(i,exp) =>
	   let val (exp',rea') = elim_exp(rea,exp)
	       val i' = on_info(rea,i)
	   in (RAISEexp(i',exp'), rea')
	   end
	  | FNexp(i,match) =>
	   let val (match',rea') = elim_match(rea,match)
	       val i' = on_info(rea,i)
	   in (FNexp(i',match'), rea')
	   end
	  | UNRES_INFIXexp(i,atexps) => die "elim_exp.UNRES_INFIX"

     and elim_match(rea,MATCH(i,mrule,matchopt)) =
       let val (mrule',rea') = elim_mrule(rea,mrule)
	   val (matchopt',rea'') = elim_opt elim_match (rea,matchopt)
	   val i' = on_info(rea,i)
       in (MATCH(i',mrule',matchopt'), plus(rea',rea''))
       end

     and elim_mrule(rea,MRULE(i,pat,exp)) =
       let val pat' = elim_pat(rea,pat)
	   val (exp',rea') = elim_exp(rea,exp)
	   val i' = on_info(rea,i)
       in (MRULE(i',pat',exp'), rea')
       end

     and elim_dec(rea,dec) =
       case dec
	 of VALdec(i,tyvars,valbind) => 
	   let val (valbind', rea') = elim_valbind(rea,valbind)
	       val i' = on_info(rea,i)
	   in (VALdec(i',tyvars,valbind'),rea')
	   end
	   | UNRES_FUNdec(i,tyvars,FValBind) => die "elim_dec.UNRES_FUNdec"
	   | TYPEdec(i,typbind) =>
           let val i' = on_info(rea,i)
	   in (TYPEdec(i',typbind), empty)
	   end
	   | DATATYPEdec(i,datbind) => 
	   let val i' = on_info(rea,i)
	   in (DATATYPEdec(i',datbind),empty)
	   end
	   | DATATYPE_REPLICATIONdec(i,tycon,longtycon) => 
	   let val i' = on_info(rea,i)
	   in (DATATYPE_REPLICATIONdec(i',tycon,longtycon), empty)
	   end
	   | ABSTYPEdec(i,datbind,dec) =>   (* MEMO: We should translate abstype here to a local datatype *)
	   let val (dec',rea') = elim_dec(rea,dec)
	       val i' = on_info(rea,i)
	       val dummyinfo = i
	       val (TE,rea'') = case ElabInfo.to_TypeInfo i'
				  of Some(TypeInfo.ABSTYPE_INFO (TE,rea)) => (TE,rea)
				   | _ => die "elim_dec.ABSTYPE.no tyenv info"
	       val datatypedecinfo = ElabInfo.plus_TypeInfo dummyinfo (TypeInfo.TYENV_INFO TE) 
	       val (TE',typbindopt) = TE.Fold (fn (tycon,tystr) => fn (TE,typbindopt) =>
					       let val theta = TyStr.to_theta tystr
						   val tystr' = TyStr.from_theta_and_VE(theta,VE.empty)
						   val arity = TypeFcn.arity theta
						   val tyvars = []
						   val longtycon = TyCon.implode_LongTyCon([],tycon)
						   val ty = CONty(dummyinfo,[],longtycon)
						   val typbind = TYPBIND(dummyinfo,tyvars,tycon,ty,typbindopt)
					       in (TE.plus(TE.singleton(tycon,tystr'), TE), Some(typbind))
					       end) (TE.empty,None) TE
	       val typbind = case typbindopt
			       of Some typbind => typbind
				| None => die "elim_dec.ABSTYPE.typbindopt is None" 
	       val typedecinfo = ElabInfo.plus_TypeInfo dummyinfo (TypeInfo.TYENV_INFO TE')
	   in (LOCALdec(dummyinfo, 
			DATATYPEdec(datatypedecinfo,datbind),
			SEQdec(dummyinfo,
			       TYPEdec(typedecinfo,typbind),
			       dec')), plus(rea',rea''))
	   end
	   | EXCEPTIONdec(i,exbind) => (EXCEPTIONdec(on_info(rea,i),exbind),empty)
	   | LOCALdec(i,dec1,dec2) =>
	   let val (dec1', rea') = elim_dec(rea,dec1)
	       val (dec2', rea'') = elim_dec(plus(rea,rea'),dec2)
	       val i' = on_info(rea,i)
	   in (LOCALdec(i',dec1',dec2'),plus(rea',rea''))
	   end
	   | OPENdec(i,longstridwithinfos) => 
	   let val i' = on_info(rea,i)
	       fun on (WITH_INFO(i,longstrid)) = WITH_INFO(on_info(rea,i), longstrid)  
	       val longstridwithinfos' = map on longstridwithinfos
	   in (OPENdec(i',longstridwithinfos'), empty)
	   end
	   | SEQdec(i,dec1,dec2) =>
	   let val (dec1', rea') = elim_dec(rea,dec1)
	       val (dec2', rea'') = elim_dec(plus(rea,rea'),dec2)
	       val i' = on_info(rea,i)
	   in (SEQdec(i',dec1',dec2'),plus(rea',rea''))
	   end
	   | INFIXdec _ => (dec,empty)
	   | INFIXRdec _ => (dec,empty)
	   | NONFIXdec _ => (dec,empty)
	   | EMPTYdec _ => (dec,empty)

     and elim_valbind(rea,valbind) =
       case valbind
	 of PLAINvalbind(i,pat,exp,valbindopt) =>
	   let val pat' = elim_pat(rea,pat)
	       val (exp', rea') = elim_exp(rea,exp)
	       val (valbindopt', rea'') = elim_opt elim_valbind (rea,valbindopt)
	       val i' = on_info(rea,i)
	   in (PLAINvalbind(i',pat',exp',valbindopt'),plus(rea',rea''))
	   end
	   | RECvalbind(i,valbind) =>
	   let val (valbind', rea') = elim_valbind (rea,valbind)
	       val i' = on_info(rea,i)
	   in (RECvalbind(i',valbind'),rea')
	   end
	   
     and elim_FValBind(rea,FVALBIND(i,FClause,FValBindopt)) =
       let val (FClause', rea') = elim_FClause(rea,FClause)
	   val (FValBindopt', rea'') = elim_opt elim_FValBind (rea, FValBindopt)
	   val i' = on_info(rea,i)
       in (FVALBIND(i',FClause',FValBindopt'), plus(rea',rea''))
       end

     and elim_FClause(rea,FCLAUSE(i,atpats,tyopt,exp,FClauseopt)) =
       let val atpats' = elim_atpats(rea,atpats)
	   val (exp', rea') = elim_exp(rea, exp)
	   val (FClauseopt',rea'') = elim_opt elim_FClause (rea,FClauseopt)
	   val i' = on_info(rea,i)
       in (FCLAUSE(i',atpats',tyopt,exp',FClauseopt'), plus(rea',rea''))
       end

		       (* Patterns *)

     and elim_atpat(rea,atpat) =
       case atpat
	 of WILDCARDatpat i => WILDCARDatpat(on_info(rea,i))
	  | SCONatpat(i,scon) => SCONatpat(on_info(rea,i),scon)
	  | LONGIDatpat(i,longid_op_opt) => LONGIDatpat(on_info(rea,i),longid_op_opt)
	  | RECORDatpat(i,patrowopt) =>
	  let val i' = on_info(rea,i) 
	      val patrowopt' = elim_opt' elim_patrow (rea,patrowopt)
	  in RECORDatpat(i', patrowopt')
	  end
	  | PARatpat(i,pat) => 
	  let val i' = on_info(rea,i)
	      val pat' = elim_pat(rea,pat)
	  in PARatpat(i',pat')
	  end

     and elim_atpats(rea,[]) = []
       | elim_atpats(rea,atpat::atpats) = elim_atpat(rea,atpat)::elim_atpats(rea,atpats)

     and elim_patrow(rea,patrow) =
       case patrow
	 of DOTDOTDOT i => DOTDOTDOT(on_info(rea,i))
	  | PATROW(i,lab,pat,patrowopt) => PATROW(on_info(rea,i),lab,elim_pat(rea,pat),elim_opt' elim_patrow (rea,patrowopt))

     and elim_pat(rea, pat) =
       case pat
	 of ATPATpat(i,atpat) => ATPATpat(on_info(rea,i),elim_atpat(rea,atpat))
	  | CONSpat(i,longid_op_opt,atpat) => CONSpat(on_info(rea,i),longid_op_opt,elim_atpat(rea,atpat))
	  | TYPEDpat(i,pat,ty) => TYPEDpat(on_info(rea,i),elim_pat(rea,pat),ty) 
	  | LAYEREDpat(i,id_op_opt,tyopt,pat) => LAYEREDpat(on_info(rea,i),id_op_opt,tyopt,elim_pat(rea,pat)) 
	  | UNRES_INFIXpat _ => die "elim_pat.UNRES_INFIX"

   end (* local *)

		     (* ------- *)
		     (* Modules *)
		     (* ------- *)

   local open TopdecGrammar
   in

    fun elim_strexp(rea, strexp) =
      case strexp
	of STRUCTstrexp(i, strdec) =>
	  let val (strdec', rea') = elim_strdec(rea, strdec)
	  in (STRUCTstrexp(on_info(rea,i), strdec'), rea')
	  end
	 | LONGSTRIDstrexp(i, longstrid) => (LONGSTRIDstrexp(on_info(rea,i), longstrid), empty)
	 | TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (strexp', rea') = elim_strexp(rea, strexp)
	  in (TRANSPARENT_CONSTRAINTstrexp(on_info(rea,i),strexp',sigexp), rea')
	  end
	 | OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (strexp', rea') = elim_strexp(rea, strexp)
	      val (E,rea'') = case ElabInfo.to_TypeInfo (on_info(rea,i))
				of Some(TypeInfo.OPAQUE_CONSTRAINT_INFO (E,rea'')) => (E,rea'')
				 | _ => die "elim_strexp.OPAQUE_CONSTRAINT.no info"
	      val i' = ElabInfo.plus_TypeInfo i (TypeInfo.TRANS_CONSTRAINT_INFO E)  
	  in (TRANSPARENT_CONSTRAINTstrexp(i',strexp',sigexp), plus(rea',rea''))
	  end
	 | APPstrexp(i, funid, strexp) => 
	  let val (strexp', rea') = elim_strexp(rea, strexp)
	  in (APPstrexp(on_info(rea,i), funid, strexp'), rea')
	  end
	 | LETstrexp(i, strdec, strexp) =>
	  let val (strdec', rea') = elim_strdec(rea, strdec)
	      val (strexp', rea'') = elim_strexp(rea, strexp)
	  in (LETstrexp(on_info(rea,i),strdec',strexp'), plus(rea',rea'))
	  end

    and elim_strdec(rea, strdec) =
      case strdec
	of DECstrdec(i, dec) =>
	  let val (dec', rea') = elim_dec(rea, dec)
	  in (DECstrdec(on_info(rea,i), dec'), rea')
	  end
	 | STRUCTUREstrdec(i, strbind) =>
	  let val (strbind', rea') = elim_strbind(rea, strbind)
	  in (STRUCTUREstrdec(on_info(rea,i), strbind'), rea')
	  end	  
	 | LOCALstrdec(i, strdec1, strdec2) =>
	  let val (strdec1', rea1) = elim_strdec(rea, strdec1)
	      val (strdec2', rea2) = elim_strdec(plus(rea,rea1), strdec2)
	  in (LOCALstrdec(on_info(rea,i), strdec1', strdec2'), plus(rea1,rea2))
	  end
	 | EMPTYstrdec i => (EMPTYstrdec (on_info(rea,i)), empty)
	 | SEQstrdec(i, strdec1, strdec2) =>
	  let val (strdec1', rea1) = elim_strdec(rea, strdec1)
	      val (strdec2', rea2) = elim_strdec(plus(rea,rea1), strdec2)
	  in (SEQstrdec(on_info(rea,i), strdec1', strdec2'), plus(rea1,rea2))
	  end

    and elim_strbind (rea, STRBIND(i, strid, strexp, strbindopt)) =
      let val (strexp', rea') = elim_strexp(rea, strexp)
	  val (strbindopt', rea'') = elim_opt elim_strbind(rea, strbindopt)
      in (STRBIND(on_info(rea,i), strid, strexp', strbindopt'), plus(rea',rea''))
      end 

    fun elim_funbind(rea, FUNBIND(i, funid, strid, sigexp, strexp, funbindopt)) =
      let val (strexp',rea') = elim_strexp(rea, strexp)
	  val (funbindopt',rea'') = elim_opt elim_funbind (rea, funbindopt)
      in (FUNBIND(on_info(rea,i),funid,strid,sigexp,strexp',funbindopt'), plus(rea',rea''))
      end

    fun elim_fundec(rea, FUNCTORfundec(i,funbind)) = 
      let val (funbind', rea') = elim_funbind(rea, funbind)
      in (FUNCTORfundec(on_info(rea,i),funbind'), rea')
      end

    fun elim_topdec(rea, topdec) =
      case topdec
	of STRtopdec(i, strdec, topdecopt) =>
	  let val (strdec', rea') = elim_strdec(rea, strdec)
	      val (topdecopt', rea'') = elim_opt elim_topdec (plus(rea,rea'), topdecopt)
	  in (STRtopdec(on_info(rea,i),strdec',topdecopt'), plus(rea',rea''))
	  end
	 | SIGtopdec(i, sigdec, topdecopt) =>
	  let val (topdecopt', rea') = elim_opt elim_topdec (rea, topdecopt)
	  in (SIGtopdec(on_info(rea,i),sigdec,topdecopt'), rea')
	  end
	 | FUNtopdec(i, fundec, topdecopt) =>
	  let val (fundec', rea') = elim_fundec(rea, fundec)
	      val (topdecopt', rea'') = elim_opt elim_topdec (plus(rea,rea'), topdecopt)
	  in (FUNtopdec(on_info(rea,i),fundec',topdecopt'), plus(rea',rea''))
	  end

   end (* local *)

   fun opacity_elimination (rea: realisation, topdec: topdec) : topdec * realisation =
     elim_topdec (rea, topdec)

   type StringTree = Environments.StringTree
   val layout : realisation -> StringTree = Environments.Realisation.layout

  end