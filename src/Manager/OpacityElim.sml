
functor OpacityElim(structure Crash : CRASH
		    structure PP : PRETTYPRINT
		    structure OpacityEnv : OPACITY_ENV
		    structure ElabInfo : ELAB_INFO
		      sharing type ElabInfo.TypeInfo.opaq_env = OpacityEnv.opaq_env
		    structure Environments : ENVIRONMENTS
		      sharing type Environments.realisation = ElabInfo.TypeInfo.realisation 
			                                 = OpacityEnv.realisation
			  and type Environments.TyEnv = ElabInfo.TypeInfo.TyEnv
			  and type Environments.Env = ElabInfo.TypeInfo.Env
			  and type Environments.StringTree = PP.StringTree
		    structure StatObject : STATOBJECT
		      sharing StatObject.TyName = Environments.TyName = OpacityEnv.TyName = ElabInfo.TypeInfo.TyName
			  and type StatObject.TypeFcn = Environments.TypeFcn
		    structure TopdecGrammar : TOPDEC_GRAMMAR
		      sharing type TopdecGrammar.info = ElabInfo.ElabInfo
			  and type TopdecGrammar.tycon = Environments.tycon
			  and type TopdecGrammar.funid = OpacityEnv.funid
		    structure ModuleEnvironments : MODULE_ENVIRONMENTS
		      sharing ModuleEnvironments.TyName = Environments.TyName
		          and type ModuleEnvironments.Basis = ElabInfo.TypeInfo.Basis
			  and type ModuleEnvironments.realisation = ElabInfo.TypeInfo.realisation
			  and type ModuleEnvironments.TyName = ElabInfo.TypeInfo.TyName
			    ) : OPACITY_ELIM =
  struct
    structure TyName = Environments.TyName
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure TyCon = DecGrammar.TyCon
    structure TypeInfo = ElabInfo.TypeInfo
    structure VE = Environments.VE
    structure TE = Environments.TE
    structure TypeFcn = StatObject.TypeFcn
    structure TyStr = Environments.TyStr
    structure OpacityEnv = OpacityEnv
    structure Realisation = Environments.Realisation

    fun die s = Crash.impossible ("OpacityElim." ^ s)
    fun pr_st st = PP.outputTree (print, st, 100)

    type realisation = Environments.realisation
    type opaq_env = OpacityEnv.opaq_env
    val plus = OpacityEnv.plus
    val from_rea = OpacityEnv.from_rea
    val rea_of = OpacityEnv.rea_of
    val from_funid = OpacityEnv.from_funid

    type topdec = TopdecGrammar.topdec

    infix oo
    val op oo = Realisation.oo
    val Id : realisation = Realisation.Id
    val on_Env = Realisation.on_Env

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
	of SOME type_info => ElabInfo.plus_TypeInfo elab_info (ElabInfo.TypeInfo.on_TypeInfo(rea,type_info))
	 | NONE => elab_info          (* plus_TypeInfo is destructive as we want it to be.. *)

    fun normalise_opt_type_info NONE = NONE
      | normalise_opt_type_info (SOME ti) = SOME (TypeInfo.normalise ti)

    fun elim_opt elim (rea, SOME phrase) =
      let val (phrase', rea') = elim(rea,phrase)
      in (SOME phrase', rea')
      end
      | elim_opt elim (rea, NONE) = (NONE, Id) 

    fun elim_opt' elim (rea, SOME phrase) = SOME(elim(rea,phrase))
      | elim_opt' elim (rea, NONE) = NONE

    fun elim_opt_oenv elim (oenv, SOME phrase) =
      let val (phrase', oenv') = elim(oenv,phrase)
      in (SOME phrase', oenv')
      end
      | elim_opt_oenv elim (oenv, NONE) = (NONE, OpacityEnv.empty) 


		     (* ------- *)
		     (*  Core   *)
		     (* ------- *)

   local open DecGrammar
   in
     
     fun elim_atexp(rea, atexp) =
       case atexp
	 of SCONatexp(i,scon) => (SCONatexp(on_info(rea,i),scon), Id)         
	  | IDENTatexp (i, longid_op_opt) => (IDENTatexp(on_info(rea,i), longid_op_opt), Id)
	  | RECORDatexp (i,exprowopt) => 
	   let val (exprowopt', rea') = elim_opt elim_exprow (rea,exprowopt)
	       val i' = on_info(rea,i)
	   in (RECORDatexp(i',exprowopt'), rea')
	   end
	  | LETatexp(i,dec,exp) =>
	   let val (dec', rea') = elim_dec(rea,dec)
	       val (exp', rea'') = elim_exp(rea oo rea', exp)
	       val i' = on_info(rea,i)
	   in (LETatexp(i',dec',exp'), rea' oo rea'')
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
       in (EXPROW(i',lab,exp',exprowopt'), rea' oo rea'')
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
	   in (APPexp(i',exp',atexp'), rea' oo rea'')
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
	   in (HANDLEexp(i',exp',match'), rea' oo rea'')
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
       in (MATCH(i',mrule',matchopt'), rea' oo rea'')
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
	   in (TYPEdec(i',typbind), Id)
	   end
	   | DATATYPEdec(i,datbind) => 
	   let val i' = on_info(rea,i)
	   in (DATATYPEdec(i',datbind), Id)
	   end
	   | DATATYPE_REPLICATIONdec(i,tycon,longtycon) => 
	   let val i' = on_info(rea,i)
	   in (DATATYPE_REPLICATIONdec(i',tycon,longtycon), Id)
	   end
	   | ABSTYPEdec(i,datbind,dec) =>   (* MEMO: We should translate abstype here to a local datatype *)
	   let val (dec',rea') = elim_dec(rea,dec)
	       val i' = on_info(rea,i)
	       val dummyinfo = i
	       val (TE,rea'') = case normalise_opt_type_info(ElabInfo.to_TypeInfo i')
				  of SOME(TypeInfo.ABSTYPE_INFO (TE,rea)) => (TE,rea)
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
					       in (TE.plus(TE.singleton(tycon,tystr'), TE), SOME(typbind))
					       end) (TE.empty,NONE) TE
	       val typbind = case typbindopt
			       of SOME typbind => typbind
				| NONE => die "elim_dec.ABSTYPE.typbindopt is NONE" 
	       val typedecinfo = ElabInfo.plus_TypeInfo dummyinfo (TypeInfo.TYENV_INFO TE')
	   in (LOCALdec(dummyinfo, 
			DATATYPEdec(datatypedecinfo,datbind),
			SEQdec(dummyinfo,
			       TYPEdec(typedecinfo,typbind),
			       dec')), rea' oo rea'')
	   end
	   | EXCEPTIONdec(i,exbind) => (EXCEPTIONdec(on_info(rea,i),exbind),Id)
	   | LOCALdec(i,dec1,dec2) =>
	   let val (dec1', rea') = elim_dec(rea,dec1)
	       val (dec2', rea'') = elim_dec(rea oo rea', dec2)
	       val i' = on_info(rea,i)
	   in (LOCALdec(i',dec1',dec2'), rea' oo rea'')
	   end
	   | OPENdec(i,longstridwithinfos) => 
	   let val i' = on_info(rea,i)
	       fun on (WITH_INFO(i,longstrid)) = WITH_INFO(on_info(rea,i), longstrid)  
	       val longstridwithinfos' = map on longstridwithinfos
	   in (OPENdec(i',longstridwithinfos'), Id)
	   end
	   | SEQdec(i,dec1,dec2) =>
	   let val (dec1', rea') = elim_dec(rea,dec1)
	       val (dec2', rea'') = elim_dec(rea oo rea', dec2)
	       val i' = on_info(rea,i)
	   in (SEQdec(i',dec1',dec2'), rea' oo rea'')
	   end
	   | INFIXdec _ => (dec,Id)
	   | INFIXRdec _ => (dec,Id)
	   | NONFIXdec _ => (dec,Id)
	   | EMPTYdec _ => (dec,Id)

     and elim_valbind(rea,valbind) =
       case valbind
	 of PLAINvalbind(i,pat,exp,valbindopt) =>
	   let val pat' = elim_pat(rea,pat)
	       val (exp', rea') = elim_exp(rea,exp)
	       val (valbindopt', rea'') = elim_opt elim_valbind (rea,valbindopt)
	       val i' = on_info(rea,i)
	   in (PLAINvalbind(i',pat',exp',valbindopt'), rea' oo rea'')
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
       in (FVALBIND(i',FClause',FValBindopt'), rea' oo rea'')
       end

     and elim_FClause(rea,FCLAUSE(i,atpats,tyopt,exp,FClauseopt)) =
       let val atpats' = elim_atpats(rea,atpats)
	   val (exp', rea') = elim_exp(rea, exp)
	   val (FClauseopt',rea'') = elim_opt elim_FClause (rea,FClauseopt)
	   val i' = on_info(rea,i)
       in (FCLAUSE(i',atpats',tyopt,exp',FClauseopt'), rea' oo rea'')
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

    fun elim_strexp(oenv, strexp) =
      case strexp
	of STRUCTstrexp(i, strdec) =>
	  let val (strdec', rea') = elim_strdec(oenv, strdec)
	  in (STRUCTstrexp(on_info(rea_of oenv,i), strdec'), rea')
	  end
	 | LONGSTRIDstrexp(i, longstrid) => (LONGSTRIDstrexp(on_info(rea_of oenv,i), longstrid), Id)
	 | TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (strexp', rea') = elim_strexp(oenv, strexp)
	  in (TRANSPARENT_CONSTRAINTstrexp(on_info((rea_of oenv) oo rea',i),strexp',sigexp), rea')
	  end
	 | OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (strexp', rea') = elim_strexp(oenv, strexp)
	      val (E,rea'') = case normalise_opt_type_info(ElabInfo.to_TypeInfo (on_info(rea_of oenv,i)))
				of SOME(TypeInfo.OPAQUE_CONSTRAINT_INFO (E,rea'')) => (E,rea'')
				 | _ => die "elim_strexp.OPAQUE_CONSTRAINT.no info"
	      val rea''' = rea' oo rea''
(*ME 1998-07-27
	      val _ = print "\n Opaque Constraint.\n"
	      fun pr s rea = (print ("\n" ^ s ^ " = "); pr_st (Realisation.layout rea); print "\n")
	      val _ = pr "rea'" rea'
	      val _ = pr "rea''" rea''
	      val _ = pr "rea'''" rea'''
*)
	      val i' = ElabInfo.plus_TypeInfo i (TypeInfo.TRANS_CONSTRAINT_INFO E)  
	  in (TRANSPARENT_CONSTRAINTstrexp(i',strexp',sigexp), rea''')
	  end
	 | APPstrexp(i, funid, strexp) => 
	  let val (strexp', rea_strexp) = elim_strexp(oenv, strexp)
	      val rea_0 = rea_of oenv
	      val (T, rea_funid) = 
		case OpacityEnv.lookup_funid oenv funid
		  of SOME(T,rea_funid) => (T, rea_funid)
		   | NONE => die "elim_strexp.APPstrexp: lookup funid"
	      val (rea_i, rea_g, E) = 
		case ElabInfo.to_TypeInfo i
		  of SOME(TypeInfo.FUNCTOR_APP_INFO{rea_inst,rea_gen,Env}) => (rea_inst, rea_gen, Env)
		   | _ => die "elim_strexp.APPstrexp: no info"
	      val rea_g_ = 
		case Realisation.inverse (Realisation.restrict (Realisation.dom rea_funid) rea_g)
		  of SOME rea_g_ => rea_g_
		   | NONE => die "elim_strexp.APPstrexp: cannot find inverse"
	      val rea_g1 = Realisation.restrict_from (Realisation.dom rea_funid) rea_g 
	      val rea_g2 = Realisation.renaming (TyName.Set.difference T (Realisation.dom rea_g1))
	      val rea_i' = rea_strexp oo rea_0 oo rea_i
	      val rea_g' = rea_g1 oo rea_g2
	      val rea' = rea_g2 oo rea_funid oo rea_g_ oo rea_0 oo rea_strexp
	      val E' = Realisation.on_Env rea' E
	      val i' = ElabInfo.plus_TypeInfo i (TypeInfo.FUNCTOR_APP_INFO{rea_inst=rea_i',rea_gen=rea_g',Env=E'})
(*ME 1998-07-27
	      val _ = print "\nOpacity Elimination of functor application.\n"
	      fun pr s rea = (print ("\n" ^ s ^ " = "); pr_st (Realisation.layout rea); print "\n")
	      val _ = pr "rea_0" rea_0
	      val _ = pr "rea_funid" rea_funid
	      val _ = pr "rea_strexp" rea_strexp
	      val _ = pr "rea_i" rea_i
	      val _ = pr "rea_g" rea_g
	      val _ = pr "rea_g_" rea_g_
	      val _ = pr "rea_g1" rea_g1
	      val _ = pr "rea_g2" rea_g2
	      val _ = pr "rea_i'" rea_i'
	      val _ = pr "rea_g'" rea_g'
	      val _ = pr "rea'" rea'
*)
	  in (APPstrexp(i', funid, strexp'), rea')
	  end
	 | LETstrexp(i, strdec, strexp) =>
	  let val (strdec', rea') = elim_strdec(oenv, strdec)
	      val (strexp', rea'') = elim_strexp(plus(oenv,from_rea rea'), strexp)
	  in (LETstrexp(on_info(rea_of oenv,i),strdec',strexp'), rea' oo rea'')
	  end

    and elim_strdec(oenv, strdec) =
      case strdec
	of DECstrdec(i, dec) =>
	  let val (dec', rea') = elim_dec(rea_of oenv, dec)
	  in (DECstrdec(on_info(rea_of oenv,i), dec'), rea')
	  end
	 | STRUCTUREstrdec(i, strbind) =>
	  let val (strbind', rea') = elim_strbind(oenv, strbind)
	  in (STRUCTUREstrdec(on_info(rea_of oenv,i), strbind'), rea')
	  end	  
	 | LOCALstrdec(i, strdec1, strdec2) =>
	  let val (strdec1', rea1) = elim_strdec(oenv, strdec1)
	      val (strdec2', rea2) = elim_strdec(plus(oenv,from_rea rea1), strdec2)
	  in (LOCALstrdec(on_info(rea_of oenv,i), strdec1', strdec2'), rea1 oo rea2)
	  end
	 | EMPTYstrdec i => (EMPTYstrdec (on_info(rea_of oenv,i)), Id)
	 | SEQstrdec(i, strdec1, strdec2) =>
	  let val (strdec1', rea1) = elim_strdec(oenv, strdec1)
	      val (strdec2', rea2) = elim_strdec(plus(oenv,from_rea rea1), strdec2)
	  in (SEQstrdec(on_info(rea_of oenv,i), strdec1', strdec2'), rea1 oo rea2)
	  end

    and elim_strbind (oenv, STRBIND(i, strid, strexp, strbindopt)) =
      let val (strexp', rea') = elim_strexp(oenv, strexp)
	  val (strbindopt', rea'') = elim_opt elim_strbind(oenv, strbindopt)
      in (STRBIND(on_info(rea_of oenv,i), strid, strexp', strbindopt'), rea' oo rea'')
      end 

    fun elim_funbind(oenv, FUNBIND(i, funid, strid, sigexp, strexp, funbindopt)) =
      let val rea = rea_of oenv
	  val (argE, elabB, T, resE) = 
	    case ElabInfo.to_TypeInfo i
	      of SOME(TypeInfo.FUNBIND_INFO {argE, elabB, T, resE, opaq_env_opt=NONE}) =>
		(on_Env rea argE, ModuleEnvironments.B.on rea elabB, T, on_Env rea resE)
	       | _ => die "elim_funbind.wrong type info."
	  val (strexp',rea') = elim_strexp(oenv, strexp)      
	  val resE' = on_Env rea' resE
	  val T' = TyName.Set.difference (Environments.E.tynames resE') (ModuleEnvironments.B.tynames elabB)
	  val i' = ElabInfo.plus_TypeInfo i (TypeInfo.FUNBIND_INFO {argE=argE, elabB=elabB, T=T', resE=resE', 
								    opaq_env_opt=SOME oenv})
	  val oenv' = from_funid(funid,(T',rea'))
	  val (funbindopt',oenv'') = elim_opt_oenv elim_funbind (oenv, funbindopt)
      in (FUNBIND(i',funid,strid,sigexp,strexp',funbindopt'), plus(oenv',oenv''))
      end

    fun elim_fundec(oenv, FUNCTORfundec(i,funbind)) = 
      let val (funbind', oenv') = elim_funbind(oenv, funbind)
      in (FUNCTORfundec(on_info(rea_of oenv,i),funbind'), oenv')
      end

    fun elim_topdec(oenv, topdec) =
      case topdec
	of STRtopdec(i, strdec, topdecopt) =>
	  let val (strdec', rea') = elim_strdec(oenv, strdec)
	      val (topdecopt', oenv'') = elim_opt_oenv elim_topdec (plus(oenv,from_rea rea'), topdecopt)
	  in (STRtopdec(on_info(rea_of oenv,i),strdec',topdecopt'), plus(from_rea rea',oenv''))
	  end
	 | SIGtopdec(i, sigdec, topdecopt) =>
	  let val (topdecopt', oenv') = elim_opt_oenv elim_topdec (oenv, topdecopt)
	  in (SIGtopdec(on_info(rea_of oenv,i),sigdec,topdecopt'), oenv')
	  end
	 | FUNtopdec(i, fundec, topdecopt) =>
	  let val (fundec', oenv') = elim_fundec(oenv, fundec)
	      val (topdecopt', oenv'') = elim_opt_oenv elim_topdec (plus(oenv,oenv'), topdecopt)
	  in (FUNtopdec(on_info(rea_of oenv,i),fundec',topdecopt'), plus(oenv',oenv''))
	  end

   end (* local *)

   fun opacity_elimination (oenv: opaq_env, topdec: topdec) : topdec * opaq_env =
     elim_topdec (oenv, topdec)

  end