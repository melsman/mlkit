(* Type information for compiler *)

functor TypeInfo (structure Crash: CRASH
                  structure Ident: IDENT
		  structure StrId: STRID
		  structure TyCon: TYCON
		  structure PP: PRETTYPRINT
		  structure OpacityEnv : OPACITY_ENV
		  structure StatObject : STATOBJECT
		    sharing type PP.StringTree = StatObject.StringTree
		  structure Environments : ENVIRONMENTS
		    sharing type Environments.StringTree = PP.StringTree
		    sharing type Environments.realisation = StatObject.realisation
		  structure ModuleEnvironments : MODULE_ENVIRONMENTS
		    sharing type ModuleEnvironments.StringTree = PP.StringTree
		    ) : TYPE_INFO =
  struct

    fun die s = Crash.impossible ("TypeInfo." ^ s)

    type longid = Ident.longid
    type Type = StatObject.Type
    type TyVar = StatObject.TyVar
    type TyEnv = Environments.TyEnv
    type Env = Environments.Env
    type realisation = StatObject.realisation
    type opaq_env = OpacityEnv.opaq_env
    type strid = StrId.strid
    type tycon = TyCon.tycon
    type id = Ident.id
    type Basis = ModuleEnvironments.Basis
    structure TyName = StatObject.TyName
    type TyName = TyName.TyName

    fun map_opt f (SOME a) = SOME (f a)
      | map_opt f NONE = NONE

    val layoutType = StatObject.Type.layout
    val layoutTyVar = StatObject.TyVar.layout
    val layoutEnv = Environments.E.layout
    val layoutTyEnv = Environments.E.layout o Environments.E.from_TE

    datatype TypeInfo =
      LAB_INFO of {index: int, tyvars: TyVar list, Type: Type}
    | RECORD_ATPAT_INFO of {Type : Type}
    | VAR_INFO of {instances: Type list}
    | VAR_PAT_INFO of {tyvars: TyVar list, Type: Type}
    | CON_INFO of {numCons: int, index: int, instances: Type list,
		   tyvars: TyVar list, Type: Type,longid:longid}
    | EXCON_INFO of {Type:Type,longid:longid}
    | EXBIND_INFO of {TypeOpt : Type option}
    | TYENV_INFO of TyEnv
    | ABSTYPE_INFO of TyEnv * realisation
    | EXP_INFO of {Type:Type}
    | MATCH_INFO of {Type:Type}
    | PLAINvalbind_INFO of {tyvars: TyVar list, Type: Type}
    | OPEN_INFO of strid list * tycon list * id list
    | INCLUDE_INFO of strid list * tycon list
    | FUNCTOR_APP_INFO of {rea_inst : realisation, rea_gen : realisation, Env : Env}
    | FUNBIND_INFO of {argE: Env,elabBref: Basis ref, T: TyName.Set.Set, resE: Env, opaq_env_opt: opaq_env option}
    | TRANS_CONSTRAINT_INFO of Env
    | OPAQUE_CONSTRAINT_INFO of Env * realisation
    | DELAYED_REALISATION of realisation * TypeInfo

    fun on_TypeInfo' (phi,ti) =
      let val phi_on_Type   = StatObject.Realisation.on_Type phi
	  val phi_on_TE     = Environments.Realisation.on_TyEnv phi (* fn TE => TE *)  (* I wonder if abstype works now - Martin *)
	  val phi_on_E      = Environments.Realisation.on_Env phi         (* it used to be the identity *)
	  val phi_on_phi' = fn phi' => StatObject.Realisation.oo(phi,phi')
      in case ti 
	   of LAB_INFO {index, tyvars, Type} => LAB_INFO {index=index,tyvars=tyvars,
							  Type=phi_on_Type Type}
	    | RECORD_ATPAT_INFO{Type} => RECORD_ATPAT_INFO{Type=phi_on_Type Type}
	    | VAR_INFO {instances} => VAR_INFO {instances = map phi_on_Type instances}
	    | VAR_PAT_INFO {tyvars,Type} => VAR_PAT_INFO{tyvars=tyvars,Type=phi_on_Type Type}
	    | CON_INFO {numCons, index, instances, tyvars, Type,longid} =>
	     CON_INFO {numCons=numCons,index=index,
		       instances=map phi_on_Type instances,
		       tyvars=tyvars,Type=phi_on_Type Type,
		       longid=longid}
	    | EXCON_INFO {Type,longid} => EXCON_INFO {Type=phi_on_Type Type, longid=longid}
	    | EXBIND_INFO {TypeOpt} => EXBIND_INFO {TypeOpt = map_opt phi_on_Type TypeOpt}
	    | TYENV_INFO TE => TYENV_INFO (phi_on_TE TE)
	    | ABSTYPE_INFO (TE,phi') => ABSTYPE_INFO (phi_on_TE TE, phi_on_phi' phi')
	    | EXP_INFO {Type} => EXP_INFO{Type=phi_on_Type Type}
	    | MATCH_INFO {Type} => MATCH_INFO{Type=phi_on_Type Type}
	    | PLAINvalbind_INFO {tyvars, Type} =>
	     PLAINvalbind_INFO {tyvars=tyvars, Type=phi_on_Type Type}
	    | OPEN_INFO i => OPEN_INFO i
	    | INCLUDE_INFO i => INCLUDE_INFO i
	    | FUNCTOR_APP_INFO {rea_inst,rea_gen,Env} => 
	     FUNCTOR_APP_INFO {rea_inst=phi_on_phi' rea_inst, rea_gen=phi_on_phi' rea_gen, Env=phi_on_E Env}
            | FUNBIND_INFO {argE,elabBref,T,resE,opaq_env_opt} => die "on_TypeInfo': FUNBIND_INFO"
(*	     FUNBIND_INFO {argE=phi_on_E argE,elabB=elabB,T=T,resE=resE,rea_opt=SOME phi} *)
            | TRANS_CONSTRAINT_INFO E => TRANS_CONSTRAINT_INFO (phi_on_E E)
            | OPAQUE_CONSTRAINT_INFO (E,phi') => OPAQUE_CONSTRAINT_INFO (phi_on_E E, phi_on_phi' phi')
(*	    | DELAYED_REALISATION (phi',ti) => on_TypeInfo'(phi_on_phi' phi', ti)  *)
	    | DELAYED_REALISATION (phi',ti) => on_TypeInfo'(phi, on_TypeInfo'(phi', ti))
      end

    fun on_TypeInfo a = DELAYED_REALISATION a
    fun normalise (DELAYED_REALISATION(phi,ti)) = on_TypeInfo'(phi, ti)
      | normalise ti = ti 

    type StringTree = PP.StringTree
    fun layout_tyvars tyvars = 
      PP.NODE{start="[",finish="]",indent=0,childsep=PP.RIGHT",",
	      children=map layoutTyVar tyvars}

    fun layout_list (s, lay_elem) elems = 
      PP.NODE{start=s ^ " = [",finish="]",indent=0,childsep=PP.RIGHT",",
	      children=map lay_elem elems}

    val layout_strids = layout_list ("strids", PP.LEAF o StrId.pr_StrId)
    val layout_tycons = layout_list ("tycons", PP.LEAF o TyCon.pr_TyCon)
    val layout_ids = layout_list ("ids", PP.LEAF o Ident.pr_id)

    fun layout info =
      case info
	of LAB_INFO{index,tyvars,Type} => 
	  PP.NODE{start="LAB_INFO(", finish=")",indent=2,
		  children=[PP.LEAF (Int.toString index),
			    layout_tyvars tyvars,
			    layoutType Type],
		  childsep = PP.RIGHT ","}

         | RECORD_ATPAT_INFO {Type} =>
	     PP.NODE{start="RECORD_ATPAT_INFO(",finish=")",indent=2,
		  children=[layoutType Type],
		  childsep = PP.NOSEP}
         | VAR_INFO {instances} => 
	     PP.NODE{start="VAR_INFO(", finish=")",indent=2,
		     children=map layoutType instances,
		     childsep = PP.RIGHT ","}
	 | VAR_PAT_INFO {tyvars, Type} =>
	     PP.NODE{start="VAR_PAT_INFO(",finish=")",indent=2,
		     children=[layout_tyvars tyvars,
			       layoutType Type],
		     childsep=PP.RIGHT ","}
	 | CON_INFO{numCons, index, tyvars,Type,longid,instances} =>
	     PP.NODE{start="CON_INFO(",  finish=")", indent=2,
		     children=[PP.LEAF("numCons: " ^ Int.toString numCons),
			       PP.LEAF("index: " ^ Int.toString index),
			       PP.NODE{start="tyvars: ",finish="",
				       indent=4,
				       children=[layout_tyvars tyvars],
				       childsep = PP.NOSEP},
			       PP.NODE{start="Type: ",finish="",
				       indent=4,
				       children=[layoutType Type],
				       childsep = PP.NOSEP},
			       PP.NODE{start="instances: ",finish="",
				       indent=4,
				       children=map layoutType instances,
				       childsep = PP.RIGHT ","},
			       PP.LEAF("longid: " ^ Ident.pr_longid longid)
			      ],
		     childsep=PP.RIGHT ", "
		    }

         | EXCON_INFO{Type,longid} =>
	     PP.NODE{start="EXCON_INFO(",finish=")",indent=2,
		     children=[PP.NODE{start="Type: ",finish="",
				       indent=4,
				       children=[layoutType Type],
				       childsep = PP.NOSEP},
			       PP.LEAF("longid: " ^ Ident.pr_longid longid)],
		       childsep = PP.NOSEP}
	 | EXBIND_INFO{TypeOpt} =>
	     PP.NODE{start="EXBIND_INFO(",finish=")",indent=2,
		     children=[case TypeOpt
				 of NONE => PP.LEAF "NONE"
				  | SOME tau => layoutType tau],
		     childsep = PP.NOSEP}
	 | TYENV_INFO TE =>
	     PP.NODE{start="TYENV_INFO(",finish=")",indent=2,
		     children=[layoutTyEnv TE],
		     childsep = PP.NOSEP}
	 | ABSTYPE_INFO (TE,phi) =>
	     PP.NODE{start="ABSTYPE_INFO(",finish=")",indent=2,
		     children=[layoutTyEnv TE, PP.LEAF "phi"],
		     childsep = PP.RIGHT ", "}
	 | EXP_INFO{Type} => 
	     PP.NODE{start="EXP_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NOSEP}
	 | MATCH_INFO{Type} => 
	     PP.NODE{start="MATCH_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NOSEP}
	 | PLAINvalbind_INFO{tyvars, Type} => 
	     PP.NODE{start="PLAINvalbind_INFO(",finish=")",indent=2,
		     children=[layout_tyvars tyvars,
			       layoutType Type],
		     childsep = PP.NOSEP}
	 | OPEN_INFO (strids,tycons,ids) => PP.NODE{start="OPEN_INFO(",finish=")",indent=2,childsep=PP.RIGHT ", ",
						    children=[layout_strids strids,
							      layout_tycons tycons,
							      layout_ids ids]}
	 | INCLUDE_INFO (strids,tycons) => PP.NODE{start="INCLUDE_INFO(",finish=")",indent=2,childsep=PP.RIGHT ", ",
						   children=[layout_strids strids,
							     layout_tycons tycons]}
	 | FUNCTOR_APP_INFO {rea_inst,rea_gen,Env} => PP.LEAF "FUNCTOR_APP_INFO{rea_inst,rea_gen,Env}"
	 | FUNBIND_INFO {argE,elabBref,T,resE,opaq_env_opt} => PP.NODE{start="FUNBIND_INFO(", finish=")",
								    indent=2,childsep=PP.NOSEP,
								    children=[layoutEnv argE]}
	 | TRANS_CONSTRAINT_INFO Env => PP.NODE{start="TRANS_CONSTRAINT_INFO(", finish=")",
						indent=2,childsep=PP.NOSEP,
						children=[layoutEnv Env]}
	 | OPAQUE_CONSTRAINT_INFO (Env,phi) => PP.NODE{start="OPAQUE_CONSTRAINT_INFO(", finish=")",
						indent=2,childsep=PP.RIGHT ", ",
						children=[layoutEnv Env, PP.LEAF "phi"]}
         | DELAYED_REALISATION(phi,ti) => layout (on_TypeInfo'(phi,ti))
  end;
