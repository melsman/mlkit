(* Type information for compiler *)

(*$TypeInfo : IDENT STRID TYCON CRASH STATOBJECT ENVIRONMENTS
              TYPE_INFO*)

functor TypeInfo (structure Ident: IDENT
		  structure StrId: STRID
		  structure TyCon: TYCON
		  structure PP: PRETTYPRINT
		  structure StatObject : STATOBJECT
		    sharing type PP.StringTree = StatObject.StringTree
		  structure Environments : ENVIRONMENTS
		    sharing type Environments.StringTree = PP.StringTree
		        and type Environments.realisation = StatObject.realisation
		    ) : TYPE_INFO =
  struct
    type longid = Ident.longid
    type Type = StatObject.Type
    type TyVar = StatObject.TyVar
    type TyEnv = Environments.TyEnv
    type Env = Environments.Env
    type realisation = StatObject.realisation
    type strid = StrId.strid
    type tycon = TyCon.tycon
    type id = Ident.id

    fun map_opt f (Some a) = Some (f a)
      | map_opt f None = None

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
    | EXBIND_INFO of {TypeOpt : Type Option}
    | DATBIND_INFO of {TE : TyEnv}
    | EXP_INFO of {Type:Type}
    | MATCH_INFO of {Type:Type}
    | PLAINvalbind_INFO of {tyvars: TyVar list, escaping: TyVar list, Type: Type}
    | OPEN_INFO of strid list * tycon list * id list
    | INCLUDE_INFO of strid list * tycon list
    | FUNCTOR_APP_INFO of realisation
    | FUNBIND_INFO of Env
    | TRANS_CONSTRAINT_INFO of Env
      
    fun on_TypeInfo (phi,ti) =
      let val phi_on_Type   = StatObject.Realisation.on_Type phi
	  val phi_on_TE     = Environments.Realisation.on_TyEnv phi (* fn TE => TE *)  (* I wonder if abstype works now - Martin *)
	  val phi_on_E      = Environments.Realisation.on_Env phi         (* it used to be the identity *)
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
	    | DATBIND_INFO {TE} => DATBIND_INFO {TE=phi_on_TE TE}
	    | EXP_INFO {Type} => EXP_INFO{Type=phi_on_Type Type}
	    | MATCH_INFO {Type} => MATCH_INFO{Type=phi_on_Type Type}
	    | PLAINvalbind_INFO {tyvars, escaping, Type} =>
	     PLAINvalbind_INFO {tyvars=tyvars, escaping = escaping,Type=phi_on_Type Type}
	    | OPEN_INFO i => OPEN_INFO i
	    | INCLUDE_INFO i => INCLUDE_INFO i
	    | FUNCTOR_APP_INFO phi' => FUNCTOR_APP_INFO (StatObject.Realisation.oo(phi,phi'))
            | FUNBIND_INFO E => FUNBIND_INFO (phi_on_E E)
            | TRANS_CONSTRAINT_INFO E => TRANS_CONSTRAINT_INFO(phi_on_E E)
      end

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
		  children=[PP.LEAF (Int.string index),
			    layout_tyvars tyvars,
			    layoutType Type],
		  childsep = PP.RIGHT ","}

         | RECORD_ATPAT_INFO {Type} =>
	     PP.NODE{start="RECORD_ATPAT_INFO(",finish=")",indent=2,
		  children=[layoutType Type],
		  childsep = PP.NONE}
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
		     children=[PP.LEAF("numCons: " ^ Int.string numCons),
			       PP.LEAF("index: " ^ Int.string index),
			       PP.NODE{start="tyvars: ",finish="",
				       indent=4,
				       children=[layout_tyvars tyvars],
				       childsep = PP.NONE},
			       PP.NODE{start="Type: ",finish="",
				       indent=4,
				       children=[layoutType Type],
				       childsep = PP.NONE},
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
				       childsep = PP.NONE},
			       PP.LEAF("longid: " ^ Ident.pr_longid longid)],
		       childsep = PP.NONE}
	 | EXBIND_INFO{TypeOpt} =>
	     PP.NODE{start="EXBIND_INFO(",finish=")",indent=2,
		     children=[case TypeOpt
				 of None => PP.LEAF "None"
				  | Some tau => layoutType tau],
		     childsep = PP.NONE}
	 | DATBIND_INFO{TE} =>
	     PP.NODE{start="DATBIND_INFO(",finish=")",indent=2,
		     children=[layoutTyEnv TE],
		     childsep = PP.NONE}
	 | EXP_INFO{Type} => 
	     PP.NODE{start="EXP_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NONE}
	 | MATCH_INFO{Type} => 
	     PP.NODE{start="MATCH_INFO(",finish=")",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NONE}
	 | PLAINvalbind_INFO{tyvars,escaping = [],Type} => 
	     PP.NODE{start="PLAINvalbind_INFO(",finish=")",indent=2,
		     children=[layout_tyvars tyvars,
			       layoutType Type],
		     childsep = PP.NONE}
	 | PLAINvalbind_INFO{tyvars,escaping,Type} => 
             let val t1 = PP.NODE{start  = "escaping tyvars: ", finish = "  ", indent = 2, childsep = PP.NONE,
                                  children =  [layout_tyvars escaping]}
                 val t2 = PP.NODE{start  = "type of expression: ", finish = "", indent = 2, childsep = PP.NONE,
                                  children = [layoutType Type]}
             in 
               PP.NODE{start="",finish="",indent=0,
		     children=[t1,t2],  childsep = PP.NONE}
             end
	 | OPEN_INFO (strids,tycons,ids) => PP.NODE{start="OPEN_INFO(",finish=")",indent=2,childsep=PP.RIGHT ", ",
						    children=[layout_strids strids,
							      layout_tycons tycons,
							      layout_ids ids]}
	 | INCLUDE_INFO (strids,tycons) => PP.NODE{start="INCLUDE_INFO(",finish=")",indent=2,childsep=PP.RIGHT ", ",
						   children=[layout_strids strids,
							     layout_tycons tycons]}
	 | FUNCTOR_APP_INFO realisation => PP.LEAF "FUNCTOR_APP_INFO(rea)"
	 | FUNBIND_INFO Env => PP.NODE{start="FUNBIND_INFO(", finish=")",
				       indent=2,childsep=PP.NONE,
				       children=[layoutEnv Env]}
	 | TRANS_CONSTRAINT_INFO Env => PP.NODE{start="TRANS_CONSTRAINT_INFO(", finish=")",
						indent=2,childsep=PP.NONE,
						children=[layoutEnv Env]}
  end;
