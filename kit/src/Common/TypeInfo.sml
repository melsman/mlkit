(* Type information for compiler *)

(*$TypeInfo : LAB IDENT CRASH TYPE_INFO*)

functor TypeInfo (type lab
		 
                 structure Ident: IDENT
		 structure PP: PRETTYPRINT

		 type Type  (* Semantic object Type, TyVar and TyEnv *)
		 type TyVar
		 type TyEnv
		 val layoutType  : Type -> PP.StringTree
		 val layoutTyVar : TyVar -> PP.StringTree
		 val layoutTyEnv : TyEnv -> PP.StringTree
		) : TYPE_INFO =
  struct
    type lab = lab
    type longid = Ident.longid
    type Type = Type
    type TyVar = TyVar
    type TyEnv = TyEnv

    datatype TypeInfo = LAB_INFO of {index: int, tyvars: TyVar list, Type: Type}
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

    type StringTree = PP.StringTree
    fun layout_tyvars tyvars = 
      PP.NODE{start="[",finish="]",indent=0,childsep=PP.RIGHT",",
	      children=map layoutTyVar tyvars}

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
	     PP.NODE{start="EXBIND_INFO(",finish="",indent=2,
		     children=[case TypeOpt
				 of None => PP.LEAF "None"
				  | Some tau => layoutType tau],
		     childsep = PP.NONE}
	 | DATBIND_INFO{TE} =>
	     PP.NODE{start="DATBIND_INFO(",finish="",indent=2,
		     children=[layoutTyEnv TE],
		     childsep = PP.NONE}
	 | EXP_INFO{Type} => 
	     PP.NODE{start="EXP_INFO(",finish="",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NONE}
	 | MATCH_INFO{Type} => 
	     PP.NODE{start="MATCH_INFO(",finish="",indent=2,
		     children=[layoutType Type],
		     childsep = PP.NONE}
	 | PLAINvalbind_INFO{tyvars,escaping = [],Type} => 
	     PP.NODE{start="PLAINvalbind_INFO(",finish="",indent=2,
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
  end;
