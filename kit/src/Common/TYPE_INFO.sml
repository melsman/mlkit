(*TypeInfo is part of the ElabInfo.  See ELAB_INFO for an
 overview of the different kinds of info.*)

(*$TYPE_INFO*)
signature TYPE_INFO =
  sig
    type longid
    type Type
    type TyVar
    type TyEnv
    type Env
    type realisation
    type strid and tycon and id

    (*
     * Note that we record tyvars and types (and not typeschemes as 
     * one could imagine); this is not accidentally: we don't 
     * want to risk that the bound type variables are renamed (by alpha-conversion) ---
     * the compiler is a bit picky on the exact type information, so alpha-conversion
     * is not allowed on recorded type information!
     *)

    datatype TypeInfo =
	LAB_INFO of {index: int, tyvars: TyVar list, Type : Type }
			(* Attached to PATROW. Gives the alphabetic
			   index (0..n-1) for the record label. 
			   The Type field is the type of the pattern
			   corresponding to the label, tyvars are the bound 
                           type variables; there will only be bound tyvars
			   when attached to a pattern in a valbind. *)

      | RECORD_ATPAT_INFO of {Type : Type}
	                (* Attachec to RECORDatpat during elaboration,
			   The type (which is a record type) is used when 
			   overloading is resolved
			   to insert the correct indeces in LAB_INFO of patrows.
			 *)

      | VAR_INFO of {instances : Type list}
	                (* Attached to IDENTatexp,
			   instances is the list of types which have been 
			   chosen to instantiate the generic tyvars at this 
			   variable.
			 *)
      | VAR_PAT_INFO of {tyvars: TyVar list, Type: Type}
	                (* Attached to LAYEREDpat and LONGIDatpat (for LONGVARs)
			   The Type field is the type of the pattern corresponding
			   to the variable, tyvars are the bound type variables;
			   there will only be bound tyvars when attached to a pattern
			   in a valbind. *)
      | CON_INFO of {numCons: int, index: int, instances: Type list,
		     tyvars : TyVar list, Type: Type,longid:longid}
			(* Attached to IDENTatexp, LONGIDatpat, CONSpat.
			   numCons is the number of constructors for this type.
			   instances is the list of types wich have been
			   chosen to instantiate the generic tyars at this 
			   occurrence of the constructor.
			   Type is the type of the occurrence of the constructor,
			   tyvars are the bound type variables; 
			   there will only be bound tyvars when 
			   attached to a pattern in a valbind
			 *)
      | EXCON_INFO of {Type: Type,longid:longid}
			(* Attached to IDENTatexp, LONGIDatpat, CONSpat.
			   The Type field is the type of the occurrence of the
			   excon. *)
      | EXBIND_INFO of {TypeOpt: Type Option}
	                (* Attached to EXBIND
			 * None if nullary exception constructor *)
      | TYENV_INFO of TyEnv
	                (* Attached to DATATYPEdec, TYPEdec, DATATYPE_REPLICATIONdec and ABSTYPEdec
			 * The type environment associated with the declaration *)
      | EXP_INFO of {Type: Type} 
	                (* Attached to all exp's *)
      | MATCH_INFO of {Type: Type}
	                (* Attached to MATCH *)
      | PLAINvalbind_INFO of {tyvars: TyVar list, escaping: TyVar list, Type: Type}
	                (* Attached to PLAINvalbind 
			   for 'pat = exp' this is the type of the exp, and 
			   a list of bound type variables. *)
      | OPEN_INFO of strid list * tycon list * id list
	                (* Attached to OPENdec; the lists contains those
			 * identifiers being declared by the dec. *)
      | INCLUDE_INFO of strid list * tycon list
	                (* Attached to INCLUDEspec; the lists contains those
			 * strids and tycons being specified by the spec. *)
      | FUNCTOR_APP_INFO of realisation * Env
                        (* Attached to functor applications; The env is the
			 * elaboration result of the functor application. *)
      | FUNBIND_INFO of Env
                        (* Attached to functor bindings; the env is the environment
			 * resulting from elaborating the sigexp in a functor 
			 * binding. *)
      | TRANS_CONSTRAINT_INFO of Env
	                (* Attached to transparent signature constraints *)

    val on_TypeInfo : realisation * TypeInfo -> TypeInfo

    type StringTree
    val layout : TypeInfo -> StringTree
  end;
