(*B in Basis, F in FunEnv, and G in SigEnv, Definition 1997,
fig. 11, p. 29*)

(*$MODULE_ENVIRONMENTS : TYNAME*)
signature MODULE_ENVIRONMENTS =
  sig
    (*types provided by this module:*)
    type Basis  
    type FunEnv 
    type SigEnv 

    (*types from other modules:*)
    structure TyName : TYNAME
    type TyName = TyName.TyName
    type TyVar
    type TyStr
    type Env
    type Sig
    type FunSig
    type Context
    type realisation
    eqtype id
    type strid
    type tycon = TyName.tycon
    type longstrid
    type longtycon
    type sigid
    type funid
    type StringTree = TyName.Set.StringTree
    type Report
    (*The report functions below are used for top-level printout*)


                       (*G, signature environments*)

    structure G :
      sig
	val empty            : SigEnv
	val singleton        : sigid  * Sig    -> SigEnv
	val plus             : SigEnv * SigEnv -> SigEnv
	val lookup           : SigEnv -> sigid -> Sig option
	val tynames          : SigEnv -> TyName.Set.Set
	val dom              : SigEnv -> sigid Edlib.EqSet.Set
	val layout           : SigEnv -> StringTree
	val report           : (sigid * Sig -> Report) * SigEnv -> Report
      end


                       (*F, functor environments*)

    structure F :
      sig
	val empty            : FunEnv
	val singleton        : funid  * FunSig -> FunEnv
	val plus             : FunEnv * FunEnv -> FunEnv
	val lookup           : FunEnv -> funid -> FunSig option
	val tynames          : FunEnv -> TyName.Set.Set
	val tyvars           : FunEnv -> TyVar list
	val tyvars'          : FunEnv -> (id * TyVar list) list
	val dom              : FunEnv -> funid Edlib.EqSet.Set
	val layout           : FunEnv -> StringTree
	val report           : (funid * FunSig -> Report) * FunEnv -> Report
      end


                          (*B, static basis*)

    structure B :
      sig
	val empty            : Basis
	val initial          : Basis
	val bogus            : Basis
	val plus             : Basis * Basis   -> Basis
	val on               : realisation -> Basis -> Basis
	val tyvars           : Basis -> TyVar list
	val tynames          : Basis -> TyName.Set.Set
	val to_C             : Basis -> Context
	val layout           : Basis -> StringTree
	val tyvars'          : Basis -> (id * TyVar list) list

                     	(*E component*)
	val plus_E           : Basis * Env     -> Basis
	val from_E           : Env -> Basis
	val to_E             : Basis -> Env
	val lookup_strid     : Basis -> strid -> Env option
	val lookup_longstrid : Basis -> longstrid -> Env option
	val lookup_longtycon : Basis -> longtycon -> TyStr option

	                (*G component*)
	val from_G           : SigEnv -> Basis
	val to_G             : Basis -> SigEnv
	val plus_G           : Basis  * SigEnv -> Basis
	val lookup_sigid  : Basis -> sigid -> Sig option

	                (*F component*)
	val from_F           : FunEnv -> Basis
	val to_F             : Basis -> FunEnv
	val plus_F           : Basis  * FunEnv -> Basis
	val lookup_funid     : Basis -> funid -> FunSig option

	           (*for compilation manager*)
	val enrich           : Basis * Basis -> bool
	val restrict         : Basis * {ids : id list, tycons : tycon list,
					strids : strid list, funids : funid list,
					sigids : sigid list} -> Basis
	val match            : Basis * Basis -> unit
      end
  end;
