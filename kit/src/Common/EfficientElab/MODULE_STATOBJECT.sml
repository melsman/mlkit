(*Sigma in Sig and Phi in FunSig, Definition 1997, fig. 11,
 p. 29*)

(*$MODULE_STATOBJECT : TYNAME*)
signature MODULE_STATOBJECT =
  sig
    (*types provided by this module:*)
    type Sig
    type FunSig

    (*types from other modules:*)
    structure TyName : TYNAME
    type TyName sharing type TyName = TyName.TyName

    type TyVar
    type Type
    type TypeScheme
    type TypeFcn
    type Env
    type realisation

    eqtype id
    type strid
    type longstrid
    type longtycon

    type StringTree sharing type StringTree = TyName.Set.StringTree

    datatype SigMatchError =
      MISSINGSTR  of longstrid
    | MISSINGTYPE of longtycon
    | S_CONFLICTINGARITY of longtycon * (TyName * TypeFcn)
    | CONFLICTINGEQUALITY of longtycon * (TyName * TypeFcn)
    | MISSINGVAR of strid list * id
    | MISSINGEXC of strid list * id
    | S_RIGIDTYCLASH of longtycon
    | S_CONFLICTING_DOMCE of longtycon
    | NOTYENRICHMENT of {qualid: strid list * id, 
			 str_sigma : TypeScheme, str_vce: string,
			 sig_sigma : TypeScheme, sig_vce: string}
    | EXCNOTEQUAL of strid list * id * (Type * Type)

    exception No_match of SigMatchError
    (*raised by Sigma.match and Phi.match when matching fails*)




			    (*signature*)

    structure Sigma :
      sig
	val triv                   : Sig
	val to_T_and_E             : Sig -> TyName.Set.Set * Env
	val from_T_and_E           : TyName.Set.Set * Env -> Sig
	val tynames                : Sig -> TyName.Set.Set
	val tyvars                 : Sig -> TyVar list
	val tyvars'                : Sig -> (id * TyVar list) list
	(*tyvars': see comment for E.tyvars' (in ENVIRONMENTS)*)
	val bogus                  : Sig
	val layout                 : Sig -> StringTree

	(*instance Sigma = the E from a signature where the bound
	 tynames have been replaced by fresh tynames:*)

	val instance               : Sig -> Env
	val instance'              : Sig -> TyName.Set.Set * Env

	(*If E' = match (Sigma, E) succeeds, then E matches Sigma, i.e.,
	 E' is an instance of Sigma and E enriches E'.
	 match will raise No_match if there is no match*)

	val match                  : Sig * Env -> Env
	val match'                 : Sig * Env -> Env * Env * realisation
	  (* for opaque signature matching *)

	val eq : Sig * Sig -> bool
      end


                         (*functor signature*)

    structure Phi : 
      sig
	val from_T_and_E_and_Sigma : TyName.Set.Set * Env * Sig -> FunSig
	val to_T_and_E_and_Sigma   : FunSig -> TyName.Set.Set * Env * Sig
	val tynames                : FunSig -> TyName.Set.Set
	val tyvars                 : FunSig -> TyVar list
	val tyvars'                : FunSig -> (id * TyVar list) list
	(*tyvars': see comment for E.tyvars' (in ENVIRONMENTS)*)
	val layout                 : FunSig -> StringTree

	(*If Sigma'= match (Phi, E) succeeds, then E matches the
	 argument signature of the functor signature Phi and Sigma' is 
	 the signature of the actual result of the functor application.
	 match will raise No_match if there is no match:*)

	val match                  : FunSig * Env -> Sig
	val match_via              : FunSig * Env -> Sig * realisation

	val eq : FunSig * FunSig -> bool
      end
  end;
