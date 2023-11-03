
signature MANAGER_OBJECTS0 =
  sig
    type StringTree = PrettyPrint.StringTree

    (* Absolute project identifiers; project identifiers with absolute
       path information (e.g.,
       /home/mael/kit/ml-yacc-lib/ml-yacc-lib.pm); there is one
       exception: the project identifier basis.pm is unique---it
       cannot be redefined---and it must be referred to without a
       path. This special treatment of basis.pm makes it possible
       to relocate the distribution of the kit, with the basis library
       compiled. *)

    type absprjid = ModuleEnvironments.absprjid

    type funid = FunId.funid
    type IntFunEnv and IntBasis
    type ElabEnv = Environments.Env
    type strid = StrId.strid
    type InfixBasis = InfixBasis.Basis
    type ElabBasis = ModuleEnvironments.Basis
    type opaq_env = OpacityElim.opaq_env
    type md5 = string
    type BodyBuilderClos = {infB     : InfixBasis,
			    elabB    : ElabBasis,
			    absprjid : absprjid,
                            filetext : string,
			    filemd5  : md5,
			    opaq_env : opaq_env,
			    T        : TyName.TyName list,
			    resE     : ElabEnv}

    structure IntFunEnv :
      sig
	val empty    : IntFunEnv
	val initial  : IntFunEnv
	val plus     : IntFunEnv * IntFunEnv -> IntFunEnv
	val add      : funid * (absprjid * strid * ElabEnv * BodyBuilderClos * IntBasis) * IntFunEnv -> IntFunEnv
	val lookup   : IntFunEnv -> funid -> absprjid * strid * ElabEnv * BodyBuilderClos * IntBasis
	val restrict : IntFunEnv * funid list -> IntFunEnv
	val enrich   : IntFunEnv * IntFunEnv -> bool
	val layout   : IntFunEnv -> StringTree
	val pu       : IntFunEnv Pickle.pu
      end

    type IntSigEnv
    type sigid = SigId.sigid
    structure IntSigEnv :
      sig
	val empty    : IntSigEnv
	val initial  : IntSigEnv
	val plus     : IntSigEnv * IntSigEnv -> IntSigEnv
	val add      : sigid * TyName.Set.Set * IntSigEnv -> IntSigEnv  (* tynames that occurs free in a signature *)
	val lookup   : IntSigEnv -> sigid -> TyName.Set.Set             (* dies on failure *)
	val restrict : IntSigEnv * sigid list -> IntSigEnv
	val enrich   : IntSigEnv * IntSigEnv -> bool
	val layout   : IntSigEnv -> StringTree
	val pu       : IntSigEnv Pickle.pu
      end

    type CEnv = CompilerEnv.CEnv
    type CompileBasis  (* generic *)
    type longtycon = TyCon.longtycon
    type longid = Ident.longid
    type longstrid = StrId.longstrid

    type longids = {funids:funid list, sigids:sigid list, longstrids: longstrid list,
		    longvids: longid list, longtycons: longtycon list}

    structure IntBasis :
      sig
	val mk       : IntFunEnv * IntSigEnv * CEnv * CompileBasis -> IntBasis
	val un       : IntBasis -> IntFunEnv * IntSigEnv * CEnv * CompileBasis
	val empty    : IntBasis
	val plus     : IntBasis * IntBasis -> IntBasis
	val match    : IntBasis * IntBasis -> IntBasis
	val agree    : longstrid list * IntBasis * IntBasis -> bool   (* structure agreement *)
	val layout   : IntBasis -> StringTree
	val enrich   : IntBasis * IntBasis -> bool
	val initial  : unit -> IntBasis
	val restrict : IntBasis * longids * TyName.Set.Set -> IntBasis
	val pu       : IntBasis Pickle.pu
      end

    type Basis
    type name = Name.name

    structure Basis :
      sig
	val empty    : Basis
	val mk       : InfixBasis * ElabBasis * opaq_env * IntBasis -> Basis
	val un       : Basis -> InfixBasis * ElabBasis * opaq_env * IntBasis
	val plus     : Basis * Basis -> Basis
	val layout   : Basis -> StringTree
	val agree    : longstrid list * Basis * (Basis * TyName.Set.Set) -> bool
	val enrich   : Basis * (Basis * TyName.Set.Set) -> bool
	val eq       : Basis * Basis -> bool
 	val restrict : Basis * longids -> Basis * TyName.Set.Set
	    (* The tyname set is the set of free type names in
	     * the elaboration basis of the result *)

	val match    : Basis * Basis -> Basis

	val closure  : Basis * Basis -> Basis
	(* closure(B',B) : the closure of B w.r.t. B' - also written closure_B'(B) *)

	val initial  : unit -> Basis
	val pu       : Basis Pickle.pu

	type Basis0 = InfixBasis * ElabBasis
	val pu_Basis0     : Basis0 Pickle.pu
	val plusBasis0    : Basis0 * Basis0 -> Basis0
	val initialBasis0 : unit -> Basis0
	val matchBasis0   : Basis0 * Basis0 -> Basis0
	val eqBasis0      : Basis0 * Basis0 -> bool

	type Basis1 = opaq_env * IntBasis
	val pu_Basis1     : Basis1 Pickle.pu
	val plusBasis1    : Basis1 * Basis1 -> Basis1
	val initialBasis1 : unit -> Basis1
	val matchBasis1   : Basis1 * Basis1 -> Basis1
	val eqBasis1      : Basis1 * Basis1 -> bool
      end

    datatype 'a cval = VAR of 'a | STR of string | UNKN
    val retrieve_longid : Basis -> Ident.longid -> string cval

    type tyvar = LambdaExp.tyvar
    type Type = LambdaExp.Type
    type coninfo = string * (tyvar list * Type)
    val tyname_reps : Basis -> TyName.TyName -> coninfo list option
  end
