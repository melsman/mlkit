(*$MANAGER_OBJECTS: TYNAME *)

signature MANAGER_OBJECTS =
  sig
    type modcode and target and linkinfo and StringTree

    type prjid = string

    structure TyName : TYNAME

    structure SystemTools :
      sig
	val delete_file : string -> unit
      end

    structure ModCode :
      sig
	val empty : modcode
	val seq : modcode * modcode -> modcode
	val mk_modcode : target * linkinfo * string -> modcode   (* Use emit or mk_exe to actually emit code.
								  * The string is a unit name. *)
	val exist : modcode -> bool
	val emit : prjid * modcode -> modcode       
	val mk_exe : prjid * modcode * string -> unit   (* produces executable `string' in target directory *)
      end
    
    type filename = string
    type funstamp and funid
    structure FunStamp :
      sig
	val new : funid -> funstamp
	val from_filemodtime : filename -> funstamp option
	val modTime : funstamp -> Time.time option
	val eq : funstamp * funstamp -> bool
      end

    val funid_from_filename : filename -> funid
    val funid_to_filename : funid -> filename

    type IntFunEnv and IntBasis and ElabEnv and strexp and strid

    structure IntFunEnv :
      sig
	val empty : IntFunEnv
	val initial : IntFunEnv
	val plus : IntFunEnv * IntFunEnv -> IntFunEnv
	val add : funid * (prjid * funstamp * strid * ElabEnv * (unit -> strexp) * IntBasis) * IntFunEnv -> IntFunEnv
	val lookup : IntFunEnv -> funid -> prjid * funstamp * strid * ElabEnv * (unit -> strexp) * IntBasis  
	val restrict : IntFunEnv * funid list -> IntFunEnv
	val enrich : IntFunEnv * IntFunEnv -> bool  (* using funstamps *)
	val layout : IntFunEnv -> StringTree
      end

    type CEnv and CompileBasis and id and tycon
    structure IntBasis :
      sig
	val mk : IntFunEnv * CEnv * CompileBasis -> IntBasis
	val un : IntBasis -> IntFunEnv * CEnv * CompileBasis
	val empty : IntBasis
	val initial : IntBasis
	val plus : IntBasis * IntBasis -> IntBasis
	val restrict : IntBasis * (funid list * strid list * id list * tycon list) -> IntBasis
	val match : IntBasis * IntBasis -> IntBasis
	val enrich : IntBasis * IntBasis -> bool
	val layout : IntBasis -> StringTree
      end

    type Basis and InfixBasis and ElabBasis and realisation and sigid
    structure Basis :
      sig
	val initial : Basis
	val empty : Basis
	val mk : InfixBasis * ElabBasis * realisation * IntBasis -> Basis
	val un : Basis -> InfixBasis * ElabBasis * realisation * IntBasis
	val plus : Basis * Basis -> Basis
	val enrich : Basis * (Basis * TyName.Set.Set) -> bool
	val layout : Basis -> StringTree
      end

    type name
    structure Repository :
      sig

	(* Repositories map pairs of a project identifier and a
           functor identifier to a repository object. Thus, a functor
           identifier (and hence source file names) can only be
           declared once in each project. However, different functors
           with the same functor identifier may co-exist in different
           projects (similarly, for source file names). *)

	val clear : unit -> unit
	val delete_entries : prjid * funid -> unit

	  (* Repository lookup's return the first entry for a (prjid,funid)
	   * which is reusable (i.e. where all export (ty-)names are
	   * marked generative.) In particular, this means that an
	   * entry which has been added, cannot be returned by a
	   * lookup, prior to executing `recover().' The integer
	   * provided by the lookup functions can be given to the
	   * overwrite functions for owerwriting a particular
	   * entry. *)

	  (* The elaboration environment in the interpretation
	   * repository is supposed to be the elaboration result of
	   * the functor application/ unit. This is used when checking
	   * if reuse is allowed. *)

	val lookup_elab : (prjid * funid) -> (int * (InfixBasis * ElabBasis * (realisation * TyName.Set.Set) * name list * 
						     InfixBasis * ElabBasis * realisation)) option
	val lookup_int : (prjid * funid) -> (int * (funstamp * ElabEnv * IntBasis * name list * modcode * IntBasis)) option
	  
	val add_elab : (prjid * funid) * (InfixBasis * ElabBasis * (realisation * TyName.Set.Set) * name list * 
					  InfixBasis * ElabBasis * realisation) -> unit
	val add_int : (prjid * funid) * (funstamp * ElabEnv * IntBasis * name list * modcode * IntBasis) -> unit

	val owr_elab : (prjid * funid) * int * (InfixBasis * ElabBasis * (realisation * TyName.Set.Set) * name list * 
						InfixBasis * ElabBasis * realisation) -> unit
	val owr_int : (prjid * funid) * int * (funstamp * ElabEnv * IntBasis * name list * modcode * IntBasis) -> unit

	val emitted_files : unit -> string list   (* returns the emitted files mentioned in the repository; *)
                                                  (* used for deleting files which are no longer mentioned. *)
	val recover : unit -> unit

          (* Before building a project the repository should be
	   * ``recovered'' meaning that all export names are marked
	   * generative (see NAME). Then when an entry is reused,
	   * export names are marked non-generative (for an entry to
	   * be reused all export names must be marked generative.) *)

      end
    
  end