(* Type names - Definition (Rev) page 15 *)

signature TYNAME =
  sig

    (* Type names are based on names which may be `matched'. In
     * particular, if two type names, n1 and n2, are successfully
     * matched, eq(n1,n2) = true. This may affect the canonical
     * ordering of type names. *)

    (* Type names and type variables have associated to them a
     * rank. This rank is used during elaboration to enforce that no
     * type variable is unified with a type containing type names
     * created `after' the type variable was created. During
     * elaboration there is a notion of a `current rank'. Type
     * variables and type names are created with the current
     * rank. Further, when a type name is created (with current rank)
     * the current rank is increased. The rank of a type name is then
     * fixed during elaboration. The rank of a type is the maximum
     * rank of all type variables and type names occuring in the
     * type. When a type variable is unified with a type it is checked
     * if the rank of the type is smaller than the rank of the type
     * variable. If not, unification fails. It is allowed to decrease
     * the rank of a type variable during elaboration to make
     * unification succeed.  After elaboration of a top-level
     * declaration all type names are decreased in rank to rank zero
     * and the current rank is decreased to rank zero.
     *)

    type TyName
    type tycon = TyCon.tycon

    val freshTyName  : {tycon : tycon, arity : int, equality : bool} -> TyName
    val pr_TyName : TyName -> string

    val eq : TyName * TyName -> bool

    val arity    : TyName -> int
    val equality : TyName -> bool
    val tycon    : TyName -> tycon
    val id       : TyName -> int * string (* the string is the base (i.e., the defining program unit) *)

    (* Names *)
    type name = Name.name
    val match : TyName * TyName -> unit
    val name : TyName -> name

    structure Rank :
      sig
	type rank
	val current : unit -> rank
	val reset : unit -> unit
	val <= : rank * rank -> bool
	val min : rank * rank -> rank
	val from_TyName : TyName -> rank
	val pu : rank Pickle.pu
	val pu_rankrefOne : rank ref Pickle.pu
        val pp : rank -> string
      end

    (* Predefined type names *)
    val tyName_BOOL    : TyName
    val tyName_INT31   : TyName
    val tyName_INT32   : TyName
    val tyName_INT63   : TyName
    val tyName_INT64   : TyName
    val tyName_INTINF  : TyName
    val tyName_IntDefault : unit -> TyName   (* int31 or int32 dependent on tagging *)
    val tyName_WORD8   : TyName
    val tyName_WORD31   : TyName
    val tyName_WORD32  : TyName
    val tyName_WORD63   : TyName
    val tyName_WORD64  : TyName
    val tyName_WordDefault : unit -> TyName  (* word31 or word32 dependent on tagging *)
    val tyName_REAL    : TyName
    val tyName_F64     : TyName              (* Internal unboxed float type *)
    val tyName_F256    : TyName
    val tyName_STRING  : TyName              (* Internal unboxed float vector type *)
    val tyName_CHAR    : TyName
    val tyName_LIST    : TyName
    val tyName_FRAG    : TyName
    val tyName_REF     : TyName
    val tyName_ARRAY   : TyName
    val tyName_VECTOR  : TyName
    val tyName_CHARARRAY : TyName
    val tyName_FOREIGNPTR : TyName
    val tyName_EXN     : TyName

    val unboxed : TyName -> bool   (* Returns true for type names that are
				    * implemented unboxed; depends on whether
				    * tagging of integers is enabled. *)
    val setUnboxed : TyName -> unit (* After calling setUnboxed(t), unboxed(t)
				     * returns true. *)

    val tynamesPredefined : TyName list

    type StringTree = PrettyPrint.StringTree
    val layout : TyName -> StringTree

    val pu : TyName Pickle.pu

    structure Map : MONO_FINMAP
                        where type StringTree = StringTree
                          and type dom = TyName
    structure Set : KIT_MONO_SET
                        where type StringTree = StringTree
                          and type elt = TyName
  end
