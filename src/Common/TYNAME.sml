(*$TYNAME : ORDER_FINMAP KIT_MONO_SET*)

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
    type tycon

    val freshTyName  : {tycon : tycon, arity : int, equality : bool} -> TyName
    val pr_TyName : TyName -> string

    val < : TyName * TyName -> bool
    val eq : TyName * TyName -> bool

    val arity    : TyName -> int
    val equality : TyName -> bool
    val tycon    : TyName -> tycon
    val id       : TyName -> int

    (* Names *)
    type name
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
      end

    (* Predefined type names *)
    val tyName_BOOL    : TyName
    val tyName_INT     : TyName
    val tyName_WORD    : TyName
    val tyName_WORD8   : TyName
    val tyName_REAL    : TyName
    val tyName_STRING  : TyName
    val tyName_CHAR    : TyName
    val tyName_LIST    : TyName
    val tyName_REF     : TyName
    val tyName_EXN     : TyName

    type StringTree
    val layout : TyName -> StringTree

    structure Order : ORDERING
    structure Map : ORDER_FINMAP
    structure Set : KIT_MONO_SET
    sharing type TyName = Set.elt = Map.dom = Order.T
    sharing type StringTree = Set.StringTree = Map.StringTree
  end
