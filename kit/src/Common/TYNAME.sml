(* Type names *)

(*$TYNAME : ORDER_FINMAP KIT_MONO_SET*)

signature TYNAME =
  sig 

    (* Type names are based on names which may be `matched'. In
     * particular, if two type names, n1 and n2, are successfully
     * matched, eq(n1,n2) = true. This may affect the canonical
     * ordering of type names. *)

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
