(* Constructors for the lambda language *)

(*$CON *)

signature CON = 
  sig 

    (* Constructors are based on names which may be `matched'. In
     * particular, if two constructors, c1 and c2, are successfully
     * matched, eq(c1,c2) = true. This may affect the canonical
     * ordering of constructors. *)

    type con

    val mk_con : string  -> con
    val pr_con : con -> string

    val < : con * con -> bool
    val eq : con * con -> bool

    (* Names *)
    type name
    val match : con * con -> unit
    val name : con -> name

    (* Predefined Constructors *)
    val con_REF   : con
    val con_TRUE  : con
    val con_FALSE : con
    val con_NIL   : con
    val con_CONS  : con

  end