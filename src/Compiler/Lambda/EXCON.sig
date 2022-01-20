(* Exception constructors for the lambda language *)

signature EXCON =
  sig

    (* Exception constructors are based on names which may be
     * `matched'. In particular, if two exception constructors, exc1
     * and exc2, are successfully matched, eq(exc1,exc2) = true. This
     * may affect the canonical ordering of exception constructors. *)

    type excon

    val mk_excon : string -> excon
    val pr_excon : excon -> string
    val pr_excon' : excon -> string  (* prints key *)

    val renew : excon -> excon

    val < : excon * excon -> bool
    val eq : excon * excon -> bool

    (* Names *)
    type name
    val match : excon * excon -> unit
    val name : excon -> name

    (* Predefined exception constructors *)
    val ex_DIV       : excon
    val ex_MATCH     : excon
    val ex_BIND      : excon
    val ex_OVERFLOW  : excon
    val ex_INTERRUPT : excon
    val ex_SUBSCRIPT : excon
    val ex_SIZE      : excon

    val exconsPredefined : excon list

    val pu : excon Pickle.pu

    structure Map : MONO_FINMAP where type dom = excon
  end
