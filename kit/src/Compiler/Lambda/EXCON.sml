(* Exception constructors for the lambda language *)

(*$EXCON *)

signature EXCON =
  sig 

    (* Exception constructors are based on names which may be
     * `matched'. In particular, if two exception constructors, exc1
     * and exc2, are successfully matched, eq(exc1,exc2) = true. This
     * may affect the canonical ordering of exception constructors. *)

    type excon

    val mk_excon : string -> excon
    val pr_excon : excon -> string

    val < : excon * excon -> bool
    val eq : excon * excon -> bool

    (* Names *)
    type name
    val match : excon * excon -> unit
    val name : excon -> name

    (* Predefined exception constructors *)
    val ex_ABS   : excon
    val ex_NEG   : excon
    val ex_SUM   : excon
    val ex_DIFF  : excon
    val ex_PROD  : excon
    val ex_MATCH : excon
    val ex_BIND  : excon

  end
