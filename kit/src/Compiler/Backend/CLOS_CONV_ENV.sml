
signature CLOS_CONV_ENV =
  sig
    type env
    type lvar
    type place
    type con
    type excon
    type offset = int
    type phsize
    type label

    datatype con_kind =     (* the integer is the index in the datatype 0,... *)
        ENUM of int
      | UB_NULLARY of int
      | UB_UNARY of int
      | B_NULLARY of int
      | B_UNARY of int

    datatype arity_excon =
        NULLARY_EXCON
      | UNARY_EXCON

    datatype access_type =
        LVAR of lvar                            (* Variable                                  *)
      | RVAR of place                           (* Region variable                           *)
      | DROPPED_RVAR of place                   (* Dropped region variable                   *)
      | SELECT of lvar * int                    (* Select from closure or region vector      *)
      | LABEL of label                          (* Global declared variable                  *)
      | FIX of label * access_type option * int (* Label is code pointer, access_type is the *)
                                                (* shared closure and int is size of closure *)
                 * (place * phsize) list        (* for region profiling graph *)

    datatype rho_kind =
        FF (* Rho is formal and finite *)
      | FI (* Rho is formal and infinite *)
      | LF (* Rho is letregion bound and finite *)
      | LI (* Rho is letregion bound and infinite *)

    val empty : env
    val plus  : env * env -> env
    val initialEnv : env

    val declareCon     : con * con_kind * env -> env
    val declareLvar    : lvar * access_type * env -> env
    val declareExcon   : excon * (access_type*arity_excon) * env -> env
    val declareRho     : place * access_type * env -> env
    val declareRhoKind : place * rho_kind * env -> env

    val lookupCon        : env -> con -> con_kind
    val lookupVar        : env -> lvar -> access_type
    val lookupVarOpt     : env -> lvar -> access_type option
    val lookupExcon      : env -> excon -> access_type
    val lookupExconOpt   : env -> excon -> access_type option
    val lookupExconArity : env -> excon -> arity_excon
    val lookupRho        : env -> place -> access_type
    val lookupRhoOpt     : env -> place -> access_type option
    val lookupRhoKind    : env -> place -> rho_kind

    val enrich : env * env -> bool
    val match : env * env -> unit
    val restrict : env * {lvars:lvar list,
			  cons:con list,
			  excons:excon list} -> env

    val labelsEnv : (label list * label list -> access_type -> label list * label list) 
      -> env -> label list * label list

    val pu : env Pickle.pu

    type StringTree
    val layoutEnv : env -> StringTree
    val pr_access_type : access_type -> string
  end;
