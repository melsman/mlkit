
(* REGION_STAT_ENV: used by SpreadExpression to look up static information
   such as, for each lvar, its region type scheme and, for each type name,
   its arity
*)


signature REGION_STAT_ENV =
  sig
    type cone
    type con                            (* Unqualified value constructors. *)
    type excon                          (* Unqualified exception constructors.*)
    type TyName
    type lvar                           (* Unique lambda identifiers. *)
    type sigma and Type and mu and place and runType and effectvar
    type tyvar

    type arity
    val mk_arity : int * runType list * int -> arity
    val un_arity : arity -> int * runType list * int

    type il
    type instance_list = (il * (il * cone -> il * cone)) ref list
    type lvar_env_range =
           bool                         (* true iff type scheme is compound *)
         * bool                         (* true iff reference to lvar should create region record *)
         * RegVar.regvar list
         * sigma
         * place option
         * instance_list ref option     (* il node at applied instances of the lvars *)
         * (il -> unit) option          (* il transformer which can be used for pruning in later topdecs*)

    type regionStatEnv

    val empty          : regionStatEnv
    val initial        : regionStatEnv

    val declareTyName  : (TyName * arity * regionStatEnv) -> regionStatEnv
    val declareCon     : (con * sigma * regionStatEnv) -> regionStatEnv
    val declareExcon   : (excon * mu * regionStatEnv) -> regionStatEnv
    val declareLvar    : (lvar * lvar_env_range * regionStatEnv) -> regionStatEnv
    val declareRegVar  : RegVar.regvar * place * regionStatEnv -> regionStatEnv
    val declareTyVar   : tyvar * effectvar * regionStatEnv -> regionStatEnv

    val plus           : regionStatEnv * regionStatEnv -> regionStatEnv

    val lookupTyName   : regionStatEnv -> TyName -> arity option
    val lookupCon      : regionStatEnv -> con -> sigma option
    val lookupExcon    : regionStatEnv -> excon -> mu option
    val lookupLvar     : regionStatEnv -> lvar -> lvar_env_range option
    val lookupRegVar   : regionStatEnv -> RegVar.regvar -> place option
    val lookupTyVar    : regionStatEnv -> tyvar -> effectvar option

    val FoldExcon      : (((excon * mu) * 'a) -> 'a) -> 'a -> regionStatEnv -> 'a
    val FoldLvar       : (((lvar * lvar_env_range) * 'a) -> 'a) -> 'a  -> regionStatEnv -> 'a

    val mapLvar        : (lvar_env_range -> lvar_env_range) -> regionStatEnv -> regionStatEnv

    val restrict       : regionStatEnv * {lvars:lvar list,
                                         tynames:TyName list,
                                         cons:con list,
                                         excons:excon list} -> regionStatEnv

    val enrich         : regionStatEnv * regionStatEnv -> bool
    val places_effectvarsRSE  : regionStatEnv -> place list * effectvar list
    val places_effectvarsRSE' : regionStatEnv -> place list * effectvar list

    val mkConeToplevel : regionStatEnv -> cone

    type StringTree
    val layout         : regionStatEnv -> StringTree

    val pu             : regionStatEnv Pickle.pu

    (* Spurious type variables and related functionality *)

    val spuriousJoin   : tyvar list -> tyvar list -> tyvar list
    val spuriousTyvars : regionStatEnv -> Type -> (lvar list * excon list) -> tyvar list
  end
