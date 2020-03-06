
(* REGION_STAT_ENV: used by SpreadExpression to look up static information
   such as, for each lvar, its region type scheme and, for each type name,
   its arity
*)


signature REGION_STAT_ENV =
  sig
    type cone
    type regionStatEnv
    type con                            (* Unqualified value constructors. *)
    type excon				(* Unqualified exception constructors.*)
    type TyName
    type lvar				(* Unique lambda identifiers. *)
    type TypeAndPlaceScheme and Type and place and runType and effectvar
    type arity

    val mk_arity: int * runType list * int -> arity
    val un_arity: arity -> int * runType list * int

    type il

    val empty: regionStatEnv
    val initial: regionStatEnv

    val declareTyName: (TyName * arity * regionStatEnv) -> regionStatEnv
    val declareCon: (con * TypeAndPlaceScheme * regionStatEnv) -> regionStatEnv
    val declareExcon: (excon * (Type * place) * regionStatEnv) -> regionStatEnv
    val declareLvar : (lvar
                       * (  bool  (* true iff type scheme is compound *)
                          * bool  (* true iff reference to lvar should create region record *)
                          * RegVar.regvar list
                          * TypeAndPlaceScheme
                          * place
                          * (il * (il * cone -> il * cone)) ref list ref option (* il node at applied instances of the lvars *)
                          * (il->unit) option) (* il transformer which can be used for pruning in later topdecs*)
                       * regionStatEnv) -> regionStatEnv
    val declareRegVar : RegVar.regvar * place * regionStatEnv -> regionStatEnv

    val plus: regionStatEnv * regionStatEnv -> regionStatEnv

    val lookupTyName : regionStatEnv -> TyName -> arity option
    val lookupCon : regionStatEnv -> con -> TypeAndPlaceScheme option
    val lookupExcon: regionStatEnv -> excon -> (Type * place) option
    val lookupLvar : regionStatEnv -> lvar ->
                     (  bool
                      * bool
                      * RegVar.regvar list
                      * TypeAndPlaceScheme
                      * place
                      * (il * (il * cone -> il * cone)) ref list ref option  (* il node at applied instances of the lvars *)
                      * (il -> unit)option  (* il transformer which can be used for pruning in later topdecs*)
                     ) option

    val lookupRegVar : regionStatEnv -> RegVar.regvar -> place option

    val FoldExcon: (((excon * (Type * place)) * 'a) -> 'a) -> 'a -> regionStatEnv -> 'a
    val FoldLvar : (((lvar * (bool * bool * RegVar.regvar list * TypeAndPlaceScheme
                      * place
                      * (il * (il * cone -> il * cone))ref list ref option
                      * (il -> unit)option
                    )) * 'a) -> 'a) -> 'a  -> regionStatEnv -> 'a


    val mapLvar : ((bool*bool*RegVar.regvar list*TypeAndPlaceScheme*place*(il * (il * cone -> il * cone))ref list ref option * (il->unit)option) ->
                   (bool*bool*RegVar.regvar list*TypeAndPlaceScheme*place*(il * (il * cone -> il * cone))ref list ref option * (il->unit)option))
        	-> regionStatEnv -> regionStatEnv

    val restrict : regionStatEnv * {lvars:lvar list,
				    tynames:TyName list,
				    cons:con list,
				    excons:excon list} -> regionStatEnv

    val enrich : regionStatEnv * regionStatEnv -> bool
    val places_effectvarsRSE : regionStatEnv -> place list * effectvar list


    val mkConeToplevel: regionStatEnv -> cone

    type StringTree
    val layout : regionStatEnv -> StringTree

    val pu : regionStatEnv Pickle.pu
  end
