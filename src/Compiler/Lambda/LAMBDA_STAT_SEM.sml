
signature LAMBDA_STAT_SEM =
  sig
    type LambdaPgm
    type env
    type lvar and TyName and con and excon

    val initial     : env
    val empty       : env
    val plus        : env * env -> env
    val enrich      : env * env -> bool
    val restrict    : env * {lvars:lvar list,
			     tynames:TyName list,
			     cons: con list,
			     excons: excon list} -> env

    val type_check  : {env: env,
		       pgm: LambdaPgm,
		       letrec_polymorphism_only: bool} -> env

    type StringTree
    val layout_env  : env -> StringTree

    val pu          : env Pickle.pu

    type tyvar and Type
    (* Generate layout constraints using precisely the ordinary typing rules.
       Hooks are scoped to this call, including exceptional exits. *)
    val constrain : {env:env, pgm:LambdaPgm,
                     unify:Type * Type -> unit, observe:Type -> unit,
                     pin:Type -> unit, switch:con list * Type -> unit} -> unit
    type LambdaExp and repvar
    (* Rewrite selected allocations bottom-up, supplying initializer types in
       their lexical environment. Types describe the original allocation. *)
    val rewrite_records : {env:env, pgm:LambdaPgm, select:repvar option -> bool,
                           rewrite:Type list * LambdaExp -> LambdaExp} -> LambdaPgm
    (* The map must preserve free type variables and quantified binders. *)
    val map_types : (Type -> Type) -> env -> env
    val look_con : env -> con -> tyvar list * Type
    val look_tyname : env -> TyName -> (con * (tyvar list * Type)) list option
  end
