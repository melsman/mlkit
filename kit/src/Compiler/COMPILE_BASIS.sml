
signature COMPILE_BASIS =
  sig

    (* CompileBasis is a product of all environments in the backend of
     * the compiler. *)

    type CompileBasis and TyName and lvar and con and excon

    type TCEnv          (* lambda type check environment *)
    type EqEnv          (* Environment for elimination of polymorphic equality *)
    type OEnv           (* optimizer env *)
    type mulenv         (* for multiplicity inference *)
    type mularefmap     (* for multiplicity inference *)
    type rse            (* region static environment *)
    type drop_env       (* lvar(fix) -> bool list *) 
    type psi_env        (* lvar -> phsize list  minimal actual ph. sizes for FIX bound lvars *)
    type clos_env       (* Closure Conversion Enviroment *)

    val empty : CompileBasis
    val initial : CompileBasis
    val plus : CompileBasis * CompileBasis -> CompileBasis

    val eq : CompileBasis * CompileBasis -> bool
    val enrich : CompileBasis * CompileBasis -> bool

    val match : CompileBasis * CompileBasis -> CompileBasis
    val restrict : CompileBasis * (lvar list * lvar list * TyName list * con list * excon list) -> CompileBasis

    val mk_CompileBasis: {TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv,rse:rse,mulenv:mulenv,mularefmap:mularefmap,
			  drop_env:drop_env,psi_env:psi_env,clos_env:clos_env} -> CompileBasis

    val de_CompileBasis: CompileBasis -> {TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv,rse:rse,mulenv:mulenv,
					  mularefmap:mularefmap,drop_env:drop_env,psi_env:psi_env,clos_env:clos_env}

    type StringTree
    val layout_CompileBasis: CompileBasis -> StringTree

  end;
