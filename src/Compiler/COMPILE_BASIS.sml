(*$COMPILE_BASIS*)

signature COMPILE_BASIS =
  sig

    type CompileBasis and ids and TyName

    type CEnv           (* compiler env: var->lvar *)
    type TCEnv          (* lambda type check environment *)
    type EqEnv          (* Environment for elimination of polymorphic equality *)
    type OEnv           (* optimizer env *)
    type mulenv         (* for multiplicity inference *)
    type mularefmap     (* for multiplicity inference *)
    type rse            (* region static environment *)
    type drop_env       (* lvar(fix) -> bool list *) 
    type psi_env        (* lvar -> phsize list  minimal actual ph. sizes for FIX bound lvars *)
    type l2kam_ce       (* lambda to kam environment *)

    val empty : CompileBasis
    val initial : CompileBasis
    val plus : CompileBasis * CompileBasis -> CompileBasis

    val eq : CompileBasis * CompileBasis -> bool
    val enrich : CompileBasis * CompileBasis -> bool
    val match : CompileBasis * CompileBasis -> CompileBasis
    val restrict : CompileBasis * ids * TyName list -> CompileBasis

    val mk_CompileBasis: {CEnv:CEnv,TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv,rse:rse,mulenv:mulenv,
			  mularefmap:mularefmap,drop_env:drop_env,psi_env:psi_env,l2kam_ce:l2kam_ce} -> CompileBasis

    val de_CompileBasis: CompileBasis -> {CEnv:CEnv,TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv,rse:rse,mulenv:mulenv,
					  mularefmap:mularefmap,drop_env:drop_env,psi_env:psi_env,l2kam_ce:l2kam_ce}
    type StringTree
    val layout_CompileBasis: CompileBasis -> StringTree

  end;
