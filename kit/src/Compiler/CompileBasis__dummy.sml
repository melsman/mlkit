
functor CompileBasisDummy(structure TyName : TYNAME
		          structure PP: PRETTYPRINT) 
  : COMPILE_BASIS =
  struct

    type lvar = unit
    type con = unit
    type excon = unit
    type TyName = TyName.TyName
    type TCEnv = unit
    type EqEnv = unit
    type OEnv = unit
    type rse = unit
    type mulenv  = unit
    type mularefmap = unit
    type drop_env = unit
    type psi_env = unit
    type clos_env = unit
    type CompileBasis = unit
(*
      {TCEnv : TCEnv, (* lambda type check environment *)
       EqEnv : EqEnv, (* elimination of polymorphic equality environment *)
       OEnv: OEnv, 
       rse: rse, 
       mulenv: mulenv,
       mularefmap: mularefmap,
       drop_env: drop_env,
       psi_env: psi_env, 
       l2kam_ce: l2kam_ce,
       clos_env: clos_env}
*)

    fun mk_CompileBasis _ = ()
    fun de_CompileBasis () = {TCEnv=(), EqEnv=(), OEnv=(),
			     rse=(), mulenv=(), mularefmap=(), drop_env=(),
			     psi_env=(), clos_env=()}


    val empty = ()
(*
      {TCEnv=(),
       EqEnv=(),
       OEnv=(),
       rse=(),
       mulenv=(),
       mularefmap=(),
       drop_env=(),
       psi_env=(),
       l2kam_ce=(),
       clos_env=()}
*)

    val initial = empty

    fun plus _ = empty

    type StringTree = PP.StringTree
    fun layout_CompileBasis _ = PP.LEAF ""
    fun enrich _ = true
    fun match _ = empty
    fun restrict _ = empty
    fun eq _ = true
  end
