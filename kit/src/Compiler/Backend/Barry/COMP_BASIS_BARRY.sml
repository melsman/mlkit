(* Standard ML Barifyer *)
signature COMP_BASIS_BARRY =
  sig
    type CompBasis (* CompBasis is a product of environments for the Compile part
                    * of the Kit *)

    type TyName and lvar and con and excon

    type TCEnv          (* lambda type check environment *)
    type EqEnv          (* Environment for elimination of polymorphic equality *)
    type OEnv           (* optimizer env *)

    val empty : CompBasis
    val initial : CompBasis
    val plus : CompBasis * CompBasis -> CompBasis
    val eq : CompBasis * CompBasis -> bool
    val enrich : CompBasis * CompBasis -> bool
    val match : CompBasis * CompBasis -> CompBasis
    val restrict : CompBasis * (lvar list * TyName list * con list * excon list) -> 
      CompBasis * lvar list * con list * excon list
    val mk_CompBasis: {TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv} -> CompBasis
    val de_CompBasis: CompBasis -> {TCEnv:TCEnv,EqEnv:EqEnv,OEnv:OEnv}
    type StringTree
    val layout_CompBasis: CompBasis -> StringTree

    val pu : CompBasis Pickle.pu

  end
