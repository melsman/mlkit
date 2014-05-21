signature COMP_BASIS_GEN =
  sig
    type CompBasis (* CompBasis is a product of environments for the Compile part
                    * of the Kit *)

    type Envs      (* Instantiated with where clause for each version 
                    * of the compiler (Native, KAM, Barry, SmlToJs). *)

    type TyName and lvar and con and excon

    val empty : CompBasis
    val initial : CompBasis
    val plus : CompBasis * CompBasis -> CompBasis
    val eq : CompBasis * CompBasis -> bool
    val enrich : CompBasis * CompBasis -> bool
    val match : CompBasis * CompBasis -> CompBasis
    val restrict : CompBasis * (lvar list * TyName list * con list * excon list) -> 
      CompBasis * lvar list * TyName list * con list * excon list

   (* restrict0: Don't include identifiers that are declared by the initial basis *)
    val restrict0 : CompBasis * (lvar list * TyName list * con list * excon list) -> 
      CompBasis * lvar list * TyName list * con list * excon list

    val mk_CompBasis: Envs -> CompBasis
    val de_CompBasis: CompBasis -> Envs

    type StringTree
    val layout_CompBasis: CompBasis -> StringTree

    val pu : CompBasis Pickle.pu
  end
