
signature COMPILE_BASIS =
  sig

    (* CompileBasis is a product of all environments in the backend of
     * the compiler. *)

    type CompileBasis and TyName and lvar and con and excon
    type CompBasis
    type BackendEnv     (* Closure Conversion Enviroment, e.g *)

    val empty : CompileBasis
    val initial : CompileBasis
    val plus : CompileBasis * CompileBasis -> CompileBasis

    val eq : CompileBasis * CompileBasis -> bool
    val enrich : CompileBasis * CompileBasis -> bool

    val match : CompileBasis * CompileBasis -> CompileBasis
    val restrict : CompileBasis * (lvar list * lvar list * TyName list * con list * excon list) -> CompileBasis

    val mk_CompileBasis: CompBasis * BackendEnv -> CompileBasis

    val de_CompileBasis: CompileBasis -> CompBasis * BackendEnv

    type StringTree
    val layout_CompileBasis: CompileBasis -> StringTree

  end
