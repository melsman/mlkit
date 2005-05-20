
signature COMPILE_BASIS =
  sig

    (* CompileBasis is a product of all environments in the backend of
     * the compiler. *)

    type CompileBasis 
    type TyName = TyName.TyName
    type lvar = Lvars.lvar
    type con = Con.con
    type excon = Excon.excon
    type CompBasis
    type BackendEnv     (* Closure Conversion Enviroment, e.g *)

    val empty : CompileBasis
    val initial : CompileBasis
    val plus : CompileBasis * CompileBasis -> CompileBasis

    val eq : CompileBasis * CompileBasis -> bool
    val enrich : CompileBasis * CompileBasis -> bool

    val match : CompileBasis * CompileBasis -> CompileBasis
    val restrict : CompileBasis * (lvar list * TyName list * con list * excon list) -> CompileBasis

    (* restrict0: Don't include identifiers that are declared by the initial basis *)
    val restrict0 : CompileBasis * (lvar list * TyName list * con list * excon list) -> CompileBasis

    val mk_CompileBasis: CompBasis * BackendEnv -> CompileBasis

    val de_CompileBasis: CompileBasis -> CompBasis * BackendEnv

    type StringTree = PrettyPrint.StringTree
    val layout_CompileBasis: CompileBasis -> StringTree

    val pu : CompileBasis Pickle.pu
  end
