
structure CompileBasisDummy: COMPILE_BASIS =
  struct
    structure PP = PrettyPrint
    type lvar = Lvars.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = TyName.TyName
    type CompileBasis = unit
    type CompBasis = unit
    type BackendEnv = unit

    fun mk_CompileBasis ((),()) = ()
    fun de_CompileBasis () = ((),())
    val empty = ()
    val initial = empty
    fun plus _ = empty

    type StringTree = PP.StringTree
    fun layout_CompileBasis _ = PP.LEAF ""
    fun enrich _ = true
    fun match _ = empty
    fun restrict _ = empty
    fun restrict0 _ = empty
    fun eq _ = true
	
    val pu = Pickle.unit
  end
