
functor CompileBasisDummy(structure TyName : TYNAME
		          structure PP: PRETTYPRINT) : COMPILE_BASIS =
  struct
    type lvar = unit
    type con = unit
    type excon = unit
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
    fun eq _ = true
  end
