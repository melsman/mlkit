
structure CompileBasisJS : COMPILE_BASIS =
  struct
    structure PP = PrettyPrint

    fun log s = TextIO.output(TextIO.stdOut,s)
    fun say s = log s
    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    structure CompBasis = CompBasisToLamb
    type CompBasis = CompBasis.CompBasis * ExpToJs.Env.t
    type lvar = CompBasis.lvar
    type con = CompBasis.con
    type excon = CompBasis.excon
    type TyName = CompBasis.TyName
    type BackendEnv = unit
    type CompileBasis = CompBasis * BackendEnv

    fun mk_CompileBasis a = a
    fun de_CompileBasis a = a

    val empty = ((CompBasis.empty, ExpToJs.Env.empty),())

    val initial = ((CompBasis.initial, ExpToJs.Env.initial),())

    fun plus (((cb1,jse1),()),((cb2,jse2),())) =
        ((CompBasis.plus(cb1,cb2), ExpToJs.Env.plus(jse1,jse2)),())

    type StringTree = PP.StringTree
    fun layout_CompileBasis ((cb,_),()) = CompBasis.layout_CompBasis cb

    fun debug (s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		       else b

    local
      fun CompBasis_enrich a = CompBasis.enrich a
    in
      fun enrich (((cb,jse),()),((cb',jse'),())) =
	CompBasis_enrich (cb,cb') andalso ExpToJs.Env.enrich(jse,jse')
    end

    fun match (((cb,jse),()),((cb0,_),())) =
      let val cb = CompBasis.match(cb,cb0)
      in ((cb,jse),())
      end

    fun restrict (((cb,jse),()),vars) =
      let
	val (cb1, lvars, tns, cons, excons) = CompBasis.restrict(cb,vars)
        val jse1 = ExpToJs.Env.restrict(jse,cons)
      in ((cb1, jse1),())
      end

    fun restrict0 (((cb,jse),()),vars) =
      let
	val (cb1, lvars, tns, cons, excons) = CompBasis.restrict0(cb,vars)
        val jse1 = ExpToJs.Env.restrict(jse,cons)
      in ((cb1, jse1),())
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

    val pu =
	Pickle.pairGen(Pickle.pairGen(CompBasis.pu,ExpToJs.Env.pu),Pickle.unit)
  end
