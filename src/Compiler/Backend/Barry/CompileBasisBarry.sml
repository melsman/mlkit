
functor CompileBasisBarry
                    (structure CompBasisBarry : COMP_BASIS_BARRY
		     structure PP: PRETTYPRINT
		       sharing type PP.StringTree = CompBasisBarry.StringTree
		     structure Flags : FLAGS) : COMPILE_BASIS =
  struct

    fun log s = TextIO.output(TextIO.stdOut,s)
    fun say s = log s
    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    structure CompBasis = CompBasisBarry
    type CompBasis = CompBasis.CompBasis
    type lvar = CompBasis.lvar
    type con = CompBasis.con
    type excon = CompBasis.excon
    type TyName = CompBasis.TyName
    type BackendEnv = unit
    type CompileBasis = CompBasis * BackendEnv

    fun mk_CompileBasis a = a
    fun de_CompileBasis a = a

    val empty = (CompBasis.empty, ())

    val initial = (CompBasis.initial, ())

    fun plus((cb1,()),(cb2,())) =
      (CompBasis.plus(cb1,cb2), ())

    type StringTree = PP.StringTree
    fun layout_CompileBasis (cb,()) = CompBasis.layout_CompBasis cb

    fun debug(s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		      else b

    local
      fun CompBasis_enrich a = CompBasis.enrich a
    in
      fun enrich ((cb,()),(cb',())) =
	CompBasis_enrich (cb,cb')
    end

    fun match ((cb,()),(cb0,())) = 
      let val cb = CompBasis.match(cb,cb0) 
      in (cb,())
      end

    fun restrict ((cb,()),vars) = 
      let
	val (cb1, lvars, cons, excons) = CompBasis.restrict(cb,vars)
      in (cb1, ())
      end

    fun restrict0 ((cb,()),vars) = 
      let
	val (cb1, lvars, cons, excons) = CompBasis.restrict(cb,vars)
      in (cb1, ())
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

    val pu =
	Pickle.pairGen(CompBasis.pu,Pickle.unit)
  end
