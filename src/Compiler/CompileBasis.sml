
functor CompileBasis(structure ClosExp : CLOS_EXP
		       where type con = CompBasis.con
		       where type excon = CompBasis.excon
		       where type lvar = CompBasis.lvar
		       where type StringTree = PrettyPrint.StringTree) 
    : COMPILE_BASIS =
  struct
    structure PP = PrettyPrint

    fun log s = TextIO.output(TextIO.stdOut,s)
    fun say s = log s
    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    type CompBasis = CompBasis.CompBasis
    type lvar = CompBasis.lvar
    type con = CompBasis.con
    type excon = CompBasis.excon
    type TyName = CompBasis.TyName
    type BackendEnv = ClosExp.env
    type CompileBasis = CompBasis * BackendEnv

    fun mk_CompileBasis a = a
    fun de_CompileBasis a = a

    val empty = (CompBasis.empty, ClosExp.empty)

    val initial = (CompBasis.initial,ClosExp.init)

    fun plus((cb1,ce1),(cb2,ce2)) =
      (CompBasis.plus(cb1,cb2), ClosExp.plus(ce1,ce2))

    type StringTree = PP.StringTree
    fun layout_CompileBasis (cb,ce) =
      PP.NODE{start="(", finish=")", indent=1, childsep=PP.RIGHT "; ",
              children=[CompBasis.layout_CompBasis cb,
			ClosExp.layout_env ce
                       ]
             }

    fun debug(s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		      else b

    local
      fun CompBasis_enrich a = CompBasis.enrich a
      fun ClosExp_enrich a = ClosExp.enrich a
    in
      fun enrich ((cb, ce),(cb',ce')) =
	CompBasis_enrich (cb,cb') andalso
	debug("clos_env", ClosExp_enrich(ce,ce'))
    end

    fun match ((cb,ce),(cb0,ce0)) = 
      let val cb = CompBasis.match(cb,cb0) 
	  val _ = ClosExp.match(ce,ce0)
      in (cb,ce)
      end

    fun restrict ((cb,ce),vars) = 
      let
	val (cb1, lvars, cons, excons) = CompBasis.restrict(cb,vars)
	val ce1 = ClosExp.restrict(ce,{lvars=lvars,excons=excons,cons=cons})
      in (cb1, ce1)
      end

    fun restrict0 ((cb,ce),vars) = 
      let (* Don't include identifiers that are declared by the initial basis *)
	val (cb1, lvars, cons, excons) = CompBasis.restrict0(cb,vars)
	val ce1 = ClosExp.restrict0(ce,{lvars=lvars,excons=excons,cons=cons})
      in (cb1, ce1)
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

    val pu =
	Pickle.pairGen(CompBasis.pu, 
		       Pickle.comment "ClosExp.pu" ClosExp.pu)

  end
