(* Standard ML Barifyer *)
functor CompBasisBarry
                 (structure Con : CON 
		  structure Excon : EXCON 
		  structure Lvars : LVARS
		  structure TyName : TYNAME
		  structure LambdaStatSem : LAMBDA_STAT_SEM
		    sharing type LambdaStatSem.con = Con.con
		    sharing type LambdaStatSem.excon = Excon.excon
		  structure EliminateEq : ELIMINATE_EQ
		    sharing type EliminateEq.lvar = LambdaStatSem.lvar = Lvars.lvar
		    sharing type EliminateEq.TyName = LambdaStatSem.TyName = TyName.TyName
		  structure OptLambda : OPT_LAMBDA
		    sharing type OptLambda.lvar = LambdaStatSem.lvar
                    sharing type OptLambda.con = Con.con
                    sharing type OptLambda.TyName = TyName.TyName
		  structure PP: PRETTYPRINT
		    sharing type PP.StringTree
				       = OptLambda.StringTree
				       = LambdaStatSem.StringTree
				       = EliminateEq.StringTree
		  structure Flags : FLAGS
			  ): COMP_BASIS_BARRY =
  struct

    fun log s = TextIO.output(TextIO.stdOut,s)
    fun say s = log s
    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    val quotation = Flags.is_on0 "quotation"

    type lvar = LambdaStatSem.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = EliminateEq.TyName
    type TCEnv = LambdaStatSem.env
    type EqEnv = EliminateEq.env
    type OEnv = OptLambda.env
    type CompBasis = {TCEnv : TCEnv, (* lambda type check environment *)
		      EqEnv : EqEnv, (* elimination of polymorphic equality environment *)
		      OEnv: OEnv}

    fun mk_CompBasis a = a
    fun de_CompBasis a = a

    val empty = {TCEnv=LambdaStatSem.empty,
		 EqEnv=EliminateEq.empty,
		 OEnv=OptLambda.empty}

    val initial = {TCEnv=LambdaStatSem.initial,
		   EqEnv=EliminateEq.initial,
		   OEnv=OptLambda.initial}

    fun plus({TCEnv,EqEnv,OEnv},
	     {TCEnv=TCEnv',EqEnv=EqEnv',OEnv=OEnv'}) =
      {TCEnv=LambdaStatSem.plus(TCEnv,TCEnv'),
       EqEnv=EliminateEq.plus(EqEnv,EqEnv'),
       OEnv=OptLambda.plus(OEnv,OEnv')}

    type StringTree = PP.StringTree
    fun layout_CompBasis {TCEnv,EqEnv,OEnv} =
      PP.NODE{start="{", finish="}", indent=1, childsep=PP.RIGHT "; ",
              children=[EliminateEq.layout_env EqEnv,
			OptLambda.layout_env OEnv,
			LambdaStatSem.layout_env TCEnv
                       ]
             }

    fun debug(s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		      else b

    local
      fun EliminateEq_enrich a = EliminateEq.enrich a
      fun LambdaStatSem_enrich a = LambdaStatSem.enrich a
      fun OptLambda_enrich a = OptLambda.enrich a
    in
      fun enrich ({TCEnv,EqEnv,OEnv},
		  {TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1}) =
	debug("EqEnv", EliminateEq_enrich(EqEnv,EqEnv1)) andalso 
	debug("TCEnv", LambdaStatSem_enrich(TCEnv,TCEnv1)) andalso
	debug("OEnv", OptLambda_enrich(OEnv,OEnv1))
    end

    fun match ({TCEnv,EqEnv,OEnv},
	       {TCEnv=TCEnv0,EqEnv=EqEnv0,OEnv=OEnv0}) = 
      let val EqEnv = EliminateEq.match(EqEnv,EqEnv0) 
      in {TCEnv=TCEnv,EqEnv=EqEnv,OEnv=OEnv}
      end

    fun restrict ({EqEnv,OEnv,TCEnv},
		  (lvars,tynames,cons,excons)) = 
      let
	
	(* Martin Elsman wants to write a comment here 09/09/1997
	 * 11:28. tho. 
	 *
	 * Ok, here it comes: There are some exception constructors,
	 * constructors and type names that cannot be derived from
	 * the free identifiers of the source program but need
	 * to be declared in the environments. These are identifiers
	 * stemming from derived forms, unexhaustive matches and
	 * equality elimination. This is why we patch the derived
	 * identifiers below.  Martin-18/03/1998 *)

	  val excons = Excon.ex_DIV :: 
	        Excon.ex_MATCH :: Excon.ex_BIND :: excons
	  val cons = Con.con_NIL :: Con.con_CONS ::
	      Con.con_TRUE :: Con.con_FALSE :: cons   (* for elim eq *)
	  val cons = if quotation() then Con.con_QUOTE :: Con.con_ANTIQUOTE :: cons
                     else cons
	  val tynames = TyName.tyName_LIST :: 
              TyName.tyName_BOOL ::
	      TyName.tyName_VECTOR :: tynames     (* for elim eq *) 
          val tynames = if quotation() then TyName.tyName_FRAG :: tynames
                        else tynames
	  val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames}) handle x =>
               (say "CompileBasis.restrict: ElimiateEq.restrict failed\n";
                say "Then equality environment is:\n";
                PP.outputTree(say,  EliminateEq.layout_env EqEnv, 70);
                say "(end of equality environment)\n";
                raise x) 
	  val lvars = lvars_eq @ lvars
	  val (OEnv1,cons,tynames) = OptLambda.restrict(OEnv,lvars,cons,tynames)
	  val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
      in ({TCEnv=TCEnv1,
	   EqEnv=EqEnv1,
	   OEnv=OEnv1}, lvars, cons, excons)
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

  end
