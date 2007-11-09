(* CompBasis for compilation from LambToLamb *)
structure CompBasisToLamb 
  : COMP_BASIS_GEN 
        where type Envs = {TCEnv : LambdaStatSem.env,
		           EqEnv : EliminateEq.env,
		           OEnv: OptLambda.env,
                           NEnv: LambdaBasics.Normalize.env} =
  struct
    structure PP = PrettyPrint

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
    type NEnv = LambdaBasics.Normalize.env
    type CompBasis = {TCEnv : TCEnv, (* lambda type check environment *)
		      EqEnv : EqEnv, (* elimination of polymorphic equality environment *)
		      OEnv: OEnv,
                      NEnv: NEnv}
    type Envs = CompBasis

    fun mk_CompBasis a = a
    fun de_CompBasis a = a

    val empty = {TCEnv=LambdaStatSem.empty,
		 EqEnv=EliminateEq.empty,
		 OEnv=OptLambda.empty,
                 NEnv=LambdaBasics.Normalize.empty}

    val initial = {TCEnv=LambdaStatSem.initial,
		   EqEnv=EliminateEq.initial,
		   OEnv=OptLambda.initial,
                   NEnv=LambdaBasics.Normalize.initial}

    fun plus({TCEnv,EqEnv,OEnv,NEnv},
	     {TCEnv=TCEnv',EqEnv=EqEnv',OEnv=OEnv',NEnv=NEnv'}) =
      {TCEnv=LambdaStatSem.plus(TCEnv,TCEnv'),
       EqEnv=EliminateEq.plus(EqEnv,EqEnv'),
       OEnv=OptLambda.plus(OEnv,OEnv'),
       NEnv=LambdaBasics.Normalize.plus(NEnv,NEnv')}

    type StringTree = PP.StringTree
    fun layout_CompBasis {TCEnv,EqEnv,OEnv,NEnv} =
      PP.NODE{start="{", finish="}", indent=1, childsep=PP.RIGHT "; ",
              children=[EliminateEq.layout_env EqEnv,
			OptLambda.layout_env OEnv,
			LambdaStatSem.layout_env TCEnv,
                        LambdaBasics.Normalize.layout NEnv
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
      fun NEnv_enrich a = LambdaBasics.Normalize.enrich a
    in
      fun enrich ({TCEnv,EqEnv,OEnv,NEnv},
		  {TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,NEnv=NEnv1}) =
	debug("EqEnv", EliminateEq_enrich(EqEnv,EqEnv1)) andalso 
	debug("TCEnv", LambdaStatSem_enrich(TCEnv,TCEnv1)) andalso
	debug("OEnv", OptLambda_enrich(OEnv,OEnv1)) andalso
	debug("NEnv", NEnv_enrich(NEnv,NEnv1))
    end

    fun match ({TCEnv,EqEnv,OEnv,NEnv},
	       {TCEnv=TCEnv0,EqEnv=EqEnv0,OEnv=OEnv0,NEnv=NEnv0}) = 
      let val EqEnv = EliminateEq.match(EqEnv,EqEnv0) 
      in {TCEnv=TCEnv,EqEnv=EqEnv,OEnv=OEnv,NEnv=NEnv}
      end

    fun restrict ({EqEnv,OEnv,TCEnv,NEnv},
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
	      Con.con_TRUE :: Con.con_FALSE :: Con.con_INTINF :: cons   (* for elim eq *)
	  val cons = if quotation() then Con.con_QUOTE :: Con.con_ANTIQUOTE :: cons
                     else cons
	  val tynames = TyName.tyName_LIST :: TyName.tyName_INTINF ::
              TyName.tyName_BOOL ::
	      TyName.tyName_VECTOR ::               (* for elim eq *) 
              TyName.tyName_CHARARRAY :: tynames    (* for inlining primitives *)
          val tynames = if quotation() then TyName.tyName_FRAG :: tynames
                        else tynames

          val NEnv1 = LambdaBasics.Normalize.restrict(NEnv,lvars)
              handle x =>
               (say "CompBasisToLamb.restrict: Normalize.restrict failed\n";
                say "Normalize environment is:\n";
                PP.outputTree(say,  LambdaBasics.Normalize.layout NEnv, 100);
                say "(end of normalize environment)\n";
                raise x) 

	  val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames}) handle x =>
               (say "CompBasisToLamb.restrict: ElimiateEq.restrict failed\n";
                say "Equality environment is:\n";
                PP.outputTree(say,  EliminateEq.layout_env EqEnv, 100);
                say "(end of equality environment)\n";
                raise x) 
	  val lvars = lvars_eq @ lvars
	  val (OEnv1,cons,tynames) = OptLambda.restrict(OEnv,lvars,cons,tynames)
	  val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
      in ({NEnv=NEnv1,
           TCEnv=TCEnv1,
	   EqEnv=EqEnv1,
	   OEnv=OEnv1}, lvars, tynames, cons, excons)
      end

    fun subtractPredefinedCons cons = 
	let fun eq a b = Con.eq(a,b)
	    fun fromList l = Set.fromList eq l
	in Set.list
	    (Set.difference eq (fromList cons) (fromList Con.consPredefined))
	end

    fun subtractPredefinedExcons excons = 
	let fun eq a b = Excon.eq(a,b)
	    fun fromList l = Set.fromList eq l
	in Set.list
	    (Set.difference eq (fromList excons) (fromList Excon.exconsPredefined))
	end
    
    fun subtractPredefinedTynames tns = 
	TyName.Set.list
	(TyName.Set.difference (TyName.Set.fromList tns) (TyName.Set.fromList TyName.tynamesPredefined))

    fun restrict0 ({EqEnv,OEnv,TCEnv,NEnv},
		  (lvars,tynames,cons,excons)) = 
      let
	  (* Don't include identifiers that are declared by the initial basis *)
	  
	  val tynames = subtractPredefinedTynames tynames
	  val cons = subtractPredefinedCons cons
	  val excons = subtractPredefinedExcons excons
          val NEnv1 = LambdaBasics.Normalize.restrict(NEnv,lvars)
	  val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames})
	  val lvars = lvars_eq @ lvars
	  val (OEnv1,cons,tynames) = OptLambda.restrict(OEnv,lvars,cons,tynames)
	  val tynames = subtractPredefinedTynames tynames
	  val cons = subtractPredefinedCons cons
	  val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
      in ({NEnv=NEnv1,
           TCEnv=TCEnv1,
	   EqEnv=EqEnv1,
	   OEnv=OEnv1}, lvars, tynames, cons, excons)
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

    val pu =
	let fun to (tce,eqe,(oe,ne)) = {TCEnv=tce, EqEnv=eqe, OEnv=oe, NEnv=ne}
	    fun from {TCEnv=tce, EqEnv=eqe, OEnv=oe, NEnv=ne} = (tce,eqe,(oe,ne))
	in Pickle.convert (to,from)
	    (Pickle.tup3Gen0(LambdaStatSem.pu,EliminateEq.pu,Pickle.pairGen0(OptLambda.pu,LambdaBasics.Normalize.pu)))
	end    
  end
