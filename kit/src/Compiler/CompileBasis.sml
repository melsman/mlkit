
functor CompileBasis(structure Con : CON 
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
		     structure RegionStatEnv: REGION_STAT_ENV
		       sharing type RegionStatEnv.lvar = LambdaStatSem.lvar
		       sharing type RegionStatEnv.excon = Excon.excon
		       sharing type RegionStatEnv.con = Con.con
		       sharing type RegionStatEnv.TyName = TyName.TyName
		     structure Mul: MUL
		       sharing type Mul.lvar = LambdaStatSem.lvar
		       sharing type Mul.effectvar = RegionStatEnv.effectvar
		       sharing type Mul.regionStatEnv = RegionStatEnv.regionStatEnv
		     structure Effect: EFFECT
		       sharing type Effect.effect = RegionStatEnv.place = RegionStatEnv.effectvar
		     structure DropRegions: DROP_REGIONS
		       sharing type DropRegions.lvar = LambdaStatSem.lvar
		       sharing type DropRegions.place = RegionStatEnv.place
		     structure PhysSizeInf : PHYS_SIZE_INF
		       sharing type PhysSizeInf.lvar = LambdaStatSem.lvar
		     structure CompLamb : COMP_LAMB
		       sharing type CompLamb.lvar = LambdaStatSem.lvar
		       sharing type CompLamb.con = Con.con
		       sharing type CompLamb.excon = Excon.excon
		       sharing type CompLamb.place = RegionStatEnv.place
		     structure PP: PRETTYPRINT
		       sharing type PP.StringTree
			                  = OptLambda.StringTree
			                  = LambdaStatSem.StringTree
			                  = EliminateEq.StringTree
                                          = RegionStatEnv.StringTree
			                  = PhysSizeInf.StringTree
                                          = Mul.StringTree
			                  = CompLamb.StringTree
			                  = DropRegions.StringTree 
		     structure Flags : FLAGS
			     ): COMPILE_BASIS =
  struct

    fun log s = TextIO.output(TextIO.stdOut,s)

    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    type lvar = LambdaStatSem.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = EliminateEq.TyName
    type TCEnv = LambdaStatSem.env
    type EqEnv = EliminateEq.env
    type OEnv = OptLambda.env
    type rse = RegionStatEnv.regionStatEnv
    type mulenv  = Mul.efenv
    type mularefmap = Mul.mularefmap
    type drop_env = DropRegions.env
    type psi_env = PhysSizeInf.env
    type l2kam_ce = CompLamb.env
    type CompileBasis = {TCEnv : TCEnv, (* lambda type check environment *)
			 EqEnv : EqEnv, (* elimination of polymorphic equality environment *)
			 OEnv: OEnv, 
			 rse: rse, 
			 mulenv: mulenv,
			 mularefmap: mularefmap,
			 drop_env: drop_env,
			 psi_env: psi_env, 
			 l2kam_ce: l2kam_ce}

    type TopCompileBasis = CompileBasis

    fun mk_CompileBasis a = a
    fun de_CompileBasis a = a

    fun topify a = a

    val empty = {TCEnv=LambdaStatSem.empty,
		 EqEnv=EliminateEq.empty,
		 OEnv=OptLambda.empty,
		 rse=RegionStatEnv.empty,
		 mulenv=Mul.empty_efenv,
		 mularefmap=Mul.empty_mularefmap,
		 drop_env=DropRegions.empty,
		 psi_env=PhysSizeInf.empty,
		 l2kam_ce=CompLamb.empty}

    val initial = {TCEnv=LambdaStatSem.initial,
		   EqEnv=EliminateEq.initial,
		   OEnv=OptLambda.initial,
		   rse=RegionStatEnv.initial,
		   mulenv=Mul.initial,
		   mularefmap=Mul.initial_mularefmap,
		   drop_env=DropRegions.init,
		   psi_env=PhysSizeInf.init,
		   l2kam_ce=CompLamb.init}

    fun plus({TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,l2kam_ce},
	     {TCEnv=TCEnv',EqEnv=EqEnv',OEnv=OEnv',rse=rse',mulenv=mulenv',
	      mularefmap=mularefmap',drop_env=drop_env',psi_env=psi_env',l2kam_ce=l2kam_ce'}) =
      {TCEnv=LambdaStatSem.plus(TCEnv,TCEnv'),
       EqEnv=EliminateEq.plus(EqEnv,EqEnv'),
       OEnv=OptLambda.plus(OEnv,OEnv'),
       rse=RegionStatEnv.plus(rse,rse'),
       mulenv=Mul.plus(mulenv,mulenv'),
       mularefmap=Mul.plus_mularefmap(mularefmap, mularefmap'),
       drop_env=DropRegions.plus(drop_env, drop_env'),
       psi_env=PhysSizeInf.plus(psi_env,psi_env'),
       l2kam_ce=CompLamb.plus(l2kam_ce, l2kam_ce')}

    val plus' = plus

    type StringTree = PP.StringTree
    fun layout_CompileBasis {TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,l2kam_ce} =
      PP.NODE{start="{", finish="}", indent=1, childsep=PP.RIGHT "; ",
              children=[EliminateEq.layout_env EqEnv,
			OptLambda.layout_env OEnv,
			LambdaStatSem.layout_env TCEnv,
                        RegionStatEnv.layout rse,
                        Mul.layout_efenv mulenv,
                        Mul.layout_mularefmap mularefmap,
			DropRegions.layout_env drop_env,
			PhysSizeInf.layout_env psi_env,
			CompLamb.layout_env l2kam_ce
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
      fun RegionStatEnv_enrich a = RegionStatEnv.enrich a
      fun Mul_enrich_efenv a = Mul.enrich_efenv a
      fun Mul_enrich_mularefmap a = Mul.enrich_mularefmap a
      fun DropRegions_enrich a = DropRegions.enrich a
      fun PhysSizeInf_enrich a = PhysSizeInf.enrich a
      fun CompLamb_enrich a = CompLamb.enrich a
    in
      fun enrich ({TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,l2kam_ce},
		  {TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,rse=rse1,mulenv=mulenv1,
		   mularefmap=mularefmap1,drop_env=drop_env1,psi_env=psi_env1,l2kam_ce=l2kam_ce1}) =
	debug("EqEnv", EliminateEq_enrich(EqEnv,EqEnv1)) andalso 
	debug("TCEnv", LambdaStatSem_enrich(TCEnv,TCEnv1)) andalso
	debug("OEnv", OptLambda_enrich(OEnv,OEnv1)) andalso
	debug("rse", RegionStatEnv_enrich(rse,rse1)) andalso
	debug("mulenv", Mul_enrich_efenv((mulenv,rse),(mulenv1,rse1))) andalso
	debug("mularefmap", Mul_enrich_mularefmap(mularefmap,mularefmap1)) andalso
	debug("drop_env", DropRegions_enrich(drop_env,drop_env1)) andalso
	debug("psi_env", PhysSizeInf_enrich(psi_env,psi_env1)) andalso
	debug("l2kam_ce", CompLamb_enrich(l2kam_ce,l2kam_ce1))
      val enrich' = enrich
    end

    fun match ({TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,l2kam_ce},
	       {TCEnv=TCEnv0,EqEnv=EqEnv0,OEnv=OEnv0,rse=rse0,mulenv=mulenv0,
		mularefmap=mularefmap0,drop_env=drop_env0,psi_env=psi_env0,l2kam_ce=l2kam_ce0}) = 

      let val EqEnv = EliminateEq.match(EqEnv,EqEnv0) 
	  val l2kam_ce = CompLamb.match(l2kam_ce, l2kam_ce0)
      in {TCEnv=TCEnv,EqEnv=EqEnv,OEnv=OEnv,rse=rse,mulenv=mulenv,
	  mularefmap=mularefmap,drop_env=drop_env,psi_env=psi_env,l2kam_ce=l2kam_ce}
      end

    fun restrict ({EqEnv,OEnv,TCEnv,rse,mulenv,mularefmap,drop_env,psi_env,l2kam_ce},
		  (lvars,lvars_with_prims,tynames,cons,excons)) = 
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
	  val tynames = TyName.tyName_LIST :: 
              TyName.tyName_BOOL ::
	      TyName.tyName_WORD_TABLE :: tynames     (* for elim eq *) 
	  val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames})
	  val lvars = lvars_eq @ lvars
	  val lvars_with_prims = Lvars.less_int_lvar :: Lvars.minus_int_lvar ::
	                         lvars_eq @ lvars_with_prims
	  val OEnv1 = OptLambda.restrict(OEnv,lvars)
	  val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
	  val lvars_with_prims = (*CompilerEnv.primlvarsOfCEnv CEnv1*) lvars_with_prims (*hack*)
	  val rse1 = RegionStatEnv.restrict(rse,{lvars=lvars_with_prims,tynames=tynames,cons=cons,excons=excons})
	  val mulenv1 = Mul.restrict_efenv(mulenv,lvars_with_prims)
	  val (places,effectvars) = (*RegionStatEnv.places_effectvarsRSE rse1*)
	    ([Effect.toplevel_region_withtype_top, Effect.toplevel_region_withtype_string,
	      Effect.toplevel_region_withtype_real,Effect.toplevel_region_withtype_bot], [Effect.toplevel_arreff])
	  val mularefmap1 = Mul.restrict_mularefmap(mularefmap,effectvars)
	  val drop_env1 = DropRegions.restrict(drop_env,lvars_with_prims)
	  val psi_env1 = PhysSizeInf.restrict(psi_env,lvars_with_prims)
	  val places = DropRegions.drop_places places
	  val l2kam_ce1 = CompLamb.restrict(l2kam_ce,{lvars=lvars,places=places,excons=excons,cons=cons})
      in {TCEnv=TCEnv1,
	  EqEnv=EqEnv1,
	  OEnv=OEnv1,
	  rse=rse1,
	  mulenv=mulenv1,
	  mularefmap=mularefmap1,
	  drop_env=drop_env1,
	  psi_env=psi_env1,
	  l2kam_ce=l2kam_ce1}
      end

    fun eq (B1,B2) = 
      let val B1 = topify B1
	val B2 = topify B2
      in enrich(B1,B2) andalso enrich(B2,B1)
      end
  end
