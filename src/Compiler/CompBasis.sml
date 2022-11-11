
structure CompBasis: COMP_BASIS =
  struct
    structure PP = PrettyPrint
    structure Normalize = LambdaBasics.Normalize

    fun log s = TextIO.output(TextIO.stdOut,s)
    fun say s = log s
    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    val quotation = Flags.is_on0 "quotation"

    type lvar = LambdaStatSem.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = EliminateEq.TyName
    type NEnv = Normalize.env
    type TCEnv = LambdaStatSem.env
    type EqEnv = EliminateEq.env
    type OEnv = OptLambda.env
    type rse = RegionStatEnv.regionStatEnv
    type mulenv  = Mul.efenv
    type mularefmap = Mul.mularefmap
    type drop_env = DropRegions.env
    type psi_env = PhysSizeInf.env
    type protenv = MulExp.ProtInf.pe

    type CompBasis = {NEnv: NEnv,    (* type scheme normalize environment *)
                      TCEnv : TCEnv, (* lambda type check environment *)
                      EqEnv : EqEnv, (* elimination of polymorphic equality environment *)
                      OEnv: OEnv,
                      rse: rse,
                      mulenv: mulenv,
                      mularefmap: mularefmap,
                      drop_env: drop_env,
                      psi_env: psi_env,
                      protenv: protenv}

    fun mk_CompBasis a = a
    fun de_CompBasis a = a

    val empty = {NEnv=Normalize.empty,
                 TCEnv=LambdaStatSem.empty,
                 EqEnv=EliminateEq.empty,
                 OEnv=OptLambda.empty,
                 rse=RegionStatEnv.empty,
                 mulenv=Mul.empty_efenv,
                 mularefmap=Mul.empty_mularefmap,
                 drop_env=DropRegions.empty,
                 psi_env=PhysSizeInf.empty,
                 protenv=MulExp.ProtInf.empPE}

    val initial = {NEnv=Normalize.initial,
                   TCEnv=LambdaStatSem.initial,
                   EqEnv=EliminateEq.initial,
                   OEnv=OptLambda.initial,
                   rse=RegionStatEnv.initial,
                   mulenv=Mul.initial,
                   mularefmap=Mul.initial_mularefmap,
                   drop_env=DropRegions.init,
                   psi_env=PhysSizeInf.init,
                   protenv=MulExp.ProtInf.initPE}

    fun plus ({NEnv,TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv},
              {NEnv=NEnv',TCEnv=TCEnv',EqEnv=EqEnv',OEnv=OEnv',rse=rse',mulenv=mulenv',
               mularefmap=mularefmap',drop_env=drop_env',psi_env=psi_env',protenv=protenv'}) =
      {NEnv=Normalize.plus(NEnv,NEnv'),
       TCEnv=LambdaStatSem.plus(TCEnv,TCEnv'),
       EqEnv=EliminateEq.plus(EqEnv,EqEnv'),
       OEnv=OptLambda.plus(OEnv,OEnv'),
       rse=RegionStatEnv.plus(rse,rse'),
       mulenv=Mul.plus(mulenv,mulenv'),
       mularefmap=Mul.plus_mularefmap(mularefmap, mularefmap'),
       drop_env=DropRegions.plus(drop_env, drop_env'),
       psi_env=PhysSizeInf.plus(psi_env,psi_env'),
       protenv=MulExp.ProtInf.plus(protenv,protenv')}

    type StringTree = PP.StringTree
    fun layout_CompBasis {NEnv,TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv} =
      PP.NODE{start="{", finish="}", indent=1, childsep=PP.RIGHT "; ",
              children=[Normalize.layout NEnv,
                        EliminateEq.layout_env EqEnv,
                        OptLambda.layout_env OEnv,
                        LambdaStatSem.layout_env TCEnv,
                        RegionStatEnv.layout rse,
                        Mul.layout_efenv mulenv,
                        Mul.layout_mularefmap mularefmap,
                        DropRegions.layout_env drop_env,
                        PhysSizeInf.layout_env psi_env,
                        MulExp.ProtInf.layoutPE protenv
                       ]
             }

    fun debug (s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
                          else log("\n" ^ s ^ ": enrich failed."); b)
                       else b

    fun debug1 (s, b,oenv,oenv1) =
        if !debug_man_enrich then
            (if b then log("\n" ^ s ^ ": enrich succeeded.")
             else (log("\n" ^ s ^ ": enrich failed.");
                   print ("*** OEnv =\n");
                   PP.printTree (OptLambda.layout_env oenv);
                   print ("\n*** OEnv1 =\n");
                   PP.printTree (OptLambda.layout_env oenv1));
                 b)
        else b

    local
      fun NEnv_enrich a = Normalize.enrich a
      fun EliminateEq_enrich a = EliminateEq.enrich a
      fun LambdaStatSem_enrich a = LambdaStatSem.enrich a
      fun OptLambda_enrich a = OptLambda.enrich a
      fun RegionStatEnv_enrich a = RegionStatEnv.enrich a
      fun Mul_enrich_efenv a = Mul.enrich_efenv a
      fun Mul_enrich_mularefmap a = Mul.enrich_mularefmap a
      fun DropRegions_enrich a = DropRegions.enrich a
      fun PhysSizeInf_enrich a = PhysSizeInf.enrich a
      fun ProtEnv_enrich a = MulExp.ProtInf.enrich a
    in
      fun enrich ({NEnv,TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv},
                  {NEnv=NEnv1,TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,rse=rse1,mulenv=mulenv1,
                   mularefmap=mularefmap1,drop_env=drop_env1,psi_env=psi_env1,protenv=protenv1}) =
        debug("NEnv", NEnv_enrich(NEnv,NEnv1)) andalso
        debug("EqEnv", EliminateEq_enrich(EqEnv,EqEnv1)) andalso
        debug("TCEnv", LambdaStatSem_enrich(TCEnv,TCEnv1)) andalso
        debug1("OEnv", OptLambda_enrich(OEnv,OEnv1), OEnv, OEnv1) andalso
        debug("rse", RegionStatEnv_enrich(rse,rse1)) andalso
        debug("mulenv", Mul_enrich_efenv((mulenv,rse),(mulenv1,rse1))) andalso
        debug("mularefmap", Mul_enrich_mularefmap(mularefmap,mularefmap1)) andalso
        debug("drop_env", DropRegions_enrich(drop_env,drop_env1)) andalso
        debug("psi_env", PhysSizeInf_enrich(psi_env,psi_env1)) andalso
        debug("protenv", ProtEnv_enrich(protenv,protenv1))
    end

    fun match ({NEnv,TCEnv,EqEnv,OEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv},
               {NEnv=NEnv0,TCEnv=TCEnv0,EqEnv=EqEnv0,OEnv=OEnv0,rse=rse0,mulenv=mulenv0,
                mularefmap=mularefmap0,drop_env=drop_env0,psi_env=psi_env0,protenv=protenv0}) =
      let val EqEnv = EliminateEq.match(EqEnv,EqEnv0)
      in {NEnv=NEnv,TCEnv=TCEnv,EqEnv=EqEnv,OEnv=OEnv,rse=rse,mulenv=mulenv,
          mularefmap=mularefmap,drop_env=drop_env,psi_env=psi_env,protenv=protenv}
      end

    fun restrict ({NEnv,EqEnv,OEnv,TCEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv},
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
         * identifiers below.  Martin-18/03/1998
         *
         * The exception constructors Subscript and Size are also
         * included here, which allows us to let the optimiser
         * inline array and string subscription and array allocation
         * across module boundaries.
         *)

          val excons =
              Excon.ex_DIV :: Excon.ex_MATCH :: Excon.ex_BIND ::
              Excon.ex_SUBSCRIPT :: Excon.ex_SIZE :: excons
          val cons = Con.con_NIL :: Con.con_CONS ::
              Con.con_TRUE :: Con.con_FALSE :: Con.con_INTINF :: cons   (* for elim eq *)
          val cons = if quotation() then Con.con_QUOTE :: Con.con_ANTIQUOTE :: cons
                     else cons
          val tynames = TyName.tyName_LIST :: TyName.tyName_INTINF ::
              TyName.tyName_BOOL ::
              TyName.tyName_FOREIGNPTR ::
              TyName.tyName_VECTOR :: tynames     (* for elim eq *)
          val tynames = if quotation() then TyName.tyName_FRAG :: tynames
                        else tynames
          val NEnv1 = Normalize.restrict(NEnv,lvars)
          val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames})
          val lvars = lvars_eq @ lvars
          val (OEnv1,lvars',cons',tynames') = OptLambda.restrict(OEnv,lvars,cons,tynames)
          val lvars = lvars' @ lvars
          val cons = cons' @ cons
          val tynames = tynames' @ tynames
          val tynames = TyName.tyName_F64 :: tynames (* for optimiser float unboxing *)
          val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
          val rse1 = RegionStatEnv.restrict(rse,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
          val mulenv1 = Mul.restrict_efenv(mulenv,lvars)
          val (places,effectvars) = (*RegionStatEnv.places_effectvarsRSE rse1*)
            ([Effect.toplevel_region_withtype_top, Effect.toplevel_region_withtype_string,
              Effect.toplevel_region_withtype_pair,Effect.toplevel_region_withtype_bot], [Effect.toplevel_arreff])
          val mularefmap1 = Mul.restrict_mularefmap(mularefmap,effectvars)
          val drop_env1 = DropRegions.restrict(drop_env,lvars)
          val psi_env1 = PhysSizeInf.restrict(psi_env,lvars)
          val protenv1 = MulExp.ProtInf.restrict(protenv,lvars)
      in ({NEnv=NEnv1,
           TCEnv=TCEnv1,
           EqEnv=EqEnv1,
           OEnv=OEnv1,
           rse=rse1,
           mulenv=mulenv1,
           mularefmap=mularefmap1,
           drop_env=drop_env1,
           psi_env=psi_env1,
           protenv=protenv1}, lvars, cons, excons)
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

    fun restrict0 ({NEnv,EqEnv,OEnv,TCEnv,rse,mulenv,mularefmap,drop_env,psi_env,protenv},
                  (lvars,tynames,cons,excons)) =
      let
          (* Don't include identifiers that are declared by the initial basis *)

          val tynames = subtractPredefinedTynames tynames
          val cons = subtractPredefinedCons cons
          val excons = subtractPredefinedExcons excons
          val NEnv1 = Normalize.restrict(NEnv,lvars)
          val (lvars_eq,EqEnv1) = EliminateEq.restrict(EqEnv,{lvars=lvars,tynames=tynames})
          val lvars = lvars_eq @ lvars
          val (OEnv1,lvars',cons',tynames') = OptLambda.restrict(OEnv,lvars,cons,tynames)
          val lvars = lvars' @ lvars
          val cons = cons' @ cons
          val tynames = tynames' @ tynames
          val tynames = subtractPredefinedTynames tynames
          val cons = subtractPredefinedCons cons
          val TCEnv1 = LambdaStatSem.restrict(TCEnv,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
          val rse1 = RegionStatEnv.restrict(rse,{lvars=lvars,tynames=tynames,cons=cons,excons=excons})
          val mulenv1 = Mul.restrict_efenv(mulenv,lvars)
          val mularefmap1 = Mul.restrict_mularefmap(mularefmap,nil)
          val drop_env1 = DropRegions.restrict(drop_env,lvars)
          val psi_env1 = PhysSizeInf.restrict(psi_env,lvars)
          val protenv1 = MulExp.ProtInf.restrict(protenv,lvars)
      in ({NEnv=NEnv1,
           TCEnv=TCEnv1,
           EqEnv=EqEnv1,
           OEnv=OEnv1,
           rse=rse1,
           mulenv=mulenv1,
           mularefmap=mularefmap1,
           drop_env=drop_env1,
           psi_env=psi_env1,
           protenv=protenv1}, lvars, cons, excons)
      end

    fun eq (B1,B2) = enrich(B1,B2) andalso enrich(B2,B1)

    val pu =
        let fun to (((ne,tce),eqe,oe,rse),(me,mm,de,pe),protenv) =
            {NEnv=ne,TCEnv=tce, EqEnv=eqe, OEnv=oe, rse=rse,
             mulenv=me, mularefmap=mm, drop_env=de, psi_env=pe,protenv=protenv}
            fun from {NEnv=ne,TCEnv=tce, EqEnv=eqe, OEnv=oe, rse,
                      mulenv=me, mularefmap=mm, drop_env=de, psi_env=pe,protenv}
                = (((ne,tce),eqe,oe,rse),(me,mm,de,pe),protenv)
        in Pickle.convert (to,from)
            (Pickle.tup3Gen0(Pickle.tup4Gen0(Pickle.pairGen0(Pickle.comment "NEnv.pu" Normalize.pu,
                                                             Pickle.comment "LambdaStatSem.pu" LambdaStatSem.pu),
                                             Pickle.comment "EliminateEq.pu" EliminateEq.pu,
                                             OptLambda.pu,
                                             Pickle.comment "RegionStatEnv" RegionStatEnv.pu),
                             Pickle.tup4Gen0(Pickle.comment "Mul.efenv" Mul.pu_efenv,
                                             Pickle.comment "Mul.mularefmap" Mul.pu_mularefmap,
                                             Pickle.comment "DropRegions.env" DropRegions.pu_env,
                                             Pickle.comment "PhysSizeInf.env" PhysSizeInf.pu_env),
                             MulExp.ProtInf.pu_pe))
        end
  end
