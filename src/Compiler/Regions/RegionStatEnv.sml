
structure RegionStatEnv: REGION_STAT_ENV =

  struct
    structure E = Effect
    structure R = RType
    structure L = LambdaExp
    structure PP = PrettyPrint
    structure Lvar = Lvars
    structure ConMap = Con.Map
    structure TyNameMap = TyName.Map
    structure ExconMap = Excon.Map
    structure LvarMap = Lvar.Map

    fun die s = Crash.impossible ("RegionStatEnv." ^ s)
    fun log s = TextIO.output(TextIO.stdOut,s ^ "\n")
    fun log_st st = PP.outputTree(fn s => TextIO.output(TextIO.stdOut,s), st, 70)
    fun layout_effects effects = PP.NODE{start="[",finish="]",indent=1,childsep=PP.RIGHT ",",
					 children=map E.layout_effect effects}
    fun dump (t: PP.StringTree):unit =
        PP.outputTree(fn s => TextIO.output(!Flags.log,s), t, !Flags.colwidth)

      (* arity: number of quantified type variables,
		list of runtime types,
		number of effect  variables *)
    type runType = E.runType
    type arity = int * E.runType list * int
    fun mk_arity x = x
    fun un_arity x = x

    type tyvar = L.tyvar
    type effectvar = E.effect

    type il = R.il
    type cone = R.cone

    type con = Con.con                           (* Unqualified value constructors. *)
    type excon = Excon.excon			(* Unqualified exception constructors.*)
    type TyName = TyName.TyName
    type lvar = Lvar.lvar			(* Unique lambda identifiers. *)
    type sigma = R.sigma
     and Type = R.Type
     and mu = R.mu
     and place = R.place

    type instance_list = (il * (il * cone -> il * cone)) ref list

    type lvar_env_range = bool * bool * RegVar.regvar list * sigma * place option
			* instance_list ref option * (il ->unit)option

    type regionStatEnv = {tyname_env : arity TyNameMap.map,
			  con_env    : R.sigma ConMap.map,
			  excon_env  : mu ExconMap.map,
			  lvar_env   : lvar_env_range LvarMap.map,
                          regvar_env : E.effect RegVar.Map.map,
                          tyvar_env  : effectvar L.TyvarMap.map
			 }

    val empty: regionStatEnv = {tyname_env = TyNameMap.empty,
				con_env = ConMap.empty,
				excon_env = ExconMap.empty,
				lvar_env = LvarMap.empty,
                                regvar_env = RegVar.Map.empty,
                                tyvar_env = L.TyvarMap.empty}

    val tyname_env0= TyNameMap.add(TyName.tyName_STRING, (0,[],0),
		     TyNameMap.add(TyName.tyName_REAL, (0,[],0),
		     TyNameMap.add(TyName.tyName_F64, (0,[],0),
		     TyNameMap.add(TyName.tyName_F256, (0,[],0),
		     TyNameMap.add(TyName.tyName_EXN, (0,[],0),
		     TyNameMap.add(TyName.tyName_REF, (1,[],0),
		     TyNameMap.add(TyName.tyName_BOOL, (0,[],0),
		     TyNameMap.add(TyName.tyName_INT31, (0,[],0),
		     TyNameMap.add(TyName.tyName_INT32, (0,[],0),
		     TyNameMap.add(TyName.tyName_INT63, (0,[],0),
		     TyNameMap.add(TyName.tyName_INT64, (0,[],0),
		     TyNameMap.add(TyName.tyName_CHAR, (0,[],0),
		     TyNameMap.add(TyName.tyName_WORD8, (0,[],0),
		     TyNameMap.add(TyName.tyName_WORD31, (0,[],0),
		     TyNameMap.add(TyName.tyName_WORD32, (0,[],0),
		     TyNameMap.add(TyName.tyName_WORD63, (0,[],0),
		     TyNameMap.add(TyName.tyName_WORD64, (0,[],0),
		     TyNameMap.add(TyName.tyName_LIST, (1,[E.PAIR_RT],0),
				   (* the auxiliary region is for a pair; hence PAIR_RT *)
		     TyNameMap.add(TyName.tyName_FRAG, (1,[E.STRING_RT],0),
				   (* the auxiliary region is for a string; hence STRING_RT *)
		     TyNameMap.add(TyName.tyName_INTINF, (0,[E.PAIR_RT,E.PAIR_RT],0),
				   (* the auxiliary region is for (1) auxiliary pairs for the digits-list
				    * and (2) the pair of a boolean and the digits-list *)
		     TyNameMap.add(TyName.tyName_CHARARRAY, (0,[],0),
		     TyNameMap.add(TyName.tyName_FOREIGNPTR, (0,[],0),
		     TyNameMap.add(TyName.tyName_ARRAY, (1,[],0),
		     TyNameMap.add(TyName.tyName_VECTOR, (1,[],0),
		     TyNameMap.empty))))))))))))))))))))))))

    local

      fun mkListType (mu,rho) = R.mkCONSTYPE(TyName.tyName_LIST, [mu], [rho], [])

      fun mk_nil_sigma c lev0 =
	let val alpha = L.fresh_tyvar()
	    val alpha_mu = R.mkTYVAR alpha
	    val (rho,c) = E.freshRhoWithTy(E.PAIR_RT, c)  (* aux region for pairs *)
	    val (c,nil_sigma) = R.generalize_all (c, lev0, [(alpha,NONE)], mkListType(alpha_mu, rho))
	in (c, nil_sigma)
	end

      fun mk_cons_sigma c lev0 =
	let val alpha = L.fresh_tyvar()
	    val alpha_mu = R.mkTYVAR alpha
	    val (rho,c) = E.freshRhoWithTy(E.PAIR_RT, c)  (* aux region for pairs *)
	    val list_mu = mkListType(alpha_mu, rho)
	    val (arreff,c) = E.freshEps c
	    val cons_mu = R.mkFUN([R.mkBOX(R.mkRECORD[alpha_mu,list_mu], rho)],
				  arreff,
				  [list_mu])
	    val (c,cons_sigma) = R.generalize_all (c, lev0, [(alpha,NONE)], cons_mu)
	in (c, cons_sigma)
	end

      fun mkFragConsTy mu1 ae (mu,rho,rho0) =
	  R.mkFUN([mu1],ae,[R.mkBOX(R.mkCONSTYPE(TyName.tyName_FRAG, [mu], [rho], []), rho0)])

      fun mk_quote_sigma c lev0 =
	let val alpha = L.fresh_tyvar()
	    val alpha_mu = R.mkTYVAR alpha
	    val (rho1,c) = E.freshRhoWithTy(E.STRING_RT, c)  (* region for auxiliary strings *)
	    val (rho2,c) = E.freshRhoWithTy(E.TOP_RT, c)     (* region for result frag *)
	    val (arreff, c) = E.freshEps c
	    val _ = E.edge(arreff, E.mkPut rho2)
	    val quote_ty = mkFragConsTy (R.mkBOX(R.mkCONSTYPE(TyName.tyName_STRING,[],[],[]),rho1))
	                                arreff (alpha_mu,rho1,rho2)
	    val (c,quote_sigma) = R.generalize_all (c, lev0, [(alpha,NONE)], quote_ty)
	in (c, quote_sigma)
	end

      fun mk_antiquote_sigma c lev0 =
	let val alpha = L.fresh_tyvar()
	    val alpha_mu = R.mkTYVAR alpha
	    val (rho1,c) = E.freshRhoWithTy(E.STRING_RT, c)  (* region for auxiliary strings *)
	    val (rho2,c) = E.freshRhoWithTy(E.TOP_RT, c)     (* region for result frag *)
	    val (arreff, c) = E.freshEps c
	    val _ = E.edge(arreff, E.mkPut rho2)
	    val antiquote_ty = mkFragConsTy alpha_mu arreff (alpha_mu,rho1,rho2)
	    val (c,antiquote_sigma) = R.generalize_all (c, lev0, [(alpha,NONE)], antiquote_ty)
	in (c, antiquote_sigma)
	end

      fun mk_bool_sigma c lev0 =
	let val (c,bool_sigma) =
	       R.generalize_all (c, lev0, [], (R.mkCONSTYPE(TyName.tyName_BOOL,[],[],[])))
	in (c, bool_sigma)
	end

      fun mk_intinf_sigma c lev0 =
	  let val (rListPair,c) = E.freshRhoWithTy(E.PAIR_RT, c)
	      val (rRec,c) = E.freshRhoWithTy(E.PAIR_RT, c)
	      val bool_mu = R.mkCONSTYPE(TyName.tyName_BOOL,[],[],[])
	      val int31_mu = R.mkCONSTYPE(TyName.tyName_INT31,[],[],[])
	      val digits_mu = mkListType(int31_mu,rListPair)
	      val arg_mu = R.mkBOX(R.mkRECORD[digits_mu,bool_mu],rRec)
	      val intinf_mu = R.mkCONSTYPE(TyName.tyName_INTINF,[],[rRec,rListPair],[])
	      val (ae, c) = E.freshEps c
	      val f = R.mkFUN([arg_mu],ae,[intinf_mu])
	      val (c,intinf_sigma) = R.generalize_all (c, lev0, [], f)
	  in (c,intinf_sigma)
	  end

      val c = E.initCone
      val lev0 = E.level c
      val c = E.push c

      val (c, nil_sigma) = mk_nil_sigma c lev0
      val (c, cons_sigma) = mk_cons_sigma c lev0
      val (c, bool_sigma) = mk_bool_sigma c lev0
      val (c, quote_sigma) = mk_quote_sigma c lev0
      val (c, antiquote_sigma) = mk_antiquote_sigma c lev0
      val (c, intinf_sigma) = mk_intinf_sigma c lev0
    in
      val conenv0 = ConMap.fromList [(Con.con_TRUE, bool_sigma),
				     (Con.con_FALSE, bool_sigma),
				     (Con.con_NIL, nil_sigma),
				     (Con.con_CONS, cons_sigma),
				     (Con.con_QUOTE, quote_sigma),
				     (Con.con_ANTIQUOTE, antiquote_sigma),
				     (Con.con_INTINF, intinf_sigma)]
    end

    val excon_env0 = ExconMap.fromList
      (map (fn excon => (excon, R.mkBOX(R.exnType,E.toplevel_region_withtype_top)))
       [Excon.ex_DIV, Excon.ex_MATCH,
	Excon.ex_BIND, Excon.ex_OVERFLOW, Excon.ex_INTERRUPT,
        Excon.ex_SUBSCRIPT, Excon.ex_SIZE])

    val regvar_env0 =
        RegVar.Map.fromList
            (map (fn (s,rv) => (RegVar.mk_Named s, rv))
                 [("greg_top",    E.toplevel_region_withtype_top),
                  ("greg_string", E.toplevel_region_withtype_string),
                  ("greg_pair",   E.toplevel_region_withtype_pair),
                  ("greg_triple", E.toplevel_region_withtype_triple),
                  ("greg_array",  E.toplevel_region_withtype_array),
                  ("greg_ref",    E.toplevel_region_withtype_ref)])

    val initial: regionStatEnv = {tyname_env = tyname_env0,
				  con_env    = conenv0,
				  excon_env  = excon_env0,
				  lvar_env   = LvarMap.empty,
                                  regvar_env = regvar_env0,
                                  tyvar_env  = L.TyvarMap.empty}

    fun declareTyName (tyname,arity,{tyname_env, con_env,excon_env,
                                     lvar_env,regvar_env,tyvar_env}) =
	{tyname_env = TyNameMap.add(tyname,arity,tyname_env),
	 con_env= con_env,
	 excon_env = excon_env,
	 lvar_env = lvar_env,
         regvar_env = regvar_env,
         tyvar_env = tyvar_env}

    fun declareCon (con,sigma,{tyname_env, con_env, excon_env,
                               lvar_env, regvar_env, tyvar_env}) =
	{tyname_env = tyname_env,
	 con_env= ConMap.add (con,sigma,con_env),
	 excon_env = excon_env,
	 lvar_env = lvar_env,
         regvar_env = regvar_env,
         tyvar_env = tyvar_env}

    fun declareExcon (excon,mu,{tyname_env, con_env, excon_env,
                                lvar_env, regvar_env, tyvar_env}) =
	{tyname_env = tyname_env,
	 con_env = con_env,
	 excon_env = ExconMap.add (excon,mu,excon_env),
	 lvar_env = lvar_env,
         regvar_env = regvar_env,
         tyvar_env = tyvar_env}

    fun declareLvar (lvar,range,{tyname_env, con_env, excon_env,
                                 lvar_env, regvar_env, tyvar_env})=
	{tyname_env = tyname_env,
	 con_env = con_env,
	 excon_env = excon_env,
	 lvar_env = LvarMap.add(lvar,range,lvar_env),
         regvar_env = regvar_env,
         tyvar_env = tyvar_env}

    fun declareRegVar (rv,effect,{tyname_env, con_env, excon_env,
                                  lvar_env, regvar_env, tyvar_env})=
	{tyname_env = tyname_env,
	 con_env = con_env,
	 excon_env = excon_env,
	 lvar_env = lvar_env,
         regvar_env = RegVar.Map.add(rv,effect,regvar_env),
         tyvar_env = tyvar_env}

    fun declareTyVar (tv,effect,{tyname_env, con_env, excon_env,
                                 lvar_env, regvar_env, tyvar_env})=
	{tyname_env = tyname_env,
	 con_env = con_env,
	 excon_env = excon_env,
	 lvar_env = lvar_env,
         regvar_env = regvar_env,
         tyvar_env = L.TyvarMap.add(tv,effect,tyvar_env)}

    fun plus ({tyname_env, con_env, excon_env,lvar_env,regvar_env,tyvar_env},
	      {tyname_env = tyname_env', con_env=con_env', excon_env=excon_env',
               lvar_env=lvar_env',regvar_env=regvar_env',tyvar_env=tyvar_env'}) =
	{tyname_env = TyNameMap.plus(tyname_env, tyname_env'),
	 con_env = ConMap.plus(con_env, con_env'),
	 excon_env = ExconMap.plus(excon_env, excon_env'),
	 lvar_env = LvarMap.plus(lvar_env, lvar_env'),
         regvar_env = RegVar.Map.plus(regvar_env, regvar_env'),
         tyvar_env = L.TyvarMap.plus(tyvar_env,tyvar_env')}

    fun lookupTyVar (rse : regionStatEnv as {tyvar_env,...}) = L.TyvarMap.lookup tyvar_env
    fun lookupRegVar (rse : regionStatEnv as {regvar_env,...}) = RegVar.Map.lookup regvar_env
    fun lookupTyName (rse : regionStatEnv as {tyname_env,...}) = TyNameMap.lookup tyname_env
    fun lookupCon (rse : regionStatEnv as {con_env,...}) con = ConMap.lookup con_env con
    fun lookupExcon (rse : regionStatEnv as {excon_env,...}) = ExconMap.lookup excon_env
    fun lookupLvar (rse  : regionStatEnv as {lvar_env,...}) = LvarMap.lookup lvar_env
    fun FoldLvar f b (rse: regionStatEnv as {lvar_env, ...}) = LvarMap.Fold f b lvar_env
    fun FoldExcon f b (rse: regionStatEnv as {excon_env, ...}) = ExconMap.Fold f b excon_env
    fun FoldTyVar f b (rse: regionStatEnv as {tyvar_env, ...}) = L.TyvarMap.Fold f b tyvar_env

    val mapLvar : (lvar_env_range -> lvar_env_range) -> regionStatEnv -> regionStatEnv  =
	  fn f => fn (env as {tyname_env,con_env,excon_env,lvar_env,regvar_env,tyvar_env}) =>
	     {tyname_env = tyname_env,
	      con_env = con_env,
	      excon_env = excon_env,
	      lvar_env = LvarMap.composemap f lvar_env,
              regvar_env = regvar_env,
              tyvar_env = tyvar_env}

    type cone = E.cone

    fun pr_effect e = PP.flatten1(Effect.layout_effect e)

    fun warn n =
        print ("** WARNING: free effect with wrong level: " ^ pr_effect n ^ "\n")

    fun rhos_epss_free_rse (rse: regionStatEnv) =
      let val rhos_epss = FoldLvar (fn ((_,(_,_,_,sigma,rho,_,_)),acc) =>
				    R.ann_sigma sigma (case rho of SOME rho => rho::acc | NONE => acc)) [] rse
	  val rhos_epss = FoldExcon (fn ((_,mu),acc) =>
				        R.ann_mus [mu] acc) rhos_epss rse
          val rhos_epss = FoldTyVar (fn ((_,e),acc) => e::acc) rhos_epss rse
	  val toplevel : int = E.level E.initCone
	  val rhos_epss_free =
	    foldl (fn (node, acc) =>
			case E.level_of node
			  of SOME level => if level = toplevel then node :: acc
					   else acc
			   | NONE => die "mkConeToplevel.rhos_epss_free.node not rho or eps.")
	    [] rhos_epss
	  val rhos_epss_free = E.remove_duplicates rhos_epss_free
	  fun closure ([],acc) = acc
	    | closure (rho_eps::rest,acc) =
	    closure(rest, let in
                           if E.is_arrow_effect rho_eps then
			    foldl (fn (node, acc) =>
                                        let in
  					  if E.is_arrow_effect node then node::acc
					  else if E.is_put node orelse E.is_get node then
					    E.rho_of node :: acc
					  else
                                            (TextIO.output(!Flags.log, "arrow effect:\n");
                                             dump(E.layout_effect_deep(rho_eps));
                                             TextIO.output(!Flags.log, "atomic effect:\n");
                                             dump(E.layout_effect_deep(node));
                                             die "mkConeToplevel.closure.node not arrow effect or get/put effect")
                                        end)
			    acc (E.represents rho_eps) (* very nasty bug fixed; the two arguments to foldL were the wrong way around;  mads *)
			  else if E.is_rho rho_eps then rho_eps :: acc
			       else acc
                         end)
	  val rhos_epss_free = E.sort(E.remove_duplicates(closure(rhos_epss_free,[])))
      in rhos_epss_free
      end

    fun rhos_epss_free_rse' (rse: regionStatEnv) =
        let val rhos_epss = FoldLvar (fn ((_,(_,_,_,sigma,rho,_,_)),acc) =>
				         R.ferv_sigma sigma @
                                         (case rho of SOME rho => rho::acc | NONE => acc))
                                     [] rse
	    val rhos_epss = FoldExcon (fn ((_,mu),acc) =>
				          R.ferv_sigma (R.type_to_scheme mu) @ acc)
                                      rhos_epss rse
            val rhos_epss = FoldTyVar (fn ((_,e),acc) => e::acc) rhos_epss rse
	    val rhos_epss = E.remove_duplicates rhos_epss
	    fun closure ([],acc) = acc
	      | closure (rho_eps::rest,acc) =
	        closure(rest, if E.is_arrow_effect rho_eps then
			        foldl (fn (node, acc) =>
  					  if E.is_arrow_effect node then node::acc
					  else if E.is_put node orelse E.is_get node then
					    E.rho_of node :: acc
					  else
                                            (TextIO.output(!Flags.log, "arrow effect:\n");
                                             dump(E.layout_effect_deep(rho_eps));
                                             TextIO.output(!Flags.log, "atomic effect:\n");
                                             dump(E.layout_effect_deep(node));
                                             die "mkConeToplevel.closure.node not arrow effect or get/put effect")
                                      )
			              acc (E.represents rho_eps)
			      else if E.is_rho rho_eps then rho_eps :: acc
			      else acc
                       )
	  val rhos_epss = E.sort(E.remove_duplicates(closure(rhos_epss,[])))
        in rhos_epss
        end

    fun mkConeToplevel (rse: regionStatEnv) : cone =
        (E.reset_cone E.emptyCone;
         E.pushLayer(rhos_epss_free_rse rse,E.emptyCone))

    fun equal_sigma (sigma1,sigma2) = R.alpha_equal (sigma1,sigma2) E.initCone

    fun eq_regvars (nil,nil) = true
      | eq_regvars (x::xs,y::ys) = RegVar.eq(x,y) andalso eq_regvars(xs,ys)
      | eq_regvars _ = false

    fun equal_lvar_res ((b1,b1',rvs1,sigma1,place1,_,_),(b2,b2',rvs2,sigma2,place2,_,_)) =
        b1 = b2 andalso b1' = b2'
        andalso (case (place1, place2) of
                     (SOME p1,SOME p2) => E.eq_effect(p1,p2)
                   | (NONE, NONE) => true
                   | _ => false)
        andalso equal_sigma(sigma1,sigma2) andalso eq_regvars (rvs1,rvs2)

    fun equal_con_res (sigma1,sigma2) = equal_sigma (sigma1,sigma2)

    fun equal_excon_res (mu1,mu2) =
        equal_sigma(R.type_to_scheme mu1,R.type_to_scheme mu2)

    local
      fun tyname_env_restrict (tyname_env,tynames) =
	  TyNameMap.restrict(TyName.pr_TyName,tyname_env,tynames)
	  handle TyNameMap.Restrict s => die ("restrict; I cannot find tyname " ^ s ^ " in the environment")
      fun con_env_restrict (con_env,cons) =
	  ConMap.restrict(Con.pr_con,con_env,cons)
	  handle ConMap.Restrict s => die ("restrict; I cannot find con " ^ s ^ " in the environment")
      fun excon_env_restrict (excon_env,excons) =
	  ExconMap.restrict(Excon.pr_excon,excon_env,excons)
	  handle ExconMap.Restrict s => die ("restrict; I cannot find excon " ^ s ^ " in the environment")
      fun lvar_env_restrict (lvar_env,lvars) =
	  LvarMap.restrict(Lvar.pr_lvar,lvar_env,lvars)
	  handle LvarMap.Restrict s => die ("restrict; I cannot find lvar " ^ s ^ " in the environment")
    in
    fun restrict ({tyname_env, con_env, excon_env,lvar_env,regvar_env=_,tyvar_env=_},
                  {tynames,cons,excons,lvars}) =
	{tyname_env=tyname_env_restrict(tyname_env,tynames),
	 con_env=con_env_restrict(con_env,cons),
	 excon_env=excon_env_restrict(excon_env,excons),
	 lvar_env=lvar_env_restrict(lvar_env,lvars),
         regvar_env=RegVar.Map.empty,
         tyvar_env=L.TyvarMap.empty}
    end

    fun places_effectvarsRSE rse =
        let val rhos_epss = rhos_epss_free_rse rse
	    val rhos = List.filter E.is_rho rhos_epss
	    val epss = List.filter E.is_arrow_effect rhos_epss
        in (rhos,epss)
        end

    fun places_effectvarsRSE' rse =
        let val rhos_epss = rhos_epss_free_rse' rse
	    val rhos = List.filter E.is_rho rhos_epss
	    val epss = List.filter E.is_arrow_effect rhos_epss
        in (rhos,epss)
        end

    type StringTree = PP.StringTree
    val layout_scheme = R.mk_lay_sigma false (* do not omit region info *)
    val (_,layout_mu) = R.mk_layout false    (* do not omit region info *)

    fun layout_pair (_,_,_,sigma,p,_,_) =
        case p of
            SOME p => PP.NODE{start= "(", finish = ")", indent = 1, childsep = PP.RIGHT ",",
			      children = [layout_scheme sigma, E.layout_effect p]}
          | NONE => layout_scheme sigma

    fun layout_arity (a,b,c) =
	PP.NODE{start = "(", finish  = ")",
		indent = 1, childsep = PP.RIGHT ", ",
		children = PP.LEAF (Int.toString a) ::
			   (map (PP.LEAF o E.show_runType) b) @ [PP.LEAF (Int.toString c)]}

    fun layout_tyname_env e =
        TyNameMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                            (PP.LEAF o TyName.pr_TyName) layout_arity e

    fun layout_con_env e =
        ConMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                         (PP.LEAF o Con.pr_con) layout_scheme e

    fun layout_excon_env e =
        ExconMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                           (PP.LEAF o Excon.pr_excon) layout_mu e

    fun layout_lvar_env e =
        LvarMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                          (PP.LEAF o Lvar.pr_lvar) layout_pair e

    fun layout_regvar_env e =
        RegVar.Map.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                             (PP.LEAF o RegVar.pr) E.layout_effect e

    fun layout_tyvar_env e =
        L.TyvarMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
                             (PP.LEAF o L.pr_tyvar) E.layout_effect e

    fun layout (rse as {tyname_env, con_env, excon_env,lvar_env,regvar_env,tyvar_env}) =
	PP.NODE{start = "RegionStaticEnvironment:", finish = "(end of RegionStatEnvironment)",
		indent = 1, childsep = PP.RIGHT",",
		children = [layout_tyname_env tyname_env,
			    layout_con_env con_env,
			    layout_excon_env excon_env,
			    layout_lvar_env lvar_env,
                            layout_regvar_env regvar_env,
                            layout_tyvar_env tyvar_env]}

    val debug_man_enrich = Flags.is_on0 "debug_man_enrich"

    fun debug (s, b) = if debug_man_enrich() then
                         (if b then log("\nRSE." ^ s ^ ": enrich succeeded.")
			  else log("\nRSE." ^ s ^ ": enrich failed."); b)
		       else b

    fun debug1 (s, b,lvenv,lvenv1) =
	if debug_man_enrich() then
	    (if b then log("\nRSE." ^ s ^ ": enrich succeeded.")
	     else (log("\nRSE." ^ s ^ ": enrich failed.");
		   print ("*** RSE.LVEnv =\n");
		   PP.printTree (layout_lvar_env lvenv);
		   print ("\n*** RSE.LVEnv1 =\n");
		   PP.printTree (layout_lvar_env lvenv1));
		 b)
	else b


    fun enrich ({tyname_env, con_env, excon_env,lvar_env,regvar_env,tyvar_env}:regionStatEnv,
		{tyname_env=tyname_env1, con_env=con_env1, excon_env=excon_env1,
                 lvar_env=lvar_env1,regvar_env=regvar_env1,tyvar_env=tyvar_env1}:regionStatEnv) =
	debug("TyNameMap", TyNameMap.enrich (op =) (tyname_env,tyname_env1)) andalso
	debug("ConMap", ConMap.enrich equal_con_res (con_env,con_env1)) andalso
	debug("ExconMap", ExconMap.enrich equal_excon_res (excon_env,excon_env1)) andalso
	debug1("LvarMap", LvarMap.enrich equal_lvar_res (lvar_env,lvar_env1),lvar_env,lvar_env1) andalso
        debug("RegVarMap", RegVar.Map.enrich E.eq_effect (regvar_env,regvar_env1)) andalso
        debug("TyVarMap", L.TyvarMap.enrich E.eq_effect (tyvar_env,tyvar_env1))

    (* Spurious type variables *)

    fun mem nil y = false
      | mem (x::xs) y = x = y orelse mem xs y

    fun spuriousJoin nil ys = ys
      | spuriousJoin (x::xs) ys = if mem ys x then spuriousJoin xs ys
                                  else spuriousJoin xs (x::ys)

    fun spuriousTyvars rse ty0 (lvs,exs) : tyvar list =
        let val tvs =
                List.foldl (fn (lv,acc) =>
                               case lookupLvar rse lv of
                                   NONE => die "spuriousTyvars.lv"
                                 | SOME(_,_,_,sigma,_,_,_) =>
                                   case R.ftv_sigma sigma of
                                       nil => acc
                                     | tvs => spuriousJoin (R.ftv_minus(tvs,R.ftv_ty ty0))
                                                           acc
                           ) nil lvs

            val tvs =
                List.foldl (fn (ex,acc) =>
                               case lookupExcon rse ex of
                                   NONE => die "spuriousTyvars.ex"
                                 | SOME mu =>
                                   case R.ftv_ty mu of
                                       nil => acc
                                     | tvs => spuriousJoin (R.ftv_minus(tvs,R.ftv_ty ty0))
                                                           acc
                           ) tvs exs
        in tvs
        end

    (* Pickling *)

    val pu_arity = Pickle.tup3Gen(Pickle.int,E.pu_runTypes,Pickle.int)

    val pu_lvar_env_range : lvar_env_range Pickle.pu =
	Pickle.convert (fn ((b1,b2,rvs),s,p) => (b1,b2,rvs,s,p,NONE,NONE),
			fn (b1,b2,rvs,s,p,NONE,NONE) => ((b1,b2,rvs),s,p)
			 | _ => die "pu_lvar_env_range")
	(Pickle.tup3Gen0(Pickle.tup3Gen0(Pickle.bool,Pickle.bool,Pickle.listGen RegVar.pu),Pickle.debugUnpickle "sigma" R.pu_sigma,Pickle.debugUnpickle "effect" (Pickle.optionGen E.pu_effect)))

    val pu : regionStatEnv Pickle.pu =
	Pickle.debugUnpickle "regionStatEnv"
	(Pickle.convert (fn (te:arity TyNameMap.map,ce: R.sigma ConMap.map,
			     ee:mu ExconMap.map,le) =>
                            {tyname_env=te,con_env=ce,excon_env=ee,lvar_env=le,
                             regvar_env=RegVar.Map.empty,tyvar_env=L.TyvarMap.empty},
			 fn {tyname_env=te,con_env=ce,excon_env=ee,lvar_env=le,
                             regvar_env=_,tyvar_env=_} => (te,ce,ee,le))
	                (Pickle.tup4Gen0(TyNameMap.pu TyName.pu pu_arity,
			                 ConMap.pu Con.pu (Pickle.debugUnpickle "con_env_range" R.pu_sigma),
			                 ExconMap.pu Excon.pu (Pickle.debugUnpickle "excon_env_range" R.pu_mu),
			                 LvarMap.pu Lvar.pu (Pickle.debugUnpickle "lvar_env_range" pu_lvar_env_range)))
	 )
  end
