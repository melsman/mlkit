(*$RegionStatEnv: REGION_STAT_ENV RTYPE EFFECT LVARS CON EXCON TYNAME
                  CRASH PRETTYPRINT LAMBDA_EXP MONO_FINMAP FLAGS*)

functor RegionStatEnv(structure R: RTYPE
		      structure E: EFFECT
                        sharing type R.cone = E.cone
			    and type R.effect = E.effect
			    and type R.runType = E.runType
                      structure TyName: TYNAME
                        sharing type  TyName.TyName = R.tyname
		      structure Con: CON
		      structure Excon: EXCON
		      structure Lvar: LVARS
		      structure Crash: CRASH
		      structure TyNameMap: MONO_FINMAP
		       sharing type TyNameMap.dom = TyName.TyName
		      structure ConMap: MONO_FINMAP
		       sharing type ConMap.dom = Con.con
		      structure ExconMap: MONO_FINMAP
		       sharing type ExconMap.dom = Excon.excon
		      structure LvarMap: MONO_FINMAP
		       sharing type LvarMap.dom = Lvar.lvar
		      structure L: LAMBDA_EXP
			sharing type L.tyvar = R.tyvar
			sharing type L.Type = R.LambdaType
                      structure Flags: FLAGS
		      structure PP: PRETTYPRINT
			sharing type PP.StringTree = R.StringTree = 
			  E.StringTree = ConMap.StringTree = ExconMap.StringTree =
			  TyNameMap.StringTree = LvarMap.StringTree 
		        sharing type L.TyName = TyName.TyName)

  : REGION_STAT_ENV =

  struct

    fun die s = Crash.impossible ("RegionStatEnv." ^ s)
    fun log s = output(std_out,s ^ "\n")
    fun log_st st = PP.outputTree(fn s => output(std_out,s), st, 70)
    fun layout_effects effects = PP.NODE{start="[",finish="]",indent=1,childsep=PP.RIGHT ",",
					 children=map E.layout_effect effects}
    fun dump(t: PP.StringTree):unit = 
        PP.outputTree(fn s => output(!Flags.log,s), t, !Flags.colwidth)

      (* arity: number of quantified type variables, 
		list of runtime types,
		number of effect  variables *)
    type runType = E.runType
    type arity = int * E.runType list * int 
    fun mk_arity x = x
    fun un_arity x = x

    type il = R.il 
    type cone = R.cone

    type instance_list = (il * (il * cone -> il * cone)) ref list

    type lvar_env_range = bool * bool * R.sigma * R.place 
			* instance_list ref Option * (il ->unit)Option

    type regionStatEnv = {tyname_env:  arity TyNameMap.map,
			  con_env: R.sigma ConMap.map,
			  excon_env: (R.Type * R.place) ExconMap.map,
			  lvar_env: lvar_env_range LvarMap.map
			 }
    type con = Con.con                           (* Unqualified value constructors. *)
    type excon = Excon.excon			(* Unqualified exception constructors.*)
    type TyName = TyName.TyName
    type lvar = Lvar.lvar			(* Unique lambda identifiers. *)
    type TypeAndPlaceScheme = R.sigma
     and TypeAndPlace = (R.Type * R.place)
     and Type = R.Type
     and place = R.place

    val empty: regionStatEnv = {tyname_env = TyNameMap.empty,
				con_env = ConMap.empty,
				excon_env = ExconMap.empty,
				lvar_env = LvarMap.empty}
    val tyname_env0= TyNameMap.add(TyName.tyName_STRING, (0,[],0),
		     TyNameMap.add(TyName.tyName_REAL, (0,[],0),
		     TyNameMap.add(TyName.tyName_EXN, (0,[],0),
		     TyNameMap.add(TyName.tyName_REF, (1,[],0),
		     TyNameMap.add(TyName.tyName_BOOL, (0,[],0),
		     TyNameMap.add(TyName.tyName_INT, (0,[],0), 
		     TyNameMap.add(TyName.tyName_CHAR, (0,[],0), 
		     TyNameMap.add(TyName.tyName_WORD, (0,[],0), 
		     TyNameMap.add(TyName.tyName_LIST, (1,[E.TOP_RT],0),  (* the auxliry region is for a pair; hence TOP_RT *)
		     TyNameMap.empty)))))))))

    local
	val c = E.initCone
	val lev0 = E.level c
	val c = E.push c
	fun mkListType(mu,rho) = R.CONSTYPE(TyName.tyName_LIST, [mu], [rho], [])

	val (c,bool_sigma,_) =  R.generalize_all (c, lev0, [],
			  (R.CONSTYPE(TyName.tyName_BOOL,[],[],[])))
	val alpha = L.fresh_tyvar()
	val alpha_ty = R.TYVAR alpha
	val (rho,c) = E.freshRho c
	val (rho',c) = E.freshRhoWithTy(E.TOP_RT, c)
	val (c,nil_sigma,_) = 
	       R.generalize_all (c, lev0,[alpha], mkListType((alpha_ty,rho), rho'))
	val (rho'',c) = E.freshRhoWithTy(E.TOP_RT, c)
	val alpha_rho_list = (mkListType((alpha_ty,rho), rho'), rho'')
	val (arreff, c) = E.freshEps c
	val _ = E.edge(arreff, E.mkPut rho'')

	val cons_mu = R.FUN([(R.RECORD[(alpha_ty,rho),alpha_rho_list],
							rho')],
				       arreff,
				       [alpha_rho_list])
	val (c,cons_sigma,_) = R.generalize_all (c, lev0, [alpha], cons_mu)
    in
      val conenv0 = ConMap.fromList [(Con.con_TRUE, bool_sigma),
				     (Con.con_FALSE, bool_sigma),
				     (Con.con_NIL, nil_sigma),
				     (Con.con_CONS, cons_sigma)]
    end

    local  (* types of built-in lvars *)

      fun lookup_tyname tyname = TyNameMap.lookup tyname_env0 tyname (* 17/10/96-Martin *)
  (*
	   case TyNameMap.lookup tyname_env0 tyname of 
	    Some (a, l, c) => Some(a, List.size (l), c)
	  | None => None
  *)
      val (mkTy, mkMu) = R.freshType lookup_tyname

      fun mkMus(taus,B) =
	  case taus of [] => ([], B)
	  | tau::rest => 
	     let val (mu', B) = mkMu(tau,B)
		 val (mus',B) = mkMus(rest,B)
	     in
		 (mu'::mus', B)
	     end	
      val B0 = E.initCone
      val lev0 = E.level B0

      (* exoClos(B, tau1, tau2): make an type scheme for exomorphism of
	 LambdaExp type tau1 -> tau2. In the resulting region type scheme,
	 there will be a Get on each region variable in the domain type
	 and a Put on each region variable in the result type. *)

      fun exoClos(B,taus1,taus2) = 
	   let val B = E.push B0
	       val (mus1,B) = mkMus(taus1,B)
	       val (mus2,B) = mkMus(taus2,B)
	       val ann1 = R.ann_mus mus1 []
	       val ann2 = R.ann_mus mus2 []
	       val (eps,B) = E.freshEps B
	       val _ = List.apply (fn rho => E.edge(eps,E.mkGet rho)) ann1
	       val _ = List.apply (fn rho => E.edge(eps,E.mkPut rho)) ann2
	       val (B, sigma, msg) = R.regEffClos(B, lev0, E.empty, R.FUN(mus1,eps,mus2))
	       val (_, B) = E.pop B

	       (* evaluate eps *)
	       val _ = E.eval_phis [eps] 
	   in sigma
	   end

      fun cl(taus,tau) = exoClos(B0, taus, [tau])

      val Int = L.intType
      val Real = L.realType
      val Bool = L.boolType
      val String = L.stringType
      val Exn = L.exnType
      val Unit = L.unitType
      val StringList = L.CONStype([String], TyName.tyName_LIST)
      val Instream = L.intType
      val Outstream = L.intType

      val int2bool = cl([Int],Bool)
      val int2string = cl([Int],String)
      val int2int = cl([Int],Int)
      val int2unit = cl([Int], Unit)
      val int2real = cl([Int], Real)
      val intXexn2int = cl( [Int, Exn], Int)
      val intXexn2string = cl([Int,Exn], String)
      val intXint2int = cl([Int,Int],Int)
      val intXint2bool = cl([Int,Int],Bool)
      val intXint2string = cl([Int,Int],String)
      val intXintXexn2int = cl([Int,Int,Exn],Int)
      val intXstringXexn2unit = cl ([Int,String,Exn],Unit)
      val real2int = cl([Real],Int)
      val real2real = cl([Real],Real)
      val realXexn2int = cl([Real,Exn],Int)
      val realXexn2real = cl([Real,Exn],Real)
      val realXreal2bool = cl([Real,Real],Bool)
      val realXreal2real = cl([Real,Real],Real)
      val realXrealXexn2real = cl([Real,Real,Exn],Real)
      val string2int = cl ([String], Int)
      val string2stringList = cl([String], StringList)
      val string2unit = cl([String], Unit)
      val stringXexn2int = cl([String,Exn], Int)
      val stringList2string = cl([StringList], String)
      val stringXexn2instream = cl([String,Exn], Instream)
      val stringXexn2outstream = cl([String,Exn], Outstream)
      val instreamXint2string = cl([Instream,Int], String)
      val instream2string = cl([Instream], String)
      val instream2unit = cl([Instream], Unit)
      val instream2bool = cl([Instream], Bool)
      val outstreamXstringXexn2unit = cl([Outstream,String,Exn], Unit)
      val outstream2unit = cl([Outstream], Unit)

      val lvars_and_sigmas_functions = let open Lvar in [

	(* Compiler-supported primitives; see SpreadExpression *)

	  (plus_int_lvar, intXint2int),                  (* integer operations *)
	  (minus_int_lvar, intXint2int),
	  (mul_int_lvar, intXint2int),
	  (div_int_lvar, intXint2int),
	  (negint_lvar, int2int),
	  (absint_lvar, int2int),
	  (less_int_lvar, intXint2bool),
	  (lesseq_int_lvar, intXint2bool),
	  (greater_int_lvar, intXint2bool),
	  (greatereq_int_lvar, intXint2bool),

	  (plus_float_lvar, realXreal2real),             (* real operations *)
	  (minus_float_lvar, realXreal2real),
	  (mul_float_lvar, realXreal2real),
	  (div_float_lvar, realXreal2real),
	  (negfloat_lvar, real2real),
	  (absfloat_lvar, real2real),
	  (less_float_lvar, realXreal2bool),
	  (greater_float_lvar, realXreal2bool),
	  (lesseq_float_lvar, realXreal2bool),
	  (greatereq_float_lvar, realXreal2bool),

	(* Non-compiler-supported primitives; Later these should be ommitted. *)

	  (floor_lvar, realXexn2int),                    (* real operations *)
	  (real_lvar, int2real),
	  (sqrt_lvar, realXexn2real),
	  (sin_lvar, real2real),
	  (cos_lvar, real2real),
	  (arctan_lvar, real2real),
	  (exp_lvar, realXexn2real),
	  (ln_lvar, realXexn2real),

	  (open_in_lvar, stringXexn2instream),           (* streams *)
	  (open_out_lvar, stringXexn2outstream),
	  (input_lvar,instreamXint2string),
	  (lookahead_lvar, instream2string),
	  (close_in_lvar, instream2unit),
	  (end_of_stream_lvar, instream2bool),
	  (output_lvar, outstreamXstringXexn2unit),
	  (close_out_lvar, outstream2unit),
	  (flush_out_lvar, outstream2unit),

	  (chr_lvar, intXexn2string),                   (* strings *)
	  (ord_lvar, stringXexn2int),
	  (size_lvar, string2int),
	  (explode_lvar, string2stringList),
	  (implode_lvar, stringList2string),        

	  (mod_int_lvar, intXintXexn2int),              (* others *)
	  (use_lvar, string2unit)

	 ]
       end (* open Lvar *)

    in

    val lvar_env00 = LvarMap.empty

    val lvar_env0 = 
       List.foldL (fn (lvar,sigma) => fn env =>
			LvarMap.add(lvar,(true,false,sigma,E.toplevel_region_withtype_top,None,None), env))
		  lvar_env00
		  lvars_and_sigmas_functions

    end

    val excon_env0 = ExconMap.fromList 
      (map (fn excon => (excon, (R.exnType,E.toplevel_region_withtype_top)))
       [Excon.ex_ABS, Excon.ex_NEG, Excon.ex_SUM, Excon.ex_DIFF,
	Excon.ex_PROD, Excon.ex_DIV, Excon.ex_MOD,
	Excon.ex_MATCH, Excon.ex_BIND])

    val initial: regionStatEnv ={tyname_env = tyname_env0,
				 con_env = conenv0,
				 excon_env = excon_env0,
				 lvar_env = lvar_env0}

    fun declareTyName(tyname,arity,rse as {tyname_env, con_env, excon_env,lvar_env})=
	{tyname_env = TyNameMap.add(tyname,arity,tyname_env), 
	 con_env= con_env,
	 excon_env = excon_env,
	 lvar_env =lvar_env}

    fun declareCon(con,sigma,rse as {tyname_env, con_env, excon_env,lvar_env})=
	{tyname_env = tyname_env,
	 con_env= ConMap.add (con,sigma,con_env),
	 excon_env = excon_env,
	 lvar_env =lvar_env}

    fun declareExcon(excon,mu,rse as {tyname_env, con_env, excon_env,lvar_env})=
	{tyname_env = tyname_env,
	 con_env= con_env,
	 excon_env = ExconMap.add (excon,mu,excon_env),
	 lvar_env =lvar_env}
    fun declareLvar(lvar,range,rse as {tyname_env, con_env, excon_env,lvar_env})=
	{tyname_env = tyname_env,
	 con_env= con_env,
	 excon_env = excon_env,
	 lvar_env = LvarMap.add(lvar,range,lvar_env)}

    fun plus(rse as {tyname_env, con_env, excon_env,lvar_env},
	     rse' as {tyname_env = tyname_env', con_env=con_env', excon_env=excon_env',lvar_env=lvar_env'})=
	{tyname_env = TyNameMap.plus(tyname_env, tyname_env'),
	 con_env = ConMap.plus(con_env, con_env'),
	 excon_env = ExconMap.plus(excon_env, excon_env'),
	 lvar_env = LvarMap.plus(lvar_env, lvar_env')}


    fun lookupTyName(rse : regionStatEnv as {tyname_env,...}) = TyNameMap.lookup tyname_env
    fun lookupCon(rse : regionStatEnv as {con_env,...}) = ConMap.lookup con_env
    fun lookupExcon(rse : regionStatEnv as {excon_env,...}) = ExconMap.lookup excon_env
    fun lookupLvar(rse  : regionStatEnv as {lvar_env,...}) = LvarMap.lookup lvar_env

    fun FoldLvar  f b (rse: regionStatEnv as {lvar_env, ...}) = LvarMap.Fold f b lvar_env
    fun FoldExcon f b (rse: regionStatEnv as {excon_env, ...}) = ExconMap.Fold f b excon_env

    val mapLvar : (lvar_env_range -> lvar_env_range) -> regionStatEnv -> regionStatEnv  =
	  fn f => fn (env as {tyname_env,con_env,excon_env,lvar_env}) =>
	     {tyname_env = tyname_env,
	      con_env= con_env,
	      excon_env=excon_env,
	      lvar_env = LvarMap.composemap f lvar_env}

    type cone = E.cone
    fun rhos_epss_free_rse (rse: regionStatEnv) =
      let val rhos_epss = FoldLvar (fn ((_,(_,_,sigma,rho,_,_)),acc) => 
				    R.ann_sigma sigma (rho::acc)) [] rse
	  val rhos_epss = FoldExcon (fn ((_,mu),acc) =>
				     R.ann_mus [mu] acc) rhos_epss rse
	  val toplevel : int = E.level E.initCone
	  val rhos_epss_free = 
	    List.foldL (fn node => fn acc => 
			case E.level_of node
			  of Some level => if level = toplevel then node :: acc
					   else acc
			   | None => die "mkConeToplevel.rhos_epss_free.node not rho or eps.") 
	    [] rhos_epss
	  val rhos_epss_free = E.remove_duplicates rhos_epss_free
	  fun closure ([],acc) = acc
	    | closure (rho_eps::rest,acc) =
	    closure(rest, let val rho_eps= E.find rho_eps in
                           if E.is_arrow_effect rho_eps then
			    List.foldL (fn node => fn acc =>
                                        let val node = E.find node in
  					  if E.is_arrow_effect node then node::acc
					  else if E.is_put node orelse E.is_get node then
					    E.rho_of node :: acc
					  else 
                                            (output(!Flags.log, "arrow effect:\n");
                                             dump(E.layout_effect_deep(rho_eps));
                                             output(!Flags.log, "atomic effect:\n");
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

    fun mkConeToplevel (rse: regionStatEnv) : cone =
      (E.reset_cone E.emptyCone;
       E.pushLayer(rhos_epss_free_rse rse,E.emptyCone))

    fun equal_sigma(sigma1,sigma2) = R.alpha_equal (sigma1,sigma2) E.initCone

    fun equal_lvar_res((b1,b1',sigma1,place1,_,_),(b2,b2',sigma2,place2,_,_)) =
      b1=b2 andalso b1' =b2' andalso E.eq_effect(place1,place2) andalso
      equal_sigma(sigma1,sigma2)

    fun equal_con_res(sigma1,sigma2) = equal_sigma (sigma1,sigma2)

    fun equal_excon_res((tau1,place1),(tau2,place2)) = 
      E.eq_effect(place1,place2) andalso 
      equal_sigma(R.type_to_scheme tau1,R.type_to_scheme tau2)

    fun enrich ({tyname_env, con_env, excon_env,lvar_env},
		{tyname_env=tyname_env1, con_env=con_env1, excon_env=excon_env1,lvar_env=lvar_env1}) =
      TyNameMap.enrich (op =) (tyname_env,tyname_env1) andalso
      ConMap.enrich equal_con_res (con_env,con_env1) andalso
      ExconMap.enrich equal_excon_res (excon_env,excon_env1) andalso
      LvarMap.enrich equal_lvar_res (lvar_env,lvar_env1)

    fun restrict_tyname_env(m,dom) = TyNameMap.restrict(m,dom)
      handle TyNameMap.Restrict => die "restrict_tyname_env"

    fun restrict_lvar_env(m,dom) = LvarMap.restrict(m,dom)
      handle LvarMap.Restrict => die "restrict_lvar_env"

    fun restrict_con_env(m,dom) = ConMap.restrict(m,dom)
      handle ConMap.Restrict => die "restrict_con_env"

    fun restrict_excon_env(m,dom) = ExconMap.restrict(m,dom)
      handle ExconMap.Restrict => die "restrict_excon_env"


    fun restrict({tyname_env, con_env, excon_env,lvar_env}, {tynames,cons,excons,lvars}) =
      {tyname_env=restrict_tyname_env(tyname_env,tynames),
       con_env=restrict_con_env(con_env,cons),
       excon_env=restrict_excon_env(excon_env,excons),
       lvar_env=restrict_lvar_env(lvar_env,lvars)}

    type effectvar = E.effect
    fun places_effectvarsRSE rse = 
      let val rhos_epss = rhos_epss_free_rse rse
	  val rhos = List.all E.is_rho rhos_epss
	  val epss = List.all E.is_arrow_effect rhos_epss
      in (rhos,epss)
      end

    type StringTree = PP.StringTree
    val layout_scheme  = R.mk_lay_sigma false (* do not omit region info *) 
    val (_,layout_mu) = R.mk_layout     false (* do not omit region info *) 

    fun layout_pair (_,_,sigma,p,_,_) = PP.NODE{start= "(", finish = ")", indent = 1, childsep = PP.RIGHT ",",
					children = [layout_scheme sigma, E.layout_effect p]}
    fun layout_arity(a,b,c) = 
	  PP.NODE{start = "(", finish  = ")",
		  indent = 1, childsep = PP.RIGHT ", ",
		  children = PP.LEAF (Int.string a) ::
			     (map (PP.LEAF o E.show_runType) b) @ [PP.LEAF (Int.string c)]}

    val layout_tyname_env = TyNameMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
					     (PP.LEAF o TyName.pr_TyName)
					     layout_arity
    val layout_con_env = ConMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
					     (PP.LEAF o Con.pr_con)
					     layout_scheme
    val layout_excon_env = ExconMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
					     (PP.LEAF o Excon.pr_excon)
					     layout_mu
    val layout_lvar_env=LvarMap.layoutMap {start = "{", eq = " -> ", finish = "}", sep = ","}
					     (PP.LEAF o Lvar.pr_lvar)
					     layout_pair 

    fun layout(rse as {tyname_env, con_env, excon_env,lvar_env}) =
	PP.NODE{start = "RegionStaticEnvironment:", finish = "(end of RegionStatEnvironment)",
		indent = 1, childsep = PP.RIGHT",",
		children = [layout_tyname_env tyname_env,
			    layout_con_env con_env,
			    layout_excon_env excon_env,
			    layout_lvar_env lvar_env]}

  end;

