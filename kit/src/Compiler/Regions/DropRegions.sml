(*$DropRegions: MUL_EXP AT_INF LVARS CRASH EFFECT DROP_REGIONS FINMAP
                FINMAPEQ PRETTYPRINT RTYPE REGION_STAT_ENV FLAGS *)

(* Drop Regions *)

functor DropRegions(structure MulExp : MUL_EXP
		    structure AtInf : AT_INF
		      sharing type AtInf.place = MulExp.place
			  and type AtInf.LambdaPgm = MulExp.LambdaPgm = MulExp.LambdaPgm
                          and type AtInf.LambdaExp = MulExp.LambdaExp
		    structure RSE : REGION_STAT_ENV
		      sharing type RSE.lvar = MulExp.lvar
		    structure RType : RTYPE
		      sharing type RType.sigma = RSE.TypeAndPlaceScheme
			  and type RType.place = MulExp.place = RSE.place = RType.effect
		    structure Lvars : LVARS
		      sharing type Lvars.lvar = MulExp.lvar
		    structure Crash : CRASH
		    structure FinMapEq : FINMAPEQ
		    structure Eff : EFFECT
		      sharing type Eff.place = MulExp.place = MulExp.effect
		    structure PP : PRETTYPRINT
		      sharing type PP.StringTree = Eff.StringTree = FinMapEq.StringTree = MulExp.StringTree
                                   = AtInf.StringTree
		    structure Flags : FLAGS
		      ) : DROP_REGIONS =
  struct
    open MulExp AtInf
    structure RE = RegionExp


    fun die s = Crash.impossible ("DropRegions."^s)

    fun log s = output(!Flags.log,s ^ "\n")
    fun device(s)         = output(!Flags.log, s)            
    fun dump(t)           = PP.outputTree(device, t, !Flags.colwidth)


   (* -----------------------------------------------------------------
    * Various functions
    * ----------------------------------------------------------------- *)

    fun rt_place place = case Eff.get_place_ty place
			   of Some rt => rt
			    | None => die "rt_place"


    fun pr_rho place = PP.flatten1 (Eff.layout_effect place)

    fun pr_rt Eff.WORD_RT = "word"
      | pr_rt Eff.STRING_RT = "string"
      | pr_rt Eff.REAL_RT = "real"
      | pr_rt Eff.BOT_RT = "bot"
      | pr_rt Eff.TOP_RT = "top"

    fun bot_region place = case rt_place place of Eff.BOT_RT => true | _ => false
    fun word_region place = case Eff.get_place_ty place
			      of Some rt => Eff.is_wordsize rt
			       | None => die "word_region"
    fun word_or_bot_region place = case rt_place place
				     of Eff.WORD_RT => true
				      | Eff.BOT_RT => true
				      | _ => false  

    fun drop_atplace (a as ATTOP place) = if word_region place then IGNORE else a
      | drop_atplace (a as ATBOT place) = if word_region place then IGNORE else a
      | drop_atplace (a as SAT place  ) = if word_region place then IGNORE else a
      | drop_atplace _ = die "drop_atplace failed (applied to IGNORE)"

    fun place_atplace (ATTOP place) = place
      | place_atplace (ATBOT place) = place
      | place_atplace (SAT place) = place
      | place_atplace _ = die "place_atplace"


									         (******************)
    local val bucket : place list ref = ref []                                   (* Visited fields *)
    in fun unvisit place = Eff.get_visited place := false		         (******************)
       fun reset_bucket () = (List.apply unvisit (!bucket); bucket := [])
       fun visit place = (Eff.get_visited place := true; bucket := (place :: !bucket))
       fun is_visited place = !(Eff.get_visited place)
    end


    (* drop_places(places, _) returns a list of booleans of the same length
       as places. The i'th boolean is true iff the i'th place should be kept
       after dropping. A region variable rho should be kept iff it has ever had
       a put effect on it. Earlier (in Version 2), the criterion was that
       rho should be kept, if there was a put(rho) effect amongst the arrow effects
       of the type of the function under consideration; but this is too weak, it turns
       out: there may be no put(rho) effect in the function type and yet be a put(rho)
       effect because rho is used as an actual argument to a region-polymorphic function
       which, however, is not called! An example is

	fun foldStateList f start = List.foldR (General.curry f) start numStateList

       where foldR and curry are defined (somewhat unusually) by

             fun curry(f: 'a*'b->'c)(x:'a)(y:'b):'c = f(x,y)
             fun foldR(f: 'b -> 'a -> 'a)(acc:'a)(l:'b list): 'a = acc

       Here curry f has a region parameter, rho, say for the region of the pair (x,y).
       In foldStateList the application (curry f) introduces an actual region, rho',
       corresponding to this pair, so rho' must not be dropped.
       However, foldR never calls its argument function, so foldStateList has no
       put on the rho'. Thus Version 2 would drop rho', leading to a compile time error
       (namely that rho' was never bound). The current version looks for a the hash-consed
       Put-node inside rho' and finds the put node that originated from the application
       of curry. Thus the present version will not drop rho'.

    *)


    fun drop_places(places, _ (*arreffs*) ) =         
      let   (*************************)
            (* a rho is marked if it *)
            (* should NOT be dropped *)
            (*************************)
        fun visit_put_rhos (places) = List.foldL Eff.acc_rho [] places
	fun unvisit_word_bot_rhos rhos = 
              List.apply (fn rho => if word_or_bot_region rho then unvisit rho else ()) rhos
	val l = visit_put_rhos places
	val _ = unvisit_word_bot_rhos places
	val bl = map is_visited places
      in (*reset_bucket();*)
         List.apply (fn rho=>Eff.get_visited rho:=false) l;
         bl
      end


    fun filter_bl([],[]) = []
      | filter_bl(true::bl,x::xs) = x :: filter_bl(bl,xs)
      | filter_bl(false::bl,x::xs) = filter_bl(bl,xs)
      | filter_bl _ = die "filter_bl"

   (* -----------------------------------------------------------------
    * Environment
    * ----------------------------------------------------------------- *)

    datatype env_res = FIXBOUND of bool list | NOTFIXBOUND
    type env = (lvar, env_res) FinMapEq.map
    val empty = FinMapEq.empty : env
    fun add (lv, res, env) = FinMapEq.add Lvars.eq (lv, res, env)
    fun lookup env lv = FinMapEq.lookup Lvars.eq env lv
                                                                           (*****************************)
    val init =                                                             (* create the initial env by *)
      let fun foldfn ((lvar, (compound,_,sigma,_,_,_)), env) =             (* folding over RSE.initial  *)
	    if compound then let val (_,places,arreffs) = RType.bv sigma   (*****************************)
				 val bl = drop_places(places,arreffs)
			     in add(lvar,FIXBOUND bl,env)
			     end
	    else add(lvar,NOTFIXBOUND,env)
      in RSE.FoldLvar foldfn empty RSE.initial
      end

    val plus = FinMapEq.plus Lvars.eq

    fun restrict(env,lvars) = 
      List.foldL(fn lv => fn acc =>
		 case lookup env lv
		   of Some res => add(lv,res,acc)
		    | None => die "restrict.lv not in env") empty lvars

    fun enrich(env1,env2) =
      FinMapEq.Fold(fn ((lv2,res2),b) => b andalso
		    case lookup env1 lv2
		      of Some res1 => res1=res2
		       | None => false) true env2 

    type StringTree = PP.StringTree
    fun layout_bool true = PP.LEAF "1"
      | layout_bool false = PP.LEAF "0"
    fun layout_env_res (FIXBOUND bl) = PP.NODE{start="FIXBOUND[",finish="]",indent=1,childsep=PP.NONE,
					       children=map layout_bool bl}
      | layout_env_res NOTFIXBOUND = PP.LEAF "NOTFIXBOUND"
    fun layout_lvar lv = PP.LEAF (Lvars.pr_lvar lv)
    val layout_env = FinMapEq.layoutMap {start="DROPENV(",finish=")",eq=" -> ",sep=", "} layout_lvar layout_env_res

    val export_env = ref empty




   (* -----------------------------------------------------------------
    * Do some checking
    * ----------------------------------------------------------------- *)

    fun check_atplace (rt, atplace) s =
      let val place = place_atplace atplace
	  val rt' = rt_place place
      in if rt = rt' then ()
	 else die ("check_atplace." ^ s ^ " : found runtype " ^ pr_rt rt' ^ " for " ^ pr_rho place ^ 
		   ", expected runtype " ^ pr_rt rt)
      end

    fun check_atp_w atp s = check_atplace (Eff.WORD_RT, atp) s
    fun check_atp_r atp s = check_atplace (Eff.REAL_RT, atp) s
    fun check_atp_s atp s = check_atplace (Eff.STRING_RT, atp) s
    fun check_atp_t atp s = check_atplace (Eff.TOP_RT, atp) s



   (* -----------------------------------------------------------------
    * Dropping regions
    * ----------------------------------------------------------------- *)

    exception AbortExpression 

    fun drop env (TR(e,metaType,ateffs,mulef)) =
      let
	fun drop_sw (SWITCH(tr,sel,opt)) =
	  let val sel' = map (fn (a,tr) => (a, drop env tr)) sel
	    val opt' = case opt of Some tr => Some (drop env tr) | None => None
	  in SWITCH(drop env tr,sel',opt')
	  end

	val e' =
	 (case e
	    of VAR {alloc=None,rhos_actuals=ref[],...} => e       (* fix-bound and prim lvars are dealed with below *)
	     | VAR _ => die "drop.should be fix-bound"
	     | INTEGER (n, atp) => (check_atp_w atp "INTEGER"; INTEGER (n, IGNORE))
	     | STRING (s, atp) => (check_atp_s atp "STRING"; e)
	     | REAL (s, atp) => (check_atp_r atp "REAL"; e)
	     | UB_RECORD trips => UB_RECORD (map (drop env) trips)
	     | FN{pat,body,free,alloc=atp} => (check_atp_t atp "FN_CLOS"; 
					       FN{pat=pat,body=drop env body,free=free,alloc=atp})
	     | LETREGION{B,rhos=ref rhos,body} =>
	      let fun drop_rhos [] = []
		    | drop_rhos ((rho as (p,mul))::rhos) =
		       if word_region p then drop_rhos rhos
		       else ((*if bot_region p then die "letregion bound variable with runtype BOT."
			     else ();mads *)   rho::drop_rhos rhos)
		  val rhos' = drop_rhos rhos
	      in LETREGION{B=B,rhos=ref rhos',body=drop env body}
	      end
	     | LET{k_let,pat,bind,scope} =>
	      let val env' = List.foldL(fn (lvar,_,_,_,_,_,_) => fn acc => 
					add(lvar,NOTFIXBOUND,acc)) env pat 
	      in LET{k_let=k_let,pat=pat,bind=drop env bind, scope=drop env' scope}
	      end
	     | FIX{free,shared_clos=atp,functions,scope} =>
	      let val _ = check_atp_t atp "FIX_CLOS"
		  type tr = lvar * bool list * (place * mul) list
		  fun tr_function {lvar,occ,tyvars,rhos,epss,Type,rhos_formals=ref formals,other,bind} : tr =
		    let val bl = drop_places(rhos,epss)
		        val formals' = filter_bl(bl,formals)
		    in (lvar, bl, formals')
		    end
		  val trs : tr list = map tr_function functions
		  val env' = List.foldL (fn (lvar,bl,_) => fn env => add(lvar,FIXBOUND bl,env)) env trs
		  fun new_functions [] [] = []
		    | new_functions ({lvar,occ,tyvars,rhos,epss,Type,rhos_formals,other,bind}::fcs)
		    ((lvar',_,formals)::trs') = (if not(Lvars.eq(lvar,lvar')) then die "new_functions.lvar"
						 else {lvar=lvar,occ = [], (* simpler to drop all occurrences 
                                                                              than to drop regions in them *)
                                                       tyvars=tyvars,
						       rhos=rhos,epss=epss,Type=Type,rhos_formals=ref formals,
						       other=other,bind=drop env' bind}:: new_functions fcs trs')
		    | new_functions _ _ = die "new_functions"
		  val functions' = new_functions functions trs
		  val scope' = drop env' scope
	      in FIX{free=free,shared_clos=atp,functions=functions',scope=scope'}
	      end
	     | APP(ck,sr,tr1 as TR(VAR{lvar,il,plain_arreffs, alloc, rhos_actuals=ref actuals, other},metaType,ateffs,mulef), tr2) =>
	      (case lookup env lvar
		 of Some (FIXBOUND bl) =>
		   let val _ = case alloc of Some atp => check_atp_t atp ("RHO_VECTOR." ^ Lvars.pr_lvar lvar) | None => ()
		       val actuals' = filter_bl(bl,actuals)
		   in APP(ck,sr,TR(VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,alloc=alloc,
				 rhos_actuals=ref actuals',other=other},metaType,ateffs,mulef), drop env tr2)
		   end
	          | _ => (case (alloc, actuals)
			       of (None, []) => APP(ck,sr,tr1, drop env tr2)
				| _ => die ("drop.APP(VAR= " ^ Lvars.pr_lvar lvar ^ " ,tr2)")))
	     | APP(ck,sr,tr1,tr2) => APP(ck,sr,drop env tr1, drop env tr2)
	     | EXCEPTION(excon,b,mu,atp,tr) => (check_atp_t atp "EXCEPTION"; EXCEPTION(excon,b,mu,atp,drop env tr))
	     | RAISE tr => RAISE (drop env tr)
	     | HANDLE (tr1,tr2) => HANDLE (drop env tr1, drop env tr2)
	     | SWITCH_I sw => SWITCH_I (drop_sw sw)
	     | SWITCH_S sw => SWITCH_S (drop_sw sw)
	     | SWITCH_C sw => SWITCH_C (drop_sw sw)
	     | SWITCH_E sw => SWITCH_E (drop_sw sw)
	     | CON0 {con, il, aux_regions, alloc} => 
		 let fun filter [] = []
		       | filter (IGNORE::xs) = filter xs
		       | filter (x::xs) = x :: filter xs
		 in CON0{con=con,il=il,aux_regions=filter(map drop_atplace aux_regions),
			 alloc=drop_atplace alloc}
		 end
	     | CON1 ({con, il, alloc}, tr) => CON1({con=con,il=il,alloc=drop_atplace alloc}, drop env tr)
	     | DECON (c, tr) => DECON (c, drop env tr)
	     | EXCON (excon, opt) => EXCON(excon, case opt
						    of Some (alloc,tr) => Some (drop_atplace alloc, drop env tr)
						     | None => None)
	     | DEEXCON (excon,tr) => DEEXCON(excon,drop env tr)
	     | RECORD (alloc, trs) => RECORD(drop_atplace alloc, map (drop env) trs)
	     | SELECT (i, tr) => SELECT(i,drop env tr)
	     | DEREF tr => DEREF (drop env tr)
	     | REF (alloc,tr) => REF(drop_atplace alloc, drop env tr)
	     | ASSIGN (alloc,tr1,tr2) => ASSIGN(drop_atplace alloc, drop env tr1, drop env tr2)
	     | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) => 
		 EQUAL ({mu_of_arg1=mu_of_arg1, mu_of_arg2=mu_of_arg2, alloc=drop_atplace alloc}, 
			drop env tr1, drop env tr2)
	     | CCALL ({name, resultMu, resultAllocs}, trs) =>
		 let fun filter [] = []
		       | filter (IGNORE::xs) = filter xs
		       | filter (x::xs) = x::filter xs
		 in CCALL ({name=name, resultMu=resultMu, resultAllocs=filter (map drop_atplace resultAllocs)}, map (drop env) trs)
		 end
	     | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => 
                  RESET_REGIONS ({force=force,alloc=drop_atplace alloc, 
                                  regions_for_resetting = map drop_atplace regions_for_resetting}, 
                                 drop env tr)
	     | FRAME{declared_lvars,declared_excons} =>
		 let val lvars = map #lvar declared_lvars
		   val env' = List.foldL (fn lv => fn env' =>
					  (case lookup env lv
					     of Some bl => add(lv,bl,env')
					      | None => die "drop.FRAME.lv not in env")) empty lvars
		   val _ = export_env := env'
		 in e
		 end
           ) handle  Crash.CRASH => 
                           (log "\nDropRegions failed at expression:";
                            dump(AtInf.layout_exp_brief e);
                            raise AbortExpression)

      in TR(e',metaType,ateffs,mulef)
      end

    fun drop_regions (env:env,
		      PGM{expression,
			  export_datbinds,
			  import_vars,
			  export_vars=(export_lvars,export_excons,export_rhos),
			  export_basis,
			  export_Psi}) =
      let val _ = export_env := empty
	  val expression' = drop env expression
                            handle AbortExpression =>
                             die "drop_regions failed"
	  val env' = ! export_env
	  val _ = export_env := empty
	  val export_rhos' = List.foldL (fn rho => fn places =>
					 if word_or_bot_region rho then places 
					 else rho::places) [] export_rhos
      in (PGM{expression=expression',
	      export_datbinds=export_datbinds,
	      import_vars=import_vars,
	      export_vars=(export_lvars, export_excons,export_rhos'),
	      export_basis=export_basis,export_Psi=export_Psi}, env')
      end

    fun drop_places places = List.dropAll word_region places


  end
