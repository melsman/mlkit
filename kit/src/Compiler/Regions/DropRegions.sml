
functor DropRegions(structure Name : NAME
		    structure ExCon: EXCON
                    structure MulExp : MUL_EXP
                    structure Mul: MUL
                      sharing type Mul.mulef = MulExp.mulef
                      sharing type Mul.mul = MulExp.mul
		    structure AtInf : AT_INF
		      sharing type AtInf.LambdaPgm = MulExp.LambdaPgm 
                      sharing type AtInf.LambdaExp = MulExp.LambdaExp
                      sharing type AtInf.mul = Mul.mul
		    structure RSE : REGION_STAT_ENV
		      sharing type RSE.lvar = MulExp.lvar
		    structure RType : RTYPE
		      sharing type RType.sigma = RSE.TypeAndPlaceScheme
                      sharing type MulExp.Type = RType.Type
		    structure Lvars : LVARS
		      sharing type Lvars.lvar = MulExp.lvar
		      sharing type Lvars.name = Name.name
		    structure Crash : CRASH
		    structure FinMapEq : FINMAPEQ
		    structure Eff : EFFECT
		      sharing type Eff.place = MulExp.place = MulExp.effect =
                                   RType.place = RSE.place = AtInf.place
		    structure PP : PRETTYPRINT
		      sharing type PP.StringTree = Eff.StringTree = FinMapEq.StringTree = MulExp.StringTree
                                   = AtInf.StringTree = Lvars.Map.StringTree
		    structure Flags : FLAGS
                    sharing type ExCon.excon = MulExp.excon
		      ) : DROP_REGIONS =
  struct

    structure EdList = Edlib.List

    structure LvarMap = Lvars.Map

    open MulExp AtInf

    val region_inference = Flags.is_on0 "region_inference"

    fun die s = Crash.impossible ("DropRegions."^s)

    fun log s = TextIO.output(!Flags.log,s ^ "\n")
    fun device s = TextIO.output(!Flags.log, s)            
    fun dump t = PP.outputTree(device, t, !Flags.colwidth)


   (* -----------------------------------------------------------------
    * Various functions
    * ----------------------------------------------------------------- *)

    fun rt_place place = case Eff.get_place_ty (Eff.find place)
			   of SOME rt => rt
			    | NONE => die "rt_place"


    fun pr_rho place = PP.flatten1 (Eff.layout_effect place)

    fun pr_rt Eff.WORD_RT = "word"
      | pr_rt Eff.STRING_RT = "string"
      | pr_rt Eff.PAIR_RT = "pair"
      | pr_rt Eff.BOT_RT = "bot"
      | pr_rt Eff.TOP_RT = "top"

    fun bot_region place = case rt_place place of Eff.BOT_RT => true | _ => false
    fun word_region place = case Eff.get_place_ty (Eff.find place)
			      of SOME rt => Eff.is_wordsize rt
			       | NONE => die "word_region"
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
    in fun unvisit place = Eff.get_visited(Eff.find place) := false              (******************)
       fun reset_bucket () = (List.app unvisit (!bucket); bucket := [])
       fun visit place = (Eff.get_visited(Eff.find  place) := true; bucket := (place :: !bucket))
       fun is_visited place = !(Eff.get_visited(Eff.find  place))
    end


    (* from version 2: *)

    fun drop_places(places, arreffs) =         (*************************)
      let                                      (* a rho is marked if it *)
	                              	       (* should NOT be dropped *)
                                               (*************************)
        val places = map Eff.find places
        val arreffs= map Eff.find arreffs
	fun visit_put_rhos [] = () 

	  | visit_put_rhos (arreff::arreffs) =
	  let fun visit_eval_effect effect = if Eff.is_put(Eff.find effect) then visit(Eff.rho_of(Eff.find effect)) else ()
	      val _ = List.app visit_eval_effect (Eff.represents arreff)
	  in visit_put_rhos arreffs
	  end
	fun unvisit_word_bot_rhos [] = ()
	  | unvisit_word_bot_rhos (rho::rhos) =
	  (if word_or_bot_region rho then unvisit rho
	   else (); unvisit_word_bot_rhos rhos)
	val _ = visit_put_rhos arreffs
	val _ = unvisit_word_bot_rhos places
	val bl = map is_visited places
      in reset_bucket(); bl
      end

    fun filter_bl([],[]) = []
      | filter_bl(true::bl,x::xs) = x :: filter_bl(bl,xs)
      | filter_bl(false::bl,x::xs) = filter_bl(bl,xs)
      | filter_bl _ = die "filter_bl"

   (* -----------------------------------------------------------------
    * Environment for Lambda Variables
    * ----------------------------------------------------------------- *)

    datatype env_res = FIXBOUND of bool list | NOTFIXBOUND
    type env = env_res LvarMap.map
    val empty = LvarMap.empty : env
    fun add (lv, res, env) = LvarMap.add (lv, res, env)
    fun lookup env lv = LvarMap.lookup env lv
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

    fun plus a = LvarMap.plus a

    fun restrict(env,lvars) = 
      foldl(fn (lv,acc) =>
	    case lookup env lv
	      of SOME res => add(lv,res,acc)
	       | NONE => die "restrict.lv not in env") empty lvars

    fun enrich(env1,env2) =
      LvarMap.Fold(fn ((lv2,res2),b) => b andalso
		     case LvarMap.lookup env1 lv2
		       of SOME res1 => res1=res2
			| NONE => false) true env2 

    type StringTree = PP.StringTree
    fun layout_bool true = PP.LEAF "1"
      | layout_bool false = PP.LEAF "0"
    fun layout_env_res (FIXBOUND bl) = PP.NODE{start="FIXBOUND[",finish="]",indent=1,childsep=PP.NOSEP,
					       children=map layout_bool bl}
      | layout_env_res NOTFIXBOUND = PP.LEAF "NOTFIXBOUND"
    fun layout_lvar lv = PP.LEAF (Lvars.pr_lvar lv)
    val layout_env = LvarMap.layoutMap {start="DROPENV(",finish=")",eq=" -> ",sep=", "} layout_lvar layout_env_res

    val export_env = ref empty




   (* -----------------------------------------------------------------
    * Environment for Region Variables
    * ----------------------------------------------------------------- *)

    datatype regenv_res = DROPIT | KEEP | LETREGION_INF (*to disable global regions*)
    type place = RType.place
    type regenv = (place, regenv_res) FinMapEq.map
    val empty_regenv = FinMapEq.empty : regenv
    fun add_regenv (rho, res, regenv) = FinMapEq.add Eff.eq_effect (rho, res, regenv)
    fun add_regenv' (rho, res, (env,renv)) = (env, add_regenv (rho,res,renv))
    type StringTree = PP.StringTree
    fun layout_regenv_res DROPIT = PP.LEAF "drop"
      | layout_regenv_res KEEP = PP.LEAF "keep"
      | layout_regenv_res LETREGION_INF = PP.LEAF "letregion_inf"
    val layout_regenv = FinMapEq.layoutMap 
           {start="Environment for region variables in DropRegions:(",finish=")",
            eq=" -> ",sep=", "} Eff.layout_effect layout_regenv_res


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
    fun check_atp_p atp s = check_atplace (Eff.PAIR_RT, atp) s
    fun check_atp_s atp s = check_atplace (Eff.STRING_RT, atp) s
    fun check_atp_t atp s = check_atplace (Eff.TOP_RT, atp) s


    fun filter [] = []
      | filter (IGNORE::xs) = filter xs
      | filter (x::xs) = x::filter xs

   (* maybe_add_rho regvar_env (rho, acc)   conses rho onto acc if
      rho is marked as DROPIT in regvar_env: *)

   fun maybe_add_rho regvar_env (rho, acc) = 
      case FinMapEq.lookup Eff.eq_effect regvar_env rho
        of SOME DROPIT => rho :: acc
      | _ =>  acc

   fun maybe_add regvar_env (atp, acc) =
     case atp of
       IGNORE => acc
     | ATTOP rho => maybe_add_rho regvar_env (rho, acc)
     | ATBOT rho => maybe_add_rho regvar_env (rho, acc)
     | SAT rho => maybe_add_rho regvar_env (rho, acc)

   (* -----------------------------------------------------------------
    * Dropping regions
    * ----------------------------------------------------------------- *)

    exception AbortExpression 

    fun drop (env as (lvar_env: env, regvar_env: regenv)) 
             (t0 as TR(e,metaType,ateffs,mulef)) (acc: place list) : 
             (place at, place*mul, unit)trip * place list=

      let
	fun S atp =
	  if region_inference() then atp
	  else
	    let 
	      fun subst rho =
		case FinMapEq.lookup Eff.eq_effect regvar_env rho
		  of SOME LETREGION_INF => ATTOP
		    (case Eff.get_place_ty (Eff.find rho)
		       of SOME Eff.STRING_RT => Eff.toplevel_region_withtype_string
			| SOME Eff.PAIR_RT => Eff.toplevel_region_withtype_pair
			| SOME _ => Eff.toplevel_region_withtype_top
			| NONE => die "S_atp.lookup")
		   | _ => atp
	    in case atp 
		 of IGNORE => atp
		  | ATTOP rho => subst rho
		  | ATBOT rho => subst rho
		  | SAT rho => subst rho
	    end

	fun drop_sw (SWITCH(tr,sel,opt)) acc =
	  let val (sel', acc) = List.foldr (fn ((a,tr), (trs, acc)) =>
                                            let val (tr', acc) = drop env tr acc
                                            in ((a, tr') :: trs, acc)
                                            end) ([] , acc) sel
	    val (opt',acc) = 
              case opt of 
                SOME tr => let val (tr', acc) = drop env tr acc
                           in (SOME tr', acc) end
              | NONE => (NONE, acc)
            val (tr', acc) = drop env tr acc
	  in (SWITCH(tr',sel',opt'), acc)
	  end

	val (e', acc) =
	 (case e
	    of VAR {fix_bound=false,rhos_actuals=ref[],...} => (e, acc)       (* fix-bound and prim lvars are dealt with below *)
	     | VAR _ => die "drop.should be fix-bound"
	     | INTEGER (n, t, atp) => 
	      if RType.unboxed t then (check_atp_w atp "INTEGER"; 
				       (INTEGER (n, t, IGNORE), acc))
	      else (check_atp_t atp "INTEGER_BOXED"; 
		    (INTEGER(n,t,S atp), maybe_add regvar_env (atp, acc)))
	     | WORD (w, t, atp) => 
	      if RType.unboxed t then (check_atp_w atp "WORD"; 
				       (WORD (w, t, IGNORE), acc))
	      else (check_atp_t atp "WORD_BOXED"; 
		    (WORD(w,t,S atp), maybe_add regvar_env (atp, acc)))
	     | STRING (s, atp) => (check_atp_s  atp "STRING";
                                   (STRING(s,S atp), maybe_add regvar_env (atp, acc)))
	     | REAL (s, atp) => (check_atp_t atp "REAL"; (REAL(s,S atp), maybe_add regvar_env (atp, acc)))
	     | UB_RECORD trips => 
                           let val (trips', acc) = List.foldr (fn (tr, (trs,acc)) =>
                                                    let val (tr', acc)= drop env tr acc
                                                    in (tr'::trs, acc)
                                                    end) ([], acc) trips
                           in (UB_RECORD trips', acc) 
                           end
	     | FN{pat,body,free,alloc=atp} => 
                           (check_atp_t atp "FN_CLOS"; 
                            let val (body', acc) = drop env body acc
                            in
                              (FN{pat=pat,body=body',free=free,alloc=S atp},
                               maybe_add regvar_env (atp, acc))
                            end)
	     | LETREGION{B,rhos=ref rhos,body} =>
	      let fun drop_rhos [] = []
		    | drop_rhos ((rho as (p,mul))::rhos) =
		       if word_region p then drop_rhos rhos
		       else ((*if bot_region p then die "letregion bound variable with runtype BOT."
			     else ();mads *)   rho::drop_rhos rhos)
		  val rhos' = drop_rhos rhos
		  val (rhos', env) = 
		    if region_inference() then (rhos', env)
		    else foldr (fn (rho as (p,Mul.INF),(rhos',env)) => (rhos',add_regenv'(p,LETREGION_INF,env))
                                 | (rho,(rhos',env)) => (rho::rhos',env)) (nil,env) rhos'		      
                  val (body', acc) = drop env body acc
	      in (LETREGION{B=B,rhos=ref rhos',body=body'}, acc)
	      end
	     | LET{k_let,pat,bind,scope} =>
	      let val env' = (List.foldl(fn ((lvar,_,_,_,_,_,_),acc) =>
					add(lvar,NOTFIXBOUND,acc)) lvar_env pat,
                              regvar_env)
                  val (scope', acc) = drop env' scope acc
                  val (bind', acc) = drop env bind acc
	      in (LET{k_let=k_let,pat=pat,bind=bind', 
                      scope=scope'}, acc)
	      end
	     | FIX{free,shared_clos=atp,functions,scope} =>
	      let val _ = check_atp_t atp "FIX_CLOS"
                  val acc = maybe_add regvar_env (atp, acc)
		  type tr = lvar * bool list * (place * mul) list * (place*mul) list * regenv 
		  fun tr_function {lvar,occ,tyvars,rhos,epss,
                                   Type,rhos_formals=ref formals,bound_but_never_written_into,
                                   other,bind} : tr =
		    let val bool_list = drop_places(rhos,epss)
		        val formals' = filter_bl(bool_list,formals)
                        val drop_formals' = (filter_bl (map not bool_list, formals))

                        (* regvars_env' extends regvar with one binding for each bound region
                           variable in the type scheme of the function. Every region variable
                           is mapped to KEEP or DROPIT *)

                        val regvar_env': regenv =  List.foldl
                                              (fn ((((rho,_),b:bool)), regenv) =>
                                                 add_regenv(rho, if b then KEEP else DROPIT, regenv))
                                              regvar_env
                                              (ListPair.zip(formals, bool_list)
                                               handle _ => die "FIX: formals and bool list have different lengths")
		    in (lvar, bool_list, formals', drop_formals',  regvar_env')
		    end
		  val trs : tr list = map tr_function functions
		  val lvar_env' = List.foldl (fn ((lvar,bool_list,_,_,_), env) =>
                                              add(lvar,FIXBOUND bool_list,env)) lvar_env trs

		  fun new_functions [] [] acc = ([], acc)
		    | new_functions ({lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                                      other,bind,bound_but_never_written_into}::fcs)
  		                     ((lvar',_,formals,drop_formals',regvar_env')::trs') acc = 
                      (if not(Lvars.eq(lvar,lvar')) then die "new_functions.lvar"
                       else 
                         let val (bind', acc1) = drop(lvar_env',regvar_env') bind []
                             val bound_but_never_written_into = (* drop_formals' intersect acc1 *)
                                 (* List.filter (fn (rho,_) => List.exists (fn rho' => 
                                             Eff.eq_effect(rho,rho')) acc1) *) drop_formals'
                             val propagate = (* acc1 setminus drop_formals' *)
                                  List.filter (fn rho => not(List.exists (fn (rho',_) => 
                                             Eff.eq_effect(rho,rho')) drop_formals')) acc1
                             val (rest, acc) = new_functions fcs trs' (propagate @ acc)
                         in
                          ({lvar=lvar,occ = [], (* simpler to drop all occurrences 
                                                  than to drop regions in them *)
                             tyvars=tyvars,
                             rhos=rhos,epss=epss,Type=Type,rhos_formals=ref formals,
                             bound_but_never_written_into = SOME(bound_but_never_written_into),
                             other=other,
                             bind=bind'}  (* function body traversed here *) 
                         :: rest, acc)
                         end
                       )
		    | new_functions _ _ acc = die "new_functions"
		  val (functions', acc) = new_functions functions trs acc
		  val (scope',acc) = drop (lvar_env', regvar_env) scope acc
	      in (FIX{free=free,shared_clos=S atp,functions=functions',scope=scope'}, acc)
	      end
	     | APP(ck,sr,tr1 as TR(VAR{lvar,il,plain_arreffs, fix_bound, 
                                       rhos_actuals=ref actuals, other},metaType,
                                   ateffs,mulef), tr2) =>
	      (case lookup lvar_env lvar
		 of SOME (FIXBOUND bool_list) =>
		   let
		       val actuals' = filter_bl(bool_list,actuals)
                       val acc = List.foldl (maybe_add regvar_env) acc actuals'
                       val (tr2', acc) = drop env tr2 acc
		       val actuals' = map S actuals'
		   in (APP(ck,sr,TR(VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,fix_bound=fix_bound,
				 rhos_actuals=ref actuals',other=other},metaType,ateffs,mulef), 
                          tr2'), acc)
		   end
	          | _ => (case (fix_bound, actuals)
			       of (false, []) => 
                                 let val (tr2', acc) = drop env tr2 acc
                                 in (APP(ck,sr,tr1,tr2'), acc)
                                 end
				| _ => die ("drop.APP(VAR= " ^ Lvars.pr_lvar lvar ^ " ,tr2)")))
	     | APP(ck,sr,tr1,tr2) => 
                 let val (tr1', acc) = drop env tr1 acc
                   val   (tr2', acc) = drop env tr2 acc
                 in
                   (APP(ck,sr,tr1',tr2'), acc)
                 end
	     | EXCEPTION(excon,b,mu,atp,tr) =>
                 let val acc = maybe_add regvar_env (atp, acc)
                     val (tr', acc) =  drop env tr acc
                 in check_atp_t atp "EXCEPTION"; 
                    (EXCEPTION(excon,b,mu,S atp, tr'), acc)
                 end
	     | RAISE tr => 
                 let val (tr', acc) = drop env tr acc in (RAISE (tr'), acc) end
	     | HANDLE (tr1,tr2) => 
                 let val (tr1', acc) = drop env tr1 acc
                     val (tr2', acc) = drop env tr2 acc
                 in
                   (HANDLE(tr1', tr2'), acc)
                 end
	     | SWITCH_I {switch, precision} => 
                 let val (switch', acc) = drop_sw switch acc
                 in (SWITCH_I {switch=switch', precision=precision}, acc)
                 end
	     | SWITCH_W {switch, precision} => 
                 let val (switch', acc) = drop_sw switch acc
                 in (SWITCH_W {switch=switch', precision=precision}, acc)
                 end
	     | SWITCH_S sw => 
                 let val (sw', acc) = drop_sw sw acc
                 in (SWITCH_S sw', acc)
                 end
	     | SWITCH_C sw => 
                 let val (sw', acc) = drop_sw sw acc
                 in (SWITCH_C sw', acc)
                 end
	     | SWITCH_E sw => 
                 let val (sw', acc) = drop_sw sw acc
                 in (SWITCH_E sw', acc)
                 end
	     | CON0 {con, il, aux_regions, alloc} => 
		 let val aux_regions' = filter(map drop_atplace aux_regions)
                     val acc = List.foldl (maybe_add regvar_env) acc (alloc::aux_regions')
		 in (CON0{con=con,il=il,
                          aux_regions=map S aux_regions',
                          alloc=S(drop_atplace alloc)}, acc)
		 end
	     | CON1 ({con, il, alloc}, tr) => 
                 let val (tr', acc) = drop env tr acc
                     val acc = maybe_add regvar_env (alloc, acc)
                 in
                   (CON1({con=con,il=il,
                          alloc=S(drop_atplace alloc)},tr'),
                    acc)
                 end
	     | DECON (c, tr) => 
                 let val (tr', acc) = drop env tr acc
                 in (DECON (c,tr'), acc)
                 end
	     | EXCON (excon, opt) => 
                 let val (tr_opt', acc) = 
                   case opt of
                     SOME (alloc,tr) => let val acc = maybe_add regvar_env (alloc, acc)
                                            val (tr', acc) = drop env tr acc
                                        in (SOME(S(drop_atplace alloc), tr'), acc)
                                        end
                   | NONE => (NONE, acc)
                 in
                   (EXCON(excon, tr_opt'), 
                    acc)
                 end
	     | DEEXCON (excon,tr) => let val (tr', acc) = drop env tr acc
                                     in (DEEXCON(excon, tr'), acc)
                                     end
	     | RECORD (alloc, trs) => 
                  let val acc = maybe_add regvar_env (drop_atplace alloc, acc)
                    val (trs', acc) = List.foldr (fn (tr, (trs, acc)) =>
                                                  let val (tr', acc) = drop env tr acc
                                                  in (tr'::trs, acc)
                                                  end)
                                      ([], acc) trs
                  in
                     (RECORD(S(drop_atplace alloc), trs'), acc)
                  end
	     | SELECT (i, tr) => 
                  let val (tr', acc) = drop env tr acc
                  in (SELECT(i, tr'), acc)
                  end
	     | DEREF tr => 
                  let val (tr', acc) = drop env tr acc
                  in (DEREF tr', acc)
                  end
	     | REF (alloc,tr) => 
                  let val acc = maybe_add regvar_env (alloc, acc)
                    val (tr', acc)= drop env tr acc
                  in
                    (REF(S(drop_atplace alloc),tr'), acc)
                  end
	     | ASSIGN (alloc,tr1,tr2) => 
                  let val acc = maybe_add regvar_env (alloc, acc)
                      val (tr1', acc) = drop env tr1 acc
                      val (tr2', acc) = drop env tr2 acc
                  in (ASSIGN(S(drop_atplace alloc), tr1', tr2'),
                      acc)
                  end
	     | DROP tr => 
                 let val (tr', acc) = drop env tr acc 
		 in (DROP (tr'), acc) 
		 end
	     | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) => 
                 let val acc = maybe_add regvar_env (alloc, acc)
                     val (tr1', acc) = drop env tr1 acc
                     val (tr2', acc) = drop env tr2 acc
                 in
                   (EQUAL ({mu_of_arg1=mu_of_arg1, 
                            mu_of_arg2=mu_of_arg2, 
                            alloc=S(drop_atplace alloc)}, tr1', tr2'), acc)
                 end
	     | CCALL ({name, mu_result, rhos_for_result}, trs) =>
		 let
                   val acc = List.foldl (maybe_add regvar_env) acc (map #1 rhos_for_result)
                   val (trs', acc) = List.foldr (fn (tr, (trs, acc)) =>
                                                 let val (tr', acc) = drop env tr acc
                                                 in (tr'::trs, acc)
                                                 end)
                                                ([],acc) trs
		   (*I do not think you can be sure that List.dropAll preserves
		    the order of the list, so filter is used:*)
		   fun filter [] = []
		     | filter ((IGNORE, _) :: xs) = filter xs
		     | filter ((_, SOME 0) :: xs) = die "filter: undropped region with size=0?"
		     | filter (x :: xs) = x :: filter xs
		 in(CCALL ({name = name,
			    mu_result = mu_result,
			    rhos_for_result =
			      filter (map (fn (atp, i_opt) =>
					   (S(drop_atplace atp), i_opt)) rhos_for_result)},
			   trs'), acc)
		 end
	     | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => 
                 let 
                   val acc = maybe_add regvar_env (alloc, acc)
                   val regions_for_resetting' = map drop_atplace regions_for_resetting
                   val acc = List.foldl (maybe_add regvar_env) acc regions_for_resetting'
                   val (tr', acc) = drop env tr acc
		   val regions_for_resetting' =
		     if region_inference() then regions_for_resetting'
		     else nil
                 in
                  (RESET_REGIONS ({force=force,alloc=S(drop_atplace alloc), 
                                  regions_for_resetting = regions_for_resetting'}, 
                                 tr'), acc)
                 end
	     | FRAME{declared_lvars,declared_excons} =>
		 let val lvars = map #lvar declared_lvars
		   val lvar_env' = List.foldl (fn (lv, env') =>
					  (case lookup lvar_env lv
					     of SOME bool_list => add(lv,bool_list,env')
					      | NONE => die "drop.FRAME.lv not in env")) empty lvars
		   val _ = export_env := lvar_env'
		 in (e, acc)
		 end
           ) handle  AbortExpression => raise AbortExpression
                  | _  => 
                           (log "\nDropRegions failed at expression:";
                            dump(AtInf.layout_exp_brief e);
                            raise AbortExpression)
      in (TR(e',metaType,ateffs,mulef), acc)
      end

    fun drop_regions (env:env,
		      PGM{expression,
			  export_datbinds,
			  import_vars,
			  export_vars=(export_lvars,export_excons,export_rhos),
			  export_basis,
			  export_Psi}) =
      let val _ = export_env := empty
	  val (expression', acc) = drop (env, empty_regenv) expression []
                            handle AbortExpression =>
                             die "drop_regions failed"
          val _ = case acc of [] => ()
                    | _ => die "non-empty list of regions to be dropped at top-level"
	  val env' = ! export_env
	  val _ = export_env := empty
	  val export_rhos' = List.foldl (fn (rho, places) =>
					 if word_or_bot_region rho then places 
					 else rho::places) [] export_rhos
      in (PGM{expression=expression',
	      export_datbinds=export_datbinds,
	      import_vars=import_vars,
	      export_vars=(export_lvars, export_excons,export_rhos'),
	      export_basis=export_basis,export_Psi=export_Psi}, env')
      end

    fun drop_places places = EdList.dropAll word_region places


  end
