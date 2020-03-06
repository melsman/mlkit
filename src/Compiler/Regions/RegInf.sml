(***************)
(* Algorithm R *)
(* Mads Tofte  *)
(***************)

structure RegInf : REGINF =
struct
  structure RSE = RegionStatEnv
  structure Exp = RegionExp
  structure Lvar = Lvars
  type cone = RType.cone
  type place = RType.place
  type effect = RType.effect
  type trip = (place,unit)Exp.trip (*redefined at the end of the functor *)
  type exp  = (place,unit)Exp.LambdaExp
  type prog = (place,unit)Exp.LambdaPgm
  type rse  = RSE.regionStatEnv

  val dangling_pointers = Flags.is_on0 "dangling_pointers"
  val print_regions = Flags.is_on0 "print_regions"

  fun footnote(x,y) = x
  infix footnote

  fun uncurry f (x,y) = f x y

  infix &&
  fun (Effect.Lf[]) && d = d
    | d && (Effect.Lf[]) = d
    | d1 && d2 = Effect.Br(d1,d2)
  val delta_emp = Effect.Lf[]

  exception AbortExp  (* Region inference of any expression is
                         enclosed in a handle which handles any
                         exception - except AbortExp - (typically Crash.impossible)
                         and converts it to AbortExp. Then AbortExp
                         is propagated to the top-level call of
                         the region inference algorithm, which
                         calls Crash.impossible *)

  fun die msg = Crash.impossible ("RegInf." ^ msg)

  fun say s = TextIO.output(TextIO.stdOut, s)

  fun show_sigma sigma = PrettyPrint.flatten1(RType.mk_lay_sigma false (sigma))

  fun instClever(sigma,il) y = RType.instClever(sigma,il) y
  fun unify_ty(tau,tau_1) = RType.unify_ty(tau,tau_1)
  fun matchSchemes x = RType.matchSchemes x
  fun insert_alphas x = RType.insert_alphas x
  fun alpha_rename x= RType.alpha_rename x
  fun regEffClos x = RType.regEffClos x
  fun alpha_equal x y = RType.alpha_equal x y
  fun ann_mus x y= RType.ann_mus x y

  fun update_areff x = Effect.update_areff x
  fun update_increment x = Effect.update_increment x
  fun lower_delta x = Effect.lower_delta x
  fun pushLayer x = Effect.pushLayer x
  fun sort x = Effect.sort x
  fun pop x = Effect.pop x
  fun current_increment x = Effect.current_increment x
  fun observeDelta x = Effect.observeDelta x
  fun popAndClean B  = Effect.popAndClean B

  fun Below(B, mus) =
    let val free_rhos_and_epss = ann_mus mus []
        val B' = foldl  (uncurry (Effect.lower(Effect.level B - 1))) B free_rhos_and_epss
                 handle _ => die "Below.lower failed\n"
    in
        popAndClean(B')
            handle _ => die "Below.popAndClean failed\n"
    end


  fun retract(B, body as Exp.TR(e, Exp.Mus mus, phi),
              delta_phi_body: Effect.delta_phi,
              discharged_basis: effect list ref,
              discharged_phi: effect list ref,
              old_effect_of_letregion): cone * Effect.delta_phi =
        let
          (*val () = print "[Retract..."*)
          val (B_discharge,B_keep) = Below(B, mus)

             (* Nodes of level at most level(B)-1 which appear in the increment of
                an arrow effect which becomes letregion-bound (i.e., has same level as B)
                must be considered as increments to the letregion expression. *)

          val (new_discharged_phi,delta_letregion) =
                 observeDelta(Effect.level B_keep,
                              Effect.Lf (!discharged_phi) && delta_phi_body,
                              old_effect_of_letregion)
          (* old_effect_of_letregion has now been increased by delta_letregion *)
        in discharged_basis:= B_discharge;
           discharged_phi:= new_discharged_phi;
           (*print "]\n";*)
           (B_keep, delta_letregion)
        end
    | retract(B, t,_,_,_,_) = (B, delta_emp)



  fun inferEffects(device: string-> unit) =
  let
    val layoutExp = Exp.layoutLambdaExp(if print_regions()
                                        then (fn rho => SOME(PrettyPrint.LEAF("at "
                                              ^ PrettyPrint.flatten1(Effect.layout_effect rho))))
                                        else (fn rho => NONE))
                                       (fn () => NONE)
    fun sayCone B = PrettyPrint.outputTree(device,Effect.layoutCone B,!Flags.colwidth);
    fun sayLn s = (TextIO.output(TextIO.stdOut, s ^ "\n") (* ;
                   device(s ^ "\n") *))
    fun logtree(t:PrettyPrint.StringTree) = PrettyPrint.outputTree(device, t, !Flags.colwidth)
    fun log_sigma(sigma_5hat, f) =
      (case RType.bv sigma_5hat of
         ([], _, _) =>
           (sayLn ("***" ^ Lvar.pr_lvar f ^ " is:");
            logtree(RType.mk_lay_sigma false sigma_5hat);
            device "\n")
       | (alpha::alphas,[],_) =>
           (sayLn ("******" ^ Lvar.pr_lvar f ^ " is polymorphic with escaping regions:");
            logtree(RType.mk_lay_sigma false sigma_5hat);
            device "\n")
       | (alpha::alphas,_,_) =>
           (sayLn ("***" ^ Lvar.pr_lvar f ^ " is polymorphic:");
            logtree(RType.mk_lay_sigma false sigma_5hat);
            device "\n"));

    val count_visited = ref 0  (* number of nodes visited in the abstract syntax tree *)
    val count_RegEffClos = ref 0 (* number of times regEffClos is called *)

    fun show_visited  result =
    (device ("Visited (number of nodes visited during R)" ^ Int.toString(! count_visited) ^ "\n");
     device ("RegEffGen (number of times called during R)" ^ Int.toString(! count_RegEffClos) ^ "\n");
     result)

    (*
     * When garbage collection is enabled, we must make sure that no
     * dangling pointers are introduced; a way to ensure this is to
     * enforce the following side condition in rule 22 of TT'93:
     *
     *     forall y in fv(\x.e) . frev(TE(y)) \subseteq frev(mu)
     *
     * As Mads pointed out at a meeting June 1st 2001, for the
     * algorithm to work correctly, it is important that the side
     * condition is closed under substitution, which is not the case
     * for the weaker side condition mentioned on page 50 of Tofte &
     * Talpin, A Theory of Stack Allocation in Polymorphically Typed
     * Languages.  1993. Technical Report.
     *
     * When an arrow effect within the body of a FIX-function is
     * updated, it is necessary to rerun R on the entire FIX to make
     * sure that dependent effects are updated correctly (this
     * happens during effectinstantiation). The ref gc_arrow_effect_update
     * is used for this; the ref last_gc_arrow_effect_update is used
     * for ensuring that R is rerun accordingly also in the presence of
     * nested FIX's. mael 2005-02-11...
     *)

    val gc_arrow_effect_update = ref false
    fun gc_compute_delta(rse,free,(ty0,rho0)) =
      if dangling_pointers() then delta_emp
      else
	let
          val fv_sigma = RType.ferv_sigma    (*was: frv_sigma*)
	  fun effects_lv (lv, acc: effect list) : effect list =
	    case RSE.lookupLvar rse lv
	      of SOME(_,_,_,sigma,p,_,_) => p :: fv_sigma sigma @ acc
	       | NONE => die "gc_compute_delta.effects_lv"
	  fun effects_ex (ex, acc: effect list) : effect list =
	    case RSE.lookupExcon rse ex
	      of SOME (ty,p) => p :: fv_sigma (RType.type_to_scheme ty) @ acc
	       | NONE => die "gc_compute_delta.effects_ex"
	  val (lvs,exs) = case free
			    of SOME p => p
			     | NONE => die "gc_compute_delta.free variables not annotated"
	  val effects = foldl effects_ex (foldl effects_lv nil lvs) exs
	  val effects = Effect.remove_duplicates effects

	  val effects_not =
	    [rho0,
	     Effect.toplevel_arreff,
	     Effect.toplevel_region_withtype_top,
	     Effect.toplevel_region_withtype_bot,
	     Effect.toplevel_region_withtype_word,
	     Effect.toplevel_region_withtype_string,
	     Effect.toplevel_region_withtype_pair,
	     Effect.toplevel_region_withtype_array,
	     Effect.toplevel_region_withtype_ref,
	     Effect.toplevel_region_withtype_triple] @
	    RType.ferv_sigma(RType.type_to_scheme ty0)
	  val effects = Effect.setminus(effects, effects_not)

	  val effects = map (fn e => if Effect.is_rho e then Effect.mkGet e
				     else if Effect.is_put e orelse Effect.is_get e then
				       die "gc_compute_delta.put or get"
				     else e) effects
	  val _ = app (fn e => if Effect.is_rho e then die "gc_compute_delta.is_rho"
			       else ()) effects
(*
	  val _ = print ("New effects are " ^ PrettyPrint.flatten1 (PrettyPrint.layout_list Effect.layout_effect_deep effects) ^ "\n")
*)
	  (* Statistics *)
	  fun incr r n = r := !r + n
	  val _ = if length effects > 0 then
                     (gc_arrow_effect_update := true;
		      incr Flags.Statistics.no_dangling_pointers_changes 1;
		      incr Flags.Statistics.no_dangling_pointers_changes_total (length effects))
		  else ()
	in Effect.Lf effects
	end

     fun R(B:cone, rse: rse, t as Exp.TR(e, mt: Exp.metaType, phi: effect)): cone * Effect.delta_phi =
      let
(*
        val () = if !count_visited mod 100 = 0 then
                   print ("R(" ^ Int.toString (!count_visited) ^ ")")
                 else ()
        val () = print ("R(" ^ Int.toString (!count_visited) ^ ")")
        val () = if !count_visited mod 10 = 0 then
                   print (Effect.info B)
                 else ()
*)
        fun R_sw(B,rse, Exp.SWITCH(t1, rules, t_opt)) =
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = foldl (fn ((lhs,rhs), (B,d)) =>
                                      let val (B',d') = R(B,rse,rhs)
                                      in (B', d && d')
                                      end) (B,d1) rules
           in
               case t_opt of NONE => (B,d2)
               | SOME t => let val (B,d3) = R(B,rse,t)
                           in (B, d2 && d3)
                           end
           end
        fun pr s = if false andalso !count_visited >= 10200 then
                     (print s;
                      if s="v" then Effect.profGlobalIncs()
                      else ())
                   else ()
      in count_visited:= !count_visited+1;
       (case e of
         Exp.VAR{lvar, il_r = il_r as ref(il,f), fix_bound} =>
         let val (il, B) = f(il, B)
         in il_r:= (il, fn p => p);
           (case RSE.lookupLvar rse lvar of
     	  SOME(_,_,_,sigma,place0,_, _) =>
                 let
                   val _ = pr "V"
                   val (tau_1,B,updates: (effect * Effect.delta_phi)list) = instClever(sigma,il)(B)
                     handle Crash.CRASH =>
                       die ("inst failed; type scheme:\n" ^
                             PrettyPrint.flatten1(RType.mk_lay_sigma false (sigma)) ^ "\n")
                   val _ = pr "-"
                 in
                   case mt of
                     Exp.Mus [(tau, _)] =>
                       (let val B' = (unify_ty(tau,tau_1)B handle _ => die "unify_ty failed\n");
                        in  pr "-";
                            List.app update_increment    updates;
                            pr "-";
                            List.app (update_areff o #1) updates    (* takes time; mael 2015-05-07 *)
                              handle _ => die "update_areff in VAR case";
                            (B',delta_emp) before pr "v"
                        end
                       )
                   | _ => die ("R.VAR{...}: bad metatype")
                 end
              | NONE => die ("R.VAR{...}: free lvar" ^ Lvar.pr_lvar lvar)
            )
         end
       | Exp.INTEGER _ => (B, delta_emp)
       | Exp.WORD _    => (B, delta_emp)
       | Exp.STRING  _ => (B, delta_emp)
       | Exp.REAL    _ => (B, delta_emp)
       | Exp.UB_RECORD ts => foldr(fn (t, (B, d)) =>
                                        let val (B', d') = R(B,rse,t) in (B', d && d') end)
                                        (B,delta_emp) ts
       | Exp.FN{pat, body, alloc, free} =>
           (case mt of
              Exp.Mus [mu0 as (ty,_)] =>
              (case RType.unFUN ty of
                 SOME(mus2,eps_phi0,mus1) =>
                 let
	            val rse' = foldl (fn ((lvar, mu as (tau,rho)), rse) =>
                          RSE.declareLvar(lvar, (false,false,[],
                                 RType.type_to_scheme tau, rho,NONE,NONE),
                                          rse)) rse
                              (ListPair.zip(map #1 pat, mus2))
                    val (B, delta_body) = R(B,rse', body)
		    val delta_gc = gc_compute_delta(rse,free,mu0)
		    val delta = delta_body && delta_gc
                    val lev_eps = case Effect.level_of eps_phi0 of SOME n => n | NONE => die "bad arrow effect (FN)"
 	            val _ = pr "L"
                    val B = lower_delta lev_eps delta B
 	            val _ = pr "-"
                 in
                    update_increment(eps_phi0, delta);
 	            pr "-";
                    update_areff(eps_phi0);   (* takes time; mael 2015-05-07 *)
 	            pr "l";
                    (B, delta_emp)
                 end
               | NONE => die "R: FN expression had bad meta type")
            | _ => die "R: FN expression had bad meta type")


       | Exp.LETREGION_B{B = B1, discharged_phi, body} =>
           let
             val discharged_basis = !B1
             val B = pushLayer(discharged_basis,B) handle _ =>
                       die "pushLayer failed\n"
             val (B, delta_phi_body) = R(B,rse,body)

           in
             retract(B, body, delta_phi_body, B1, discharged_phi, phi)
           end
       | Exp.LET{pat = [(lvar, alphas, tau1',rho1')],
                 bind = bind (*as Exp.TR(_,Exp.Mus([(tau1',rho1')]), phi1')*),   (* mads, 13/3/97 *)
                 scope} =>
           (* case for one variable *)
           let
               val (B,d1) = R(B,rse,bind)
               val sigma' = RType.type_to_scheme tau1'
              (* val _ = log_sigma(RType.insert_alphas(alphas, sigma'), lvar)*)
               val rse' = RSE.declareLvar(lvar,(false,false,[],insert_alphas(alphas, sigma'),
                                                rho1', NONE, NONE),
                                          rse)
               val (B, d2) = R(B,rse', scope)
            in
               (B, d1 && d2)
            end
       | Exp.LET{pat = nil, bind = bind, scope} =>  (* wild card *)
           let val (B,d1) = R(B,rse,bind)
               val (B,d2) = R(B,rse,scope)
	   in (B, d1 && d2)
	   end
       | Exp.LET _ => die "LET.multiple bindings not implemented."
       | Exp.FIX{shared_clos = rho0,
                 functions,
                 scope = t2} =>
            let
              (* val _ = sayCone B*)
              fun addBindingForRhs ({lvar = f, occ, tyvars = alphavec,
                                     rhos as ref rhovec, epss as ref epsvec,
                                     Type = tau0, formal_regions,bind}, rse) =
                  let val sigma = RType.FORALL([] ,rhovec,epsvec,tau0)
                  in RSE.declareLvar(f,(true,true,[],sigma, rho0, SOME occ, NONE),rse)
                  end

              fun doOneRhs rse {lvar = f,occ,tyvars = alphavec,
                                rhos = rhosr as ref rhovec, epss = epssr as ref epsvec,
                                Type = tau0, formal_regions,
                                bind as Exp.TR(_,Exp.Mus[(tau4,rho4)],phi4)}=
              let
                    fun Rrec(B3,sigma_3hat,previous_functions_ok:bool) =
                      let
(*
                        val _ = sayLn("fix:entering Rrec " ^ Lvar.pr_lvar f ^ ":" ^ show_sigma sigma_3hat)
                        val _ = sayCone B3
                        val _ = sayLn("before rename , sigma is " ^ show_sigma (sigma_3hat))
*)
                        val sigma3_hat_save = alpha_rename(sigma_3hat,B3) (* for checking alpha_equality below *)
                               handle _ => die("failed to rename type scheme " ^
                                                show_sigma sigma_3hat)

			(* val _ = sayLn("after  rename , sigma is " ^ show_sigma sigma3_hat_save) *)
                        val rse' = RSE.declareLvar(f,(true,true,[],(*sigma3_hat_save*) sigma_3hat, rho0, SOME occ, NONE),rse) (*mads 5/2/97*)
                        val bv_sigma3_hat as (_,rhos,epsilons) = RType.bv sigma_3hat
                        val B3' = pushLayer(sort(epsilons@rhos), B3)
                                    handle _ => die "pushLayer failed\n"
                        val (B4,delta_rhs) = R(B3', rse',bind)   (* bind is a fn, so delta_rhs is empty *)
                        val _ = count_RegEffClos:= !count_RegEffClos+1
                        val (B5,sigma_5hat) = regEffClos(B4, Effect.level(B3),phi4,tau4)
                        val (_, B5) = pop(B5)
                        val (_,newrhos,newepss) = RType.bv sigma_5hat
			(* val _ = sayLn("sigma_5hat is " ^ show_sigma (sigma_5hat)) *)
                        (*val _ = Profile.profileOn();*)
                      in
                         if alpha_equal(sigma3_hat_save, sigma_5hat) B5 (*footnote Profile.profileOff()*)
                              handle _ => die ("alpha_equal failed\n" ^
                                               "sigma_3_hat_save = \n" ^ show_sigma sigma3_hat_save ^
                                               "\nsigma5_hat       = \n" ^ show_sigma sigma_5hat)
                         then
                                ((*sayLn("fix:  leaving " ^ Lvar.pr_lvar f);*)
                                 (*log_sigma(RType.insert_alphas(alphavec,sigma_5hat), f);*)
                                 (* update bindings in syntax tree *)
                                 rhosr:= newrhos;
                                 epssr:= newepss;
                                 (B5, previous_functions_ok)
                                )
                         else
                           let
                             val transformer = matchSchemes(sigma_3hat, sigma_5hat)
                                    handle RType.FAIL_MATCH msg =>
                                    die ("failed to match type schemes at FIX " ^ Lvar.pr_lvar f ^ "\n" ^ msg)
                             val _ = map (fn (r as ref(il,f)) => r:= (il, transformer o f)) (!occ)
                                    handle RType.FAIL_MATCH msg =>
                                    die ("failed to transform instantiation lists at FIX "
                                          ^ Lvar.pr_lvar f ^ "\n" ^ msg)
                           in
                             (* update bindings in syntax tree *)
                             rhosr:= newrhos;
                             epssr:= newepss;
			     (*sayLn("fix:looping for " ^ Lvar.pr_lvar f);*)
                             (B5,false)
                           end
                      end
               in
                   (fn B => Rrec(B, RType.FORALL([],rhovec,epsvec,tau0),true))
               end
		| doOneRhs _ _ = die "doOneRhs.wrong bind"

              fun loop {B, fcn=[], previous_were_ok=true, rse} = B
                | loop {B, fcn=[], previous_were_ok=false, rse} = loop {B=B,fcn=functions,previous_were_ok=true,rse=rse}
                | loop {B, fcn=fcn::rest, previous_were_ok,rse} =
                    let val (B, rhs_was_ok) = doOneRhs rse fcn B
                    in loop {B=B, fcn=rest, previous_were_ok=previous_were_ok andalso rhs_was_ok,
                             rse=addBindingForRhs (fcn,rse)}
                    end

              fun addBindingForScope ({lvar = f, occ, tyvars = alphavec,
                                       rhos as ref rhovec, epss as ref epsvec,
                                       Type = tau0, formal_regions,bind}, rse) =
                  let val sigma1hat' = RType.FORALL(alphavec ,rhovec,epsvec,tau0)
                  in RSE.declareLvar(f,(true,true,[],sigma1hat', rho0, SOME occ, NONE),rse)
                  end

              val B1 = loop {B=B,fcn=functions,previous_were_ok=true, rse=foldl addBindingForRhs rse functions}

              val rse' = foldl addBindingForScope rse functions
            in
		R(B1, rse', t2)
            end

       | Exp.APP(t1,t2) =>
           let val (B,d1) = R(B,rse,t1)
               val eps_phi0 =
                   case t1 of
                     Exp.TR(_, Exp.Mus [(ty,_)],_) =>
                     (case RType.unFUN ty of
                        SOME(_,eps_phi0,_) => eps_phi0
                      | NONE => die "APP: not function")
                   | _ => die "APP: not function"
               val (B,d2) = R(B,rse,t2)
           in  (B, current_increment eps_phi0 && d1 && d2)
               (*(B, Effect.Lf[eps_phi0] && d1 && d2)*)
           end
       | Exp.EXCEPTION(excon, nullary:bool, mu, alloc, t1) =>
                           R(B,RSE.declareExcon(excon,mu,rse),t1)
       | Exp.RAISE t1 => R(B,rse,t1)
       | Exp.HANDLE(t1,t2) =>
           let val (B,d1) = R(B,rse,t1)
               val eps_phi0 =
                   case t2 of
                     Exp.TR(_, Exp.Mus [(ty,_)],_) =>
                     (case RType.unFUN ty of
                        SOME(_,eps_phi0,_) => eps_phi0
                      | NONE => die "HANDLE: not function")
                   | _ => die "HANDLE: not function"
               val (B,d2) = R(B,rse,t2)
           in  (B, current_increment eps_phi0 && d1 && d2)
               (*(B, Effect.Lf[eps_phi0] && d1 && d2)*)
           end
       | Exp.SWITCH_I {switch,precision} => R_sw(B,rse,switch)
       | Exp.SWITCH_W {switch,precision} => R_sw(B,rse,switch)
       | Exp.SWITCH_S sw => R_sw(B,rse,sw)
       | Exp.SWITCH_C sw => R_sw(B,rse,sw)
       | Exp.SWITCH_E sw => R_sw(B,rse,sw)
       | Exp.CON0 _ => (B, delta_emp)
       | Exp.CON1 (_, t) => R(B,rse,t)
       | Exp.DECON (_, t) => R(B,rse,t)
       | Exp.EXCON (_, NONE) => (B, delta_emp)
       | Exp.EXCON (_, SOME (_,t)) => R(B,rse,t)
       | Exp.DEEXCON (_, t) => R(B,rse,t)
       | Exp.RECORD (_, ts) => foldr(fn (t, (B, d)) =>
					let val (B', d') = R(B,rse,t) in (B', d && d') end)
                                    (B,delta_emp) ts
       | Exp.SELECT (_, t) => R(B,rse,t)
       | Exp.DEREF t => R(B,rse,t)
       | Exp.REF (_, t) => R(B,rse,t)
       | Exp.ASSIGN (_,t1,t2) =>
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = R(B,rse,t2)
           in  (B, d1 && d2)
           end
       | Exp.DROP t => R(B,rse,t)
       | Exp.EQUAL (_,t1,t2) =>
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = R(B,rse,t2)
           in  (B, d1 && d2)
           end
       | Exp.CCALL (_, ts) => foldr(fn (t,(B, d))  =>
				       let val (B', d') = R(B,rse,t) in (B', d && d') end)
                                   (B,delta_emp) ts
       | Exp.EXPORT (_, t) => R(B,rse,t)
       | Exp.RESET_REGIONS (_, t) => R(B,rse,t)
       | Exp.FRAME{declared_lvars,declared_excons} =>
           (List.app(fn {lvar, sigma, ...} =>
                        case RSE.lookupLvar rse lvar of
                          SOME(_,_,_,sigma',_,_,_) => sigma:=sigma'
                        | _ => die ("R: cannot build frame; lvar \"" ^ Lvar.pr_lvar lvar
                                     ^ "\" is not in scope at frame")
                     ) declared_lvars;
            (B, delta_emp)
           )
       ) (* case *)
          handle AbortExp  => raise AbortExp
               | _ =>
          (device "Region inference failed (function R)\n";
           device "Smallest enclosing expression:\n";
           PrettyPrint.outputTree(device,layoutExp(e),!Flags.colwidth);
           device "Region Static Environment:\n";
           PrettyPrint.outputTree(device,RSE.layout(rse),!Flags.colwidth);
           device "\n";
           raise AbortExp
          )

      end (* let fun R_sw ...*)

    fun loopR (B,rse,tr) =
	let val _ = gc_arrow_effect_update := false
	    val _ = Effect.reset();
            val B = #1(R (B,rse,tr)) handle AbortExp => Crash.impossible "R failed"
  	    (* for toplas submission: insert call show_visited *)
	    val B = (* show_visited *) B
	in if !gc_arrow_effect_update then loopR (B,rse,tr)
	   else B
	end
  in
     Effect.algorithm_R:=true;
     loopR
  end

  type ('a,'b)trip = ('a,'b)Exp.trip

end; (*R*)
