(*$RegInf: CON EXCON TYNAME REGION_EXP EFFECT REGION_STAT_ENV LVARS CRASH PRETTYPRINT FLAGS RTYPE REGINF *)
functor RegInf(
                                               (***************)
  structure TyName: TYNAME                     (* Algorithm R *)
  structure Exp: REGION_EXP                    (* Mads Tofte  *)
                                               (***************)
  structure RType: RTYPE
  structure Effect: EFFECT
  structure RSE: REGION_STAT_ENV
  structure Lvar : LVARS
    sharing type Lvar.lvar = RSE.lvar = Exp.lvar
  structure Crash: CRASH
  structure PrettyPrint : PRETTYPRINT
     sharing type PrettyPrint.StringTree
                  = RType.StringTree = RSE.StringTree 
                  = Effect.StringTree = Exp.StringTree
  structure Flags: FLAGS
    sharing type RSE.place = RType.place = Exp.place
        and type Exp.effect = RType.effect = RType.place = Effect.effect
        and type RSE.Type = RType.Type = Exp.Type
        and type RSE.TypeAndPlaceScheme = RType.sigma = Exp.sigma
        and type RSE.excon = Exp.excon
        and type RSE.lvar = Exp.lvar
        and type RType.il = Exp.il = RSE.il
        and type RType.cone= Effect.cone = Exp.cone = RSE.cone
        and type Exp.tyvar = RType.tyvar
        and type RType.delta_phi = Effect.delta_phi
):REGINF =
struct
  type cone = RType.cone
  type place = RType.place
  type effect = RType.effect
  type trip = (place,unit)Exp.trip (*redefined at the end of the functor *)
  type exp  = (place,unit)Exp.LambdaExp
  type prog = (place,unit)Exp.LambdaPgm
  type rse  = RSE.regionStatEnv

  fun footnote(x,y) = x
  infix footnote

  exception AbortExp of string

  fun die msg = Crash.impossible ("R." ^ msg)
  fun dieR msg = raise AbortExp (msg)

  fun say s = output(std_out, s)
  fun checkMsgOpt (Some msg) = Flags.warnings:= (msg^"\n") :: (!Flags.warnings)
    | checkMsgOpt None = ()

  fun show_sigma sigma = PrettyPrint.flatten1(RType.mk_lay_sigma false (sigma))

  fun instClever(sigma,il) y = RType.instClever(sigma,il) y
  fun unify_ty(tau,tau_1) = RType.unify_ty(tau,tau_1)
  fun alpha_rename'(x) = RType.alpha_rename'(x)
  fun effClos x = RType.effClos x
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
        val B' = List.foldL  (Effect.lower(Effect.level B - 1)) B free_rhos_and_epss
                 handle _ => raise AbortExp "Below.lower failed\n"
    in 
        popAndClean(B')
            handle _ => dieR "Below.popAndClean failed\n"
    end


  fun retract(B, body as Exp.TR(e, Exp.Mus mus, phi), 
              delta_phi_body: Effect.delta_phi,
              discharged_basis: effect list ref, 
              discharged_phi: effect list ref,
              old_effect_of_letregion): cone * Effect.delta_phi =
        let
          val (B_discharge,B_keep) = Below(B, mus)

             (* Nodes of level at most level(B)-1 which appear in the increment of
                an arrow effect which becomes letregion-bound (i.e., has same level as B)
                must be considered as increments to the letregion expression. *)

          val (new_discharged_phi,delta_letregion) = 
                 observeDelta(Effect.level B_keep, 
                                     Effect.Br(Effect.Lf (!discharged_phi),
	                                       delta_phi_body), 
                                     old_effect_of_letregion)
          (* old_effect_of_letregion has now been increased by delta_letregion *)
        in discharged_basis:= B_discharge;
           discharged_phi:= new_discharged_phi;	
           (B_keep, delta_letregion)
        end
    | retract(B, t,_,_,_,_) = (B, Effect.Lf[])



  fun inferEffects(device: string-> unit) =
  let
    val layoutExp = Exp.layoutLambdaExp(if !Flags.print_regions 
                                        then (fn rho => Some(PrettyPrint.LEAF("at " 
                                              ^ PrettyPrint.flatten1(Effect.layout_effect rho))))
                                        else (fn rho => None))
                                       (fn () => None)
    fun sayCone B = PrettyPrint.outputTree(device,Effect.layoutCone B,!Flags.colwidth);
    fun sayLn s = (output(std_out, s ^ "\n");
                   device(s ^ "\n"))

    val count_visited = ref 0  (* number of nodes visited in the abstract syntax tree *)
    val count_RegEffClos = ref 0 (* number of times regEffClos is called *)

    fun show_visited  result = 
    (device ("Visited (number of nodes visited during R)" ^ Int.string(! count_visited) ^ "\n");
     device ("RegEffGen (number of times called during R)" ^ Int.string(! count_RegEffClos) ^ "\n");
     result)


    fun R(B:cone, rse: rse, t as Exp.TR(e, mt: Exp.metaType, phi: effect)): cone * Effect.delta_phi =
      let 
        fun R_sw(B,rse, Exp.SWITCH(t1, rules, t_opt)) =
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = List.foldL (fn (lhs,rhs) => fn (B,d) => 
                                      let val (B',d') = R(B,rse,rhs)
                                      in (B', Effect.Br(d,d'))
                                      end) (B,d1) rules
           in
               case t_opt of None => (B,d2)
               | Some t => let val (B,d3) = R(B,rse,t)
                           in (B, Effect.Br(d2,d3))
                           end
           end
      in count_visited:= !count_visited+1;
       (case e of
         Exp.VAR{lvar, il_r = il_r as ref(il,f), alloc} =>
         let val (il, B) = f(il, B)
         in il_r:= (il, fn p => p);
           (case RSE.lookupLvar rse lvar of
     	  Some(_,_,sigma,place0,_, _) =>
                 let 
                   val (tau_1,B,updates: (effect * Effect.delta_phi)list) = instClever(sigma,il)(B)
                     handle Crash.CRASH =>
                       dieR ("inst failed; type scheme:\n" ^
                             PrettyPrint.flatten1(RType.mk_lay_sigma false (sigma)) ^ "\n")
                 in
                   case mt of 
                     Exp.Mus [(tau, _)] =>
                       (let val B' = (unify_ty(tau,tau_1)B handle _ => dieR "unify_ty failed\n");
                        in
                            List.apply update_increment    updates;
                            List.apply (update_areff o #1) updates
                              handle _ => die "update_areff in VAR case";
                            (B',Effect.Lf[])
                        end
                       )
                   | _ => dieR ("R.VAR{...}: bad metatype")
                 end
              | None => dieR ("R.VAR{...}: free lvar" ^ Lvar.pr_lvar lvar)
            )
         end
       | Exp.INTEGER _ => (B, Effect.Lf [])
       | Exp.STRING  _ => (B, Effect.Lf [])
       | Exp.REAL    _ => (B, Effect.Lf [])
       | Exp.UB_RECORD ts => List.foldR(fn t => fn (B, d)  => 
                                        let val (B', d') = R(B,rse,t) in (B',Effect.Br(d,d')) end) 
                                        (B,Effect.Lf[]) ts
       | Exp.FN{pat, body, alloc} =>
           (case mt of
              Exp.Mus [(RType.FUN(mus2,eps_phi0,mus1),_)] =>
                let val rse' = List.foldL (fn (lvar, mu as (tau,rho))=>fn rse =>
                          RSE.declareLvar(lvar, (false,false,
                                 RType.type_to_scheme tau, rho,None,None), 
                                          rse)) rse
                              (ListPair.zip(map #1 pat, mus2))
                    val (B, delta_body) = R(B,rse', body)
                    val lev_eps = case Effect.level_of eps_phi0 of Some n => n | None => die "bad arrow effect (FN)"
                    val B = lower_delta lev_eps delta_body B  
                in 
                    update_increment(eps_phi0, delta_body);
                    update_areff(eps_phi0);
                    (B, Effect.Lf [])
                end
            | _ => dieR "R: FN expression had bad meta type")
           
   
       | Exp.LETREGION_B{B = B1, discharged_phi, body} =>
           let
             val discharged_basis = !B1
             val B = pushLayer(discharged_basis,B) handle _ =>
                       dieR "pushLayer failed\n"
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
               val rse' = RSE.declareLvar(lvar,(false,false,insert_alphas(alphas, sigma'), 
                                                rho1', None, None),
                                          rse)
               val (B, d2) = R(B,rse', scope)
            in
               (B, Effect.Br(d1,d2))
            end
       | Exp.LET _ => dieR "LET.multiple bindings not implemented." 
       | Exp.FIX{shared_clos = rho0,
                 functions,
                 scope = t2} =>
            let 
              (*val _ = sayLn("fix: entering   " ^ Lvar.pr_lvar f )*)
              (* val _ = sayCone B*)


              fun addBindingForRhs {lvar = f, occ, tyvars = alphavec, 
                               rhos as ref rhovec, epss as ref epsvec, 
                               Type = tau0, formal_regions,bind} rse =
                  let 
                     val sigma = RType.FORALL([] ,rhovec,epsvec,tau0)
                  in RSE.declareLvar(f,(true,true,sigma, rho0, Some occ, None),rse)
                  end


              fun doOneRhs rse {lvar = f,occ,tyvars = alphavec, 
                                rhos = rhosr as ref rhovec, epss = epssr as ref epsvec, 
                                Type = tau0, formal_regions, 
                                bind as Exp.TR(_,Exp.Mus[(tau4,rho4)],phi4)}=
              let
                    fun Rrec(B3,sigma_3hat,previous_functions_ok:bool) =
                      let
                       (* val _ = sayLn("fix:entering Rrec " ^ Lvar.pr_lvar f ^ ":" ^ show_sigma sigma_3hat)*)
                       (* val _ = sayCone B3*)
                       (* val _ = sayLn("before rename , sigma is " ^ show_sigma (sigma_3hat))*)
                        val sigma3_hat_save = alpha_rename(sigma_3hat,B3) (* for checking 
                                                                                   alpha_equality below *)

                       (* val _ = sayLn("after  rename , sigma is " ^ show_sigma sigma3_hat_save)*)
                        val rse' = RSE.declareLvar(f,(true,true,sigma3_hat_save, rho0, Some occ, None),rse) (*mads 5/2/97*)
                        val bv_sigma3_hat as (_,rhos,epsilons) = RType.bv sigma_3hat
                        val B3' = pushLayer(sort(epsilons@rhos), B3)
                                    handle _ => dieR "pushLayer failed\n"
                        val (B4,delta_rhs) = R(B3', rse',bind)
                        val _ = count_RegEffClos:= !count_RegEffClos+1
                        val (B5,sigma_5hat, msg_opt) = regEffClos(B4, Effect.level(B3),phi4,tau4)
                        val _ = checkMsgOpt msg_opt 
                        val (_, B5) = pop(B5)
                        val (_,newrhos,newepss) = RType.bv sigma_5hat
                        (*val _ = Profile.profileOn();*)
                      in
                         if alpha_equal(sigma3_hat_save, sigma_5hat) B5 (*footnote Profile.profileOff()*)
                              handle _ => dieR ("alpha_equal failed\n" ^
                                                "sigma_3_hat_save = \n" ^ show_sigma sigma3_hat_save ^
                                                "\nsigma5_hat       = \n" ^ show_sigma sigma_5hat)
                         then 
                                ((*sayLn("fix:  leaving " ^ Lvar.pr_lvar f);*)
                                 (* update bindings in syntax tree *)
                                 rhosr:= newrhos;  
                                 epssr:= newepss;
                                 (B5, previous_functions_ok)
                                )
                         else
                           let 
                             val transformer = matchSchemes(sigma_3hat, sigma_5hat)
                                    handle RType.FAIL_MATCH msg =>
                                    dieR ("failed to match type schemes at FIX " ^ Lvar.pr_lvar f ^ "\n" ^ msg)
                             val _ = map (fn (r as ref(il,f)) => r:= (il, transformer o f)) (!occ)
                                    handle RType.FAIL_MATCH msg =>
                                    dieR ("failed to transform instantiation lists at FIX " 
                                          ^ Lvar.pr_lvar f ^ "\n" ^ msg)
                           in
                             (* update bindings in syntax tree *)
                             rhosr:= newrhos;  
                             epssr:= newepss;
                             (*sayLn("fix:looping for " ^ Lvar.pr_lvar f);         *)
                             (B5,false)
                           end
                      end
               in
                   (fn B => Rrec(B, RType.FORALL([],rhovec,epsvec,tau0),true))
               end
		| doOneRhs _ _ = dieR "doOneRhs.wrong bind"
              
              fun loop(B, [], true, rse) = B
                | loop(B, [], false, rse) = loop(B,functions,true,rse)
                | loop(B, fcn::rest, previous_were_ok,rse) =
                    let val (B, rhs_was_ok) = doOneRhs rse fcn B 
                    in loop(B, rest, previous_were_ok andalso rhs_was_ok, 
                            addBindingForRhs fcn rse)
                    end                

              fun addBindingForScope {lvar = f, occ, tyvars = alphavec, 
                               rhos as ref rhovec, epss as ref epsvec, 
                               Type = tau0, formal_regions,bind} rse =
                  let 
                     val sigma1hat' = RType.FORALL(alphavec ,rhovec,epsvec,tau0)
                  in RSE.declareLvar(f,(true,true,sigma1hat', rho0, Some occ, None),rse)
                  end

              val B1 = loop(B,functions,true,List.foldL addBindingForRhs rse functions)

              val rse' = List.foldL addBindingForScope rse functions
              
            in
              R(B, rse', t2)
            end
   
       | Exp.APP(t1,t2) => 
           let val (B,d1) = R(B,rse,t1)
               val eps_phi0 = case t1 of Exp.TR(_, Exp.Mus [(RType.FUN(_,eps_phi0,_),_)],_) => eps_phi0 | _ => die "APP: not function"
               val (B,d2) = R(B,rse,t2)
           in  (B,Effect.Br(current_increment(eps_phi0),Effect.Br(d1,d2)))
               (*(B,Effect.Br(Effect.Lf[eps_phi0],Effect.Br(d1,d2)))*)
           end
       | Exp.EXCEPTION(excon, nullary:bool, mu, alloc, t1) => 
                           R(B,RSE.declareExcon(excon,mu,rse),t1)
       | Exp.RAISE t1 => R(B,rse,t1)
       | Exp.HANDLE(t1,t2) => 
           let val (B,d1) = R(B,rse,t1)
               val eps_phi0 = case t2 of Exp.TR(_, Exp.Mus [(RType.FUN(_,eps_phi0,_),_)],_) => eps_phi0 | _ => die "HANDLE: not function"
               val (B,d2) = R(B,rse,t2)
           in  (B,Effect.Br(current_increment(eps_phi0),Effect.Br(d1,d2)))
               (*(B,Effect.Br(Effect.Lf[eps_phi0],Effect.Br(d1,d2)))*)
           end
       | Exp.SWITCH_I sw => R_sw(B,rse,sw)
       | Exp.SWITCH_S sw => R_sw(B,rse,sw)
       | Exp.SWITCH_C sw => R_sw(B,rse,sw)
       | Exp.SWITCH_E sw => R_sw(B,rse,sw)
       | Exp.CON0 _ => (B, Effect.Lf[])
       | Exp.CON1 (_, t) => R(B,rse,t)
       | Exp.DECON (_, t) => R(B,rse,t)
       | Exp.EXCON (_, None) => (B, Effect.Lf[])
       | Exp.EXCON (_, Some (_,t)) => R(B,rse,t)
       | Exp.DEEXCON (_, t) => R(B,rse,t)
       | Exp.RECORD (_, ts) => List.foldR(fn t => fn (B, d)  => 
					  let val (B', d') = R(B,rse,t) in (B',Effect.Br(d,d')) end) 
                               (B,Effect.Lf[]) ts
       | Exp.SELECT (_, t) => R(B,rse,t)
       | Exp.DEREF t => R(B,rse,t)
       | Exp.REF (_, t) => R(B,rse,t)
       | Exp.ASSIGN (_,t1,t2) => 
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = R(B,rse,t2)
           in  (B,Effect.Br(d1,d2))
           end
       | Exp.EQUAL (_,t1,t2) => 
           let val (B,d1) = R(B,rse,t1)
               val (B,d2) = R(B,rse,t2)
           in  (B,Effect.Br(d1,d2))
           end
       | Exp.CCALL (_, ts) => List.foldR(fn t => fn (B, d)  => 
					 let val (B', d') = R(B,rse,t) in (B',Effect.Br(d,d')) end) 
                              (B,Effect.Lf[]) ts
       | Exp.RESET_REGIONS (_, t) => R(B,rse,t)
       | Exp.FRAME{declared_lvars,declared_excons} =>
           (List.apply(fn {lvar, sigma, ...} =>
                        case RSE.lookupLvar rse lvar of
                          Some(_,_,sigma',_,_,_) => sigma:=sigma'
                        | _ => die ("R: cannot build frame; lvar \"" ^ Lvar.pr_lvar lvar 
                                     ^ "\" is not in scope at frame")
                     ) declared_lvars;
            (B, Effect.Lf[])
           )
       ) (* case *)
          handle AbortExp msg =>
          (device "Region inference failed (function R)\n";
           device msg;
           PrettyPrint.outputTree(device,layoutExp(e),!Flags.colwidth);
           device "\n";
           die "***Region inference failed***\n"
          )

      end (* let fun R_sw ...*)
  in
     Effect.algorithm_R:=true;
     (*show_visited o  *) #1 o R   (* for toplas submission: call show_visited*)
  end;


  type ('a,'b)trip = ('a,'b)Exp.trip
          
end; (*R*)

