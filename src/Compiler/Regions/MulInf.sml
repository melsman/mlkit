(*$MulInf: MUL_INF MUL REGION_EXP EFFECT CRASH MUL_EXP RTYPE TYNAME PRETTYPRINT FLAGS LVARS*)

      (***************************************************)
      (*          Multiplicity Inference                 *)
      (* - classifies regions as finite or infinite -    *)
      (*   "infinite" means "of unbounded size";         *)
      (*   "finite" means "statically known finite size" *)        
      (***************************************************)

functor MulInf(
  structure TyName: TYNAME
  structure RType: RTYPE
  structure MulExp: MUL_EXP
  structure RegionExp: REGION_EXP
  structure Mul: MUL
  structure Eff: EFFECT
    sharing type Eff.cone = RegionExp.cone = Mul.Effect.cone
    sharing type Eff.place = RegionExp.place = MulExp.place = RType.place = Mul.place
    sharing type Mul.Effect.effect = RegionExp.effect = RType.effect 
                 = MulExp.effectvar = MulExp.ateffect = MulExp.RegionExp.place
                 = Mul.effectvar = MulExp.effect
    sharing type Mul.mularefmap = MulExp.mularefmap
    sharing type RegionExp.trip = MulExp.RegionExp.trip
    sharing type Mul.dependency_map = MulExp.dependency_map
    sharing type RType.il = RegionExp.il = MulExp.il
    sharing type Mul.lvar = MulExp.lvar = MulExp.RegionExp.lvar
    sharing type MulExp.excon = MulExp.RegionExp.excon 
    sharing type Mul.mulef = MulExp.mulef
    sharing type RegionExp.metaType = MulExp.metaType = MulExp.RegionExp.metaType
    sharing type MulExp.RegionExp.Type = RType.Type  = MulExp.Type
    sharing type Mul.mul = MulExp.mul
    sharing type MulExp.qmularefset =  Mul.qmularefset = MulExp.qmularefset
    sharing type TyName.TyName = RType.tyname
    sharing type Mul.efenv = MulExp.efenv
    sharing type MulExp.datbinds = RegionExp.datbinds
    sharing type Mul.mularef = MulExp.mularef
  structure Lvar: LVARS
    sharing type Lvar.lvar = Mul.lvar
  structure Flags: FLAGS
  structure Crash: CRASH
  structure PP: PRETTYPRINT
    sharing type Mul.StringTree = PP.StringTree = MulExp.StringTree = Eff.StringTree
  ): MUL_INF =
struct
  type ('a,'b)LambdaPgm_phi = ('a,'b) RegionExp.LambdaPgm
  type ('a,'b,'c)LambdaPgm_psi = ('a,'b,'c) MulExp.LambdaPgm
  type ('a,'b,'c)LambdaExp = ('a,'b,'c) MulExp.LambdaExp
  type ('a,'b,'c)trip_psi = ('a,'b,'c) MulExp.trip
  type mul = Mul.mul
  type cone = Eff.cone
  type place= Eff.place
  type mulef= Mul.mulef
  type efenv= Mul.efenv
  type qmularefset = Mul.qmularefset
  type mularefmap = Mul.mularefmap

  fun say s = ((*output(std_out, s ^ "\n");*) output(!Flags.log, s ^ "\n"))
  fun say' s = ((*output(std_out, s);*) output(!Flags.log, s ))
  fun outtree t = PP.outputTree(say', t, !Flags.colwidth)
  fun die s = (output(!Flags.log, "Crashing: MulInf."^s^"\n");
               Crash.impossible ("MulInf."^s^"\n"))
  infix footnote
  fun x footnote y = x;

  fun sum_psis psis = Mul.sum_psis psis
  fun max_psis psis = Mul.max_psis psis

  fun get_psi(MulExp.TR(_,_,_,psi_r as ref psi)) = psi
  fun get_mu(MulExp.TR(_,mu,_,_)) = mu
  fun get_place tr = case get_mu tr of RegionExp.Mus[(_,p)] => p | _ => die "get_place"

  fun frv(mu): RType.place list = 
        Eff.remove_duplicates(List.all Eff.is_rho (RType.ann_mus [mu] []))

  fun cons_if_there (None) l = l
    | cons_if_there (Some x) l = x::l

  val return_EE = ref Mul.empty_efenv  (* the efenv to be returned by multiplicity inference*)

  type StringTree = PP.StringTree
  fun layoutp(t1,t2) = PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT":", children = [t1,t2]}

  fun layoutExp e = MulExp.layoutLambdaExp
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions
                        then (fn (rho,mul) => Some(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => None))
                       (fn _ => None)  (* do not print qmularefset's *)
                       e

  fun layouttrip tr = MulExp.layoutLambdaTrip
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions
                        then (fn (rho,mul) => Some(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => None))
                       (fn _ => None)  (* do not print qmularefset's *)
                       tr

  fun layoutLambdaPgm p = MulExp.layoutLambdaPgm
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions 
                        then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => None)
                       (if !Flags.print_regions
                        then (fn (rho,mul) => Some(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => None))
                       (fn _ => None)  (* do not print qmularefset's *)
                       p

(*
  fun bin(tr1,tr2,a) = sum_psis[Mul.put(a),
                                get_psi tr1,
                                get_psi tr2]
*)
  exception Abort of exn

  fun mulinf(Psi: Mul.imp_mularefmap, dep: Mul.dependency_map, cone: Eff.cone,
             tr as MulExp.TR(e, mu, phi, psi_r as ref psi): (place, (place*Mul.mul), Mul.qmularefset ref)trip_psi) =
    let 
      open MulExp  (* to get constructors of expressions *)

      fun infer_trip(tr as MulExp.TR(e, mu, phi, psi_r as ref psi): (place, (place*Mul.mul), Mul.qmularefset ref)trip_psi): unit =
        let 
           fun infer_sw (MulExp.SWITCH(tr0, choices, opt_else)) =
             let 
                val right_hand_sides = (cons_if_there opt_else (map #2 choices))

                val _ = List.apply (fn tr => infer_trip(tr)) (tr0 :: right_hand_sides)
                val case_object_place = get_place tr0
                val choices_psi = max_psis (map get_psi right_hand_sides)
             in
                psi_r:= Mul.sumef(get_psi tr0, choices_psi)
                     (*   Mul.sumef(Mul.get case_object_place, Mul.sumef(get_psi tr0, choices_psi)) *)
             end
                             
        in
          case e of
            VAR{lvar,alloc,rhos_actuals,il,plain_arreffs,other: qmularefset ref} =>
              let 
                val (_,places,_) = RType.un_il il
                val qmul = Mul.instantiateRegions(places,!other)
                val arreffs = Mul.make_arroweffects plain_arreffs
                (*val _ = say "\nMulInf.VAR: calling instantiateeffects with"
                val _ = say "\narrow effects: " 
                val _ = outtree (Mul.layout_Phi arreffs)
                val _ = say "\n and qmul : " 
                val _ = outtree (Mul.layout_qmularefset qmul) *)
                val _ = Mul.instantiateeffects(arreffs,
                                               qmul, Psi, dep) (* updates 
                                                                 shared semantic objects *)
                val psi = case alloc 
			    of Some p => Mul.put p 
			     | None => Mul.empty_psi
              in
  	      psi_r:= psi
  	    end		    
          | INTEGER(_,p) => psi_r:= Mul.put p
          | STRING(_,p) => psi_r:= Mul.put p
          | REAL(_,p) => psi_r:= Mul.put p
          | UB_RECORD(trips) =>
             let 
                val _ = List.apply(fn tr => infer_trip(tr))trips
                val psi = sum_psis(map get_psi trips)
             in 
                psi_r:= psi
             end
          | FN{pat,body,free,alloc} =>
             (case mu of
               RegionExp.Mus[(RType.FUN(_,eps,_),_)] =>
                let 
                   val _ = infer_trip(body) 
                   val psi = get_psi body
                   val psi_eps = #2(Mul.un_mularef(Mul.nf(!(
                                     Mul.lookup_mularefmap(Psi, eps)))))
                   val almost_new_psi = Mul.maxef(psi,psi_eps)
                   (* eps.almost_new_psi is not necessarily acyclic; so normalise it: *)
                   val (_,new_psi) = Mul.un_mularef(Mul.nf(Mul.makearef(eps,almost_new_psi)))
  		 val _ = Mul.doSubst(eps, Mul.diffef(new_psi,psi_eps), dep)
  		in
  		    psi_r:= Mul.put alloc
  		end
              | _ => die "function not of function type"
             )
          | LETREGION{B: effect list ref, rhos, body} =>
             let val _ = infer_trip(body)
                 val psi_body = get_psi body
                 val psi' = Mul.removeatomiceffects(psi_body, !B)
             in
                 psi_r := psi'
             end
          | LET{k_let,pat,bind,scope} =>
             let
                val _ = infer_trip(bind)
                val _  = List.apply (fn (lvar,il_r,alphas,epss,tau,p,Xi_ref) =>    (* 13/3/97 *)
                           (Xi_ref:= Mul.makeqmularefset([],!epss,Psi,p,cone)
                            ))
                           pat
                val _ = infer_trip(scope)
             in
                psi_r:= sum_psis[get_psi bind, get_psi scope]
             end              
  
          | FIX{free,shared_clos,functions,scope} =>
              let 
                 val _ = inf_rh_sides(functions, shared_clos)
                 val _ = infer_trip(scope)
              in 
                 psi_r:= Mul.sumef(Mul.put shared_clos, get_psi scope)
              end

          | APP(ck,sr,tr1, tr2) =>
  		(* application is by the inference rules (non-smart) *)
   	   let 
               val  (eps, p) = case get_mu tr1 of
                 			RegionExp.Mus[(RType.FUN(_,eps, _), p)]=> (eps, p)
  		             | _ => die "non-function type at application"
               val _ = infer_trip(tr1)
               val _ = infer_trip(tr2)
               val psi1 = get_psi tr1   (* may have been updated by mulinf(e2)! *)
               val psi2 = get_psi tr2
               val psi_aux = Mul.efvar eps (*Mul.sumef(Mul.get p, Mul.efvar eps)*)
               val psi = sum_psis[psi_aux, #2(Mul.un_mularef(!(Mul.lookup_mularefmap(Psi, eps)))),
                                  psi1, psi2]
             in
               psi_r := psi
  	   end
  
  
          | EXCEPTION(excon, nullary: bool, mu as (tau,rho), alloc, body) =>
              let
                 val _ = infer_trip(body); (* no need to bind excon; won't have to look it up! *)
  
                 (* Nullary constructors are bound to a pointer
                  * in region rho which points to object consisting of exname and 
                  * string in region rho. Unary constructors on the other hand 
                  * are simply bound to an object consisting of exname and string
                  * (i.e., excluding extra indirection level). The extra indirection
                  * level used for nullary constructors is to ensure simply that
                  * the exname of a nullary constructor occurrence in an expression,
                  * EXCONprim(excon,[]), is obtained in the exact same way as 
                  * the exname of an unary constructor occurrence in an expression,
                  * EXCONprim(excon,[e]) --- this is needed in SWITCH_E.
                  * Thus there is either two puts (nullary) or one put (unary)
                  * into region rho.
                  *)
  
                 val psi_excon = if nullary then Mul.sumef(Mul.put rho, Mul.put rho) else Mul.put rho
                 
              in
                 psi_r:= Mul.sumef(psi_excon, get_psi body)
              end
  
          | RAISE(tr) =>
              (infer_trip(tr);
               psi_r:= get_psi tr
              )
          | HANDLE(tr1, tr2) =>
              let
                 val _ = infer_trip(tr1)
                 val _ = infer_trip(tr2)
                 val (eps,rho_handler) = case get_mu tr2 of
                                           RegionExp.Mus[(RType.FUN(_,eps,_),rho)] =>(eps,rho)
                                         | _ => die "HANDLE: handler did not have functional type"
                 val psi_of_eps = #2(Mul.un_mularef(!(Mul.lookup_mularefmap(Psi,eps))))
                 val psi_aux = Mul.sum_psis[psi_of_eps,Mul.efvar eps(*, Mul.get rho_handler*)]
              in
                 psi_r:= sum_psis[psi_aux, get_psi tr1, get_psi tr2]
              end
          | SWITCH_I sw => infer_sw sw
          | SWITCH_S sw => infer_sw sw
          | SWITCH_C sw => infer_sw sw
          | SWITCH_E sw => infer_sw sw
          | CON0{con, il, aux_regions,alloc = p} => psi_r:= Mul.put p
          | CON1({con, il, alloc = p}, tr) =>
                (infer_trip(tr);
                 psi_r:= Mul.sumef(Mul.put p, get_psi tr))
          | DECON({con, il}, tr) =>
                (infer_trip(tr);
                 psi_r:= get_psi tr(*Mul.sumef(Mul.get(get_place(tr)), get_psi tr)*) )
          | EXCON(excon, None) => psi_r:= Mul.empty_psi
          | EXCON(excon, Some (p,tr)) =>
                (infer_trip(tr);
                 psi_r:= Mul.sumef(Mul.put p, get_psi tr))
          | DEEXCON(excon, tr) =>
                (infer_trip(tr);
                 psi_r:= get_psi tr (*Mul.sumef(Mul.get(get_place(tr)), get_psi tr)*) )
          | RECORD(p, triples) =>
                let 
                   val _ = List.apply(fn tr => infer_trip(tr))triples
                   val psi = sum_psis(Mul.put p :: map get_psi triples)
                in 
                   psi_r:= psi
                end
          | SELECT(i, tr)=>
                (infer_trip(tr);
                 case get_mu tr of
                    RegionExp.Mus[(_,place_of_tuple)] => 
                      psi_r:= get_psi tr (*Mul.sumef(Mul.get place_of_tuple, get_psi tr)*)
                 | _ => die "SELECT: expected single type and place")
          | DEREF tr =>
                (infer_trip(tr);
                 case get_mu tr of
                    RegionExp.Mus[(_,place_of_ref)] => 
                      psi_r:= get_psi tr (*Mul.sumef(Mul.get place_of_ref, get_psi tr)*)
                 | _ => die "DEREF: expected single type and place")
          | REF(p, tr1) =>
                (infer_trip(tr1);
                 case get_mu tr of
                    RegionExp.Mus[(_,place_of_ref)] => 
                      psi_r:= Mul.sumef(Mul.put place_of_ref, get_psi tr1)
                 | _ => die "REF: expected single type and place")
          | ASSIGN(p, tr1, tr2) =>
                (infer_trip(tr1);
                 infer_trip(tr2);
                 case get_mu tr1 of 
                   RegionExp.Mus[(_,place_of_ref)] =>
                     psi_r:= sum_psis[Mul.put p, Mul.putzero place_of_ref, get_psi tr1, get_psi tr2]
                 | _ => die "ASSIGN: expected single type and place of reference")
          | EQUAL({mu_of_arg1,mu_of_arg2, alloc}, tr1, tr2)=>
                (infer_trip(tr1);
                 infer_trip(tr2);
                 let val annotations = RType.ann_mus[mu_of_arg1, mu_of_arg2] []
                     val frv = Eff.remove_duplicates(List.all Eff.is_rho annotations)
                 in psi_r:= sum_psis(get_psi tr1::get_psi tr2 :: Mul.put alloc :: [] (*map Mul.getInf frv*))
                 end)                
          | CCALL({name, resultMu, resultAllocs}, trips) => (* Calling C functions *)
                (List.apply (fn tr => infer_trip(tr)) trips;
                 (* (a) we produce a  get(rho): INF, for each rho which occurs in
                        the argument type.
                    (b) we produce a put(rho): m for every rho which occurs
                        in the result type. If rho occurs in a LIST type then m is
                        INFINITE - otherwise it is NUM 1.
                 *)
                 let fun get_arg_rhos ([],acc) = Eff.remove_duplicates(List.all Eff.is_rho acc)
                       | get_arg_rhos (tr::rest,acc) = 
                            case get_mu tr of
                              RegionExp.Mus mus => get_arg_rhos(rest, RType.ann_mus mus acc)
                            | _ => die "get_arg_rhos: Metatype not Mus"
(*old                     val arg_psi = sum_psis(map Mul.getInf (get_arg_rhos(trips,[]))) *)
(*                     val arg_psis = map Mul.getInf (get_arg_rhos(trips,[])) *)

        	    (* We only want to ensure multiplicity infinity of rho
                     * variables located under a list type in mu_res. Hence,
                     * extra put effects are inserted in the resulting effect
                     * for these region variables. *)

                     fun get_rhos_res_under_list mu : RType.place list=
                       case #1 mu of
                         RType.RECORD mus => 
                           List.foldL (fn mu => fn L => get_rhos_res_under_list mu @ L) [] mus
                       | RType.CONSTYPE (tyname, mus, places, arreffs) =>
                           if TyName.eq(tyname,TyName.tyName_LIST) then frv mu
		           else []
		       | RType.TYVAR tyvar => []
		       | _ => die "CCALL.unexpected result type"   

                     val rhos_res_under_list = Eff.remove_duplicates (get_rhos_res_under_list resultMu)
(*old	             val psi_res_under_list = sum_psis (map Mul.putInf rhos_res_under_list)  *)
	             val psis_res_under_list = map Mul.putInf rhos_res_under_list
	             val rhos_res = frv resultMu
		     val rhos_res_without_rhos_for_tyvars =
		           List.dropAll (fn effect => (case Eff.get_place_ty effect of
							 Some Eff.BOT_RT => true
						       | _ => false)) rhos_res
(*	             val _ = print ("\nrhos_res CCALL : " ^ 
                             List.string (pp o R.layout_place) rhos_res ^ "\n") 
*)
(*old                     val psi_res = sum_psis (map Mul.put rhos_res) *)
                     val psis_res = map Mul.put rhos_res_without_rhos_for_tyvars
(*old     	             val psi_res = Mul.sumef(psi_res, psi_res_under_list)  *)
                     (* total effects *)
		     val psis = psis_res @ psis_res_under_list @ (*arg_psis @ *) map get_psi trips

(*old	             val psi = sum_psis(psi_res(*:: arg_psi*):: map get_psi trips) *)
	             val psi = sum_psis psis 
                 in 
                   psi_r:= psi
                 end)

          | RESET_REGIONS({force: bool, alloc,regions_for_resetting}, tr) =>
                    (* for programmer-directed resetting of regions;
                       resetting is forced iff "force" is true.*)
               (infer_trip(tr);
                let val rhos = case get_mu tr of
                                 RegionExp.Mus [mu] => frv mu
                               | _ => die "RESET_REGIONS: expected single mu"
                    val psi = sum_psis(get_psi tr :: map Mul.putzero rhos)
                in  
                    psi_r:= psi
                end)
          | FRAME{declared_lvars, declared_excons} =>
             return_EE := 
                List.foldL(fn {lvar, other, ...} => 
                           fn EE => ((*say(Lvar.pr_lvar lvar ^ ":"); (*mads*)
                                     outtree(Mul.layout_qmularefset(!other));
                                     say "\n";*)
                                             Mul.declare(EE,lvar, other)))
                     Mul.empty_efenv
                     declared_lvars
        end handle Abort exn => raise Abort exn
                 | exn => (outtree(layouttrip tr);
                           raise Abort exn)

      and inf_rh_sides(functions, shared_clos) =
        let
          val t0 = Mul.last_increment()
          val _ = List.apply (fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,other,bind} =>
                let val qmul = Mul.makeqmularefset(rhos,epss,Psi,shared_clos,cone)
                in
                    other:= qmul
                end) functions
        in
          List.apply(fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,other,bind} => 
                           (infer_trip(bind);
                            (* update type scheme for the function, if there has been
                               a change. *)
                            if t0 <> Mul.last_increment()
                            then other:= Mul.makeqmularefset(rhos,epss,Psi,shared_clos,cone)
                            else ()
                           )
                    ) functions;
          if t0 = Mul.last_increment()
             then ()
          else inf_rh_sides(functions, shared_clos)
        end
    in
      (infer_trip(tr) handle Abort x => 
                  (say "\nWHOLE EXPRESSION:\n";
                   outtree(layouttrip tr);
                   raise x));
      tr
    end


  (* setmuls(Psi, tr): unit

     Insert multiplicities in tr, by traversing tr and looking up the
     multiplicities in Psi *)

  fun setmuls(Psi, tr as MulExp.TR(e, mu, phi, psi_r as ref psi)): unit =
    let 
      open MulExp  (* to get constructors of expressions *)

      fun set_trip(tr as MulExp.TR(e, mu, phi, psi_r as ref psi)): unit =
        let 
           fun set_sw (MulExp.SWITCH(tr0, choices, opt_else)) =
             let 
                val right_hand_sides = (cons_if_there opt_else (map #2 choices))
             in
                List.apply set_trip (tr0 :: right_hand_sides)
             end
        in
          case e of
            VAR _ => ()
          | INTEGER _ => ()
          | STRING _ => ()
          | REAL _ => ()
          | UB_RECORD(trips) => List.apply(fn tr => set_trip(tr))trips
          | FN{body, ...} => set_trip(body) 
          | LETREGION{B: effect list ref, rhos, body} =>
             let val _ = set_trip(body)
                 val psi_body = get_psi body
                 val sorted_places =  map #1 (! rhos)
                 val multiplicities = Mul.getmultiplicities(psi_body, sorted_places)
             in
                 rhos:= ListPair.zip(sorted_places,multiplicities)
             end
          | LET{bind,scope,...} => (set_trip(bind); set_trip(scope))
          | FIX{free,shared_clos,functions,scope} =>
                (set_rh_sides(functions, shared_clos); 
                 set_trip(scope))
          | APP(_,_,tr1, tr2) => (set_trip tr1; set_trip tr2)
          | EXCEPTION(_, _, _ , _, body) => set_trip body
          | RAISE(tr1) => set_trip tr1
          | HANDLE(tr1, tr2) => (set_trip(tr1); set_trip(tr2))
          | SWITCH_I sw => set_sw sw
          | SWITCH_S sw => set_sw sw
          | SWITCH_C sw => set_sw sw
          | SWITCH_E sw => set_sw sw
          | CON0 _ => ()
          | CON1 (_,tr) => set_trip tr
	  | DECON (_,tr) => set_trip tr 
	  | EXCON(_,Some(_,tr)) => set_trip tr
	  | EXCON(_,None) => ()  
          | DEEXCON(_, tr) => set_trip tr
          | RECORD (_, triples) => List.apply set_trip triples
          | SELECT(_, tr) => set_trip tr
          | DEREF tr => set_trip tr
          | REF(_, tr) => set_trip tr
          | ASSIGN(_, tr1, tr2) => (set_trip tr1; set_trip tr2)
          | EQUAL(_, tr1, tr2) => (set_trip tr1; set_trip tr2)
          | CCALL(_, trips) => List.apply set_trip trips
          | RESET_REGIONS(_, tr) => set_trip tr
          | FRAME _ => ()
        end handle Abort exn => raise Abort exn
                 | exn => (outtree(layouttrip tr);
                           raise Abort exn)

      and set_rh_sides(functions, shared_clos) =
          List.apply(fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,other,bind} => 
                           (set_trip(bind);
                            (* Set the PUT multiplicites of the formal region variables *)
                            case Type of
                              RType.FUN(_,areff,_) =>
                                let val mularef = Mul.nf(!(Mul.lookup_mularefmap(Psi,areff)))
                                    val psi = #2(Mul.un_mularef mularef)
                                    val places = map #1 (!rhos_formals)
                                    val muls = Mul.getmultiplicities_unsorted(psi, places)
                                in rhos_formals:= ListPair.zip(places,muls)
                                end
                            | _ => die "set_rh_sides: expected function type"
                                                           
                           )
                    ) functions;

    in
      (set_trip(tr) handle Abort x => 
                  (say "\nWHOLE EXPRESSION:\n";
                   outtree(layouttrip tr);
                   raise x))
    end


  fun mkPhi(c,tr) = RegionExp.mkPhi(c,tr)
  fun makezero_Phi Phi= Mul.makezero_Phi Phi
  fun mk_init_dependency_map Psi = Mul.mk_init_dependency_map Psi
  fun mk_initial_mulexp(mulenv,tr, dep) = MulExp.mk_initial_mulexp(mulenv,tr, dep)
  fun eval_phis x = Eff.eval_phis x
  fun combine(Psi0, Psi)= Mul.combine(Psi0, Psi)

  (* test of k-normalisation: *)
  fun printnormal(msg, trip) = 
          (say msg; outtree(layouttrip trip))

  fun printerror(e1,e2) = 
          (say "***** test of  k-normalisation failed ****\nFIRST EXPRESSION:\n";
           outtree(layoutExp e1);
           say "\nSECOND EXPRESSION:\n";
           outtree(layoutExp e2))

  val dummy_'c = ref Mul.empty_qmularefset

  fun test_knorm pgm = MulExp.test_knorm printnormal printerror dummy_'c pgm

  fun k_normPgm (pgm : (place,place*mul, qmularefset ref)MulExp.LambdaPgm) :
                (place,place*mul, qmularefset ref)MulExp.LambdaPgm =
      MulExp.k_normPgm printnormal dummy_'c pgm
   
  fun mulInf(p as RegionExp.PGM{expression = tr,export_datbinds,export_basis},
             Psi0: Mul.mularefmap, (* the multiplicity arrow effect map in which free effect variables
                                  of tr may be looked up; it is applicative *)
             c: Eff.cone, mulenv: Mul.efenv): (place,place*Mul.mul,Mul.qmularefset ref)LambdaPgm_psi * efenv * mularefmap= 
	let 
            val test = false
            val _ = if test then say "\nmulInf:" else ();
            val _ = if test then say "  collecting all effects..." else ()
                    (* collect all region variables, locally bound within tr,
                       plus the region and effect variables that are exported by tr *)
            val effects= mkPhi(tr,export_basis)
            val _ = if test then say "  computing transitive closure ..." else ()
            val _ = eval_phis effects  (* computes transitive closure  of effect graph, 
                                          including only PUT and EPS nodes *)
            val _ = if test then say "  making the arrow effect set Phi..." else ()
            val Phi = map (fn eps => (eps, Eff.represents eps)) 
                          (Eff.toplevel_arreff :: (List.all Eff.is_arrow_effect effects))
            val _ = if test then say "  made Phi, now constructing the map Psi..." else ()
            val Psi = makezero_Phi Phi
                      (* Psi records multiplicities for effect variables that are
                         bound locally within the program unit or are exported from
                         the program unit. Psi is a quasi-map (i.e., partly imperative)*)
            val dep = mk_init_dependency_map Psi
                      (* dep is purely local to this program unit; no global
                         dependencies between semantic objects are required,
                         as we assume that all top-level multiplicities are infinite;
			 Yes, but we need to add top-level effectvars anyway, for
			 lookup_dep not to fail! 12/01/97-Martin *)
            val _ = if test then say "  Psi = " else ()
            val _ = if test then outtree(Mul.layout_imp_mularefmap Psi) else ()
            val _ = if test then say "\n  made Psi, now making initial multiplicity expression" else ()
            val (tr_psi, dep) = mk_initial_mulexp(mulenv,tr, dep)
            val _ = if test then say "\n  made multiplicity expression, now adding local and external Psi"
                    else ()
            val Psi_combined = combine(Psi0, Psi)
            val _ = if test then say "\n  now starting multiplicity inference proper (i.e., calling mulinf)..." else ()
            val tr' = mulinf(Psi_combined, dep, c, tr_psi) footnote
              (if test then say "\n  inserting multiplicities in term..." else ();
               setmuls(Psi_combined, tr_psi);
               if test then say "\n  multiplicities inferred." else ())

            val EE' = !return_EE

            (* lookup exported effect variables in Psi_combined, so that they
               can be exported in the term. Also, set the multiplicities of
               atomic effects in exported multiplicity arrow effects to infinity. *)

            val export_Psi_list = 
                  List.foldR (fn node => fn acc => 
                               let val r:Mul.mularef ref =  
                                          Mul.lookup_mularefmap(Psi, node) 
                               in 
                                  r:= Mul.mk_infinite(!r);
                                  r :: acc
                               end handle _ => acc) [] (List.all Eff.is_arrow_effect export_basis)

            val Psi_export = Mul.reify export_Psi_list

	    val (export_lvars, export_excons) =
	      let open MulExp
		  val TR(_,metatype,_,_) = tr'
	      in case metatype
		   of RegionExp.Frame{declared_lvars,declared_excons} =>
		     (map #lvar declared_lvars, map #1 declared_excons)
		    | RegionExp.RaisedExnBind => ([],[])
		    | RegionExp.Mus _ => die "export"
	      end

	    val export_rhos = 
	      List.foldL (fn effect => fn rhos => if Eff.is_rho effect then effect::rhos else rhos)
	      [] export_basis
		
            val pgm' = MulExp.PGM{expression = tr',
			export_datbinds = export_datbinds, (* unchanged *)
			import_vars=ref None,
			export_vars=(export_lvars,export_excons,export_rhos),
			export_basis = export_basis,       (* unchanged *)
			export_Psi = export_Psi_list}

	in
          if test
          then
            if test_knorm(pgm') then ()
            else (output(std_out, "\n ********   knorm test failed **********\n"))
          else ();

          (pgm', EE',  Psi_export)
	end
  
end;
