
      (***************************************************)
      (*          Multiplicity Inference                 *)
      (* - classifies regions as finite or infinite -    *)
      (*   "infinite" means "of unbounded size";         *)
      (*   "finite" means "statically known finite size" *)
      (***************************************************)

structure MulInf: MUL_INF =
struct
  structure PP = PrettyPrint
  structure Eff = Effect
  structure Lvar = Lvars
  structure RegionExp = MulExp.RegionExp

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

  val print_regions = Flags.is_on0 "print_regions"

  fun say s = ((*TextIO.output(TextIO.stdOut, s ^ "\n");*) TextIO.output(!Flags.log, s ^ "\n"))
  fun say' s = ((*TextIO.output(TextIO.stdOut, s);*) TextIO.output(!Flags.log, s ))
  fun outtree t = PP.outputTree(say', t, !Flags.colwidth)
  fun die s = (TextIO.output(!Flags.log, "Crashing: MulInf."^s^"\n");
               Crash.impossible ("MulInf."^s^"\n"))
  infix footnote
  fun x footnote y = x;

  fun sum_psis psis = Mul.sum_psis psis
  fun max_psis psis = Mul.max_psis psis

  fun get_psi (MulExp.TR(_,_,_,psi_r as ref psi)) = psi
  fun get_mu (MulExp.TR(_,mu,_,_)) = mu

  fun get_boxed_place s tr =
      case get_mu tr of
          RegionExp.Mus[mu] =>
          (case RType.unBOX mu of
               SOME (_,p) => p
             | NONE => die ("get_boxed_place.expecting boxed mu: " ^ s))
        | _ => die ("get_boxed_place.expecting single mu: " ^ s)

  fun frv mu : RType.place list =
      Eff.remove_duplicates(List.filter Eff.is_rho (RType.ann_mus [mu] []))

  fun cons_if_there (NONE) l = l
    | cons_if_there (SOME x) l = x::l

  val return_EE = ref Mul.empty_efenv  (* the efenv to be returned by multiplicity inference*)

  type StringTree = PP.StringTree
  fun layoutp (t1,t2) = PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT":", children = [t1,t2]}

  fun layoutExp e = MulExp.layoutLambdaExp
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn (rho,mul) => SOME(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => NONE))
                       (fn _ => NONE)  (* do not print qmularefset's *)
                       e

  fun layouttrip tr = MulExp.layoutLambdaTrip
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn (rho,mul) => SOME(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => NONE))
                       (fn _ => NONE)  (* do not print qmularefset's *)
                       tr

  fun layoutLambdaPgm p = MulExp.layoutLambdaPgm
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
                        else fn _ => NONE)
                       (if print_regions()
                        then (fn (rho,mul) => SOME(layoutp(Eff.layout_effect rho, Mul.layout_mul mul)))
                        else (fn _ => NONE))
                       (fn _ => NONE)  (* do not print qmularefset's *)
                       p

(*
  fun bin(tr1,tr2,a) = sum_psis[Mul.put(a),
                                get_psi tr1,
                                get_psi tr2]
*)
  exception Abort of exn

  fun mulinf (Psi: Mul.imp_mularefmap, dep: Mul.dependency_map, cone: Eff.cone,
              tr as MulExp.TR(e, mu, phi, psi_r as ref psi): (place, (place*Mul.mul), Mul.qmularefset ref)trip_psi) =
    let
      open MulExp  (* to get constructors of expressions *)

      fun infer_trip (tr as MulExp.TR(e, mu, phi, psi_r as ref psi): (place, (place*Mul.mul), Mul.qmularefset ref)trip_psi) : unit =
        let
           fun infer_sw (MulExp.SWITCH(tr0, choices, opt_else)) =
             let
                val right_hand_sides = (cons_if_there opt_else (map #2 choices))

                val _ = app (fn tr => infer_trip(tr)) (tr0 :: right_hand_sides)
                (* val case_object_place = get_place tr0 *)
                val choices_psi = max_psis (map get_psi right_hand_sides)
             in
                psi_r:= Mul.sumef(get_psi tr0, choices_psi)
                     (*   Mul.sumef(Mul.get case_object_place, Mul.sumef(get_psi tr0, choices_psi)) *)
             end

        in
          case e of
            VAR{lvar,fix_bound,rhos_actuals,il,plain_arreffs,other: qmularefset ref} =>
              let
                val (places,_,_) = RType.un_il il
                val qmul = Mul.instantiateRegions(places,!other)
                val arreffs = Mul.make_arroweffects plain_arreffs
(*
                val _ = say ("\nMulInf.VAR " ^ Lvars.pr_lvar lvar ^ ": calling instantiateeffects with")
                val _ = say "\narrow effects: "
                val _ = outtree (Mul.layout_Phi arreffs)
                val _ = say "\n and qmul : "
                val _ = outtree (Mul.layout_qmularefset qmul)
*)
                val _ = Mul.instantiateeffects(arreffs,
                                               qmul, Psi, dep) (* updates
                                                                 shared semantic objects *)
(*
                val psi = case alloc
                            of SOME p => Mul.put p
                             | NONE => Mul.empty_psi
*)
                val psi = Mul.empty_psi
              in
                psi_r:= psi
              end
          | INTEGER(_,t,p) => (case p of SOME p => psi_r:= Mul.put p | NONE => ())
          | WORD(_,t,p) => (case p of SOME p => psi_r:= Mul.put p | NONE => ())
          | STRING(_,p) => psi_r:= Mul.put p
          | REAL(_,p) => psi_r:= Mul.put p
          | F64 _ => ()
          | UB_RECORD(trips) =>
             let val _ = app(fn tr => infer_trip(tr))trips
                 val psi = sum_psis(map get_psi trips)
             in psi_r:= psi
             end
          | FN{pat,body,free,alloc} =>
            (case mu of
                 RegionExp.Mus[mu] =>
                 (case RType.unBOX mu of
                      SOME (ty,_) =>
                      (case RType.unFUN ty of
                           SOME (_,eps,_) =>
                           let val _ = infer_trip(body)
                               val psi = get_psi body
                               val psi_eps = #2(Mul.un_mularef(Mul.nf(!(
                                     Mul.lookup_mularefmap(Psi, eps)))))
                               val almost_new_psi = Mul.maxef(psi,psi_eps)
                               (* eps.almost_new_psi is not necessarily acyclic; so normalise it: *)
                               val (_,new_psi) = Mul.un_mularef(Mul.nf(Mul.makearef(eps,almost_new_psi)))
                               fun debug () =
                                   (print "DEBUG FN\n";
                                    print " eps = \n" ; outtree(Eff.layout_effect eps);
                                    print "\n psi =\n" ; outtree(Mul.layout_mulef psi);
                                    print "\n psi_eps =\n"; outtree(Mul.layout_mulef psi_eps);
                                    print "\n almost_new_psi=\n";  outtree(Mul.layout_mulef almost_new_psi);
                                    print "\n new_psi=\n"; outtree(Mul.layout_mulef new_psi);
                                    print "\n")
                                val _ = Mul.doSubst(eps, Mul.diffef(new_psi,psi_eps), dep)
                                        handle X =>
                                               (say "\nMulInf(FN) fails:\n";
                                                debug(); raise X)
                           in
                             psi_r:= Mul.put alloc
                           end
                         | NONE => die "function not of function type")
                    | NONE => die "function not boxed")
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
                val _  = app (fn (lvar,il_r,alphas,epss,tau,p,Xi_ref) =>    (* 13/3/97 *)
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
                                   RegionExp.Mus[mu] =>
                                   (case RType.unBOX mu of
                                        NONE => die "non-boxed function type at application"
                                      | SOME (ty,p) =>
                                        (case RType.unFUN ty of
                                             SOME (_,eps,_) => (eps, p)
                                           | NONE => die "non-function type at application"))
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

          | EXCEPTION(excon, nullary: bool, mu, alloc, body) =>
              let
                 val _ = infer_trip body (* no need to bind excon; won't have to look it up! *)

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

                 val (_,rho) = case RType.unBOX mu of SOME p => p | NONE => die "EXCEPTION: expecting boxed value"
                 val psi_excon = if nullary then Mul.sumef(Mul.put rho, Mul.put rho) else Mul.put rho
              in psi_r:= Mul.sumef(psi_excon, get_psi body)
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
                                             RegionExp.Mus[mu] =>
                                             (case RType.unBOX mu of
                                                  SOME(ty,rho) =>
                                                  (case RType.unFUN ty of
                                                       SOME (_,eps,_) => (eps,rho)
                                                     | NONE => die "HANDLE: handler did not have functional type")
                                                | NONE => die "HANDLE: handler did not have boxed functional type")
                                           | _ => die "HANDLE: handler did not have functional type"
                 val psi_of_eps = #2(Mul.un_mularef(!(Mul.lookup_mularefmap(Psi,eps))))
                 val psi_aux = Mul.sum_psis[psi_of_eps,Mul.efvar eps(*, Mul.get rho_handler*)]
              in
                 psi_r:= sum_psis[psi_aux, get_psi tr1, get_psi tr2]
              end
          | SWITCH_I {switch,precision} => infer_sw switch
          | SWITCH_W {switch,precision} => infer_sw switch
          | SWITCH_S sw => infer_sw sw
          | SWITCH_C sw => infer_sw sw
          | SWITCH_E sw => infer_sw sw
          | CON0{con, il, aux_regions,alloc} =>
            (case alloc of
                 SOME p => psi_r:= Mul.put p
               | NONE => ())
          | CON1({con, il, alloc}, tr) =>
            (infer_trip tr;
             case alloc of
                 SOME p => psi_r:= Mul.sumef(Mul.put p, get_psi tr)
               | NONE => psi_r:= get_psi tr)
          | DECON({con, il}, tr) =>
            (infer_trip tr;
             psi_r:= get_psi tr(*Mul.sumef(Mul.get(get_place(tr)), get_psi tr)*) )
          | EXCON(excon, NONE) => psi_r:= Mul.empty_psi
          | EXCON(excon, SOME (p,tr)) =>
            (infer_trip tr;
             psi_r:= Mul.sumef(Mul.put p, get_psi tr))
          | DEEXCON(excon, tr) =>
            (infer_trip tr;
             psi_r:= get_psi tr (*Mul.sumef(Mul.get(get_place(tr)), get_psi tr)*) )
          | RECORD(NONE, nil) => ()
          | RECORD(NONE,_) => die "mulinf: RECORD"
          | RECORD(SOME p, triples) =>
            let val _ = app (fn tr => infer_trip tr) triples
                val psi = sum_psis(Mul.put p :: map get_psi triples)
            in psi_r:= psi
            end
          | SELECT(i, tr)=>
            (infer_trip(tr);
             let val place_of_tuple = get_boxed_place "SELECT" tr
             in psi_r:= get_psi tr (*Mul.sumef(Mul.get place_of_tuple, get_psi tr)*)
             end)
          | DEREF tr =>
            (infer_trip tr;
             let val place_of_ref = get_boxed_place "DEREF" tr
             in psi_r:= get_psi tr (*Mul.sumef(Mul.get place_of_ref, get_psi tr)*)
             end)
          | REF(p, tr1) =>
            (infer_trip tr1;
             let val place_of_ref = get_boxed_place "REF" tr
             in psi_r:= Mul.sumef(Mul.put place_of_ref, get_psi tr1)
             end)
          | ASSIGN(tr1, tr2) =>
            (infer_trip tr1;
             infer_trip tr2;
             psi_r:= sum_psis[get_psi tr1, get_psi tr2]
            )
          | DROP(tr) =>
              (infer_trip tr;
               psi_r:= get_psi tr
              )
          | EQUAL({mu_of_arg1,mu_of_arg2}, tr1, tr2)=>
                (infer_trip(tr1);
                 infer_trip(tr2);
                 let val annotations = RType.ann_mus[mu_of_arg1, mu_of_arg2] []
                     val frv = Eff.remove_duplicates(List.filter Eff.is_rho annotations)
                 in psi_r:= sum_psis [get_psi tr1, get_psi tr2] (*map Mul.getInf frv*)
                 end)
          | CCALL ({name, rhos_for_result, ...}, trips) => (*Calling C functions*)
                (app infer_trip trips;
                 (*We produce a `put(rho) : m' for every rho which occurs in
                  the result type.  If rho occurs in a LIST type then m is
                  INFINITE---otherwise it is NUM 1.  To do this, we use the
                  ``physical size'' of rho according to `rhos_for_result',
                  which was annotated in SpreadExpression.  (See also the
                  comment in MUL_EXP.)*)

                 let val (rhos_inf, rhos_fin) =
                       foldl (fn ((rho, i_opt),(rhos_inf, rhos_fin)) =>
                                   (case i_opt of
                                      SOME i => (rhos_inf, rho :: rhos_fin)
                                    | NONE =>   (rho :: rhos_inf, rhos_fin)))
                           ([], []) rhos_for_result
                     val psis = map Mul.putInf (Eff.remove_duplicates rhos_inf)
                                @ map Mul.put (Eff.remove_duplicates rhos_fin)
                                @ map get_psi trips
                 in case psis of
                        nil => ()
                      | _ => psi_r := sum_psis psis
                 end)
          | BLOCKF64(p, triples) =>
                let val _ = app infer_trip triples
                    val psi = sum_psis(Mul.put p :: map get_psi triples)
                in psi_r:= psi
                end
          | SCRATCHMEM(n,p) => psi_r:= Mul.put p
          | EXPORT(_,tr) =>
              (infer_trip(tr);
               psi_r:= get_psi tr
              )
          | RESET_REGIONS({force: bool, regions_for_resetting, liveset}, tr) =>
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
                foldl(fn ({lvar, other, ...}, EE) =>
                      ((*say(Lvar.pr_lvar lvar ^ ":"); (*mads*)
                       outtree(Mul.layout_qmularefset(!other));
                       say "\n";*)
                      Mul.declare(EE,lvar, other)))
                Mul.empty_efenv
                declared_lvars
        end handle Abort exn => raise Abort exn
                 | exn => (outtree(layouttrip tr);
                           raise Abort exn)

      and inf_rh_sides (functions, shared_clos) =
        let val t0 = Mul.last_increment()
            val _ = app (fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                             bound_but_never_written_into,
                             other,bind} =>
                            let val qmul = Mul.makeqmularefset(rhos,epss,Psi,SOME shared_clos,cone)
                            in other := qmul
                            end) functions
        in
          app (fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                   bound_but_never_written_into,
                   other,bind} =>
                  (infer_trip bind;
                   (* update type scheme for the function, if there has been
                      a change. *)
                   if t0 <> Mul.last_increment()
                   then other:= Mul.makeqmularefset(rhos,epss,Psi,SOME shared_clos,cone)
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
                app set_trip (tr0 :: right_hand_sides)
             end
        in
          case e of
            VAR _ => ()
          | INTEGER _ => ()
          | WORD _ => ()
          | STRING _ => ()
          | REAL _ => ()
          | F64 _ => ()
          | UB_RECORD(trips) => app(fn tr => set_trip(tr))trips
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
          | SWITCH_I {switch,precision} => set_sw switch
          | SWITCH_W {switch,precision} => set_sw switch
          | SWITCH_S sw => set_sw sw
          | SWITCH_C sw => set_sw sw
          | SWITCH_E sw => set_sw sw
          | CON0 _ => ()
          | CON1 (_,tr) => set_trip tr
          | DECON (_,tr) => set_trip tr
          | EXCON(_,SOME(_,tr)) => set_trip tr
          | EXCON(_,NONE) => ()
          | DEEXCON(_, tr) => set_trip tr
          | RECORD (_, triples) => app set_trip triples
          | SELECT(_, tr) => set_trip tr
          | DEREF tr => set_trip tr
          | REF(_, tr) => set_trip tr
          | ASSIGN(tr1, tr2) => (set_trip tr1; set_trip tr2)
          | DROP(tr1) => (set_trip tr1)
          | EQUAL(_, tr1, tr2) => (set_trip tr1; set_trip tr2)
          | CCALL(_, trips) => app set_trip trips
          | BLOCKF64 (_, triples) => app set_trip triples
          | SCRATCHMEM _ => ()
          | EXPORT(_,tr) => set_trip tr
          | RESET_REGIONS(_, tr) => set_trip tr
          | FRAME _ => ()
        end handle Abort exn => raise Abort exn
                 | exn => (outtree(layouttrip tr);
                           raise Abort exn)

      and set_rh_sides(functions, shared_clos) =
          app(fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,bound_but_never_written_into,other,bind} =>
                           (set_trip(bind);
                            (* Set the PUT multiplicites of the formal region variables *)
                            case RType.unFUN Type of
                              SOME(_,areff,_) =>
                                let val mularef = Mul.nf(!(Mul.lookup_mularefmap(Psi,areff)))
                                    val psi = #2(Mul.un_mularef mularef)
                                    val places = map #1 (!rhos_formals)
                                    val muls = Mul.getmultiplicities_unsorted(psi, places)
                                in rhos_formals:= ListPair.zip(places,muls)
                                end
                            | NONE => die "set_rh_sides: expected function type"

                           )
                    ) functions;

    in
      (set_trip(tr) handle Abort x =>
                  (say "\nWHOLE EXPRESSION:\n";
                   outtree(layouttrip tr);
                   raise x))
    end


  fun mkPhi (c,tr) = RegionExp.mkPhi(c,tr)
  fun makezero_Phi Phi = Mul.makezero_Phi Phi
  fun mk_init_dependency_map Psi = Mul.mk_init_dependency_map Psi
  fun mk_initial_mulexp (mulenv,tr, dep) = MulExp.mk_initial_mulexp(mulenv,tr, dep)
  fun eval_phis x = Eff.eval_phis x
  fun combine (Psi0, Psi)= Mul.combine(Psi0, Psi)

  (* test of k-normalisation: *)
  fun printnormal (msg, trip) =
      (say msg; outtree(layouttrip trip))

  fun printerror (e1,e2) =
      (say "***** test of  k-normalisation failed ****\nFIRST EXPRESSION:\n";
       outtree(layoutExp e1);
       say "\nSECOND EXPRESSION:\n";
       outtree(layoutExp e2))

  val dummy_'c = ref Mul.empty_qmularefset

  fun test_knorm pgm = MulExp.test_knorm printnormal printerror dummy_'c pgm

  fun k_normPgm (pgm : (place,place*mul, qmularefset ref)MulExp.LambdaPgm) :
                (place,place*mul, qmularefset ref)MulExp.LambdaPgm =
      MulExp.k_normPgm printnormal dummy_'c pgm

  fun mulInf (p as RegionExp.PGM{expression = tr,export_datbinds,export_basis},
              Psi0: Mul.mularefmap, (* the multiplicity arrow effect map in which free effect variables
                                       of tr may be looked up; it is applicative *)
              c: Eff.cone, mulenv: Mul.efenv)
      : (place,place*Mul.mul,Mul.qmularefset ref)LambdaPgm_psi * efenv * mularefmap =
        let
            val test = false
            val _ = if test then say "\nmulInf:" else ();
            val _ = if test then say "  collecting all effects..." else ()
                    (* collect all region variables, locally bound within tr,
                       plus the region and effect variables that are exported by tr *)
            val effects = mkPhi(tr,export_basis)
            val _ = if test then say "  computing transitive closure ..." else ()

            val () =
                let val allnodes = eval_phis effects  (* computes transitive closure  of effect graph,
                                                         including only PUT and EPS nodes *)
                                   handle ? as Report.DeepError _ => raise ?
                                        | exn => (say "  eval_phis called from MulInf (transitive closure of all effects) ";
                                                  raise exn)
                    val () = Eff.check_nodes {allnodes=allnodes,
                                              letregions=RegionExp.letregionBound tr}
                in ()
                end

            val _ = if test then say "  making the arrow effect set Phi..." else ()

            val Psi =
                (* Psi records multiplicities for effect variables that are
                 * bound locally within the program unit or are exported from
                 * the program unit. Psi is a quasi-map (i.e., partly imperative)*)
                let val Phi = map (fn eps => (eps, Eff.represents eps))
                          ( (*Eff.toplevel_arreff :: ;mael 2004-03-31*) (List.filter Eff.is_arrow_effect effects))
                    val _ = if test then say "  made Phi, now constructing the map Psi..." else ()
                in makezero_Phi Phi
                end

            val _ = if test then (say "  Psi0 = "; outtree(Mul.layout_mularefmap Psi0))
                    else ()
            val _ = if test then (say "  Psi = "; outtree(Mul.layout_imp_mularefmap Psi))
                    else ()
            val _ = if test then say "\n  made Psi, now adding local and external Psi"
                    else ()
            val Psi_combined = combine(Psi0, Psi)
            val _ = if test then (say "  Psi_combined = ";
                                  outtree(Mul.layout_imp_mularefmap Psi_combined))
                    else ()

            val _ = if test then say "\n  now making initial multiplicity expression" else ()
            val dep = mk_init_dependency_map Psi_combined
                      (* dep is purely local to this program unit; no global
                         dependencies between semantic objects are required,
                         as we assume that all top-level multiplicities are infinite;
                         Yes, but we need to add top-level effectvars anyway, for
                         lookup_dep not to fail! 12/01/97-Martin *)
            val (tr_psi, dep) = mk_initial_mulexp(mulenv,tr, dep)
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
                  foldr (fn (node, acc) =>
                               let val r:Mul.mularef ref =
                                          Mul.lookup_mularefmap(Psi, node)
                               in
                                  r:= Mul.mk_infinite(!r);
                                  r :: acc
                               end handle _ => acc) [] (List.filter Eff.is_arrow_effect export_basis)

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
              foldl (fn (effect, rhos) => if Eff.is_rho effect then effect::rhos else rhos)
              [] export_basis

            val pgm' = MulExp.PGM{expression = tr',
                        export_datbinds = export_datbinds, (* unchanged *)
                        import_vars=ref NONE,
                        export_vars=(export_lvars,export_excons,export_rhos),
                        export_basis = export_basis,       (* unchanged *)
                        export_Psi = export_Psi_list}

            val _ = case export_rhos of nil => () | _ =>
                print ("** MulInf: export_rhos non-empty\n")

            val _ = case export_Psi_list of nil => () | _ =>
                print ("** MulInf: export_Psi_list non-empty\n")

        in
          if false
          then
            if test_knorm(pgm') then ()
            else (TextIO.output(TextIO.stdOut, "\n ********   knorm test failed **********\n"))
          else ();

          (pgm', EE',  Psi_export) footnote Mul.reset_dep()
        end

  (* Contract: traverse program and combine regions of the same region
   * type that are bound by the same letregion construct. *)

  local open MulExp in
      datatype app_cont = APP_CONT | APP_BREAK
      local
          fun apps appt (SWITCH(t,ts,d)) : unit =
              (appt t; List.app (fn (_,t) => appt t) ts;
               case d of SOME t => appt t | NONE => ())
          fun appt f (TR(e,_,_,_)) : unit = appe f e
          and appe f e =
              case f e of
                  APP_BREAK => ()
                | APP_CONT =>
              case e of
                  VAR _ => ()
                | INTEGER _ => ()
                | WORD _ => ()
                | STRING _ => ()
                | REAL _ => ()
                | F64 _ => ()
                | UB_RECORD ts => List.app (appt f) ts
                | FN {pat,body,free,alloc} => appt f body
                | LET {k_let, pat, bind, scope} => (appt f bind; appt f scope)
                | LETREGION {B,rhos,body} => appt f body
                | FIX {free, shared_clos, functions,scope} =>
                      let fun appf {lvar,occ,tyvars,rhos,epss,
                                    Type,rhos_formals,bound_but_never_written_into,
                                    other, bind} = appt f bind
                      in  List.app appf functions; appt f scope
                      end
                | APP (_,_,t1,t2) => (appt f t1; appt f t2)
                | EXCEPTION (_,_,_,_,t) => appt f t
                | RAISE t => appt f t
                | HANDLE (t1,t2) => (appt f t1; appt f t2)
                | SWITCH_I {switch,precision} => apps (appt f) switch
                | SWITCH_W {switch,precision} => apps (appt f) switch
                | SWITCH_S switch => apps (appt f) switch
                | SWITCH_C switch => apps (appt f) switch
                | SWITCH_E switch => apps (appt f) switch
                | CON0 _ => ()
                | CON1 (_,t) => appt f t
                | DECON (_,t) => appt f t
                | EXCON (_,opt) => (case opt of
                                        SOME (_,t) => appt f t
                                      | NONE => ())
                | DEEXCON (_,t) => appt f t
                | RECORD (_,ts) => List.app (appt f) ts
                | SELECT (_,t) => appt f t
                | DEREF t => appt f t
                | REF (_,t) => appt f t
                | ASSIGN (t1,t2) => (appt f t1; appt f t2)
                | DROP t => appt f t
                | EQUAL (_,t1,t2) => (appt f t1; appt f t2)
                | CCALL (_,ts) => List.app (appt f) ts
                | BLOCKF64 (_,ts) => List.app (appt f) ts
                | SCRATCHMEM _ => ()
                | EXPORT (_,t) => appt f t
                | RESET_REGIONS (_,t) => appt f t
                | FRAME _ => ()
      in
          fun app f (PGM{expression,...}) = appt f expression
          val appt = appt
      end

      local exception LVARS of lvar list
      in
          fun exported_lvars p =
              let fun r (FRAME {declared_lvars,...}) =
                     raise LVARS (map #lvar declared_lvars)
                    | r _ = APP_CONT
              in app r p ; die "exported_lvars.shouldn't get here"
              end handle LVARS lvs => lvs
      end

      fun contract_letregions (p : (place,place*mul, qmularefset ref)LambdaPgm_psi) : unit =
          let
              fun on_letregion x =
                  let fun seek r nil = NONE
                        | seek r ((_,Mul.NUM _)::rs) = seek r rs
                        | seek r ((r',Mul.INF)::rs) =
                      (case (Eff.get_place_ty r, Eff.get_place_ty r') of
                           (SOME rt, SOME rt') => if rt=rt' then SOME r'
                                                  else seek r rs
                         | _ => die "contract")
                      fun t (nil,acc) = rev acc
                        | t ((p as (_,Mul.NUM _))::rs,acc) = t(rs,p::acc)
                        | t ((p as (r,Mul.INF))::rs, acc) =
                          (* look for infinite regions in acc with same runType *)
                          (case seek r acc of
                               SOME r' => (Eff.unifyRho (r,r') Eff.initCone; t(rs,acc))
                             | NONE => t(rs,p::acc))
                  in t (x,nil)
                  end
              fun f (LETREGION {B,rhos,body}) = (rhos := on_letregion(!rhos); APP_CONT)
                | f _ = APP_CONT
          in
              app f p
          end

      fun member lv nil = false
        | member lv (x::xs) = Lvar.eq(lv,x) orelse member lv xs

      type rng = {formals:(place*mul) list ref, rargss: place list ref list ref,
                  argss:place list ref list ref}

      (* Build initial a-list from formal region arguments; an a-list describes
       * which region parameters that may be collapsed. Because we do not wish
       * to collapse finite regions, unique numbers are chosen for finite
       * regions, whereas 1 is chosen for infinite regions. Further, infinite
       * regions of different types should not be collapsed. *)
      fun init_alist (pl: (place*mul)list) : int list =
          let
              fun lookup (ty,e,n) =
                  let fun look nil = (n,(ty,n)::e,n+1)
                        | look ((ty',i)::xs) = if ty=ty' then (i,e,n) else look xs
                  in look e
                  end
          in
              rev(#1(List.foldl (fn ((p,m),(l,e,n)) =>
                                 case m of
                                     Mul.NUM _ => (n::l,e,n+1)
                                   | Mul.INF =>
                                         (case Eff.get_place_ty p of
                                              SOME ty => let val (i,e,n) = lookup(ty,e,n)
                                                         in (i::l,e,n)
                                                         end
                                            | NONE => die "init_alist"))
                     (nil,nil,1) pl))
          end

      (* Build a-list from region vector. *)
      fun args_alist (pl: place list) : int list =
          let fun get n p e =
                let fun loop nil = (n, (p,n)::e,n+1)
                      | loop ((p',i)::rest) = if Eff.eq_effect(p,p') then (i,e,n)
                                              else loop rest
                in loop e
                end
          in
              rev(#1(List.foldl (fn (p,(l,e,n)) =>
                                 let val (i,e,n) = get n p e
                                 in (i::l,e,n)
                                 end)
                     (nil,nil,1) pl))
          end

      fun eq_E (E:(place list * int list)list) (p,p') =
          let fun loop z (nil,_) = NONE
                | loop z (_,nil) = NONE
                | loop z (x::xs,y::ys) = if Eff.eq_effect(z,x) then SOME y
                                         else loop z (xs,ys)
              fun loop2 n z nil = NONE
                | loop2 n z (x::xs) = (case loop z x of
                                           SOME i => SOME (n,i)
                                         | NONE => loop2 (n+1) z xs)
              fun find p : (int*int) option = loop2 1 p E
          in case find p of
              SOME pair => (case find p' of
                                SOME pair' => pair=pair'
                              | NONE => false)
            | NONE => false
          end

      fun rargs_alist (E: (place list * int list)list) (pl: place list) : int list =
          let fun eq (p,p') = Eff.eq_effect (p,p') orelse eq_E E (p,p')
              fun get n p e =
                  let fun loop nil = (n, (p,n)::e,n+1)
                        | loop ((p',i)::rest) = if eq(p,p') then (i,e,n)
                                                else loop rest
                  in loop e
                  end
          in
              rev(#1(List.foldl (fn (p,(l,e,n)) =>
                                 let val (i,e,n) = get n p e
                                 in (i::l,e,n)
                                 end)
                     (nil,nil,1) pl))
          end

      (* Collapse two a-lists into one a-list *)
      fun collapse_alist (l1:int list) (l2:int list) : int list =
          let fun get n p e =
                  let fun look nil = (n, (p,n)::e, n+1)
                        | look ((p',i)::rest) = if p = p' then (i,e,n) else look rest
                  in look e
                  end
          in rev(#1(ListPair.foldl (fn (i1,i2,(l,e,n)) =>
                                    let val (i,e,n) = get n (i1,i2) e
                                    in (i::l,e,n)
                                    end) (nil,nil,1) (l1,l2)))
          end

      fun pp_ls nil = ""
        | pp_ls [x] = x
        | pp_ls (x::xs) = x ^ "," ^ pp_ls xs
      fun pp_list nil = "[]"
        | pp_list xs = "[" ^ pp_ls xs ^ "]"
      fun pp_mul (Mul.NUM _) = "f"
        | pp_mul (Mul.INF) = ""
      fun pp_places xs = pp_list (map (fn p => PP.flatten1 (Eff.layout_effect p)) xs)
      fun pp_formals xs = pp_list (map (fn (p,m) => (PP.flatten1 (Eff.layout_effect p) ^ pp_mul m)) xs)
      fun pp_args xs = pp_places (!xs)
      fun pp_ints xs = pp_list (map Int.toString xs)
      fun pp_argss nil = ""
        | pp_argss (x::xs) = "  " ^ pp_args x ^ "\n" ^ pp_argss xs

      val touched = ref false
      val touch_count = ref 0
      fun unify_formals lv (alist: int list, formals: (place*mul)list) : unit =
          let fun unify (_,_,nil,nil) = ()
                | unify (x,p,x'::xs,(p',_)::ps) =
              if x = x' then (Eff.unifyRho_no_lowering (p,p');
                              touched := true;
                              touch_count := !touch_count + 1
(*                            ; print("UNIFYING(" ^ Lvar.pr_lvar lv ^ "\n") *)
                              )
              else unify (x,p,xs,ps)
                | unify _ = die "unify_formals.unify"
              fun loop (nil,nil) = ()
                | loop (x::xs, (p,_)::ps) = (unify (x,p,xs,ps); loop (xs,ps))
                | loop _ = die "unify_formals.loop"
          in loop (alist,formals)
          end

      fun trim_args (alist: int list, l:'a list) =
          let fun is_in (x,nil) = false
                | is_in (x,y::ys) = x=y orelse is_in(x,ys)
              fun loop (nil,nil,e) = nil
                | loop (x::xs,p::ps,e) = if is_in (x,e) then loop (xs,ps,e)
                                         else p :: loop (xs,ps,x::e)
                | loop _ = die "trim_args"
          in loop(alist,l,nil)
          end

      fun contract_args' (fss: (lvar*rng) list list) : unit =
          let
              fun alist_before_rec (formals,argss) =
                  let val alist1 = init_alist (!formals)
                      val alist2 = List.foldl (fn (args,al) =>
                                               collapse_alist (args_alist (!args)) al)
                          alist1 (!argss)
                  in alist2
                  end

              fun onef ((lvar,{formals,rargss,argss}), alist) =
                  ((* print ("FUNCTION " ^ Lvar.pr_lvar lvar ^ ":\n  ");
                   print (pp_formals (!formals) ^ "\n recursive apps:\n");
                   print (pp_argss (!rargss) ^ " apps:\n");
                   print (pp_argss (!argss) ^ " alist: ");
                   print (pp_ints alist ^ "\n"); *)
                   unify_formals lvar (alist,!formals);
                   formals := trim_args (alist,!formals);
                   List.app (fn args => args := (trim_args (alist,!args))) (!argss);
                   List.app (fn rargs => rargs := (trim_args (alist,!rargs))) (!rargss))

              fun one_group (fs : (lvar*rng)list) : unit =
                  let
                      fun process (with_alists) : ((lvar * rng) * int list) list =
                          let
                              val E = map (fn ((lv,{formals,...}),al) =>
                                           (map (fn (p,_) => p) (!formals), al))
                                  with_alists
                              val alists = map (fn ((lv,{rargss,...}),al) =>
                                                List.foldl (fn (rargs,al) =>
                                                            collapse_alist (rargs_alist E (!rargs)) al)
                                                al (!rargss))
                                  with_alists
                          in if map #2 with_alists = alists then with_alists
                             else (* compute new `with_alists' based on assumptions
                                   * and result, and reprocess. *)
                                 let
                                     val with_alists =
                                         ListPair.map (fn ((f,al),al') =>
                                                       (f,collapse_alist al al'))
                                         (with_alists,alists)
                                 in process with_alists
                                 end
                          end
                      val with_alists_init = map (fn r as (_, {formals,argss,...}) =>
                                                  (r,alist_before_rec (formals,argss))) fs
                      val with_alists = process with_alists_init
                  in List.app onef with_alists
                  end

              fun loop n =
                  (touched:=false;
                   List.app one_group fss;
                   if !touched then loop (n+1)
                   else n)
              val n = (touch_count := 0; loop 1)
          in
              print ("Argument contractions: " ^ Int.toString (!touch_count) ^ " - "
                     ^ Int.toString n ^ " rounds.\n")
          end

      fun contract_args p : unit =
          let val M : rng Lvar.Map.map ref = ref Lvar.Map.empty    (* for quick lookup *)
              val L : (lvar * rng) list list ref = ref nil         (* lists of mutually recursive functions *)
              val exported = exported_lvars p
              fun is_exported lv = member lv exported
              local
                  val fix_stack : lvar list ref = ref nil
              in
                  fun push lv : unit = fix_stack := (lv :: (!fix_stack))
                  fun pop() : unit =
                      case !fix_stack of
                          x::xs => fix_stack := xs
                        | _ => die "contract_args.pop"
                  fun is_rec lv = member lv (!fix_stack)
              end
              fun build e : app_cont =
                  case e of
                      VAR {lvar,fix_bound=true,rhos_actuals,...} =>
                          (case Lvar.Map.lookup (!M) lvar of
                               SOME {argss,rargss,...} =>
                                   if is_rec lvar then
                                       (rargss := (rhos_actuals :: (!rargss)); APP_BREAK)
                                   else
                                       (argss := (rhos_actuals :: (!argss)); APP_BREAK)
                             | NONE => APP_BREAK)
                    | FIX {functions,scope,...} =>
                     let val funs : (lvar * rng) list =
                         List.foldl (fn ({lvar,rhos_formals,...},acc) =>
                                     if is_exported lvar then acc
                                     else let val rng = {formals=rhos_formals,
                                                         argss=ref nil, rargss=ref nil}
                                          in M := Lvar.Map.add(lvar,rng,!M);
                                              (lvar,rng)::acc
                                          end)
                         nil functions
                     in
                         L := funs :: !L;  (* for processing   *)
                         List.app (fn {lvar,bind,...} => push lvar) functions;
                         List.app (fn {bind,...} => appt build bind) functions;
                         List.app (fn _ => pop ()) functions;
                         appt build scope;
                         APP_BREAK
                     end
                    | _ => APP_CONT
          in app build p;
              contract_args' (!L)
          end

      (* Region specialization; tranform

            let fun f [...r...] x = e
            in  ... f [...r'...] e' ...
            end

         into

            let fun f [...] x = e{r'/r}
            in ... f [...] e' ...
            end

         provided r' is in scope at the declaration of f and f is invariant in r
         and all applications of f instantiate r' for r.
      *)

      type srng = {R:place list list, formals:(place*mul) list ref,
                   rargss: place list ref list ref, argss:place list ref list ref}

      fun inR (p,nil) = false
        | inR (p,l::ls) = inR'(p,l) orelse inR (p,ls)
      and inR' (p,nil) = false
        | inR' (p,x::xs) = Eff.eq_effect(p,x) orelse inR' (p,xs)

      fun drop (nil,       nil)   = nil
        | drop (true::ds,  x::xs) = drop(ds,xs)
        | drop (false::ds, x::xs) = x :: drop(ds,xs)
        | drop _ = die "drop"

      val touched = ref false
      val counter = ref 0
      fun touch () = (touched := true;
                      counter:= (!counter + 1))

      fun same_arg n xss =
          let fun loop nil = NONE
                | loop (xs::xss) =
                 let val r = List.nth (!xs,n)
                 in case loop xss of
                     res as SOME r' => if Eff.eq_effect(r,r') then res
                                       else NONE
                   | NONE => SOME r
                 end
          in loop xss
          end handle _ => die "same_arg"

      fun invariant n p xss =
          let fun loop nil = true
                | loop (xs::xss) = (Eff.eq_effect(List.nth (!xs,n),p)
                                    andalso invariant n p xss)
          in loop xss
          end handle _ => die "invariant"

      fun spec (fss : (lvar * srng) list list) =
          let
              fun one_f (lv,{R,formals,rargss,argss}) =
                  let
                      (*
                      val _ = print ("FUNCTION " ^ Lvar.pr_lvar lv ^ "\n")
                      val _ = print ("R = " ^ String.concat (map pp_places R) ^ "\n")
                      val _ = print ("  formals: " ^ pp_formals (!formals) ^ "\n")
                      val _ = print ("  argss: " ^ pp_argss (!argss))
                      val _ = print ("  rargss: " ^ pp_argss (!rargss))
                          *)
                      fun go (n,nil) : bool list = nil  (* true=drop*)
                        | go (n,(f as (p,_))::fs) =
                          (case same_arg n (!argss) of
                               SOME r =>
                                   if inR (r, R) andalso invariant n p (!rargss)
                                       then (Eff.unifyRho_no_lowering (p,r);
                                             touch();
                                             true::go(n+1,fs))
                                   else false::go(n+1,fs)
                             | NONE => false::go(n+1,fs))
                      val ds = go (0,!formals)
                  in  formals := drop(ds,!formals)
                    ; List.app (fn rargs => rargs := drop(ds,!rargs)) (!rargss)
                    ; List.app (fn args => args := drop(ds,!args)) (!argss)
                  end
              fun loop n =
                  (touched := false;
                   List.app (List.app one_f) fss;
                   if !touched then loop (n+1)
                   else n)
              val n = (counter := 0; loop 1)
          in
              print ("Region specialization: " ^ Int.toString (!counter) ^ " - "
                     ^ Int.toString n ^ " rounds.\n")
          end

      fun specialize p : unit =
          let val M : srng Lvar.Map.map ref = ref Lvar.Map.empty    (* for quick lookup *)
              val L : (lvar * srng) list list ref = ref nil         (* lists of mutually recursive functions *)
              val exported = exported_lvars p
              fun is_exported lv = member lv exported
              local
                  val fix_stack : lvar list ref = ref nil
                  val reg_stack : place list list ref = ref
                      [[Eff.toplevel_region_withtype_top,
                        Eff.toplevel_region_withtype_string,
                        Eff.toplevel_region_withtype_pair,
                        Eff.toplevel_region_withtype_array,
                        Eff.toplevel_region_withtype_ref,
                        Eff.toplevel_region_withtype_triple]]
              in
                  fun push_lv lv : unit = fix_stack := (lv :: (!fix_stack))
                  fun pop_lv() : unit =
                      case !fix_stack of
                          x::xs => fix_stack := xs
                        | _ => die "specialize.pop_lv"
                  fun is_rec_lv lv = member lv (!fix_stack)
                  fun push_regs regs = reg_stack := (regs :: (!reg_stack))
                  fun pop_regs () : unit =
                      case !reg_stack of
                          x::xs => reg_stack := xs
                        | _ => die "specialize.pop_regs"
                  fun get_reg_stack () = !reg_stack
              end
              fun build e : app_cont =
                  case e of
                      VAR {lvar,fix_bound=true,rhos_actuals,...} =>
                          (case Lvar.Map.lookup (!M) lvar of
                               SOME {argss,rargss,...} =>
                                   if is_rec_lv lvar then
                                       (rargss := (rhos_actuals :: (!rargss)); APP_BREAK)
                                   else
                                       (argss := (rhos_actuals :: (!argss)); APP_BREAK)
                             | NONE => APP_BREAK)
                    | FIX {functions,scope,...} =>
                     let val funs : (lvar * srng) list =
                         List.foldl (fn ({lvar,rhos_formals,...},acc) =>
                                     if is_exported lvar then acc
                                     else let val srng = {R=get_reg_stack(),
                                                          formals=rhos_formals,
                                                          argss=ref nil, rargss=ref nil}
                                          in M := Lvar.Map.add(lvar,srng,!M);
                                              (lvar,srng)::acc
                                          end)
                         nil functions
                     in
                         L := funs :: !L;  (* for processing   *)
                         List.app (fn {lvar,bind,rhos_formals,...} =>
                                   (push_lv lvar; push_regs (map #1 (!rhos_formals)))) functions;
                         List.app (fn {bind,...} => appt build bind) functions;
                         List.app (fn _ => (pop_lv (); pop_regs())) functions;
                         appt build scope;
                         APP_BREAK
                     end
                    | LETREGION{B,rhos,body} =>
                     (push_regs (map #1 (!rhos));
                      appt build body;
                      pop_regs();
                      APP_BREAK)
                    | _ => APP_CONT
          in app build p;
              spec (!L)
          end


      val _ = Flags.add_bool_entry
          {long="contract_regions", short=SOME"cr", item=ref false,
           menu=["Control Region Analyses", "Regions", "contract regions"], neg=false,
           desc=
           "When this option is enabled, identically typed\n\
            \regions bound by the same letregion construct\n\
            \are unified. Moreover, region parameters to\n\
            \non-exported functions are trimmed whenever\n\
            \possible."}

      val contract_p = Flags.is_on0 "contract_regions"

      fun contract p =
           if contract_p() then
               (contract_letregions p;
                contract_args p;
                specialize p;
                contract_args p)
           else ()

  end

end
