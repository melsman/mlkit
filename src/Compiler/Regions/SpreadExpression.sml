(*
 *   The module introduces region and effect variables into the
 *   input lambda expression. This is done in such a way that the subsequent
 *   phases of translation will not have to generate fresh region or effect
 *   variables. In other words, all generation of fresh region and effect
 *   variables takes place in SpreadExpression.
 *)

structure SpreadExpression: SPREAD_EXPRESSION =
struct
  structure PP = PrettyPrint
  structure RSE = RegionStatEnv
  structure R = RType
  structure E = LambdaExp
  structure E' = RegionExp
  structure Eff = Effect
  structure LB = LambdaBasics

  fun uncurry f (a,b) = f a b

  structure E = E
  structure E' = E'
  structure RegionStatEnv = RSE

  val preserve_tail_calls = Flags.is_on0 "preserve_tail_calls"

  val warn_spurious_p = Flags.add_bool_entry
    {long="warn_spurious",short=NONE, menu=["Control", "Regions"],
     item=ref false, neg=false, desc=
     "Warn on the presence of a spurious type variable. This\n\
     \flag is relevant only when garbage collection is enabled."}

  val stats_spurious_p = Flags.add_bool_entry
    {long="statistics_spurious",short=SOME "stats_spurious",
     menu=["Control", "Regions"],
     item=ref false, neg=false, desc=
     "Report statistics on spurious functions and instantiations\n\
     \of spurious type variable. This flag is relevant only when\n\
     \garbage collection is enabled."}

  val disable_spurious_p = Flags.is_on0 "disable_spurious_type_variables"

  structure SpuriousStats : sig
    val fundeclare : unit -> unit
    val funspurious : unit -> unit
    val tvinstance : bool -> unit
    val report     : unit -> unit
    val reset      : unit -> unit
  end = struct
    val insts_total = ref 0
    val funs_total = ref 0
    val insts_spurious = ref 0
    val funs_spurious = ref 0
    fun reset () =
        ( insts_spurious := 0
        ; insts_total := 0
        ; funs_spurious := 0
        ; funs_total := 0
        )
    fun pr (ref n, ref t) =
        Int.toString n ^ " / " ^ Int.toString t
    fun report () =
        ( print ("*** Spurious instantiations: " ^ pr (insts_spurious, insts_total) ^ "\n")
        ; print ("*** Spurious functions: " ^ pr (funs_spurious, funs_total) ^ "\n")
        )
    fun incr r = r := !r + 1
    fun fundeclare () = incr funs_total
    fun funspurious () = incr funs_spurious
    fun tvinstance b =
        ( incr insts_total
        ; if b then incr insts_spurious else ()
        )
  end


  type rse = RSE.regionStatEnv

  type source_pgm = E.LambdaPgm
  type source_exp = E.LambdaExp

  type targetExp = (R.place, unit) E'.LambdaExp
  type targetPgm = (R.place, unit) E'.LambdaPgm

  type tyname = TyName.TyName
  type tyvar = E.tyvar
   and con = E.con
   and Type = E.Type
   and effect = Eff.effect
   and place = Eff.place
   and cone = Eff.cone

  infix footnote
  fun x footnote y = x
  fun say s = TextIO.output(TextIO.stdOut, s ^ "\n");
  fun logsay s = TextIO.output(!Flags.log, s);
  fun logtree (t:PP.StringTree) = PP.outputTree(logsay, t, !Flags.colwidth)

  fun log_sigma (sigma1, lvar) =
    case R.bv sigma1 of
      (_, _, []) =>
        (say ("***" ^ Lvars.pr_lvar lvar ^ " is:");
         logsay (Lvars.pr_lvar lvar ^ " is:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n")
    | ([],_,alpha::alphas) =>
        (say ("******" ^ Lvars.pr_lvar lvar ^ " is  polymorphic with escaping regions");
         logsay (Lvars.pr_lvar lvar ^ " is polymorphic with escaping regions:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n")
    | (_,_,alpha::alphas) =>
        (say ("***" ^ Lvars.pr_lvar lvar ^ " is  polymorphic");
         logsay (Lvars.pr_lvar lvar ^ " is polymorphic:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n");

  fun print_tree t = PP.outputTree(print, t, !Flags.colwidth)
  fun print_mu (tau,rho) = print_tree
    (PP.NODE{start="(",finish=")",childsep=PP.RIGHT",",indent=1,
             children=[R.mk_lay_sigma' false ([],[],[],tau), Eff.layout_effect rho]})

  fun print_effects effects = print_tree
      (PP.NODE{start="{",finish="}",childsep=PP.RIGHT",",indent=1,
               children=map Eff.layout_effect effects})

  fun print_tau tau = print_tree (R.mk_lay_sigma' false ([],[],[],tau))
  fun print_sigma sigma = print_tree (R.mk_lay_sigma false sigma)

  fun noSome x msg =
    case x of
      SOME it => it
    | NONE => Crash.impossible msg

  fun die s = Crash.impossible ("SpreadExpression." ^ s)

  (* functionality to avoid dangling pointers when garbage collection
   * is enabled; see page 50 of Tofte & Talpin, A Theory of Stack
   * Allocation in Polymorphically Typed Languages. 1993. Technical
   * Report. *)

  val dangling_pointers = Flags.is_on0 "dangling_pointers"

(*
  fun gc_no_dangling_pointers(rse,blvs,e,B,mus1,mus2,eps,rho) =
    if not(tag_values()) then B
    else
      let
        fun rhos_sigma lv : place list =
          case lookupLvar rse lv
            of SOME(_,_,sigma,p,_,_) => [p] @ R.frv_sigma sigma
             | NONE => die "gc_no_dangling_pointers.rhos_sigma"
        fun rhos_sigma' ex : place list =
          case lookupExcon rse ex
            of SOME mu => R.frv_mu mu
             | NONE => die "gc_no_dangling_pointers.rhos_sigma"
        val (lvs,exs) = LB.freevars (blvs,e)
        val rhos = List.foldl (fn (lv, acc) => rhos_sigma lv @ acc) nil lvs
        val rhos = List.foldl (fn (ex, acc) => rhos_sigma' ex @ acc) rhos exs
        val rhos_not = R.frv_mu (R.mkFUN(mus1,eps,mus2),rho)
        val rhos = Eff.setminus(Eff.remove_duplicates rhos, rhos_not)
        fun drop_rho_p (r:place) =
          Eff.eq_effect(r, Eff.toplevel_region_withtype_top)
          orelse Eff.eq_effect(r, Eff.toplevel_region_withtype_bot)
          orelse Eff.eq_effect(r, Eff.toplevel_region_withtype_string)
          orelse Eff.eq_effect(r, Eff.toplevel_region_withtype_real)

        val rhos = (List.filter (not o Eff.is_wordsize o valOf o Eff.get_place_ty) rhos)
          handle _ => die "gc_no_dangling_pointers.rhos"
        val rhos = List.filter (not o drop_rho_p) rhos
(*
        val B = List.foldl (fn (r,B) => Eff.lower (Eff.level B) r B) B rhos
*)
        val phi = mkUnion (map Eff.mkGet rhos)
        val (eps2,B) = freshEps B
        val _ = edge (eps2,phi)
      in Eff.unifyEps (eps,eps2) B
      end
*)

  fun crash_resetting force =
      let val fcn = if force then "forceResetting" else "resetRegions"
      in die ("S: argument to " ^ fcn ^ " must be a variable which\
       \ is monomorphic (also in regions and effects)")
      end

  val exn_ty  = E.CONStype([], TyName.tyName_EXN)

  fun declareMany (rho,rse)([],[]) = rse
    | declareMany (rho,rse)((lvar,regvars,tyvars,sigma_hat,bind):: rest1, occ::occ1) =
        declareMany(rho,RSE.declareLvar(lvar,(true,true,regvars,sigma_hat,SOME rho,SOME occ,NONE), rse))(rest1,occ1)
    | declareMany _ _ = die ".declareMany"


  fun repl ([],[]) = []
    | repl ({lvar,regvars,tyvars,Type,bind}::fcns1, sigma_hat::sigma_hat_rests) =
            (lvar,regvars,tyvars,sigma_hat,bind):: repl(fcns1,sigma_hat_rests)
    | repl _ = die ".repl: sigma_hat_list and rhs lists have different lengths"


  fun adjust_instances (transformer, occ as ref l) =
      app (fn r as ref(il, f)=> r:= (il, transformer o f)) l

  fun mkRhs (rse,rho) ([],[],[]) = (rse,[])
    | mkRhs (rse,rho) ((lvar,regvars,tyvars,sigma_hat,bind)::rest1,
                       (t1,tau1,sigma1,tvs1)::rest2,
                       occ::rest3) =
      let val (brhos, bepss,_) = R.bv sigma1
          val transformer = R.matchSchemes (sigma_hat, sigma1)
                            handle R.FAIL_MATCH msg =>
                                   die ("mkRhs: lvar = " ^ Lvars.pr_lvar lvar ^ "\n" ^ msg)
          val _ = adjust_instances (transformer, occ)
          val function = {lvar = lvar, occ = occ, tyvars = ref tvs1, rhos = ref brhos, epss = ref bepss,
                          Type = tau1, formal_regions = NONE, bind = t1}
          val rse2 = RSE.declareLvar(lvar,(true, true, regvars, R.insert_alphas(tvs1, sigma1),
                                           SOME rho, SOME occ, NONE), rse)
          val (rse2', l) = mkRhs (rse2,rho) (rest1,rest2,rest3)
      in (rse2', function::l)
      end
    |  mkRhs _ _ = die ".mkRhs"

  exception Abort

  fun die_from_S e =
         (TextIO.output(TextIO.stdOut,
                "Failed to spread expression:" ^
                 PP.flatten(PP.format(200, E.layoutLambdaExp e )) ^ "\n");
          raise Abort)

  fun save_il (instances_opt, il_r) =
    (* record il in the environment ---
     to be picked up for letrec-bound variables at fix *)
      case instances_opt of
          SOME(r as ref(list)) =>
         (* lvar is fix bound or global
          (from earlier topdec);
          extend the instances list for
          lvar in the region-static environment
          with the instantiation list of the lvar
         *)
         r:= il_r::list
        | NONE => ()

  fun pushIfNotTopLevel (toplevel,B) =
      if toplevel then B else Eff.push B;

  fun Below (B, mus) =
    let val free_rhos_and_epss = R.ann_mus mus []
        val B' = List.foldl (uncurry (Eff.lower(Eff.level B - 1)))
                            B free_rhos_and_epss
    in
        Eff.popAndClean(B')
    end

  datatype cont = TAIL | NOTAIL

  fun retract (B, t as E'.TR(e, E'.Mus mus, phi), cont, tvs) =
    if false (*preserve_tail_calls()*) andalso cont = TAIL then   (* (Eff.restrain B, t, TAIL) *)
      let val free_rhos_and_epss = R.ann_mus mus []
          val B = List.foldl (uncurry (Eff.lower(Eff.level B - 1)))
                             B free_rhos_and_epss
(*        val _ = app (fn effect =>
                       let val effect = if Eff.is_get effect orelse Eff.is_put effect then Eff.rho_of effect
                                        else effect
                       in Eff.unify_with_toplevel_effect effect
                       end) (Eff.topLayer B) *)
          val B = List.foldl (fn (eff,B) => Eff.lower 1 eff B) B (Eff.topLayer B)
(*
          val phi' = mkUnion([])
          val (discharged_phi,_) = observeDelta(Eff.level B - 1, Eff.Lf[phi],phi')
(*
          val _ = app (fn effect =>
                       let val effect = if Eff.is_get effect orelse Eff.is_put effect then Eff.rho_of effect
                                        else effect
                       in Eff.unify_with_toplevel_effect effect
                       end) discharged_phi
*)
          val B = List.foldl (fn (eff,B) => lower 1 eff B) B discharged_phi
*)
(*        val B = Eff.restrain B *)
      in (#2 (Eff.pop B), t, TAIL, tvs)
      end
    else
    let
      val (B_discharge,B_keep) = Below(B, mus)
      val phi' = Eff.mkUnion([])
      val (discharged_phi,_) = Eff.observeDelta(Eff.level B_keep, Eff.Lf[phi],phi')
      (* phi' updated to contain observed effect *)
    in (B_keep, E'.TR(E'.LETREGION_B{B= ref B_discharge,
                                     discharged_phi = ref discharged_phi,
                                     body = t}, E'.Mus mus, phi'),
        NOTAIL, tvs)
    end
    | retract (B, t, c, tvs) = (B, t, c, tvs)

  val count_RegEffClos = ref 0 (* for statistics (toplas submission) *)

  (* The `level' parameter is used to separate those type, region
     and effect variables that occur free in the type environement
     from those that do not. In a call (spreadExp rse tyvarmap l e)
     the invariant is that a variable occurs free in rse if and only
     if it has level less than or equal to l.
        The lambda level is increased locally each time new variables may be
     added to the type environment. This happens at lambda abstractions
     and fix expressions.
  *)

  fun unMus s (E'.Mus mus) = mus
    | unMus s (E'.Frame _) = die ("unMus - " ^ s ^ ": expecting Mus metaType, got a Frame")
    | unMus s (E'.RaisedExnBind) = die ("unMus - " ^ s ^ ": expecting Mus metaType, got a RaisedExnBind")

  fun deepError (rv:RegVar.regvar) (msg:string) =
      let open Report infix //
          val report0 = case RegVar.get_location_report rv of
                            SOME rep => rep
                          | NONE => null
          val report = line msg
      in raise DeepError (report0 // report)
      end

  fun mem nil y = false
    | mem (x::xs) y = x = y orelse mem xs y

  fun findSpurious cone lvar tyvars tvs : tyvar list * (tyvar*Eff.effect option) list * cone =
      let val (bound,free) = (* split into bound spurious and free spurious *)
              foldl (fn (tv,(bound,free)) =>
                        if mem tyvars tv then (tv::bound,free)
                        else (bound,tv::free))
                    (nil,nil) tvs
          val () = SpuriousStats.fundeclare()
          val () = if not(List.null bound) then SpuriousStats.funspurious() else ()
          val () = if warn_spurious_p() andalso not(List.null bound)
                   then print ("*** WARNING: Spurious quantified type variables for function " ^
                               Lvars.pr_lvar lvar ^ ": " ^
                               String.concatWith ", " (map E.pr_tyvar bound) ^
                               "\n")
                   else ()
          val (tvs,cone) = foldr (fn (tv,(tvs,cone)) =>
                                     if mem bound tv andalso not (disable_spurious_p()) then
                                       let val (eff,cone) = Eff.freshEps cone
                                       in ((tv,SOME eff)::tvs,cone)
                                       end
                                     else ((tv,NONE)::tvs,cone))
                                 (nil,cone)
                                 tyvars
      in (free,tvs,cone)
      end

  val spuriousJoin = RSE.spuriousJoin

  fun proper_recursive (lvar, _, _, _, bind) : bool = (* does lvar occur in bind? *)
      LB.foldTD (fn a => (fn E.VAR {lvar=lv,...} => a orelse Lvars.eq(lv,lvar)
                           | _ => a)) false bind

  fun spreadExp (B: cone, rse,  e: E.LambdaExp, toplevel, cont:cont)
      : cone * (place,unit)E'.trip * cont * tyvar list =
  let
    fun lookup tyname = case RSE.lookupTyName rse tyname of
          SOME arity =>
            let val (a, l, c) = RSE.un_arity arity
            in SOME(a, (*List.size*) (l), c)
            end
        | NONE => NONE

    val (freshType, freshMu) = R.freshType lookup

(*
    fun freshTypes (cone:cone, types: E.Type list) =
        case types of
            [] => ([],cone)
          | (tau_ml::rest) => let val (tau, cone) = freshType(tau_ml,cone)
                                  val (taus, cone) = freshTypes(cone,rest)
                              in (tau::taus, cone)
                              end
*)
    fun freshTypesWithPlaces (cone:cone, types: E.Type list) =
        case types of
            [] => ([],cone)
          | (tau_ml::rest) => let val (mu, cone) = freshMu(tau_ml,cone)
                                  val (mus, cone) = freshTypesWithPlaces(cone, rest)
                              in (mu::mus, cone)
                              end

     fun mk_sigma_hat_list (B,retract_level) [] = (B,[])
       | mk_sigma_hat_list (B,retract_level)({lvar,regvars,tyvars,Type,bind}::rest) =
          let
            (*val _ = TextIO.output(TextIO.stdOut, "mk_sigma_hat_list: " ^ Lvars.pr_lvar lvar ^ "\n")*)
            val B = Eff.push B         (* for generalize_all *)
            val (tau_x_ml, tau_1_ml) =
                case Type of
                    E.ARROWtype p => p
                  | _ => die "mk_sigma_hat_list"
              val (tau_0, B) = freshType(Type,B)
              val (B,sigma) = R.generalize_all(B,retract_level,map (fn tv => (tv,NONE)) tyvars,tau_0)
              val sigma_hat = R.drop_alphas sigma
            val (_,B) = Eff.pop B (* back to retract level *)
            val (B, l) = mk_sigma_hat_list(B,retract_level) rest
          in
             (B,sigma_hat::l)
          end

    fun newInstance (lvopt:Lvars.lvar option,A: cone,sigma:R.sigma, taus: E.Type list): cone*R.Type*R.il =
      let val (rhos, epss, alphas) = R.bv sigma
          val (taus', A) = freshTypesWithPlaces(A,taus)
          val (rhos', A) = Eff.freshRhosPreserveRT(rhos, A)
          val (epss', A) = Eff.freshEpss(epss, A)
          val il = R.mk_il(rhos',epss',taus')
          val (tau', A1) = R.inst(sigma,il) A (* side-effects il *)
          val () = if warn_spurious_p() then
                     ListPair.appEq (fn ((tv,NONE),t) => ()
                                      | ((tv,SOME _),t) =>
                                        if R.unboxed t then ()
                                        else
                                        let val f = case lvopt of
                                                        SOME lv => Lvars.pr_lvar lv
                                                      | NONE => "?"
                                        in print ("*** WARNING: Instantiation of spurious type variable "
                                                  ^ E.pr_tyvar tv
                                                  ^ " with boxed type (or tyvar) in function "
                                                  ^ f
                                                  ^ "\n")
                                        end)
                                    (alphas,taus')
                   else ()
          val () = if stats_spurious_p() then
                     ListPair.appEq (fn ((tv,opt),t) =>
                                        let val b = Option.isSome opt andalso not(R.unboxed t)
                                        in SpuriousStats.tvinstance b
                                        end)
                                    (alphas,taus')
                   else ()
      in
          (A1, tau', il)
      end

    (* get_exn_mu(mu') if mu' is the type and place of a nullary exception constructor,
       return mu'; otherwise mu' = (mu_1 -> mu_2, rho): return mu_2 *)

    fun get_exn_mu mu =
        case R.unBOX mu of
            SOME (ty,_) =>
            (case R.unFUN ty of
                 SOME(_,_,[mu2]) => mu2
               | _ => mu)
          | NONE => mu

    fun maybe_explicit_rho (rse:rse) (B:cone) (tau:R.Type) (rv_opt:RegVar.regvar option) : place * R.mu * cone =
        let val rt = case R.runtype tau of
                         SOME rt => rt
                       | NONE => die "maybe_explicit_rho: expecting boxed type"
            val (rho,B) =
                case rv_opt of
                    NONE => Eff.freshRhoWithTy(rt, B)
                  | SOME rv =>
                    case RSE.lookupRegVar rse rv of
                        SOME rho =>
                        (case Eff.get_place_ty rho of
                             NONE => die "impossible: maybe_explicit_rho"
                           | SOME Eff.BOT_RT => (rho,B) before Eff.setRunType rho rt
                           | SOME rt' => if rt = rt' then (rho,B)
                                         else deepError rv ("Mismatching region types "
                                                            ^ Eff.show_runType rt ^ " and "
                                                            ^ Eff.show_runType rt' ^ " for "
                                                            ^ "explicit region variable `"
                                                            ^ RegVar.pr rv))
                      | NONE => deepError rv ("Explicit region variable `" ^ RegVar.pr rv
                                              ^ " is not in scope.")
        in (rho, R.mkBOX(tau,rho), B)
        end

    fun maybe_explicit_rho_opt (rse:rse) (B:cone) (tau:R.Type) (rv_opt:RegVar.regvar option)
        : place option * R.mu * cone =
        let val (rho:place option,B) =
                case rv_opt of
                    NONE =>
                    (case R.runtype tau of
                         SOME rt =>
                         let val (rho,B) = Eff.freshRhoWithTy(rt, B)
                         in (SOME rho,B)
                         end
                       | NONE => (NONE,B))
                  | SOME rv =>
                    case RSE.lookupRegVar rse rv of
                        SOME rho =>
                        (case R.runtype tau of
                             NONE => deepError rv ("Cannot associate explicit region variable `"
                                                   ^ RegVar.pr rv ^ " with value of unboxed type")
                           | SOME rt =>
                             (case Eff.get_place_ty rho of
                                  NONE => die "impossible: maybe_explicit_rho"
                                | SOME Eff.BOT_RT => (SOME rho,B) before Eff.setRunType rho rt
                                | SOME rt' => if rt = rt' then (SOME rho,B)
                                              else deepError rv ("Mismatching region types "
                                                                 ^ Eff.show_runType rt ^ " and "
                                                                 ^ Eff.show_runType rt' ^ " for "
                                                                 ^ "explicit region variable `"
                                                                 ^ RegVar.pr rv)))
                      | NONE => deepError rv ("Explicit region variable `" ^ RegVar.pr rv
                                              ^ " is not in scope.")
        in case rho of
               SOME p => (rho, R.mkBOX(tau,p), B)
             | NONE => (rho, tau, B)
        end

    fun meetSwitch _    TAIL = TAIL
      | meetSwitch TAIL _    = TAIL
      | meetSwitch _    _    = NOTAIL

    fun spreadSwitch (B:cone) spread con excon_mus
                     (E.SWITCH(e0: E.LambdaExp,
                               choices: ('c * E.LambdaExp) list,
                               last: E.LambdaExp option),toplevel,cont)
        : cone * (place,unit)E'.trip * cont * tyvar list =
    let
      val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
      val (B,t0 as E'.TR(e', meta_0, phi_0),_,tvs0) = spread(B,e0,false,NOTAIL)
      val mu_0 = case unMus "spreadSwitch" meta_0 of
                     [mu_0] => mu_0
                   | _ => die "S. ill-typed object of switch"
      val B = List.foldl (uncurry (fn mu => R.unify_mu(get_exn_mu mu,mu_0)))
                         B excon_mus

      val (B, new_choices, contAcc, tvsAcc) =
          List.foldr (fn ((c, e), (B, ts, contAcc, tvsAcc)) =>
                         let val (B, t, cont, tvs) = spread(B,e,toplevel,cont)
                         in (B, t:: ts, meetSwitch cont contAcc,spuriousJoin tvs tvsAcc)
                         end) (B,[],NOTAIL,tvs0) choices

      val (B, new_last, contAcc, tvsAcc) =
          case last of
              NONE => (B,NONE,contAcc,tvsAcc)
            | SOME e_last => let val (B, t_last, cont, tvs) = spread(B,e_last,toplevel,cont)
                             in (B, SOME t_last, meetSwitch cont contAcc, spuriousJoin tvs tvsAcc)
                             end

      (* unify types of branches - when they are not frames or raised Bind types *)

      val (B,metatype) =
          case List.find (fn E'.TR(_,E'.Mus mus,_) => true | _ => false) new_choices of
              SOME(E'.TR(_,E'.Mus mus1,_)) =>
              (List.foldl (fn (E'.TR(_,E'.Mus mus,_),B) => R.unify_mus(mus,mus1)B
                          | (E'.TR(_, _, _),B) => B)
                          B
                          (case new_last of NONE => new_choices | SOME t' => t'::new_choices),
               E'.Mus mus1)
            | SOME _ => die "spreadSwitch"
            | NONE =>
              case List.find (fn E'.TR(_,E'.Frame _, _) => true | _ => false) new_choices of
                  SOME (E'.TR(_,metatype,_)) => (B,metatype)
                | NONE => (B, E'.RaisedExnBind)

      (* val accumulate effects*)
      val phis = map (fn E'.TR(_,_,phi_i) => phi_i) new_choices
      val phis = case new_last of NONE => phis
                                | SOME (E'.TR(_,_,phi_n)) => phi_n :: phis
      val phis = phi_0 :: phis
      val phis = case R.unBOX mu_0 of
                     SOME(_,object_rho) => Eff.mkGet object_rho :: phis
                   | NONE => phis
      val e' = E'.SWITCH(t0,ListPair.zip(map #1 choices, new_choices), new_last)
    in
      retract(B,E'.TR(con(e'), metatype, Eff.mkUnion phis), contAcc, tvsAcc)
    end handle X as Report.DeepError _ => raise X
             | X => die ("spreadSwitch: cannot spread; exception " ^ exnName X ^ " raised")

    fun freshBoxMu s B tau =
        case R.runtype tau of
            NONE => die ("freshBoxMu: " ^ s)
          | SOME rt => let val (rho,B) = Eff.freshRhoWithTy(rt, B)
                       in (B, rho, R.mkBOX(tau,rho))
                       end

    fun spreadSwitch' (B:cone) spread con excon_mus
                      (E.SWITCH(e0: E.LambdaExp,
                                choices: (('c * 'ignore) * E.LambdaExp) list,
                                last: E.LambdaExp option),toplevel,cont)
        : cone * (place,unit)E'.trip * cont * tyvar list =
        spreadSwitch B spread con excon_mus (E.SWITCH(e0, map (fn ((c,_),e) => (c,e)) choices, last),
                                             toplevel,cont)

    fun S (B,e,toplevel:bool,cont:cont) : cone * (place,unit)E'.trip * cont * tyvar list =
      (case e of
      E.VAR{lvar, instances : E.Type list, regvars} =>
       (case RSE.lookupLvar rse lvar of
          SOME(compound,create_region_record,formal_regvars,
               sigma,place0opt,instances_opt, transformer) =>
            let
              val (B, tau, il_1) = newInstance(SOME lvar,B,sigma,instances)
              val il_r = ref (il_1, fn p => p)
              val _ = save_il(instances_opt, il_r)
              val fix_bound = compound andalso create_region_record
              val phi = if fix_bound then
                          case place0opt of
                              SOME place0 => Eff.mkGet place0
                            | NONE => die "S.VAR:expecting boxed fix-bound function"
                        else Eff.empty
              val () =
                  case regvars of
                      [] => ()
                    | rv::_ => if length regvars = length formal_regvars then ()
                               else if length regvars = 1 andalso null formal_regvars then
                                 die "VAR - currently not supported case - one actual regvar / zero formal regvars"
                               else deepError rv ("The number of actual explicit region variables does \
                                                  \not match the number of formal explicit region variables.")
              val B =
                  if null regvars then B
                  else let val all = ListPair.zipEq (#1 (R.bv sigma), #1 (R.un_il il_1))
                           val explicits = ListPair.zipEq (formal_regvars, regvars)
                           fun find f nil = NONE
                             | find f ((p,p')::rest) = case Eff.getRegVar p of
                                                           SOME rv => if RegVar.eq(rv,f) then SOME p'
                                                                      else find f rest
                                                         | NONE => find f rest
                       in List.foldl(fn ((f,a),B) =>
                                        (* find actual associated with explicit formal in all *)
                                        case find f all of
                                            NONE => die "S.VAR.expecting to find formal explicit region variable in scheme"
                                          | SOME place_actual =>
                                            case RSE.lookupRegVar rse a of
                                                SOME rho => Eff.unifyRho (place_actual,rho) B
                                              | NONE => deepError a ("Explicit region variable `" ^ RegVar.pr a ^ " not in scope"))
                                    B explicits
                       end
              (* For each spurious bound tyvar in sigma, tyvars in the instantiated type are
                 returned as spurious... *)
              val tvs_spurious =
                  ListPair.foldlEq
                      (fn ((_,NONE),_,acc) => acc
                        | ((_,SOME _),ty,acc) => RSE.spuriousJoin (R.ftv_ty ty) acc)
                      nil
                      (#3(R.bv sigma), #3(R.un_il il_1))
                  handle ListPair.UnequalLengths => die "VAR:instantiation list error"
              val mu = case place0opt of
                           SOME p => R.mkBOX(tau,p)
                         | NONE => tau
            in
                (B,E'.TR(E'.VAR{lvar = lvar, fix_bound=fix_bound, il_r = il_r},
                         E'.Mus [mu], phi),
                 NOTAIL,
                 tvs_spurious)
            end
         | NONE => die "spreadExp: free lvar"
       )
    | E.INTEGER (i,tau_ml) =>
      let val (mu, B) = freshMu(tau_ml,B)
          val (tau,rho_opt) = R.unbox mu
          val phi = case rho_opt of SOME rho => Eff.mkPut rho | NONE => Eff.empty
      in (B,E'.TR(E'.INTEGER(i, tau, rho_opt),E'.Mus[mu], phi),
          NOTAIL,
          [])
      end
    | E.WORD (i, tau_ml) =>
      let val (mu, B) = freshMu(tau_ml,B)
          val (tau,rho_opt) = R.unbox mu
          val phi = case rho_opt of SOME rho => Eff.mkPut rho | NONE => Eff.empty
      in (B,E'.TR(E'.WORD(i, tau, rho_opt),E'.Mus[mu], phi),
          NOTAIL,
          [])
      end
    | E.STRING(s: string,rv_opt)=>
      let val (rho, mu, B) = maybe_explicit_rho rse B R.stringType rv_opt
      in (B, E'.TR(E'.STRING(s, rho),E'.Mus [mu], Eff.mkPut rho),
          NOTAIL,
          [])
      end
    | E.REAL(r: string,rv_opt)=>
      let val (rho, mu, B) = maybe_explicit_rho rse B R.realType rv_opt
      in (B, E'.TR(E'.REAL(r, rho),E'.Mus [mu], Eff.mkPut rho),
          NOTAIL,
          [])
      end
    | E.F64 r =>
      let val (mu, B) = freshMu(E.f64Type,B)
      in (B, E'.TR(E'.F64 r,E'.Mus[mu], Eff.empty),
          NOTAIL,
          [])
      end
    | E.PRIM(E.UB_RECORDprim, args) =>
        (* For simplicity, we demand that the arguments of UB_RECORDprim must themselves
           have a singleton list of type and places. Thus we do not allow, for example
           UB_RECORD[UB_RECORD[2, 3], UB_RECORD[4,5]] although we do allow
           UB_RECORD[RECORD[2, 3], RECORD[4,5]] and
           UB_RECORD[2, 3, 4,5]
        *)
      let val (B, triples, mus, phis, tvs) =
              List.foldl(fn (exp, (B, triples', mus, phis, tvs)) =>
                            let val (B,trip as E'.TR(e',meta1,phi), _, tvs') = S(B, exp, false, NOTAIL)
                                val mus1 = unMus "S.UB_RECORDprim" meta1
                            in case mus1 of
                                   [mu] => (B, trip::triples', mu :: mus, phi::phis, spuriousJoin tvs' tvs)
                                 | _ => die ".S: unboxed record expression with compound, unboxed argument"
                            end) (B,[],[],[],[]) args
          val phi = Eff.mkUnion(rev phis)
          val mus = rev mus
          val triples = rev triples
        in
          (B, E'.TR(E'.UB_RECORD triples, E'.Mus mus, phi),
           NOTAIL,
           tvs)
        end
    | E.FN{pat: (E.lvar * E.Type) list, body: E.LambdaExp} =>
        let
          val (mus, B) = freshTypesWithPlaces (B, map #2 pat)
          val rse' = List.foldl (fn ((lvar, mu), rse) =>
                                    let val (tau,rho_opt) = R.unbox mu
                                    in RSE.declareLvar(lvar, (false,false,[],
                                                              R.type_to_scheme tau,
                                                              rho_opt,NONE,NONE), rse)
                                    end) rse
                                (ListPair.zip(map #1 pat, mus))
          val (B,t1 as E'.TR(e1',meta1, phi1), _, tvs) = spreadExp(B,rse',body,false,TAIL)
          val mu_list1 = unMus "S.FN" meta1
          val (eps, B) = Eff.freshEps B
          val _ = Eff.edge(eps, phi1)

          val (rho, B) = Eff.freshRhoWithTy(Eff.TOP_RT, B)
          val ty0 = R.mkFUN(mus,eps,mu_list1)

          val (free, tvs') =
              if dangling_pointers() then (NONE, nil)
              else
                (*region inference without dangling pointers*)
                let val free = LB.freevars e
                in (SOME free, RSE.spuriousTyvars rse ty0 free)
                end
        in
          (B, E'.TR(E'.FN{pat = ListPair.zip(map #1 pat, mus), body = t1, alloc = rho, free=free},
                    E'.Mus [R.mkBOX(ty0,rho)], Eff.mkPut(rho)),
           NOTAIL,
           spuriousJoin tvs' tvs)
        end
    | E.APP(e1_ML: E.LambdaExp, e2_ML: E.LambdaExp,_) =>
        let
            val simple_application: bool =
                case e1_ML of E.VAR{lvar, ...} =>
                       (case RSE.lookupLvar rse lvar of
                          SOME(compound, _,_,_,_,_,_) => not(compound)
                        | _ => die ("E.APP(E.VAR ...): Lvar " ^ Lvars.pr_lvar lvar ^ " not in RSE."))
                | _ => false

            val B = if simple_application then B else pushIfNotTopLevel(toplevel,B)
            val (B,t1 as E'.TR(e1, meta1, phi1), _, tvs1) = S(B,e1_ML, false, NOTAIL)
            val (ty,rho_0) =
                case unMus "S.APP" meta1 of
                  [mu] => noSome (R.unBOX mu) "S.APP: expecting boxed function"
                | _ => die "E.APP.singleton mus expected"
            val (mus2,eps_phi0,mus1) =
                case R.unFUN ty of
                  SOME f => f
                | NONE => die "E.APP.function expected"
            val (B,t2 as E'.TR(e2, meta2, phi2), _, tvs2) = S(B,e2_ML, false, NOTAIL)
            val mus2' = unMus "S.APP2" meta2
            val B = R.unify_mus (mus2,mus2') B
            val tvs = spuriousJoin tvs1 tvs2
        in
          if simple_application then
            (B, E'.TR(E'.APP(t1,t2), E'.Mus mus1,
                      Eff.mkUnion([eps_phi0, Eff.mkGet rho_0, phi1,phi2])),
             cont,
             tvs)
          else retract(B, E'.TR(E'.APP(t1,t2), E'.Mus mus1,
                                Eff.mkUnion([eps_phi0, Eff.mkGet rho_0, phi1,phi2])),
                       cont,
                       tvs)
        end

   | E.LET{pat=nil, bind = e1_ML, scope = e2_ML} =>   (* wild card *)
        let
           val (B, t1 as E'.TR(e1, meta1, phi1), _, tvs1) = S(B, e1_ML, false, NOTAIL)
           val mus = unMus "S.LET-wildcard" meta1
           val (B, t2 as E'.TR(e2, meta2, phi2), cont, tvs2) = S(B, e2_ML, toplevel, cont)
        in
          (B, E'.TR(E'.LET{pat = nil,
                           bind = t1, scope = t2}, meta2, Eff.mkUnion([phi1,phi2])),
           cont,
           spuriousJoin tvs1 tvs2)
        end

   | E.LET{pat, bind = e1_ML, scope = e2_ML} =>
        let
           val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
           val (B, t1 as E'.TR(e1, meta, phi1), _, tvs1) = S(B, e1_ML, false, NOTAIL)
           val mus = unMus "S.LET" meta
           fun loop_pat ([], [], B, rse, pat'_list) = (B,rse, rev pat'_list)
             | loop_pat ((lvar,alphas,tau_ML):: rest_bind, mu1 :: mu_rest,
                         B, rse, pat'_list) =
               let val (tau1,rho_opt) = R.unbox mu1
                   val sigma = R.type_to_scheme tau1
(*                 val _ = log_sigma(R.insert_alphas(alphas, sigma),lvar)*)
                   val alphas = map (fn tv => (tv,NONE)) alphas            (* TODO MAEL: for those in tvs1, SOME eps, where eps is fresh... *)
                   val rse = RSE.declareLvar(lvar,
                                             (false,false,[],R.insert_alphas(alphas, sigma),
                                              rho_opt, NONE, NONE),rse)
               in
                 loop_pat(rest_bind, mu_rest, B, rse,
                          (lvar,alphas,tau1,rho_opt) :: pat'_list)
               end
             | loop_pat _ = die ".loop_pat: length of pattern and list of types and places differ"

           val (B,rse, pat'_list) = loop_pat(pat, mus, B, rse, [])
           val (B, t2 as E'.TR(e2, meta2, phi2),cont,tvs2) = spreadExp(B,rse,e2_ML,toplevel,cont)
        in
          retract(B, E'.TR(E'.LET{pat = pat'_list,
                                  bind = t1, scope = t2}, meta2, Eff.mkUnion([phi1,phi2])),
                  cont,
                  spuriousJoin tvs1 tvs2)
        end

(* good (as in paper):
    | E.FIX{functions as[{lvar, tyvars,Type,bind}], scope} =>
        let
          val B = Eff.push(B);         (* for pop in retract *)
            val retract_level = Eff.level B
            val (rho,B) = Eff.freshRho B (* for shared region closure *)
            val B = Eff.push(B);         (* for R.generalize_all *)
              val E.ARROWtype(tau_x_ml, tau_1_ml) = Type
              val (tau_0, B) = freshType(Type,B)
              val (B,sigma) = R.generalize_all(B,retract_level,tyvars,tau_0)
            val (_,B) = Eff.pop B (* back to retract level *)
            val B = Eff.push(B)
              val sigma_hat = R.drop_alphas sigma
              val occ = ref []  : R.il ref list ref
              val (B, t1 as E'.TR(E'.FN{pat, body = t1', alloc}, E'.Mus [(tau1, rho1)], phi1)) =
                  spreadExp(B, RSE.declareLvar(lvar,(true,true,sigma_hat, rho, SOME occ, NONE), rse), bind)
              val B = Eff.unifyRho(rho1,rho) B
              val (B,sigma1,msg_opt) = R.regEffClos(B, retract_level, phi1, tau1)
            val (_,B) = Eff.pop B (* back to retract level *)
            val transformer = R.matchSchemes(sigma_hat, sigma1) handle R.FAIL_MATCH msg =>
                  die ("fix: lvar = " ^ Lvars.pr_lvar lvar ^ "\n" ^ msg)
            val _ = adjust_instances(transformer, occ)
            val (B, t2 as E'.TR(e2, E'.Mus mus, phi2)) =
                  spreadExp(B, RSE.declareLvar(lvar,(true, true, R.insert_alphas(tyvars, sigma1),
                                                     rho1, SOME occ, NONE), rse), scope)
            val (_,brhos, bepss) = R.bv(sigma1)
            val e' = E'.FIX{shared_clos = rho,
                          functions = [{lvar = lvar, tyvars = tyvars, rhos = brhos, epss = bepss,
                                        Type = tau1, formal_regions = NONE, bind = t1}],
                          scope = t2}
        in
          retract(B, E'.TR(e', E'.Mus mus, Eff.mkUnion[phi1,phi2]))
        end (* FIX *)
good *)

    | E.FIX{functions=nil, scope} => S(B,scope,toplevel,cont)
    | E.FIX{functions, scope} =>
        let
          val B = pushIfNotTopLevel(toplevel,B) ;         (* for pop in retract *)
            val retract_level = Eff.level B
            val (rho,B) = Eff.freshRhoWithTy(Eff.TOP_RT,B) (* for shared region closure *)
            val phi1 = Eff.mkPut rho
            val (B,sigma_hat_list) = mk_sigma_hat_list(B,retract_level) functions
            val (B,rse2,functions',tvs) = spreadFcns(B,rho,retract_level,rse)(repl(functions,sigma_hat_list))
            val (B, t2 as E'.TR(_, meta2, phi2), cont, tvs2) = spreadExp(B, rse2, scope,toplevel,cont)
            val e' = E'.FIX{shared_clos=rho,functions = functions',scope = t2}
        in
          retract(B, E'.TR(e', meta2, Eff.mkUnion([phi1,phi2])),
                  cont,
                  spuriousJoin tvs tvs2)
        end (* FIX *)

    | E.EXCEPTION(excon, ty_opt: E.Type option, e2: E.LambdaExp) =>
        let
            val B = pushIfNotTopLevel(toplevel,B); (* for pop in retract *)
            val (ty,nullary) =
                case ty_opt of
                    SOME ty1 => (E.ARROWtype([ty1], [exn_ty]),false)
                  | NONE => (exn_ty, true)
            val (mu, B) = freshMu(ty, B)
            val (tau,rho) = noSome (R.unBOX mu) "S.EXCEPTION: expecting boxed type"
            (* lower all the region and effect variables of mu to have level 2 (not 0),
               so that they cannot be generalised over. Level 2, because it is generated
               in this program unit, unless unified with another lower-level rho. *)
          (*
            val B = EdList.foldL (Eff.lower 2) B (R.ann_mus [mu] [])
          *)
          (*NO! Lower only rho! (Otherwise region variables that are associated
           with type variables and have runtime type BOT become global.) *)

            (* If GC is enabled, we need to lower all region and
               effect variables in mu to avoid dangling pointers in
               exception values that escape to toplevel, for instance!
             *)

            val B = Eff.lower 2 rho B
            val B = if dangling_pointers() then B
                    else let val ty = #1(R.unbox mu)
                         in case R.unFUN ty of
                                SOME([mu],_,_) =>
                                foldl (fn (e,B) => Eff.lower 2 e B) B (R.ann_mus [mu] [])
                              | SOME _ => die "EXCEPTION.impossible"
                              | NONE => B
                         end

            (* if exception constructor is unary: unify place of exception
               constructor  and place of its result type. Note: I think
               we could have chosen not to identify these two regions, but
               both have runtype RT_TOP... *)
            val B = case R.unFUN tau of
                        SOME(_,_,mus as [mu_res]) =>
                        let val (_,rho_res) = noSome (R.unBOX mu_res) "S.EXCEPTION: expecting boxed result"
                        in Eff.unifyRho (rho_res, rho) B
                        end
                      | _ => B
            val rse' = RSE.declareExcon(excon, mu, rse)
            val (B, t2 as E'.TR(e2', meta2, phi2), cont, tvs) = spreadExp(B,rse',e2, toplevel, cont)
            val tvs' = if dangling_pointers()
                       then nil
                       else R.ftv_ty tau
        in
          retract(B, E'.TR(E'.EXCEPTION(excon, nullary, mu, rho, t2), meta2,
                           Eff.mkUnion([Eff.mkPut rho,phi2])),
                  cont,
                  RSE.spuriousJoin tvs tvs')
        end

    | E.RAISE(e1: E.LambdaExp, description) =>
      let val (description',B) =
              case description of
                  E.Types (taus : E.Type list) =>
                  let val (mus, B) = freshTypesWithPlaces(B,taus)
                  in (E'.Mus mus, B)
                  end
                | E.RaisedExnBind => (E'.RaisedExnBind, B)
                | E.Frame _ => die "spreading of RAISE failed: RAISE was annotated with a FRAME type"
          val (B, t2 as E'.TR(e2', meta2, phi2), _, tvs) = S(B,e1,false,NOTAIL)
          val _ = unMus "S.RAISE" meta2
      in (B,E'.TR(E'.RAISE(t2),description', phi2),NOTAIL,tvs)
      end
    | E.LETREGION {regvars,scope} =>
      let val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (B,rse) = List.foldl (fn (rv,(B,rse)) =>
                                       let val (rho,B) = Eff.freshRhoRegVar (B,rv)
                                       in (B,RSE.declareRegVar(rv,rho,rse))
                                       end) (B,rse) regvars
      in retract(spreadExp(B,rse,scope,toplevel,NOTAIL))
      end
    | E.HANDLE(e1,e2) =>
        let
          val B = pushIfNotTopLevel(toplevel,B); (* for retract *)
          val (B, t1 as E'.TR(e1', meta1, phi1), _,tvs1) = S(B,e1, false, NOTAIL)
          val (B, t2 as E'.TR(e2', meta2, phi2), _,tvs2) = S(B,e2, false, NOTAIL)
          val mus1 = unMus "S.HANDLE1" meta1
          val mus2 = unMus "S.HANDLE2" meta2
        in
          case mus2 of
              [mu2] =>
              (case R.unBOX mu2 of
                   NONE => die "S.HANDLE: expecting boxed handler"
                 | SOME(ty,rho2) =>
                   (case R.unFUN ty of
                        SOME(mus21,arreff,mus22) =>
                        let val B = R.unify_mus(mus22,mus1) B
                            val phi = Eff.mkUnion([phi1,phi2,arreff,Eff.mkGet rho2])
                         (* lower all the region and effect variables of mus21 to have level 2 (not 0),
                            so that they cannot be generalised ever. Level 2, because it is generated
                            in this program unit, unless unified with another lower-level rho. *)
                           val B = List.foldl (uncurry (Eff.lower 2))
                                              B (R.ann_mus mus21 [])
                        in
                          retract(B, E'.TR( E'.HANDLE(t1,t2), E'.Mus mus22, phi),
                                  NOTAIL,
                                  spuriousJoin tvs1 tvs2)
                        end
                      | NONE => die "S: ill-typed handle expression"))
            | _ => die "S: ill-typed handle expression"
        end
    | E.PRIM(E.REFprim{instance,regvar}, [e1]) =>
(*

                                          e1' : [mu], phi1
                 -------------------------------------------------------------
                        ref e1'  : [(mu ref, rho_new)],  phi1 u {put rho_new}
*)
        let
          val (B, t1 as E'.TR(e1', meta1, phi1), _,tvs) = S(B,e1, false, NOTAIL)
          val mus1 = unMus "S.REFprim" meta1
          val tau = R.mkCONSTYPE(TyName.tyName_REF, mus1,[],[])
          val (rho_new, mu, B) = maybe_explicit_rho rse B tau regvar
          val phi = Eff.mkUnion([Eff.mkPut rho_new, phi1])
        in (B, E'.TR(E'.REF (rho_new, t1), E'.Mus [mu],phi),
            NOTAIL,
            tvs)
        end

    | E.PRIM(E.DEREFprim{instance}, [e1])=>
(*
                           e1' : [([mu] ref,rho)], phi1
                       --------------------------------------
                        ! e1'  : [mu],  phi1 u {get rho}
*)
        let
          val B = pushIfNotTopLevel(toplevel,B)
          val (B, t1 as E'.TR(e1', meta1, phi1), _, tvs) = S(B,e1, false, NOTAIL)
        in
          case unMus "S.DEREFprim" meta1 of
              [mu] =>
              (case R.unBOX mu of
                   SOME(ty,rho) =>
                   (case R.unCONSTYPE ty of
                        SOME(tyname_ref, mus, [], []) =>
                        retract(B, E'.TR(E'.DEREF t1, E'.Mus mus,
                                         Eff.mkUnion([Eff.mkGet rho, phi1])),
                                NOTAIL,
                                tvs)
                      | _ => die "S: ill-typed dereferencing")
                 | NONE => die "S: DEREF")
            | _ => die "S: ill-typed dereferencing"
        end

    | E.PRIM(E.ASSIGNprim{instance}, [e1, e2]) =>
(*

                        e1' : [(mu ref, rho1)], phi1                e2': [mu],phi2
                       ---------------------------------------------------------------------
                        e1' := e2'   : [(unit, rho3)],    phi1 u phi2 u {get rho1, put rho1, put rho3}

        We have a get on rho1 since one needs to access the reference in order to update it;
        and we have a put rho1, since we write the updated ref object back in memory.
        (Note: in multiplicity inference, the put effect should not be counted: it does not
        generate a new ref object and hence does not require allocation of more space.

        Moreover, the put effect causes the region to be passed to := at runtime; it seems more
        natural to leave out the put effect.

*)
        let
          val B = pushIfNotTopLevel(toplevel,B); (* for retract *)
          val (B, t1 as E'.TR(e1', meta1, phi1), _, tvs1) = S(B,e1, false, NOTAIL)
          val (B, t2 as E'.TR(e2', meta2, phi2), _, tvs2) = S(B,e2, false, NOTAIL)
          val mus1 = unMus "S.ASSIGNprim1" meta1
          val mus2 = unMus "S.ASSIGNprim2" meta2
        in case (mus1,mus2) of
               ([mu1], [mu2]) =>
               (case R.unBOX mu1 of
                    SOME(ty1,rho1) =>
                    (case R.unCONSTYPE ty1 of
                         SOME(ref_tyname, [mu1],[],[]) =>
                         let val B = R.unify_mu (mu1,mu2) B
                             val phi = Eff.mkUnion([(*Eff.mkPut rho1,mael*) Eff.mkGet rho1,phi1, phi2])
                         in retract(B, E'.TR(E'.ASSIGN(t1,t2), E'.Mus [R.unitType], phi),
                                    NOTAIL,
                                    spuriousJoin tvs1 tvs2)
                         end
                       | _ => die "S: ill-typed assignment")
                  | NONE => die "S: ASSIGN: expecting boxed mu")
             | _ => die "S: ill-typed assignment"
        end


    | E.PRIM(E.DROPprim, [e1]) =>   (* to do wild cards properly *)
(*
                           e1' : [mu], phi1
                       ----------------------
                        drop e1' : [],  phi1
*)
        let
          val B = pushIfNotTopLevel(toplevel,B); (* for retract *)
          val (B, t1 as E'.TR(e1', meta1, phi1), _, tvs) = S(B,e1, false, NOTAIL)
(*          val mus1 = unMus "S.DROPprim" meta1 *)
        in
          retract(B, E'.TR(E'.DROP t1, E'.Mus [], phi1), NOTAIL, tvs)
        end

    | E.SWITCH_I {switch: IntInf.int E.Switch, precision} =>
        (spreadSwitch B S (fn sw => E'.SWITCH_I{switch=sw,precision=precision}) [] (switch,toplevel,cont))
    | E.SWITCH_W {switch: IntInf.int E.Switch, precision} =>
        (spreadSwitch B S (fn sw => E'.SWITCH_W{switch=sw,precision=precision}) [] (switch,toplevel,cont))

    | E.SWITCH_S(stringsw: string E.Switch) => (spreadSwitch B S E'.SWITCH_S [] (stringsw,toplevel,cont))
    | E.SWITCH_C(consw: (E.con*E.lvar option) E.Switch) => (spreadSwitch' B S E'.SWITCH_C [] (consw,toplevel,cont))
    | E.SWITCH_E(exconsw: (E.excon*E.lvar option) E.Switch as
                 E.SWITCH(_,choices,_))=>(spreadSwitch' B S E'.SWITCH_E
                                          (map (fn ((excon,_),_) => noSome (RSE.lookupExcon rse excon)
                                                "spreadExceptionSwitch: excon not in rse")
                                           choices) (exconsw, toplevel, cont))

    | E.PRIM(E.CONprim{con, instances, regvar}, []) =>
        let
          val sigma = noSome (RSE.lookupCon rse con) ".S: constructor not in RSE"
          val (B, tau, il) = newInstance(NONE,B,sigma,instances)
          val aux_regions = (case R.unCONSTYPE tau of
                               SOME(_,_,rhos,_) => rhos
                             | NONE => die "S: nullary constructor not of constructed type")
          val (rho, mu, B) = maybe_explicit_rho_opt rse B tau regvar
          val rhos = case rho of
                         SOME p => p :: aux_regions
                       | NONE => aux_regions
        in
          (B, E'.TR(E'.CON0{con=con, il = il, aux_regions=aux_regions, alloc = rho}, E'.Mus [mu],
                    Eff.mkUnion(map Eff.mkPut rhos)),
           NOTAIL, [])
        end
    | E.PRIM(E.CONprim{con, instances, regvar}, [arg]) =>
        let
          val sigma = noSome (RSE.lookupCon rse con) "S (CONprim): constructor not in RSE"
          val (B, tau', il) = newInstance(NONE,B,sigma,instances)
          val (mu1,_,mus2,mu2) =
              case R.unFUN tau' of
                SOME(mu1,areff, mus2 as [mu2]) => (mu1,areff,mus2,mu2)
              | _ => die "S: unary constructor not functional"
          val (B, t1 as E'.TR(e1', meta1', phi1), _, tvs) = S(B, arg, false, NOTAIL)
          val mu1' = unMus "S.CONprim" meta1'
          val B = R.unify_mus(mu1',mu1) B
          val (rho,phi) =
              case R.unBOX mu2 of
                  SOME(_,p) => (SOME p, Eff.mkUnion [phi1,Eff.mkPut p])
                | NONE => (NONE, phi1)
          val B = case regvar of
                      NONE => B
                    | SOME rv =>
                      case RSE.lookupRegVar rse rv of
                          SOME rho' =>
                          (case rho of
                               SOME rho => Eff.unifyRho (rho',rho) B
                             | NONE => deepError rv ("Cannot associate explicit region variable `" ^ RegVar.pr rv
                                                     ^ " with unboxed value constructor"))
                        | NONE => deepError rv ("Explicit region variable `" ^ RegVar.pr rv
                                                ^ " not in scope")
        in
          (B, E'.TR(E'.CON1({con=con, il=il, alloc=rho},t1), E'.Mus mus2,
                    phi),
           NOTAIL,
           tvs)
        end
    | E.PRIM(E.DECONprim{con, instances,...}, [arg]) =>
        let
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val sigma = noSome (RSE.lookupCon rse con) "S (DECONprim): constructor not in RSE"
          val (B, tau', il) = newInstance(NONE,B,sigma,instances)
          val (mus1,arreff,mus2,mu2) =
              case R.unFUN tau' of
                  SOME(mus1,areff, mus2 as [mu2]) => (mus1,areff,mus2,mu2)
                | _ => die "S: unary constructor not functional"
          val (B, t1 as E'.TR(e1', meta1', phi1), _, tvs) = S(B, arg, false, NOTAIL)
          val mus1' = unMus "S.DECONprim" meta1'
          val B = R.unify_mus(mus1',mus2) B
          val phi = case R.unBOX mu2 of
                        SOME (_,r) => Eff.mkUnion [phi1,Eff.mkGet r]
                      | NONE => phi1
        in retract(B, E'.TR(E'.DECON({con=con, il=il},t1), E'.Mus mus1,
                            phi),
                   NOTAIL,
                   tvs)
        end
    | E.PRIM(E.EXCONprim excon, []) =>
        let
          val mu = noSome (RSE.lookupExcon rse excon) ".S: nullary exception constructor not in RSE"
          (* No effect since nullary constructors are simply looked up in the
             environment, as in the Definition, rule 106
          *)
        in
          (B, E'.TR(E'.EXCON(excon,NONE), E'.Mus [mu], Eff.mkUnion([])),
           NOTAIL,
           [])
        end
    | E.PRIM(E.EXCONprim excon, [arg]) =>
        (case S(B,arg, false, NOTAIL) of
          (* expression denotes value *)
          (B,t_arg as E'.TR(arg_e, E'.Mus mus, phi_arg), _, tvs) =>
            let val mu = noSome (RSE.lookupExcon rse excon) ".S: unary exception constructor not in RSE"
                val (tau,_) = noSome (R.unBOX mu) ".S: unary exception constructor function not boxed"
            in case R.unFUN tau of
                   SOME(mus1,arreff,mus_result as [mu_result]) =>
                   let val (_,rho_result) = noSome (R.unBOX mu_result) ".S: unary exception constructor not boxed"
                       val B = R.unify_mus(mus1,mus) B
                       val phi = Eff.mkPut rho_result               (* maybe unify region for function and region for exception *)
                   in
                     (B, E'.TR(E'.EXCON(excon,SOME (rho_result,t_arg)),
                               E'.Mus mus_result, Eff.mkUnion([phi,phi_arg])),
                      NOTAIL,
                      tvs)
                   end
                 | _ => die "S: unary exception constructor ill-typed"
            end
          (* expression denotes frame or failing top-level binding : *)
          | (B,t_arg as E'.TR(arg_e, E'.RaisedExnBind, phi_arg), _, _) =>
            die "S: exception constructor applied to frame or raised Bind exception"
          | _ => die "S(B,PRIM(EXCON...),...)"
       )
    | E.PRIM(E.DEEXCONprim excon, [arg]) =>
        let
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val mu = noSome (RSE.lookupExcon rse excon) "S (DEEXCONprim): exception constructor not in RSE"
          val (tau,p) = noSome (R.unBOX mu) "S (DEEXCONprim): expecting boxed type"
          val (mus1,arreff,mus2,mu2) =
              case R.unFUN tau of
                SOME(mus1,areff, mus2 as [mu2]) => (mus1,areff,mus2,mu2)
              | _ => die "S: unary exception constructor not functional"
          val (B, t1 as E'.TR(e1', meta1', phi1), _, tvs) = S(B, arg, false, NOTAIL)
          val mus1' = unMus "S.DEEXCONprim" meta1'
          val B = R.unify_mus(mus1',mus2) B
          val rho = case R.unBOX mu2 of
                        SOME (_,p) => p
                      | NONE => die "expecting boxed exception type"
        in
          retract(B, E'.TR(E'.DEEXCON(excon,t1), E'.Mus mus1,
                           Eff.mkUnion([phi1,Eff.mkGet rho])),
                  NOTAIL,
                  tvs)
        end
    | E.PRIM(E.RECORDprim rv_opt,args) =>
        let val (B, trips, tvs) = List.foldr (fn (arg, (B, trips, tvs)) =>
                    let val (B, trip, _, tvs') = S(B,arg, false, NOTAIL)
                    in (B, trip::trips, spuriousJoin tvs' tvs)
                    end) (B,[],[]) args
            val tau = R.mkRECORD(map (fn E'.TR(_,E'.Mus [mu],_) => mu | _ => die "S.record: boxed arg") trips)
            val (rho, mu, B) = maybe_explicit_rho_opt rse B tau rv_opt
            val phis = map (fn E'.TR(_,_,phi) => phi) trips
            val phis = case rho of
                           SOME p => Eff.mkPut p :: phis
                         | NONE => phis
            val phi = Eff.mkUnion phis
        in
          (B, E'.TR(E'.RECORD(rho, trips), E'.Mus [mu], phi),
           NOTAIL,
           tvs)
        end
    | E.PRIM(E.SELECTprim i, [arg as E.VAR _]) => (* avoid retract for this case *)
        let
          val (B, t1 as E'.TR(e1', meta1, phi1), _, tvs) = S(B,arg, false, NOTAIL)
          val (mus,rho) =
              case unMus "S.SELECTprim-VAR" meta1 of
                  [mu] =>
                  (case R.unBOX mu of
                       SOME (ty,rho) =>
                       (case R.unRECORD ty of
                            SOME mus => (mus,rho)
                          | NONE => die "S (select) : not record type")
                     | NONE => die "S (select) : not boxed record type")
                | _ => die "S (select) : not record type"
          val mu = List.nth(mus,i) handle Subscript => die "S (select) : select index out of range"
          val phi = Eff.mkUnion([Eff.mkGet rho, phi1])
        in
          (B, E'.TR(E'.SELECT(i, t1), E'.Mus [mu], phi),
           NOTAIL,
           tvs)
        end
    | E.PRIM(E.SELECTprim i, [arg]) =>
        let
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (B, t1 as E'.TR(e1', meta1, phi1), _, tvs) = S(B,arg, false, NOTAIL)
          val (mus,rho) =
              case unMus "S.SELECTprim" meta1 of
                  [mu] =>
                  (case R.unBOX mu of
                       SOME (ty,rho) =>
                       (case R.unRECORD ty of
                            SOME mus => (mus,rho)
                          | NONE => die "S (select) : not record type")
                     | NONE => die "S (select) : not boxed record type")
                | _ => die "S (select) : not record type"
          val mu = List.nth(mus,i) handle Subscript => die "S (select) : select index out of range"
          val phi = Eff.mkUnion([Eff.mkGet rho, phi1])
        in
          retract(B, E'.TR(E'.SELECT(i, t1), E'.Mus [mu], phi),
                  NOTAIL,
                  tvs)
        end

    | E.PRIM(E.EQUALprim{instance: E.Type}, [arg1, arg2]) =>

            (*            arg1 => [mu1], phi1  arg2 => [mu2], phi2  rho fresh
                          ---------------------------------------------------
                           arg1 = arg2 => [(bool,rho)], phi

                where phi = {Put rho} u {Get(rho') | rho' in frv(mu1,mu2)}
            *)

        let
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (B,  t1 as E'.TR(e1', meta1, phi1),_,tvs1) = S(B,arg1,false,NOTAIL)
          val (B,  t2 as E'.TR(e2', meta2, phi2),_,tvs2) = S(B,arg2,false,NOTAIL)
          val mus1 = unMus "S.EQUALprim1" meta1
          val mus2 = unMus "S.EQUALprim2" meta2
          val phi = Eff.mkUnion(phi1 :: phi2 ::
                                map Eff.mkGet(List.filter Eff.is_rho (R.ann_mus (mus1 @ mus2) [])))
          val (mu1,mu2) = case (mus1,mus2) of ([mu1],[mu2]) => (mu1,mu2)
                                            | _ => die "S: ill-typed equality"
        in
          retract(B, E'.TR(E'.EQUAL({mu_of_arg1 = mu1, mu_of_arg2 = mu2},t1,t2),
                           E'.Mus [R.boolType], phi),
                  NOTAIL,
                  spuriousJoin tvs1 tvs2)
        end

    (*`CCALLprim {name, ...} es' is like an application of a variable `name'
     to `es', only `name' does not have a region type scheme in the
     environment & it can take multiple arguments.  How, then, do we obtain a
     region type scheme for `name'?  The code for `name' is not available to
     make region inference on so we use its ML type scheme.  So we spread
     (freshMu) the ML type scheme.  As `name' is a function, its ML type
     scheme has an arrow & we must decide what arrow effect to put there.  We
     assume `name' gets from every region in the argument type & puts in
     every region in the result type.  Except that we assume there are no
     effects on regions that are associated with tyvars.  (The polymorphic c
     function with region type scheme "All 'a'r'rr.('a,'r) ->
     (('a,'r)*('a,'r), 'rr)" does not really get or put on 'r (but it puts on
     'rr).)  So it is a little troublesome to find the get & put regions.
     Also, we must ensure that it is always the same region variable that is
     paired with a specific tyvar.  (In the example above, if we just spread
     the underlying ML type scheme we would get different region variables on
     the occurences of 'a: "All 'a'r1'r2'r3'rr.('a,'r1) ->
     (('a,'r2)*('a,'r3), 'rr)".  See?)  See also the chapter `Calling C
     Functions' in the documentation.*)

    | E.PRIM (E.CCALLprim {name, instances, tyvars, Type}, es) =>
       (let val B = pushIfNotTopLevel (toplevel, B) (* for retract *)
            val (B, sigma) =
              let val B = Eff.push B   (* for sigma *)
                  val (ty, B) = freshType (Type, B)
                    handle X => (print "CCALL-1.1\n"; raise X)
                  val (sigma, B) = R.sigma_for_c_function tyvars ty B
                    handle X => (print "CCALL-1.2\n"; raise X)
                  val B = #2(Eff.pop B)
                    handle X => (print "CCALL-1.3\n"; raise X)
              in (B, sigma)  (* for sigma *)
              end handle X => (print "CCALL-1\n"; raise X)
            (*much of the rest is analogous to the case for (APP (VAR ..., ...))*)
            val (B, tau, _) = newInstance (NONE,B, sigma, instances)
              handle X => (print "CCALL-2\n"; raise X)
        in
          (case R.unFUN tau of
             SOME (mus_a, eps_phi0, [mu_r]) =>
               let
                 val (B, trs', mus_es, phis, tvs) =
                       List.foldr (fn (e, (B, trs', mus_es, phis, tvs)) =>
                       let val (B, tr' as E'.TR (_, meta', phi), _, tvs') = S (B, e, false, NOTAIL)
                           val mus' = unMus "S.CCALLprim" meta'
                       in (case mus' of
                             [mu'] => (B, tr' :: trs', mu' :: mus_es, phi :: phis, spuriousJoin tvs' tvs)
                           | _ => die "S: CCALL argument had not precisely one mu")
                       end) (B, [], [], [], []) es
                 val B = R.unify_mus (mus_a, mus_es) B
                   handle X => (print "CCALL-3\n"; raise X)
                 val rhos_for_result = R.c_function_effects (sigma,mu_r)
                   handle X => (print "CCALL-4\n"; raise X)
                 val e' = E'.CCALL ({name = name, mu_result = mu_r,
                                     rhos_for_result = rhos_for_result}, trs')
               in
                 retract (B, E'.TR (e', E'.Mus [mu_r], Eff.mkUnion (eps_phi0 :: phis)),
                          NOTAIL,
                          tvs)
               end
           | _ => die "CCALL: tau not function type")
        end handle (X as Report.DeepError _) => raise X
                 | X => (print ("CCALL FAILED\n"); raise X))

    | E.PRIM(E.BLOCKF64prim, args) =>
        let val (B, trips, tvs) = List.foldr (fn (arg, (B, trips, tvs)) =>
                    let val (B, trip, _, tvs') = S(B,arg, false, NOTAIL)
                    in (B, trip::trips, spuriousJoin tvs' tvs)
                    end) (B,[],[]) args
            val () = List.app (fn E'.TR(_,E'.Mus [mu],_) =>
                                  if R.isF64Type mu then ()
                                  else die "S.blockf64: expecting f64 type"
                                | _ => die "S.blockf64: expecting one mu") trips
            val (B, rho, mu) = freshBoxMu "BLOCKF64prim" B R.stringType
            val phi = Eff.mkUnion(Eff.mkPut rho :: map (fn E'.TR(_,_,phi) => phi) trips)
        in
          (B, E'.TR(E'.BLOCKF64(rho, trips), E'.Mus [mu], phi),
           NOTAIL,
           tvs)
        end

    | E.PRIM(E.SCRATCHMEMprim n, []) =>
        let val (B, rho, mu) = freshBoxMu "SCRATCHMEMprim" B R.stringType
            val phi = Eff.mkPut rho
        in
          (B, E'.TR(E'.SCRATCHMEM(n,rho), E'.Mus [mu], phi),
           NOTAIL,
           [])
        end

    | E.PRIM (E.EXPORTprim {name, instance_arg, instance_res}, [e0]) =>
          (*
                     e  => [mu], phi    mu=(mu1 -phi0-> mu2,rho)
                   \/v \in frev(mu). v is toplevel
                  ------------------------------------------------
                   _export(name,mu1,mu2,e) : [unit], phi
           *)

       (let val B = pushIfNotTopLevel (toplevel, B) (* for retract *)
            val (B, tr' as E'.TR (_, meta, phi), _, tvs) = S (B, e0, false, NOTAIL)
            val mus = unMus "S.EXPORTprim" meta
        in case mus of
               [mu] =>
               let val (ty,rho) = noSome (R.unBOX mu) "S.EXPORT: expecting boxed function"
               in case R.unFUN ty of
                      SOME([mu1],eps_phi0,[mu2]) =>
                      let (*val (mu1',B) = freshMu(instance_arg,B)
                            val B = R.unify_mu(mu1,mu1')B
                            val (mu2',B) = freshMu(instance_res,B)
                            val B = R.unify_mu(mu2,mu2')B *)
                          val e' = E'.EXPORT ({name=name, mu_arg=mu1, mu_res=mu2}, tr')
                          val mu = R.unitType
                          val effects = R.ann_mus mus []
(*
                          val _ = (  print "effects before unification with toplevel effects: "
                                   ; print_effects effects
                                   ; print "\n")
*)
                          (* First, lower effects to top-level(i.e., level 1) *)
                          val B = foldl (fn (e,B) => Eff.lower 1 e B) B effects
                          val B = Eff.unify_with_toplevel_rhos_eps (B, effects)
(*
                          val _ = (  print "effects after unification with toplevel effects: "
                                   ; print_effects effects
                                   ; print "\n")
*)
                      in
                        retract (B, E'.TR (e', E'.Mus [mu], (*was: eps_phi0*) phi),
                                 NOTAIL,
                                 tvs)
                      end
                    | _ => die "EXPORT: function does not have function type"
               end
             | _ => die "EXPORT: function does not have function type"
        end handle X => (print "EXPORT-1\n"; raise X))

    | E.PRIM(E.RESET_REGIONSprim{instance = _}, [e0 as (E.VAR _)] ) =>
          (*
                     x  => [mu], empty  rho fresh
                  -----------------------------------
                   resetRegions x : [(unit,rho)], phi

              where phi = {Put rho} u {Put rho' | rho' in frv(mu)}
              and x has to be lambda-bound. The put effects on frv(mu) ensure
              that these regions are not removed by dropping of Put regions;
              these puts should not be added in the multiplicity inference.
          *)
          let
            val (B, t as E'.TR(e',meta0,_), _, tvs) = S(B,e0,false,NOTAIL)
            val mus0 = unMus "S.RESET_REGIONSprim" meta0
            val mu = R.unitType
            val phi = Eff.mkUnion(map Eff.mkPut(List.filter Eff.is_rho (R.ann_mus mus0 [])))
          in
            case e' of
              E'.VAR{il_r as ref il, ...} =>
                 (case  R.un_il (#1 il) of ([],[],[]) =>
                    (B,E'.TR(E'.RESET_REGIONS({force=false, regions_for_resetting = []},t), E'.Mus [mu], phi),
                     NOTAIL,
                     tvs)
                  | _ => crash_resetting false)
            | _ => crash_resetting false
          end
    | E.PRIM(E.RESET_REGIONSprim{instance = _}, _ ) => crash_resetting false
    | E.PRIM(E.FORCE_RESET_REGIONSprim{instance = _}, [e0 as (E.VAR _)] ) =>
          (*  same as RESET_REGIONSprim, except that "force" is set to true in the result *)
          let
            val (B, t as E'.TR(e',meta0,_), _, tvs) = S(B,e0,false,NOTAIL)
            val mus0 = unMus "S.FORCE_RESET_REGIONSprim" meta0
            val mu = R.unitType
            val phi = Eff.mkUnion(map Eff.mkPut(List.filter Eff.is_rho (R.ann_mus mus0 [])))
          in
            case e' of
              E'.VAR{il_r as ref il, ...} =>
                 (case  R.un_il (#1 il) of ([],[],[]) =>
                    (B,E'.TR(E'.RESET_REGIONS({force=true, regions_for_resetting = []},t), E'.Mus [mu], phi),
                     NOTAIL,
                     tvs)
                  | _ => crash_resetting true)
            | _ => crash_resetting true
          end
    | E.PRIM(E.FORCE_RESET_REGIONSprim{instance = _}, _ ) => crash_resetting true

    | E.FRAME{declared_lvars, declared_excons} =>
        let
          val new_declared_lvars' = List.foldr( fn (lvar, acc) =>
                 let val (compound,create_region_record,regvars,sigma,p,_,_) =
                       noSome (RSE.lookupLvar rse lvar) "declared lvar of frame not in scope"
                 in {lvar=lvar, compound=compound, create_region_record=create_region_record,
                     regvars=regvars,sigma=ref sigma, place=p} :: acc
                 end) [](map #lvar declared_lvars)
          val new_declared_lvars =
            map (fn {lvar,regvars,sigma,place,...} => {lvar=lvar,regvars=regvars,sigma=sigma,place=place}) new_declared_lvars'
          val new_declared_excons = List.foldr( fn (excon, acc) =>
                 (excon,RSE.lookupExcon rse excon)::acc) [](map #1 declared_excons)
        in
          (B,E'.TR(E'.FRAME{declared_lvars = new_declared_lvars, declared_excons = new_declared_excons},
                   E'.Frame{declared_lvars = new_declared_lvars', declared_excons = new_declared_excons},
                   Eff.empty),
           NOTAIL,
           [])
        end
    | _ => die "S: unknown expression"
    ) handle
        Crash.CRASH => die_from_S e
      | Bind => die_from_S e
      | Match => die_from_S e

  (*
   and S_built_in(B,lvar, es) =
       S(B, E.APP(E.VAR{lvar = lvar, instances = []},
                  E.PRIM(E.UB_RECORDprim, es)),false)
*)
(*
   and S_binop_inline(B,bop,e1,e2,tau_res) : cone * (place,unit)E'.trip =
     let
       val B = Eff.push B (* for retract *)
       val (B, t1 as (E'.TR(e1, E'.Mus mus1,phi1))) = S(B, e1, false)
       val (B, t2 as (E'.TR(e2, E'.Mus mus2,phi2))) = S(B, e2, false)
     in
       case (mus1,mus2) of
         ([(tau1,rho1)], [(tau2,rho2)]) =>
          let
            val (rho3, B) = Eff.freshRhoWithTy(R.runtype tau_res,B)
            val phi = Eff.mkUnion([Eff.mkGet rho1, Eff.mkGet rho2, Eff.mkPut rho3,phi1,phi2])
          in
            retract(B, E'.TR(bop(t1,t2,rho3), E'.Mus [(tau_res,rho3)], phi))
          end
       | _ => die "S_binop_inline: ill-typed binary operator"
     end
*)

  in
    S(B,e,toplevel,cont)
  end (* spreadExp *)

  and spreadFcns (B,rho,retract_level,rse) functions (* each one: (lvar,tyvars,sigma_hat,bind) *) =
         let
            val occs = map (fn _ => ref []  : (R.il * (R.il * cone -> R.il * cone)) ref list ref) functions
            val rse1 = declareMany (rho,rse) (functions, occs)
            val proper_rec : bool =
                case functions of
                    [f] => proper_recursive f
                  | _ => true (* mutually declared functions are proper recursive *)
            fun spreadRhss B [] = (B,[],[])
              | spreadRhss B ((lvar,regvars,tyvars,sigma_hat,bind)::rest) =
                  let
                     (*val _ = TextIO.output(TextIO.stdOut, "spreading: " ^ Lvars.pr_lvar lvar ^ "\n")*)
                      val B = Eff.push B
                      (*val () = print ("spreadFcns - length(regvars) = " ^ Int.toString (length regvars) ^ "\n") *)
                      val (B,rse1') = List.foldl (fn (rv,(B,rse)) =>
                                                     let val (rho,B) = Eff.freshRhoRegVar (B,rv)
                                                     in (B,RSE.declareRegVar(rv,rho,rse))
                                                     end) (B,rse1) regvars
                      val (B, t1 as E'.TR(_, meta1, phi1),_,tvs') = spreadExp(B, rse1', bind,false,NOTAIL)
                      val (tau1,rho1) =
                          case unMus "spreadFcns" meta1 of
                            [p] => noSome (R.unBOX p) "spreadRhss: expecting boxed function type"
                          | _ => die "spreadFcns: expecting singleton mus"
                      val B = Eff.unifyRho (rho1,rho) B
                      val _ = count_RegEffClos:= !count_RegEffClos + 1

                      val ((tvs'',tvs1,B),sigma1) =
                          if proper_rec then
                            let val (B,sigma1) = R.regEffClos(B, retract_level, phi1, tau1)
                                val (_,B) = Eff.pop B (* back to retract level *)
                            in (findSpurious B lvar tyvars tvs', sigma1)
                            end
                          else let val (tvs'',tvs1,B) = findSpurious B lvar tyvars tvs' (* at most one function *)
                                   val epss_tv = List.foldr (fn ((_,SOME e),a) => e::a | (_,a) => a) nil tvs1
                                   val (B,sigma1) =
                                       R.regEffClos0(fn () => Lvars.pr_lvar lvar,
                                                     B, retract_level, phi1, tau1, epss_tv)
                                   val (_,B) = Eff.pop B  (* back to retract level *)
                               in ((tvs'',tvs1,B),sigma1)
                               end
(*                    val _  = log_sigma(R.insert_alphas(tyvars, sigma1), lvar)*)
                    val (B, l, tvs) = spreadRhss B rest
                  in
                    (B, (t1,tau1,sigma1,tvs1)::l, spuriousJoin tvs'' tvs)
                  end
            val (B, t1_tau1_sigma1_tvs1_list, tvs) = spreadRhss B functions
            fun look tv nil = NONE
              | look tv ((tv',e)::rest) = if tv=tv' then SOME e else look tv rest
            val (B, _) =
                List.foldl (fn ((_,_,_,tvs),(B,a)) =>
                               List.foldl (fn ((tv,SOME e),(B,a)) =>
                                              (case look tv a of
                                                   SOME e' => (Eff.unifyEps (e,e') B,a)
                                                 | NONE => (B,(tv,e)::a))
                                          | _ => (B,a)) (B,a) tvs)
                           (B,nil) t1_tau1_sigma1_tvs1_list

            val (rse2, functions') =
                mkRhs (rse, rho) (functions,t1_tau1_sigma1_tvs1_list, occs)
         in
            (B, rse2, functions', tvs)
         end

  fun spreadPgm (cone, rse: rse,p: E.LambdaPgm): cone * rse * (place,unit)E'.LambdaPgm =
  let
     fun msg(s: string) = (TextIO.output(TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut)
     fun chat s = if !Flags.chat then msg(s ^ "\n") else ()
     val _ = Eff.algorithm_R:=false
     (*val _ = Eff.trace := []*)
     val E.PGM(datbinds,e) = p
     val () = SpuriousStats.reset()
     val _ = chat "Spreading datatypes ..."
     val (new_rse, new_datbinds) = SpreadDatatype.spreadDatbinds rse datbinds cone
     val _ = chat "Spreading expression ..."
     val _ = count_RegEffClos := 0
     val (cone',t',_,_) = spreadExp (cone, RSE.plus(rse,new_rse), e,true,NOTAIL)

     val () = if stats_spurious_p() then SpuriousStats.report()
              else ()

    (* for toplas submission:
     val _ = TextIO.output(!Flags.log, "\nRegEffGen (times called during S)" ^ Int.string (!count_RegEffClos) ^ "\n")
    *)
     (*val _ = Eff.traceOrderFinMap()*)
  in
     (cone',new_rse,
      E'.PGM{expression = t',
             export_datbinds =  new_datbinds,
             export_basis = Eff.topLayer cone'
            }
     )
  end handle Abort => die "spreadPgm: SpreadExpression failed"
           | Bind => die "spreadPgm: uncaught exception Bind"
           | Match => die  "spreadPgm: uncaught exception Match"
end
