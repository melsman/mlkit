
(* Storage Mode Analysis *)

structure AtInf: AT_INF =
  struct
    structure PP = PrettyPrint
    structure Eff = Effect
    structure LLV = LocallyLiveVariables
    structure BT = IntStringFinMap
    structure RegvarBT = EffVarEnv

    (* In the old storage mode analysis an environment was propagated to later
     * program units. Since we must assign storage mode attop to regions passed
     * to functions declared outside a program unit, the environment is of no
     * use. 13/10/96-Martin
     *)

  type sigma = RType.sigma
  type place = RType.place

  (* -----------------------------------------------------------------------*)
  (* Debugging flags; updated from Flags each time main function in module  *)
  (* (AtInference) is called                                                *)
  (* -----------------------------------------------------------------------*)

  val print_regions = Flags.is_on0 "print_regions"
  val print_effects = Flags.is_on0 "print_effects"

  val debug_which_at = Flags.add_bool_entry
      {long="debug_which_at", short=NONE, menu=["Debug","debug which at (storage mode analysis)"],
       item=ref false, neg=false, desc=
       "Debug storage mode analysis."}

  val disable_atbot_analysis = Flags.add_bool_entry
      {long="disable_atbot_analysis", short=NONE, menu=["Control","all storage modes attop (for POPL 96)"],
       item=ref false, neg=false, desc=
       "Disable storage mode analysis. That is, turn all\n\
        \allocation directives into attop."}

  (***********************)
  (* Storage modes       *)
  (***********************)

  open MulExp
  datatype 'a at = ATTOP of 'a | ATBOT of 'a | SAT of 'a

  (**************************)
  (* utilities              *)
  (**************************)

  fun footnote (x,y) = x
  infix footnote

  fun die s = Crash.impossible ("AtInf." ^ s)

  fun log s = TextIO.output (!Flags.log , s ^ "\n")
  fun device s = TextIO.output(!Flags.log, s)
  fun dump t = PP.outputTree(device, t, !Flags.colwidth)
  fun warn report = Flags.warn report
  fun chat (s : string) = if !Flags.chat then log s else ()

  fun show_place p = PP.flatten1(Eff.layout_effect p)
  fun show_arreffs epss = concat(map (fn eps => " " ^ show_place eps) epss)
  fun show_places rhos = show_arreffs rhos

  fun forceATBOT (ATTOP p) = (ATBOT p)
    | forceATBOT (ATBOT p) = (ATBOT p)
    | forceATBOT (SAT p)   = (ATBOT p)

  fun debug0 (rho, how_bound) =
      if debug_which_at() then
        log ("\nwhich_at: " ^ show_place rho ^ how_bound)
      else ()

  fun show_live_vars liveset : string =
    PP.flatten1(LLV.layout_liveset liveset)

  fun debug1 (rho_related, liveset) =
      if debug_which_at() then
        (log ("locally live variables: " ^ show_live_vars liveset);
         log ("<rho>     = {" ^ show_places rho_related ^ "}"))
      else ()

  fun debug2 atbot_or_sat  =
      (if debug_which_at() then
         log (case atbot_or_sat of
                  ATBOT _     => "ATBOT"
                | SAT _ => "SAT"
                | ATTOP _      => "ATTOP")
       else ();
       (NONE, atbot_or_sat))

  (* error reporting for resetRegions: *)

  local
    fun lay_pair (t1,p) =
        PP.NODE{start="(",finish = ")", indent= 1,
                childsep = PP.RIGHT",",
                children = [t1,PP.LEAF (show_place p)]}
  in
    fun lay_sigma_p (sigma,p:place option) =
      let val a = !Flags.print_types
(*
          val b = print_regions()
          val c = print_effects()
*)
      in
        Flags.print_types:= true;
(*
        Flags.print_regions:=true;
        Flags.print_effects := true;
*)
        case p of
             SOME p =>
             lay_pair(RType.mk_lay_sigma false sigma, p)
                     footnote(Flags.print_types:= a (* ;
                   Flags.print_regions:= b;
                   Flags.print_effects := c *))
           | NONE => RType.mk_lay_sigma false sigma
      end
  end

  fun lay_header (force,lvar,(tau,p:place option)) =
  if force
  then PP.NODE{start= "", finish = "", indent = 0, childsep = PP.NOSEP,
               children = [PP.LEAF "You have requested resetting the regions that appear free ",
                           PP.LEAF ("in the type scheme with place of '" ^ Lvars.pr_lvar lvar ^ "', i.e., in"),
                           lay_sigma_p(RType.type_to_scheme tau,p),
                           PP.LEAF "I have done as you requested, but I cannot guarantee that it is safe.",
                           PP.LEAF "Here are my objections (one for each region variable concerned):"]}

  else PP.NODE{start= "", finish = "", indent = 0, childsep = PP.NOSEP,
               children = [PP.LEAF "You have suggested resetting the regions that appear free ",
                           PP.LEAF ("in the type scheme with place of '" ^ Lvars.pr_lvar lvar ^ "', i.e., in"),
                           lay_sigma_p(RType.type_to_scheme tau,p)]}

  fun lay_set (rhos: place list) =
      PP.HNODE{start ="{", finish = "}", childsep = PP.RIGHT",",
               children = map Eff.layout_effect rhos}

  fun indent (t:StringTree) =
      PP.NODE{start ="     ",finish = "", indent = 5, childsep = PP.NOSEP, children = [t]}

  (*****************************)
  (* Storage Mode Environments *)
  (*****************************)

  structure SME =
  struct

    datatype rho_desc = LETREGION_BOUND | LETREC_BOUND

    abstype regvar_env = REGVAR_ENV of rho_desc RegvarBT.map
    with
      exception RegvarEnv
      val empty_regvar_env = REGVAR_ENV(RegvarBT.empty)
      fun declare_regvar_env(x, y, REGVAR_ENV m) = REGVAR_ENV(RegvarBT.add(x,y,m))
      fun retrieve_regvar_env(x, REGVAR_ENV m) = case (RegvarBT.lookup m x)
           of SOME v => v
            | NONE => raise RegvarEnv
    end

    type lvar_env_range = (sigma*place option) * place list
    abstype lvar_env =
      LVAR_ENV of  lvar_env_range BT.map
    with
      exception LvarEnv
      val empty_lvar_env = LVAR_ENV(BT.empty)
      fun declare_lvar_env (x,y,LVAR_ENV m) = LVAR_ENV(BT.add(Lvars.key x,y,m))
      fun retrieve_lvar_env (x,LVAR_ENV m) =
        case BT.lookup m x of
        SOME x => x
        | NONE => raise LvarEnv
    end

    type excon_env_range = (sigma*place) * place list
    abstype excon_env =
        EXCON_ENV of (excon * excon_env_range) list
    with
      exception ExconEnv
      val empty_excon_env = EXCON_ENV []
      fun declare_excon_env (x,y,EXCON_ENV m) = EXCON_ENV((x,y)::m)
      fun retrieve_excon_env (x,EXCON_ENV m) =
          case List.find (fn x' => Excon.eq(x, #1 x')) m of
              SOME (_,res) => res
            | NONE => raise ExconEnv
    end

    type storage_mode_env = regvar_env * lvar_env * excon_env

    val empty_sme = (empty_regvar_env,empty_lvar_env,empty_excon_env)

  end (* SME *)

  (******************)
  (* more utilities *)
  (******************)

  exception AbortExpression

  datatype conflict =
    LVAR_PROBLEM of place * lvar * SME.lvar_env_range * place (* the witness *)
  | GLOBAL_LVAR_PROBLEM of place * lvar * place list * place (* the witness *)
  | EXCON_PROBLEM of place * excon * SME.excon_env_range * place
  | GLOBAL_EXCON_PROBLEM of place * excon * place list * place (* the witness *)
  | NON_LOCAL of place
  | ALL_ATTOP of place
  | FORMAL_REGION_PARAM of place (* used with forceResetting, when SAT is changed to ATBOT *)

  val global_regions = [Eff.toplevel_region_withtype_top,
                        Eff.toplevel_region_withtype_bot    ,
                        Eff.toplevel_region_withtype_string ,
                        Eff.toplevel_region_withtype_pair   ,
                        Eff.toplevel_region_withtype_array  ,
                        Eff.toplevel_region_withtype_ref    ,
                        Eff.toplevel_region_withtype_triple ,
                        Eff.toplevel_arreff]


  fun lines (l: string list) =
      PP.NODE{start="",finish="", indent=0,childsep=PP.NOSEP, children = map PP.LEAF l}

  fun item (item_number:int) (t:StringTree) =
      let val s = "(" ^ Int.toString item_number ^ ")"
      in PP.NODE{start = s, finish = "",
                 indent = Int.max (size s+1, 5),
                 childsep = PP.NOSEP, children = [t]}
      end

  fun layout_message (rho,kind:string,var:string,sigma_p:sigma*place option,reachable,witness,item_number:int,force:bool) =
      item item_number
      (PP.NODE{start="",finish ="", indent=0,childsep= PP.NOSEP,children=[
                 PP.LEAF "                                                  ",  (* to provoke linebreak *)
                 if force then
                   PP.LEAF ("I cannot reset '" ^ show_place rho ^ "', because of conflict with the locally")
                 else PP.LEAF ("'" ^ show_place rho ^ "': there is a conflict with the locally"),
                 PP.LEAF ("live " ^ kind),
                 PP.NODE{start = var ^ " :", finish = "", indent = 5, childsep= PP.NOSEP,
                         children = [lay_sigma_p sigma_p]},
                 PP.LEAF ("from which the following region variables can be reached "),
                 PP.LEAF ("in the region flow graph:" ),
                 indent (lay_set reachable),
                 PP.LEAF ("Amongst these, '" ^ show_place witness ^ "' can also be reached from '" ^ show_place rho^ "'."),
                 if force then
                   PP.LEAF ("This suggests that you may be destroying data in '" ^ show_place witness ^ "'.")
                 else PP.LEAF ("Thus I have given '" ^ show_place rho ^ "' storage mode \"attop\".")]})

  fun layout_global_message (rho,kind:string,var:string,reachable,witness,item_number:int,force) =
      item item_number
      (PP.NODE{start="",finish ="", indent=0,childsep= PP.NOSEP,children=[
            PP.LEAF "                                                  ",  (* to provoke linebreak *)
            if force then
               PP.LEAF ("'" ^ show_place rho ^ "': there is a  conflict with the locally")
            else
               PP.LEAF ("I cannot reset '" ^ show_place rho ^ "', because of conflict with the locally"),
            PP.LEAF ("live, but imported " ^ kind ^ var ^ "."),
            PP.LEAF ("The global regions are: "),
            indent (lay_set reachable),
            PP.LEAF ("Amongst these, '" ^ show_place witness ^ "' can also be reached from '" ^ show_place rho^ "'."),
            if force then
               PP.LEAF ("This suggests that you may be destroying data in '" ^ show_place witness ^ "'.")
            else
               PP.LEAF ("Thus I have given '" ^ show_place rho ^ "' storage mode \"attop\".")]})

  fun layout_conflict (item_number:int, force: bool, c: conflict) =
      case c of
          LVAR_PROBLEM(rho,lvar,(sigma_p,reachable),witness) =>
          layout_message(rho,"variable", Lvars.pr_lvar lvar,sigma_p,reachable,witness,item_number,force)
        | GLOBAL_LVAR_PROBLEM(rho,lvar,reachable,witness) =>
          layout_global_message(rho,"variable", Lvars.pr_lvar lvar,reachable,witness,item_number,force)
        | EXCON_PROBLEM(rho,excon,((sigma,p),reachable), witness) =>
          layout_message(rho,"exception constructor", Excon.pr_excon excon,(sigma,SOME p),reachable,witness,item_number,force)
        | GLOBAL_EXCON_PROBLEM(rho,excon,reachable,witness) =>
          layout_global_message(rho,"exception constructor", Excon.pr_excon excon,reachable,witness,item_number,force)
        | NON_LOCAL rho => item item_number
                                (if force then
                                   PP.LEAF ("'" ^ show_place rho ^ "': this region variable is bound\
                                    \ outside the present function")
                                 else
                                   lines[("I cannot reset '" ^ show_place rho ^ "', for it "),
                                         ("is bound outside the present function.")]
                                )
        | ALL_ATTOP rho => item item_number
                                (if force then
                                   lines[("'" ^ show_place rho ^ "': the flag \"Storage Mode Analysis/all modes attop\""),
                                         (" is enabled.")]
                                 else
                                   lines[("I cannot reset '" ^ show_place rho ^ "', for the flag "),
                                         ("\"Storage Mode Analysis/all modes attop\" is enabled.")]
                                )
        | FORMAL_REGION_PARAM rho =>
          (* here force is true *)
          item item_number
               (lines[("'" ^ show_place rho ^ "' is a formal parameter of a region-polymorphic "),
                      ("function (which is or may in future program units be "),
                      ("applied to regions containing live values).")])

  fun lay_conflicts (force:bool,l : conflict list) =
      let fun loop(item_number:int, l: conflict list) =
              case l of [] => []
                      | (c::rest) => (layout_conflict(item_number, force, c)::
                                      loop(item_number+1,rest))
      in loop(1,l)
      end

  fun lay_report (force:bool, lvar, mu, conflicts) : StringTree =
      let val tau_p =
              case RType.unBOX mu of
                  SOME (tau,p) => (tau,SOME p)
                | NONE => (mu,NONE)
      in PP.NODE{start = if force then ("forceResetting(" ^ Lvars.pr_lvar lvar ^ "): ")
                         else ("resetRegions(" ^ Lvars.pr_lvar lvar ^ "): "),
                 finish = "", indent = 3, childsep = PP.NOSEP,
                 children = lay_header(force,lvar,tau_p) :: lay_conflicts(force,conflicts)}
      end

  fun any_live (rho,sme as (_,LE,EE), liveset,
                rho_points_into, atbot_or_sat): conflict option * place at=
      let
        (* val _ = Profile.profileOn();*)
        fun conflicting_local_lvar lvar : conflict option =
            let val lvar_res as (_,lrv) = SME.retrieve_lvar_env(Lvars.key lvar, LE)
            in case rho_points_into lrv of
                   SOME (witness: place) => SOME(LVAR_PROBLEM(rho,lvar,lvar_res,witness))
                 | NONE => NONE
            end handle SME.LvarEnv =>
                     (* lvar from previous program module. The follwing code assumes that
                        the only region variables that can occur free in the type of an
                        lvar from a previous module are global regions declared in Effect!!! *)
                       (case rho_points_into global_regions of
                            SOME (witness: place) => SOME(GLOBAL_LVAR_PROBLEM(rho,lvar,global_regions,witness))
                          | NONE => NONE)

        fun conflicting_local_excon (excon: Excon.excon): conflict option =
            let val excon_res as (_,lrv)  = SME.retrieve_excon_env(excon, EE)
            in case rho_points_into(lrv) of
                   SOME (witness: place) => SOME(EXCON_PROBLEM(rho,excon,excon_res,witness))
                 | _ => NONE
            end handle SME.ExconEnv =>
                     (* excon from previous program module. The following code assumes that
                        the only region variables that can occur free in the type of an
                        lvar or excon from a previous module are global regions declared
                        in Effect!!! *)
                       (case rho_points_into global_regions of
                            SOME (witness: place) => SOME(GLOBAL_EXCON_PROBLEM(rho,excon,global_regions,witness))
                          | NONE => NONE)
      in
        case LLV.findLvar conflicting_local_lvar liveset of
            SOME(lvar,conflict) => (SOME conflict, ATTOP rho)
          | _ => case LLV.findExcon conflicting_local_excon liveset of
                     SOME(excon, conflict) => (SOME conflict,ATTOP rho)
                   | _ => (NONE, atbot_or_sat)
      end handle _ => die "any_live failed"

  fun equal_places rho1 rho2 = Eff.eq_effect(rho1,rho2)

  fun letregion_bound (rho,sme,liveset): conflict option * place at=
      let fun rho_points_into rhos= List.find (equal_places rho) rhos
      in debug1([],liveset);
         any_live(rho,sme,liveset,rho_points_into,ATBOT rho)
      end

  fun is_visited rho = !(Eff.get_visited rho)
  fun visit rho = Eff.get_visited rho := true;
  fun unvisit rho = Eff.get_visited rho := false;

  fun letrec_bound (rho, sme, liveset): conflict option * place at=
      let (*val _ = Profile.profileOn();*)
          val rho_related = RegFlow.reachable_in_graph_with_insertion (rho)
          (*val _ = Profile.profileOff();*)
          fun rho_points_into lrv = List.find is_visited lrv
      in debug1(rho_related,liveset);
         List.app visit rho_related;
         any_live(rho,sme,liveset,rho_points_into, SAT rho)
                 footnote List.app unvisit rho_related
      end

  fun show_place_at (ATTOP p) = "attop " ^ show_place p
    | show_place_at (ATBOT p) = "atbot " ^ show_place p
    | show_place_at (SAT p) = "sat " ^ show_place p

  fun which_at0 explain (sme as (RE,LE,EE),rho,liveset)
      : conflict option * place at =
      (* Invariant: all rhos have their visited field false *)
      (if disable_atbot_analysis() then
         (SOME(ALL_ATTOP rho), ATTOP rho)
       else
         (case SME.retrieve_regvar_env(rho,RE) of
              SME.LETREGION_BOUND =>   (* SMA rules 25 and 26 *)
              (debug0 (rho,"(letregion-bound)");
               letregion_bound(rho,sme,liveset))
            | SME.LETREC_BOUND  =>  (* SMA rules 27 and 28 *)
              (debug0(rho, "(letrec-bound)");
               letrec_bound(rho,sme,liveset))
         )
         handle SME.RegvarEnv =>              (* SMA rule 29 *)
                (debug0(rho, "(non-locally bound)");(SOME (NON_LOCAL rho), ATTOP rho))
      )

  fun which_at env (rho,liveset) :  place at =
      #2(which_at0 false (env,rho,liveset))

  fun which_at_with_explanation (env,rho,liveset) : conflict option * place at =
      which_at0 true (env,rho,liveset)

  fun analyse_rhos_for_resetting (sme, liveset, rhos) : place at list * conflict list=
    let fun loop ([]:place list, acc1: place at list, acc2: conflict list) = (acc1,acc2)
          | loop (rho::rest, acc1,acc2)=
            case which_at_with_explanation(sme,rho,liveset) of
                (NONE, place_at) => loop(rest,place_at::acc1, acc2)
              | (SOME problem, place_at) => loop(rest,place_at::acc1, problem::acc2)
    in loop(rhos,[],[])
    end

  (* rvars(sigma, p0) = ((sigma, p0), lrv) where lrv is the set of all
     region and effect variables reachable from p or a free region or
     effect variables of sigma. *)

  fun rvars (sigma, p0:place option) : SME.lvar_env_range =
      let val free_vars = RType.ferv_sigma sigma
          val free_vars' = case p0 of SOME p => p :: free_vars
                                    | NONE => free_vars
          val lrv = RegFlow.reachable_with_insertion free_vars'
                    handle Find => Crash.impossible "AtInference: rvars "
      in ((sigma, p0),lrv)
      end

  fun mu_to_scheme_and_place (tau:RType.Type, rho_opt : place option) : sigma * place option =
      (RType.type_to_scheme tau, rho_opt)

  (********************************)
  (*  sma0 traverses the program  *)
  (*  and inserts storage modes   *)
  (********************************)

  fun sma0 (pgm0 as PGM{expression=trip,
                 export_datbinds,
                 import_vars,
                 export_vars,
                 export_basis,
                 export_Psi}: (place * LLV.liveset, place*mul, qmularefset ref)LambdaPgm)
      : (place at, place*mul, unit)LambdaPgm =
      let fun sma_trip sme (TR(e, metaType, ateffects, mulef_r)) =
            let fun sma_sw sme (SWITCH(tr,choices,opt)) =
                  let val tr' = sma_trip sme tr
                      val choices' = map (fn (a,tr) => (a,sma_trip sme tr)) choices
                      val opt' = case opt of SOME tr => SOME (sma_trip sme tr) | NONE => NONE
                  in SWITCH(tr',choices',opt')
                  end
                val e' =
                 (case e
                    of VAR{lvar,il,plain_arreffs,fix_bound,rhos_actuals=ref actuals,other} =>
                      let val actuals' = map (which_at sme) actuals  (* also liveset here*)
                      in VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,
                             fix_bound=fix_bound,rhos_actuals=ref actuals',other=()}
                      end
                     | INTEGER(n, t, alloc) => INTEGER(n, t, Option.map (which_at sme) alloc)
                     | WORD(w, t, alloc) => WORD(w, t, Option.map (which_at sme) alloc)
                     | STRING(s,alloc) => STRING(s, which_at sme alloc)
                     | REAL(s,alloc) => REAL(s, which_at sme alloc)
                     | F64 s => F64 s
                     | UB_RECORD trips => UB_RECORD(map (sma_trip sme) trips)
                     | FN{pat,body,free,alloc} => sma_fn(sme,SME.empty_regvar_env,pat,body,free,alloc)
                     | LETREGION{B,rhos,body} =>
                         let val (RE,LE,EE) = sme
                             fun extend ((rho,mul), RE') = SME.declare_regvar_env(rho,SME.LETREGION_BOUND,RE')
                             val sme_body = (foldl extend RE (!rhos), LE, EE)
                         in LETREGION{B=B,rhos=rhos,body=sma_trip sme_body body}
                         end
                     | LET{k_let,pat,bind,scope} =>
                         let val (RE,LE,EE) = sme
                             fun do_pat (lv,ils,tvs,effs,tau,p,other) = (lv,ils,tvs,effs,tau,p,())
                             fun extend ((lvar,_,alphas,ref [], ty, rho, other), LE) =
                                           SME.declare_lvar_env(lvar,rvars(mu_to_scheme_and_place(ty,rho)), LE)
                               | extend _ = die "non-empty list of bound region or effect variables at LET"
                             val sme_scope = (RE, foldl extend LE pat,EE)
                         in LET{k_let=k_let,pat=map do_pat pat,
                                bind=sma_trip sme bind,
                                scope=sma_trip sme_scope scope}
                         end
                     | FIX{free,shared_clos = shared_clos as (shared_rho,liveset),functions,scope} =>
                         let
                            val (RE,LE,EE) = sme
                            val LE' = foldl  (fn ({lvar,tyvars, rhos_formals, epss, Type, ...}, acc) =>
                                              let val rhos = map (fn (a,_) => a) (!rhos_formals)
                                              in SME.declare_lvar_env(lvar,rvars(RType.FORALL(rhos,
                                                        epss,tyvars,Type),SOME shared_rho),acc)
                                              end)
                                      LE functions
                            val sme' = (RE,LE',EE)
                            fun do_function {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                                             bound_but_never_written_into,
                                             other,bind} =
                               (case bind of
                                  TR(FN{pat,body,free,alloc}, mu_lam, phi_lam, psi_lam) =>
                                    let
                                       fun extend (rho, RE') = SME.declare_regvar_env(rho,SME.LETREC_BOUND,RE')
                                       val rhos' = map (fn (a,_) => a) (!rhos_formals)
                                       val RE_for_body_of_fn = foldl extend SME.empty_regvar_env rhos'
                                       val fn' = sma_fn(sme',RE_for_body_of_fn,pat,body,free,alloc)
                                    in
                                      {lvar=lvar,occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,Type=Type,
                                       rhos_formals=rhos_formals,
                                       bound_but_never_written_into=bound_but_never_written_into,
                                       other=(),
                                       bind=TR(fn', mu_lam,phi_lam,psi_lam)}
                                    end
                                | _ => die "right-hand side of fun must be a lambda-abstraction"
                               )

                         in FIX{free=free,
                                shared_clos=which_at sme shared_clos,
                                functions=map do_function functions,
                                scope=sma_trip sme' scope}
                         end
                     | APP(ck,sr,tr1,tr2) => APP(ck,sr,sma_trip sme tr1, sma_trip sme tr2)
                     | EXCEPTION(excon,b,mu,alloc as (rho,liveset),scope) =>
                       let val (RE,LE,EE) = sme
                           val mu' = case RType.unBOX mu of SOME(tau,p) => (tau,SOME p)
                                                          | NONE => (mu, NONE)
                           val rng = case rvars(mu_to_scheme_and_place mu') of
                                         ((sigma, SOME p0),lrv) => ((sigma,p0),lrv)
                                       | _ => die "EXCEPTION.expecting boxed type"
                           val sme_body = (RE,LE,SME.declare_excon_env(excon,rng,EE))
                       in EXCEPTION(excon,b,mu, ATTOP rho,
                                    sma_trip sme_body scope)
                       end
                     | RAISE tr => RAISE (sma_trip sme tr)
                     | HANDLE(tr1,tr2) => HANDLE(sma_trip sme tr1, sma_trip sme tr2)
                     | SWITCH_I {switch,precision} => SWITCH_I {switch=sma_sw sme switch, precision=precision}
                     | SWITCH_W {switch,precision} => SWITCH_W {switch=sma_sw sme switch, precision=precision}
                     | SWITCH_S sw => SWITCH_S (sma_sw sme sw)
                     | SWITCH_C sw => SWITCH_C (sma_sw sme sw)
                     | SWITCH_E sw => SWITCH_E (sma_sw sme sw)
                     | CON0 {con, il, aux_regions, alloc} =>
                       CON0 {con=con, il=il,
                             aux_regions=map (which_at sme) aux_regions,
                             alloc=Option.map (which_at sme) alloc}
                     | CON1 ({con, il, alloc}, tr) =>
                       CON1 ({con=con,il=il,alloc=Option.map (which_at sme) alloc},
                             sma_trip sme tr)
                     | DECON ({con, il}, tr) => DECON({con=con,il=il},sma_trip sme tr)
                     | EXCON (excon, opt) => EXCON(excon, case opt
                                                            of SOME (alloc as (p, liveset),tr) =>
                                                                 SOME (ATTOP p, sma_trip sme tr)
                                                             | NONE => NONE)
                     | DEEXCON (excon,tr) => DEEXCON(excon, sma_trip sme tr)
                     | RECORD (alloc, trs) => RECORD(Option.map (which_at sme) alloc,map (sma_trip sme) trs)
                     | SELECT (i, tr) => SELECT(i,sma_trip sme tr)
                     | DEREF tr => DEREF (sma_trip sme tr)
                     | REF (alloc,tr) => REF(which_at sme alloc, sma_trip sme tr)
                     | ASSIGN (tr1,tr2) => ASSIGN (sma_trip sme tr1, sma_trip sme tr2) (* no need for analysis *)
                     | DROP tr => DROP (sma_trip sme tr)
                     | EQUAL ({mu_of_arg1, mu_of_arg2}, tr1,tr2) =>
                       EQUAL ({mu_of_arg1=mu_of_arg1, mu_of_arg2=mu_of_arg2},  (* no need for analysis *)
                              sma_trip sme tr1,sma_trip sme tr2)
                     | CCALL ({name, mu_result, rhos_for_result}, trs) =>
                         CCALL ({name = name, mu_result = mu_result,
                                 rhos_for_result =
                                     map (fn ((rho, liveset), i_opt) =>
                                          (which_at sme (rho, liveset), i_opt))
                                     rhos_for_result},
                                map (sma_trip sme) trs)
                     | BLOCKF64 (alloc, trs) => BLOCKF64(which_at sme alloc,map (sma_trip sme) trs)
                     | SCRATCHMEM (n,alloc) => SCRATCHMEM(n,which_at sme alloc)
                     | EXPORT(i,tr) => EXPORT(i,sma_trip sme tr)
                     | RESET_REGIONS ({force, liveset=SOME liveset, ...}, tr as (TR(VAR{lvar,...},meta,_,_))) =>
                          (case meta of
                             MulExp.RegionExp.Mus [mu] =>
                                   let val free_regions = Eff.remove_duplicates(RType.frv_mu mu)
                                       val (place_at_list, conflicts) =
                                           analyse_rhos_for_resetting(sme,liveset,free_regions)
                                       val conflicts' =
                                           if force then
                                             foldl (fn (SAT rho, acc) => FORMAL_REGION_PARAM rho :: acc
                                                   | (_, acc) => acc) conflicts place_at_list
                                           else conflicts
                                   in
                                     case conflicts' of
                                         [] => ()
                                       | _ => warn (PP.reportStringTree(lay_report(force,lvar,mu,conflicts')));
                                     RESET_REGIONS({force=force,regions_for_resetting = place_at_list, liveset=NONE},
                                                   sma_trip sme tr)
                                   end
                            | _ => die "RESET_REGIONS: expected a type and place on argument to resetRegions"
                          )
                     | RESET_REGIONS _ => die "ill-formed expression: argument to RESET_REGIONS should be a variable"
                     | FRAME{declared_lvars, declared_excons} =>
                      let fun f {lvar,sigma,other,place} = {lvar=lvar,sigma=sigma,other=(),place=place}
                      in FRAME{declared_lvars=map f declared_lvars, declared_excons = declared_excons}
                      end
                   ) handle _ =>
                           (log "\nStorage Mode Analysis failed at expression:";
                            dump(MulExp.layoutLambdaExp(fn _ => NONE)(fn _ => NONE)(fn _ => NONE)(fn _ => NONE)
                             e);
                            raise AbortExpression)

            in TR(e', metaType, ateffects, mulef_r)
            end
          and sma_fn (sme,regvar_env0,pat,body,free,alloc) =
              let val (_, LE, EE) = sme
                  fun extend ((lvar, mu), LE) =
                      let val mu = case RType.unBOX mu of SOME (tau,rho) => (tau,SOME rho)
                                                        | NONE => (mu, NONE)
                      in SME.declare_lvar_env(lvar,rvars(mu_to_scheme_and_place mu), LE)
                      end
                  val sme_body = (regvar_env0,
                                  foldl extend LE pat,
                                  EE)
              in
                FN{pat=pat,body=sma_trip sme_body body,
                   free=free,alloc=which_at sme alloc}
              end
      in
         PGM{expression=sma_trip SME.empty_sme trip,
             export_datbinds=export_datbinds,
             import_vars=import_vars,
             export_vars=export_vars,
             export_basis=export_basis,
             export_Psi=export_Psi}
      end

    (********************************************)
    (*  main function: sma                      *)
    (*  --------------------------------------  *)
    (* Storage mode analysis is done in three   *)
    (* passes over the syntax tree.             *)
    (*                                          *)
    (* (1) build region flow graph,             *)
    (* (2) compute locally live variables, and  *)
    (* (3) compute storage modes.               *)
    (*                                          *)
    (* See POPL 96 paper for an explanation of  *)
    (* the general principles.                  *)
    (********************************************)

  fun sma (pgm: (place, place*mul, qmularefset ref)LambdaPgm)
      : (place at, place*mul, unit)LambdaPgm =
      (chat "Building region flow graph ...";
       Timing.timing_begin();
       RegFlow.mk_graph(pgm) handle _ => die "call of RegFlow.mk_graph failed";
       Timing.timing_end("RegFlow");
       Timing.timing_begin();
       chat "Computing locally live variables ...";
       let val pgm' = LLV.llv pgm
                      handle _ => die "call of LLV.llv failed"
       in
         Timing.timing_end("LocLive");
         chat "Storage mode analysis ...";
         Timing.timing_begin();
         (sma0(pgm')handle AbortExpression => die "call of sma0 failed")
             footnote Timing.timing_end("SMA")
       end
      )

    (***********************************)
    (* Pretty printing                 *)
    (***********************************)

    type StringTree = PP.StringTree
    fun lay (s : string) (p: 'a -> StringTree) (a : 'a) : StringTree option =
        SOME(PP.HNODE{start=s^" ",finish="",children=[p a],childsep=PP.NOSEP})

    fun layout_at (p: 'a -> StringTree) (at : 'a at) =
        case at of
            ATTOP a => lay "attop" p a
          | ATBOT a => lay "atbot" p a
          | SAT a => lay "sat" p a

    fun layout_placeXmul (place,mul) =
        PP.HNODE{start="",finish="",childsep=PP.RIGHT ":",
                 children=[Eff.layout_effect place, Mul.layout_mul mul]}
    fun layout_unit () = NONE
    val layout_trip : (place at, place*mul, unit)trip -> StringTree =
      MulExp.layoutLambdaTrip (layout_at Eff.layout_effect)(layout_at Eff.layout_effect) (SOME o layout_placeXmul) layout_unit

    (* brief printing of expressions: *)
    fun layout_at' (p: 'a -> StringTree) (at : 'a at) =
      case at
        of ATTOP a => lay "at" p a
         | ATBOT a => lay "at" p a
         | SAT a => lay "at" p a

    fun layout_at'' (p: 'a -> StringTree) (at : 'a at) =
      case at
        of ATTOP a => SOME(p a)
         | ATBOT a => SOME(p a)
         | SAT a => SOME(p a)

    fun ignore _ = NONE

    fun layout_trip_brief (tr : (place at, place*mul, unit)trip): StringTree =
      if print_regions() then
         MulExp.layoutLambdaTrip
             (layout_at' Eff.layout_effect)(layout_at'' Eff.layout_effect) (SOME o layout_placeXmul) layout_unit tr
      else
         MulExp.layoutLambdaTrip ignore ignore ignore layout_unit tr

    fun layout_exp_brief(e : (place at, place*mul, unit)LambdaExp): StringTree =
      if print_regions() then
          MulExp.layoutLambdaExp (layout_at' Eff.layout_effect)(layout_at'' Eff.layout_effect) (SOME o layout_placeXmul) layout_unit e
      else
          MulExp.layoutLambdaExp ignore ignore ignore layout_unit e


    fun layout_pgm (PGM{expression,...}) = layout_trip expression
    fun layout_pgm_brief (PGM{expression,...}) = layout_trip_brief expression

end
