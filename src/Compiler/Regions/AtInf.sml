
(* Storage Mode Analysis *)

functor AtInf(structure Lvars: LVARS
              structure Excon: EXCON
              structure MulExp: MUL_EXP
	      structure Mul: MUL
	      structure Eff: EFFECT
              structure RType: RTYPE
              structure LLV: LOCALLY_LIVE_VARIABLES
              structure RegFlow: REG_FLOW
              structure BT: ORDER_FINMAP (* finite maps with domain = keys of lvars *)
                            where type dom = int  
              structure RegvarBT: ORDER_FINMAP 
	      structure PP: PRETTYPRINT
		sharing type PP.StringTree = Mul.StringTree 
		           = MulExp.StringTree = Eff.StringTree = LLV.StringTree
                           = RType.StringTree
	      structure Flags: FLAGS
	      structure Crash: CRASH
	      structure Report: REPORT
	      sharing type Report.Report = Flags.Report = PP.Report
              structure Timing: TIMING
      	        sharing type Eff.place = MulExp.place = MulExp.effect 
                        = LLV.place = RegFlow.place = RegFlow.effect  
                        = RegvarBT.dom = RType.place = RType.effect 
                        = Eff.place = Eff.effect = MulExp.RegionExp.place
	        sharing type Mul.mul = MulExp.mul = LLV.mul = RegFlow.mul
                sharing type LLV.qmularefset = Mul.qmularefset 
                        = RegFlow.qmularefset = MulExp.qmularefset
                sharing type LLV.LambdaPgm = MulExp.LambdaPgm = RegFlow.LambdaPgm
                sharing type LLV.lvar = Lvars.lvar = MulExp.lvar
                sharing type Excon.excon = MulExp.excon = LLV.excon
                sharing type RType.Type = MulExp.Type = MulExp.RegionExp.Type
                sharing type RType.tyvar = MulExp.tyvar
                sharing type MulExp.metaType = MulExp.RegionExp.metaType
                sharing type RType.sigma = MulExp.sigma
) : AT_INF =
  struct

    structure NewList = List
    structure List = Edlib.List
    structure Int = Edlib.Int
    
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

  val print_K_normal_forms = ref false
  val debug_which_at = ref false
  val all_attop      = ref false

  (***********************)
  (* Storage modes       *)
  (***********************)

  open MulExp
  datatype 'a at = ATTOP of 'a | ATBOT of 'a | SAT of 'a | IGNORE

  (**************************)
  (* utilities              *)
  (**************************)

  fun footnote(x,y) = x
  infix footnote

  fun die s = Crash.impossible ("AtInf." ^ s)

  fun log s = TextIO.output (!Flags.log , s ^ "\n")
  fun device s = TextIO.output(!Flags.log, s)            
  fun dump t = PP.outputTree(device, t, !Flags.colwidth)
  fun warn report       = Flags.warn report
  fun chat (s : string) = if !Flags.chat then log s else ()

  fun show_place p = PP.flatten1(Eff.layout_effect p)
  fun show_arreffs epss = concat(map (fn eps => " " ^ show_place eps) epss)
  fun show_places  rhos = show_arreffs rhos

  fun forceATBOT (ATTOP p) = (ATBOT p)
    | forceATBOT (ATBOT p) = (ATBOT p)
    | forceATBOT (SAT p)   = (ATBOT p)
    | forceATBOT IGNORE    = die "forceATBOT: I exptected storage\
                       \ mode analysis to happen before dropping of regions."

  fun debug0(rho, how_bound) =
	      if !debug_which_at then 
		  log ("\nwhich_at: " ^ show_place rho ^ how_bound)
	      else ()

  fun show_live_vars liveset : string = 
    PP.flatten1(LLV.layout_liveset liveset)

  fun debug1(rho_related, liveset) =
     if !debug_which_at then 
	  (log ("locally live variables: " ^ show_live_vars liveset);
	   log ("<rho>     = {" ^ show_places rho_related ^ "}"))
     else ()

  fun debug2 atbot_or_sat  =
	      (if !debug_which_at then 
		   log (case atbot_or_sat of
			    ATBOT _     => "ATBOT"
			  | SAT _ => "SAT" 
			  | ATTOP _      => "ATTOP"
			  | IGNORE      => "IGNORE")
	       else ();
	       (NONE, atbot_or_sat))


  (* error reporting for resetRegions: *)

  local
    fun lay_pair(t1,p)=
      let fun lay (t1,p) = PP.NODE{start="(",finish = ")", indent= 1, 
				   childsep = PP.RIGHT",", children = 
				   [t1,PP.LEAF (show_place p)]}
      in if !Flags.print_word_regions then lay (t1,p)
	 else case Eff.get_place_ty p
		of SOME Eff.WORD_RT => t1
		 | _ => lay (t1,p)
      end
  in
    fun lay_sigma_p(sigma,p) =
      let val a = !Flags.print_types
          val b = !Flags.print_regions
	  val c = !Flags.print_effects     
      in 
        Flags.print_types:= true;
        Flags.print_regions:=true;
        Flags.print_effects := true;
        lay_pair(RType.mk_lay_sigma false sigma, p)
          footnote(Flags.print_types:= a;
                   Flags.print_regions:= b;
                   Flags.print_effects := c)
      end
  end  

  fun lay_header(force,lvar,(tau,p)) = 
  if force
     then
        PP.NODE{start= "", finish = "", indent = 0, childsep = PP.NOSEP,
          children = [PP.LEAF "You have requested resetting the regions that appear free ",
                      PP.LEAF ("in the region type scheme of '" ^ Lvars.pr_lvar lvar ^ "', i.e., in"),
                      lay_sigma_p(RType.type_to_scheme tau,p),
                      PP.LEAF "I have done as you requested, but I cannot guarantee that it is safe.",
                      PP.LEAF "Here are my objections (one for each region variable concerned):"]}                 

  else 
        PP.NODE{start= "", finish = "", indent = 0, childsep = PP.NOSEP,
          children = [PP.LEAF "You have suggested resetting the regions that appear free ",
                      PP.LEAF ("in the region type scheme of '" ^ Lvars.pr_lvar lvar ^ "', i.e., in"),
                      lay_sigma_p(RType.type_to_scheme tau,p)]}                 


  fun lay_set (rhos: place list) = 
    let val rhos = if !Flags.print_word_regions then rhos
		   else NewList.filter (fn rho => case Eff.get_place_ty rho
						    of SOME Eff.WORD_RT => false
						     | _ => true) rhos
    in PP.HNODE{start ="{", finish = "}", childsep = PP.RIGHT",",
		children = map Eff.layout_effect rhos}
    end

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
  
    type lvar_env_range = (sigma*place) * place list
    abstype lvar_env = 
      LVAR_ENV of  lvar_env_range BT.map	
    with 
      exception LvarEnv
      val empty_lvar_env = LVAR_ENV(BT.empty)
      fun declare_lvar_env(x,y,LVAR_ENV m) = LVAR_ENV(BT.add(Lvars.key x,y,m))
      fun retrieve_lvar_env(x,LVAR_ENV m) =
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
      fun declare_excon_env(x,y,EXCON_ENV m) = EXCON_ENV((x,y)::m)
      fun retrieve_excon_env(x,EXCON_ENV m) = 
        #2 (List.first (fn x' => Excon.eq(x, #1 x')) m)
        handle List.First _ => raise ExconEnv
    end
  
  
    type storage_mode_env = regvar_env * lvar_env * excon_env
  
    val empty_sme = (empty_regvar_env,empty_lvar_env,empty_excon_env)
  
  end; (* SME *)

  (******************)
  (* more utilities *)
  (******************)

  fun count_inner_conflicting_lvar() = ()
  fun count_inner_conflicting_excon() = ()

  exception AbortExpression 

  datatype conflict =
    LVAR_PROBLEM of place * lvar *SME.lvar_env_range * place (* the witness *)
  | GLOBAL_LVAR_PROBLEM of place * lvar * place list * place (* the witness *)
  | EXCON_PROBLEM of place * excon * SME.excon_env_range * place
  | GLOBAL_EXCON_PROBLEM of place * excon * place list * place (* the witness *)
  | NON_LOCAL of place
  | ALL_ATTOP of place
  | FORMAL_REGION_PARAM of place (* used with forceResetting, when SAT is changed to ATBOT *)

  val global_regions = [Eff.toplevel_region_withtype_top,
                        Eff.toplevel_region_withtype_word,
                        Eff.toplevel_region_withtype_bot    ,
                        Eff.toplevel_region_withtype_string ,
                        Eff.toplevel_region_withtype_real   ,
                        Eff.toplevel_arreff]


  fun lines (l: string list) = PP.NODE{start="",finish="", indent=0,childsep=PP.NOSEP, children = map PP.LEAF l}

  fun item (item_number:int) (t:StringTree) = 
    let val s = "(" ^ Int.string item_number ^ ")"
    in
       PP.NODE{start = s, finish = "", 
               indent = Int.max (size s+1) 5,
               childsep = PP.NOSEP, children = [t]}
    end

  fun layout_message(rho,kind:string,var:string,sigma_p,reachable,witness,item_number:int,force:bool) = 
    item item_number
     (PP.NODE{start="",finish ="", indent=0,childsep= PP.NOSEP,children=[
            PP.LEAF "                                                  ",  (* to provoke linebreak *)
            if force then 
                 PP.LEAF ("I cannot reset '" ^ show_place rho ^ "', because of conflict with the locally")
            else PP.LEAF ("'" ^ show_place rho ^ "': there is a conflict with the locally"),
            PP.LEAF ("live " ^ kind),
            PP.NODE{start = var ^ " :", finish = "", indent = 5, childsep= PP.NOSEP, children = [
                    lay_sigma_p sigma_p]},
            PP.LEAF ("from which the following region variables can be reached "),
            PP.LEAF ("in the region flow graph:" ),
            indent (lay_set reachable),
            PP.LEAF ("Amongst these, '" ^ show_place witness ^ "' can also be reached from '" ^ show_place rho^ "'."),
            if force then
                 PP.LEAF ("This suggests that you may be destroying data in '" ^ show_place witness ^ "'.")
            else PP.LEAF ("Thus I have given '" ^ show_place rho ^ "' storage mode \"attop\".")]})

  fun layout_global_message(rho,kind:string,var:string,reachable,witness,item_number:int,force) = 
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
  | EXCON_PROBLEM(rho,excon,(sigma_p,reachable), witness) => 
       layout_message(rho,"exception constructor", Excon.pr_excon excon,sigma_p,reachable,witness,item_number,force)
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
  | FORMAL_REGION_PARAM(rho) => 
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

  fun lay_report(force:bool, lvar, mu, conflicts) : StringTree =
      PP.NODE{start = if force then ("forceResetting(" ^ Lvars.pr_lvar lvar ^ "): ")
                               else ("resetRegions(" ^ Lvars.pr_lvar lvar ^ "): "),
              finish = "", indent = 3, childsep = PP.NOSEP,children = 
                  lay_header(force,lvar,mu) :: lay_conflicts(force,conflicts)}

  fun any_live (rho,sme as (_,LE,EE), liveset, 
                rho_points_into, atbot_or_sat): conflict option * place at=
      let 
        (* val _ = Profile.profileOn();*)
        fun conflicting_local_lvar(lvar): conflict option =  
             let val lvar_res as (_,lrv) = SME.retrieve_lvar_env(Lvars.key lvar, LE)
             in
               case rho_points_into(lrv)  of
                 SOME (witness: place) => SOME(LVAR_PROBLEM(rho,lvar,lvar_res,witness))
               | NONE => NONE
             end handle SME.LvarEnv => 
                     (* lvar from previous program module. The follwing code assumes that
                        the only region variables that can occur free in the type of an
                        lvar from a previous module are global regions declared in Effect!!! *)
               (case rho_points_into(global_regions)  of
                 SOME (witness: place) => SOME(GLOBAL_LVAR_PROBLEM(rho,lvar,global_regions,witness))
               | NONE => NONE)

        fun conflicting_local_excon(excon: Excon.excon): conflict option =
             let val excon_res as (_,lrv)  = SME.retrieve_excon_env(excon, EE)
             in
               case rho_points_into(lrv) of
                 SOME (witness: place) => SOME(EXCON_PROBLEM(rho,excon,excon_res,witness))
               | _ => NONE
             end handle SME.ExconEnv => 
                     (* excon from previous program module. The following code assumes that
                        the only region variables that can occur free in the type of an
                        lvar or excon from a previous module are global regions declared 
                        in Effect!!! *)
               (case rho_points_into(global_regions)  of
                 SOME (witness: place) => SOME(GLOBAL_EXCON_PROBLEM(rho,excon,global_regions,witness))
               | NONE => NONE)
      in
          case LLV.findLvar conflicting_local_lvar liveset of
            SOME(lvar,conflict)  => (SOME conflict, ATTOP rho)
          | _ =>  (case LLV.findExcon conflicting_local_excon liveset  of
                       SOME(excon, conflict) => (SOME conflict,ATTOP rho)
                     | _ => (NONE, atbot_or_sat)
                  )
          (*footnote Profile.profileOff()*)
      end handle _ => die "any_live failed"

  fun equal_places rho1 rho2 = Eff.eq_effect(rho1,rho2)

  fun letregion_bound (rho,sme,liveset): conflict option * place at=
      let 
	  fun rho_points_into rhos= SOME(List.first (equal_places rho) rhos) 
                                    handle List.First _ => NONE
      in
	  debug1([],liveset);
	  any_live(rho,sme,liveset, rho_points_into, ATBOT rho)
      end

  fun is_visited rho = !(Eff.get_visited rho)
  fun visit rho = Eff.get_visited rho := true;
  fun unvisit rho = Eff.get_visited rho := false;

  fun letrec_bound (rho, sme, liveset): conflict option * place at= 
      let 
         (*val _ = Profile.profileOn();*)
      	  val rho_related = RegFlow.reachable_in_graph_with_insertion (rho)
         (*val _ = Profile.profileOff();*)
	  fun rho_points_into lrv = 
                 SOME(List.first is_visited lrv) handle List.First _  => NONE
      in
	  debug1(rho_related,liveset);
	  List.apply visit rho_related;
	  any_live(rho,sme,liveset,rho_points_into, SAT rho)
	    footnote List.apply unvisit rho_related
      end

  fun show_place_at (ATTOP p) = "attop " ^ show_place p
    | show_place_at (ATBOT p) = "atbot " ^ show_place p
    | show_place_at (SAT p) = "sat " ^ show_place p
    | show_place_at IGNORE  = "(ignore)"

  fun which_at0 explain (sme as (RE,LE,EE),rho,liveset) 
      : conflict option * place at = 
      (* Invariant: all rhos have their visited field false *)
     case Eff.get_place_ty rho of
       SOME Eff.WORD_RT => (NONE, ATTOP rho)
     | _ => 
      (if !all_attop then 
  	      (SOME(ALL_ATTOP rho), ATTOP rho)
       else 
        (case SME.retrieve_regvar_env(rho,RE) of
  	 SME.LETREGION_BOUND =>   (* SMA rules 25 and 26 *)
    	   (debug0 (rho,"(letregion-bound)");letregion_bound(rho,sme,liveset))
         | SME.LETREC_BOUND  =>	 (* SMA rules 27 and 28 *)
  	   (debug0(rho, "(letrec-bound)"); letrec_bound(rho,sme,liveset))
        )
        handle SME.RegvarEnv =>		     (* SMA rule 29 *)
         (debug0(rho, "(non-locally bound)");(SOME (NON_LOCAL rho), ATTOP rho))
      )

  fun which_at env (rho,liveset) :  place at =
          #2(which_at0 false (env,rho,liveset))

  fun which_at_with_explanation(env,rho,liveset) 
      : conflict option * place at = which_at0 true (env,rho,liveset)

  fun analyse_rhos_for_resetting (sme, liveset, rhos) : place at list * conflict list= 
    let 
       fun loop([]:place list, acc1: place at list, acc2: conflict list) = (acc1,acc2)
         | loop(rho::rest, acc1,acc2)=
             case which_at_with_explanation(sme,rho,liveset) of
               (NONE, place_at) => loop(rest,place_at::acc1, acc2)
             | (SOME problem, place_at)          => loop(rest,place_at::acc1, problem::acc2)
    in
      loop(rhos,[],[])
    end

  (* rvars(sigma, p0) = ((sigma, p0), lrv) where lrv is the set of all 
     region and effect variables reachable from p or a free region or
     effect variables of sigma. *)

  fun rvars (sigma, p0): SME.lvar_env_range = 
      ((*Profile.profileOn();*)
       let val free_vars = RType.ferv_sigma sigma
           val free_vars' = p0 :: free_vars
           val lrv = RegFlow.reachable_with_insertion free_vars'
		     handle Find => Crash.impossible "AtInference: rvars "
       in 
          (*Profile.profileOff();*)
          ((sigma, p0),lrv)
       end)

  fun mu_to_scheme_and_place(mu) = 
      let val (tau,p) = mu
          val sigma = RType.type_to_scheme tau
      in 
          (sigma,p)
      end

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
		    of VAR{lvar,il,plain_arreffs,alloc,rhos_actuals=ref actuals,other} =>
		      let val alloc' = case alloc of 
                                         SOME alloc => 
                                           (if !debug_which_at then
                                               log("application of lvar: " ^ Lvars.pr_lvar lvar)
                                            else ();
                                            SOME(which_at sme alloc))
                                       | NONE => NONE
			  val actuals' = map (which_at sme) actuals  (* also liveset here*)
		      in VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,
			     alloc=alloc',rhos_actuals=ref actuals',other=()}
		      end
		     | INTEGER(n,(place,liveset)) => INTEGER(n, ATTOP place) (* no need for analysis *)
		     | STRING(s,(place,liveset)) => STRING(s, ATTOP place)       (* no need for analysis *)
		     | REAL(s,alloc) => REAL(s, which_at sme alloc)
		     | UB_RECORD trips => UB_RECORD(map (sma_trip sme) trips)
		     | FN{pat,body,free,alloc} => sma_fn(sme,SME.empty_regvar_env,pat,body,free,alloc)
		     | LETREGION{B,rhos,body} => 
                         let 
                            val (RE,LE,EE) = sme
                            fun extend (rho,mul) RE' = SME.declare_regvar_env(rho,SME.LETREGION_BOUND,RE')
                            val sme_body = (List.foldL extend RE (!rhos), LE, EE)
                         in 
                            LETREGION{B=B,rhos=rhos,body=sma_trip sme_body body}
                         end
		     | LET{k_let,pat,bind,scope} =>
  		         let val (RE,LE,EE) = sme
                             fun do_pat (lv,ils,tvs,effs,tau,p,other) = (lv,ils,tvs,effs,tau,p,())
                             fun extend (lvar,_,alphas,ref [], ty, rho, other) LE =
                                           SME.declare_lvar_env(lvar,rvars(mu_to_scheme_and_place(ty,rho)), LE)
                               | extend _ _ = die "non-empty list of bound region or effect variables at LET"
                             val sme_scope = (RE,List.foldL extend LE pat,EE)
		         in LET{k_let=k_let,pat=map do_pat pat,
                                bind=sma_trip sme bind,
                                scope=sma_trip sme_scope scope}
		         end
		     | FIX{free,shared_clos = shared_clos as (shared_rho,liveset),functions,scope} =>
  		         let 
                            val (RE,LE,EE) = sme
                            val LE' = List.foldL  (fn {lvar,tyvars, rhos, epss, Type, ...} => fn acc => 
                                       SME.declare_lvar_env(lvar,rvars(RType.FORALL(tyvars,rhos,epss,Type),shared_rho),acc))
                                      LE functions
                            val sme' = (RE,LE',EE)
                            fun do_function {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                                             bound_but_never_written_into,
                                             other,bind} =
                               (case bind of
                                  TR(FN{pat,body,free,alloc}, mu_lam, phi_lam, psi_lam) =>
                                    let
                                       fun extend rho RE' = SME.declare_regvar_env(rho,SME.LETREC_BOUND,RE')
                                       val RE_for_body_of_fn = List.foldL extend SME.empty_regvar_env rhos
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
		     | EXCEPTION(excon,b,tp,alloc as (rho,liveset),scope) =>
                         let val (RE,LE,EE) = sme
                             val sme_body = (RE,LE,SME.declare_excon_env(excon,rvars(mu_to_scheme_and_place tp), EE))
                     
                         in
		           EXCEPTION(excon,b,tp, ATTOP rho, 
                                     sma_trip sme_body scope)
                         end
		     | RAISE tr => RAISE (sma_trip sme tr)
		     | HANDLE(tr1,tr2) => HANDLE(sma_trip sme tr1, sma_trip sme tr2)
		     | SWITCH_I sw => SWITCH_I (sma_sw sme sw) 
		     | SWITCH_S sw => SWITCH_S (sma_sw sme sw) 
		     | SWITCH_C sw => SWITCH_C (sma_sw sme sw) 
		     | SWITCH_E sw => SWITCH_E (sma_sw sme sw) 
		     | CON0 {con, il, aux_regions, alloc} => 
  		           CON0 {con=con, il=il, 
                                  aux_regions=map (which_at sme) aux_regions, 
                                  alloc=which_at sme alloc} 
		     | CON1 ({con, il, alloc}, tr) => 
                           CON1 ({con=con,il=il,alloc= which_at sme alloc},
                                 sma_trip sme tr)
		     | DECON ({con, il}, tr) => DECON({con=con,il=il},sma_trip sme tr)
		     | EXCON (excon, opt) => EXCON(excon, case opt
							    of SOME (alloc as (p, liveset),tr) => 
                                                                 SOME (ATTOP p, sma_trip sme tr)
							     | NONE => NONE)
		     | DEEXCON (excon,tr) => DEEXCON(excon, sma_trip sme tr)
		     | RECORD (alloc, trs) => RECORD(which_at sme alloc,map (sma_trip sme) trs)
		     | SELECT (i, tr) => SELECT(i,sma_trip sme tr)
		     | DEREF tr => DEREF (sma_trip sme tr)
		     | REF (alloc,tr) => REF(which_at sme alloc, sma_trip sme tr)
		     | ASSIGN (alloc as (rho,_),tr1,tr2) => ASSIGN (ATTOP rho,sma_trip sme tr1, sma_trip sme tr2) (* no need for analysis *)
		     | EQUAL ({mu_of_arg1, mu_of_arg2, alloc = (p,liveset)}, tr1,tr2) => 
		      EQUAL ({mu_of_arg1=mu_of_arg1, mu_of_arg2=mu_of_arg2, alloc=ATTOP p},  (* no need for analysis *)
			     sma_trip sme tr1,sma_trip sme tr2)
		     | CCALL ({name, mu_result, rhos_for_result}, trs) =>  
		         CCALL ({name = name, mu_result = mu_result, 
				 rhos_for_result =
				     map (fn ((rho, liveset), i_opt) =>
					  (which_at sme (rho, liveset), i_opt))
				     rhos_for_result},
				map (sma_trip sme) trs)
		     | RESET_REGIONS ({force, alloc = (p, liveset), ...}, tr as (TR(VAR{lvar,...},meta,_,_))) => 
                          (case meta of
                             MulExp.RegionExp.Mus [mu] =>
                                   let 
				       val free_regions = 
                                             Eff.remove_duplicates(RType.frv_mu mu)

                                       val (place_at_list, conflicts) = 
                                               analyse_rhos_for_resetting(sme,liveset,free_regions)      
                                       val conflicts' = 
                                              if force then
                                                    List.foldL (fn SAT rho => (fn acc => FORMAL_REGION_PARAM rho :: acc)
                                                                 | _ => (fn acc => acc)) conflicts place_at_list
                                              else conflicts
                                   in
                                      case conflicts' of 
                                        [] => ()
                                      | _ => warn (PP.reportStringTree(lay_report(force,lvar,mu,conflicts')));
(*ME 1998-08-30
                                      | _ => (dump(lay_report(force,lvar,mu,conflicts'));
                                              warn (Report.// (Report.line "Warnings concerning resetting of regions \
							            \printed earlier in this file!",
							       Report.line "(Search on \"You have\")")));
*)
                                      RESET_REGIONS({force=force,alloc=ATTOP p,regions_for_resetting = place_at_list}, 
                                                       (* the place_at_list may contain word regions *)
                                                    sma_trip sme tr)
                                   end
                           | _ => die "RESET_REGIONS: expected a type and place on argument to resetRegions"
                          )
                     | RESET_REGIONS _ => die "ill-formed expression: argument to RESET_REGIONS should be a variable"
		     | FRAME{declared_lvars, declared_excons} =>
		      let fun f {lvar,sigma,other,place} = {lvar=lvar,sigma=sigma,other=(),place=place}
		      in FRAME{declared_lvars=map f declared_lvars, declared_excons = declared_excons}
		      end
                   ) handle Crash.CRASH => 
                           (log "\nStorage Mode Analysis failed at expression:";
                            dump(MulExp.layoutLambdaExp(fn _ => NONE)(fn _ => NONE)(fn _ => NONE)(fn _ => NONE)
                             e);
                            raise AbortExpression)

	    in TR(e', metaType, ateffects, mulef_r)
	    end
        and sma_fn(sme,regvar_env0,pat,body,free,alloc) = 
                     let val (_, LE, EE) = sme
                         fun extend (lvar, mu) LE = 
                           SME.declare_lvar_env(lvar,rvars(mu_to_scheme_and_place mu), LE)
                         val sme_body = (regvar_env0, 
                                         List.foldL extend LE pat,  
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

    fun sma(pgm: (place,    place*mul, qmularefset ref)LambdaPgm):
                 (place at, place*mul, unit)LambdaPgm =
        (debug_which_at := (Flags.is_on "debug_which_at");
         all_attop := (Flags.is_on "disable_atbot_analysis");
         chat "\nBuilding region flow graph...\n";
         Timing.timing_begin();
         RegFlow.mk_graph(pgm) handle _ => die "call of RegFlow.mk_graph failed";
         Timing.timing_end("RegFlow");
         Timing.timing_begin();
         chat "\nComputing locally live variables...\n";
         let val pgm' = LLV.llv pgm 
                        handle _ => die "call of LLV.llv failed"
         in 
           Timing.timing_end("LocLiveVar");
           chat "\nStorage mode analysis (NEW)...\n";
           Timing.timing_begin();
           (sma0(pgm')handle AbortExpression => die "call of sma0 failed")
             footnote Timing.timing_end("SMA (NEW)")
         end
        )

    (***********************************)
    (* Pretty printing                 *)
    (***********************************)

    type StringTree = PP.StringTree
    fun lay (s : string) (p: 'a -> StringTree) (a : 'a) : StringTree option = 
      SOME(PP.HNODE{start=s^" ",finish="",children=[p a],childsep=PP.NOSEP})

    fun layout_at (p: 'a -> StringTree) (at : 'a at) =
      case at
	of ATTOP a => lay "attop" p a
	 | ATBOT a => lay "atbot" p a
	 | SAT a => lay "sat" p a
	 | IGNORE => NONE

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
	 | IGNORE => NONE

    fun layout_at'' (p: 'a -> StringTree) (at : 'a at) =
      case at
	of ATTOP a => SOME(p a)
	 | ATBOT a => SOME(p a)
	 | SAT a => SOME(p a)
	 | IGNORE => NONE

    fun ignore _ = NONE

    fun layout_trip_brief(tr : (place at, place*mul, unit)trip): StringTree =
      if !Flags.print_regions then
         MulExp.layoutLambdaTrip 
             (layout_at' Eff.layout_effect)(layout_at'' Eff.layout_effect) (SOME o layout_placeXmul) layout_unit tr
      else
         MulExp.layoutLambdaTrip ignore ignore ignore layout_unit tr

    fun layout_exp_brief(e : (place at, place*mul, unit)LambdaExp): StringTree =
      if !Flags.print_regions then
          MulExp.layoutLambdaExp (layout_at' Eff.layout_effect)(layout_at'' Eff.layout_effect) (SOME o layout_placeXmul) layout_unit e
      else
          MulExp.layoutLambdaExp ignore ignore ignore layout_unit e


    fun layout_pgm (PGM{expression,...}) = layout_trip expression
    fun layout_pgm_brief (PGM{expression,...}) = layout_trip_brief expression

end (* AtInf *)




