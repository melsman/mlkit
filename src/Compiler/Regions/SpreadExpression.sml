(*$SpreadExpression: SPREAD_EXPRESSION CON EXCON LAMBDA_EXP REGION_EXP 
   FINMAP RTYPE EFFECT SPREAD_DATATYPE REGION_STAT_ENV 
   LVARS TYNAME CRASH PRETTYPRINT FLAGS C_CONST REPORT*)

(*
*
*   The module introduces region and effect variables into the
*   input lambda expression. This is done in such a way that the subsequent
*   phases of translation will not have to generate fresh region or effect
*   variables. In other words, all generation of fresh region and effect
*   variables takes place in SpreadExpression.

*)


functor SpreadExpression(
  structure Con: CON
  structure ExCon: EXCON
  structure E: LAMBDA_EXP
  structure E': REGION_EXP
    sharing type E.con = E'.con = Con.con
	and type E.TyName = E'.TyName 
        and type E.excon = E'.excon = ExCon.excon
  structure Eff: EFFECT
  structure R: RTYPE
    sharing type E'.tyvar = R.tyvar = E.tyvar
        and type R.cone = Eff.cone
        and type R.LambdaType  = E.Type 
        and type R.place = Eff.place = Eff.effect = E'.place = E'.effect 
        and type R.il = E'.il 
        and type R.Type = E'.Type
        and type R.runType = Eff.runType
  structure RSE: REGION_STAT_ENV
    sharing type RSE.TypeAndPlaceScheme = R.sigma = E'.sigma
        and type RSE.place = R.place
        and type RSE.Type = R.Type = E'.Type
        and type RSE.place = R.place
        and type RSE.runType = R.runType
        and type RSE.con = E.con = Con.con 
        and type RSE.excon = E.excon  = E'.excon
        and type RSE.il = R.il
        and type RSE.cone = R.cone = E'.cone
  structure SpreadDatatype: SPREAD_DATATYPE
        sharing type RSE.regionStatEnv = SpreadDatatype.rse
            and type SpreadDatatype.LambdaExp.datbinds = E.datbinds
            and type SpreadDatatype.cone = Eff.cone
            and type SpreadDatatype.RegionExp.datbinds = E'.datbinds
  structure FinMap : FINMAP
  structure Flags: FLAGS
  structure Report : REPORT
  sharing type Report.Report = Flags.Report
  structure Lvars: LVARS
    sharing type Lvars.lvar = E.lvar = E'.lvar = RSE.lvar
  structure TyName: TYNAME
    sharing type TyName.TyName = E.TyName = E'.TyName = RSE.TyName =  R.tyname
  structure Crash: CRASH
  structure PP: PRETTYPRINT
    sharing type PP.StringTree =  E.StringTree = RSE.StringTree  = R.StringTree
  structure CConst : C_CONST
    sharing TyName = CConst.TyName
): SPREAD_EXPRESSION =
struct
  structure E = E
  structure E' = E'
  structure RegionStatEnv = RSE 

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
  fun say(s) = output(std_out, s ^ "\n");
  fun logsay(s) = output(!Flags.log, s);
  fun logtree(t:PP.StringTree) = PP.outputTree(logsay, t, !Flags.colwidth)

  fun log_sigma(sigma1, lvar) = 
    case R.bv sigma1 of 
      ([], _, _) => 
        (say ("***" ^ Lvars.pr_lvar lvar ^ " is:");
         logsay (Lvars.pr_lvar lvar ^ " is:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n")
    | (alpha::alphas,[],_) =>
        (say ("******" ^ Lvars.pr_lvar lvar ^ " is  polymorphic with escaping regions");
         logsay (Lvars.pr_lvar lvar ^ " is polymorphic with escaping regions:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n")
    | (alpha::alphas,_,_) =>
        (say ("***" ^ Lvars.pr_lvar lvar ^ " is  polymorphic");
         logsay (Lvars.pr_lvar lvar ^ " is polymorphic:\n");
         logtree(R.mk_lay_sigma false sigma1);
         logsay "\n");

  fun noSome x msg = 
    case x of 
      Some it => it
    | None => Crash.impossible msg

  fun die s = Crash.impossible ("SpreadExpression." ^ s)

  fun warn (lvar, Some s) = Flags.warn
	(Report.// (Report.line ("algorithm S (SpreadExpression), while processing\
				 \ lvar: " ^ Lvars.pr_lvar lvar ^ ":"),
		    Report.line s))
    | warn _ = ()


  (* operations which are imported from other modules (for profiling) *)

  fun matchSchemes(sigma1,sigma2) = R.matchSchemes(sigma1,sigma2)
  fun generalize_all(B,level,tyvars,tau) = R.generalize_all(B,level,tyvars,tau)
  fun ann_mus mus l = R.ann_mus mus l
  fun inst(sigma,il)B = R.inst(sigma,il)B
  fun unify_mu(mu1,mu2)B = R.unify_mu(mu1,mu2)B
  fun unify_mus(mus1,mus2)B = R.unify_mus(mus1,mus2)B
  fun regEffClos(B,level,phi,tau) = R.regEffClos(B,level,phi,tau)
  fun ann_mus mus l = R.ann_mus mus l
  fun runtype tau = R.runtype tau

  (* from Eff: *)
  fun push B = Eff.push B
  fun pop B = Eff.pop B
  fun lower level eps B = Eff.lower level eps B
  fun popAndClean B = Eff.popAndClean B
  fun mkUnion l = Eff.mkUnion l
  fun observeDelta(level,t,phi) = Eff.observeDelta(level,t,phi)
  fun freshRhos(rhos,A) = Eff.freshRhos(rhos,A)
  fun freshEps B = Eff.freshEps B
  fun freshEpss(epss,A) = Eff.freshEpss(epss,A)
  fun mkGet rho = Eff.mkGet rho
  fun mkPut rho = Eff.mkPut rho
  fun freshRhoWithTy(rt,B) = Eff.freshRhoWithTy(rt,B)
  fun edge(eps,phi) = Eff.edge(eps,phi)
  fun unifyRho(rho1,rho2)B = Eff.unifyRho(rho1,rho2)B
  fun is_rho node = Eff.is_rho node
  fun remove_duplicates l = Eff.remove_duplicates l

  (* from SpreadDatatype: *)
  fun spreadDatbinds rse datbinds cone = 
      SpreadDatatype.spreadDatbinds rse datbinds cone

  (* from RSE: *)
  fun declareLvar(lvar,tuple,rse) = RSE.declareLvar(lvar,tuple,rse)
  fun lookupLvar rse lvar = RSE.lookupLvar rse lvar
  fun lookupTyName rse tyname = RSE.lookupTyName rse tyname
  fun declareExcon(excon,mu,rse) = RSE.declareExcon(excon,mu,rse)
  fun lookupExcon rse excon = RSE.lookupExcon rse excon
  fun lookupCon rse con = RSE.lookupCon rse con
  fun plus(rse,rse') = RSE.plus(rse,rse')

  (* end of imported functions *)

  fun crash_resetting force = 
      let val fcn = if force then "forceResetting" else "resetRegions"
      in die ("S: argument to " ^ fcn ^ " must be a variable which\
       \ is monomorphic (also in regions and effects)")
      end

  val exn_ty  = E.CONStype([], TyName.tyName_EXN)

  fun declareMany(rho,rse)([],[]) = rse
    | declareMany(rho,rse)((lvar,tyvars,sigma_hat,bind):: rest1, occ::occ1) =
        declareMany(rho,(*RSE.*)declareLvar(lvar,(true,true,sigma_hat, rho, Some occ, None), rse))(rest1,occ1)
    | declareMany _ _ = die ".declareMany"


  fun repl([],[]) = []
    | repl({lvar,tyvars,Type,bind}::fcns1, sigma_hat::sigma_hat_rests) =
            (lvar,tyvars,sigma_hat,bind):: repl(fcns1,sigma_hat_rests)
    | repl _ = die ".repl: sigma_hat_list and rhs lists have different lengths"


  fun adjust_instances(transformer, occ as ref l) =
       List.apply (fn r as ref(il, f)=> r:= (il, transformer o f)) l

  fun mkRhs(rse,rho)([],[],[]) = (rse,[])
    | mkRhs(rse,rho)((lvar,tyvars,sigma_hat,bind)::rest1,
                     (t1,tau1,sigma1)::rest2, 
                     occ::rest3) =
    let
      val (_,brhos, bepss) = R.bv(sigma1)
      val transformer = matchSchemes(sigma_hat, sigma1) handle R.FAIL_MATCH msg =>
          die ("mkRhs: lvar = " ^ Lvars.pr_lvar lvar ^ "\n" ^ msg)
      val _ = adjust_instances(transformer, occ)
      val function = {lvar = lvar, occ = occ, tyvars = tyvars, rhos = ref brhos, epss = ref bepss,
                      Type = tau1, formal_regions = None, bind = t1}
      val rse2 = (*RSE.*)declareLvar(lvar,(true, true, R.insert_alphas(tyvars, sigma1),
                                       rho, Some occ, None), rse)
      val (rse2', l) = mkRhs(rse2,rho)(rest1,rest2,rest3)
    in
      (rse2', function::l)
    end
   |  mkRhs _ _ = die ".mkRhs"

  exception Abort

  fun die_from_S(e) =
         (output(std_out,
		"Failed to spread expression:" ^
		 PP.flatten(PP.format(200, E.layoutLambdaExp e )) ^ "\n");
          raise Abort)

  fun save_il(instances_opt, il_r) = 
    (* record il in the environment --- 
     to be picked up for letrec-bound variables at fix *)
    (case instances_opt of
       Some(r as ref(list)) => 
         (* lvar is fix bound or global 
          (from earlier topdec);
          extend the instances list for 
          lvar in the region-static environment
          with the instantiation list of the lvar
         *)
         r:= il_r::list
     | None => ()
  );

  fun pushIfNotTopLevel(toplevel,B) =
      if toplevel then B else (*Eff.*)push B;

  fun Below(B, mus) =
    let val free_rhos_and_epss = ann_mus mus []
        val B' = List.foldL  ((*Eff.*)lower(Eff.level B - 1)) B free_rhos_and_epss
    in 
        (*Eff.*)popAndClean(B')
    end

  fun retract(B, t as E'.TR(e, E'.Mus mus, phi)) =
    let
      val (B_discharge,B_keep) = Below(B, mus)
      val phi' = (*Eff.*)mkUnion([])
      val (discharged_phi,_) = (*Eff.*)observeDelta(Eff.level B_keep, Eff.Lf[phi],phi')
      (* phi' updated to contain observed effect *)
    in (B_keep, E'.TR(E'.LETREGION_B{B= ref B_discharge, 
                                     discharged_phi = ref discharged_phi,
                                     body = t}, E'.Mus mus, phi'))	
    end
    | retract(B, t) = (B,t)


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

  fun spreadExp(B: cone, rse,  e: E.LambdaExp, toplevel): cone * (place,unit)E'.trip =
  let
    fun lookup tyname = case (*RSE.*)lookupTyName rse tyname of
          Some arity =>
            let val (a, l, c) = RSE.un_arity arity
            in Some(a, (*List.size*) (l), c)
            end
        | None => None

    local
      val (freshType', freshMu') = R.freshType lookup 
    in
      fun freshType(tau,B) = freshType'(tau,B)   (* for profiling *)
      fun freshMu(tau,B) = freshMu'(tau,B)
    end

    fun freshTypes(cone:cone, types: E.Type list) = 
    case types of [] => ([],cone)
    | (tau_ml::rest) => 
         let val (tau, cone) = freshType(tau_ml,cone)
             val (taus, cone) = freshTypes(cone,rest)
         in
             (tau::taus, cone)
         end

    fun freshTypesWithPlaces(cone:cone, types: E.Type list) = 
    case types of [] => ([],cone)
    | (tau_ml::rest) => 
         let val (mu, cone) = freshMu(tau_ml,cone)
             val (mus, cone) = freshTypesWithPlaces(cone, rest)
         in
             (mu::mus, cone)
         end

     fun count_fix_rhs() = ()

     fun mk_sigma_hat_list(B,retract_level) [] = (B,[])
       | mk_sigma_hat_list(B,retract_level)({lvar, tyvars,Type,bind}::rest) = 
          let
            (*val _ = output(std_out, "mk_sigma_hat_list: " ^ Lvars.pr_lvar lvar ^ "\n")*)
            val _ = count_fix_rhs()
            val B = (*Eff.*)push(B);         (* for generalize_all *)
              val E.ARROWtype(tau_x_ml, tau_1_ml) = Type
              val (tau_0, B) = freshType(Type,B)
              val (B,sigma, msg_opt) = generalize_all(B,retract_level,tyvars,tau_0)
              val sigma_hat = R.drop_alphas sigma
            val (_,B) = (*Eff.*)pop B (* back to retract level *)
            val (B, l) = mk_sigma_hat_list(B,retract_level) rest
          in
             (B,sigma_hat::l)
          end

    fun newInstance(A: cone,sigma:R.sigma, taus: E.Type list): cone*R.Type*R.il = 
      let val (alphas, rhos, epss) = R.bv sigma
          val (taus', A) = freshTypes(A,taus)
          val (rhos', A) = (*Eff.*)freshRhos(rhos, A)
          val (epss', A) = (*Eff.*)freshEpss(epss, A)
          val il = R.mk_il(taus',rhos',epss')
          val (tau', A1) = inst(sigma,il) A (* side-effects il *)
      in
          (A1, tau', il)
      end

    (* get_exn_mu(mu') if mu' is the type and place of a nullary exception constructor,
       return mu'; otherwise mu' = (mu_1 -> mu_2, rho): return mu_2 *)

    fun get_exn_mu(R.FUN(_,_,[mu2]),_) = mu2
      | get_exn_mu mu' = mu'

    fun spreadSwitch (B:cone) spread con excon_mus
                 (E.SWITCH(e0: E.LambdaExp,
                           choices: ('c * E.LambdaExp) list,
                           last: E.LambdaExp Option),toplevel): cone * (place,unit)E'.trip=
    let
      val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
      val (B,t0 as E'.TR(e', E'.Mus mus_0, phi_0)) = spread(B,e0,false)
      val mu_0 as (_,object_rho) = 
          case mus_0 of [mu_0] => mu_0 | _ => die "S. ill-typed object of switch"
      val B = List.foldL (fn mu => unify_mu(get_exn_mu mu,mu_0)) B excon_mus
      val (B, new_choices) = List.foldR (fn (c, e) => fn (B, ts) => 
                                let val (B, t) = spread(B,e,toplevel)
                                in (B, t:: ts)
                                end) (B,[]) choices
      val (B, new_last) = case last of None => (B,None)
                            | Some e_last => let val (B, t_last) = spread(B,e_last,toplevel)
                                             in (B, Some t_last)
                                             end
      (* unify types of branches - when they are not frames or raised Bind types *)

      val (B,metatype) = 
          (case List.first (fn E'.TR(_,E'.Mus mus,_) => true | _ => false) new_choices of
            E'.TR(_,E'.Mus mus1,_) => 
                (List.foldL (fn E'.TR(_,E'.Mus mus,_) => unify_mus(mus,mus1)
                             | E'.TR(_, _, _) => (fn B=>B)) B
                           (case new_last of None => new_choices | Some t' => t'::new_choices),
                 E'.Mus mus1)
	  | _ => die "spreadSwitch" 
          )handle List.First _ => 
          (case List.first (fn E'.TR(_,E'.Frame _, _) => true | _ => false) new_choices of
            E'.TR(_,metatype,_) => (B,metatype)
          )handle List.First _ => (B, E'.RaisedExnBind)
          
      (* val accumulate effects*)
      val phi = (*Eff.*)mkUnion(phi_0:: (*Eff.*)mkGet object_rho::
                            (fn rest => case new_last of None => rest 
                                          | Some (E'.TR(_,_,phi_n)) => phi_n ::rest)
                            (map (fn E'.TR(_,_,phi_i)=> phi_i) new_choices)
                            )
      val e' = E'.SWITCH(t0,ListPair.zip(map #1 choices, new_choices), new_last)
    in
      retract(B,E'.TR(con(e'), metatype,phi))
    end handle _ => die "spreadSwitch: cannot spread (uncaught exception)"

   fun S(B,e,toplevel:bool): cone * (place,unit)E'.trip = 
      (case e of
      E.VAR{lvar, instances : E.Type list} =>
       (case (*RSE.*)lookupLvar rse lvar of
	  Some(compound,create_region_record, 
               sigma,place0,instances_opt, transformer) =>
            let 
              val (B, tau, il_1) = newInstance(B,sigma,instances)
              val il_r = ref (il_1, fn p => p)
              val _ = save_il(instances_opt, il_r)
            in
              if compound andalso create_region_record then
                     let val (rho',B) = (*Eff.*)freshRhoWithTy(Eff.TOP_RT, B)
                         val phi =(*Eff.*)mkUnion([(*Eff.*)mkGet place0,
                                               (*Eff.*)mkPut rho'] (*@  deleted - cf. final submission to Information and Computation
                                               map (*Eff*).mkGet (#2 (R.un_il il_1))*))
                          
                     in 
                        (B,E'.TR(E'.VAR{lvar = lvar, alloc = Some rho', il_r = il_r}, 
                                 E'.Mus [(tau,rho')], phi))
                     end 
              else
                  (B,E'.TR(E'.VAR{lvar = lvar, alloc = None, il_r = il_r}, 
                           E'.Mus [(tau,place0)], Eff.empty))
            end
         | None => die "spreadExp: free lvar"
       )

    | E.INTEGER(i: int)=> 
        let 
            val (rho, B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
            val tau = R.intType
        in
	    (B,E'.TR(E'.INTEGER(i, rho),E'.Mus[(tau,rho)], (*Eff.*)mkPut rho))
        end
    | E.STRING(s: string)=> 
        let 
            val (rho, B) = (*Eff.*)freshRhoWithTy(Eff.STRING_RT, B)
            val tau = R.stringType
        in
	    (B,E'.TR(E'.STRING(s, rho),E'.Mus [(tau,rho)], (*Eff.*)mkPut rho))
        end
    | E.REAL(r: real)=> 
        let 
            val (rho, B) = (*Eff.*)freshRhoWithTy(Eff.REAL_RT, B)
            val tau = R.realType
        in
	    (B,E'.TR(E'.REAL(r, rho),E'.Mus [(tau,rho)], (*Eff.*)mkPut rho))
        end
    | E.PRIM(E.UB_RECORDprim, args) =>
        (* For simplicity, we demand that the arguments of UB_RECORDprim must themselves 
           have a singleton list of type and places. Thus we do not allow, for example
           UB_RECORD[UB_RECORD[2, 3], UB_RECORD[4,5]] although we do allow
           UB_RECORD[RECORD[2, 3], RECORD[4,5]] and
           UB_RECORD[2, 3, 4,5]
        *)
           
        let val (B, triples, mus, phis) = List.foldL(fn exp => fn (B, triples', mus, phis) =>
              let val (B,trip as E'.TR(e',E'.Mus mus1,phi)) = S(B, exp, false)
              in case mus1 of
                   [mu] => (B, trip::triples', mu :: mus, phi::phis)
                 | _ => die ".S: unboxed record expression with compound, unboxed argument"
              end) (B,[],[],[]) args
            val phi = (*Eff.*)mkUnion(rev phis)
            val mus = rev mus
	    val triples = rev triples
        in
          (B, E'.TR(E'.UB_RECORD(triples), E'.Mus mus, phi))
        end
    | E.FN{pat: (E.lvar * E.Type) list, body: E.LambdaExp} =>
        let 
          val (mus, B) = freshTypesWithPlaces (B, map #2 pat)
          val rse' = List.foldL (fn (lvar, mu as (tau,rho))=> fn rse =>
                         (*RSE.*)declareLvar(lvar, (false,false,R.type_to_scheme tau, rho,None,None), rse)) rse
                         (ListPair.zip(map #1 pat, mus))
          val (B,t1 as E'.TR(e1',E'.Mus mu_list1, phi1)) = spreadExp(B,rse',body,false)
          val (eps, B) = (*Eff.*)freshEps B
          val _ = (*Eff.*)edge(eps, phi1)
          val (rho, B) = (*Eff.*)freshRhoWithTy(Eff.TOP_RT, B)
        in
          (B, E'.TR(E'.FN{pat = ListPair.zip(map #1 pat, mus), body = t1, alloc = rho},
                    E'.Mus [(R.FUN(mus,eps,mu_list1), rho)], (*Eff.*)mkPut(rho)))
        end
    | E.APP(e1_ML: E.LambdaExp, e2_ML: E.LambdaExp) => 
        let
            val simple_application: bool =
                case e1_ML of E.VAR{lvar, ...} =>
                       (case (*RSE.*)lookupLvar rse lvar of
                          Some(compound, _,_,_,_,_) => not(compound)
                        | _ => die ("E.APP(E.VAR ...): Lvar " ^ Lvars.pr_lvar lvar ^ " not in RSE."))
                | _ => false
         
            val B = if simple_application then B else pushIfNotTopLevel(toplevel,B)
	    val (B,t1 as E'.TR(e1, E'.Mus [(R.FUN(mus2,eps_phi0,mus1),rho_0)],phi1)) = S(B,e1_ML, false)
            val (B,t2 as E'.TR(e2, E'.Mus mus2', phi2)) = S(B,e2_ML, false)
            val B = unify_mus (mus2,mus2') B
        in
            if simple_application then (B, E'.TR(E'.APP(t1,t2), E'.Mus mus1, 
                    (*Eff.*)mkUnion([eps_phi0, (*Eff.*)mkGet rho_0, phi1,phi2])))
            else retract(B, E'.TR(E'.APP(t1,t2), E'.Mus mus1, 
                    (*Eff.*)mkUnion([eps_phi0, (*Eff.*)mkGet rho_0, phi1,phi2])))
        end

   | E.LET{pat, bind = e1_ML, scope = e2_ML} =>
        let
           val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
           val (B, t1 as E'.TR(e1, E'.Mus mus, phi1)) = S(B, e1_ML, false)

           fun loop_pat([], [], B, rse, pat'_list) = (B,rse, rev pat'_list)
             | loop_pat((lvar,alphas,tau_ML):: rest_bind, (tau_1, rho_1):: mu_rest,
                        B, rse, pat'_list) =
                 let  
                      val sigma = R.type_to_scheme tau_1
(*                      val _ = log_sigma(R.insert_alphas(alphas, sigma),lvar)*)
                      val rse = (*RSE.*)declareLvar(lvar,
                                 (false,false,R.insert_alphas(alphas, sigma), 
                                  rho_1, None, None),rse)
                 in
                      loop_pat(rest_bind, mu_rest, B, rse, 
                               (lvar,alphas, tau_1, rho_1) :: pat'_list)
                 end
            | loop_pat _ = die ".loop_pat: length of pattern and list of types and places differ"

           val (B,rse, pat'_list) = loop_pat(pat, mus, B, rse, [])
           val (B, t2 as E'.TR(e2, meta2, phi2)) = spreadExp(B,rse,e2_ML,toplevel)
        in
          retract(B, E'.TR(E'.LET{pat = pat'_list,
                                  bind = t1, scope = t2}, meta2, (*Eff.*)mkUnion([phi1,phi2])))
        end

(* good (as in paper):
    | E.FIX{functions as[{lvar, tyvars,Type,bind}], scope} =>
        let
          val B = (*Eff.*)push(B);         (* for pop in retract *)
            val retract_level = Eff.level B 
            val (rho,B) = Eff.freshRho B (* for shared region closure *)
            val B = Eff.push(B);         (* for generalize_all *)
              val E.ARROWtype(tau_x_ml, tau_1_ml) = Type
              val (tau_0, B) = freshType(Type,B)
              val (B,sigma, msg_opt) = generalize_all(B,retract_level,tyvars,tau_0)
            val (_,B) = Eff.pop B (* back to retract level *)
            val B = Eff.push(B)
              val sigma_hat = R.drop_alphas sigma
              val occ = ref []  : R.il ref list ref
              val (B, t1 as E'.TR(E'.FN{pat, body = t1', alloc}, E'.Mus [(tau1, rho1)], phi1)) =
                  spreadExp(B, (*RSE.*)declareLvar(lvar,(true,true,sigma_hat, rho, Some occ, None), rse), bind)
              val B = Eff.unifyRho(rho1,rho) B          
              val (B,sigma1,msg_opt) = regEffClos(B, retract_level, phi1, tau1)
            val (_,B) = Eff.pop B (* back to retract level *)
            val transformer = matchSchemes(sigma_hat, sigma1) handle R.FAIL_MATCH msg =>
                  die ("fix: lvar = " ^ Lvars.pr_lvar lvar ^ "\n" ^ msg)
            val _ = adjust_instances(transformer, occ)
            val (B, t2 as E'.TR(e2, E'.Mus mus, phi2)) = 
                  spreadExp(B, (*RSE.*)declareLvar(lvar,(true, true, R.insert_alphas(tyvars, sigma1),
                                                     rho1, Some occ, None), rse), scope)
            val (_,brhos, bepss) = R.bv(sigma1)
            val e' = E'.FIX{shared_clos = rho,
                          functions = [{lvar = lvar, tyvars = tyvars, rhos = brhos, epss = bepss,
                                        Type = tau1, formal_regions = None, bind = t1}],
                          scope = t2}
        in
          retract(B, E'.TR(e', E'.Mus mus, Eff.mkUnion[phi1,phi2]))
        end (* FIX *)
good *)
      
    | E.FIX{functions, scope} =>
        let
          val B = pushIfNotTopLevel(toplevel,B) ;         (* for pop in retract *)
            val retract_level = Eff.level B 
            val (rho,B) = (*Eff.*)freshRhoWithTy(Eff.TOP_RT,B) (* for shared region closure *)
            val phi1 = (*Eff.*)mkPut rho
            val (B,sigma_hat_list)= mk_sigma_hat_list(B,retract_level)(functions)
            val (B,rse2,functions') = spreadFcns(B,rho,retract_level,rse)(repl(functions,sigma_hat_list))
            val (B, t2 as E'.TR(_, meta2, phi2)) = spreadExp(B, rse2, scope,toplevel)
            val e' = E'.FIX{shared_clos = rho,functions = functions',scope = t2}
        in
          retract(B, E'.TR(e', meta2, (*Eff.*)mkUnion([phi1,phi2])))
        end (* FIX *)

    | E.EXCEPTION(excon, ty_opt: E.Type Option, e2: E.LambdaExp) =>  
        let
            val (ty,nullary) = 
              case ty_opt of
                Some ty1 => (E.ARROWtype([ty1], [exn_ty]),false)
              | None => (exn_ty, true)
            val (mu as (tau, rho), B) = freshMu(ty, B)
            (* lower all the region and effect variables of mu to have level 2 (not 0),
               so that they cannot be generalised ever. Level 2, because it is generated 
	       in this program unit, unless unified with another lower-level rho. *)
            val B = List.foldL ((*Eff.*)lower 2) B (ann_mus [mu] [])
            (* if exception constructor is unary: unify place of exception
               constructor  and place of its result type. Note: I think 
               we could have chosen not to identify these two regions *)
            val B = case tau of
                      R.FUN(_,_,mus as [(tau_res, rho_res)]) => 
                          (*Eff.*)unifyRho(rho_res, rho) B
                    | _ => B
            val rse' = (*RSE.*)declareExcon(excon, mu, rse)
            val (B, t2 as E'.TR(e2', meta2, phi2)) = spreadExp(B,rse',e2, toplevel)
        in
            (B, E'.TR(E'.EXCEPTION(excon, nullary, mu, rho, t2), meta2, 
                      (*Eff.*)mkUnion([(*Eff.*)mkPut rho,phi2])))
        end

    | E.RAISE(e1: E.LambdaExp, description) =>
        let 
          val (description',B) = 
                case description of
                  E.Types (taus : E.Type list) => 
                    let val (mus, B) = freshTypesWithPlaces(B,taus)
                    in (E'.Mus mus, B)
                    end	
                | E.RaisedExnBind => (E'.RaisedExnBind, B)
                | E.Frame _ => die "spreading of RAISE failed: RAISE was annotated with a FRAME type"
          val (B, t2 as E'.TR(e2', E'.Mus mus2, phi2)) = S(B,e1,false)
        in
            (B,E'.TR(E'.RAISE(t2),description', phi2))
        end

    | E.HANDLE(e1,e2) =>
        let 
          val B = pushIfNotTopLevel(toplevel,B); (* for retract *)
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,e1, false)
          val (B, t2 as E'.TR(e2', E'.Mus mus2, phi2)) = S(B,e2, false)
        in 
          case mus2 of 
            [(R.FUN(mus21,arreff,mus22),rho2)] => 
             let val B = unify_mus(mus22,mus1) B
                 val phi = (*Eff.*)mkUnion([phi1,phi2,arreff,(*Eff.*)mkGet rho2])
                 (* lower all the region and effect variables of mus21 to have level 2 (not 0),
                    so that they cannot be generalised ever. Level 2, because it is generated 
    	            in this program unit, unless unified with another lower-level rho. *)
                 val B = List.foldL ((*Eff.*)lower 2) B (ann_mus mus21 [])
             in
               retract(B, E'.TR( E'.HANDLE(t1,t2), E'.Mus mus22, phi)) 
             end
          | _ => die "S: ill-typed handle expression"
        end
    | E.PRIM(E.REFprim{instance}, [e1])=>
(*

                                          e1' : [mu], phi1    
                 -------------------------------------------------------------
                        ref e1'  : [(mu ref, rho_new)],  phi1 u {put rho_new}
*)
        let
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,e1, false)
          val (rho_new, B) = (*Eff.*)freshRhoWithTy(Eff.TOP_RT, B)
          val phi = (*Eff.*)mkUnion([(*Eff.*)mkPut rho_new, phi1])
          val mus = [(R.CONSTYPE(TyName.tyName_REF, mus1,[],[]),rho_new)]
        in
          (B, E'.TR(E'.REF (rho_new, t1), E'.Mus mus,phi))
        end

    | E.PRIM(E.DEREFprim{instance}, [e1])=>
(*
                           e1' : [([mu] ref,rho)], phi1    
                       --------------------------------------
                        ! e1'  : [mu],  phi1 u {get rho}
*)
        let
          val B = pushIfNotTopLevel(toplevel,B)
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,e1, false)
        in
          case mus1 of
            [(R.CONSTYPE(tyname_ref, mus, [], []), rho)] =>
               retract(B, E'.TR(E'.DEREF t1, E'.Mus mus,
                        (*Eff.*)mkUnion([(*Eff.*)mkGet rho, phi1])))
          | _ => die "S: ill-typed rereferencing"
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

*)
        let
          val B = pushIfNotTopLevel(toplevel,B); (* for retract *)
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,e1, false)
          val (B, t2 as E'.TR(e2', E'.Mus mus2, phi2)) = S(B,e2, false)
        in case (mus1,mus2) of
             ([(R.CONSTYPE(ref_tyname, [mu1],[],[]),rho1)], [mu2]) =>
               let val B = unify_mu(mu1,mu2)B
                   val (rho3, B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
                   val phi = (*Eff.*)mkUnion([(*Eff.*)mkPut rho1, (*Eff.*)mkGet rho1, (*Eff.*)mkPut rho3,phi1, phi2])
               in
                  retract(B, E'.TR(E'.ASSIGN(rho3,t1,t2), E'.Mus [(R.unitType, rho3)], phi))
               end
           | _ => die "S: ill-typed assignment"
        end


    (* -----------------------------------------------------------------------
     * Compiler supported primitives; For all these primitives the backend
     * knows of the exceptions that the primitives can raise, hence we
     * do not explicitly pass exception constructors. Note that the types
     * in rse and other environments following spread-expression, must use
     * the same convention. For now, we explicitly filter out exception
     * constructors in primitives. Later we could adjust CompileDec, etc.
     * ----------------------------------------------------------------------- *)

    (* INTEGER OPERATIONS *)
    | E.PRIM(E.PLUS_INTprim,e1::e2::_)    => S_built_in(B, Lvars.plus_int_lvar, [e1,e2])
    | E.PRIM(E.MINUS_INTprim,e1::e2::_)   => S_built_in(B, Lvars.minus_int_lvar, [e1,e2])
    | E.PRIM(E.MUL_INTprim,e1::e2::_)     => S_built_in(B, Lvars.mul_int_lvar, [e1,e2])
    | E.PRIM(E.NEG_INTprim,e1::_)         => S_built_in(B, Lvars.negint_lvar, [e1])
    | E.PRIM(E.ABS_INTprim,e1::_)         => S_built_in(B, Lvars.absint_lvar, [e1])
    | E.PRIM(E.LESS_INTprim,[e1,e2])      => S_built_in(B, Lvars.less_int_lvar, [e1,e2])
    | E.PRIM(E.LESSEQ_INTprim,[e1,e2])    => S_built_in(B, Lvars.lesseq_int_lvar, [e1,e2])
    | E.PRIM(E.GREATER_INTprim,[e1,e2])   => S_built_in(B, Lvars.greater_int_lvar, [e1,e2])
    | E.PRIM(E.GREATEREQ_INTprim,[e1,e2]) => S_built_in(B, Lvars.greatereq_int_lvar, [e1,e2])
    | E.PRIM(E.EQUAL_INTprim,[e1_ML: E.LambdaExp,e2_ML: E.LambdaExp]) => 
        let
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
	    val (B,t1 as E'.TR(e1, E'.Mus [mu1 as (_,rho_1)],phi1)) = S(B,e1_ML, false)
	    val (B,t2 as E'.TR(e2, E'.Mus [mu2 as (_,rho_2)],phi2)) = S(B,e2_ML, false)
            val (rho,B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
            val mus = [(R.boolType,rho)]
        in
          retract
            (B, E'.TR(E'.EQUAL({mu_of_arg1=mu1, mu_of_arg2 = mu2, alloc = rho},
                               t1, t2),
                      E'.Mus mus,
                    (*Eff.*)mkUnion([phi1,phi2,mkGet rho_1, mkGet rho_2, mkPut rho])))
        end

    (* REAL OPERATIONS *)
    | E.PRIM(E.PLUS_REALprim,e1::e2::_)   => S_built_in(B, Lvars.plus_float_lvar, [e1,e2])
    | E.PRIM(E.MINUS_REALprim,e1::e2::_)  => S_built_in(B, Lvars.minus_float_lvar, [e1,e2])
    | E.PRIM(E.MUL_REALprim,e1::e2::_)    => S_built_in(B, Lvars.mul_float_lvar, [e1,e2])
    | E.PRIM(E.NEG_REALprim,e1::_)        => S_built_in(B, Lvars.negfloat_lvar, [e1])
    | E.PRIM(E.ABS_REALprim,e1::_)        => S_built_in(B, Lvars.absfloat_lvar, [e1])
    | E.PRIM(E.LESS_REALprim,[e1,e2])     => S_built_in(B, Lvars.less_float_lvar, [e1,e2])
    | E.PRIM(E.LESSEQ_REALprim,[e1,e2])   => S_built_in(B, Lvars.lesseq_float_lvar, [e1,e2])
    | E.PRIM(E.GREATER_REALprim,[e1,e2])  => S_built_in(B, Lvars.greater_float_lvar, [e1,e2])
    | E.PRIM(E.GREATEREQ_REALprim,[e1,e2]) => S_built_in(B, Lvars.greatereq_float_lvar, [e1,e2])


    | E.SWITCH_I(intsw: int E.Switch)       => (spreadSwitch B S E'.SWITCH_I [] (intsw,toplevel))
    | E.SWITCH_S(stringsw:  string E.Switch)=> (spreadSwitch B S E'.SWITCH_S [] (stringsw,toplevel))
    | E.SWITCH_C(consw: E.con E.Switch) => (spreadSwitch B S E'.SWITCH_C [] (consw,toplevel))
    | E.SWITCH_E(exconsw: E.excon E.Switch as
                 E.SWITCH(_,choices,_))=>(spreadSwitch B S E'.SWITCH_E
                                          (map (fn (excon,_) => noSome ((*RSE.*)lookupExcon rse excon) 
                                                "spreadExceptionSwitch: excon not in rse") 
                                           choices) (exconsw, toplevel))

    | E.PRIM(E.CONprim{con, instances}, []) =>
        let 
          val sigma = noSome ((*RSE.*)lookupCon rse con) ".S: constructor not in RSE"
          val (B, tau', il) = newInstance(B,sigma,instances)
          val aux_regions = (case tau' of R.CONSTYPE(_,_,rhos,_) => rhos 
                             | _ => die "S: nullary constructor not of constructed type")
          val (rho, B) = (*Eff.*)freshRhoWithTy(runtype tau', B)
          val result_type = (tau',rho)
        in
          (B, E'.TR(E'.CON0{con=con, il = il, aux_regions=aux_regions, alloc = rho}, E'.Mus [(tau',rho)], 
                    (*Eff.*)mkUnion(map (*Eff.*)mkPut (rho::aux_regions))))
        end 
    | E.PRIM(E.CONprim{con, instances}, [arg]) =>
        let 
          val sigma = noSome ((*RSE.*)lookupCon rse con) "S (CONprim): constructor not in RSE"
          val (B, tau', il) = newInstance(B,sigma,instances)
          val (mu1,_,mus2,mu2) = case tau' of R.FUN(mu1,areff, mus2 as [mu2]) => (mu1,areff,mus2,mu2)
                                  | _ => die "S: unary constructor not functional"
          val (B, t1 as E'.TR(e1', E'.Mus mu1', phi1)) = S(B, arg, false)
          val B = unify_mus(mu1',mu1) B
        in
          (B, E'.TR(E'.CON1({con=con, il = il, alloc = #2(mu2)},t1), E'.Mus mus2, 
                 (*Eff.*)mkUnion([phi1,(*Eff.*)mkPut(#2(mu2))])))
        end 
    | E.PRIM(E.DECONprim{con, instances}, [arg]) =>
        let 
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val sigma = noSome ((*RSE.*)lookupCon rse con) "S (DECONprim): constructor not in RSE"
          val (B, tau', il) = newInstance(B,sigma,instances)
          val (mu1,arreff,mus2,mu2) = case tau' of R.FUN(mu1,areff, mus2 as [mu2]) => (mu1,areff,mus2,mu2)
                                  | _ => die "S: unary constructor not functional"
          val (B, t1 as E'.TR(e1', E'.Mus mu1', phi1)) = S(B, arg, false)
          val B = unify_mus(mu1',mus2) B
        in
          retract(B, E'.TR(E'.DECON({con=con, il = il},t1), E'.Mus mu1, 
                           (*Eff.*)mkUnion([phi1,(*Eff.*)mkGet(#2 mu2)])))
        end 
    | E.PRIM(E.EXCONprim excon, []) =>
        let 
          val mu = noSome ((*RSE.*)lookupExcon rse excon) ".S: nullary exception constructor not in RSE"
          (* No effect since unary constructors are simply looked up in the
             environment, as in the Definition, rule 106
          *)
        in
          (B, E'.TR(E'.EXCON(excon,None), E'.Mus [mu], (*Eff.*)mkUnion([])))
        end 
    | E.PRIM(E.EXCONprim excon, [arg]) =>
        (case S(B,arg, false) of 
          (* expression denotes value *)
          (B,t_arg as E'.TR(arg_e, E'.Mus mus, phi_arg)) =>
            let
              val mu = noSome ((*RSE.*)lookupExcon rse excon) ".S: unary exception constructor not in RSE"
            in
                case mu of 
                  (R.FUN(mus1,arreff,mus_result as [(_, rho_result)]),_) =>
                    let                  
                      val B = unify_mus(mus1,mus) B
                      val phi = (*Eff.*)mkPut(rho_result)
                    in
                      (B, E'.TR(E'.EXCON(excon,Some (rho_result,t_arg)), 
                         E'.Mus mus_result, (*Eff.*)mkUnion([phi,phi_arg])))
                    end
                  | _ => die "S: unary exception constructor ill-typed"
           end 
          (* expression denotes frame or failing top-level binding : *)
        | (B,t_arg as E'.TR(arg_e, E'.RaisedExnBind, phi_arg)) => 
             die "S: exception constructor applied to frame or raised Bind exception"
	| _ => die "S(B,PRIM(EXCON...),...)"
       )
    | E.PRIM(E.DEEXCONprim(excon), [arg]) =>
        let 
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (tau, p) = noSome ((*RSE.*)lookupExcon rse excon) "S (DEEXCONprim): exception constructor not in RSE"
          val (mu1,arreff,mus2,mu2) = case tau of R.FUN(mu1,areff, mus2 as [mu2]) => (mu1,areff,mus2,mu2)
                                  | _ => die "S: unary exception constructor not functional"
          val (B, t1 as E'.TR(e1', E'.Mus mu1', phi1)) = S(B, arg, false)
          val B = unify_mus(mu1',mus2) B
        in
          retract(B, E'.TR(E'.DEEXCON(excon,t1), E'.Mus mu1, 
                           (*Eff.*)mkUnion([phi1,(*Eff.*)mkGet(#2 mu2)])))
        end 
    | E.PRIM(E.RECORDprim,args) =>
        let val (B, trips) = List.foldR (fn arg => fn (B, trips) =>
                    let val (B, trip) = S(B,arg, false)
                    in (B, trip::trips)
                    end) (B,[]) args
            val tau = R.RECORD(map (fn E'.TR(_,E'.Mus [mu],_) => mu | _ => die "S.record: boxed arg") trips)
            val (rho,B) = (*Eff.*)freshRhoWithTy(runtype tau, B)
            val phi = (*Eff.*)mkUnion((*Eff.*)mkPut rho :: map (fn E'.TR(_,_,phi) => phi) trips)
        in
          (B, E'.TR(E'.RECORD(rho, trips), E'.Mus [(tau, rho)], phi))
        end
    | E.PRIM(E.SELECTprim i, [arg as E.VAR _]) =>
        let 
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,arg, false)
          val (mus,rho) = case mus1 of [(R.RECORD mus,rho)] => (mus,rho) | _ => die "S (select) : not record type"
          val mu = List.nth i mus handle List.Subscript _ => die "S (select) : select index out of range"
          val phi = (*Eff.*)mkUnion([(*Eff.*)mkGet rho, phi1])
        in
          (B, E'.TR(E'.SELECT(i, t1), E'.Mus [mu], phi))
        end
    | E.PRIM(E.SELECTprim i, [arg]) =>
        let 
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (B, t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,arg, false)
          val (mus,rho) = case mus1 of [(R.RECORD mus,rho)] => (mus,rho) | _ => die "S (select) : not record type"
          val mu = List.nth i mus handle List.Subscript _ => die "S (select) : select index out of range"
          val phi = (*Eff.*)mkUnion([(*Eff.*)mkGet rho, phi1])
        in
          retract(B, E'.TR(E'.SELECT(i, t1), E'.Mus [mu], phi))
        end

    | E.PRIM(E.EQUALprim{instance: E.Type}, [arg1, arg2]) => 

            (*            arg1 => [mu1], phi1  arg2 => [mu2], phi2  rho fresh
                          ---------------------------------------------------
                           arg1 = arg2 => [(bool,rho)], phi

                where phi = {Put rho} u {Get(rho') | rho' in frv(mu1,mu2)}
            *)

        let 
          val B = pushIfNotTopLevel(toplevel,B) (* for retract *)
          val (B,  t1 as E'.TR(e1', E'.Mus mus1, phi1)) = S(B,arg1,false)
          val (B,  t2 as E'.TR(e2', E'.Mus mus2, phi2)) = S(B,arg2,false)
          val (rho,B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
          val mus = [(R.boolType,rho)]
          val phi = (*Eff.*)mkUnion(phi1::phi2::(*Eff.*)mkPut rho :: 
                                 map (*Eff.*)mkGet(List.all (*Eff.*)is_rho (ann_mus (mus1 @ mus2) [])))
          val (mu1,mu2) = case (mus1,mus2) of ([mu1],[mu2]) => (mu1,mu2)
                                            | _ => die "S: ill-typed equality"
        in
          retract(B, E'.TR(E'.EQUAL({mu_of_arg1 = mu1, mu_of_arg2 = mu2, alloc = rho},t1,t2), 
                           E'.Mus mus, phi))
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
	let val B = pushIfNotTopLevel (toplevel, B) (* for retract *)
	    val (mu, B) = freshMu (Type, B)
	    val (sigma, B) = R.sigma_for_c_function tyvars mu B
	    (*much of the rest is analogous to the case for (APP (VAR ..., ...))*)
	    val (B, tau, il) = newInstance (B, sigma, instances)
	in
	  (case tau of
	     R.FUN (mus_a, eps_phi0, [mu_r]) =>
	       let
		 val (B, trs', mus_es, phis) =
		       List.foldR (fn e => fn (B, trs', mus_es, phis) =>
                       let val (B, tr' as E'.TR (_, E'.Mus mus', phi)) = S (B, e, false)
		       in (case mus' of
			     [mu'] => (B, tr' :: trs', mu' :: mus_es, phi :: phis)
			   | _ => die "S: CCALL argument had not precisely one mu")
		       end) (B, [], [], []) es
		 val B = unify_mus (mus_a, mus_es) B
		 val e' = E'.CCALL ({name = name, mu_result = mu_r,
				     rhos_for_result = R.c_function_effects mu_r}, trs')
	       in
		 retract (B, E'.TR (e', E'.Mus [mu_r], Eff.mkUnion (eps_phi0 :: phis)))
	       end
	   | _ => die "CCALL: tau not function type")
	end

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
            val (B, t as E'.TR(e',E'.Mus mus0,_)) = S(B,e0,false)
	    val (fresh_rho,B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
	    val mu = (R.unitType, fresh_rho)
            val phi = (*Eff.*)mkUnion(map (*Eff.*)mkPut(fresh_rho::List.all (*Eff.*)is_rho (ann_mus mus0 [])))
          in
            case e' of
              E'.VAR{il_r as ref il, ...} =>
                 (case  R.un_il (#1 il) of ([],[],[]) => 
                    (B,E'.TR(E'.RESET_REGIONS({force = false, alloc = fresh_rho, regions_for_resetting = []},t), E'.Mus [mu], phi))
                  | _ => crash_resetting false)
            | _ => crash_resetting false
          end 
    | E.PRIM(E.RESET_REGIONSprim{instance = _}, _ ) => crash_resetting false
    | E.PRIM(E.FORCE_RESET_REGIONSprim{instance = _}, [e0 as (E.VAR _)] ) =>
          (*  same as RESET_REGIONSprim, except that "force" is set to true in the result *)
          let 
            val (B, t as E'.TR(e',E'.Mus mus0,_)) = S(B,e0,false)
	    val (fresh_rho,B) = (*Eff.*)freshRhoWithTy(Eff.WORD_RT, B)
	    val mu = (R.unitType, fresh_rho)
            val phi = (*Eff.*)mkUnion(map (*Eff.*)mkPut(fresh_rho::List.all (*Eff.*)is_rho (ann_mus mus0 [])))
          in
            case e' of
              E'.VAR{il_r as ref il, ...} =>
                 (case  R.un_il (#1 il) of ([],[],[]) => 
                    (B,E'.TR(E'.RESET_REGIONS({force = true, alloc = fresh_rho, regions_for_resetting = []},t), E'.Mus [mu], phi))
                  | _ => crash_resetting true)
            | _ => crash_resetting true
          end 
    | E.PRIM(E.FORCE_RESET_REGIONSprim{instance = _}, _ ) => crash_resetting true

    | E.FRAME{declared_lvars, declared_excons} =>
        let 
          val new_declared_lvars' = List.foldR( fn lvar => fn acc => 
                 let val (compound,create_region_record,sigma,p,_,_) = 
		       noSome ((*RSE.*)lookupLvar rse lvar) "declared lvar of frame not in scope"
                 in {lvar=lvar, compound=compound, create_region_record=create_region_record,
		     sigma=ref sigma, place=p} :: acc
                 end) [](map #lvar declared_lvars)
	  val new_declared_lvars = 
	    map (fn {lvar,sigma,place,...} => {lvar=lvar,sigma=sigma,place=place}) new_declared_lvars'
          val new_declared_excons = List.foldR( fn excon => fn acc => 
                 (excon,(*RSE.*)lookupExcon rse excon)::acc) [](map #1 declared_excons)              
        in
          (B,E'.TR(E'.FRAME{declared_lvars = new_declared_lvars, declared_excons = new_declared_excons},
                   E'.Frame{declared_lvars = new_declared_lvars', declared_excons = new_declared_excons},
                   Eff.empty))
        end

   | _ => die "S: unknown expression"
    ) handle 
        Crash.CRASH => die_from_S(e)
      | Bind => die_from_S(e)
      | Match => die_from_S(e)

   and S_built_in(B,lvar, es) =
       S(B, E.APP(E.VAR{lvar = lvar, instances = []}, 
                  E.PRIM(E.UB_RECORDprim, es)),false)
(*
   and S_binop_inline(B,bop,e1,e2,tau_res) : cone * (place,unit)E'.trip = 
     let
       val B = (*Eff.*)push B (* for retract *)
       val (B, t1 as (E'.TR(e1, E'.Mus mus1,phi1))) = S(B, e1, false)
       val (B, t2 as (E'.TR(e2, E'.Mus mus2,phi2))) = S(B, e2, false)
     in
       case (mus1,mus2) of
         ([(tau1,rho1)], [(tau2,rho2)]) =>
          let 
            val (rho3, B) = (*Eff.*)freshRhoWithTy(runtype tau_res,B)
            val phi = (*Eff.*)mkUnion([(*Eff.*)mkGet rho1, (*Eff.*)mkGet rho2, (*Eff.*)mkPut rho3,phi1,phi2])
          in
            retract(B, E'.TR(bop(t1,t2,rho3), E'.Mus [(tau_res,rho3)], phi))
          end
       | _ => die "S_binop_inline: ill-typed binary operator"
     end
*)

  in
    S(B,e,toplevel)
  end (* spreadExp *)

  and spreadFcns(B,rho,retract_level,rse) functions (* each one: (lvar,tyvars,sigma_hat,bind) *) = 
         let
            val occs = map (fn _ => ref []  : (R.il * (R.il * cone -> R.il * cone)) ref list ref) functions
            val rse1 = declareMany(rho,rse)(functions, occs)
            fun spreadRhss(B)[] = (B,[]) 
              | spreadRhss(B)((lvar,tyvars,sigma_hat,bind)::rest) =
                  let 
                    (*val _ = output(std_out, "spreading: " ^ Lvars.pr_lvar lvar ^ "\n")*)
                    val B = (*Eff.*)push(B)
                      val (B, t1 as E'.TR(_, E'.Mus [(tau1, rho1)], phi1)) = spreadExp(B, rse1, bind,false)
                      val B = (*Eff.*)unifyRho(rho1,rho) B          
                      val _ = count_RegEffClos:= !count_RegEffClos + 1
                      val (B,sigma1,msg_opt) = regEffClos(B, retract_level, phi1, tau1)
                      val _ = warn(lvar,msg_opt)
                    val (_,B) = (*Eff.*)pop B (* back to retract level *)
                    
(*                    val _  = log_sigma(R.insert_alphas(tyvars, sigma1), lvar)*)
                    val (B, l) = spreadRhss(B)(rest)
                  in  
                    (B, (t1,tau1,sigma1)::l)
                  end
            val (B, t1_tau1_sigma1_list) = spreadRhss(B)(functions)
            val (rse2, functions') = mkRhs(rse, rho)(functions,t1_tau1_sigma1_list,occs)
         in
            (B, rse2, functions')
         end

  fun spreadPgm(cone, rse: rse,p: E.LambdaPgm): cone * rse * (place,unit)E'.LambdaPgm =
  let
     fun msg(s: string) = (output(std_out, s); NonStandard.flush_out (std_out)) 
     val _ = Eff.algorithm_R:=false
     (*val _ = Eff.trace := []*)
     val E.PGM(datbinds,e) = p
     val _ = if !Flags.chat then msg("\nSpreading datatypes ...") else ()
     val (new_rse, new_datbinds) = (*SpreadDatatype.*)spreadDatbinds rse datbinds cone
     val _ = if !Flags.chat then msg("\nSpreading expression ...") else ()
     val _ = count_RegEffClos := 0
     val (cone',t') = spreadExp (cone,(*RSE.*)plus(rse,new_rse), e,true) 

    (* for toplas submission: 
     val _ = output(!Flags.log, "\nRegEffGen (times called during S)" ^ Int.string (!count_RegEffClos) ^ "\n")
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

end; (* SpreadExpression *)



