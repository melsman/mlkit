
functor RType(structure Flags : FLAGS
              structure Crash : CRASH
              structure E: EFFECT
              structure DiGraph : DIGRAPH
              structure L: LAMBDA_EXP
              structure TyName: TYNAME
                sharing type L.TyName = TyName.TyName 
	      structure RegConst : REG_CONST
			sharing TyName = RegConst.TyName
	      structure FinMap : FINMAP
              structure PP : PRETTYPRINT
                sharing type PP.StringTree = E.StringTree

             ) : RTYPE = 
struct

  structure List = Edlib.List
  structure ListPair = Edlib.ListPair
  fun curry f a b = f(a,b)

  fun say s= TextIO.output(TextIO.stdOut, s ^ "\n");
  fun logsay s= TextIO.output(!Flags.log, s );
  fun log_tree t= PP.outputTree(logsay, t, !Flags.colwidth)
  fun show_rho rho = PP.flatten1(E.layout_effect rho)
  fun show_rhos rhos = List.string show_rho rhos

  fun die (s:string) = Crash.impossible ("RType." ^ s)
  fun pp (stringtree) = PP.flatten1(stringtree)

  fun noSome (SOME v) s = v
    | noSome NONE s = die s

  fun concat_lists l = List.foldR (curry (op @)) [] l

  infix footnote
  fun x footnote y = x
  infix footnote'
  fun x footnote' f = (f x; x)

  type effect = E.effect
  type cone = E.cone
  type delta_phi = E.delta_phi
(*  type tyname_env = TyNameEnv.tyname_env *)
  type LambdaType = L.Type
  type StringTree = PP.StringTree

  type tyvar = L.tyvar
  type tyname = L.TyName
  type place = E.effect
  type arroweffect = E.effect

  datatype Type = 
      TYVAR of tyvar
    | CONSTYPE of tyname * mu list * place list * arroweffect list
    | RECORD of mu list
    | FUN of mu list * arroweffect * mu list

  withtype mu = Type*place

  type runType = E.runType

  (* details of runtype are exploited in SpreadDataType.infer_arity_ty *)

  local open TyName
        fun tyname_unboxed tn =                                     (* NB: unit is also unboxed *)
	  eq(tn, tyName_INT) orelse eq(tn, tyName_BOOL) orelse eq(tn, tyName_LIST)
  in
    fun runtype (CONSTYPE(tn, _, _, _)) =  
      if tyname_unboxed tn then E.WORD_RT
      else if eq(tn, tyName_STRING) orelse eq(tn, tyName_WORD_TABLE) then E.STRING_RT
      else if eq(tn, tyName_REAL) then E.REAL_RT
      else E.TOP_RT
    | runtype (TYVAR _) = E.BOT_RT
    | runtype (RECORD[]) = E.WORD_RT 
    | runtype _ = E.TOP_RT
  end

  fun isWordRegion(rho) = 
        case E.get_place_ty rho of
          SOME E.WORD_RT => true
        | _ => false

  fun isTopWordRegion rho = E.eq_effect(rho,E.toplevel_region_withtype_word)

  fun discard_top_wordrho places = List.all (not o isTopWordRegion) places 
  fun discard_word_rhos places = List.all (not o isWordRegion) places 

  fun ann_ty ty (acc : effect list) = 
       case ty of 
         TYVAR _ => acc
       | CONSTYPE(_,mus,rhos,epss) => 
          List.foldR ann_mu (discard_top_wordrho rhos @ (epss @ acc)) mus
       | RECORD mus =>
          List.foldR ann_mu acc mus
       | FUN(mus1,eps,mus2) =>
          ann_mus mus1 (eps:: ann_mus mus2 acc)
  and ann_mu (tau,rho) acc = ann_ty tau (if isTopWordRegion rho then acc else rho::acc)
  and ann_mus [] acc  = acc
    | ann_mus (mu::rest) acc = ann_mu mu (ann_mus rest acc)

  (* free region variables of mu, including secondary occurrences *)

  fun frv_mu mu = 
      let val annotations = ann_mu mu []
          val all_nodes = E.subgraph annotations
      in
          List.all (fn e => E.is_rho e andalso not(isTopWordRegion e)) all_nodes
      end

  local  (* free primary region and effect variables ("pfrv tau" etc.) *)
    fun pfrv0 ty acc =
         case ty of 
           TYVAR _ => acc
         | CONSTYPE(_,mus,places,_) => List.foldR pfrvMu0 (discard_top_wordrho places @ acc)  mus
         | RECORD mus => List.foldR pfrvMu0 acc mus
         | FUN(mus1,_,mus2) => pfrvMus0 mus1 (pfrvMus0 mus2 acc)
    and pfrvMu0 (tau,rho) acc = pfrv0 tau (if isTopWordRegion rho then acc else rho::acc)
    and pfrvMus0 [] acc = acc
      | pfrvMus0 (mu::rest) acc = pfrvMu0 mu (pfrvMus0 rest acc)

    fun pfev0 ty acc =
         case ty of 
           TYVAR _ => acc
         | CONSTYPE(_,mus,_,arreffs) => List.foldR pfevMu0 (arreffs@acc) mus
         | RECORD mus => List.foldR pfevMu0 acc mus
         | FUN(mus1,arreff,mus2) => pfevMus0 mus1 (arreff::(pfevMus0 mus2 acc))
    and pfevMu0 (tau,rho) acc = pfev0 tau acc
    and pfevMus0 [] acc = acc
      | pfevMus0 (mu::rest) acc= pfevMu0 mu (pfevMus0 rest acc)
  in
    fun pfrv ty = pfrv0 ty []
    fun pfrvMu mu = pfrvMu0 mu []
  
    fun pfev ty = pfev0 ty []
    fun pfevMu mu = pfevMu0 mu []
  end (* local *)

  fun repeat 0 f acc = acc
    | repeat n f acc = 
        repeat (n-1) f (f acc)

  fun freshType(lookup: tyname -> (int*runType list*int)option) : 
                               (L.Type * cone -> Type * cone) 
                             * (L.Type * cone -> mu * cone) =
    let 
      fun mkTy(ty,cone) = case ty of
              L.TYVARtype alpha  => (TYVAR alpha, cone)
            | L.ARROWtype(tys1,tys2)=>
                let val (eps,cone') = E.freshEps(cone)
                    val (cone1,mus1) = List.foldR mkMus (cone',[]) tys1
                    val (cone2,mus2) = List.foldR mkMus (cone1,[]) tys2
                in (FUN(mus1,eps,mus2), cone2) end
            | L.CONStype(tys,tyname)=>
              let val arity as (alpha_count, rhos_runtypes, eps_count) = 
	  	    case lookup tyname
		      of SOME arity => arity
		       | NONE => die ("mkTy: type name " ^ TyName.pr_TyName tyname ^ " not declared")
                  val (cone, mus) = List.foldR mkMus (cone,[]) tys
		  fun repeat2 ([],cone,rhos) = (cone, rev rhos)
		    | repeat2 (rt::rts,cone,rhos) =
		    let val (rho,cone') = E.freshRhoWithTy(rt,cone)
		    in repeat2 (rts,cone',rho::rhos)
		    end 
                  val (cone, rhos) = repeat2(rhos_runtypes,cone,[]) 

(*17/10/96-Martin
                    repeat rho_count (fn (cone, acc: place list) => 
                                      let val (rho,cone') = E.freshRho cone
                                      in (cone', rho::acc) end) (cone,[])
*)
                  val (cone, epss) = 
                    repeat eps_count (fn (cone, acc: arroweffect list) => 
                                      let val (eps,cone') = E.freshEps cone
                                      in (cone', eps::acc) end) (cone,[])
              in
                 (CONSTYPE(tyname,mus,rhos,epss), cone)
              end 
            | L.RECORDtype(tys) => 
                 let val (cone,mus) = List.foldR mkMus (cone,[]) tys
                 in     
                   (RECORD(mus),cone)
                 end
      and mkMu(ty, cone) = 
          let val (tau, cone) = mkTy(ty,cone)
	      val (rho,cone) = E.freshRhoWithTy(runtype tau, cone)
          in
              ((tau,rho),cone)
          end
      and mkMus ty (cone, acc: mu list) = 
        let val (mu,cone') = mkMu(ty,cone)
        in (cone', mu::acc) end
    in
     (mkTy, mkMu)
    end
  
  (* pretty-printing of types *)

  fun leaf s = PP.LEAF s
  fun Node x = PP.NODE x

  fun layout_pair (x,y) = 
        Node{start ="(", finish = ")", indent = 1, 
             childsep = PP.RIGHT",",
             children = [x, y]}

  fun layout_list f [ ] = leaf ""
    | layout_list f l = 
           Node{start = "[", finish = "]", indent = 1, 
                childsep = PP.RIGHT ", ",
                children = map f l}

  fun layout_list' f [ ] = NONE
    | layout_list' f l = SOME (Node{start = "[", finish = "]", indent = 1, 
				    childsep = PP.RIGHT ", ",
				    children = map f l})
              
  fun layout_tuple f [ ] = leaf ""
    | layout_tuple f [x] = f x
    | layout_tuple f l = Node{start = "(", finish = ")", indent = 1, 
			      childsep = PP.RIGHT ", ",
			      children = map f l}
              
  fun layout_tuple' f [ ] = NONE
    | layout_tuple' f [x] = SOME(f x)
    | layout_tuple' f l = SOME(Node{start = "(", finish = ")", indent = 1, 
				    childsep = PP.RIGHT ", ",
				    children = map f l})

  fun layout_tuple'' l = 
    let fun mk_list([],acc) = rev acc
	  | mk_list(SOME t::rest, acc) = mk_list(rest,t::acc) 
	  | mk_list(NONE::rest, acc) = mk_list(rest,acc)
    in layout_tuple' (fn x => x) (mk_list (l,[]))
    end

  fun layout_tyvar alpha = PP.LEAF(L.pr_tyvar alpha)

  fun lay_node n = E.layout_effect_deep n
  fun lay_node_short n = E.layout_effect n

  fun mk_layout omit_region_info =
  let
     fun layout_arrow_rec arreff = 
              if !Flags.print_effects
              then Node{start = "-", finish = "->", 
                   indent = 2, childsep = PP.NOSEP,
                         children = [lay_node arreff]} 
              else leaf "->"

    fun lay_tau_rec parenthesise ty = 
          case ty of
            TYVAR v => layout_tyvar v
          | FUN(mus1,areff,mus2) =>
	      let val children = [layout_arg_res mus1, layout_arrow_rec areff, layout_arg_res mus2]
	      in if parenthesise then
		   Node{start = "(", finish = ")", indent = 1, childsep = PP.NOSEP, children = children}
		 else Node{start="", finish="", indent=1, childsep=PP.NOSEP, children=children}
	      end
          | CONSTYPE(tyname,[],[],[]) => 
              leaf (TyName.pr_TyName tyname)
          | CONSTYPE(tyname,mu_list,place_list,arreff_list) =>
              if omit_region_info 
              then case layout_tuple' (lay_mu_rec true) mu_list
		     of SOME mu_tree =>
		       Node{start = "", finish = "", indent = 0, 
			    childsep = PP.RIGHT " ",
			    children = [mu_tree, leaf (TyName.pr_TyName tyname)]}
		      | NONE => leaf (TyName.pr_TyName tyname)
              else
                let val mu_tree =  layout_tuple' (lay_mu_rec true) mu_list
 	  	    val place_list = if !Flags.print_word_regions then place_list
				     else discard_word_rhos place_list
                    val rho_tree = layout_list' lay_node place_list
                    val effect_tree = layout_list' lay_node arreff_list
                in case layout_tuple'' [mu_tree,rho_tree,effect_tree]
		     of SOME t => Node{start = "", finish = "", indent = 0, 
				       childsep = PP.RIGHT " ",
				       children = [t, leaf (TyName.pr_TyName tyname)]}
		      | NONE => leaf (TyName.pr_TyName tyname)
                end
          | RECORD [] => leaf "unit"
          | RECORD mu_list =>
		if parenthesise then Node{start = "(", finish = ")", childsep = PP.RIGHT "*", indent = 1,
					 children = map (lay_mu_rec true) mu_list}
		else Node{start = "", finish = "", childsep = PP.RIGHT "*", indent = 1,
			  children = map (lay_mu_rec true) mu_list}

    and lay_mu_rec parenthesise (tau,rho)= 
      if omit_region_info orelse (not(!Flags.print_word_regions) 
				  andalso isWordRegion rho) then
	lay_tau_rec parenthesise tau
      else layout_pair (lay_tau_rec false tau, lay_node rho)

    and layout_arg_res [mu] = lay_mu_rec true mu
      | layout_arg_res mus = layout_list (lay_mu_rec true) mus
  in
    (lay_tau_rec false, lay_mu_rec false)
  end;

  (* unification *)

  fun u (node1,node2) cone= 
    if E.is_arrow_effect node1 
      then E.unifyEps(node1,node2) cone
    else E.unifyRho(node1, node2) cone

  fun unify_ty(ty1, ty2:Type) cone: E.cone =
    List.foldL u cone (ListPair.zip(ann_ty ty1 [],ann_ty ty2 []))
    handle _ =>  let val (lay_ty,_) = mk_layout false;
                     fun dump(ty) = PP.outputTree(fn s => TextIO.output(!Flags.log, s), lay_ty ty, !Flags.colwidth)
                 in
                     TextIO.output(!Flags.log, "ty1 = \n"); dump ty1;
                     TextIO.output(!Flags.log, "ty2 = \n"); dump ty2;
                     die "unify: types do not unify"
                 end

  fun unify_mu(mu1, mu2:mu) cone: E.cone =
    List.foldL u cone (ListPair.zip(ann_mu mu1 [],ann_mu mu2 []))
    handle _ => let val (_,lay_mu) = mk_layout false;
                     fun dump(mu) = PP.outputTree(fn s => TextIO.output(!Flags.log, s), lay_mu mu, !Flags.colwidth)
                 in
                     TextIO.output(!Flags.log, "mu1 = \n"); dump mu1;
                     TextIO.output(!Flags.log, "mu2 = \n"); dump mu2;
                     die "unify: types with places do not unify"
                 end
        
  fun unify_mus (mus1, mus2) cone: E.cone = 
     List.foldL unify_mu cone 
       (ListPair.zip(mus1,mus2)) handle ListPair.Zip => 
         die "unify_mus: lists have different lengths"

  (* type schemes: bound variable must be listed in bottom-up 
     depth first search order. No cycles amongst bound effect variables
     are permitted. There can be at most one secondary effect variable.
     There can be at most one secondary region variable pr runtime 
     type. Every bound region or effect variable has in it a field, pix,
     which uniquely identifies its syntactic position in the term.
     Negative pix indicates secondary variable; for region variables,
     each runtime type correpsonds to a distinct negative number.
  *)


  datatype sigma = 
     FORALL of tyvar list * place list * arroweffect list * Type

  fun bv(FORALL(alphas,rhos,epss,_)) = (alphas,rhos,epss)

  fun type_to_scheme tau = FORALL([],[],[],tau)

  fun frv_sigma(FORALL(alphas,rhos,epss,tau)) =
      let val annotations = ann_ty tau []
          val all_nodes = E.subgraph annotations
          val frv_tau = List.all E.is_rho all_nodes
      in
          (* subtract the bound rhos *)
          List.apply (fn bound_rho => E.get_visited bound_rho := true) rhos;
          List.all   (fn free_rho => not(!(E.get_visited free_rho))) frv_tau
            footnote List.apply (fn bound_rho => E.get_visited bound_rho := false) rhos
      end

  (* ferv_sigma(sigma) computes a list of all region and effect
     variables that occur free in sigma *)

  fun ferv_sigma(FORALL(alphas,rhos,epss,tau)): E.effect list =
      let val annotations = ann_ty tau []
          val all_nodes = E.subgraph annotations
          val free = List.all (fn node => E.is_rho node orelse E.is_arrow_effect node) all_nodes
          val bound  = epss @rhos
      in
          (* subtract the bound region and effect variables *)
          List.apply (fn b => E.get_visited b := true) bound;
          List.all   (fn f => not(!(E.get_visited f))) free
            footnote List.apply (fn b => E.get_visited b := false) bound
      end

  fun free_puts(FORALL(alphas,rhos,epss,tau)) =
      let val annotations = ann_ty tau []
          val all_nodes = E.subgraph annotations
          val put_nodes = List.all E.is_put all_nodes
          val rhos_in_put_nodes = map E.rho_of put_nodes
      in
          (* subtract the bound rhos *)
          List.apply (fn bound_rho => E.get_visited bound_rho := true) rhos;
          List.all   (fn free_rho => not(!(E.get_visited free_rho)) andalso not(isTopWordRegion free_rho)) rhos_in_put_nodes
            footnote List.apply (fn bound_rho => E.get_visited bound_rho := false) rhos
      end


  fun insert_alphas(alphas, FORALL(_, rhos,epss,tau)) = 
      FORALL(alphas,rhos,epss,tau)

  fun drop_alphas(FORALL(_, rhos,epss,tau)) = 
      FORALL([],rhos,epss,tau)



  fun mk_lay_sigma_aux (omit_region_info: bool):  
    tyvar list * StringTree list * arroweffect list * Type->  PP.StringTree =
  let 
    val (lay_ty, _) = mk_layout omit_region_info
    fun lay_sig (alphas, rho_trees, epsilons,tau) = 
      (case(alphas,rho_trees, epsilons) of
         ([],[],[]) => if !Flags.print_types then lay_ty(tau) else PP.LEAF ""
       | _ => 
          let val children = 
	          if !Flags.print_effects then 
                    (*print regions and effect and -perhaps- types: *)
		    (if !Flags.print_types then map layout_tyvar alphas
		     else [])  @  rho_trees  @  map lay_node_short epsilons
		  else (if !Flags.print_types then map layout_tyvar alphas
			else [])  @  (if !Flags.print_regions then  rho_trees
				      else [])
	    
  	      val binders = PP.HNODE{start="",finish="",childsep=PP.RIGHT ",",
                                     children=children}
              
          in if !Flags.print_types 
             then 
               Node{start = "all ", finish = "", indent = 3, 
                    childsep = PP.RIGHT ".", 
                    children = [binders,lay_ty tau]}
             else (case children
		     of [] => PP.LEAF ""
		      | _ => if !Flags.print_regions orelse 
                                !Flags.print_effects
                               then Node{start = "[", finish = "]", 
                                         indent = 1, childsep = PP.NOSEP, 
					 children = [binders]}
			     else binders)
          end
      )
  in
    lay_sig
  end

  fun mk_lay_sigma omit_region_info  =
      let val f = mk_lay_sigma_aux omit_region_info 
      in fn (FORALL (raw as(alphas,rhos,epss,tau))) => 
              f(alphas, map lay_node rhos, epss, tau)
      end

  

  fun mk_lay_sigma' (omit_region_info: bool) (tyvars,rhos,epss,tau): PP.StringTree =
      mk_lay_sigma(omit_region_info)(FORALL(tyvars,rhos,epss,tau))
  
  fun mk_lay_sigma'' (lay_bind: 'b -> StringTree option) omit_region_info  =
      let val f = mk_lay_sigma_aux omit_region_info 
      in fn (alphas,rhos,epss,tau) => 
             let val ts = List.foldR (fn rho => fn acc => case lay_bind rho of 
                                          SOME t => t::acc | _ => acc) [] rhos
             in f(alphas, ts, epss, tau)
             end
      end
  
  (* maybe_increase_runtype(mu as (tau,rho)) increases the runType of rho to 
     be the least upper bound of its current runType and the runType of tau.
     This must be done during instantiation of type schemes when (tau,rho)
     is the result of instantiating (alpha',rho') where rho' has runtype BOT_RT *)

  fun maybe_increase_runtype(mu as (tau,rho)) =
    let val old = case E.get_place_ty rho of SOME old => old | _ => die "maybe_increase_runtype"
      val new = runtype tau
    in if old<>new then E.setRunType rho (E.lub_runType(old,new)) else ();
      mu
    end


  (* instantiation of type schemes *)

  (* inst: sigma*il -> cone -> (Type * cone)

     let (tau,c') = inst(sigma,c)
     Then tau is the result of instantiating sigma via il.
     Only the part of the body that has to be copied is copied (when
     a sub-term of the body of sigma does not contain bound variables it
     is traversed, but not copied).
  *)

  type il = Type list * effect list * effect list (* instantiation lists *)

  fun mk_il(taus,places,effects) = (taus,places,effects)
  fun un_il(taus,places,effects) = (taus,places,effects)

  fun instAux(S as (St,Sr,Se),tau) cone = 
        let
          (* debugging 

          val _ = logsay("instAux enter, tau = \n");
          val (lay_ty, _) = mk_layout false
          val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
          val _ = logsay ("\nSr = " ^ concat (map (fn (rho, rho') => show_rho rho ^ "->" ^show_rho rho' ^ ",") Sr))
          *)
          val find = E.find

          val _ = List.apply E.setInstance Sr
          val _ = List.apply E.setInstance Se

          fun fst(x,y) = x

          fun cp_var node = 
            case (E.get_instance node) of
               ref(SOME node') => (true,node')
            | _ => (false,node)

          fun applySt([],alpha) = NONE
            | applySt((alpha',ty)::rest, alpha) = 
                if alpha=alpha' then SOME ty 
                else applySt(rest,alpha)

         (* cp: copy as much of the body of the type scheme as is necessary *)

          fun cp_rho rho = cp_var rho

          fun cp_eps eps = cp_var eps

          fun cp_ty ty  = case ty of
                  TYVAR alpha => (case applySt(#1 S, alpha) of 
                                   NONE => (false,ty)
                                 | SOME ty' => (true, ty'))
                | RECORD mus => 
                    let val l = map cp_mu mus
                    in  if List.exists fst l 
                        then (true, RECORD(map #2 l))
                        else (false, ty)
                    end
                | CONSTYPE(tyname,mus,aux_places,aux_arreffs) => 
                    let val l1 = map cp_mu mus
                        val (b1, mus1) = if List.exists fst l1 
                                           then (true, map #2 l1)
                                         else (false, mus)
                        val l2 = map cp_rho aux_places
                        val (b2, aux_places1) = 
                          if List.exists fst l2 
                            then (true, map #2 l2)
                          else (false, aux_places)
                        val l3 = map cp_eps aux_arreffs
                        val (b3, aux_arreffs1) = 
                          if List.exists fst l3 
                            then (true, map #2 l3)
                          else (false, aux_arreffs)
                    in                      
                      if b1 orelse b2 orelse b3 
                        then (true, 
                              CONSTYPE(tyname,mus1,aux_places1,aux_arreffs1))
                      else (false, ty)
                    end
            | FUN(mus1,arreff,mus3) =>
                let val l1 = map cp_mu mus1
                    val (b1, mus1') = if List.exists fst l1
                                      then (true, map #2 l1)
                                      else (false, mus1)
                    val (b2, arreff') = cp_eps arreff
                    val l3 = map cp_mu mus3
                    val (b3, mus3') = if List.exists fst l3
                                      then (true, map #2 l3)
                                      else (false, mus3)
                in
                    if b1 orelse b2 orelse b3 
                      then (true, FUN(mus1',arreff',mus3')) 
                    else (false, ty)
                end
                
          and cp_mu (mu as (tau,rho))  =
                let val (chng1, tau1) = cp_ty tau
                    val (chng2, rho2) = cp_rho rho
                in
                    if chng1 orelse chng2 
                      then case tau of
                             TYVAR _ => (true, maybe_increase_runtype(tau1,rho2))
                           | _ =>(true,(tau1, rho2)) 
                    else (false,mu)
                end



            val Ty = #2(cp_ty tau)
                                  (* this is where arrow effects are instantiated*)
            val (cone, updates) = E.instNodesClever (#2 S @ #3 S) cone

            val _ = List.apply E.clearInstance Sr
            val _ = List.apply E.clearInstance Se

            (*val _ = footnote Profile.profileOff()*)
        in
           (Ty,cone,updates)
        end

  fun ann_sigma(FORALL(_,_,_,ty)) : effect list -> effect list = ann_ty ty


  (* update_runtypes(actuals,formals) -> unit:
   * make sure runtypes of actuals >= runtypes of formals.
   *)

  fun runtype_place place = case E.get_place_ty place
			      of SOME rt => rt
			       | NONE => die "runtype_place"

  fun update_runtypes([],[]) = ()
    | update_runtypes(rho_a::actuals,rho_f::formals) =
        let val rt_a = runtype_place rho_a
	    val rt_lub = E.lub_runType(rt_a,runtype_place rho_f)
            val _ = if rt_lub = rt_a then ()
		    else E.setRunType rho_a rt_lub
	in update_runtypes(actuals,formals)
	end
    | update_runtypes _ = die "update_runtypes"

  fun unify_with_toplevel_wordregion (cone, rhos) =
    let val rhos = List.all isWordRegion rhos
    in foldl (fn (rho, cone) => 
	      E.unifyRho(E.toplevel_region_withtype_word, rho) cone) 
      cone rhos
    end
    
  fun instClever(FORALL([],[],[],tau),il) cone = (tau, cone,[])
    | instClever(FORALL(alphas,rhos,epsilons,tau),
           il as (types,places,arreffs)) cone =
        let 
          (*val _ = Profile.profileOn();*)
          val find = E.find
          val rhos = map find rhos  and epsilons = map find epsilons 
          and places = map find places and arreffs = map find arreffs

	  (* set types of places according to rhos *)
	  val _ = update_runtypes(places, rhos)

          val S = (ListPair.zip(alphas,types),
                   ListPair.zip(rhos,places),
                   ListPair.zip(epsilons,arreffs))
                  handle _ => die "inst: type scheme and \
                                 \instantiation list have different arity"
	  val (Ty,cone,updates) = instAux(S, tau) cone
(*	  val cone = unify_with_toplevel_wordregion (cone, places) *)
        in (Ty,cone,updates)
        end

  fun inst sigma_il cone = 
      let val (a,cone,c) = instClever sigma_il cone
	  val places = map E.find (#2(#2(sigma_il)))
	  val cone = unify_with_toplevel_wordregion (cone, places)
      in (a,cone)
      end

  (* generalisation: RegEffClos *)

  fun warn effects =  case effects of
        [] => NONE
      |  _ => SOME("regEffClos: escaping from generalisation: " 
                   ^ List.string (pp o E.layout_effect) effects ^ "\n")
                  
   fun combine_messages(NONE, msg2) = msg2
     | combine_messages(msg1, NONE) = msg1
     | combine_messages(SOME s1, SOME s2) = SOME(s1 ^ s2)
      

  fun potentially_generalisable n effect =
      noSome (E.level_of effect) ".potentially_generalisable: not variable"
      > n


  fun tickSomeBound() = ()
  fun tickNoBound() = ()

  fun checkSigma(sigma as FORALL([],[],[],tau)) = (tickNoBound(); sigma)
    | checkSigma sigma = (tickSomeBound(); sigma)

  exception MONOMORPHIC of E.cone * sigma * string option

  fun visit node = E.get_visited node := true
  fun unvisit node = E.get_visited node := false

  fun unify_generic_secondary_epss(cone,n,reachable_nodes, principal_nodes): E.cone = 
      (List.apply visit principal_nodes;
       let val secondary_epss = List.foldL (fn reachable_node => fn acc =>
                if E.is_arrow_effect reachable_node then
                   if !(E.get_visited reachable_node) then (* primary *) acc
                   else if potentially_generalisable n reachable_node
                        then reachable_node :: acc
                        else acc
                else acc) [] reachable_nodes
       in
          List.apply unvisit principal_nodes;

          case secondary_epss of 
            [] => cone
          | (x::xs) => List.foldL (fn eps => fn cone => E.unifyEps(eps,x) cone) cone xs
       end)

  (* partition_rhos rhos partitions rhos into region variables that have the
     same runtime type *)

  fun skey rho = case E.get_place_ty rho of SOME rt => E.ord_runType rt | _ => die "skey"

  fun partition_rhos (rhos:place list): place list list =
      let val sorted_rhos = ListSort.sort (fn rho1 => fn rho2=> skey rho1 <= skey rho2) rhos
          fun runs [] = []
            | runs (x::xs) = 
                let val (run1, rest)= List.splitFirst (fn rho => skey rho <> skey x) xs
                                      handle _ => (xs, [])
                in (x::run1) :: runs rest
                end
      in runs sorted_rhos
      end

  fun unifyRhos(rhos as [], cone): place * cone = 
               die ".unifyRhos applied to empty list of region variables"
    | unifyRhos(rho::rhos, cone) = 
               (rho, List.foldL (fn rho' => fn cone => 
                                 E.unifyRho(rho',rho) cone) cone rhos)

  fun unify_rho_partition(cone0, partition: place list list): place list * E.cone = 
      List.foldR (fn (l : place list) => fn (representatives,cone) =>
                  let val (rho', cone) = unifyRhos(l, cone)
                  in (rho' :: representatives, cone) end
                 )([], cone0) partition

  fun unifyEpss(epss as [], cone): place * cone = 
               die ".unifyEpss applied to empty list of effect variables"
    | unifyEpss(eps::epss, cone) = 
               (eps, List.foldL (fn eps' => fn cone => 
                                 E.unifyEps(eps',eps) cone) cone epss)

  fun unify_eps_partition(cone0, partition: place list list): effect list * E.cone = 
      List.foldR (fn (l : place list) => fn (representatives,cone) =>
                  let val (eps', cone) = unifyEpss(l, cone)
                  in (eps' :: representatives, cone) end
                 )([], cone0) partition

  


  (* set_pix_primary(bound_primary, tau_nodes_in_fixed_order) assigns the pix field
     of each member of bound_primary to the leftmost position in tau_nodes_in_fixed_order at
     which the member occurs. *)

  fun set_pix_primary (bound_primary: E.effect list, tau_nodes_in_fixed_order: E.effect list) :unit =
      let fun loop ([], _) = ()
            | loop (candidate::rest, n) = 
                    let val r = E.get_visited candidate
                    in  if !r then (r:= false; 
                                    E.pix candidate := n;   (* set the pre-order index of the bound variable 
                                                               (only done here) *)
                                    loop(rest, n+1))
                        else loop(rest, n+1)
                    end
      in
         List.apply visit bound_primary;
         loop(tau_nodes_in_fixed_order, 0)
      end

  fun set_pix_of_secondary_epss [] = ()
    | set_pix_of_secondary_epss [eps] = E.pix eps:= ~15
    | set_pix_of_secondary_epss _ = 
          die "set_pix_of_secondary_epss: there was only supposed \
             \to be one secondary generalisable effect variable left"

  fun set_pix_of_secondary_rhos rhos : unit= 
       List.apply (fn rho => (E.pix rho := skey rho * ~10)) rhos


  fun regEffClos(B: E.cone, B_0: int, phi: E.effect, tau: Type): 
                                    E.cone * sigma * string option =
  let
     (*val _ = Profile.profileOn()*)
     (* debugging
     val _ = logsay("regEffClos enter, tau = \n");
     val (lay_ty, _) = mk_layout false
     val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
     *)
     val n = B_0
     val B_1 = E.lower B_0 phi B
     val annotations = (ann_ty tau [])
     (* if there are no potentially generalisable nodes, we can escape right away,
        without going into the expensive operation of contracting effects *)
     val _ = if List.exists (potentially_generalisable n) annotations 
             then () else raise MONOMORPHIC(B_1,FORALL([],[],[],tau),NONE)

     (* make sure there is at most one generalisable secondary effect variable *)
     val reachable_nodes = E.subgraph annotations
     val B_2 = unify_generic_secondary_epss(B_1,n,reachable_nodes, annotations)

     val subgraph = E.contract_effects annotations
                    (* nodes in "subgraph" are listed in bottom-up order, without
                       duplicates *)
     val frv_tau = List.all E.is_rho subgraph  (* no duplicates in frv_tau *)
     val pfrv_tau = pfrv tau  (* syntactic order *)
     val problematic_secondary_frv_tau =  (* no duplicates *)
               List.all (potentially_generalisable n)
                        (E.setminus(frv_tau, pfrv_tau))

     val (bound_secondary_rhos, B_3) = unify_rho_partition(B_2, 
                                           partition_rhos problematic_secondary_frv_tau)
     val _ = set_pix_of_secondary_rhos bound_secondary_rhos

     val primary_bound_rhos = E.remove_duplicates(List.all (potentially_generalisable n) pfrv_tau)
     val _ = set_pix_primary(primary_bound_rhos, pfrv_tau)
     val bound_rhos = bound_secondary_rhos @ primary_bound_rhos

     val fev_tau = List.all E.is_arrow_effect subgraph (* bottom-up order, no duplicates *)
     val pfev_tau = pfev tau      (* syntactic order *)
     val problematic_secondary_fev_tau =  List.all (potentially_generalisable n)
                                                (E.setminus(fev_tau,pfev_tau))
     val _ = set_pix_of_secondary_epss problematic_secondary_fev_tau

     val bound_epss = List.all (potentially_generalisable n) fev_tau (* bottom-up order *)
     val _ = set_pix_primary(E.setminus(bound_epss,problematic_secondary_fev_tau), pfev_tau)
     val sigma = FORALL([], bound_rhos, bound_epss, tau)

     (* debugging
     val _ = logsay("regEffClos leave, sigma = \n");
     val lay_sigma = mk_lay_sigma false
     val _ = PP.outputTree(logsay,lay_sigma sigma,!Flags.colwidth)
     *)
  in
    (B_3, sigma, NONE) (*footnote Profile.profileOff()*)
  end handle MONOMORPHIC result => result;

  fun effClos(B: E.cone, B_0: int, phi: E.effect, tau: Type): 
                                    E.cone * sigma * string option = die "effClos not implemented"

  fun generalize_all(cone, level: int, alphas, tau): cone * sigma * string option =
      let val (cone,sigma, msg) = regEffClos(cone,level,E.empty,tau)
      in (cone, insert_alphas(alphas,sigma), msg)
      end


  (* alpha_rename(sigma, B) ->  sigma
     sigma' = alpha_rename(sigma, B):
     B is a cone covering the free part of sigma (but not the bound part): level B = level sigma
     sigma' is a version of sigma which uses fresh bound region and effect variables
  *)

  fun alpha_rename(sigma, B: E.cone): sigma = 
    let val FORALL(alphas,rhos,epss,tau) = sigma
        val c = E.push B
        val (rhos', c) = E.renameRhos(rhos,c)
        val (epss', c) = E.renameEpss(epss,c)
        val (tau',c) = inst (FORALL([],rhos,epss,tau),([],rhos',epss')) c
        val sigma' = FORALL([], rhos', epss', tau')
        val (_, c) = E.pop c
    in
        sigma'
    end

  fun alpha_rename'((rhos,epss,tau), B: E.cone): sigma = 
    let 
        val c = E.push B
        val (rhos', c) = E.renameRhos(rhos,c)
        val (epss', c) = E.renameEpss(epss,c)
        val (tau',c) = inst (FORALL([],rhos,epss,tau),([],rhos',epss')) c
        val sigma' = FORALL([], rhos', epss', tau')
        val (_, c) = E.pop c
    in
        sigma'
    end

  (* normalised type schemes: bound region and effect variables
     annotated with positions indicating where in the type they
     are first bound *)

  (*  alpha_equal: sigma * sigma -> bool
      alpha_equal(sigma1,sigma2) returns true if sigma1 and sigma2 are equal
      up to renaming of bound region and effect variables. 
      Type variables are not checked. Also the bound region variables (and effect variables)
      of sigma1 and sigma2 are assumed to be disjoint. *)

  fun pair_pix node = (node, !(E.pix node))

  fun intsort l = ListSort.sort (fn i: int => fn j: int => i<=j) l
  fun show_int_list (l:int list)  = concat (map (fn i => " " ^ Int.toString i)l)

  fun tell_int_list msg (l: int list) = 
      (logsay(msg ^ show_int_list l ^ "\n");  l)

  fun layout_sigma sigma = mk_lay_sigma false sigma

  fun alpha_equal(sigma1 as FORALL(_,rhos1,epsilons1,tau1),
                  sigma2 as FORALL(_,rhos2,epsilons2,tau2)) cone = 
      
      let val cone = E.push cone
        (*val _ = logsay "enter alpha_equal\n"
          val _ = logsay "sigma1=\n"
          val _ = PP.outputTree(logsay,layout_sigma sigma1,!Flags.colwidth)
          val _ = logsay "sigma2=\n"
          val _ = PP.outputTree(logsay,layout_sigma sigma2,!Flags.colwidth)
        *)
          val rhos_and_ints1 = map pair_pix rhos1
          val rhos_and_ints2 = map pair_pix rhos2
          val epsilons_and_ints1 = map pair_pix epsilons1  val eps_indices = map #2 epsilons_and_ints1
          val epsilons_and_ints2 = map pair_pix epsilons2
      in
         (map #2 rhos_and_ints1  = map #2 rhos_and_ints2  (* int list equality: bound region variables 
                                    occur in the same syntactic positions in sigma1 and sigma2 *) 
          andalso 
          ((* logsay "quantification of places in same positions\n";*)
          (*tell_int_list "eps1_indices"*) (intsort(eps_indices))  = 
          (*tell_int_list "eps2_indices"*) (intsort(map #2 epsilons_and_ints2))  (* int list equality *) )
          andalso 
          let
            (*val _ = logsay "same positions of bound effect vars\n"*)
            val (fresh_rhos, cone) = E.freshRhos(rhos1, cone)
            val (fresh_epss', cone) = E.freshEpss(epsilons1,cone)
            val fresh_epss'_with_ints = ListPair.zip(fresh_epss', eps_indices)
            val (fresh_epss'', cone) = E.freshEpss(epsilons1,cone)
            val fresh_epss''_with_ints = ListPair.zip(fresh_epss'', eps_indices)
            val (fresh_rhos_of_epss, cone) = E.freshRhos(epsilons1, cone)
            val _ = List.apply (fn (eps, rho) => E.edge(eps, E.mkPut rho))
                               (ListPair.zip(fresh_epss',fresh_rhos_of_epss))
            val _ = List.apply (fn (eps, rho) => E.edge(eps, E.mkPut rho))
                               (ListPair.zip(fresh_epss'',fresh_rhos_of_epss))
            val Se' = map (fn (bound_eps,ix) => 
                            (bound_eps, #1(List.first (fn (new_eps,ix') => ix=ix') fresh_epss'_with_ints))
                          ) 
                          epsilons_and_ints1
                          
            val (tau', cone, updates) = 
                 instAux(([],ListPair.zip(rhos1,fresh_rhos),Se'),tau1) cone
                   handle x => (say "first call\n"; 
                      List.apply (fn node => say(PP.flatten1(E.layout_effect node))) 
                                 (rhos1 @ epsilons1); raise x)

            val Se'' = map (fn (bound_eps,ix) => 
                            (bound_eps, #1(List.first (fn (new_eps,ix') => ix=ix') fresh_epss''_with_ints))
                          ) 
                          epsilons_and_ints2
                          
            val (tau'', cone, updates) = 
                 instAux(([],ListPair.zip(rhos2,fresh_rhos), Se''),tau2) cone
                   handle x => (say "second call\n";
                      List.apply (fn node => say(PP.flatten1(E.layout_effect node))) 
                                 (rhos2 @ epsilons2); raise x) 
          in
            (List.forAll E.eq_effect
                        (ListPair.zip(E.remove_duplicates(pfrv tau'),
                                      E.remove_duplicates(pfrv tau'')))
             )
            andalso
            ((*logsay "regions correspond\n";*)
             List.forAll E.sameEffect
                        (ListPair.zip(E.remove_duplicates(pfev tau'),
                                      E.remove_duplicates(pfev tau'')))
             )
          end
         ) footnote' (fn b => ((*logsay ("leave alpha_equal: " ^ Bool.string b ^ "\n");*) E.pop cone))
      end handle ListPair.Zip => ((*logsay "leave alpha_equal: false\n";*) false (*footnote say "zip raised"*) )

  (**************************)
  (* Matching type schemes  *)
  (**************************)


  exception FAIL_MATCH of string

  (* (f: il * cone -> il * cone) = mk_transformer(origins: int list list * int list list):
     f is a function which selects and unifies members of its argument
     instantiation list, according to the partitioning given by origins.
  *)

  fun select_and_unify(oldvars: '_var list, origins, unify, cone): '_var list * cone =
    let 
       val a = Array.fromList oldvars
       val var_classes = map (map (fn ix => Array.sub(a, ix))) origins
    in
       unify(cone, var_classes)
    end handle _ => raise FAIL_MATCH "select_and_unify"

  fun mk_transformer(origins as (rho_origins: int list list, eps_origins: int list list))
                    ((taus, old_rhos: place list, old_epss: effect list), cone) : il * cone =
    let
      val (new_rhos, cone) = select_and_unify(old_rhos,rho_origins,unify_rho_partition,cone)
      val (new_epss, cone) = select_and_unify(old_epss, eps_origins, unify_eps_partition, cone)
    in
      ((taus,new_rhos,new_epss), cone)
    end
 
  (* l:int list = find_origin(vars : effect list)(var': effect)
     Here l is a list of indices i in vars for which 
         find(nth i vars) = find var'
     vars is a list of bound variables of an (old, more general) type
     scheme while var' is a bound variable of a (new, less general) type
     scheme. *)

  fun find_origin vars var' = 
    let 
       fun search(ix, vars as [], acc) = (acc:int list)
         | search(ix, var::vars,acc) = 
             if E.eq_effect(var, var')  (* intuitively: var was mapped to var' by the
                                            instantiating substitution *)
             then search(ix+1,vars,ix::acc)
             else search(ix+1,vars,acc)
    in
       search(0,vars,[])
    end

  (* vars2 = select_empty(origins, vars1) 
     Assumption: origins and vars have the same length.
     vars2 are those variables amongst vars1 whose partner
     in origins is empty. *)

  fun select_empty([]::rest,rho::rhos)= rho::select_empty(rest,rhos)
    | select_empty(_::rest,_::rhos) = select_empty(rest,rhos)
    | select_empty _ = []

  (* 
     enumerate(l)  = [[0], [1], ...., [length(l)]]
  *)

  fun enumerate l =
    let
       fun loop(ix, []) = []
         | loop(ix, _ :: xs) = [ix] :: loop(ix+1,xs)
    in 
       loop(0, l)
    end

  (* (transformer: il * cone -> il * cone) = matchSchemes(sigma, sigma') 
     Assumption: sigma >= sigma' via a substitution S which is 
     represented implicitly be links in the union-find data structure
     which implements region and effect variables. *)
  
  fun add (i: int) (j: int) = i+j

  fun merge([], l2) = l2
    | merge(l::ls, [] :: l2) = l :: merge(ls, l2)
    | merge(l::ls, ixs :: l2) = ixs :: merge(l::ls,l2)
    | merge _ = die ".merge: ill-formed merge"

  fun fail_aux(sigma,sigma'): unit =
          (logsay "MatchSchemes: matching of type schmes failed\n";
           logsay "  the supposedly more general type scheme :\n    ";
           log_tree (layout_sigma(sigma)); logsay "\n";
           logsay "  the supposedly less general type scheme :\n    ";
           log_tree (layout_sigma(sigma')); logsay "\n")

  fun failwith(x,sigma,sigma') = 
   (fail_aux(sigma,sigma');
    raise x
   )

  fun matchSchemes(sigma as FORALL([], rhos, epss,tau),
            sigma' as FORALL([], rhos', epss', tau')) :
                                 (il * cone) -> (il * cone) =
   (
    let
      (* debugging 
     val lay_sigma = mk_lay_sigma false
     val _ = logsay("matchSchemes enter, sigma = \n");
     val _ = PP.outputTree(logsay,lay_sigma sigma,!Flags.colwidth)
     val _ = logsay("\nmatchSchemes enter, sigma' = \n");
     val _ = PP.outputTree(logsay,lay_sigma sigma',!Flags.colwidth)
     *)
      val rhos'_origins = map (find_origin rhos) rhos'
      val add_rhos = select_empty(rhos'_origins, rhos')

      (* val _ = logsay("\nadd_rhos = " ^ show_rhos add_rhos ^ "\n");   debugging*)

      val rhos'_origins_extended = 
             case add_rhos of
               [] => (* common special case: *) rhos'_origins
             | _ =>  merge(enumerate add_rhos,  
                           map (map(add(List.size add_rhos))) rhos'_origins)

      val epss'_origins = map (find_origin epss) epss'
      val add_epss = select_empty(epss'_origins, epss')
      val epss'_origins_extended = 
             case add_epss of 
               [] => (* common special case: *) epss'_origins
             | _  => merge(enumerate add_epss, 
                           map (map(add(List.size add_epss))) epss'_origins)

      val thin = mk_transformer(rhos'_origins_extended, epss'_origins_extended)
    in
      fn ((old_taus, old_rhos, old_epss), cone) =>
         (let val (new_rhos, cone) = E.cloneRhos(add_rhos, cone)
              val (new_epss, cone) = E.cloneEpss(add_epss, cone)
          in
              thin ((old_taus,new_rhos@old_rhos, new_epss @ old_epss), cone)
          end handle x => failwith (x,sigma,sigma'))
    end  handle x => failwith(x,sigma,sigma')
    )
  | matchSchemes _ = raise FAIL_MATCH "matchSchemes: type scheme had bound type variables"


  val exnType: Type = CONSTYPE(TyName.tyName_EXN,[],[],[])  
  val intType: Type = CONSTYPE(TyName.tyName_INT,[],[],[])  
  val boolType: Type = CONSTYPE(TyName.tyName_BOOL,[],[],[])  
  val realType: Type = CONSTYPE(TyName.tyName_REAL,[],[],[])  
  val stringType: Type = CONSTYPE(TyName.tyName_STRING,[],[],[])  
  val unitType: Type = RECORD[]


  (*the following two functions are only used when spreading ccalls (in
   SpreadExpression---see also the comment there):

   sigma_for_c_function tyvars mu B = a region type scheme corresponding to
   the ML type scheme that was freshMu'ed to get mu and has bound tyvars
   `tyvars'.

   c_function_effects mu = the `rhos_for_result' to be annotated on a ccall
   with return type-and-place mu; see comment in MUL_EXP.*)


  (*unify_rhos_on_same_tyvars mu = for each pair of occurences of
   (tyvar, rho1) & (tyvar, rho2), unify rho1 & rho2.  To do this, an
   environment ttr maps a tyvar that has been seen before to the rho
   it was seen with.*)

  fun unify_rhos_on_same_tyvars mu B = #2 (unify_rhos_on_same_tyvars0 mu
        (FinMap.empty : (tyvar, place) FinMap.map, B : cone))
  and unify_rhos_on_same_tyvars0 (tau, rho) (ttr, B) =
        (case tau of
  	   TYVAR tyvar =>
  	     (case FinMap.lookup ttr tyvar of
  		SOME rho' => (ttr, E.unifyRho (rho, rho') B)
  	      | NONE =>      (FinMap.add (tyvar, rho, ttr), B))
  	 | CONSTYPE (tyname, mus, _, _) =>
  	     unify_rhos_on_same_tyvars00 mus (ttr, B)
  	 | RECORD mus => unify_rhos_on_same_tyvars00 mus (ttr, B)
  	 | FUN (mus1, _, mus2) =>
  	     unify_rhos_on_same_tyvars00 mus1
  	     (unify_rhos_on_same_tyvars00 mus2 (ttr, B)))
  and unify_rhos_on_same_tyvars00 mus (ttr, B) =
        List.foldL unify_rhos_on_same_tyvars0 (ttr, B) mus
  
  (*c_function_effects mus = the `rhos_for_result' to be annotated on
   a ccall; see comment in MUL_EXP.*)
       
  fun c_function_effects mu : (place * int option) list =
        c_function_effects1 no mu
  and c_function_effects1 in_list (tau, rho) =
        (case tau of
  	   TYVAR tyvar => []
  	 | CONSTYPE (tyname, mus, rhos, epss) =>
  	     if TyName.eq (tyname, TyName.tyName_LIST) then
  	       (case (mus, rhos) of
  		  ([mu1], [rho1]) =>
  		    (*rho is for cons cells & rho1 is for auxiliary pairs*)
  		    [(rho, NONE), (rho1, NONE)] @ c_function_effects1 yes mu1
  		| _ => die "c_function_effects1: strange list type")
  	     else [(rho, in_list (size_of_tyname tyname))]
  	 | RECORD [] => [(rho, SOME 0)] (*unit is not allocated*)
  	 | RECORD mus =>
  	     (rho, in_list (SOME (RegConst.size_of_record mus)))
  	     :: concat_lists (map (c_function_effects1 in_list) mus)
  	     (*it is assumed that concat_lists does not concat the lists in
  	      opposite order, i.e., that concat_list [[1,2], [3], [4]] is
  	      [1,2,3,4] and not [4,3,1,2]*)
  	 | FUN (mus, eps0, mus') => die "c_function_effects1 (FUN ...)")
  and yes i_opt = NONE (*i.e., `yes, we are below a list constructor'*)
  and no i_opt = i_opt (*i.e., `no, we are not below a list constructor'*)
  and size_of_tyname tyname =
        if TyName.eq (tyname, TyName.tyName_REAL)
  	then SOME (RegConst.size_of_real ())
  	else if TyName.eq (tyname, TyName.tyName_STRING)
	 orelse TyName.eq (tyname, TyName.tyName_WORD_TABLE) then NONE
        else if RegConst.unboxed_tyname tyname then SOME 0
  	else die ("S (CCALL ...): \nI am sorry, but c functions returning "
  		  ^ TyName.pr_TyName tyname
  		  ^ " are not supported yet.\n")
  
  fun frv_except_tyvar_rhos mus =
        E.remove_duplicates (frv_except_tyvar_rhos0 mus)
  and frv_except_tyvar_rhos0 mus =
        concat_lists (map frv_except_tyvar_rhos1 mus)
  and frv_except_tyvar_rhos1 (tau, rho) =
        (case tau of
  	   TYVAR tyvar => []
  	 | CONSTYPE (tyname, mus, rhos, epss) =>
  	     rho :: epss @ rhos @ frv_except_tyvar_rhos0 mus
  	 | RECORD mus =>
  	     rho :: frv_except_tyvar_rhos0 mus
  	 | FUN (mus, eps0, mus') => die "frv_except_tyvar_rhos1")
  
  fun sigma_for_c_function tyvars mu B =
        let val B = unify_rhos_on_same_tyvars mu B
  	in
  	  (case mu of
  	     (FUN (mus1, eps0, mus2), rho) =>
  	       let val rhos_get = frv_except_tyvar_rhos mus1
  		   val rhos_put = frv_except_tyvar_rhos mus2
  		   val phi0 = E.mkUnion (map E.mkGet rhos_get
  					 @ map E.mkPut rhos_put)
  	       in
  		 E.edge (eps0, phi0) ; (*insert effect phi0 on the arrow in mu*)
  		 let val (B, sigma, msg_opt) = generalize_all (B, 0, tyvars, #1 mu)
  		 in (sigma, B)
  		 end
  	       end
  	   | _ => die "sigma_for_c_function")
	end


end; (* RType ends here *)

(*
functor TestRType() =
struct
(*$TestRType: Crash PrettyPrint Flags DiGraph Effect Lvars LambdaExp TyName 
              Ident Con Excon Report TyCon Timestamp
              BasicIO  Stack UnionFindPoly StrId RType*)


structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Flags = Flags(structure Crash = Crash);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
                           structure Crash = Crash
                           structure Flags = Flags);
structure UF = UF_with_path_halving_and_union_by_rank();
structure Stack = Stack();
structure DiGraph = DiGraph(structure UF = UF
                            structure Stack = Stack
                            structure PP = PP
                            structure Flags = Flags
                            structure Crash = Crash)

structure Effect = Effect(structure G = DiGraph
                          structure PP = PP
                          structure Crash = Crash
                          structure Report = Report);

structure Timestamp = Timestamp()
structure StrId = StrId(structure Timestamp= Timestamp
                        structure Crash= Crash
                          )
structure Ident = Ident(structure StrId = StrId
                        structure Crash = Crash)
structure Con = Con(structure Ident = Ident)
structure Excon = Excon(structure Ident = Ident)

structure TyCon = TyCon(
              structure StrId= StrId
              structure Crash= Crash
             )

structure TyName =TyName(structure TyCon = TyCon
               structure Timestamp = Timestamp
               structure Flags= Flags
              );

structure Lvars = Lvars()

structure LambdaExp = LambdaExp(structure Lvars=Lvars
                       structure Con= Con
                       structure Excon= Excon
                       structure TyName = TyName
                       structure PP= PP
                       structure Crash= Crash
                       structure Flags = Flags
                     )

structure TyNameEnv =
   struct
     type tyname = TyName.TyName
     type tyname_env = (tyname * (int*int*int)) list
     val tyname_env0 = [(TyName.tyName_BOOL, (0,0,0)),
                        (TyName.tyName_INT, (0,0,0)),
                        (TyName.tyName_REAL, (0,0,0)),
                        (TyName.tyName_STRING, (0,0,0)),
                        (TyName.tyName_LIST, (1,1,0)) (* .... more built-ins *)
                       ]

     fun lookup [] tyname = NONE
       | lookup ((tyname',arity)::rest) tyname = if tyname=tyname' then SOME arity else lookup rest tyname

     val lookup = lookup tyname_env0
   end;

structure RType = RType(
              structure Flags = Flags
              structure Crash = Crash
              structure E= Effect
              structure DiGraph = DiGraph
              structure L = LambdaExp
              structure TyName= TyName
              structure TyNameEnv = TyNameEnv
              structure PP = PP
             );

val _ = Flags.print_effects_in_types:= true;
fun pp(t) = PP.flatten1 t
fun say s = TextIO.output(std_out, s^"\n")
fun etest(label,expected,found) =
 say(label ^ (if expected = found then " OK" else " ****** NOT OK *******" ^
"\n expected: " ^ expected ^ 
"\n found:    " ^ found));
fun etest'(label,expected,found) = say (label ^ found);
open LambdaExp
open RType


val (lay_tau, lay_mu) = mk_layout false
fun show_tau t = pp(lay_tau t);
fun layout_sigma sigma = mk_lay_sigma false sigma
fun layout_sigma' sigma = mk_lay_norm_sigma false sigma



(*val _ = Flags.interact();*)

val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c
(*fun mltyvar s = TYVARtype(Tyvar.mk_TyVar "a")*)
fun mlarrow(tau1,tau2) = ARROWtype([tau1],[tau2])
val mlint = CONStype([], TyName.tyName_INT)
val int_to_int = mlarrow(mlint,mlint)
val(mk_tau,mk_mu) = freshType TyNameEnv.lookup;
val _ = say ("ml type: int -> int");
val (tau as (FUN(mu1,arreff,mu2)),c) = mk_tau(int_to_int,c);
val (rho4,c) = Effect.freshRho c;
val _ = Effect.edge (arreff, Effect.mkPut rho4);
val _ = etest ("rtype.unify tau: ","((int,r2)-e1(put(r4))->(int,r3))" ,show_tau(tau));
val (tau' as (FUN(_,arreff',_)),c) = mk_tau(int_to_int,c);
val (rho8,c) = Effect.freshRho c;
val _ = Effect.edge (arreff', Effect.mkGet rho8);
val _ = Effect.edge (arreff', Effect.mkPut rho4);
val _ = etest ("rtype.unify (2) ","tau': ((int,r6)-e5(put(r4),get(r8))->(int,r7))", "tau': " ^ show_tau(tau'));
val c = unify_ty(tau,tau')c;
val _ = say "unification done";
val _ = etest  ("rtype.unify (3) ", "tau: ((int,r2)-e1(put(r4),get(r8))->(int,r3))", "tau: " ^ show_tau(tau));
val _ = etest  ("rtype.unify (4) ", "tau': ((int,r2)-e1(put(r4),get(r8))->(int,r3))", "tau': " ^ show_tau(tau'));

(* test of inst *)
(* case 1:
   instantiate all alpha rho1 eps1. (alpha,rho1) -eps2.{}-> (alpha,rho1)
   with {alpha -> int, rho1 -> rho3, eps2 -> eps4.{get rho3}}:
*)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val body_sigma = FUN([(TYVAR alpha, rho1)], eps2, [(TYVAR alpha, rho1)]);
val sigma = FORALL(1, [alpha], [rho1], [eps2], body_sigma);
val (_,c) = Effect.pop c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val _ = Effect.edge(eps4, Effect.mkGet rho3)
val il = ([rint],[rho3],[eps4]);
val _ = say ("sigma = " ^ pp(layout_sigma sigma));
val (tau',c) = inst(sigma,il) c;
val _ = etest ("test of inst, case 1", "inst(sigma,il) = ((int,r3)-e4(get(r3))->(int,r3))", "inst(sigma,il) = " ^ show_tau tau');

(* case 2:
   instantiate all alpha rho1,3,5 eps2,4,6. ((alpha,rho1) -eps2.{}-> (alpha,rho1),rho3)
                       -eps4{put rho5}->((alpha,rho1) -eps6.{get rho3, eps2}-> (alpha,rho1),rho5)
   with {alpha -> int, rho1 -> rho7, rho3 ->r8, rho5 -> rho9, 
         eps2 -> eps10{get rho7, put rho7}, eps 4 -> eps11,eps6 -> eps12}:
*)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val body_sigma = FUN([(FUN([(TYVAR alpha, rho1)], eps2, [(TYVAR alpha, rho1)]),rho3)],
                     eps4,
                     [(FUN([(TYVAR alpha, rho1)], eps6, [(TYVAR alpha, rho1)]),rho5)]);
val sigma = FORALL(1, [alpha], [rho1,rho3,rho5], [eps2,eps4,eps6], body_sigma);
val (_,c) = Effect.pop c;
val (rho7,c) = Effect.freshRho c;
val (rho8,c) = Effect.freshRho c;
val (rho9,c) = Effect.freshRho c;
val (rho3,c) = Effect.freshRho c;
val (eps10,c) = Effect.freshEps c;
val (eps11,c) = Effect.freshEps c;
val (eps12,c) = Effect.freshEps c;
val _ = Effect.edge(eps10, Effect.mkGet rho7)
val _ = Effect.edge(eps10, Effect.mkPut rho7)
val il = ([rint],[rho7,rho8,rho9],[eps10,eps11,eps12]);
fun layout_sigma sigma = mk_lay_sigma false sigma
val _ = say ("sigma = " ^ pp(layout_sigma sigma));
val (tau',c) = inst(sigma,il) c;
val _ = etest ("test of inst, case 2 ", 
               "inst(sigma,il) = ((((int,r7)-e11(put(r7),get(@r7))->(int,r7)),r8)-e12(put(r9))->\
               \(((int,r7)-e13(e11(put(r7),get(@r7)),get(r8))->(int,r7)),r9))",
               "inst(sigma,il) = " ^ show_tau tau');

(* test af regEffClos *)

(* case 1: regEffClos(((alpha,rho1) -eps2.{}-> (alpha,rho1),rho3)
                       -eps4{put rho5}->((alpha,rho1) -eps6.{get rho3, eps2}-> (alpha,rho1),rho5) *)

val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val phi = Effect.mkPut(rho7)
val (_,sigma,msg) = regEffClos(c,1,phi,
                     FUN([(FUN([(TYVAR alpha, rho1)], eps2, [(TYVAR alpha, rho1)]),rho3)],
                     eps4,
                     [(FUN([(TYVAR alpha, rho1)], eps6, [(TYVAR alpha, rho1)]),rho5)]));
val _ = etest("test of regEffGen, case1, sigma = ", "all r5 r3 r1 e2 e6 e4.((((a3,r1)-e2->(a3,r1)),r3)-e4(put(r5))->(((a3,r1)-e6(e2,get(r3))->(a3,r1)),r5))", pp(layout_sigma sigma));

(* case 2, as above, but with phi = {Put rho1} *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
fun FUN'(x,y,z) = FUN([x],y,[z])
val (_,sigma,msg) = regEffClos(c,1,Effect.mkPut rho1,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case2, sigma = ", "all r5 r3 e2 e6 e4.((((a4,r1)-e2->(a4,r1)),r3)-e4(put(r5))->(((a4,r1)-e6(e2,get(r3))->(a4,r1)),r5))", pp(layout_sigma sigma));

(* case 3, as above, but with phi = eps6 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (_, sigma,msg) = regEffClos(c,1,eps6 ,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case3, sigma = ", "all r5 r1 e4.((((a5,r1)-e2->(a5,r1)),r3)-e4(put(r5))->(((a5,r1)-e6(e2,get(r3))->(a5,r1)),r5))", pp(layout_sigma sigma));

(* case 4, as above, but with phi = eps2 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (_,sigma,msg) = regEffClos(c,1,eps2,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case4, sigma = ", "all r5 r3 r1 e6 e4.((((a6,r1)-e2->(a6,r1)),r3)-e4(put(r5))->(((a6,r1)-e6(e2,get(r3))->(a6,r1)),r5))", pp(layout_sigma sigma));

(* case 5, a secondary region variable, rho8 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (rho8,c) = Effect.freshRho c
val _ = Effect.edge(eps6, Effect.mkPut rho8)
val _ = Effect.edge(eps6, Effect.mkGet rho8)
val (_,sigma,msg) = regEffClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case5, sigma = ", "all r5 r3 r1 e2 e6 e4.((((a7,r1)-e2->(a7,r1)),r3)-e4(put(r5))->(((a7,r1)-e6(get(r8),put(@r8),e2,get(r3))->(a7,r1)),r5))", pp(layout_sigma sigma));

(* case 6, a secondary effect variable, eps8 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val (_,sigma,msg) = regEffClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case6, sigma = ", 
"all r5 r3 r1 e2 e6 e4.((((a8,r1)-e2->(a8,r1)),r3)-e4(put(r5))->(((a8,r1)-e6(e8,e2,get(r3))->(a8,r1)),r5))", 
pp(layout_sigma sigma));

(* case 7, a secondary effect variable, eps8 which is in same scc as a
            primary effect variable, eps6*)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val _ = Effect.edge(eps8, eps6)
val (_,sigma,msg) = regEffClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of regEffGen, case6, sigma = ", "all r5 r3 r1 e2 e8 e4.\
\((((a9,r1)-e2->(a9,r1)),r3)-e4(put(r5))->(((a9,r1)-e8(e2,get(r3))->(a9,r1)),r5))", 
pp(layout_sigma sigma));

(* test af effClos *)

(* case 1: effClos(((alpha,rho1) -eps2.{}-> (alpha,rho1),rho3)
                       -eps4{put rho5}->((alpha,rho1) -eps6.{get rho3, eps2}-> (alpha,rho1),rho5) *)

val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val phi = Effect.mkPut(rho7)
val (_,sigma,msg) = effClos(c,1,phi,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case1, sigma = ", "all e2 e6 e4.((((a10,r1)-e2->(a10,r1)),r3)-e4(put(r5))->(((a10,r1)-e6(e2,get(r3))->(a10,r1)),r5))", pp(layout_sigma sigma));

(* case 2, as above, but with phi = {Put rho1} *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (_,sigma,msg) = effClos(c,1,Effect.mkPut rho1,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case2, sigma = ", "all e2 e6 e4.((((a11,r1)-e2->(a11,r1)),r3)-e4(put(r5))->(((a11,r1)-e6(e2,get(r3))->(a11,r1)),r5))", pp(layout_sigma sigma));

(* case 3, as above, but with phi = eps6 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (_, sigma,msg) = effClos(c,1,eps6 ,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case3, sigma = ", "all e4.((((a12,r1)-e2->(a12,r1)),r3)-e4(put(r5))->(((a12,r1)-e6(e2,get(r3))->(a12,r1)),r5))", pp(layout_sigma sigma));

(* case 4, as above, but with phi = eps2 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (_,sigma,msg) = effClos(c,1,eps2,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case4, sigma = ", "all e6 e4.((((a13,r1)-e2->(a13,r1)),r3)-e4(put(r5))->(((a13,r1)-e6(e2,get(r3))->(a13,r1)),r5))", pp(layout_sigma sigma));

(* case 5, a secondary region variable, rho8 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (rho8,c) = Effect.freshRho c
val _ = Effect.edge(eps6, Effect.mkPut rho8)
val _ = Effect.edge(eps6, Effect.mkGet rho8)
val (_,sigma,msg) = effClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case5, sigma = ", "all e2 e6 e4.((((a14,r1)-e2->(a14,r1)),r3)-e4(put(r5))->(((a14,r1)-e6(get(r8),put(@r8),e2,get(r3))->(a14,r1)),r5))", pp(layout_sigma sigma));

(* case 6, a secondary effect variable, eps8 *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val (_,sigma,msg) = effClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case6, sigma = ", 
"all e2 e6 e4.((((a15,r1)-e2->(a15,r1)),r3)-e4(put(r5))->(((a15,r1)-e6(e8,e2,get(r3))->(a15,r1)),r5))", pp(layout_sigma sigma));

(* case 7, a secondary effect variable, eps8 which is in same scc as a
            primary effect variable, eps6*)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val _ = Effect.edge(eps8, eps6)
val (_,sigma,msg) = effClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = etest("test of effGen, case6, sigma = ", "all e2 e8 e4.((((a16,r1)-e2->(a16,r1)),r3)-e4(put(r5))->(((a16,r1)-e8(e2,get(r3))->(a16,r1)),r5))", pp(layout_sigma sigma));

(* test af normSigma *)

(* case 1, all r5 r3 r1 e2 e8 e4.\
\((((a17,r1)-e2->(a17,r1)),r3)-e4(put(r5))->(((a17,r1)-e8(e2,get(r3))->(a17,r1)),r5))
*)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val _ = Effect.edge(eps8, eps6)
val (_,sigma,msg) = regEffClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
val _ = say ("sigma            = " ^ pp(layout_sigma sigma));
val sigma' = normSigma  sigma;
val _ = say ("normalised sigma = "^ pp(layout_sigma' sigma'));
val sigma1 = 
     let val FORALL(n,alpha,rhos,epsilons,tau) = sigma
     in FORALL(n, alpha, rev rhos, rev epsilons, tau)
     end
val _ = say ("sigma1           = "^ pp(layout_sigma sigma));
val sigma1' = normSigma  sigma1;
val _ = say ("normalised sigma1= "^ pp(layout_sigma' sigma1'));
val _ = etest("test of normSigma, case 1", "true", Bool.string(pp(layout_sigma' sigma') = pp(layout_sigma' sigma1')));

(* test af alpha_rename *)
val sigma2 = alpha_rename(sigma,c)
val _ = say ("sigma            = " ^ pp(layout_sigma sigma));
val _ = say ("alpha-conv sigma = "^ pp(layout_sigma sigma2));
val _ = etest("test of alpha_rename, case 1", "all r11 r10 r9 e14 e13 e12.((((a17,r9)-e14->(a17,r9)),r10)\
\-e12(put(r11))->(((a17,r9)-e13(e14,get(r10))->(a17,r9)),r11))", pp(layout_sigma sigma2))
val _ = etest("test of alpha_equal, case 1", "true", Bool.string(alpha_equal(sigma,sigma2)));




(* Test of matchSchemes *)
val _ = Effect.resetCount();
val c = Effect.emptyCone;
val c = Effect.push c;
val c = Effect.push c;
val alpha = fresh_tyvar();
val (rho1,c) = Effect.freshRho c;
val (eps2,c) = Effect.freshEps c;
val (rho3,c) = Effect.freshRho c;
val (eps4,c) = Effect.freshEps c;
val (rho5,c) = Effect.freshRho c;
val (eps6,c) = Effect.freshEps c;
val rint = CONSTYPE(TyName.tyName_INT,[],[],[]);
val _ = Effect.edge(eps4, Effect.mkPut rho5)
val _ = Effect.edge(eps6, Effect.mkGet rho3)
val _ = Effect.edge(eps6, eps2)
val (rho7,c) = Effect.freshRho c
val (eps8,c) = Effect.freshEps c
val _ = Effect.edge(eps6, eps8)
val (c,sigma,msg) = regEffClos(c,1,Effect.mkPut rho7,
                     FUN'((FUN'((TYVAR alpha, rho1), eps2, (TYVAR alpha, rho1)),rho3),
                     eps4,
                     (FUN'((TYVAR alpha, rho1), eps6, (TYVAR alpha, rho1)),rho5)));
(*val (_,c) = Effect.pop c*)
val _ = say ("sigma = " ^ pp(layout_sigma sigma));
val (rho9,c) = Effect.freshRho c
val (rho10,c) = Effect.freshRho c
val (eps11,c) = Effect.freshEps c
val (eps12,c) = Effect.freshEps c
val (tau', c)=  RType.inst(sigma, ([], [rho9,rho10,rho10], [eps11, eps11, eps12])) c
val _ = say ("the instance = " ^ show_tau tau')
val (c, sigma', msg )= RType.regEffClos(c,1, Effect.empty, tau')
val _ = say ("sigma' = " ^ pp(layout_sigma sigma'));
val transformer = matchSchemes (sigma, sigma');
val example1 = transformer([],[10,20,30],[40,50,60]);
val expected_output1 = ([],[10,30],[60,40])

val _ = etest("test of matchSchemes, case1, ", "true", Bool.string (example1 = expected_output1)); 

end (*TestRType*)

*)