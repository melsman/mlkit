
structure RType: RTYPE =
struct
  structure PP = PrettyPrint
  structure L = LambdaExp
  structure E = Effect

  val print_word_regions = Flags.add_bool_entry
      {long="print_word_regions", short=SOME "Pwordregions", item=ref false, neg=false,
       menu=["Layout", "print word regions"], desc=
       "Also print word regions that have been dropped."}

  val print_regions = Flags.is_on0 "print_regions"
  val print_effects = Flags.is_on0 "print_effects"

  fun uncurry f (a,b) = f a b

  fun say s= TextIO.output(TextIO.stdOut, s ^ "\n");
  fun logsay s= TextIO.output(!Flags.log, s );
  fun log_tree t= PP.outputTree(logsay, t, !Flags.colwidth)
  fun show_rho rho = PP.flatten1(E.layout_effect rho)
  fun show_eps e = PP.flatten1(E.layout_effect e)
  fun show_rhos rhos = ListUtils.stringSep "[" "]" ", " show_rho rhos

  fun die (s:string) = Crash.impossible ("RType." ^ s)
  fun pp (stringtree) = PP.flatten1(stringtree)

  fun noSome (SOME v) s = v
    | noSome NONE s = die s

  infix footnote'
  fun x footnote' f = (f x; x)

  type effect = E.effect
  type cone = E.cone
  type delta_phi = E.delta_phi

  type LambdaType = L.Type
  type StringTree = PP.StringTree

  type tyvar = L.tyvar
  type tyname = L.TyName
  type place = E.effect
  type arroweffect = E.effect

  fun pr_place r = PP.flatten1(E.layout_effect r)

  datatype Type =
      TYVAR of tyvar
    | CONSTYPE of tyname * mu list * place list * arroweffect list
    | RECORD of mu list
    | FUN of mu list * arroweffect * mu list

  withtype mu = Type*place

  val mkTYVAR = TYVAR
  val mkCONSTYPE = CONSTYPE
  val mkRECORD = RECORD
  val mkFUN = FUN

  fun unTYVAR (TYVAR a) = SOME a
    | unTYVAR _ = NONE

  fun unCONSTYPE (CONSTYPE a) = SOME a
    | unCONSTYPE _ = NONE

  fun unRECORD (RECORD a) = SOME a
    | unRECORD _ = NONE

  fun unFUN (FUN a) = SOME a
    | unFUN _ = NONE


  type runType = E.runType

  (* details of runtype are exploited in SpreadDataType.infer_arity_ty *)

  local open TyName
  in

      (* When Generational GC is used, vectors need to have runtype
       ARRAY_RT due to the implementation of vectors using the
       wordtable.sml file (WordTable functor) which updates the
       content of the vector after creation. If a minor gc occurs
       between the creation and the update, the new updated content
       could be mistakenly garbage collected during the next
       collection. *)

    fun runtype (CONSTYPE(tn, _, _, _)) =
        if TyName.unboxed tn then E.WORD_RT
        else if eq(tn, tyName_REF) then E.REF_RT
        else if eq(tn, tyName_ARRAY) orelse eq(tn, tyName_VECTOR) then E.ARRAY_RT
        else if eq(tn, tyName_STRING) orelse eq(tn, tyName_CHARARRAY) then E.STRING_RT
        else E.TOP_RT
      | runtype (TYVAR _) = E.BOT_RT
      | runtype (RECORD[_,_]) = E.PAIR_RT
      | runtype (RECORD[_,_,_]) = E.TRIPLE_RT
      | runtype (RECORD[]) = E.WORD_RT    (* unit is also unboxed *)
      | runtype _ = E.TOP_RT
  end

  fun isWordRegion (rho) =
      case E.get_place_ty rho of
          SOME E.WORD_RT => true
        | _ => false

  fun isTopWordRegion rho = E.eq_effect(rho,E.toplevel_region_withtype_word)

  fun discard_top_wordrho places = List.filter (not o isTopWordRegion) places
  fun discard_word_rhos places = List.filter (not o isWordRegion) places

(*
  local
    val (lay_ty,lay_mu) = mk_layout false
    fun dump_ty(ty) =
        PP.outputTree(fn s => TextIO.output(!Flags.log, s),
                      lay_ty ty, !Flags.colwidth)

    fun dump_mu(mu) =
        PP.outputTree(fn s => TextIO.output(!Flags.log, s),
                      lay_mu mu, !Flags.colwidth)

    fun unify_ty0 (t1,t2,c) : c =
        case (t1, t2) of
          (TYVAR _, TYVAR _) => c
        | (CONSTYPE(_,mus1,rs1,es1),CONSTYPE(_,mus2,rs2,es2)) =>
          unify_mus0(mus1,mus2,
                     unify_rhos0(rs1,rs2,
                                 unify_epss0(es1,es2,c)))
        | (RECORD mus1, RECORD mus2) => unify_mus0(mus1,mus2,c)
        | (FUN(mus1,e1,mus'1),FUN(mus2,e2,mus'2)) =>
          unify_mus0(mus1,mus2, E.unifyEps(e1,e2) (unify_mus0(mus'1,mus'2,c)))
        | _ => (TextIO.output(!Flags.log, "ty1 = \n"); dump_ty t1;
                TextIO.output(!Flags.log, "ty2 = \n"); dump_ty t2;
                die ("unify: types do not unify"))

    and unify_mu0 ((t1,r1),(t2,r2),c) =
        unify_ty0(t1,t2, E.unifyRho(r1,r2)c)

    and unify_mus0 (mus1,mus2,c) =
        ListPair.foldlEq unify_mu0 c (mus1,mus2)
        handle ListPair.UnequalLengths =>
               die "unify_mus0: UnequalLengths"
    and unify_rhos0 (rs1,rs2,c) =
        ListPair.foldlEq (fn (a,b,c) => E.unifyRho(a,b)c) c (rs1,rs2)
        handle ListPair.UnequalLengths =>
               die "unify_rhos0: UnequalLengths"
    and unify_epss0 (es1,es2,c) =
        ListPair.foldlEq (fn (a,b,c) => E.unifyEps(a,b)c) c (es1,es2)
        handle ListPair.UnequalLengths =>
               die "unify_epss0: UnequalLengths"
  in
  fun unify_ty (a,b) c = unify_ty0 (a,b,c)
      handle _ => (TextIO.output(!Flags.log, "mu1 = \n"); dump_ty a;
                   TextIO.output(!Flags.log, "mu2 = \n"); dump_ty b;
                   die "unify_ty: types do not unify")
  fun unify_mus (a,b) c = unify_mus0 (a,b,c)
      handle _ => (die "unify_mus: types with places do not unify")
  fun unify_mu (a,b) c = unify_mu0 (a,b,c)
      handle _ => (TextIO.output(!Flags.log, "mu1 = \n"); dump_mu a;
                   TextIO.output(!Flags.log, "mu2 = \n"); dump_mu b;
                   die "unify_mu: types with places do not unify")

  end
*)

  (* ann_XX0 ty acc: return effects in the order they appear in the
   * underlying ML-type; this function is used for unifying effects in
   * types; we do not remove the topWordRegions from the lists as this
   * could mess up the lists... mael 2007-11-14 *)

  fun ann_ty0 ty (acc : effect list) =
      case ty of
          TYVAR _ => acc
        | CONSTYPE(_,mus,rhos,epss) =>
          List.foldr ann_mu0 (rhos @ (epss @ acc)) mus
        | RECORD mus =>
          List.foldr ann_mu0 acc mus
        | FUN(mus1,eps,mus2) =>
          ann_mus0 mus1 (eps:: ann_mus0 mus2 acc)
  and ann_mu0 ((tau,rho), acc) = ann_ty0 tau (rho::acc)
  and ann_mus0 [] acc  = acc
    | ann_mus0 (mu::rest) acc = ann_mu0 (mu, ann_mus0 rest acc)

  fun ann_ty ty (acc : effect list) =
      case ty of
          TYVAR _ => acc
        | CONSTYPE(_,mus,rhos,epss) =>
          List.foldr ann_mu (discard_top_wordrho rhos @ (epss @ acc)) mus
        | RECORD mus =>
          List.foldr ann_mu acc mus
        | FUN(mus1,eps,mus2) =>
          ann_mus mus1 (eps:: ann_mus mus2 acc)
  and ann_mu ((tau,rho), acc) =
      ann_ty tau (if isTopWordRegion rho then acc else rho::acc)
  and ann_mus [] acc  = acc
    | ann_mus (mu::rest) acc = ann_mu (mu, ann_mus rest acc)

  (* free region variables of mu, including secondary occurrences but excluding the topWordRegion *)

  fun frv_mu mu =
      let val annotations = ann_mu (mu, [])
          val all_nodes = E.subgraph annotations
      in
          List.filter (fn e => E.is_rho e andalso not(isTopWordRegion e)) all_nodes
      end

  local  (* free primary region and effect variables ("pfrv tau" etc.) *)
    fun pfrv0 (ty, acc) =
         case ty of
           TYVAR _ => acc
         | CONSTYPE(_,mus,places,_) => List.foldr pfrvMu0 (discard_top_wordrho places @ acc) mus
         | RECORD mus => List.foldr pfrvMu0 acc mus
         | FUN(mus1,_,mus2) => pfrvMus0 (mus1, pfrvMus0 (mus2, acc))
    and pfrvMu0 ((tau,rho), acc) = pfrv0 (tau, if isTopWordRegion rho then acc else rho::acc)
    and pfrvMus0 ([], acc) = acc
      | pfrvMus0 (mu::rest, acc) = pfrvMu0 (mu, pfrvMus0 (rest, acc))

    fun pfev0 (ty, acc) =
         case ty of
           TYVAR _ => acc
         | CONSTYPE(_,mus,_,arreffs) => List.foldr pfevMu0 (arreffs@acc) mus
         | RECORD mus => List.foldr pfevMu0 acc mus
         | FUN(mus1,arreff,mus2) => pfevMus0 (mus1,arreff::(pfevMus0(mus2,acc)))
    and pfevMu0 ((tau,rho), acc) = pfev0 (tau, acc)
    and pfevMus0 ([], acc) = acc
      | pfevMus0 (mu::rest, acc) = pfevMu0 (mu, pfevMus0 (rest, acc))
  in
    fun pfrv ty = pfrv0 (ty, [])
    fun pfrvMu mu = pfrvMu0 (mu, [])

    fun pfev ty = pfev0 (ty, [])
    fun pfevMu mu = pfevMu0 (mu, [])
  end (* local *)

  fun repeat 0 f acc = acc
    | repeat n f acc =
        repeat (n-1) f (f acc)

  fun freshType (lookup: tyname -> (int*runType list*int)option) :
                                (L.Type * cone -> Type * cone)
                              * (L.Type * cone -> mu * cone) =
    let
      fun mkTy (ty,cone) = case ty of
              L.TYVARtype alpha  => (TYVAR alpha, cone)
            | L.ARROWtype(tys1,tys2)=>
                let val (eps,cone') = E.freshEps(cone)
                    val (cone1,mus1) = List.foldr mkMus (cone',[]) tys1
                    val (cone2,mus2) = List.foldr mkMus (cone1,[]) tys2
                in (FUN(mus1,eps,mus2), cone2) end
            | L.CONStype(tys,tyname)=>
              let val arity as (alpha_count, rhos_runtypes, eps_count) =
                    case lookup tyname
                      of SOME arity => arity
                       | NONE => die ("mkTy: type name " ^ TyName.pr_TyName tyname ^ " not declared")
                  val (cone, mus) = List.foldr mkMus (cone,[]) tys
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
                 let val (cone,mus) = List.foldr mkMus (cone,[]) tys
                 in
                   (RECORD(mus),cone)
                 end
      and mkMu(ty, cone) =
          let val (tau, cone) = mkTy(ty,cone)
              val (rho,cone) = E.freshRhoWithTy(runtype tau, cone)
          in
              ((tau,rho),cone)
          end
      and mkMus (ty, (cone, acc: mu list)) =
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

  fun layout_tyvar' (alpha,NONE) = PP.LEAF(L.pr_tyvar alpha)
    | layout_tyvar' (alpha,SOME e) =
      Node{start="",finish="",indent=0,
           childsep=PP.RIGHT":",children=[layout_tyvar alpha,E.layout_effect e]}

  fun lay_node n = E.layout_effect_deep n
  fun lay_node_short n = E.layout_effect n

  fun mk_layout omit_region_info =
  let
     fun layout_arrow_rec arreff =
              if print_effects()
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
                    val place_list = if print_word_regions() then place_list
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
      if omit_region_info orelse (not(print_word_regions())
                                  andalso isWordRegion rho) then
        lay_tau_rec parenthesise tau
      else layout_pair (lay_tau_rec false tau, lay_node rho)

    and layout_arg_res [mu] = lay_mu_rec true mu
      | layout_arg_res mus = layout_list (lay_mu_rec true) mus
  in
    (lay_tau_rec false, lay_mu_rec false)
  end;

  (* unification *)

  fun u ((node1,node2), cone) =
    if E.is_arrow_effect node1
      then E.unifyEps(node1,node2) cone
    else E.unifyRho(node1, node2) cone

  fun unify_ty (ty1, ty2:Type) cone: E.cone =
    let val effs1 = ann_ty0 ty1 []
        val effs2 = ann_ty0 ty2 []
    in
      List.foldl u cone (BasisCompat.ListPair.zipEq(effs1,effs2))
      handle X =>  let val (lay_ty,_) = mk_layout false;
                       fun dump(ty) = PP.outputTree(fn s => TextIO.output(!Flags.log, s), lay_ty ty, !Flags.colwidth)
                   in
                     TextIO.output(!Flags.log, "ty1 = \n"); dump ty1;
                     TextIO.output(!Flags.log, "ty2 = \n"); dump ty2;
                     die ("unify: types do not unify. Exception " ^ exnName X ^
                          " raised; length(effs1) = " ^ Int.toString(length effs1) ^
                          " and length(effs2) = " ^ Int.toString (length effs2) ^ ". ")
                   end
    end

  fun unify_mu (mu1, mu2:mu) cone: E.cone =
    List.foldl u cone (BasisCompat.ListPair.zipEq(ann_mu0 (mu1, []),ann_mu0 (mu2, [])))
    handle (X as Report.DeepError _) => raise X
         | X => let val (_,lay_mu) = mk_layout false;
                     fun dump(mu) = PP.outputTree(fn s => TextIO.output(!Flags.log, s), lay_mu mu, !Flags.colwidth)
                 in
                     TextIO.output(!Flags.log, "mu1 = \n"); dump mu1;
                     TextIO.output(!Flags.log, "mu2 = \n"); dump mu2;
                     die ("unify: types with places do not unify: " ^ General.exnMessage X)
                 end

  fun unify_mus (mus1, mus2) cone: E.cone =
     List.foldl (uncurry unify_mu) cone
       (BasisCompat.ListPair.zipEq(mus1,mus2)) handle BasisCompat.ListPair.UnequalLengths =>
         die "unify_mus: lists have different lengths"

  (* type schemes: bound variable must be listed in bottom-up
     depth first search order. No cycles amongst bound effect variables
     are permitted. There can be at most one secondary effect variable.
     There can be at most one secondary region variable pr runtime
     type. Every bound region or effect variable has in it a field, pix,
     which uniquely identifies its syntactic position in the term.
     Negative pix indicates secondary variable; for region variables,
     each runtime type corresponds to a distinct negative number.
  *)

  fun ann_alphas nil acc = acc
    | ann_alphas ((_,NONE)::xs) acc = ann_alphas xs acc
    | ann_alphas ((_,SOME e)::xs) acc = ann_alphas xs (e::acc)

  (* locate a corresponding arrow effect *)
  fun locate_effect_ty (e:effect) (a:effect option) t0 t1 : effect option =
      case a of SOME _ => a | NONE =>
      (case (t0,t1) of
          (TYVAR _, TYVAR _) => NONE
        | (CONSTYPE (tn0,mus0,rs0,es0), CONSTYPE (tn1,mus1,rs1,es1)) => locate_effect_mus e a mus0 mus1
        | (RECORD mus0, RECORD mus1) => locate_effect_mus e a mus0 mus1
        | (FUN (mus0,ae0,mus0'), FUN (mus1,ae1,mus1')) =>
          if E.eq_effect (ae0,e) then SOME ae1
          else let val es0 = E.represents ae0
                   val es1 = E.represents ae1
               in case (List.filter E.is_arrow_effect es0, List.filter E.is_arrow_effect es1) of
                      ([e0],[e1]) => if E.eq_effect(e,e0) then SOME e1 else NONE
                    | _ => NONE
               end
        | _ => die "locate_effect_ty: type mismatch!")
  and locate_effect_mus e a mus0 mus1 =
      case a of SOME _ => a | NONE =>
      case (mus0,mus1) of
          (nil,nil) => a
        | ((ty0,_)::mus0, (ty1,_)::mus1) =>
          locate_effect_mus e (locate_effect_ty e a ty0 ty1) mus0 mus1
        | _ => die "locate_effect_mus: non-matching number of mus"

  fun locate_arrow_effect (e:effect) t0 t1 : effect option =
      locate_effect_ty e NONE t0 t1

  (* Type schemes *)

  datatype sigma =
     FORALL of place list * arroweffect list * (tyvar*arroweffect option) list * Type

  fun bv (FORALL(rhos,epss,alphas,_)) = (rhos,epss,alphas)

  fun un_scheme (FORALL x) = x

  fun type_to_scheme tau = FORALL([],[],[],tau)

  fun frv_sigma (FORALL(rhos,epss,alphas,tau)) =
      let val annotations = ann_alphas alphas []
          val annotations = ann_ty tau annotations
          val all_nodes = E.subgraph annotations
          val frv_tau = List.filter E.is_rho all_nodes
      in
          (* subtract the bound rhos *)
          List.app (fn bound_rho => E.get_visited bound_rho := true) rhos;
          List.filter (fn free_rho => not(!(E.get_visited free_rho))) frv_tau
          before List.app (fn bound_rho => E.get_visited bound_rho := false) rhos
      end

  (* ferv_sigma(sigma) computes a list of all region and effect
     variables that occur free in sigma *)

  fun ferv_sigma (FORALL(rhos,epss,alphas,tau)): E.effect list =
      let val annotations = ann_alphas alphas []
          val annotations = ann_ty tau annotations
          val all_nodes = E.subgraph annotations
          val free = List.filter (fn node => E.is_rho node orelse E.is_arrow_effect node) all_nodes
          val bound = epss @ rhos
      in
          (* subtract the bound region and effect variables *)
          List.app (fn b => E.get_visited b := true) bound;
          List.filter (fn f => not(!(E.get_visited f))) free
          before List.app (fn b => E.get_visited b := false) bound
      end

  fun free_puts (FORALL(rhos,epss,alphas,tau)) =
      let val annotations = ann_alphas alphas []
          val annotations = ann_ty tau annotations
          val all_nodes = E.subgraph annotations
          val put_nodes = List.filter E.is_put all_nodes
          val rhos_in_put_nodes = map E.rho_of put_nodes
      in
          (* subtract the bound rhos *)
          List.app (fn bound_rho => E.get_visited bound_rho := true) rhos;
          List.filter (fn free_rho => not(!(E.get_visited free_rho))
                                      andalso not(isTopWordRegion free_rho)) rhos_in_put_nodes
          before List.app (fn bound_rho => E.get_visited bound_rho := false) rhos
      end

  local
    fun mem tv nil = false
      | mem tv (tv'::tvs) = tv = tv' orelse mem tv tvs
  in
    fun ftv_sigma (FORALL(_,_,alphas,tau)) : tyvar list =
        let
            fun ftv (t,(seen,acc)) =
                case t of
                    TYVAR tv => if mem tv seen then (seen,acc) else (tv::seen,tv::acc)
                  | CONSTYPE(_,mus,_,_) => ftv_mus (mus,(seen,acc))
                  | RECORD mus => ftv_mus (mus,(seen,acc))
                  | FUN(mus1,_,mus2) => ftv_mus (mus2,ftv_mus (mus1,(seen,acc)))
            and ftv_mus (mus,p) = List.foldr (fn ((ty,_),p) => ftv(ty,p)) p mus
        in #2 (ftv(tau,(map #1 alphas,nil)))
        end

    fun ftv_ty ty = ftv_sigma (type_to_scheme ty)

    fun ftv_minus (nil, tvs) = nil
      | ftv_minus (x::xs, tvs) =
        if mem x tvs then ftv_minus (xs, tvs)
        else x :: ftv_minus (xs, tvs)
  end

  fun insert_alphas (alphas, FORALL(rhos,epss,_,tau)) =
      let (* A type variable in alphas may be associated with different regions
             when occuring as a mu in tau. However, the same region cannot be
             associated with different type variables. Here is a property that
             should hold:

             For each mu=(tv,r) in tau,
               a) rt(r) = RT_BOT
               b) tv in alphas implies r in rhos.
               c) for any other mu'=(tv',r) in tau, tv=tv'

             Moreover, for any n-array type name tn, the type name has associated
             with it m >= n auxiliary region variables and we assume that the region
             variable associated with the i'th type variable is given by the function

               RV : TyName x N => N

             Thus, for a region-annotated datatype declaration

               datatype ([tv1..tv..tvn],[r0..r..rm],[..]) tn =
                  C1 of ty1 | ... | Ck of tyk

             it holds that for each mu=(tv,r) in ty1...tyk, we have

               1) tv = tvi for some i in {1,...,n}  (!)
               2) mu=(tvi,rj) and j = RV(tn,i)

             (!) Notice that the type schemes for the datatype constructors are
             guaranteed to be closed (enforced by the SML definition).

             When this check is enabled, compiling SMLtoJS fails with
             a message "Impossible: RType.quantified type variable
             'a56 associated with a non-quantified region r2546",
             which happens during spreading of pickler code for
             IntFinMapPT.
           *)

        fun is_abs_tv tv = List.exists (fn (tv',_) => tv=tv') alphas
        type tvenv = tyvar option IntFinMap.map
        fun chk (E:tvenv) (ty,rhoOpt) : tvenv =
            case ty of
                TYVAR tv =>
                if is_abs_tv tv then
                  (case rhoOpt of
                       NONE => E (* either toplevel or argument to CONSTYPE *)
                     | SOME rho =>
                       let val key = E.key_of_eps_or_rho rho
                       in case IntFinMap.lookup E key of
                              NONE => (* rho not in rhos *)
                              die ("quantified type variable " ^ L.pr_tyvar tv ^
                                   " associated with a non-quantified region " ^
                                   pr_place rho)
                            | SOME NONE => (* rho in rhos *)
                              (case E.get_place_ty rho of
                                   SOME E.BOT_RT => IntFinMap.add(key,SOME tv,E)
                                 | SOME rt => die ("quantified type variable " ^ L.pr_tyvar tv ^
                                                   " associated with region " ^ pr_place rho ^
                                                   " of type " ^ E.show_runType rt)
                                 | NONE => die ("quantified type variable " ^ L.pr_tyvar tv ^
                                                " associated with node " ^ pr_place rho ^
                                                ", which has no runtype")
                              )
                            | SOME (SOME tv') =>
                              if tv'<>tv then
                                die ("quantified type variable " ^ L.pr_tyvar tv ^
                                     " is associated with the region " ^ pr_place rho ^
                                     ", which is also associated with the type variable " ^
                                     L.pr_tyvar tv')
                              else E
                       end)
                else E
              | RECORD mus => chks E mus (* memo: check rhoOpt *)
              | CONSTYPE(tn,mus,rhos,effs) => chks E mus (* memo: check rhoOpt *)
              | FUN(mus1,_,mus2) => chks (chks E mus1) mus2 (* memo: check rhoOpt *)
        and chks E nil = E
          | chks E ((ty,rho)::mus) = chks (chk E (ty,SOME rho)) mus
(*
        val E0 = IntFinMap.fromList (map (fn r => (E.key_of_eps_or_rho r,NONE)) rhos)
        val _ = chk E0 (tau,NONE)
*)
      in FORALL(rhos,epss,alphas,tau)
      end

  fun drop_alphas (FORALL(rhos,epss,_,tau)) =
      FORALL(rhos,epss,[],tau)

  fun mk_lay_sigma_aux (omit_region_info:bool) :
    StringTree list * arroweffect list * (tyvar*arroweffect option) list * Type -> PP.StringTree =
  let
    val (lay_ty, _) = mk_layout omit_region_info
    fun lay_sig (rho_trees,epsilons,alphas,tau) =
      (case(rho_trees,epsilons,alphas) of
         ([],[],[]) => if !Flags.print_types then lay_ty tau else PP.LEAF ""
       | _ =>
          let val children =
                  if print_effects() then
                    (*print regions and effect and -perhaps- types: *)
                    rho_trees  @  map lay_node_short epsilons @
                    (if !Flags.print_types then map layout_tyvar' alphas
                     else [])
                  else (if print_regions() then rho_trees
                        else []) @
                       (if !Flags.print_types then map layout_tyvar (map #1 alphas)
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
                      | _ => if print_regions() orelse
                                print_effects()
                               then Node{start = "[", finish = "]",
                                         indent = 1, childsep = PP.NOSEP,
                                         children = [binders]}
                             else binders)
          end
      )
  in
    lay_sig
  end

  fun mk_lay_sigma omit_region_info =
      let val f = mk_lay_sigma_aux omit_region_info
      in fn (FORALL (raw as (rhos,epss,alphas,tau))) =>
              f(map lay_node rhos, epss, alphas, tau)
      end

  fun mk_lay_sigma' (omit_region_info: bool) (rhos,epss,tyvars,tau): PP.StringTree =
      mk_lay_sigma(omit_region_info)(FORALL(rhos,epss,tyvars,tau))

  fun mk_lay_sigma'' (lay_bind: 'b -> StringTree option) omit_region_info  =
      let val f = mk_lay_sigma_aux omit_region_info
      in fn (rhos,epss,alphas,tau) =>
             let val ts = List.foldr (fn (rho,acc) => case lay_bind rho of
                                          SOME t => t::acc | _ => acc) [] rhos
             in f(ts, epss, alphas, tau)
             end
      end

  (* maybe_increase_runtype(mu as (tau,rho)) increases the runType of rho to
     be the least upper bound of its current runType and the runType of tau.
     This must be done during instantiation of type schemes when (tau,rho)
     is the result of instantiating (alpha',rho') where rho' has runtype BOT_RT *)

  fun maybe_increase_runtype (mu as (tau,rho)) cone =
    let val old = case E.get_place_ty rho of SOME old => old | _ => die "maybe_increase_runtype"
        val new = runtype tau
    in
        if old<>new then
            if E.eq_effect(rho, E.toplevel_region_withtype_bot) then   (* This should not happen too often ; ughhh *)
                (tau, E.toplevelRhoFromTy new)
            else
                ((case E.lub_runType(old,new) of
                      E.WORD_RT =>
                          (E.unifyRho(E.toplevel_region_withtype_word, rho) cone; mu)   (* the toplevel region is already in the cone, so the resulting cone is unaltered (the entry for rho could be gc'ed) *)
                    | rt => (E.setRunType rho rt; mu))
              handle X =>
                let val (_,lay_mu) = mk_layout false;
                    fun dump mu = PP.outputTree(fn s => TextIO.output(!Flags.log, s), lay_mu mu, !Flags.colwidth)
                in TextIO.output(!Flags.log, "mu = ")
                 ; dump mu
                 ; TextIO.output(!Flags.log, "\n")
                 ; TextIO.output(!Flags.log, "new: " ^ E.show_runType new ^ "\n")
                 ; TextIO.output(!Flags.log, "old: " ^ E.show_runType old ^ "\n")
                 ; die "maybe_increase_runtype"
                end)
        else mu
    end

  (* instantiation of type schemes *)

  (* inst: sigma*il -> cone -> (Type * cone)

     let (tau,c') = inst(sigma,c)
     Then tau is the result of instantiating sigma via il.
     Only the part of the body that has to be copied is copied (when
     a sub-term of the body of sigma does not contain bound variables it
     is traversed, but not copied).
  *)

  type il = effect list * effect list * Type list (* instantiation lists *)

  fun mk_il x = x
  fun un_il x = x

  fun instAux (S as (St,Sr,Se),tau) cone =
      let
        (* debugging

          val _ = logsay("instAux enter, tau = \n");
          val (lay_ty, _) = mk_layout false
          val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
          val _ = logsay ("\nSr = " ^ concat (map (fn (rho, rho') => show_rho rho ^ "->" ^show_rho rho' ^ ",") Sr))
        *)
        fun fst (x,y) = x

        fun cp_var node =
            case E.get_instance node of
                ref(SOME node') => (true,node')
              | _ => (false,node)

        fun applySt ([],alpha) = NONE
          | applySt (((alpha',_),ty)::rest, alpha) =
            if alpha=alpha' then SOME ty
            else applySt(rest,alpha)

        (* cp: copy as much of the body of the type scheme as is necessary *)

        fun cp_rho rho = cp_var rho

        fun cp_eps eps = cp_var eps

        fun cp_ty ty =
            case ty of
                TYVAR alpha => (case applySt(St, alpha) of
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
                       TYVAR _ => (true, maybe_increase_runtype(tau1,rho2)cone)
                     | _ =>(true,(tau1, rho2))
              else (false,mu)
            end

        val _ = List.app E.setInstance Sr
        val _ = List.app E.setInstance Se

        val Ty = #2(cp_ty tau)
        (* this is where arrow effects are instantiated*)
        val (cone, updates) = E.instNodesClever (Sr @ Se) cone

        val spuriousPairs =
            foldr (fn (((tv,NONE),_),acc) => acc
                    | (((tv,SOME e),ty),acc) => (#2(cp_eps e), ty)::acc) nil St

        val _ = List.app E.clearInstance Sr
        val _ = List.app E.clearInstance Se

        (*val _ = before Profile.profileOff()*)
      in
        (Ty,cone,updates,spuriousPairs)
      end
      handle X =>
             let val _ = logsay("instAux error; tau = \n");
                 val (lay_ty, _) = mk_layout false
                 val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
                 val _ = logsay ("\nSr = {" ^ concat (map (fn (rho, rho') => show_rho rho ^ "->" ^
                                                                             show_rho rho' ^ ",") Sr)
                                 ^ "}\n")
                 val _ = logsay ("\nSt = {" ^ concat (map (fn ((tv,NONE), tau) => L.pr_tyvar tv ^ "->" ^
                                                                                  PP.flatten1(lay_ty tau) ^ ","
                                                            | ((tv,SOME e), tau) => L.pr_tyvar tv ^ ":" ^ show_eps e ^ "->" ^
                                                                                    PP.flatten1(lay_ty tau) ^ ",") St)
                                 ^ "}")
             in raise X
             end

  fun ann_sigma (FORALL(_,_,alphas,ty)) : effect list -> effect list =
      ann_ty ty o ann_alphas alphas

  (* update_runtypes(actuals,formals) -> unit:
   * make sure runtypes of actuals >= runtypes of formals.
   *)

  fun runtype_place place =
      case E.get_place_ty place of
          SOME rt => rt
        | NONE => die "runtype_place"

  fun update_runtypes ([],[]) = ()
    | update_runtypes (rho_a::actuals,rho_f::formals) =
      let val rt_a = runtype_place rho_a
          val rt_lub = E.lub_runType(rt_a,runtype_place rho_f)
                       handle X => ( print ("RType.update_runtypes: Failed to find lub_runtype for "
                                            ^ pr_place rho_a ^ " and " ^ pr_place rho_f ^ "\n")
                                   ; raise X
                                   )
          val _ = if rt_lub = rt_a then ()
                  else E.setRunType rho_a rt_lub
      in update_runtypes(actuals,formals)
      end
    | update_runtypes _ = die "update_runtypes"

  fun unify_with_toplevel_wordregion (cone, rhos) =
      let val rhos = List.filter isWordRegion rhos
      in foldl (fn (rho, cone) =>
                   E.unifyRho(E.toplevel_region_withtype_word, rho) cone)
               cone rhos
      end

  fun instClever (FORALL([],[],[],tau),il) cone = (tau, cone, [], [])
    | instClever (sigma as FORALL(rhos,epsilons,alphas,tau),
                  il as (places,arreffs,types)) cone =
      let
        (*val _ = Profile.profileOn();*)
        (* set types of places according to rhos *)
        val _ = update_runtypes(places, rhos)
(*
          val _ = app (fn rho => case E.get_place_ty rho
                                   of SOME E.WORD_RT => die "instClever.quantified word region!!"
                                    | _ => ()) rhos
*)
        val S = (BasisCompat.ListPair.zipEq(alphas,types),
                 BasisCompat.ListPair.zipEq(rhos,places),
                 BasisCompat.ListPair.zipEq(epsilons,arreffs))
                handle _ => die "inst: type scheme and \
                                        \instantiation list have different arity"
        val (Ty,cone,updates,spuriousPairs) = instAux(S, tau) cone
          handle X =>
                 let val () = print "\nFailed to instantiate type scheme (no ri):\n"
                     val () = print (PP.flatten1(mk_lay_sigma true sigma) ^ "\n")
                     val () = print "\nFailed to instantiate type scheme:\n"
                     val () = print (PP.flatten1(mk_lay_sigma false sigma) ^ "\n")
                 in raise X
                 end


(*      val cone = unify_with_toplevel_wordregion (cone, places) *)
      in (Ty,cone,updates,spuriousPairs)
      end

  fun inst sigma_il cone =
      let val (a,cone,c,_) = instClever sigma_il cone
          val places = #1(#2 sigma_il)
          val cone = unify_with_toplevel_wordregion (cone, places)
      in (a,cone)
      end

  (* generalisation: RegEffClos *)

  fun warn effects =
      case effects of
          [] => NONE
       |  _ => SOME("regEffClos: escaping from generalisation: "
                    ^ ListUtils.stringSep
                          "[" "]" ", "
                          (pp o E.layout_effect) effects ^ "\n")

   fun combine_messages (NONE, msg2) = msg2
     | combine_messages (msg1, NONE) = msg1
     | combine_messages (SOME s1, SOME s2) = SOME(s1 ^ s2)

  fun potentially_generalisable n effect =
      noSome (E.level_of effect) ".potentially_generalisable: not variable"
      > n

  fun tickSomeBound () = ()
  fun tickNoBound () = ()

  fun checkSigma (sigma as FORALL([],[],[],tau)) = (tickNoBound(); sigma)
    | checkSigma sigma = (tickSomeBound(); sigma)

  exception MONOMORPHIC of E.cone * sigma

  fun visit node = E.get_visited node := true
  fun unvisit node = E.get_visited node := false

  fun unify_generic_secondary_epss (cone,n,reachable_nodes, principal_nodes): E.cone =
      (List.app visit principal_nodes;
       let val secondary_epss = List.foldl (fn (reachable_node, acc) =>
                if E.is_arrow_effect reachable_node then
                   if !(E.get_visited reachable_node) then (* primary *) acc
                   else if potentially_generalisable n reachable_node
                        then reachable_node :: acc
                        else acc
                else acc) [] reachable_nodes
       in
         List.app unvisit principal_nodes;

         case secondary_epss of
             [] => cone
           | (x::xs) => List.foldl (fn (eps,cone) => E.unifyEps(eps,x) cone) cone xs
       end)

  (* partition_rhos rhos partitions rhos into region variables that have the
     same runtime type *)

  fun skey rho = case E.get_place_ty rho of SOME rt => E.ord_runType rt | _ => die "skey"

  fun partition_rhos (rhos:place list): place list list =
      let val sorted_rhos = ListSort.sort (fn rho1 => fn rho2=> skey rho1 <= skey rho2) rhos
          fun runs ([],acc) = acc
            | runs (x::xs,(y::ys)::acc) =
              if skey x = skey y then runs(xs,(x::y::ys)::acc)
              else runs(xs,[x]::(y::ys)::acc)
            | runs (x::xs,nil::acc) = runs(xs,[x]::acc)
            | runs (x::xs,[]) = runs(xs,[x]::nil)
(*
          fun runs [] = []
            | runs (x::xs) =
                let val (run1, rest)= EdList.splitFirst (fn rho => skey rho <> skey x) xs (* DELETE *)
                                      handle _ => (xs, [])
                in (x::run1) :: runs rest
                end
*)
      in runs (sorted_rhos,nil)
      end

  fun unifyRhos (rhos as [], cone): place * cone =
      die ".unifyRhos applied to empty list of region variables"
    | unifyRhos (rho::rhos, cone) =
      (rho, List.foldl (fn (rho',cone)=> E.unifyRho(rho',rho) cone) cone rhos)

  fun unify_rho_partition (cone0, partition: place list list): place list * E.cone =
      List.foldr (fn ((l : place list), (representatives,cone)) =>
                     let val (rho', cone) = unifyRhos(l, cone)
                     in (rho' :: representatives, cone) end
                 ) ([], cone0) partition

  fun unifyEpss (epss as [], cone): place * cone =
      die ".unifyEpss applied to empty list of effect variables"
    | unifyEpss (eps::epss, cone) =
      (eps, List.foldl (fn (eps',cone) =>
                           E.unifyEps(eps',eps) cone) cone epss)

  fun unify_eps_partition (cone0, partition: place list list): effect list * E.cone =
      List.foldr (fn ((l : place list), (representatives,cone)) =>
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
        List.app visit bound_primary;
        loop(tau_nodes_in_fixed_order, 0)
      end

  fun set_pix_of_secondary_epss [] = ()
    | set_pix_of_secondary_epss [eps] = E.pix eps:= ~15
    | set_pix_of_secondary_epss _ =
          die "set_pix_of_secondary_epss: there was only supposed \
             \to be one secondary generalisable effect variable left"

  fun set_pix_of_secondary_rhos rhos : unit =
      List.app (fn rho => (E.pix rho := skey rho * ~10)) rhos

(*
  fun check_eff s e =
      let fun check_rho s r = (* mael 2004-10-19 *)
              let val k = E.key_of_eps_or_rho r
              in if k = 1 andalso not(E.eq_effect(E.toplevel_region_withtype_top,r)) then
                   die ("check_eff: " ^ s)
                 else ()
              end
      in
        if E.is_put e orelse E.is_get e then
          check_rho (s ^ ".putget") (E.rho_of e)
        else if E.is_rho e then check_rho (s ^ ".rho") e
        else ()
      end
*)

  fun pr_mu s mu =
      print ("\n" ^ s ^ ": " ^ PP.flatten1(#2 (mk_layout false) mu) ^ "\n")

  fun pr_effects s effs =
      print ("\n" ^ s ^ ": " ^ PP.flatten1(PP.layout_list E.layout_effect effs) ^ "\n")

  fun regEffClos0 (pr_lv, B: E.cone, B_0: int, phi: E.effect, tau: Type, ann: E.effect list) : E.cone * sigma =
      let
        (*val _ = Profile.profileOn()*)
(*
        val _ = logsay("\nregEffClos enter, tau = ");
        val (lay_ty, _) = mk_layout false
        val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
*)
        val n = B_0
(*
        val _ = print ("\nphi = " ^ PP.flatten1(E.layout_effect_deep phi) ^ "\n")
*)
        val B_1 = E.lower B_0 phi B

        val annotations = ann_ty tau []

        (* if there are no potentially generalisable nodes, we can escape right away,
           without going into the expensive operation of contracting effects *)
        val _ = if List.exists (potentially_generalisable n) annotations then ()
                else raise MONOMORPHIC(B_1,FORALL([],[],[],tau))

        (* make sure there is at most one generalisable secondary effect variable *)
        val reachable_nodes = E.subgraph annotations

(*
        val _ = app (check_eff "regEffClos2") reachable_nodes
                handle X =>
                       let
                         val _ = logsay("\nregEffClos enter, tau = ");
                         val (lay_ty, _) = mk_layout false
                         val _ = PP.outputTree(logsay,lay_ty tau,!Flags.colwidth)
                         val _ = print ("\nphi = " ^ PP.flatten1(E.layout_effect_deep phi) ^ "\n")
                       in raise X
                       end
  *)

        val B_2 = unify_generic_secondary_epss(B_1,n,reachable_nodes, annotations)

(*
        val _ = app (check_eff "regEffClos3") reachable_nodes
*)

        val subgraph = E.contract_effects annotations
                    (* nodes in "subgraph" are listed in bottom-up order, without
                       duplicates *)

        val frv_tau = List.filter E.is_rho subgraph  (* no duplicates in frv_tau *)
        val pfrv_tau = pfrv tau  (* syntactic order *)
        val problematic_secondary_frv_tau =  (* no duplicates *)
            List.filter (potentially_generalisable n)
                        (E.setminus(frv_tau, pfrv_tau))

        val (bound_secondary_rhos, B_3) =
            unify_rho_partition(B_2,
                                partition_rhos problematic_secondary_frv_tau)
        val _ = set_pix_of_secondary_rhos bound_secondary_rhos

        val primary_bound_rhos = E.remove_duplicates(List.filter (potentially_generalisable n) pfrv_tau)
        val _ = set_pix_primary(primary_bound_rhos, pfrv_tau)
        val bound_rhos = bound_secondary_rhos @ primary_bound_rhos

        val fev_tau = List.filter E.is_arrow_effect subgraph (* bottom-up order, no duplicates *)
        val pfev_tau = pfev tau      (* syntactic order *)
        val problematic_secondary_fev_tau =  List.filter (potentially_generalisable n)
                                                         (E.setminus(fev_tau,pfev_tau))
        val _ = set_pix_of_secondary_epss problematic_secondary_fev_tau

        (* deal with tv-annotated epss that have not yet been dealt with; give them pix-numbers according to
         * how they appear in quantified type variable specs... *)
        val epss_tvs = List.filter (potentially_generalisable n)
                                   (E.setminus(E.remove_duplicates ann,fev_tau))

(*
        val () =
            case ann of
                nil => ()
              | _ => pr_effects ("Quantified special effect vars for function " ^ pr_lv()) epss_tvs
*)

        val bound_epss = List.filter (potentially_generalisable n) (fev_tau @ epss_tvs) (* bottom-up order *)
        val _ = set_pix_primary(E.setminus(bound_epss,problematic_secondary_fev_tau), pfev_tau @ epss_tvs)
        val sigma = FORALL(bound_rhos, bound_epss, [], tau)

        (* debugging
        val _ = logsay("regEffClos leave, sigma = \n");
        val lay_sigma = mk_lay_sigma false
        val _ = PP.outputTree(logsay,lay_sigma sigma,!Flags.colwidth)
        *)
      in
        (B_3, sigma) (*before Profile.profileOff()*)
      end handle MONOMORPHIC result => result
               | X => (print "regEffClos failed\n"; raise X)

  fun regEffClos (B: E.cone, B_0: int, phi: E.effect, tau: Type) : E.cone * sigma =
      regEffClos0 (fn () => "uggh", B, B_0, phi, tau, nil)

  fun generalize_all (cone, level: int, alphas, tau): cone * sigma =
      let val ann = List.foldl (fn ((_,SOME e),a) => e::a | (_,a) => a) nil alphas
          val (cone,sigma) = regEffClos0(fn () => "generalize_all", cone,level,E.empty,tau,ann)
      in (cone, insert_alphas(alphas,sigma))
      end

  (* alpha_rename(sigma, B) ->  sigma
     sigma' = alpha_rename(sigma, B):
     B is a cone covering the free part of sigma (but not the bound part): level B = level sigma
     sigma' is a version of sigma which uses fresh bound region and effect variables
  *)

  fun alpha_rename (sigma, B: E.cone): sigma =
      let val FORALL(rhos,epss,alphas,tau) = sigma
          val () = case alphas of
                       nil => ()
                     | _ => die "alpha_rename: quantified alphas not supported"
        val c = E.push B
        val (rhos', c) = E.renameRhos(rhos,c)
        val (epss', c) = E.renameEpss(epss,c)
        val (tau',c) = inst (FORALL(rhos,epss,[],tau),(rhos',epss',[])) c
        val sigma' = FORALL(rhos', epss', [], tau')
        val (_, c) = E.pop c
    in
        sigma'
    end
(*
  fun alpha_rename'((rhos,epss,tau), B: E.cone): sigma =
    let
        val c = E.push B
        val (rhos', c) = E.renameRhos(rhos,c)
        val (epss', c) = E.renameEpss(epss,c)
        val (tau',c) = inst (FORALL(rhos,epss,[],tau),(rhos',epss',[])) c
        val sigma' = FORALL(rhos', epss', [], tau')
        val (_, c) = E.pop c
    in
        sigma'
    end
*)

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

  fun alpha_equal (sigma1 as FORALL(rhos1,epsilons1,alphas1,tau1),
                   sigma2 as FORALL(rhos2,epsilons2,alphas2,tau2)) cone : bool =  (* MAEL MEMO: here we could probably also check equality of the instantiated effects associated with the alphas *)
      let val cone = E.push cone
        (*val _ = logsay "enter alpha_equal\n"
          val _ = logsay "sigma1=\n"
          val _ = PP.outputTree(logsay,layout_sigma sigma1,!Flags.colwidth)
          val _ = logsay "sigma2=\n"
          val _ = PP.outputTree(logsay,layout_sigma sigma2,!Flags.colwidth)
         *)
          val () = case (alphas1, alphas2) of
                       (nil, nil) => ()
                     | _ => die "alpha_equal: quantified alphas not supported"
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
            val fresh_epss'_with_ints = BasisCompat.ListPair.zipEq(fresh_epss', eps_indices)
            val (fresh_epss'', cone) = E.freshEpss(epsilons1,cone)
            val fresh_epss''_with_ints = BasisCompat.ListPair.zipEq(fresh_epss'', eps_indices)
            val (fresh_rhos_of_epss, cone) = E.freshRhos(epsilons1, cone)
            val _ = List.app (fn (eps, rho) => E.edge(eps, E.mkPut rho))
                               (BasisCompat.ListPair.zipEq(fresh_epss',fresh_rhos_of_epss))
            val _ = List.app (fn (eps, rho) => E.edge(eps, E.mkPut rho))
                               (BasisCompat.ListPair.zipEq(fresh_epss'',fresh_rhos_of_epss))
            val Se' = map (fn (bound_eps,ix) =>
                              (bound_eps, case List.find (fn (new_eps,ix') => ix=ix') fresh_epss'_with_ints of
                                              SOME (e,_) => e
                                            | NONE => die "alpha_equal.impossible")
                          )
                          epsilons_and_ints1

            val (tau', cone, updates, _) =
                 instAux(([],BasisCompat.ListPair.zipEq(rhos1,fresh_rhos),Se'),tau1) cone
                   handle x => (say "first call\n";
                      List.app (fn node => say(PP.flatten1(E.layout_effect node)))
                                 (rhos1 @ epsilons1); raise x)

            val Se'' = map (fn (bound_eps,ix) =>
                               (bound_eps, case List.find (fn (new_eps,ix') => ix=ix') fresh_epss''_with_ints of
                                               SOME (e,_) => e
                                             | NONE => die "alpha_equal.impossible2")
                           )
                          epsilons_and_ints2

            val (tau'', cone, updates, _) =
                 instAux(([],BasisCompat.ListPair.zipEq(rhos2,fresh_rhos), Se''),tau2) cone
                   handle x => (say "second call\n";
                      List.app (fn node => say(PP.flatten1(E.layout_effect node)))
                                 (rhos2 @ epsilons2); raise x)
          in
            (List.all E.eq_effect
                        (BasisCompat.ListPair.zipEq
                             (E.remove_duplicates(pfrv tau'),
                              E.remove_duplicates(pfrv tau'')))
             )
            andalso
            ((*logsay "regions correspond\n";*)
             List.all E.sameEffect
                        (BasisCompat.ListPair.zipEq
                             (E.remove_duplicates(pfev tau'),
                              E.remove_duplicates(pfev tau'')))
             )
          end
         ) footnote' (fn b => ((*logsay ("leave alpha_equal: " ^ Bool.string b ^ "\n");*) E.pop cone))
      end handle BasisCompat.ListPair.UnequalLengths =>
                 ((*logsay "leave alpha_equal: false\n";*) false (*before say "zip raised"*) )

  (**************************)
  (* Matching type schemes  *)
  (**************************)

  exception FAIL_MATCH of string

  (* (f: il * cone -> il * cone) = mk_transformer(origins: int list list * int list list):
     f is a function which selects and unifies members of its argument
     instantiation list, according to the partitioning given by origins.
  *)

  fun select_and_unify (oldvars: '_var list, origins, unify, cone): '_var list * cone =
      let
        val a = Array.fromList oldvars
        val var_classes = map (map (fn ix => Array.sub(a, ix))) origins
      in
        unify(cone, var_classes)
      end handle _ => raise FAIL_MATCH "select_and_unify"

  fun mk_transformer (origins as (rho_origins: int list list, eps_origins: int list list))
                     ((old_rhos:place list, old_epss:effect list,taus), cone) : il * cone =
      let
        val (new_rhos, cone) = select_and_unify(old_rhos,rho_origins,unify_rho_partition,cone)
        val (new_epss, cone) = select_and_unify(old_epss, eps_origins, unify_eps_partition, cone)
      in
        ((new_rhos,new_epss,taus), cone)
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

  fun select_empty ([]::rest,rho::rhos)= rho::select_empty(rest,rhos)
    | select_empty (_::rest,_::rhos) = select_empty(rest,rhos)
    | select_empty _ = []

  (*
     enumerate(l)  = [[0], [1], ...., [length(l)]]
  *)

  fun enumerate l =
      let fun loop(ix, []) = []
            | loop(ix, _ :: xs) = [ix] :: loop(ix+1,xs)
      in
        loop(0, l)
      end

  (* (transformer: il * cone -> il * cone) = matchSchemes(sigma, sigma')
     Assumption: sigma >= sigma' via a substitution S which is
     represented implicitly by links in the union-find data structure
     which implements region and effect variables. *)

  fun add (i: int) (j: int) = i+j

  fun merge ([], l2) = l2
    | merge (l::ls, [] :: l2) = l :: merge(ls, l2)
    | merge (l::ls, ixs :: l2) = ixs :: merge(l::ls,l2)
    | merge _ = die ".merge: ill-formed merge"

  fun fail_aux (sigma,sigma'): unit =
      (logsay "MatchSchemes: matching of type schmes failed\n";
       logsay "  the supposedly more general type scheme :\n    ";
       log_tree (layout_sigma sigma); logsay "\n";
       logsay "  the supposedly less general type scheme :\n    ";
       log_tree (layout_sigma sigma'); logsay "\n")

  fun failwith (x,sigma,sigma') =
      (fail_aux(sigma,sigma');
       raise x
      )

  fun matchSchemes (sigma as FORALL(rhos, epss,[],tau),
                    sigma' as FORALL(rhos', epss',[],tau')) :
      (il * cone) -> (il * cone) =
   (let
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
                           map (map(add(List.length add_rhos))) rhos'_origins)

      val epss'_origins = map (find_origin epss) epss'
      val add_epss = select_empty(epss'_origins, epss')
      val epss'_origins_extended =
             case add_epss of
               [] => (* common special case: *) epss'_origins
             | _  => merge(enumerate add_epss,
                           map (map(add(List.length add_epss))) epss'_origins)

      val thin = mk_transformer(rhos'_origins_extended, epss'_origins_extended)
    in
      fn ((old_rhos, old_epss, old_taus), cone) =>
         (let val (new_rhos, cone) = E.cloneRhos(add_rhos, cone)
              val (new_epss, cone) = E.cloneEpss(add_epss, cone)
          in
              thin ((new_rhos@old_rhos, new_epss @ old_epss, old_taus), cone)
          end handle x => failwith (x,sigma,sigma'))
    end  handle x => failwith(x,sigma,sigma')
    )
  | matchSchemes _ = raise FAIL_MATCH "matchSchemes: type scheme had bound type variables"


  (* Whether word32 and int32 (and word64 and int64) types are boxed
   is determined dynamically in SpreadExpression on the basis of the
   function TyName.unboxed(tn), which depends on the flag
   tag_values. At the stage of region inference, integer and word
   types are resolved to be either word8, word31, word32, word63,
   word64, int31, int32, int63, or int64. The default integer type is
   dynamically determined to be the largest integer type that fits in
   64 bits (i.e., int63 or int64, depending on tagging scheme);
   similarly for words.
   *)

  val int31Type     : Type = CONSTYPE(TyName.tyName_INT31,[],[],[])
  val int32Type     : Type = CONSTYPE(TyName.tyName_INT32,[],[],[])
  val int63Type     : Type = CONSTYPE(TyName.tyName_INT63,[],[],[])
  val int64Type     : Type = CONSTYPE(TyName.tyName_INT64,[],[],[])
  val word8Type     : Type = CONSTYPE(TyName.tyName_WORD8,[],[],[])
  val word31Type    : Type = CONSTYPE(TyName.tyName_WORD31,[],[],[])
  val word32Type    : Type = CONSTYPE(TyName.tyName_WORD32,[],[],[])
  val word63Type    : Type = CONSTYPE(TyName.tyName_WORD63,[],[],[])
  val word64Type    : Type = CONSTYPE(TyName.tyName_WORD64,[],[],[])

  val exnType       : Type = CONSTYPE(TyName.tyName_EXN,[],[],[])
  val boolType      : Type = CONSTYPE(TyName.tyName_BOOL,[],[],[])
  val realType      : Type = CONSTYPE(TyName.tyName_REAL,[],[],[])
  val f64Type       : Type = CONSTYPE(TyName.tyName_F64,[],[],[])
  val stringType    : Type = CONSTYPE(TyName.tyName_STRING,[],[],[])
  val chararrayType : Type = CONSTYPE(TyName.tyName_CHARARRAY,[],[],[])

  val unitType      : Type = RECORD[]

  fun isF64Type t =
      case t of
          CONSTYPE(tn,_,_,_) => TyName.eq(tn,TyName.tyName_F64)
        | _ => false

  fun unboxed t =
    case t
      of RECORD[] => true
       | CONSTYPE(tn,_,_,_) => TyName.unboxed tn
       | _ => false

  (*the following two functions are used only when spreading ccalls (in
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

  fun unify_rhos_on_same_tyvars mu B = #2 (unify_rhos_on_same_tyvars0 (mu,
        (L.TyvarMap.empty : place L.TyvarMap.map, B : cone)))
  and unify_rhos_on_same_tyvars0 ((tau, rho), (ttr, B)) =
        (case tau of
           TYVAR tyvar =>
             (case L.TyvarMap.lookup ttr tyvar of
                SOME rho' => (ttr, E.unifyRho (rho, rho') B)
              | NONE =>      (L.TyvarMap.add (tyvar, rho, ttr), B))
         | CONSTYPE (tyname, mus, _, _) =>
             unify_rhos_on_same_tyvars00 (mus, (ttr, B))
         | RECORD mus => unify_rhos_on_same_tyvars00 (mus, (ttr, B))
         | FUN (mus1, _, mus2) =>
             unify_rhos_on_same_tyvars00 (mus1,
             (unify_rhos_on_same_tyvars00 (mus2, (ttr, B)))))
  and unify_rhos_on_same_tyvars00 (mus, (ttr, B)) =
        List.foldl unify_rhos_on_same_tyvars0 (ttr, B) mus

  (*c_function_effects mus = the `rhos_for_result' to be annotated on
   a ccall; see comment in MUL_EXP.*)

  local
      fun size_of_tyname tyname =
        if TyName.unboxed tyname then SOME 0
        else if TyName.eq (tyname, TyName.tyName_REAL) then
          SOME (RegConst.size_of_real ())
        else if (TyName.eq (tyname, TyName.tyName_WORD32)
                 orelse TyName.eq (tyname, TyName.tyName_INT32)
                 orelse TyName.eq (tyname, TyName.tyName_INT64)
                 orelse TyName.eq (tyname, TyName.tyName_WORD64)) then
          (* boxed because RegConst.unboxed_tyname(tyname) returned false! *)
          SOME (RegConst.size_of_record [1]) (* 2001-02-17, Niels - dummy list [1] with one element! *)
        else if (TyName.eq (tyname, TyName.tyName_STRING)
                 orelse TyName.eq (tyname, TyName.tyName_CHARARRAY)
                 orelse TyName.eq (tyname, TyName.tyName_ARRAY)
                 orelse TyName.eq (tyname, TyName.tyName_VECTOR)) then NONE
        else die ("S (CCALL ...): \nI am sorry, but c functions returning "
                  ^ TyName.pr_TyName tyname
                  ^ " are not supported yet.\n")

      fun below_list _ = NONE (*i.e., `yes, we are below a list constructor'*)

      fun c_function_effects1 in_list ((tau_schema,_),(tau, rho)) =
        case (tau_schema, tau)
          of (TYVAR _,           _) => []
           | (CONSTYPE (_, mus_schema, _, _), CONSTYPE (tyname, mus, rhos, epss)) =>
            if TyName.eq (tyname, TyName.tyName_LIST) then
              (case (mus_schema, mus, rhos) of
                 ([mu1_schema], [mu1], [rho1]) =>
                   (*rho is for cons cells & rho1 is for auxiliary pairs*)
                   [(rho, NONE), (rho1, NONE)] @ c_function_effects1 below_list (mu1_schema,mu1)
               | _ => die "c_function_effects1: strange list type")
            else [(rho, in_list (size_of_tyname tyname))]
           | (RECORD nil, RECORD nil) => [(rho, SOME 0)] (*unit is not allocated*)
           | (RECORD mus_schema, RECORD mus) =>
             (rho, in_list (SOME (RegConst.size_of_record mus)))
             :: List.concat (map (c_function_effects1 in_list) (BasisCompat.ListPair.zipEq(mus_schema,mus)
                                                                 handle _ => die "c_function_effects1.zip"))
             (*it is assumed that List.concat does not concat the lists in
              opposite order, i.e., that concat_list [[1,2], [3], [4]] is
              [1,2,3,4] and not [4,3,1,2]*)
           | (FUN _, FUN (mus, eps0, mus')) => die "c_function_effects1 (FUN ...)"
           | _ => die "c_function_effects1: schema does not match instance"
  in
    fun c_function_effects (FORALL(_,_,_,tau), mu) : (place * int option) list =
      case tau
        of FUN(_,_,[mu_schema]) => c_function_effects1 (fn i=>i) (mu_schema, mu)
         | _ => die "c_function_effects.expecting function type with one return value"
  end

  local
    fun add_rho (rho,acc) =
        case E.get_place_ty rho of
            SOME E.WORD_RT => acc
          | SOME _ => if !(E.get_visited rho) then acc
                      else (E.get_visited rho := true; rho::acc)
          | NONE => die "add_rho"
    fun add_rhos (rhos,acc) = foldl add_rho acc rhos
    fun fv_mus (mus,acc) = foldl fv_mu acc mus
    and fv_mu ((tau,rho),acc) =
        case tau of
            TYVAR tyvar => acc
          | CONSTYPE (tyname, mus, rhos, nil) => fv_mus(mus,add_rho(rho,add_rhos(rhos,acc)))
          | CONSTYPE (tyname, _, _, _) => die "frv_except_tyvar_rhos.non-empty arrow-effect set"
          | RECORD mus => fv_mus(mus,add_rho(rho,acc))
          | FUN (mus, eps0, mus') => acc
             (*die "frv_except_tyvar_rhos1"*)
             (* support 'pointer' : 'a -> foreignptr function with 'a instantiated to a function *)
  in
    fun frv_except_tyvar_rhos mus =
      let val rhos = fv_mus (mus, nil)
      in rhos before app (fn r => E.get_visited r := false) rhos
      end
  end

  fun sigma_for_c_function tyvars mu B =
      let val B = unify_rhos_on_same_tyvars mu B
          val tyvars = map (fn tv => (tv,NONE)) tyvars
        in
          (case mu of
             (FUN (mus1, eps0, mus2), rho) =>
               let (* val _ = pr_mu "cf1" mu *)
                   val rhos_get = frv_except_tyvar_rhos mus1
                   val rhos_put = frv_except_tyvar_rhos mus2
(*                 val _ = pr_effects "rhos_get" rhos_get
                   val _ = pr_effects "rhos_put" rhos_put
*)
                   val _ =
                       case map E.mkGet rhos_get @ map E.mkPut rhos_put of
                           nil => ()
                         | rhos_gets_puts =>
                               (*insert effects on the arrow in mu*)
                               E.edge (eps0, E.mkUnion rhos_gets_puts)
               in
                 let (* val _ = pr_mu "cf2" mu *)
                     val (B, sigma) = generalize_all (B, 0, tyvars, #1 mu)
                   handle X => (print ("generalize_all failed\n"); raise X)
                 in (sigma, B)
                 end
               end
           | _ => die "sigma_for_c_function")
        end

  (* Picklers *)
  val pu_mu : Type Pickle.pu -> (Type * place) Pickle.pu
      = Pickle.cache "mu" (fn pu_Type => Pickle.nameGen "RType.mu"
                           (Pickle.pairGen(pu_Type,E.pu_effect)))

  val pu_mus : Type Pickle.pu -> (Type * place) list Pickle.pu
      = Pickle.cache "mus" (Pickle.nameGen "RType.mus" o Pickle.listGen o pu_mu)

  val pu_Type =
      let fun toInt (TYVAR _) = 0
            | toInt (CONSTYPE _) = 1
            | toInt (RECORD _) = 2
            | toInt (FUN _) = 3
          fun fun_TYVAR _ =
              Pickle.con1 TYVAR (fn TYVAR a => a | _ => die "pu_Type.TYVAR")
              L.pu_tyvar
          fun fun_CONSTYPE pu_Type =
              Pickle.con1 CONSTYPE (fn CONSTYPE a => a | _ => die "pu_Type.CONSTYPE")
              (Pickle.tup4Gen0(TyName.pu,pu_mus pu_Type,E.pu_effects,E.pu_effects))
          fun fun_RECORD pu_Type =
              Pickle.con1 RECORD (fn RECORD a => a | _ => die "pu_Type.RECORD")
              (pu_mus pu_Type)
          fun fun_FUN pu_Type =
              Pickle.debugUnpickle "FUN"
              (Pickle.con1 FUN (fn FUN a => a | _ => die "pu_Type.FUN")
               (Pickle.tup3Gen0(pu_mus pu_Type,E.pu_effect,pu_mus pu_Type)))
      in Pickle.dataGen("RType.Type",toInt,[fun_TYVAR,fun_CONSTYPE,fun_RECORD,fun_FUN])
      end
  val pu_mu = pu_mu pu_Type

  val pu_tyvars = Pickle.listGen (Pickle.pairGen (L.pu_tyvar,Pickle.optionGen E.pu_effect))
  val pu_sigma =
      Pickle.convert (FORALL, fn FORALL a => a)
      (Pickle.tup4Gen0(E.pu_effects,E.pu_effects,pu_tyvars,Pickle.debugUnpickle "Type" pu_Type))

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
