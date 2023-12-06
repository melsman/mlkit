
structure Mul: MUL =
struct
  structure Eff = Effect
  structure Lam = LambdaExp
  structure Lvar = Lvars
  structure RSE = RegionStatEnv
  structure PP = PrettyPrint
  structure QM_EffVarEnv =
      QuasiEnv(structure OFinMap = EffVarEnv
               val key = Effect.key_of_eps_or_rho
               val eq = Effect.eq_effect)

  structure LvarMap = Lvar.Map

  structure GlobalEffVarEnv = QM_EffVarEnv.Env

  (* auxiliaries *)
  fun say s = print (s^"\n")
  fun mes s = TextIO.output(!Flags.log, s)
  fun outtree t = PP.outputTree(print, t, !Flags.colwidth)

  infix footnote
  fun x footnote y = x

  fun die s = (mes("Mul: " ^ s ^ "\n");
               Crash.impossible("Mul: " ^ s ^ "\n"))

  type lvar = Lam.lvar
  type place = Eff.place
  type cone = Eff.cone
  structure Effect = Eff

  fun rho_of ae = Eff.rho_of ae

  datatype mul = INF | NUM of int
  val K =  1 (* the largest finite multiplicity *)

  type ateffect = Eff.effect

  (* A multiplicity effect psi is a list of pairs  (ae:ateffect, m: mul). Note
     that m may be zero. Atomic effects with multiplicity zero are still important,
     (i.e., they cannot just be removed) cf comments on arrow effects below *)

  type mulef = (ateffect * mul) list


  (* A multiplicity arrow effect is a pair mularef = (eps,psi)
     of an effect variable and a multiplicty effect. Here psi may contain atomic
     effects with multiplicity zero. Also the multiplicity arrow effect may be
     cyclic, i.e., eps in Dom(psi). By keeping the zero-multiplicities in psi,
     we make sure that it makes no difference whether one first "contracts" a
     cycle - making all multiplicities in psi INF - and then increase the multiplicity
     of an atomic effect in Dom psi OR do those two operations in the reverse order.

     We maintain an invariant that whenever a plain arrow effect eps.phi appears
     in the derivation of the input term to multiplicity inference then eps.psi
     appears at the corresponding place during multiplicity inference, for some
     psi which satisfies:

           Dom psi = phi
  *)

  type effectvar = Eff.effect                  (* eps *)

  type effect = Eff.effect list                (* phi *)
  type arroweffect = effectvar * effect        (* areff or eps.phi *)

  type mularef = effectvar * mulef             (* eps.psi *)
  type mularefset = mularef list               (* Psi'    *)

  (* A multiplicity arrow effect map maps a effect variable to the
     multiplicity arrow effect which the effect variable stands for.
     Just one global such map is used. The multiplicity arrow effects
     in its range increase monotonically during multiplicity inference.

     The multiplicity arrow effects in the range are shared (hence the "ref" below).
     The idea is that if Psi(eps) = r as ref(eps.psi) and eps' \in fev(dom(psi))
     and the multiplicities of eps' are increased, then that increment must also
     be made for eps. This dependency is recorded in a dependency map (see below)
     which maps eps' to a list [..., r, ...] of references to semantic objects that contain a
     free occurrence of eps'. Thus only one occurrence of the semantic object needs
     to be updated. *)

  structure EffVarEnv =
    struct
      type 'a map = 'a QM_EffVarEnv.qmap

      val lookup = QM_EffVarEnv.lookup
      fun layoutMap format f g Psi = QM_EffVarEnv.layout format f g Psi

      fun add (eps,range,qmap) = (QM_EffVarEnv.update(eps,range,qmap);qmap)
      val plus = QM_EffVarEnv.Env.plus
      fun Fold f recipient (source:mularef ref map) =
           QM_EffVarEnv.Fold (fn ((key, y), acc) => f((key,y), acc)) recipient source
    end

  val plus_mularefmap = EffVarEnv.plus
  val empty_mularefmap = GlobalEffVarEnv.empty
  val initial_mularefmap =
    let val _ = Eff.eval_phis [Eff.toplevel_arreff]
        val mulef = map (fn ae => (ae,INF)) (Eff.represents Eff.toplevel_arreff)
        val mularef = (Eff.toplevel_arreff,mulef)
    in GlobalEffVarEnv.add(Eff.toplevel_arreff, ref mularef, GlobalEffVarEnv.empty)
    end

  type mularefmap = mularef ref GlobalEffVarEnv.map        (* cross-module Psi *)
  type imp_mularefmap = mularef ref EffVarEnv.map          (* intra-compilation-unit Psi *)
  fun combine (Psi_global: mularefmap, Psi_local: imp_mularefmap): imp_mularefmap=
       QM_EffVarEnv.combine(Psi_global, Psi_local)

  fun summul (NUM n1, NUM n2) = if n1 + n2 > K then INF else NUM (n1 + n2)
    | summul _ = INF

  fun compress [] = []
    | compress [(ae,m)] = [(ae,m)]
    | compress ((ae1,m1)::(ae2,m2)::rest) =
        if Eff.eq_effect(ae1, ae2) then
           compress((ae1, summul(m1,m2))::rest)
        else (ae1,m1)::compress((ae2,m2)::rest)

  fun equal_mulef (mulef1,mulef2) =
    let val sort = ListSort.sort (fn (ae1,_) => fn (ae2,_) => Eff.ae_lt(ae1,ae2))
        val mulef1 = compress(sort mulef1)
        val mulef2 = compress(sort mulef2)
        fun eq ([],[]) = true
          | eq ((ae1,mul1)::mulef1,(ae2,mul2)::mulef2) =
          Eff.eq_effect(ae1,ae2) andalso mul1=mul2 andalso eq (mulef1,mulef2)
          | eq _ = false
    in eq(mulef1,mulef2)
    end handle Eff.AE_LT => die "equal_mulef.effect in dom of mulef1 or mulef2 not atomic"

  fun equal_mularef ((eps1,mulef1),(eps2,mulef2)) =
      Eff.eq_effect(eps1,eps2) andalso equal_mulef(mulef1,mulef2)

  fun enrich_mularefmap (mularefmap1,mularefmap2) =
      GlobalEffVarEnv.Fold(fn ((effectvar2,ref res2),b) => b andalso
                           case GlobalEffVarEnv.lookup mularefmap1 effectvar2
                            of SOME (ref res1) => equal_mularef(res1,res2)
                             | NONE => false) true mularefmap2

  fun restrict_mularefmap (mularefmap,effectvars) =
    List.foldl(fn (effectvar,acc) =>
               case GlobalEffVarEnv.lookup mularefmap effectvar
                 of SOME res => GlobalEffVarEnv.add(effectvar,res,acc)
                  | NONE => die "restrict_mularefmap") GlobalEffVarEnv.empty effectvars

  (* A quantified multiplicity arrow effect set represents a type scheme.
     An effect environment is a finite map from program variables (lvars) to
     (refs to) quantified multiplicity arrow effect sets. *)

  type qmularefset = (effectvar list * place list * mularefset) * place option     (* Xi *)
  val empty_qmularefset :qmularefset = (([], [],[]), SOME Eff.toplevel_region_withtype_top )

  type efenv = qmularefset ref LvarMap.map

  fun restrict_efenv (efenv,lvars) =
    foldl(fn (lv,acc) =>
          case LvarMap.lookup efenv lv
            of SOME res => LvarMap.add(lv,res,acc)
             | NONE => die "restrict_efenv") LvarMap.empty lvars

  (* normalize_qmularefset(qmularefset,sigma) normalizes qmularefset
   * with respect to the order arroweffects occur in tau of sigma.
   * Rhos are normalized during S and R. *)
  fun normalize_qmularefset (qmularefset : qmularefset,sigma) : qmularefset =
    let fun visit n = Eff.get_visited n := true
        fun unvisit n = Eff.get_visited n := false
        val ((epss,rhos,mularefs),rho) = qmularefset
        val epss' = Eff.remove_duplicates (List.filter Eff.is_arrow_effect (RType.ann_sigma sigma []))
        val _ = List.app visit epss
        val epss' = List.filter (! o Eff.get_visited) epss'
        val _ = List.app unvisit epss
        fun shuffle [] = []
          | shuffle (eps::epss) =
            let fun grep [] = die "normalize_qmularefset.shuffle"
                  | grep ((mularef as (eps',mulef))::mularefs) = if Eff.eq_effect(eps,eps') then mularef
                                                                 else grep mularefs
            in grep mularefs :: shuffle epss
            end
    in ((epss',rhos,shuffle epss'),rho)
    end

  fun arity_qmularefset ((epss,rhos,_),_) : int * int = (List.length epss, List.length rhos)

  fun equal_mularefset ([],[]) = true
    | equal_mularefset (mularef1::mularefset1,mularef2::mularefset2) =
      equal_mularef(mularef1,mularef2) andalso equal_mularefset(mularefset1,mularefset2)
    | equal_mularefset _ = false

  fun instantiate_qmularefset (epss',rhos',((epss,rhos,mularefset),_) : qmularefset) : mularefset =
    let val eps_pairs = BasisCompat.ListPair.zipEq (epss,epss') handle BasisCompat.ListPair.UnequalLengths => die "instantiate_qmularefset.Zip"
        val rho_pairs = BasisCompat.ListPair.zipEq (rhos,rhos') handle BasisCompat.ListPair.UnequalLengths => die "instantiate_qmularefset.Zip"
        fun setInstance (node,node') = (Eff.get_instance node) := SOME node'
        fun clearInstance (node,node') = (Eff.get_instance node) := NONE
        fun on_ae ae = if Eff.is_arrow_effect ae then
                          case !(Eff.get_instance ae)
                            of SOME ae => ae
                             | NONE => ae
                       else if Eff.is_put ae then
                              case !(Eff.get_instance (rho_of ae))
                                of SOME rho => Eff.mkPut rho
                                 | NONE => ae
                       else if Eff.is_get ae then
                              case !(Eff.get_instance (rho_of ae))
                                of SOME rho => Eff.mkGet rho
                                 | NONE => ae
                       else die "on_ae.not atomic effect"
        fun on_mulef [] = []
          | on_mulef ((ae,mul)::mulef) = (on_ae ae,mul) :: on_mulef mulef
        fun on_mularef (eps,mulef) =
          case !(Eff.get_instance eps)
            of SOME node => (node, on_mulef mulef)
             | NONE => die "on_mularef.no forward info"
        fun on_mularefset [] = []
          | on_mularefset (mularef::mularefset) = on_mularef mularef :: on_mularefset mularefset
        val _ = List.app setInstance eps_pairs
        val _ = List.app setInstance rho_pairs
        val mularefset' = on_mularefset mularefset
        val _ = List.app setInstance eps_pairs
        val _ = List.app setInstance rho_pairs
    in mularefset'
    end

  fun equal_qmularefset ((qmularefset1:qmularefset,sigma1),(qmularefset2:qmularefset,sigma2)) =
      arity_qmularefset(qmularefset1) = arity_qmularefset(qmularefset2) andalso

     (* (1) normalize qmularefsets so that bvs comes in the order of first
      * occurrences in its type. (2) instantiate qmularefsets to fresh
      * rhos and arrow effects. (3) check for equality of instantiated
      * mularefsets. *)

      let val qmularefset1 as ((epss1,rhos1,_),_) = normalize_qmularefset(qmularefset1,sigma1)
          val qmularefset2 = normalize_qmularefset(qmularefset2,sigma2)
          val cone = Eff.push Eff.initCone
          val (fresh_epss,cone) = Eff.freshEpss(epss1,cone)
          val (fresh_rhos,cone) = Eff.freshRhos(rhos1,cone)
          val mularefset1 = instantiate_qmularefset(fresh_epss,fresh_rhos,qmularefset1)
          val mularefset2 = instantiate_qmularefset(fresh_epss,fresh_rhos,qmularefset2)
          val _ = Eff.pop cone
      in equal_mularefset(mularefset1,mularefset2)
      end

  type regionStatEnv = RSE.regionStatEnv
  fun enrich_efenv ((efenv1,rse1),(efenv2,rse2)) =
    LvarMap.Fold(fn ((lv2,ref res2),b) => b andalso
            case LvarMap.lookup efenv1 lv2
              of SOME (ref res1) =>
                let val sigma1 = case RSE.lookupLvar rse1 lv2
                                   of SOME a => #4 a
                                    | NONE => die "enrich_efenv.lv not in rse1"
                    val sigma2 = case RSE.lookupLvar rse2 lv2
                                   of SOME a => #4 a
                                    | NONE => die "enrich_efenv.lv not in rse2"
                in equal_qmularefset((res1,sigma1),(res2,sigma2))
                end
               | NONE => false) true efenv2

  fun placeof (_, b) = b

  (* Semantic objects that contain free effect variables which might have to be
     affected by substitution are accessed via references so that only one occurrence
     of each object needs to be affected by the substitution *)

  datatype shared = MULEFF    of mulef ref
                  | MULAREFF  of mularef ref
                  | MULSCHEME of qmularefset ref

  structure DepEnv =
    struct
      type 'a map = (int * 'a) list array
      val size = 999
      val empty: shared list map = Array.array(size, [])

      fun lookup _ eps =
        let val key = Effect.key_of_eps_or_rho eps
            val hash = key mod size
        in case List.find (fn (i':int,r) => i' = key) (Array.sub(empty, hash)) of
               SOME (_,res) => SOME res
             | NONE => NONE
        end
      fun layoutMap _ _ _ _ = PP.LEAF "(not implemented)"
      fun hash key = key mod size
      val key_of_toplevel_arreff = Effect.key_of_eps_or_rho Effect.toplevel_arreff
      fun add (eps, range, _) =
        let val key = Effect.key_of_eps_or_rho eps
            val hash = hash key
            val old_list = Array.sub(empty, hash)
        in
            if key <> key_of_toplevel_arreff
              then
              Array.update(empty,hash, (key,range)::old_list)
            else ();
            empty
        end
      fun reset () = (* reset all entries of the depency map array to [] and then
                       insert toplevel_arreff (e9) in the map with an empty list of
                       dependants. *)
          let fun loop n = if n>=size then ()
                           else
                           (Array.update(empty,n,[]);
                            loop(n+1))
          in loop 0;
             Array.update(empty,
                                    hash(key_of_toplevel_arreff),
                                    [(key_of_toplevel_arreff,[])])
          end

      fun plus (old_hash, new_hash) =  new_hash
     end

  type dependency_map = shared list DepEnv.map

  type efsubst = (effectvar*mularef)list

  fun reset_dep () = DepEnv.reset()

  (*pretty printing*)

  type StringTree = PP.StringTree
  fun layout_list children=
       PP.NODE{start = "[", finish = "]", indent = 1, childsep = PP.RIGHT ",",
               children = children}

  fun layout_set children=
       PP.NODE{start = "{", finish = "}", indent = 1, childsep = PP.RIGHT ",",
               children = children}

  fun layout_set_horizontal children=
       PP.HNODE{start = "{", finish = "}", childsep = PP.RIGHT ",",
               children = children}

  fun layout_pair(t1,sep,t2) =
         PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT sep,
                 children =   [t1,t2]}

  fun show_mul (NUM n) = Int.toString n
    | show_mul INF = "INF"

  fun layout_mul m = PP.LEAF (show_mul m)

  (* layout_ateffect ae sets and clears visited fields*)
  fun layout_ateffect ae =
       if Eff.is_arrow_effect ae then Eff.layout_effect ae
       else Eff.layout_effect_deep ae

  (* layout_effectvar: no side effect *)
  fun layout_effectvar eps = Eff.layout_effect eps

  (* show_effectvar: no side effect *)
  fun show_effectvar eps = PP.flatten1(layout_effectvar eps)

  (* layout_effect: sets and clears visited fields *)
  fun layout_effect (ef:effect) =
        layout_list (map layout_ateffect ef)

  (* layout_aref sets and clears visited fields *)
  fun layout_aref(eps, phi) =
        layout_pair(layout_ateffect eps, ".", layout_effect phi)


  (*  layout_atompair sets and clears visited fields *)
  fun layout_atompair (ae, mul) =
      layout_pair(layout_ateffect ae, ":", layout_mul mul)

  (*  layout_mulef sets and clears visited fields *)
  fun layout_mulef psi =
      layout_set_horizontal (map layout_atompair psi)

  (*  layout_mularef sets and clears visited fields *)
  fun layout_mularef (eps, psi)=
      layout_pair(layout_effectvar eps,".", layout_mulef psi)

  (*  layout_mularefset sets and clears visited fields *)
  fun layout_mularefset Psi =
      layout_set (map layout_mularef Psi)

(*
  (*  layout_effectvar_int: no side-effect *)
  fun layout_effectvar_int e = PP.LEAF(Int.toString (Effect.key_of_eps_or_rho e))
*)

  (*  layout_mularefmap sets and clears visited fields *)
  fun layout_mularefmap Psi =
      GlobalEffVarEnv.layoutMap{start = "Mularefmap: [", finish = "]", eq = " -> ", sep = ", "}
                               layout_effectvar (layout_mularef o !) Psi

  (*  layout_imp_mularefmap: sets and clears visited fields *)
  fun layout_imp_mularefmap Psi =
      EffVarEnv.layoutMap{start = "{", finish = "}", eq = "=", sep = ","}
                         layout_effectvar (layout_mularef o !) Psi

  (*  layout_effectvars: no side-effect *)
  fun layout_effectvars epss =
      PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT" ",
               children = map layout_effectvar epss}

  (*  layout_place : no side-effect *)
  fun layout_place p = Eff.layout_effect p

  (*  layout_places: no side-effects *)
  fun layout_places rhos =
      PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT" ",
              children = map layout_place rhos}
  fun at t =
      PP.NODE{start = " at ", finish = "", indent = 4, childsep = PP.NOSEP,
              children = [t]}

  (* layout_qmul: sets and clears visited fields *)
  fun layout_qmul (epses, rhos, Psi)=
      PP.NODE{start = "forall ", finish = "", indent = 7, childsep = PP.NOSEP,
              children = [layout_effectvars epses,
                          layout_places rhos,
                          layout_mularefset Psi]}

  fun layout_qmularefset (((epses, rhos, Psi), p):qmularefset)=
      PP.NODE{start = "forall ", finish = "", indent = 7, childsep = PP.NOSEP,
              children = [layout_effectvars epses,
                          layout_places rhos,
                          layout_mularefset Psi,
                          at(case p of NONE => PP.LEAF "none" | SOME p => layout_place p)]}

  fun layout_efenv mulenv =
      LvarMap.layoutMap{start="Efenv: [", finish = "]", eq = " -> ", sep = ", "}
                       (PP.LEAF o Lvar.pr_lvar)
                       (layout_qmularefset o !)
                       mulenv

  fun layout_mapping (eps, aref) =
      layout_pair(layout_effectvar eps, "->", layout_mularef aref)

  fun layout_subst subst =
      layout_set(map layout_mapping subst)

  fun layout_Phi Phi = layout_set (map layout_aref Phi)

  fun layout_shared (MULEFF(ref muleff)) = layout_mulef muleff
    | layout_shared (MULAREFF(ref mularef)) = layout_mularef mularef
    | layout_shared (MULSCHEME(ref qmularefset)) = layout_qmularefset qmularefset

  fun layout_dependency_map dep =
      DepEnv.layoutMap{start="{", finish = "}", eq = "=", sep = ","}
                      (fn eps => Effect.layout_effect eps)
                      (fn shared_list => layout_list(map layout_shared shared_list))
                      dep

  (* operations on multiplicity effects *)

  fun intof_mul (NUM i) = i
    | intof_mul INF = 10000

  fun inteffect psi = map (fn(x, mul)=>(x, intof_mul mul)) psi

  fun maxmul (NUM n1, NUM n2) = if n1 > n2 then NUM n1 else NUM n2
    | maxmul _ = INF

  fun timesmul (NUM n1, NUM n2) = if n1 * n2 > K then INF else NUM (n1 * n2)
    | timesmul _ = INF

  fun diffmul (NUM n1, NUM n2) = if n1 - n2 < 0 then NUM 0 else NUM (n1 - n2)
    | diffmul (INF, _) = INF    (* even when second arg i INF; see use of diffef in instantiate*)
    | diffmul (NUM n, INF) = NUM 0

  (* sumef(psi1,psi2): computes the sum of psi1 and psi2.
     One does not necessarily have  Dom(psi1) = Dom(psi2).
     Done by merging psi1 and psi2
  *)

  fun sumef ([]:mulef,psi2:mulef) = psi2
    | sumef (psi1,[]) = psi1
    | sumef (psi1 as ((ae1, mul1)::psi1'),psi2 as ((ae2, mul2)::psi2'))=
      if Eff.eq_effect(ae1,ae2)
         then (ae1, summul(mul1,mul2))::sumef(psi1', psi2')
      else if  Eff.ae_lt(ae1, ae2)
         then (ae1, mul1)::sumef (psi1',psi2)
      else (ae2, mul2)::sumef (psi1,psi2')

  structure MultiMerge =
    struct
      (* A multi-way merge can be implemented by keeping a heep
         of list of elements to be sorted. The lists in the heap
         are non-empty. The key value of a list is the key value
         of the first element of the list.*)

      fun leq_key (i, j) = Eff.ae_lt(i,j) orelse Eff.eq_effect(i,j)

      structure HI = struct
        type elem = (Eff.effect * mul)list
        fun leq ((x,_)::_, (y,_)::_) = leq_key(x,y)
          | leq _ = die "leq"
        fun layout _ =  die "layout"
      end

      structure Heap = Heap(structure HeapInfo = HI)

      fun merge ((ae1,m1), (ae2,m2)) = (ae1, summul(m1,m2))
      fun eq ((ae1, m1), (ae2,m2)) = Eff.eq_effect(ae1, ae2)

      fun makeHeap ll =
        let fun mkHeap([], h) = h
              | mkHeap([]::rest, h) = mkHeap(rest,h)
              | mkHeap( l::rest, h) = mkHeap(rest, Heap.insert l h)
        in
            mkHeap(ll, Heap.empty)
        end

      fun insert ([], h) = h
        | insert (l, h) = Heap.insert l h

      fun merge_against (min, h) =
          if Heap.is_empty h then [min]
          else let (*val (l1 as (x1::xs1), h1) = Heap.delete_min h*)
                   val (l1,x1,xs1,h1) =
                     case Heap.delete_min h
                       of (l1 as (x1::xs1), h1) => (l1,x1,xs1,h1)
                        | _ => die "merge_against"
               in  if eq(min,x1) then
                      if Heap.is_empty h1 then merge(min,x1)::xs1
                         else merge_against(merge(min,x1), insert(xs1, h1))
                   else
                      if Heap.is_empty h1 then min :: l1
                      else min :: merge_against(x1, insert(xs1, h1))
               end

       fun merge_all h =
          if Heap.is_empty h
             then []
          else let (*val (l1 as (x1::xs1), h1) = Heap.delete_min h*)
                   val (l1,x1,xs1,h1) =
                     case Heap.delete_min h
                       of (l1 as (x1::xs1), h1) => (l1,x1,xs1,h1)
                        | _ => die "merge_against"
               in  merge_against(x1, insert(xs1,h1))
               end

      fun multimerge (ll: HI.elem list) =
          merge_all(makeHeap ll)
    end

  fun sum_psis [psi] = psi
    | sum_psis [psi1,psi2] = sumef(psi1,psi2)
    | sum_psis [] = die "sum_psis: expects at least one argument"
    | sum_psis (at_least_three) = MultiMerge.multimerge(at_least_three) (*sumef(psi, sum_psis psis)*)

  (* maxef(psi1,psi2): computes the max of psi1 and psi2.
     One does not necessarily have  Dom(psi1) = Dom(psi2).
     Done by merging psi1 and psi2
  *)

  fun maxef ([]:mulef,psi2:mulef) = psi2
    | maxef (psi1,[]) = psi1
    | maxef (psi1 as ((ae1, mul1)::psi1'),psi2 as ((ae2, mul2)::psi2'))=
      if Eff.eq_effect(ae1,ae2)
         then (ae1, maxmul(mul1,mul2))::maxef(psi1', psi2')
      else if  Eff.ae_lt(ae1, ae2)
         then (ae1, mul1)::maxef (psi1',psi2)
      else (ae2, mul2)::maxef (psi1,psi2')

  fun max_psis [psi] = psi
    | max_psis (psi::psis) = maxef(psi, max_psis psis)
    | max_psis [] = die "max_psis: expects at least one argument"

  (* diffef(psi1,psi2) computes the difference between psi1 and psi2.
     Precondition: Dom(psi1) = Dom(psi2)
  *)

  exception DiffEf

  fun diffef_aux ([]:mulef,[]:mulef) = []
    | diffef_aux ((ae1, mul1)::psi1', (ae2, mul2)::psi2')=
       (case diffmul (mul1, mul2) of
                NUM 0 => diffef_aux(psi1', psi2')
              | mul => (ae1, mul)::diffef_aux(psi1', psi2'))
    | diffef_aux _ = raise DiffEf

(* does not work:
  fun diffef_aux (psi1:mulef,[]:mulef) = psi1
    | diffef_aux ((ae1, mul1)::psi1', psi2 as (ae2, mul2)::psi2')=
      if Ed.eq_effect(ae1,ae2) then
       (case diffmul (mul1, mul2) of
                NUM 0 => diffef_aux(psi1', psi2')
              | mul => (ae1, mul)::diffef_aux(psi1', psi2'))
      else (ae1, mul1)::diffef_aux(psi1', psi2)
    | diffef_aux _ = raise DiffEf
*)

  local
      fun check ((e1,_)::(e2,_)::x) =
          let fun toStr e = PP.flatten1 (Effect.layout_effect e)
              val r1 = Effect.rho_of e1
              val r2 = Effect.rho_of e2
          in
              say ("e1 = e2: " ^ Bool.toString(Eff.eq_effect(e1,e2)));
              say ("rhoOf(e1) = rhoOf(e2): " ^ Bool.toString(Eff.eq_effect(r1,r2)));
              say ("e1 = " ^ toStr e1);
              say ("e2 = " ^ toStr e2);
              say ("r1 = " ^ toStr r1);
              say ("r2 = " ^ toStr r2)
          end
        | check _ = die "check bad"
  in
  fun diffef (mulef1,mulef2) =
      diffef_aux(mulef1,mulef2)
      handle DiffEf =>
             (say "oh-oh ... cannot subtract effects";
              say "mulef1: ";
              outtree(layout_mulef mulef1);
              say "\nmulef2: ";
              outtree(layout_mulef mulef2);
              (*      check mulef1; *)
              die "diffef failed")
  end

  fun timesef (mul,[]) = []
    | timesef (mul,(ae1, mul1)::psi)=
                 (ae1, timesmul(mul, mul1))::timesef(mul, psi)

(* (*removeatomiceffects(psi,phi): computes psi\\phi
     assumes phi \subseteq Dom(psi) *)
  fun removeatomiceffects(psi, [])= psi
    | removeatomiceffects((ae1, mul1)::psi, ae2::phi) =
      if Eff.eq_effect(ae1,ae2)
         then removeatomiceffects(psi, phi)
      else (ae1, mul1)::removeatomiceffects(psi, ae2::phi)
    | removeatomiceffects([], ae2::phi) = die "removeatomiceffects"
*)

(*
  fun removeatomiceffects(psi, []: Effect.effect list) = psi
    | removeatomiceffects(psi, discharged_basis: Effect.effect list) =
      (* each member of discharged_basis is either a region variable or an arrow effect;
         now remove from psi all ae:m for which ae takes the form eps in discharged_basis
         or PUT rho or GET rho for rho in discharged_basis:
      *)
      let val _ = List.app (fn eps_or_rho => Eff.get_visited eps_or_rho := true) discharged_basis
          fun keep (ae,mul): bool =
            let val ae = Eff.find ae
            in if Eff.is_arrow_effect ae then not(!(Eff.get_visited ae))
               else if Eff.is_put ae then not(!(Eff.get_visited(rho_of ae)))
               else (*if Eff.is_get (Eff.find ae)
                     then not(!(Eff.get_visited(Eff.rho_of ae)))
                else*) die "removeatomiceffects.keep"
            end
      in
         List.filter keep psi footnote
            List.app (fn eps_or_rho => Eff.get_visited eps_or_rho := false) discharged_basis
      end
*)
  fun removeatomiceffects (psi, discharged_basis: Effect.effect list) =
      Eff.removeatomiceffects(psi, discharged_basis)

  (* getmultiplicities(psi, [rho_1,...,rho_k]) returns [mul_1,..., mul_k], where mul_i
     is the multiplicity of PUT(rho_i) in psi; mul_i is 0 if PUT(rho_i)\notin\Dom(\psi);
     getmultiplicities assumes that psi is sorted on Eff.ae_lt and that [rho_1,...,rho_k]
     are sorted according to their "key" fields. *)

  fun getmultiplicities_loop (psi, [])= []
    | getmultiplicities_loop ((ae1, mul1)::psi, rho::rhos) =
        let
            val ae2 = Eff.mkPut rho (* important: mkPut returns existing Put rho, if one exists*)
        in
          if Eff.eq_effect(ae1,ae2)
            then mul1:: getmultiplicities_loop(psi, rhos)
          else if Eff.ae_lt(ae2, ae1) handle Eff.AE_LT => die "getmultiplicities_loop: ae_lt"
               then NUM 0 :: getmultiplicities_loop((ae1, mul1)::psi, rhos)
          else getmultiplicities_loop(psi, rho::rhos)
        end

    | getmultiplicities_loop ([], ae2::phi) = map (fn _ => NUM 0) (ae2::phi)

  fun getmultiplicities (psi,rhos) = getmultiplicities_loop(psi,rhos)

  fun getmultiplicities_unsorted (psi,rhos) =
      map (fn rho => let val ae_rho = Eff.mkPut rho
                     in case List.find (fn (ae,mul) => Eff.eq_effect(ae, ae_rho)) psi of
                            SOME (_,res) => res
                          | NONE => NUM 0
                     end)
          rhos

  val empty_psi = []
  val empty_efenv = LvarMap.empty

  fun put place = [(Eff.mkPut place, NUM 1)]
  fun putInf place = [(Eff.mkPut place, INF)]
  fun putzero place = [(Eff.mkPut place, NUM 0)]
  fun get place = [(Eff.mkGet place, NUM 1)]
  fun getInf place = [(Eff.mkGet place, INF)]

  fun efvar eps = [(eps, NUM 1)]
  fun makearef (eps,psi)= (eps,psi)
  fun makesubst (eps, mularef):efsubst = [(eps, mularef)]
  fun mk_infinite (eps, mulef) = (eps, map (fn (ae, _) => (ae, INF)) mulef)

  fun lookup_mularefset ((eps, psi)::Psi, eps') =
      if Eff.eq_effect(eps, eps') then psi else lookup_mularefset(Psi, eps')
    | lookup_mularefset ([], eps') =
      die ("lookup_mularefset: " ^ show_effectvar eps')

  fun lookup_mularefmap (Psi,eps) =
      case EffVarEnv.lookup Psi eps of
          SOME x => x
        | NONE => die ("lookup_mularefmap: "^show_effectvar eps)


  fun reify mularefs =
      List.foldl (fn ((r as ref(eps,mulef)),acc) =>
                     GlobalEffVarEnv.add((*Eff.key_of_eps_or_rho*) eps, r, acc))
                 GlobalEffVarEnv.empty
                 mularefs

  fun lookup_efenv (EE, lvar) =
      case LvarMap.lookup EE lvar of
          SOME x => x
        | NONE => die ("lookup_efenv: "^Lvar.pr_lvar lvar)

  fun declare (EE,x,Xi) = LvarMap.add(x,Xi,EE)
  fun plus (EE,EE') = LvarMap.plus(EE,EE')

  fun getimage ([], x)= NONE
    | getimage ((x, fx)::f, x')= if Eff.eq_effect(x,x') then SOME fx else getimage(f, x')

  fun apply_mulef (S, [])= []
    | apply_mulef (S, psi0 as (ae_m as (eps,mul))::psi) =
            if Eff.is_arrow_effect eps
              then case getimage(S, eps) of
                     SOME (eps', psi') =>
                       sumef([(eps', mul)],
                             sumef(timesef(mul, psi'), apply_mulef(S, psi)))
                    | NONE => sumef([ae_m], apply_mulef(S,psi))
            else psi0 (*sumef([ae_m], apply_mulef(S, psi))*)


  fun getarefs (Psi, epses)=
      ListPair.zip(epses, map (fn eps => lookup_mularefmap(Psi,eps)) epses)

  fun apply_mularef (S, (eps,psi)) =
      case getimage(S, eps) of
          SOME (eps', psi') => (eps', sumef( psi', apply_mulef(S, psi)))
        | NONE => (eps, apply_mulef(S, psi))

  fun pairmap f S [] = []
    | pairmap f S (x::xs) = f(S, x)::(pairmap f S xs)

  fun apply_mularefset (S, Psi) = pairmap apply_mularef S Psi


  fun sort_psi psi = ListSort.sort (fn (ae1,_) => fn (ae2,_) => Eff.ae_lt(ae1,ae2)) psi

  fun apply_regionsubst_mulef (S, psi) =
      let val unsorted: mulef ref = ref []
          val psi'_list =
              (List.foldr (fn ((ae,mul),l) =>
                              if Eff.is_put ae then
                                let val rho = rho_of ae
                                in if !(Eff.get_visited(rho)) then (* generic *)
                                     (case getimage(S, rho)
                                       of SOME place => (unsorted:= (Eff.mkPut place, mul):: !unsorted; l)
                                        | NONE => die "apply_regionsubst_mulef: non-generic node visited")
                                   else (* non-generic *)
                                     (ae,mul)::l
                                end
                              else (ae,mul)::l)
                          [] psi)
      in case !unsorted of
             [] => psi
           | l => ((sumef(compress(sort_psi l), psi'_list)) handle X => die "apply_regionsubst_mulef.sumef")
      end


  fun apply_regionsubst_mularef (S, (eps,psi)) =
      (eps, apply_regionsubst_mulef(S, psi))

  fun apply_regionsubst_mularefset (S, Psi)=
      let val visited_refs = map (fn (rho,_) => Eff.get_visited rho) S
      in List.app (fn r => r:=true) visited_refs;
         (* mark dom(Sr) as visited; for faster instantiation *)
         pairmap apply_regionsubst_mularef S Psi
                 footnote List.app (fn r => r:=false) visited_refs
      end

  fun apply_qmularefset (S, ((epses,rhos, Psi), p)) =
      ((epses, rhos, apply_mularefset(S, Psi)), p)

  fun instantiateRegions ([], qmularefset as((epses, [], Psi), place))= qmularefset
    | instantiateRegions (places, ((epses, rhos, Psi), place))=
      let val Sr = BasisCompat.ListPair.zipEq(rhos, places)
      in ((epses, [], apply_regionsubst_mularefset(Sr, Psi)), place)
      end
      handle BasisCompat.ListPair.UnequalLengths =>
             die ("instantiateRegions: " ^ Int.toString(List.length rhos) ^ " formals, " ^
                  Int.toString (List.length places) ^ "actuals")

  fun cyclic (eps, []) = false
    | cyclic (eps, (eps',_):: rest) =
      Eff.eq_effect(eps,eps') orelse
      Eff.ae_lt(eps', eps) andalso cyclic(eps,rest)


  (* find_cyclic mularefs returns (SOME aref, mularefs') if
     there exists an aref in mularefs which is cyclic (in which case
     mularefs' is mularefs \ aref) or it returns (NONE, mularefs)
  *)

  fun find_cyclic [] = (NONE, [])
    | find_cyclic ((eps,psi)::rest) =
       if cyclic (eps,psi) then (SOME(eps,psi), rest)
       else let val (c_opt, rest') = find_cyclic rest
            in (c_opt, (eps,psi)::rest')
            end

  fun remove_cycles (l : mularef list): mularef list =
      case find_cyclic l of
          (NONE, _) => l
        | (SOME(eps,psi), rest) =>
          let val new_psi =  map (fn (eps, _) => (eps, INF))
                                 (List.filter (fn (eps',_) => not(Eff.eq_effect(eps,eps'))) psi)
              val Se = makesubst(eps,(eps,new_psi))
          in remove_cycles(apply_mularefset(Se,rest)@[(eps,new_psi)])
          end

(*
  fun checkPsi [] = true
    | checkPsi [(ae,m)] = true
    | checkPsi ((ae1,m1)::(ae2,m2)::rest) =
         not(Eff.eq_effect(ae1,ae2)) andalso checkPsi((ae2,m2)::rest)

  fun checkPsi Psi = List.al (not o cyclic) Psi
*)

  fun makeqmularefset (rhos,epses, Psi:imp_mularefmap, place, cone) : qmularefset =
      let val c = Eff.push cone
          val Psi':mularefset=
              remove_cycles(map (fn (eps, r as ref mularef) => mularef)
                                (getarefs(Psi, epses)))
          val (freshrhos,c) = Eff.freshRhos(rhos,c)
          val (freshepses,c) = Eff.freshEpss(epses, c)
          val S = BasisCompat.ListPair.zipEq(epses, BasisCompat.ListPair.zipEq(freshepses,
                                                                               map (fn x => []) freshepses))
          val Sr= BasisCompat.ListPair.zipEq(rhos, freshrhos)
          val Psi'' : mularefset = apply_mularefset(S, Psi')
          val Psi'' : mularefset = apply_regionsubst_mularefset(Sr, Psi'')
      in
        (*if checkPsi Psi'' then () else die "makeqmularefset creates bad scheme"*)
        Eff.pop c;
        ((freshepses, freshrhos, Psi''), place)
      end
      handle BasisCompat.ListPair.UnequalLengths => die "makeqmularefset.Zip"
           | _ => die "makeqmularefset"

  val empty_dep = DepEnv.empty

  fun lookup_dep (dep, eps) =
      case DepEnv.lookup dep eps of
          SOME x => x
        | NONE => die ("lookup_dep: "^ show_effectvar eps ^ "\n" ^
                       PP.flatten1(layout_dependency_map dep))

  fun add_dependency (dep,eps,shared) =
      let val shared_list =  case DepEnv.lookup dep eps of
                                 SOME x => x
                               | NONE => []
      in
        DepEnv.add(eps, shared :: shared_list, dep)
      end handle _ => die "add_dependency"

  val count_increment = ref 0
  val no_increase_sofar = ref true
  fun last_increment () = !count_increment

  (* normal form of arrow effects:
     an arrow effect eps.psi is said to be in normal form if,

      if eps in Dom(psi) then for all ae:mul in psi, mul = infinity

     Normal form is achieved by calling function nf, when normal form is important.
  *)

  fun nf (eps,psi) =
      if cyclic(eps, psi)
      then (* cyclic effect: make all multiplicities infinite *)
        (eps, map(fn (eps,_) => (eps, INF))
                 (List.filter (fn (eps',_) => not(Eff.eq_effect(eps,eps'))) psi))
      else (eps, psi)

  (* mulef_has_grown(old_mulef, new_mulef) determines whether new_mulef>old_mulef
     Precondition: Dom(old_mulef) = Dom(new_mulef) and new_mulef>= old_mulef*)

  fun mulef_has_grown_loop (mulef_old:mulef as [], muleff_new:mulef as []) = false
    | mulef_has_grown_loop ((ae1,m1)::mulef_old, (ae2,m2)::muleff_new) =
      if not(Eff.eq_effect(ae1,ae2))
      then die "mulef_has_grown_loop: mismatching atomic effects"
      else intof_mul m2 > intof_mul m1
           orelse mulef_has_grown_loop(mulef_old, muleff_new)
    | mulef_has_grown_loop _ =
      die "mulef_has_grown: mismatching multiplicity effects"

  fun mulef_has_grown (mulef_old, mulef_new) =
      mulef_has_grown_loop (mulef_old, mulef_new)
      handle x =>
             (mes("\nold: " ^ PP.flatten1(layout_mulef mulef_old)
                  ^ "\nnew: " ^ PP.flatten1(layout_mulef mulef_new) ^ "\n");
              raise x)

  fun mularef_has_grown (mularef_old:mularef, mularef_new: mularef) =
      mulef_has_grown(#2(nf mularef_old), #2(nf mularef_new))

  fun nf_mularef_has_grown (mularef_old:mularef as (_,psi_old),
                            mularef_new:mularef as (_,psi_new)) =
      mulef_has_grown (psi_old, psi_new)

  fun qmularefset_has_grown (old as ((_,_,Psi_old),_): qmularefset,
                             new as ((_,_,Psi_new),_): qmularefset) =
      List.exists nf_mularef_has_grown (BasisCompat.ListPair.zipEq(Psi_old,Psi_new))
      handle _ => (say ("qmularefset_has_grown, old scheme:");
                   outtree(layout_qmularefset old);
                   say ("qmularefset_has_grown, new scheme:");
                   outtree(layout_qmularefset old);
                   die "qmularefset_has_grown: mismatching type schemes")

  (* doSubst(eps,mulef,dep)  performs the substitution eps |-> eps.mulef  on
     every element of the list  dep(eps), i.e., on every semantic object in
     which eps occurs free. *)

  fun saw_progress () =
      (no_increase_sofar:= false;
       count_increment:= !count_increment+1)

  fun doSubst (eps,[],dep): unit = ()
    | doSubst (eps,mulef,dep): unit =
      let val dependants = lookup_dep(dep,eps)
          val Se = makesubst(eps,(eps,mulef))
          fun update_shared (MULEFF(r as ref mulef)) =
              let val new = apply_mulef(Se, mulef)
              in
                if !no_increase_sofar andalso mulef_has_grown(mulef, new)
                then saw_progress()
                else ();
                r:= new
              end
            | update_shared (MULSCHEME(r as ref qmularefset)) =
              let val new = apply_qmularefset(Se,qmularefset)
              in
                if !no_increase_sofar andalso
                   qmularefset_has_grown(qmularefset, new)
                then saw_progress()
                else ();
                r:= new
              end
            | update_shared (MULAREFF(r as ref mularef)) =
              let val new = apply_mularef(Se, mularef)
              in
                if !no_increase_sofar andalso mularef_has_grown(mularef, new)
                   handle x =>
                          (say ("mularef_has_grown: old: ");
                           outtree(layout_mularef mularef);
                           say ("mularef_has_grown: new: ");
                           outtree(layout_mularef new);
                           raise x)
                then saw_progress()
                else ();
                r:= new
              end
      in
        no_increase_sofar:= true;
        List.app update_shared dependants
        handle x => (mes("substitution: " ^ PP.flatten1(layout_subst Se) ^ "\n");
                     raise x)
      end
(*
  fun eq_epss(epses1,epses2): bool =
      List.all Eff.eq_effect (ListPair.zip(epses1,epses2))

  fun makeinf_arroweffect(eps, phi) = (eps, map (fn x => (x, INF)) phi) (* phi must be sorted! *)
*)

  (* makezero_muleffect phi sets all multiplicites to 1 (!).
     In Magnus' dissertation, they are all set to 0, but it turns out
     that there is a marked improvement is compilation speed if one
     starts out by setting multiplicities to 1. This should not be
     a great loss of precision: the reason an atomic effect got into
     an effect in the first place is presumably most often that
     there was at least one occurrence of it.

     For assignments this solution may be too imprecise: assignment
     gives rise to multiplicities 0. This remains to be seen.
  *)

  fun makezero_muleffect phi = map (fn x => (x, NUM 1)) phi  (* phi must be sorted! *)

  fun makezero_arroweffect (eps, phi) = (eps, makezero_muleffect phi)

  fun makezero_Phi (Phi: arroweffect list) : imp_mularefmap =
    List.foldl (fn ((eps,phi), Psi) =>
               EffVarEnv.add(eps, ref(makezero_arroweffect(eps,phi)), Psi))
            (QM_EffVarEnv.mk 1000 GlobalEffVarEnv.empty)
            Phi

  (* instantiate(actuals: arefset, qmul, Psi, dep)
     instantiates one ground segment of actuals at a time *)

  fun check_eff s e =
      if Eff.is_put e orelse Eff.is_get e then
          let val r = Eff.rho_of e
              val k = Eff.key_of_eps_or_rho r
          in if k = 1 andalso not(Eff.eq_effect(Eff.toplevel_region_withtype_top,r)) then
              die ("check_eps: " ^ s)
             else ()
          end
      else ()

  fun check_psi s nil = ()
    | check_psi s ((e,_)::xs) =
      (check_eff s e; check_psi s xs)

  fun instantiate ([], Xi, Psi, dep)= ()
    | instantiate ((aref0 as (eps0,phi0))::plainarroweffects,
                  qmul as (eps0' :: epses', [], Psi'), Psi, dep)=
         let
(*             val _ = if checkPsi Psi' then () else
                        (say "instantiate applied to ill-formed Xi = ";
                         outtree(layout_qmul qmul))
*)
(*
             val t0 = layout_qmul qmul
             val t1 = layout_mularef (!(lookup_mularefmap(Psi, eps0)))
             val t2 = layout_mularef (nf(!(lookup_mularefmap(Psi, eps0))))
             val t3 = layout_mulef   (lookup_mularefset(Psi', eps0'))
*)
             val actual_psi = #2(nf(!(lookup_mularefmap(Psi, eps0))))
             val _ = check_psi "actual_psi" actual_psi (* mael 2004-10-19 *)
             val formal_psi = lookup_mularefset(Psi', eps0')
             val _ = check_psi "formal_psi" formal_psi (* mael 2004-10-19 *)
             val new_actual_psi:mulef =
                       maxef(actual_psi,
                             formal_psi) (* formal, acyclic *)
             val _ = check_psi "before nf" new_actual_psi (* mael 2004-10-19 *)
             (* (eps0,new_actual_psi) is not necessarily acyclic, so normalise it: *)
             val (eps0,new_actual_psi) = nf (eps0,new_actual_psi)
             val _ = check_psi "after nf" new_actual_psi (* mael 2004-10-19 *)
(*
             val t4 = layout_mulef new_actual_psi
*)
             val Se:efsubst = [(eps0', (eps0,new_actual_psi))]
             val _ = doSubst(eps0, diffef(new_actual_psi,actual_psi), dep)
               handle X => (say "\ninstantiate.doSubst or diffef failed\n"; raise X)
                                handle x =>
                                  (say "eps0 is "; outtree (Eff.layout_effect eps0);
                                   say "eps0' is "; outtree (Eff.layout_effect eps0');
(*
                                   say "\nqmul="; outtree t0;
                                   say "\nlookup Psi gave"; outtree t1;
                                   say "\nnf gave"; outtree t2;
                                   say "\nlookup Psi' gave"; outtree t3;
                                   say "\nnew_actual_psi was:"; outtree t4;
*)
                                   raise x)
         in
              instantiate(plainarroweffects,
                          (epses', [],
                           apply_mularefset(Se,
                                            List.filter (fn (eps, psi) =>
                                                      not(Eff.eq_effect(eps,eps0')))
                                            Psi')),
                          Psi,
                          dep)

         end
       | instantiate _ = die "instatiate: wrong arguments"
     handle Zip => die "instantiate"

  fun instantiateeffects (arroweffects, qmul as ((epses, [], Psi'), _), Psi, dep) =
             instantiate(arroweffects, (epses, [], Psi'), Psi, dep)
    | instantiateeffects _ = die "instantiateeffects: non-empty list of formal regions"

  (* add_dependencies(dep, shared, phi):
     for each eps free in phi, add shared to dep(eps) *)

  fun is_arrow_effect eps = Eff.is_arrow_effect (eps)

  fun add_dependencies (dep:dependency_map,shared,phi): dependency_map =
      let fun loop ([], dep) = dep
            | loop (eps::rest, dep) =
               if is_arrow_effect (eps)
               then loop(rest,add_dependency(dep, eps, shared))
               else dep

      in loop(phi,dep)
      end
(*
       List.foldl (fn (eps,dep) =>
                    if Eff.is_arrow_effect (Eff.find eps)
                       then add_dependency(dep, eps, shared)
                    else dep
                  ) dep
                  phi
*)
  (* cons_if_not_there(eps', phi): insert eps' into phi, which is sorted,
     at the appropriate place which makes the result sorted. If eps' is
     already in phi, it is not inserted again. *)

  fun cons_if_not_there (eps',[]) = [eps']
    | cons_if_not_there (eps',eps::rest) =
           if Eff.eq_effect(eps,eps') then eps::rest
           else if Eff.ae_lt(eps', eps) then eps' :: eps :: rest
           else eps::cons_if_not_there(eps',rest)

  fun mk_init_dep ((eps, r as ref (mularef as (eps',psi))), dep) : dependency_map=
       (* add r as a shared semantic object of off effectvariables in eps'.psi  *)
       add_dependencies(dep, MULAREFF r, cons_if_not_there(eps',map #1 psi))

  fun mk_init_dependency_map (Psi:imp_mularefmap) =
        let val _ = reset_dep()
            val result = EffVarEnv.Fold mk_init_dep empty_dep Psi
        in
            (*say "initial dependency map";*)
            (*say "omitted"*)
            (*outtree(layout_dependency_map result);*)
            result
        end

  fun make_arroweffects x = x
  fun un_mularef x = x

    (*************************************)
    (*                                   *)
    (* The initial multiplicity          *)
    (* environment (Mul.initial: efenv)  *)
    (*                                   *)
    (*************************************)

  val initial: efenv = empty_efenv

  (* Picklers *)
  val pu_mul =
      let fun toInt INF = 0
            | toInt (NUM _) = 1
          val fun_INF = Pickle.con0 INF
          fun fun_NUM _ = Pickle.con1 NUM (fn NUM a => a | _ => die "pu_mul.NUM") Pickle.int
      in Pickle.dataGen("Mul.mul",toInt,[fun_INF,fun_NUM])
      end
  val pu_mulef = Pickle.nameGen "mulef" (Pickle.listGen(Pickle.pairGen0(Eff.pu_effect,pu_mul)))
  val pu_mularef = Pickle.nameGen "mularef" (Pickle.pairGen0(Eff.pu_effect,pu_mulef))
  val pu_mularefset = Pickle.listGen pu_mularef
  val pu_qmularefset = Pickle.pairGen0(Pickle.tup3Gen0(Eff.pu_effects,Eff.pu_effects,pu_mularefset),
                                       Pickle.optionGen Eff.pu_effect)
(*  val pu_efenv = LvarMap.pu Lvar.pu (Pickle.ref0Gen pu_qmularefset) *)
  val pu_efenv = LvarMap.pu Lvar.pu (Pickle.convert (ref,!) pu_qmularefset)

  val pu_mularefmap = GlobalEffVarEnv.pu Eff.pu_effect (Pickle.ref0Gen pu_mularef)
end
