
(*
*
*   The purpose of SpreadDatatype is to analyse datatype declarations
*   and find out for each type name what its arity is (not just the
*   type arity, which is given in the input program) but also the
*   region and effect arity). Moreover, the module infers a region-polymorphic
*   type scheme for each value constructor declared in the source program.
*   
*)

functor SpreadDatatype(
  structure Con: CON
  structure ExCon: EXCON
  structure FinMap: FINMAP
  structure Effect: EFFECT
  structure E: LAMBDA_EXP
   sharing type E.con = Con.con
  structure R: RTYPE
   sharing type R.tyvar = E.tyvar
       and type R.cone = Effect.cone
  structure TyName: TYNAME
  structure E': REGION_EXP
    sharing type E'.sigma = R.sigma and type E'.con = Con.con
     and type E'.TyName = TyName.TyName
  structure RSE: REGION_STAT_ENV
    sharing type RSE.TypeAndPlaceScheme = R.sigma
        and type RSE.place = R.effect = Effect.effect
        and type RSE.Type = R.Type
        and type RSE.place = R.place
        and type RSE.runType = Effect.runType = R.runType
        and type RSE.con = E.con = Con.con 
        and type RSE.excon = E.excon 
  sharing type TyName.TyName = E.TyName = E'.TyName = 
           RSE.TyName = R.tyname
  structure Flags : FLAGS
  structure Crash: CRASH
  structure PP: PRETTYPRINT
    sharing type PP.StringTree = FinMap.StringTree = E.StringTree = RSE.StringTree = R.StringTree
): SPREAD_DATATYPE =
struct

  open Edlib

  structure E = E
  structure E' = E'
  structure LambdaExp = E
  structure RegionExp = E'
  structure RegionStatEnv = RSE 
(*  structure Info = Info*)

  type rse = RSE.regionStatEnv
  
  type TyName = TyName.TyName
  type tyname = TyName.TyName
  type tyvar = E.tyvar
   and con = E.con
   and Type = E.Type
  type cone = Effect.cone
  type place = Effect.place
  type effect = Effect.effect
  type sigma = R.sigma

         
  fun die s = Crash.impossible ("SpreadDatatype." ^ s)

  fun noSome x msg = 
    case x of 
      SOME it => it
    | NONE => die msg

  fun apply_n f 0 = []
    | apply_n f n = f() :: apply_n f (n-1)

  (* one target_db for each tyname *)
  type target_db = tyname *  (Con.con * E'.constructorKind * R.sigma) list  

  (* one target_datbind for each set of mutually recursive datatype bindings *)
  type target_datbind = target_db list   


   (**************************)
   (*                        *)
   (*  arities               *)
   (*                        *)
   (**************************)



  fun merge_runtypes([], l2) = l2
    | merge_runtypes(l1, []) = l1
    | merge_runtypes(l1 as (x::xs), l2 as (y::ys)) = 
        let val i1 = Effect.ord_runType x
            val i2 = Effect.ord_runType y
        in
            if i1 < i2 then x :: merge_runtypes(xs, l2)
            else if i1 = i2 then x :: merge_runtypes(xs, ys)
            else (* i2 < i1 *) y :: merge_runtypes(l1, ys)
        end

  abstype eff_arity = EA of int
  with
    val zero = EA 0
    val one  = EA 1
    fun eplus(EA 0, EA 0) = EA 0
      | eplus _ = EA 1
    fun eff_arity_int(EA i) = i
  end

  type arity = int * Effect.runType list * eff_arity
  val arity0 = (0, [], zero)


  infix ++
  fun ((a,b,c) ++ (a',b',c')) = (a+a':int, merge_runtypes(b,b'), eplus(c,c'))
  fun plus (a,b,c)(a',b',c') = (a+a':int, merge_runtypes(b,b'), eplus(c,c'))

  (* An {\sl arity} is a triple

             (the number of tyvars, runtypes of  regvars, the number of effectvars).

     The first of these is apparent from the source expression. The two others
     have to be found by analysis of the type declarations. 
  *)


  fun layout_arity(a,b,c) = PP.LEAF ("(" ^ Int.string a ^ "," ^ Int.string(length(b)) ^ "," 
                                     ^ Int.string c ^ ")")


  (* All type names in a mutually recursive datbind have the same
    region and effect arity; these are computed by summing up
    the arities found for individual type names: *)
       
  fun sum_arities new_tyenv_association_list = 
    List.foldR(fn (tyname,arity:arity) => fn acc: arity => 
       (* add region and effect arities (type arities are already the same in all
          the mutually recursive type names *)
      (#1 arity, merge_runtypes(#2 acc , #2 arity), eplus(#3 acc, #3 arity))) 
    arity0  new_tyenv_association_list

  fun zap_ty_arity(_,l,c) = (0, l, c)
  fun mk_abstract(a,b,0) = (a,b,zero)
    | mk_abstract(a,b,1) = (a,b,one)
    | mk_abstract _ = Crash.impossible "SpreadDataType.mk_abstract"

  fun mk_concrete(a,b,c) = (a,b,eff_arity_int c)

  fun infer_arity_ty rse (current_tynames: tyname list) (tau: E.Type): arity =
    (case tau of
     E.TYVARtype _ =>  arity0
       (* this does not give a contribution to the arity: all occurrences of
        the same type variable in the source expression are translated into
        the same pair of a type variable and a region variable in the target.
        All the other forms of types contribute at least with one region to
        the arity.                       *) 

  |  E.ARROWtype(taus1,taus2) => 
        List.foldL plus 
         (List.foldL plus (0,[Effect.TOP_RT],one)    (* closures have runtype TOP_RT *)
          (map (infer_arity_ty rse current_tynames) taus1)
         )
        (map (infer_arity_ty rse current_tynames) taus2)

  |  E.CONStype(types,tyname) => 
         List.foldR plus arity0 (map (infer_arity_ty rse current_tynames) types)
         ++ (if List.exists (fn tn => TyName.eq(tn,tyname)) current_tynames 
               then arity0
               else (0,[R.runtype(R.CONSTYPE(tyname,[],[],[]))],zero) ++
                     let val (global, local_rse) = rse 
                     in
                      (case RSE.lookupTyName local_rse tyname of
                         SOME arity => mk_abstract(zap_ty_arity(RSE.un_arity arity))
                       | NONE => (case RSE.lookupTyName global tyname of
                           SOME arity => mk_abstract(zap_ty_arity(RSE.un_arity arity))
                         | NONE => die ("infer_arity_ty. Type name: " 
                                  ^ TyName.pr_TyName tyname)))
                     end)
  |  E.RECORDtype(types) => 
         List.foldR plus (0,[case types of [] => Effect.WORD_RT
                               | _ => Effect.TOP_RT],zero) 
          (map (infer_arity_ty rse current_tynames) types)
  )

  fun infer_arity_conbind_list rse current_tynames conbind_list =
    List.foldR 
     (fn (con, SOME tau) => (fn  acc => infer_arity_ty rse  current_tynames tau   ++ acc)
       | _  => fn acc => acc
     ) arity0 conbind_list

  type single_datbind = tyvar list * tyname * (con * Type option) list
  type datbind = single_datbind list

  fun infer_arity_single_datbind rse (current_tynames : tyname list)
                                     (single_datbind:single_datbind as  
                                      (tyvar_list, tyname, conbind_list)): (tyname * arity)=
    (tyname, (length (tyvar_list), [], zero)
             ++ (infer_arity_conbind_list rse current_tynames conbind_list))

  (******************************)
  (* spreading a single type    *)
  (* from within a conbind      *)
  (******************************)

  fun fresh_list(f,0,cone) = ([],cone)
    | fresh_list(f,n, cone) =
       let val (new, cone) = f cone
           val (rest, cone) = fresh_list(f,n-1,cone)
       in  (new::rest, cone)
       end

  fun get_place (rho_resource: (Effect.runType * place)list ref) rt = 
      #2(List.first (fn (rt',rho) => rt = rt') (! rho_resource)) 
      handle List.First _ => die "get_place: no more places"

  fun get_eps arreff_resource () = 
    case !arreff_resource of [] => die "get_eps: no more epsilons"
  | arreff::rest => ((*arreff_resource:= rest; always choose the same "fresh" variable! *)
                      arreff)


  fun spread_ty_to_mu(tyvar_to_place: tyvar -> place option,
                      get_with_rt: Effect.runType -> place,
                      get_eps: unit -> effect,
                      being_defined: tyname -> bool,
                      fresh_rhos: place list,
                      fresh_epss: effect list,
                      common_place: place,
                      rse,
                      global_rse,
                      ty: E.Type): R.Type * R.effect  = 
    let

      fun extend(tau': R.Type): R.Type*R.effect = 
        (tau', get_with_rt(R.runtype tau'))

      fun get_list_with_runtypes(runtypes: R.runType list): R.effect list =
        map get_with_rt runtypes

      fun ty_to_mu(tau: E.Type): R.Type*R.effect=
        case tau of 
          E.TYVARtype alpha => 
             let val place = noSome (tyvar_to_place alpha)
                             "ty_to_mu: tyvar not in domain"
             in (R.TYVAR alpha, place)
             end
        | E.ARROWtype(taus1,taus2) => 
             extend(R.FUN(map ty_to_mu taus1, get_eps(), map ty_to_mu taus2))
        | E.CONStype(taus, tyname) => 
             if being_defined tyname
               then (R.CONSTYPE(tyname, map ty_to_mu taus, fresh_rhos,fresh_epss), 
                     common_place)
             else (* tyname not in the current datbind. 
                   Look for it amongst previously declared datbinds: *)
               (case spread_constructed_type(rse, tyname, taus) of
                  SOME mu => mu
                | NONE => (* look for it in the global rse *)
                    (case spread_constructed_type(global_rse, tyname, taus) of
                       SOME mu => mu
                     | NONE => die ("ty_to_mu: \
                      \ undeclared type name: " ^ TyName.pr_TyName tyname)
                    )
               )
        | E.RECORDtype(taus) => 
            extend(R.RECORD(map ty_to_mu taus)) 

      and spread_constructed_type(rse, tyname, taus): 
                                           (R.Type*R.effect) option = 
         case RSE.lookupTyName rse tyname of
           SOME arity => 
             let val (number_of_alphas, rho_runtypes, number_of_epsilons) = 
                  RSE.un_arity arity 
             in
               SOME(extend(R.CONSTYPE(tyname, map ty_to_mu taus,
                                             get_list_with_runtypes rho_runtypes,
                                             apply_n get_eps number_of_epsilons)))
             end
         | NONE => NONE

    in
       ty_to_mu ty
    end

  fun mk_rse_constructors (target_db: target_db) con_rse: rse=
          List.foldL (fn (con,_,sigma) => fn con_rse => 
                         RSE.declareCon(con, sigma, con_rse))
                     con_rse
                     (#2 target_db) (* the list of constructors for one type name *)

  fun mk_rse_one_mutual_recursion (target_dbs: target_datbind) con_rse =
           List.foldL mk_rse_constructors con_rse target_dbs


  (************************************************************************)
  (* a datbind  takes the form             tyvarseq1 tyname1 = conbinds_1  
                                       AND 
                                   ... AND tyvarseqn tynamen = conbinds_n*)
  (************************************************************************)


  (********************************************************************)
  (* spreadDatbind:                                                   *)
  (* In a call spreadDatbind level (global_rse)(datbind)(rse), global_rse
     stands for the rse defined prior to the current program, while
     rse is the rse from the beginning of the current program to
     datbind. *)
  (********************************************************************)
                                            

  fun spreadDatbind (level_of_TE:int)(global_rse: rse)(datbind: datbind) 
                    (rse, datbind'_acc: target_datbind list,cone) : 
                        (rse * target_datbind list * cone) =   

  let 
      val current_tynames: tyname list  = map (#2) datbind;

      (* find out the common arity of all the type names in 
         one mutually recursive datbind: *)

      val new_tyenv_association_list = 
            map (infer_arity_single_datbind (global_rse,rse) 
                 current_tynames
                ) datbind

      val common_arity = sum_arities new_tyenv_association_list

      (* all types declared in a mutally recursive datatype declaration 
         use the same region for their values: *)

      val (common_place,cone) = Effect.freshRhoWithTy(Effect.TOP_RT, cone)

   (*mads   val _ = TextIO.output(TextIO.stdOut,PP.flatten(PP.format(80,layout_arity(mk_concrete( common_arity)))))*)

      val (fresh_aux_rhos,cone) = fresh_list(Effect.freshRho,length(#2 common_arity),cone) 
      val l = map (fn (rt,rho) => (Effect.setRunType rho rt; (rt,rho)))
              (ListPair.zip(#2 common_arity,fresh_aux_rhos))
              handle _ => die "spreadDatbind: zip"

      val rho_resource = ref l

      val (fresh_aux_arreffs,cone) = fresh_list (Effect.freshEps,eff_arity_int(#3 common_arity), cone)
      val arreff_resource = ref fresh_aux_arreffs

      (* The datbind is run through and a datatype binding is
       produced.  (This has the desirable side-effect of setting the runtime
       types of the region variables in the region resource.)  *)

      fun spread_single_datbind (db: single_datbind) 
                                (acc as (tdb_list,cone): target_db list * cone): 
                                                             target_db list * cone = 
          let 
              val (tyvar_list, tyname, conbind_list) = db
              val (tyvar_conversion0,cone) = 
                  List.foldR 
                          (fn alpha => fn (rho_list, cone) => 
                           let val (rho, cone') = 
                                 Effect.freshRhoWithTy (Effect.BOT_RT, cone)
                           in 
                             ((alpha, rho):: rho_list, cone')
                           end) ([], cone) tyvar_list
              val new_mus0 = map (fn (alpha,rho) => (R.TYVAR alpha, rho))
                                 tyvar_conversion0
              val tyvarPairMap0 = 
                   List.foldR 
                    (fn (alpha,rho) => fn m => FinMap.add (alpha, rho, m))
                    FinMap.empty tyvar_conversion0

              val result_type = 
                    R.CONSTYPE(tyname,new_mus0, fresh_aux_rhos, fresh_aux_arreffs)

              fun spreadCon (con, tau_opt) (acc as (list,cone) : 
                        ((Con.con * E'.constructorKind * R.sigma)list * cone)) =
                   (case tau_opt of
                        SOME tau => 
                          let val mu1 = spread_ty_to_mu(
                                          FinMap.lookup tyvarPairMap0,
                                          (get_place rho_resource),
                                          (get_eps arreff_resource),
                                          (fn tyname => List.exists (fn tn => TyName.eq(tn, tyname)) 
                                                          current_tynames),
                                          fresh_aux_rhos,
                                          fresh_aux_arreffs,
                                          common_place,
                                          rse,
                                          global_rse,
                                          tau) 
                              val (eps, cone) = Effect.freshEps cone
                              val _ = Effect.edge(eps, Effect.mkPut common_place)  (* inserted 21/5/96 mads*)
                              val (cone,sigma,_) = 
                                    R.generalize_all(cone,level_of_TE,tyvar_list,
                                                 R.FUN([mu1],eps,[(result_type,
                                                                common_place)]))
                          in
                            ((con, E'.VALUE_CARRYING, sigma)::list,cone)
                          end
                      | NONE     => 
                          let val (cone,sigma,_) = 
                                  R.generalize_all(cone,level_of_TE,tyvar_list,result_type)
                          in ((con, E'.CONSTANT, sigma)::list, cone)
                          end
                  ) (*spreadCon*)

              val (db': (Con.con * E'.constructorKind * R.sigma) list, cone) = 
                    (List.foldR spreadCon ([],cone) conbind_list)
          in
              ((tyname,db') :: tdb_list, cone) 
          end (* spread_single_datbind *)

      fun msg(s) = if !Flags.chat then (*mads*) TextIO.output(TextIO.stdOut, s^"\n") else ()

      val _ = msg "Computing new datatype bindings..."
      val (target_datbind,cone) = List.foldL (spread_single_datbind) ([],cone) datbind
      val _ = msg "Computing new constructor env..."

      val rse = mk_rse_one_mutual_recursion target_datbind rse 

      (* make a region static environment mapping all type names of the
         mutually recursive datbind to the common arity: *)

      val rse  =  (* extend local rse with tyname bindings *)
          List.foldR (fn (tyname, (a,b,c) ) => fn  new_rse => 
                      let 
                        val export_arity = 
                          RSE.mk_arity(a, #2 common_arity, eff_arity_int(#3 common_arity))
                      in
                        RSE.declareTyName(tyname,export_arity,new_rse)
                      end
                      ) rse new_tyenv_association_list
  in
    (rse, target_datbind:: datbind'_acc, cone)
  end 

(* spreadDatbind *)


  (* `spreadDatbinds rse datbinds' cone  produces the type name 
      arities and constructor types of datbind in a (residual) rse.
  *)

  fun spreadDatbinds (global_rse: rse) 
                     (E.DATBINDS(datbinds : datbind list)) cone
                        : rse * E'.datbinds=
     let val (rse1, reversed_target_datbind, cone') = 
            List.foldL (spreadDatbind (Effect.level cone) global_rse) 
              (RSE.empty, [], Effect.push cone) datbinds 
         (* no need to pop cone *)
     in
        (rse1, E'.DATBINDS(rev reversed_target_datbind))
     end;

end; (* SpreadDatatype *)

