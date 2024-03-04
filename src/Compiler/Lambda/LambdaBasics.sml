
structure LambdaBasics: LAMBDA_BASICS =
  struct
    structure PP = PrettyPrint
    structure TLE = LambdaExp
    open TLE

    fun die s = Crash.impossible ("LambdaBasics." ^ s)

    fun log x = TextIO.output(!Flags.log,x)

    fun foldl' f a []      = a
      | foldl' f a (x::xs) = foldl' f (f a x) xs

    fun member a [] = false
      | member a (x::xs) = a=x orelse member a xs

    (* passTD: (LambdaExp -> LambdaExp) -> LambdaExp ->
     * LambdaExp. Applies a transformation over an entire lambda
     * expression in a "top down" manner (i.e. visiting sub
     * expressions second).  *)

    fun passTD f lamb =
      let
        fun passSwitch f (SWITCH(arg, selections, wildcard)) =
          SWITCH(f arg,
                 map (fn (const,x) => (const, f x)) selections,
                 case wildcard
                       of SOME lamb => SOME(f lamb)
                        | NONE => NONE)
      in
        case f lamb
          of VAR _ => lamb
           | INTEGER _ => lamb
           | WORD _ => lamb
           | STRING _ => lamb
           | REAL _ => lamb
           | F64 _ => lamb
           | FN{pat,body} => FN{pat=pat,body=passTD f body}
           | LET{pat,bind,scope} => LET{pat=pat,bind=passTD f bind,scope = passTD f scope}
           | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=passTD f scope}
           | FIX{functions,scope} => FIX{functions=map (fn {lvar, regvars, tyvars, Type, constrs, bind} =>
                                                        {lvar=lvar,regvars=regvars,tyvars=tyvars,
                                                         Type=Type,constrs=constrs,bind=passTD f bind}) functions,
                                         scope=passTD f scope}
           | APP(lamb1, lamb2, tc) => APP(passTD f lamb1, passTD f lamb2, tc)
           | EXCEPTION(excon,tauOpt,lamb) => EXCEPTION(excon,tauOpt, passTD f lamb)
           | RAISE(lamb,tl) => RAISE(passTD f lamb,tl)
           | HANDLE(lamb1, lamb2) => HANDLE(passTD f lamb1, passTD f lamb2)
           | SWITCH_I {switch, precision} =>
            SWITCH_I {switch=passSwitch (passTD f) switch, precision=precision}
           | SWITCH_W {switch, precision} =>
            SWITCH_W {switch=passSwitch (passTD f) switch, precision=precision}
           | SWITCH_S switch => SWITCH_S(passSwitch (passTD f) switch)
           | SWITCH_C switch => SWITCH_C(passSwitch (passTD f) switch)
           | SWITCH_E switch => SWITCH_E(passSwitch (passTD f) switch)
           | TYPED(lamb,t,cs) => TYPED(passTD f lamb,t,cs)
           | PRIM(prim,lambs) => PRIM(prim,map (passTD f) lambs)
           | FRAME _ => lamb
      end


    (* passBU: (LambdaExp -> LambdaExp) -> LambdaExp ->
     * LambdaExp. Applies a transformation over an entire lambda
     * expression in a "bottom up" manner (i.e. visiting sub
     * expressions first).  *)

    fun passBU f lamb =
      let
        fun passSwitch f (SWITCH(arg, selections, wildcard)) =
          SWITCH(f arg,
                 map (fn (const,x) => (const, f x)) selections,
                 case wildcard
                       of SOME lamb => SOME(f lamb)
                        | NONE => NONE)
      in
        f (case lamb
             of VAR _ => lamb
              | INTEGER _ => lamb
              | WORD _ => lamb
              | STRING _ => lamb
              | REAL _ => lamb
              | F64 _ => lamb
              | FN{pat,body} => FN{pat=pat,body=passBU f body}
              | LET{pat,bind,scope} => LET{pat=pat,bind=passBU f bind,scope = passBU f scope}
              | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=passBU f scope}
              | FIX{functions,scope} => FIX{functions=map (fn {lvar, regvars, tyvars, Type, constrs, bind} =>
                                                              {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                                                               constrs=constrs,bind=passBU f bind})
                                                          functions,
                                            scope=passBU f scope}
              | APP(lamb1, lamb2, tc) => APP(passBU f lamb1, passBU f lamb2, tc)
              | EXCEPTION(excon,tauOpt,lamb) => EXCEPTION(excon,tauOpt, passBU f lamb)
              | RAISE(lamb,tl) => RAISE(passBU f lamb,tl)
              | HANDLE(lamb1, lamb2) => HANDLE(passBU f lamb1, passBU f lamb2)
              | SWITCH_I {switch, precision} =>
               SWITCH_I {switch=passSwitch (passBU f) switch, precision=precision}
              | SWITCH_W {switch, precision} =>
               SWITCH_W {switch=passSwitch (passBU f) switch, precision=precision}
              | SWITCH_S switch => SWITCH_S(passSwitch (passBU f) switch)
              | SWITCH_C switch => SWITCH_C(passSwitch (passBU f) switch)
              | SWITCH_E switch => SWITCH_E(passSwitch (passBU f) switch)
              | TYPED(lamb,t,cs) => TYPED(passBU f lamb,t,cs)
              | PRIM(prim,lambs) => PRIM(prim,map (passBU f) lambs)
              | FRAME _ => lamb)
      end


    (* foldTD : ('a -> LambdaExp -> 'a) -> 'a -> LambdaExp -> 'a. *)
    fun foldTD (f:'a->LambdaExp->'a) (acc:'a) (lamb:LambdaExp) =
      let
        val new_acc = f acc lamb

        fun foldSwitch (SWITCH(arg, selections, wildcard)) =
          let val acc' = foldl' (foldTD f) (foldTD f new_acc arg) (map #2 selections)
          in case wildcard
               of SOME lamb => foldTD f acc' lamb
                | NONE => acc'
          end
      in
        case lamb
          of VAR _ => new_acc
           | INTEGER _ => new_acc
           | WORD _ => new_acc
           | STRING _ => new_acc
           | REAL _ => new_acc
           | F64 _ => new_acc
           | FN{pat,body} => foldTD f new_acc body
           | LET{pat,bind,scope} => foldTD f (foldTD f new_acc bind) scope
           | LETREGION{regvars,scope} => foldTD f new_acc scope
           | FIX{functions,scope} => foldTD f (foldl' (foldTD f) new_acc  (map #bind functions)) scope
           | APP(lamb1, lamb2, _) => foldTD f (foldTD f new_acc lamb1) lamb2
           | EXCEPTION(excon,tauOpt,lamb) => foldTD f new_acc lamb
           | RAISE(lamb,tl) => foldTD f new_acc lamb
           | HANDLE(lamb1, lamb2) => foldTD f (foldTD f new_acc lamb1) lamb2
           | SWITCH_I {switch,precision} => foldSwitch switch
           | SWITCH_W {switch,precision} => foldSwitch switch
           | SWITCH_S switch => foldSwitch switch
           | SWITCH_C switch => foldSwitch switch
           | SWITCH_E switch => foldSwitch switch
           | TYPED(lamb,t,_) => foldTD f new_acc lamb
           | PRIM(prim,lambs) => foldl' (foldTD f) new_acc lambs
           | FRAME _ => new_acc
      end


   (* -----------------------------------------------------------------
    * map_lamb f lamb - computes a new lamb' from lamb by applying f
    *   to sub-expressions.
    * ----------------------------------------------------------------- *)

    fun map_lamb_sw f (SWITCH(e,sel,opt_e)) =
       let fun map_sel [] = []
             | map_sel ((a,e)::rest) = (a,f e) :: map_sel rest
           fun map_opt (SOME e) = SOME (f e)
             | map_opt NONE = NONE
       in SWITCH(f e, map_sel sel, map_opt opt_e)
       end

    fun map_lamb f lamb =
      case lamb
        of VAR _ => lamb
         | INTEGER _ => lamb
         | WORD _ => lamb
         | REAL _ => lamb
         | F64 _ => lamb
         | STRING _ => lamb
         | FN{pat,body} => FN{pat=pat,body=f body}
         | LET{pat,bind,scope} => LET{pat=pat,bind=f bind,scope=f scope}
         | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=f scope}
         | FIX{functions,scope} =>
           FIX{functions=map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                                 {lvar=lvar,regvars=regvars,
                                  tyvars=tyvars,Type=Type,constrs=constrs,
                                  bind=f bind}) functions,
                   scope=f scope}
         | APP(e1,e2, tc) => APP(f e1, f e2, tc)
         | EXCEPTION(excon,ty_opt,scope) => EXCEPTION(excon,ty_opt, f scope)
         | RAISE(e,tl) => RAISE(f e, tl)
         | HANDLE(e1,e2) => HANDLE(f e1, f e2)
         | SWITCH_I {switch,precision} =>
               SWITCH_I {switch=map_lamb_sw f switch, precision=precision}
         | SWITCH_W {switch,precision} =>
               SWITCH_W {switch=map_lamb_sw f switch, precision=precision}
         | SWITCH_S sw => SWITCH_S (map_lamb_sw f sw)
         | SWITCH_C sw => SWITCH_C (map_lamb_sw f sw)
         | SWITCH_E sw => SWITCH_E (map_lamb_sw f sw)
         | TYPED(e,t,cs) => TYPED(f e,t,cs)
         | PRIM(prim, lambs) => PRIM(prim, map f lambs)
         | FRAME _ => lamb


   (* -----------------------------------------------------------------
    * app_lamb f lamb - apply f to sub-expressions.
    * ----------------------------------------------------------------- *)

    fun app_lamb_sw f (SWITCH(e,sel,opt_e)) =
       let fun app_sel [] = ()
             | app_sel ((a,e)::rest) = (f e; app_sel rest)
           fun app_opt (SOME e) = f e
             | app_opt NONE = ()
       in f e; app_sel sel; app_opt opt_e
       end

    fun app_lamb f lamb =
      case lamb
        of VAR _ => ()
         | INTEGER _ => ()
         | WORD _ => ()
         | REAL _ => ()
         | F64 _ => ()
         | STRING _ => ()
         | FN{pat,body} => f body
         | LET{pat,bind,scope} => (f bind; f scope)
         | LETREGION{regvars,scope} => f scope
         | FIX{functions,scope} => (app (f o #bind) functions; f scope)
         | APP(e1,e2,_) => (f e1; f e2)
         | EXCEPTION(excon,ty_opt,scope) => f scope
         | RAISE(e,tl) => f e
         | HANDLE(e1,e2) => (f e1; f e2)
         | SWITCH_I {switch,...} => app_lamb_sw f switch
         | SWITCH_W {switch,...} => app_lamb_sw f switch
         | SWITCH_S sw => app_lamb_sw f sw
         | SWITCH_C sw => app_lamb_sw f sw
         | SWITCH_E sw => app_lamb_sw f sw
         | TYPED(e,t,_) => f e
         | PRIM(prim, lambs) => app f lambs
         | FRAME _ => ()


    (* -----------------------------------------------------------------
     * Computation of free lambda variables and free exception
     * constructors; we set a mark on a lambda variable when it is
     * collected, so that it is collected only once.
     * ----------------------------------------------------------------- *)

    fun freevars e : lvar list * excon list =
      let val excons_seen : excon list ref = ref nil
          val excon_bucket : excon list ref = ref nil
          val lvar_bucket : lvar list ref = ref nil
          fun insert_lv lv = if !(Lvars.is_inserted lv) then ()
                             else (Lvars.is_inserted lv := true;
                                   lvar_bucket := lv :: !lvar_bucket)
          fun insert_excon ex = if List.exists (fn ex1 => Excon.eq(ex,ex1)) (!excons_seen) then ()
                                else (excon_bucket := ex :: !excon_bucket;
                                      excons_seen := ex :: !excons_seen)
          fun clean e : unit =
            case e
              of VAR {lvar,...} => Lvars.is_inserted lvar := false
               | _ => app_lamb clean e

          fun fv e : unit =
            case e
              of VAR {lvar,...} => insert_lv lvar
               | FN{pat,body} => (app (fn (lv,_) => Lvars.is_inserted lv := true) pat;
                                  fv body;
                                  app (fn (lv,_) => Lvars.is_inserted lv := false) pat)
               | LET{pat,bind,scope} => (fv bind;
                                         app (fn (lv,_,_) => Lvars.is_inserted lv := true) pat;
                                         fv scope;
                                         app (fn (lv,_,_) => Lvars.is_inserted lv := false) pat)
               | FIX{functions,scope} => (app (fn {lvar,...} => Lvars.is_inserted lvar := true) functions;
                                          app (fv o #bind) functions; fv scope;
                                          app (fn {lvar,...} => Lvars.is_inserted lvar := false) functions)
               | EXCEPTION(excon,ty_opt,scope) =>
                (excons_seen := excon :: !excons_seen;
                 fv scope;
                 excons_seen := List.filter (fn ex => not(Excon.eq(ex,excon))) (!excons_seen))
               | PRIM(EXCONprim excon, lambs) => (insert_excon excon; app fv lambs)
(*             | PRIM(DEEXCONprim excon, lambs) => (insert_excon excon; app fv lambs) *)
               | _ => app_lamb fv e
          val _ = clean e
          val _ = fv e
          val lvs = !lvar_bucket before (app (fn lv => Lvars.is_inserted lv := false) (!lvar_bucket);
                                         lvar_bucket := nil)
          val exs = !excon_bucket before excon_bucket := nil
(*
          val _ = print ("Free lvars " ^ PP.flatten1 (PP.layout_list (PP.LEAF o Lvars.pr_lvar) lvs) ^ "\n")
          val _ = print ("Free excons " ^ PP.flatten1 (PP.layout_list (PP.LEAF o Excon.pr_excon) exs) ^ " in\n")
          val _ = PP.outputTree (print, layoutLambdaExp e, 100)
          val _ = print "\n"
*)
      in
        (lvs, exs)
      end


    (* --------- *)
    (* Renamings *)
    (* --------- *)

    local
      fun new_tv tv = if equality_tyvar tv then fresh_eqtyvar()
                      else fresh_tyvar()

      structure LvMap = Lvars.Map
      structure TvMap = LambdaExp.TyvarMap
      structure ExMap = Excon.Map

      type ren = lvar LvMap.map * tyvar TvMap.map * excon ExMap.map

      val empty_ren = (LvMap.empty, TvMap.empty, ExMap.empty)

      fun add_lv (lv, lv', (lv_map, tv_map, ex_map)) =
          (LvMap.add (lv,lv',lv_map), tv_map, ex_map)

      fun add_lvs [] ren = ren
        | add_lvs ((lv,lv')::pairs) ren =
          add_lvs pairs (add_lv (lv,lv',ren))

      fun add_tv (tv, tv', (lv_map, tv_map, ex_map)) =
          (lv_map, TvMap.add(tv,tv',tv_map), ex_map)

      fun add_tvs [] ren = ren
        | add_tvs ((tv,tv')::pairs) ren =
          add_tvs pairs (add_tv (tv,tv',ren))

      fun add_ex (ex, ex', (lv_map, tv_map, ex_map)) =
          (lv_map, tv_map, ExMap.add (ex,ex',ex_map))

      fun add_exs [] ren = ren
        | add_exs ((ex,ex')::pairs) ren = add_exs pairs (add_ex (ex,ex',ren))

      fun on_tv (_, tv_map, _) tv = case TvMap.lookup tv_map tv of
                                        SOME tv => tv
                                      | NONE => tv

      fun on_lv (lv_map, _, _) lv = case LvMap.lookup lv_map lv of
                                        SOME lv => lv
                                      | NONE => lv

      fun on_lv_opt s (SOME lv) = SOME (on_lv s lv)
        | on_lv_opt _ NONE = NONE

      fun on_ex (_, _, ex_map) ex = case ExMap.lookup ex_map ex of
                                        SOME ex => ex
                                      | NONE => ex

      fun on_tau ren tau =
        let fun on_t (TYVARtype {tv}) = TYVARtype {tv=on_tv ren tv}
              | on_t (ARROWtype (tl,rv0,tl',rv)) = ARROWtype(map on_t tl,rv0,map on_t tl',rv)
              | on_t (CONStype (tl,tn,rvs)) = CONStype (map on_t tl,tn,rvs)
              | on_t (RECORDtype (tl,rv)) = RECORDtype (map on_t tl,rv)
        in on_t tau
        end

      fun on_tl ren (Types ts) = Types (map (on_tau ren) ts)
        | on_tl _ tl = tl

      fun on_fnpat ren [] = []
        | on_fnpat ren ((lv,tau)::pat) =
          (on_lv ren lv, on_tau ren tau) :: on_fnpat ren pat

      fun on_letpat ren [] = []
        | on_letpat ren ((lv,tvs,tau)::pat) =
          (on_lv ren lv, map (on_tv ren) tvs, on_tau ren tau) :: on_letpat ren pat

      fun new_fnpat pat ren =
        let val lvs = map #1 pat
            val lvs_pairs = map (fn lv => (lv, Lvars.renew lv)) lvs
            val ren' = add_lvs lvs_pairs ren
        in (on_fnpat ren' pat, ren')
        end

      fun flatten [] = []
        | flatten (tvs::tvss) = tvs @ flatten tvss

      fun rem_dubs tvs [] = tvs
        | rem_dubs tvs' (tv::tvs) =
        let fun is_in tv [] = false
              | is_in tv (tv'::tvs) = tv = tv' orelse is_in tv tvs
        in if is_in tv tvs' then rem_dubs tvs tvs'
           else rem_dubs (tv::tvs') tvs
        end

      fun new_letpat pat ren =
        let val lvs = map #1 pat
            val tvs = ((rem_dubs []) o flatten) (map #2 pat)
            val lvs_pairs = map (fn lv => (lv, Lvars.renew lv)) lvs
            val tvs_pairs = map (fn tv => (tv, new_tv tv)) tvs
            val ren_bind = add_tvs tvs_pairs ren
            val ren_scope = add_lvs lvs_pairs ren
            val ren_pat = add_lvs lvs_pairs ren_bind
        in (on_letpat ren_pat pat, ren_bind, ren_scope)
        end

      fun on_sw on_a on_e (SWITCH(e,sel,opt_e)) =
          SWITCH(on_e e, map (fn (a, e) => (on_a a, on_e e)) sel,
                 case opt_e of SOME e => SOME (on_e e) | NONE => NONE)

      fun on_constr ren c =
          case c of
              DISJOINTconstr (e1,e2,p,rep,lvopt) => DISJOINTconstr (e1,e2,p,rep,on_lv_opt ren lvopt)
            | INCLconstr (r,e,rep,lvopt) => INCLconstr (r,e,rep,on_lv_opt ren lvopt)
            | PROPconstr (p,e,rep,lvopt) => PROPconstr (p,e,rep,on_lv_opt ren lvopt)

      fun on_functions ren on_e fns =
          let val lvs = map #lvar fns
              val tvs = ((rem_dubs []) o flatten) (map #tyvars fns)
              val lvs_pairs = map (fn lv => (lv, Lvars.renew lv)) lvs
              val tvs_pairs = map (fn tv => (tv, new_tv tv)) tvs
              val ren' = add_lvs lvs_pairs ren
              val ren_binds = add_tvs tvs_pairs ren'
              fun on_function {lvar,regvars,tyvars,Type,constrs,bind} =
                  {lvar=on_lv ren' lvar,
                   regvars=regvars,
                   tyvars=map (on_tv ren_binds) tyvars,
                   Type=on_tau ren_binds Type,
                   constrs=map (on_constr ren_binds) constrs,
                   bind=on_e ren_binds bind}
          in (map on_function fns, ren')
          end

      fun on_prim ren prim =
          case prim of
              CONprim {con,instances,regvar} =>
              CONprim {con=con, instances=map (on_tau ren) instances, regvar=regvar}
            | DECONprim {con,instances,lv_opt} =>
              DECONprim {con=con, instances=map (on_tau ren) instances,lv_opt=on_lv_opt ren lv_opt}
            | EXCONprim excon => EXCONprim (on_ex ren excon)
            | DEEXCONprim excon => DEEXCONprim (on_ex ren excon)
            | DEREFprim {instance} => DEREFprim {instance=on_tau ren instance}
            | REFprim {instance,regvar} => REFprim {instance=on_tau ren instance,regvar=regvar}
            | ASSIGNprim {instance} => ASSIGNprim {instance=on_tau ren instance}
            | EQUALprim {instance} => EQUALprim {instance=on_tau ren instance}
            | CCALLprim {name, instances, tyvars, Type} =>
              let val tvs_pairs = map (fn tv => (tv, new_tv tv)) tyvars
                  val ren_local = add_tvs tvs_pairs empty_ren
              in CCALLprim {name=name, instances=map (on_tau ren) instances,
                            tyvars=map (on_tv ren_local) tyvars, Type=on_tau ren_local Type}
              (*the type scheme (tyvars, Type) is for a special purpose in the
               region infere    nce and back end; it must not be changed; we must rename bound
               tyvars, however.     *)
              end
            | RESET_REGIONSprim {instance} => RESET_REGIONSprim {instance=on_tau ren instance}
            | FORCE_RESET_REGIONSprim {instance} => FORCE_RESET_REGIONSprim {instance=on_tau ren instance}
            | x => x

      fun on_e ren lamb =
          case lamb of
              VAR{lvar,instances,regvars} =>
              VAR{lvar=on_lv ren lvar,
                  instances=map (on_tau ren) instances,
                  regvars=regvars}
            | INTEGER _ => lamb
            | WORD _ => lamb
            | STRING _ => lamb
            | REAL _ => lamb
            | F64 _ => lamb
            | FN{pat,body} => let val (pat', ren') = new_fnpat pat ren
                              in FN{pat=pat', body=on_e ren' body}
                              end
            | LET{pat,bind,scope} => let val (pat', ren_bind, ren_scope) = new_letpat pat ren
                                     in LET{pat=pat',bind=on_e ren_bind bind, scope=on_e ren_scope scope}
                                     end
            | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=on_e ren scope}
            | FIX{functions,scope} => let val (functions', ren') = on_functions ren on_e functions
                                      in FIX{functions=functions', scope=on_e ren' scope}
                                      end
            | APP(e1,e2,tc) => APP(on_e ren e1, on_e ren e2, tc)
            | EXCEPTION(ex, ty_opt, e) =>
              let val ex' = Excon.renew ex
                  val ren' = add_ex(ex,ex',ren)
              in EXCEPTION(ex', case ty_opt of SOME tau => SOME (on_tau ren tau) | NONE => NONE,
                           on_e ren' e)
              end
            | RAISE(e,tl) => RAISE(on_e ren e, on_tl ren tl)
            | HANDLE(e1,e2) => HANDLE(on_e ren e1, on_e ren e2)
            | SWITCH_I {switch,precision} =>
              SWITCH_I {switch=on_sw (fn a => a) (on_e ren) switch, precision=precision}
            | SWITCH_W {switch,precision} =>
              SWITCH_W {switch=on_sw (fn a => a) (on_e ren) switch, precision=precision}
            | SWITCH_S sw => SWITCH_S (on_sw (fn a => a) (on_e ren) sw)
            | SWITCH_C sw => SWITCH_C (on_sw (fn (c,lv_opt) => (c,on_lv_opt ren lv_opt)) (on_e ren) sw)
            | SWITCH_E sw => SWITCH_E (on_sw (fn (e,lv_opt) => (on_ex ren e, on_lv_opt ren lv_opt)) (on_e ren) sw)
            | TYPED(e,t,cs) => TYPED(on_e ren e, on_tau ren t,cs)
            | PRIM(prim,es) => PRIM(on_prim ren prim, map (on_e ren) es)
            | FRAME _ => lamb

            (* MEMO: frames; hopefully, we will name rename expressions containing frames... *)
    in
      fun new_instance (lamb : LambdaExp) : LambdaExp = on_e empty_ren lamb
    end (*local*)


     (* exports lamb - finds the exported lvars and excons of a lambda
      * expression lamb. There may not be any frame!! *)
    fun exports lamb : (lvar list * excon list) =
      let
        exception Found of lvar list * excon list
        fun f () (FRAME{declared_lvars, declared_excons}) =
                  raise Found (map #lvar declared_lvars, map #1 declared_excons)
          | f () _ = ()
      in (foldTD f () lamb;
          ([], []))      (* in case there is no frame, no variables are exported... *)
        handle Found p => p
      end

    (* ------------- *)
    (* Substitutions *)
    (* ------------- *)

    local
      exception TypeVariableCapture
        (* raised if the application of a substitution
         * would lead to type variable capture
         *)

      type subst = (tyvar * Type) list         (* `Oldest' substitutions are at the head of the list *)

      fun mk_subst f p =
        let fun mk ([],[]) = []
              | mk (tv::tvs,t::ts) = (tv,t) :: mk(tvs,ts)
              | mk (l1,l2) = die ("mk_subst: "
                                  ^ Int.toString (length l1) ^ " tyvars, "
                                  ^ Int.toString (length l2) ^ " types; " ^ f())
        in mk p
        end

      fun on_Type [] tau : Type = tau
        | on_Type S tau : Type =
        let
          fun tv_Subst tau =
            (case tau
                 of TYVARtype {tv=tyvar} => (case List.find (fn (tyvar':tyvar, tau') => tyvar = tyvar') S
                                              of SOME res => #2 res
                                               | NONE => tau)
                  | ARROWtype(taus1,rv0,taus2,rv) => ARROWtype(map tv_Subst taus1,rv0,map tv_Subst taus2,rv)
                  | CONStype(taus,tyname,rvs) => CONStype(map tv_Subst taus,tyname,rvs)
                  | RECORDtype (taus,rv) => RECORDtype (map tv_Subst taus,rv)
            )
        in tv_Subst tau
        end

      fun on_Types [] types : Type list = types
        | on_Types S types = map (on_Type S) types

      fun on_TypeList [] typelist = typelist
        | on_TypeList S (Types ts) = Types (on_Types S ts)
        | on_TypeList S tl = tl                              (* no free tyvars in a frame! *)

      fun on_prim [] (prim: Type prim) : Type prim = prim   (* basically the same function as above for *)
        | on_prim S (prim: Type prim) : Type prim =         (* renamings; mael *)
        case prim
          of CONprim {con,instances,regvar} => CONprim {con=con, instances=on_Types S instances, regvar=regvar}
           | DECONprim {con, instances,lv_opt} => DECONprim {con=con,instances=on_Types S instances,lv_opt=lv_opt}
           | DEREFprim {instance} => DEREFprim{instance=on_Type S instance}
           | REFprim {instance,regvar} => REFprim{instance=on_Type S instance,regvar=regvar}
           | ASSIGNprim {instance} => ASSIGNprim{instance=on_Type S instance}
           | EQUALprim {instance} => EQUALprim{instance=on_Type S instance}
           | CCALLprim {name, instances, tyvars, Type} =>
            CCALLprim {name=name, instances=map (on_Type S) instances,
                       tyvars=tyvars, Type=Type}
              (*the type scheme (tyvars, Type) is for a special purpose in the
               region inference and back end; it is closed (i.e., ftv(Type) \subseteq {tyvars}) *)
           | RESET_REGIONSprim {instance} => RESET_REGIONSprim{instance=on_Type S instance}
           | FORCE_RESET_REGIONSprim {instance} => FORCE_RESET_REGIONSprim{instance=on_Type S instance}
           | _ => prim

      (* tyvarsType : Find the type variables in a type. We use Set
       * (instead of EqSet) as one easily could imagine that type
       * variables one day or another are changed in such a way that
       * they don't admit equality. *)

      fun equal_tyvar x y = x = y
      fun tyvarsType tau : tyvar Set.Set =
        case tau
          of TYVARtype {tv=tyvar} => Set.singleton tyvar
           | ARROWtype(taus1,_,taus2,_) => Set.union equal_tyvar (tyvarsTypes taus1) (tyvarsTypes taus2)
           | CONStype(taus,_,_) => tyvarsTypes taus
           | RECORDtype (taus,_) => tyvarsTypes taus
      and tyvarsTypes taus =
        foldl (fn (tau, set) =>
                    Set.union equal_tyvar (tyvarsType tau) set) Set.empty taus


      fun on_LambdaExp [] lamb = lamb
        | on_LambdaExp S lamb =
        let
          fun tyvars_rangeS S : tyvar Set.Set =
            let val domS = map ((fn tv => TYVARtype {tv=tv}) o #1) S
                val rangeS = on_Types S (domS)
            in tyvarsTypes rangeS
            end

          fun restrictS S from = List.filter (fn (tv,tau) => not(member tv from)) S

          fun check_capture S bound_tyvars =
            let val S' = restrictS S bound_tyvars
              val intersection =
                Set.intersect equal_tyvar
                (tyvars_rangeS S')
                (Set.fromList equal_tyvar bound_tyvars)
            in
              if Set.isEmpty intersection then S'
              else raise TypeVariableCapture
            end

          fun on_bound_lvar S {lvar,tyvars,Type} =
            let val S' = check_capture S tyvars
            in (S', {lvar=lvar,tyvars=tyvars,Type=on_Type S' Type})
            end

          fun on_let_pat S atpats =
            foldr (fn ((lvar,tyvars,tau),(S, atpats)) =>
                        let val S' = check_capture S tyvars
                        in (S', (lvar, tyvars, on_Type S' tau)::atpats)
                        end)
            (S, []) atpats

          fun f S lamb =
            let
              fun on_switch S (SWITCH(arg,selections,wildcard)) =
                SWITCH(f S arg,
                       map (fn (const,x) => (const, f S x)) selections,
                       case wildcard
                         of SOME lamb => SOME(f S lamb)
                          | NONE => NONE)
            in
              case lamb
                of VAR{lvar,instances,regvars} => VAR{lvar=lvar,instances=on_Types S instances,regvars=regvars}
                 | INTEGER _ => lamb
                 | WORD _ => lamb
                 | STRING _ => lamb
                 | REAL _ => lamb
                 | F64 _ => lamb
                 | FN{pat,body} => FN{pat = map (fn (lv, Type) => (lv, on_Type S Type)) pat,
                                      body = f S body}
                 | LET{pat,bind,scope} => let val (S',pat') = on_let_pat S pat
                                          in LET{pat = pat',
                                                 bind = f S' bind,
                                                 scope = f S scope}
                                          end
                 | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=f S scope}
                 | FIX{functions,scope} =>
                  let
                    fun on_function {lvar,regvars,tyvars,Type,constrs,bind} =
                      let val S' = check_capture S tyvars
                      in {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=on_Type S' Type,constrs=constrs,bind=f S' bind}
                      end
                  in FIX{functions=map on_function functions, scope=f S scope}
                  end
                 | APP(lamb1,lamb2,tc) => APP(f S lamb1,f S lamb2,tc)
                 | EXCEPTION(excon,tau_opt,lamb) =>
                  EXCEPTION(excon,
                            case tau_opt
                              of NONE => NONE
                               | SOME tau => SOME (on_Type S tau),
                            f S lamb)
                 | RAISE(lamb,tl) => RAISE(f S lamb,on_TypeList S tl)
                 | HANDLE(lamb1,lamb2) => HANDLE(f S lamb1,f S lamb2)
                 | SWITCH_I {switch,precision} =>
                  SWITCH_I {switch=on_switch S switch, precision=precision}
                 | SWITCH_W {switch,precision} =>
                  SWITCH_W {switch=on_switch S switch, precision=precision}
                 | SWITCH_S switch => SWITCH_S(on_switch S switch)
                 | SWITCH_C switch => SWITCH_C(on_switch S switch)
                 | SWITCH_E switch => SWITCH_E(on_switch S switch)
                 | TYPED(lamb,t,cs) => TYPED(f S lamb,on_Type S t,cs)
                 | PRIM (prim,lambs) => PRIM(on_prim S prim,map (f S) lambs)
                 | FRAME _ => lamb (*MEMO*)
            end
        in
          f S lamb
        end

    in (*local*)

      exception TypeVariableCapture = TypeVariableCapture

      structure TvMap = LambdaExp.TyvarMap

      type subst = subst

      val mk_subst = mk_subst

      fun mk_subst' ps = ps

      val on_Type = on_Type

      val on_LambdaExp = on_LambdaExp

      fun eq_regvar_opt (NONE, NONE) = true
        | eq_regvar_opt (SOME rv1, SOME rv2) = RegVar.eq(rv1,rv2)
        | eq_regvar_opt _ = false

    (* Equality of types, but disregarding regvar information *)
      fun eq_Type (tau1, tau2) =
        case (tau1,tau2)
          of (TYVARtype {tv=tv1}, TYVARtype {tv=tv2}) => tv1=tv2
           | (ARROWtype(taus1,rv0,taus1',rv1), ARROWtype(taus2,rv0',taus2',rv2)) =>
            eq_Types(taus1,taus2) andalso eq_Types(taus1',taus2') (*andalso eq_regvar_opt (rv1,rv2)*)
           | (CONStype(taus1,tn1,rvs1), CONStype(taus2,tn2,rvs2)) =>
            eq_Types(taus1,taus2) andalso TyName.eq(tn1,tn2) (*andalso eq_regvar_list_opt (rvs1,rvs2)*)
           | (RECORDtype (taus1,rv1), RECORDtype (taus2,rv2)) => eq_Types(taus1,taus2) (*andalso eq_regvar_opt (rv1,rv2)*)
           | _ => false
      and eq_Types ([],[]) = true
        | eq_Types (tau1::taus1,tau2::taus2) = eq_Type(tau1,tau2) andalso eq_Types(taus1,taus2)
        | eq_Types _ = false

      fun eq_sigma_with_il (([],tau1,[]),([],tau2,[])) = eq_Type(tau1,tau2)
        | eq_sigma_with_il ((tvs1,tau1,il1),(tvs2,tau2,il2)) =
        if length tvs1 <> length tvs2 then false
        else let val tv_taus = map (fn _ => TYVARtype {tv=fresh_tyvar()}) tvs1
                 val S1 = mk_subst (fn () => "eq_sigma_with_il1") (tvs1,tv_taus)
                 val S2 = mk_subst (fn () => "eq_sigma_with_il2") (tvs2,tv_taus)
                 val tau1' = on_Type S1 tau1
                 val tau2' = on_Type S2 tau2
                 val il1' = map (on_Type S1) il1
                 val il2' = map (on_Type S2) il2
             in eq_Type(tau1',tau2') andalso eq_Types(il1',il2')
             end

      fun eq_sigma ((tvs1,tau1),(tvs2,tau2)) =
        eq_sigma_with_il((tvs1,tau1,[]),(tvs2,tau2,[]))

      fun match_sigma ((tvs,tau), tau') =
        let fun add (tv,tau,S) =
              case TvMap.lookup S tv
                of SOME tau' => if eq_Type(tau,tau') then S
                                else die "match_sigma.add"
                 | NONE => TvMap.add(tv,tau,S)

            fun match_tau (S, tau, tau') =
              case (tau, tau')
                of (TYVARtype {tv}, _) => add(tv,tau',S)
                 | (ARROWtype(taus1,_,taus1',_), ARROWtype(taus2,_,taus2',_)) =>
                  let val S' = match_taus(S,taus1,taus2)
                  in match_taus(S',taus1',taus2')
                  end
                 | (RECORDtype (taus, _), RECORDtype (taus',_)) => match_taus(S,taus,taus')
                 | (CONStype(taus,tn,_), CONStype(taus', tn',_)) =>
                  if TyName.eq(tn,tn') then match_taus(S,taus,taus')
                  else die ("match_tau.CONStype: type name " ^ TyName.pr_TyName tn ^ " <> " ^ TyName.pr_TyName tn')
                 | _ => die "match_tau3"

            and match_taus (S,[],[]) = S
              | match_taus (S,tau::taus,tau'::taus') =
              let val S' = match_tau(S,tau,tau')
              in match_taus(S',taus,taus')
              end
              | match_taus _ = die "match_taus"

            val S = match_tau(TvMap.empty,tau,tau')
            val subst = map (fn tv => case TvMap.lookup S tv
                                        of SOME tau => (tv,tau)
                                         | NONE => (tv,TYVARtype {tv=tv})) tvs
        in subst
        end

      fun contains_f64Type t =
          case t of
              TYVARtype _ => false
	    | ARROWtype(ts1,_,ts2,_) => contains_f64Types ts1 orelse contains_f64Types ts2
	    | CONStype(ts,tn,_) => TyName.eq(TyName.tyName_F64,tn) orelse contains_f64Types ts
	    | RECORDtype (ts,_) => contains_f64Types ts
      and contains_f64Types nil = false
        | contains_f64Types (t::ts) = contains_f64Type t orelse contains_f64Types ts

    end (*local*)

    structure TVS = TyvarSet

    fun close (PGM(db,e)) =
        let
(*          val e = new_instance e *)
          val tvs = tyvars_Exp (TVS.empty) e (TVS.empty)
          val tvs = TVS.list tvs
          val S = mk_subst' (map (fn tv => (tv,intDefaultType())) tvs)
          val e = on_LambdaExp S e
        in PGM(db,e)
        end

    structure Normalize =
    struct

      (* Normalize type schemes so that type schemes only bind type
       * variables that occur in the body of the type scheme *)
      type StringTree = PP.StringTree
      type sigma = tyvar list * Type
      type env = bool list option Lvars.Map.map
      val empty : env = Lvars.Map.empty
      val initial : env = Lvars.Map.empty
      val plus: env * env -> env = Lvars.Map.plus
      fun restrict (m:env,d) = Lvars.Map.restrict (Lvars.pr_lvar,m,d)
      fun enrich (m:env,m') = Lvars.Map.enrich (op =) (m,m')
      fun lookup m lv = Lvars.Map.lookup m lv
      val add = Lvars.Map.add
      fun pp_bl bl = String.concat (map (fn true => "1" | false => "0") bl)
      val layout = Lvars.Map.layoutMap {start="{",finish="}",eq="->",sep=","}
                                       (PP.LEAF o Lvars.pr_lvar)
                                       (fn NONE => PP.LEAF "none"
                                         | SOME bl => PP.LEAF (pp_bl bl))
      val pu = let open Pickle
               in Lvars.Map.pu Lvars.pu (optionGen(listGen(bool)))
               end

      fun t (nil,nil) = nil
        | t (true::bl,x::xs) = x::t(bl,xs)
        | t (false::bl,x::xs) = t(bl,xs)
        | t _ = die "T.error"

      fun normScheme (sigma : sigma) : sigma * bool list option =
          let val (tvs,tau) = sigma
            val tvs' = tyvars_Type TVS.empty tau TVS.empty
            val bl = map (fn tv => TVS.member tv tvs') tvs
            val a = List.all (fn x => x) bl
          in if a then (sigma, NONE)
             else ((t(bl,tvs),tau), SOME bl)
          end

      val F : env option ref = ref NONE

      fun N (E:env) (e:LambdaExp) : LambdaExp =
          let
            fun Ns (SWITCH(arg, sels, opt)) =
                SWITCH(N E arg, map (fn (a,e) => (a,N E e)) sels,
                       case opt of
                         SOME e => SOME(N E e)
                       | NONE => NONE)
          in
            case e of
              VAR{instances, lvar, regvars} =>
              (case lookup E lvar of
                 SOME(SOME bl) => VAR{instances=t(bl,instances),lvar=lvar,regvars=regvars}
               | SOME NONE => e
               | NONE => die "N.VAR")
            | INTEGER _ => e
            | WORD _ => e
            | STRING _ => e
            | REAL _ => e
            | F64 _ => e
            | FN{pat,body} =>
              let val E = foldl (fn ((lv,_),E) => add(lv,NONE,E)) E pat
              in FN{pat=pat,body=N E body}
              end
            | LET{pat,bind,scope} =>
              let val (pat,E') = foldr (fn ((lv,tvs,tau),(pat,E)) =>
                                           let val ((tvs,tau),obl) = normScheme (tvs,tau)
                                           in ((lv,tvs,tau)::pat, add(lv,obl,E))
                                           end) (nil,E) pat
              in LET{pat=pat,bind=N E bind,scope=N E' scope}
              end
            | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=N E scope}
            | FIX{functions,scope} =>
              let val Eb = foldl (fn ({lvar,...},E) => add(lvar,NONE,E)) E functions
                  val (functions,E') =
                      foldr (fn ({lvar,regvars,tyvars,Type,constrs,bind},(fns,E)) =>
                                let val ((tyvars,Type),obl) = normScheme (tyvars,Type)
                                in ({lvar=lvar,regvars=regvars,tyvars=tyvars,
                                     Type=Type,constrs=constrs,bind=N Eb bind}::fns, add(lvar,obl,E))
                                end) (nil,E) functions
              in FIX{functions=functions,scope=N E' scope}
              end
            | APP(e1, e2, tc) => APP(N E e1, N E e2, tc)
            | EXCEPTION(excon,tauOpt,e) => EXCEPTION(excon,tauOpt,N E e)
            | RAISE(e,tl) => RAISE(N E e, Ntl E tl)
            | HANDLE(e1, e2) => HANDLE(N E e1, N E e2)
            | SWITCH_I {switch,precision} => SWITCH_I{switch=Ns switch,precision=precision}
            | SWITCH_W {switch,precision} => SWITCH_W{switch=Ns switch,precision=precision}
            | SWITCH_S switch => SWITCH_S(Ns switch)
            | SWITCH_C switch => SWITCH_C(Ns switch)
            | SWITCH_E switch => SWITCH_E(Ns switch)
            | TYPED(e,t,cs) => TYPED(N E e, t,cs)
            | PRIM(p,es) => PRIM(p, map (N E) es)
            | FRAME fr => FRAME(Nf E fr)
          end

      and Ntl E tl =
          case tl of
            Types _ => tl
          | Frame fr => Frame (Nf E fr)
          | RaisedExnBind => tl

      and Nf E fr =
          let
            val {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
                 declared_excons: (excon * Type option) list} = fr
            val Ef = foldl (fn ({lvar,...},acc) =>
                               case lookup E lvar of
                                 SOME r => add(lvar,r,acc)
                               | NONE => die "N.Frame") empty declared_lvars
            val _ = F := SOME Ef
            val declared_lvars =
                map (fn {lvar,tyvars,Type} =>
                        let val ((tyvars,Type),_) = normScheme (tyvars,Type)
                        in {lvar=lvar,tyvars=tyvars,Type=Type}
                        end) declared_lvars
          in
            {declared_lvars=declared_lvars,
             declared_excons=declared_excons}
          end

      fun norm (E:env, PGM(db,e)) =
          let val _ = F := NONE
            val e = N E e
            val E' = case !F of SOME E' => E'
                              | NONE => die "norm"
            val _ = F := NONE
          in (PGM(db,e),E')
          end

    end

    (* Annotate tail call on APP constructs *)
    fun annotate_tail_calls (e: LambdaExp) : LambdaExp =
        let
          fun t tail e =
              let
                fun t_sw tail (SWITCH(arg, sels, opt)) =
                    SWITCH(t false arg, map (fn (a,e) => (a,t tail e)) sels,
                           case opt of
                             SOME e => SOME(t tail e)
                           | NONE => NONE)
              in
                case e of
                  VAR _ => e
                | INTEGER _ => e
                | WORD _ => e
                | STRING _ => e
                | REAL _ => e
                | F64 _ => e
                | FN{pat,body} => FN{pat=pat,body=t true body}
                | LET{pat,bind,scope} =>
                  LET{pat=pat,bind=t false bind,scope=t tail scope}
                | LETREGION{regvars,scope} => LETREGION{regvars=regvars,scope=t false scope}
                | FIX{functions,scope} =>
                  let val functions = map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                                              {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                                               constrs=constrs,bind=t false bind}) functions
                  in FIX{functions=functions,
                         scope=t tail scope}
                  end
                | APP(e1, e2, _) => APP(t false e1,t false e2,SOME tail)
                | EXCEPTION(excon,tauOpt,e) => EXCEPTION(excon,tauOpt,t tail e)
                | RAISE(e,tl) => RAISE(t false e, tl)
                | HANDLE(e1, e2) => HANDLE(t false e1, t false e2)
                | SWITCH_I {switch,precision} => SWITCH_I{switch=t_sw tail switch,precision=precision}
                | SWITCH_W {switch,precision} => SWITCH_W{switch=t_sw tail switch,precision=precision}
                | SWITCH_S switch => SWITCH_S(t_sw tail switch)
                | SWITCH_C switch => SWITCH_C(t_sw tail switch)
                | SWITCH_E switch => SWITCH_E(t_sw tail switch)
                | TYPED(e,tau,cs) => TYPED(t false e, tau,cs)
                | PRIM(p,es) => PRIM(p, map (t false) es)
                | FRAME fr => e
              end
        in t false e
        end

  end
