(* OptLambda - lambda code optimiser. *)

structure OptLambda : OPT_LAMBDA =
  struct

    structure LvarDiGraphScc =
      DiGraphScc(struct
                     type nodeId = Lvars.lvar
                     type info = Lvars.lvar
                     type edgeInfo = unit
                     val lt = Lvars.lt
                     fun getId lv = lv
                     val pu = Lvars.pu
                     structure Map = Lvars.Map
                 end)

    structure PP = PrettyPrint

    structure LvarMap = Lvars.Map

    open LambdaExp LambdaBasics
    type bound_lvar = {lvar:lvar, tyvars:tyvar list, Type: Type}

    fun mapi f xs =
        let fun loop (i,nil) = nil
              | loop (i,x::xs) = f(i,x) :: loop(i+1,xs)
        in loop (0,xs)
        end

    fun appi f xs =
        let fun loop (i,nil) = ()
              | loop (i,x::xs) =
                let val () = f(i,x) in loop(i+1,xs) end
        in loop (0,xs)
        end

    fun findi f xs =
        let fun loop (i,nil) = NONE
              | loop (i,x::xs) = if f x then SOME i
                                 else loop (i+1,xs)
        in loop (0,xs)
        end

    fun mem y xs = List.exists (fn x => x = y) xs

    fun concat2 nil = (nil,nil)
      | concat2 ((x,y)::ps) = let val (xs,ys) = concat2 ps
                              in (x @ xs, y @ ys)
                              end

    fun pr_Type t = PP.flatten1 (layoutType t)
    fun pr_Types ts = "[" ^ String.concatWith "," (map (PP.flatten1 o layoutType) ts) ^ "]"

    fun on_Types S ts = map (on_Type S) ts

   (* -----------------------------------------------------------------
    * Some Optimisation Constants
    * ----------------------------------------------------------------- *)

    val max_optimise = 20  (* maximal number of times the entire term is traversed. *)


   (* -----------------------------------------------------------------
    * Dynamic flags
    * ----------------------------------------------------------------- *)

    val optimise_p = Flags.add_bool_entry
        {long="optimiser",short=SOME "opt", menu=["Optimiser Control", "optimiser"],
         item=ref true, neg=true, desc=
         "Enable optimisation of intermediate language code\n\
          \(Lambda Expressions). Which optimisations are performed\n\
          \is controlled by individual flags. The optimisations\n\
          \include function inlining, function specialisation,\n\
          \fix-minimization, unboxing of function arguments, and\n\
          \elimination of unnecessary record constructions."}

    val statistics_after_optimisation = Flags.add_bool_entry
        {long="statistics_after_optimisation", short=NONE,
         menu=["Optimiser Control", "statistics after optimisation"],
         item=ref false, neg=false, desc=
         "Report optimisation statistics after optimisation of\n\
          \Lambda Expression."}

    val minimize_fixs = Flags.add_bool_entry
         {long="minimize_fixs", short=NONE, menu=["Optimiser Control", "minimize fixs"],
          item=ref true, neg=true, desc=
          "Minimize fix constructs (Lambda Expression Optimiser)."}

    val fix_conversion_ref = ref true  (* users should not be able to disable fix_conversion
                                        * because polymorphism in regions must co-work
                                        * with polymorphism in type variables *)

    val contract_p = Flags.add_bool_entry
         {long="contract", short=NONE, menu=["Optimiser Control", "contract"],
          item=ref true, neg=true, desc=
          "Contract is responsible for inlining, specialization,\n\
           \elimination of dead code, and much else (Lambda\n\
           \Expression Optimiser)."}

    val specialize_recursive_functions = Flags.add_bool_entry
          {long="specialize_recursive_functions", short=NONE,
           menu=["Optimiser Control", "specialize recursive functions"],
           item=ref true, neg=true, desc=
           "Specialise recursive functions. Use the option\n\
            \maximum_specialise_size to control which functions\n\
            \are specialised. If this flag is on, functions that are\n\
            \applied only once are specialised, no matter the setting\n\
            \of maximum_specialise_size (Lambda Expression Optimiser)."}

    val eliminate_explicit_records_p = Flags.add_bool_entry
           {long="eliminate_explicit_records", short=NONE,
            menu=["Optimiser Control", "eliminate explicit records"],
            item=ref true, neg=true, desc=
            "Eliminate bindings of explicit records only used for\n\
             \selections. Transform\n\
             \      let r = (e1,...,en) in ... #i r .. #j r ...\n\
             \into\n\
             \      let x1=e1 in ... let xn=en in ... xi .. xj ...\n\
             \(Lambda Expression Optimiser)."}

    val unbox_function_arguments = Flags.add_bool_entry
            {long="unbox_function_arguments", short=NONE,
             menu=["Optimiser Control", "unbox function arguments"],
             item=ref true, neg=true, desc=
             "Unbox arguments to fix-bound functions, for which the\n\
              \argument `a' is used only in contexts `#i a'. All call \n\
              \sites are transformed to match the new function (Lambda\n\
              \Expression Optimiser)."}

    val unbox_reals = Flags.add_bool_entry
            {long="unbox_reals", short=NONE,
             menu=["Optimiser Control", "unbox real values"],
             item=ref true, neg=true, desc=
             "Unbox real values and computations on real values inside\n\
             \functions. Real values stored in data structures and\n\
             \passed to functions are still boxed."}

    (* max size of recursive function defs. to be specialised. *)
    val max_specialise_size = Flags.add_int_entry
        {long="maximum_specialise_size",short=NONE,
         menu=["Optimiser Control", "maximum specialise size"],
         item=ref 200, desc=
         "Curried functions smaller than this size (counted in\n\
          \abstract syntax tree nodes) are specialised if all\n\
          \applications of the function within its own body are\n\
          \applied to its formal argument, even if they are used\n\
          \more than once. Functions that are used only once are\n\
          \specialised no matter their size. See also the option\n\
          \--specialize_recursive_functions."}

    (* max size of non-recursive function defs. to be inlined. *)
    val max_inline_size = Flags.add_int_entry
        {long="maximum_inline_size", short=NONE,
         menu=["Optimiser Control", "maximum inline size"],
         item=ref 200, desc=
         "Functions smaller than this size (counted in abstract\n\
          \syntax tree nodes) are inlined, even if they are used\n\
          \more than once. Functions that are used only once are\n\
          \always inlined."}

    (* names of functions to inline no-matter the setting of the flag
     * maximum_inline_size. *)
    val inline_names = Flags.add_stringlist_entry
        {long="inline_names",short=NONE,
         menu=["Optimiser Control", "inline names"],
         item=ref [], desc=
         "Names of functions that should always be inlined\n\
          \if possible, no matter the setting of the flag\n\
          \--maximum_inline_size."}

    fun always_inline_function (lvar:lvar) : bool =
        let val names = inline_names()
            val n = Lvars.pr_lvar lvar
        in List.exists (fn x => x = n) names
        end

    val cross_module_opt = Flags.add_bool_entry
        {long="cross_module_opt",short=SOME "cross_opt",
         menu=["Optimiser Control", "cross module optimisation"],
         item=ref true,neg=true,
         desc=
         "Enable cross-module optimisation including inlining\n\
          \of small functions and specialisation of small\n\
          \recursive functions. Which optimisations are performed\n\
          \across modules is controlled by individual optimisation\n\
          \flags."}

    val aggressive_opt = Flags.add_bool_entry
        {long="aggresive_opt",short=SOME "aopt",
         menu=["Optimiser Control", "aggressive optimisation"],
         item=ref true,neg=true,
         desc=
         "Enable aggressive optimisations, including constant\n\
         \folding and aggressive inlining. These\n\
         \optimisations are not guaranteed to be region\n\
         \safe. Turning off garbage collection automatically\n\
         \turns off this option."}

   val uncurrying_p = Flags.add_bool_entry
       {long="uncurrying",short=SOME "uncurry",
        menu=["Optimiser Control", "uncurrying"],
        item=ref true,neg=true,
        desc=
        "Enable uncurrying of curried functions. The uncurried\n\
         \function takes its arguments unboxed in registers or\n\
         \on the stack. For partial applications and non-\n\
         \application uses of the function, appropriate eta-\n\
         \expansions are applied."}

   (* -----------------------------------------------------------------
    * Some helpful functions
    * ----------------------------------------------------------------- *)

    val unit_Type = RECORDtype ([],NONE)

    fun log x = if statistics_after_optimisation() then TextIO.output(!Flags.log,x)
                else ()

    fun die s = Crash.impossible ("OptLambda." ^ s)

    fun reportBadLambda (msg,lamb) =
      (PP.outputTree((fn s => TextIO.output(!Flags.log, s)),
                     LambdaExp.layoutLambdaExp lamb,
                     !Flags.colwidth);
       die ("optimisation failed (" ^ msg ^ ")"))

    fun prLambdaExp s e =
        (print ("\n" ^ s ^ ": \n");
         PP.outputTree(print, LambdaExp.layoutLambdaExp e, 200);
         print "\n")

    fun fst (a,_) = a
    and snd (_,b) = b

    fun removeNth 0 (x::xs) = SOME(x,xs)
      | removeNth _ nil = NONE
      | removeNth n (x::xs) = case removeNth (n-1) xs of
                                  SOME(y,ys) => SOME(y,x::ys)
                                | NONE => NONE

    fun size_lamb lamb = foldTD (fn s => fn _ => s + 1) 0 lamb

    fun lvars_fn_pat pat = map (fn (lv,_) => lv) pat
    fun lvars_let_pat pat = map (fn (lv,_,_) => lv) pat

    fun fn_to_let_pat [] = []
      | fn_to_let_pat ((lv,tau)::rest) = (lv,[],tau) :: fn_to_let_pat rest

    fun is_in_lv lv lvs = List.exists (fn lv' => Lvars.eq(lv,lv')) lvs
    fun is_in_ex ex exs = List.exists (fn ex' => Excon.eq(ex,ex')) exs

    val lexp_true = PRIM(CONprim{con=Con.con_TRUE,instances=nil,regvar=NONE},nil)
    val lexp_false = PRIM(CONprim{con=Con.con_FALSE,instances=nil,regvar=NONE},nil)
    fun lexp_raise_ov t = RAISE (PRIM(EXCONprim Excon.ex_OVERFLOW,nil),Types [t])

    (* Operations on unboxed floats *)

    local
      type exp = LambdaExp
      fun ccall name argtypes restype =
          CCALLprim {name=name,instances=[],tyvars=[],
                     Type=ARROWtype(argtypes,NONE,[restype],NONE)}
      fun f64_bin opr (x:exp,y:exp) : exp =
          PRIM(ccall ("__" ^ opr ^ "_f64") [f64Type,f64Type] f64Type, [x,y])
      fun f64_uno opr (x:exp) : exp =
          PRIM(ccall ("__" ^ opr ^ "_f64") [f64Type] f64Type, [x])
      fun f64_cmp opr (x:exp,y:exp) : exp =
          PRIM(ccall ("__" ^ opr ^ "_f64") [f64Type,f64Type] boolType, [x,y])
    in
      fun f64_to_real (x:exp) : exp = PRIM(ccall "__f64_to_real" [f64Type] realType, [x])
      fun real_to_f64 (x:exp) : exp = PRIM(ccall "__real_to_f64" [realType] f64Type, [x])
      val f64_plus = f64_bin "plus"
      val f64_minus = f64_bin "minus"
      val f64_mul = f64_bin "mul"
      val f64_div = f64_bin "div"
      val f64_max = f64_bin "max"
      val f64_min = f64_bin "min"
      val f64_sqrt = f64_uno "sqrt"
      val f64_neg = f64_uno "neg"
      val f64_abs = f64_uno "abs"
      val f64_less = f64_cmp "less"
      val f64_lesseq = f64_cmp "lesseq"
      val f64_greater = f64_cmp "greater"
      val f64_greatereq = f64_cmp "greatereq"
    end

   (* -----------------------------------------------------------------
    * Statistical functions
    * ----------------------------------------------------------------- *)

    local
      type stat_map = int StringFinMap.map
      fun pad m =
        let val l = StringFinMap.list m
            val ss = map #1 l
            val max = List.foldl (fn (s,max) => if String.size s > max then String.size s else max) 0 ss
            fun space 0 = ""
              | space n = " " ^ space (n-1)
            fun add_space s = s ^ space (max - String.size s)
            val ss' = map add_space ss
            val l' = BasisCompat.ListPair.zipEq (ss', map #2 l) handle _ => die "pad"
            fun fromList [] = StringFinMap.empty
              | fromList ((a,b)::rest) = StringFinMap.add(a,b,fromList rest)
        in fromList l'
        end
      val stat_map : stat_map ref = ref StringFinMap.empty
      val layout_stat = (StringFinMap.layoutMap {start="Optimiser Statistics:", eq=" : ", sep=", ", finish =""}
        PP.LEAF (PP.LEAF o Int.toString)) o pad
      fun print_size_difference size_before size_after =
        let
          val per     = 100.0*(real (size_before - size_after))/(real size_before)
          val dec     = 10.0
          val rounded = (real (Real.round (dec*per)))/dec
        in
          log("\n------------------------------------------------\n" ^
              "   Size before: " ^ (Int.toString size_before) ^ ".\n" ^
              "   Size after:  " ^ (Int.toString size_after) ^ ".\n" ^
              (if size_before > 0 then
                 "   Reduced:     " ^ (Real.toString rounded) ^ "%.\n"
               else "\n") ^
              "-------------------------------------------------\n")
        end

      val tick_count_list = ref [0]
      fun end_round () = tick_count_list := 0 :: !tick_count_list
      fun incr_tick_counter () = case !tick_count_list
                                   of n::ns => tick_count_list := (n+1)::ns
                                    | _ => die "incr_tick_counter"
      fun print_tick_count_list () =
        let fun p [] = ""
              | p [n] = Int.toString n
              | p (n::ns) = Int.toString n ^ ", " ^ p ns
        in log ("ticks pr. round: " ^ p (rev (!tick_count_list)) ^ "\n")
        end

    in
      fun reset_statistics () = (stat_map := StringFinMap.empty; tick_count_list := [0])
      fun add_statistics (s:string) =
        let val map = !stat_map
            val i = case StringFinMap.lookup map s
                      of SOME i => i
                       | NONE => 0
            val new_stat_map = StringFinMap.add(s, i+1, map)
            val _ = incr_tick_counter()
        in stat_map := new_stat_map
        end
      val last_tick = ref ""
      fun print_stat lamb lamb' =
        if not (statistics_after_optimisation()) then ()
        else let val st = layout_stat (!stat_map)
             in PP.outputTree (log, st, !Flags.colwidth)
              ; print_size_difference (size_lamb lamb) (size_lamb lamb')
              ; print_tick_count_list ()
              ; log ("Last tick: '" ^ !last_tick ^ "'\n")
             end
      val end_round = end_round
    end


   (* -----------------------------------------------------------------
    * Ticking: tick is called every time a transformation is performed
    * reset_tick is called at the beginning of each pass. If no
    * transformations are performed in a pass tick is not called,
    * and the optimiser has done its job. Further, if statistics is
    * enabled, statistical information is recorded.
    * ----------------------------------------------------------------- *)

    local
      val flag = ref false
    in
      fun tick (s : string) = (flag := true; last_tick := s;
                               if statistics_after_optimisation() then add_statistics s
                               else ())
      fun reset_tick () = flag := false
      fun test_tick () = !flag
    end

   (* -----------------------------------------------------------------
    * Equality on lambda expressions (conservative approximation)
    * ----------------------------------------------------------------- *)

    local
        fun lvarsEq m (lv,lv') : bool = Lvars.eq(lv,lv')  (*free lvar*)
            orelse
            (case LvarMap.lookup m lv of
                 SOME lv'' => Lvars.eq(lv',lv'')
               | NONE => false)

        fun eq_pat m ((lv,t)::ps,(lv',t')::ps') =
            if eq_Type(t,t') then
                (case eq_pat m (ps,ps') of
                     SOME m => SOME (LvarMap.add(lv,lv',m))
                   | NONE => NONE)
            else NONE
          | eq_pat m (nil,nil) = SOME m
          | eq_pat _ _ = NONE

        fun eqOpt eq (NONE,NONE) = true
          | eqOpt eq (SOME a, SOME b) = eq (a,b)
          | eqOpt _ _ = false

        fun eqAll eq (nil,nil) = true
          | eqAll eq (x::xs,y::ys) = eq(x,y) andalso eqAll eq (xs,ys)
          | eqAll _ _ = false

        fun eq_TypeList (Types ts,Types ts') = eq_Types(ts,ts')
          | eq_TypeList _ = false

        fun eq_prim m (p,p') =
            case (p,p') of
                (RECORDprim {regvar=NONE}, RECORDprim {regvar=NONE}) => true
              | (RECORDprim {regvar=SOME rv1}, RECORDprim {regvar=SOME rv2}) => RegVar.eq(rv1,rv2)
              | (SELECTprim {index=i},SELECTprim {index=i'}) => i=i'
              | (CONprim {con,instances=il,regvar=NONE}, CONprim {con=con',instances=il',regvar=NONE}) =>
                    Con.eq(con,con') andalso eq_Types(il,il')
              | (CONprim {con,instances=il,regvar=SOME rv}, CONprim {con=con',instances=il',regvar=SOME rv'}) =>
                    Con.eq(con,con') andalso eq_Types(il,il') andalso RegVar.eq(rv,rv')
              | (EXCONprim excon, EXCONprim excon') => Excon.eq(excon,excon')
              | (DEEXCONprim excon, DEEXCONprim excon') => Excon.eq(excon,excon')
              | (UB_RECORDprim,UB_RECORDprim) => true
              | (DECONprim{con,instances=il,lv_opt}, DECONprim{con=con',instances=il',lv_opt=lv_opt'}) =>
                    Con.eq(con,con') andalso eq_Types(il,il') (* andalso eqOpt (lvarsEq m) (lv_opt,lv_opt') <-- only used with Barry *)
              | (DROPprim, DROPprim) => true
              | (DEREFprim {instance=t}, DEREFprim {instance=t'}) => eq_Type(t,t')
              | (REFprim {instance=t,regvar=NONE}, REFprim {instance=t',regvar=NONE}) => eq_Type(t,t')
              | (REFprim {instance=t,regvar=SOME rv}, REFprim {instance=t',regvar=SOME rv'}) =>
                eq_Type(t,t') andalso RegVar.eq(rv,rv')
              | (ASSIGNprim {instance=t}, ASSIGNprim {instance=t'}) => eq_Type(t,t')
              | (EQUALprim {instance=t}, EQUALprim {instance=t'}) => eq_Type(t,t')
              | (RESET_REGIONSprim {instance=t}, RESET_REGIONSprim {instance=t'}) => eq_Type(t,t')
              | (FORCE_RESET_REGIONSprim {instance=t}, FORCE_RESET_REGIONSprim {instance=t'}) => eq_Type(t,t')
              | (CCALLprim{name=n,instances=il,tyvars=tvs,Type=t}, CCALLprim{name=n',instances=il',tyvars=tvs',Type=t'}) =>
                    n = n' andalso eq_Types (il,il') andalso eq_sigma((tvs,t),(tvs',t'))
              | (EXPORTprim{name=n,instance_arg=a,instance_res=r}, EXPORTprim{name=n',instance_arg=a',instance_res=r'}) =>
                    n = n' andalso eq_Type(a,a') andalso eq_Type(r,r')
              | (BLOCKF64prim, BLOCKF64prim) => true
              | (SCRATCHMEMprim n, SCRATCHMEMprim n') => n = n'
              | _ => false

        fun eq_sw eq_lamb0m eq (SWITCH(e,es,eo),SWITCH(e',es',eo')) =
            eq_lamb0m (e,e')
            andalso eqAll (fn ((a,e),(a',e')) => eq(a,a') andalso eq_lamb0m (e,e')) (es,es')
            andalso eqOpt (eq_lamb0m) (eo,eo')

        fun eq_ateff (ae1,ae2) =
            case (ae1,ae2) of
                (VARateff r, VARateff r') => RegVar.eq(r,r')
              | (PUTateff r, PUTateff r') => RegVar.eq(r,r')
              | (GETateff r, GETateff r') => RegVar.eq(r,r')
              | _ => false

        fun eq_eff (e1,e2) =
            case (e1,e2) of
                (SETeff ats,SETeff ats') => ListPair.allEq eq_ateff (ats,ats')
              | (VAReff r, VAReff r') => RegVar.eq(r,r')
              | _ => false

        fun eq_lvopt (NONE, NONE) = true
          | eq_lvopt (SOME lv, SOME lv') = Lvars.eq(lv,lv')
          | eq_lvopt _ = false

        fun eq_constr (c1,c2) =
            case (c1,c2) of
                (DISJOINTconstr(e1,e2,p,rep,lvopt), DISJOINTconstr(e1',e2',p',rep',lvopt')) =>
                eq_eff (e1,e1') andalso eq_eff(e2,e2') andalso Report.eq(rep,rep') andalso eq_lvopt(lvopt,lvopt') andalso p=p'
              | (INCLconstr (r,e,rep,lvopt), INCLconstr (r',e',rep',lvopt')) =>
                RegVar.eq(r,r') andalso eq_eff(e,e') andalso Report.eq(rep,rep') andalso eq_lvopt(lvopt,lvopt')
              | _ => false

        fun eq_regvars (nil,nil) = true
          | eq_regvars (x::xs,y::ys) = RegVar.eq(x,y) andalso eq_regvars(xs,ys)
          | eq_regvars _ = false

        fun eq_lamb0 m (INTEGER (n,t), INTEGER (n',t')) = n=n' andalso eq_Type(t,t')
          | eq_lamb0 m (WORD(n,t), WORD(n',t')) = n=n' andalso eq_Type(t,t')
          | eq_lamb0 m (REAL (r,rvo), REAL (r',rvo')) = r = r' andalso RegVar.eq_opt(rvo,rvo')
          | eq_lamb0 m (STRING (s,rvo), STRING (s',rvo')) = s = s' andalso RegVar.eq_opt(rvo,rvo')
          | eq_lamb0 m (VAR{lvar,instances=il,regvars=rvs},VAR{lvar=lvar',instances=il',regvars=rvs'}) = lvarsEq m (lvar,lvar') andalso eq_Types(il,il') andalso eq_regvars(rvs,rvs')
          | eq_lamb0 m (PRIM(p,lambs),PRIM(p',lambs')) = eq_prim m (p,p') andalso eqAll (eq_lamb0 m) (lambs,lambs')
          | eq_lamb0 m (APP(e1,e2,tc),APP(e1',e2',tc')) = eq_lamb0 m (e1,e1') andalso eq_lamb0 m (e2,e2') andalso tc=tc'
          | eq_lamb0 m (HANDLE(e1,e2),HANDLE(e1',e2')) = eq_lamb0 m (e1,e1') andalso eq_lamb0 m (e2,e2')
          | eq_lamb0 m (RAISE(e,tl),RAISE(e',tl')) = eq_lamb0 m (e,e') andalso eq_TypeList(tl,tl')
          | eq_lamb0 m (SWITCH_I {switch=sw,precision=p}, SWITCH_I {switch=sw',precision=p'}) =
            p = p' andalso eq_sw (eq_lamb0 m) (op =) (sw,sw')
          | eq_lamb0 m (SWITCH_W {switch=sw,precision=p,tyname=tn}, SWITCH_W {switch=sw',precision=p',tyname=tn'}) =
            p = p' andalso eq_sw (eq_lamb0 m) (op =) (sw,sw') andalso TyName.eq(tn,tn')
          | eq_lamb0 m (SWITCH_S sw, SWITCH_S sw') = eq_sw (eq_lamb0 m) (op =) (sw,sw')
          | eq_lamb0 m (SWITCH_C sw, SWITCH_C sw') =
            (* optional lvars are only used with Barry, where they are used to make pretty-printing prettier *)
            eq_sw (eq_lamb0 m) (fn((c,lvo),(c',lvo')) => Con.eq(c,c') (* andalso eqOpt (eqLvars m) (lvo,lvo') *) ) (sw,sw')
          | eq_lamb0 m (SWITCH_E sw, SWITCH_E sw') =
            eq_sw (eq_lamb0 m) (fn((c,lvo),(c',lvo')) => Excon.eq(c,c') (* andalso eqOpt (eqLvars m) (lvo,lvo') *) ) (sw,sw')
          | eq_lamb0 m (TYPED(e,t,cs),TYPED(e',t',cs')) = eq_lamb0 m (e,e') andalso eq_Type(t,t') andalso ListPair.allEq eq_constr (cs,cs')
          | eq_lamb0 m (FN{pat,body},FN{pat=pat',body=body'}) =
            (case eq_pat m (pat,pat') of
                 SOME m => eq_lamb0 m (body,body')
               | NONE => false)
          | eq_lamb0 m (LET{pat=[(lv,tvs,t)],bind=b,scope=s},LET{pat=[(lv',tvs',t')],bind=b',scope=s'}) =
                 length tvs = length tvs'
                 andalso
                 let
                     val tv_taus = map (fn _ => TYVARtype {tv=fresh_tyvar()}) tvs
                     val S = mk_subst (fn () => "eq_lamb01.LET") (tvs,tv_taus)
                     val S' = mk_subst (fn () => "eq_lamb02.LET") (tvs',tv_taus)
                     val t = on_Type S t
                     val t' = on_Type S' t'
                 in eq_Type(t,t')
                     andalso
                     let
                         val b = on_LambdaExp S b
                         val b' = on_LambdaExp S' b'
                     in eq_lamb0 m (b,b')
                     end
                 end
                 andalso eq_lamb0 (LvarMap.add(lv,lv',m)) (s,s')
          | eq_lamb0 _ _ = false
    in
        fun eq_lamb p = eq_lamb0 LvarMap.empty p
    end


   (* -----------------------------------------------------------------
    * lvar_in_lamb lv lamb - Returns true if lv occurs free in lamb
    * and false otherwise.
    * ----------------------------------------------------------------- *)

   fun lvar_in_lamb (lv:lvar) (lamb:LambdaExp) =
     let
       exception Occurs
       fun occurs acc lamb =
         case lamb
           of VAR{lvar,...} => if Lvars.eq(lvar,lv) then raise Occurs else false
            | FRAME{declared_lvars,...} =>
              ( app (fn {lvar,...} => if Lvars.eq(lvar,lv)
                                      then raise Occurs else ()) declared_lvars
              ; false)
            | _ => false
     in
       (foldTD occurs false lamb) handle Occurs => true
     end



   (* -----------------------------------------------------------------
    * small_lamb max_size lamb - returns TRUE if the number of nodes
    * in lamb is no more than max_size.
    * ----------------------------------------------------------------- *)

    fun small_lamb max_size lamb =
      let exception Big
          fun count acc _ = if acc >= max_size then raise Big else acc+1
      in (foldTD count 0 lamb; true) handle Big => false
      end

    fun small_const lamb =
        case lamb of
          REAL _ => true
        | INTEGER _ => true
        | WORD _ => true
        | PRIM(CONprim {con,...},[]) => true
        | STRING (s, _) => String.size s < 100
        | _ => false

   (* -----------------------------------------------------
    * Closedness of a lambda expression ; used only for
    * cross-module optimisation...
    * ----------------------------------------------------- *)

   fun closed (lvars_free_ok:lvar list,
               excons_free_ok:excon list,
               lamb) : bool =
     let
       exception OPEN
       fun check_con con =
         if (Con.eq(con,Con.con_NIL) orelse Con.eq(con,Con.con_CONS)
             orelse Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE)) then ()
         else raise OPEN

       fun check_excon ex x =
         if is_in_ex ex x then () else raise OPEN
       fun appOpt f NONE = ()
         | appOpt f (SOME x) = f x
       fun c b x e =
         case e
           of VAR{lvar,...} => if is_in_lv lvar b then () else raise OPEN
            | PRIM(CONprim{con,...},es) => (check_con con ; app (c b x) es)
            | PRIM(DECONprim{con,...},es) => (check_con con ; app (c b x) es)
            | PRIM(EXCONprim ex,es) => (check_excon ex x ; app (c b x) es)
            | PRIM(DEEXCONprim ex,es) => (check_excon ex x ; app (c b x) es)
            | LET{pat,bind,scope} =>
             let fun add [] b = b
                   | add ((lv,_,_)::pat) b = add pat (lv::b)
             in c b x bind; c (add pat b) x scope
             end
            | FN{pat,body} =>
             let fun add [] b = b
                   | add ((lv,_)::pat) b = add pat (lv::b)
             in c (add pat b) x body
             end
            | FIX{functions,scope} =>
             let fun add [] b = b
                   | add ({lvar,regvars,tyvars,Type,constrs,bind}::fcs) b = add fcs (lvar::b)
                 val b' = add functions b
             in app ((c b' x) o #bind) functions; c b' x scope
             end
            | EXCEPTION(excon,_,lamb) => c b (excon::x) lamb
            | SWITCH_C(SWITCH(e,l,opt)) =>
             (  c b x e
              ; app (fn ((con,_),e) => (check_con con ; c b x e)) l   (*MEMO*)
              ; appOpt (c b x) opt)
            | SWITCH_E(SWITCH(e,l,opt)) =>
             (  c b x e
              ; app (fn ((ex,_),e) => (check_excon ex x ; c b x e)) l
              ; appOpt (c b x) opt)
            | FRAME _ => die "closed"
            | _ => app_lamb (c b x) e
     in (c lvars_free_ok excons_free_ok lamb; true) handle OPEN => false
     end

   (* Function for determining if a variable v (of type real) occurs in an
    * expression e only in contexts of the form __real_to_f64(v)
    * given that e is *simple* (contains no function applications,
    * no handlers, switches, or functions. *)

   fun real_lvar_f64_in_lamb (lv:lvar) (lamb:LambdaExp) : bool =
     let exception Bad
         fun check e = if lvar_in_lamb lv e then raise Bad else false
         fun safeLook_sw safeLook (SWITCH(e,es,eopt)) =
             if safeLook e then
               let val ss = map (safeLook o #2) es
                   val sopt = Option.map safeLook eopt
               in Option.getOpt (sopt,true) andalso List.all (fn x => x) ss
               end
             else (app (ignore o check o #2) es; Option.app (ignore o check) eopt; false)
         fun safeLook e =
             case e of
                 REAL _ => true
               | F64 _ => true
               | WORD _ => true
               | INTEGER _ => true
               | VAR{lvar,...} => if Lvars.eq(lvar,lv) then raise Bad else true
               | PRIM(CCALLprim{name="__real_to_f64",...},[VAR _]) => true
               | PRIM(_, es) => safeLooks es
               | LET{pat,bind,scope} => if safeLook bind then safeLook scope
                                        else check scope
               | SWITCH_C sw => safeLook_sw safeLook sw
               | FIX {functions,scope} =>
                 if safeLooks (map #bind functions) then safeLook scope
                 else check scope
               | FN {pat,body} => safeLook body
               | APP (e1,e2,_) => if safeLook e1 then safeLook e2 else check e2
               | _ => check e
         and safeLooks es =
             List.foldl (fn (e,s) => if s then safeLook e else check e) true es
     in
       (safeLook lamb; true) handle Bad => false
     end

   fun subst_real_lvar_f64_in_lamb (lv:lvar) (lamb:LambdaExp) : LambdaExp =
       let fun subst e =
             case e of
                 REAL _ => e
               | F64 _ => e
               | WORD _ => e
               | INTEGER _ => e
               | VAR _ => e
               | PRIM(CCALLprim{name="__real_to_f64",...},[e' as VAR {lvar,...}]) =>
                 if Lvars.eq(lvar,lv) then e' else e
               | PRIM(p,es) => PRIM(p,map subst es)
               | LET{pat,bind,scope} => LET{pat=pat,bind=subst bind,scope=subst scope}
               | FIX{functions,scope} =>
                 FIX{functions=map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                                       {lvar=lvar,regvars=regvars,
                                        tyvars=tyvars,Type=Type,constrs=constrs,bind=subst bind}) functions,
                     scope=subst scope}
               | FN{pat,body} => FN{pat=pat,body=subst body}
               | APP(e1,e2,b) => APP(subst e1,subst e2,b)
               | SWITCH_C(SWITCH(e,es,eopt)) =>
                 SWITCH_C(SWITCH(subst e, map (fn (x,e) => (x,subst e)) es, Option.map subst eopt))
               | _ => if lvar_in_lamb lv e then die "subst_real_lvar_f64_in_lamb: impossible"
                      else e
       in subst lamb
       end

   (* -----------------------------------------------------------------
    * Marking Lambda Variables
    * ----------------------------------------------------------------- *)

   local
     val lvar_bucket : lvar list ref = ref []
     fun add_to_bucket lvar = lvar_bucket := (lvar :: !lvar_bucket)
   in
     fun is_marked_lvar lvar = !(Lvars.is_free lvar)
     fun mark_lvar lvar = if is_marked_lvar lvar then ()
                          else (Lvars.is_free lvar := true;
                                add_to_bucket lvar)
     fun unmark_lvar lvar = Lvars.is_free lvar := false
     fun reset_lvar_bucket() = (map unmark_lvar (!lvar_bucket); lvar_bucket := [])
   end

   (* Functionality for avoiding inlining and specialisation of certain functions *)
   local
     fun suffix suf lv =
         let val s = Lvars.str lv
             val i = String.size s - String.size suf
         in i > 0 andalso
            let val ext = String.extract(s,i,NONE)
            in suf = ext
            end
         end
   in
   fun noinline_lvar lv = suffix "__noinline" lv
   fun inline_lvar lv = suffix "__inline" lv
   end

   (* -----------------------------------------------------------------
    * Specialization of recursive functions is performed during
    * contract. Here we give two functions; one that decides if a
    * function is specializable and one that specializes an occurrence
    * of a specializable function.
    * ----------------------------------------------------------------- *)

   fun specializable {lvar=lv_f, regvars=[], tyvars, Type=ARROWtype([tau_1'],_,[ARROWtype([tau_2'],_,_,_)],_),
                      constrs=[],
                      bind=FN{pat=[(lv_x,tau_1)],body=FN{pat=[(lv_y,tau_2)],body}}} =
       if noinline_lvar lv_f then false
       else
         let exception Fail
             fun app_f_x (APP(VAR{lvar=lv_f',...}, VAR{lvar=lv_x',...}, _)) =
                 if Lvars.eq(lv_f',lv_f) then if Lvars.eq(lv_x',lv_x) then () else raise Fail
                 else if Lvars.eq(lv_x',lv_f) then raise Fail else ()
               | app_f_x (VAR{lvar,...}) = if Lvars.eq(lvar,lv_f) then raise Fail else ()
               | app_f_x e = app_lamb app_f_x e
         in eq_Type(tau_1,tau_1') andalso eq_Type(tau_2,tau_2') andalso
            ((app_f_x body; true) handle Fail => false)
         end
     | specializable _ = false

   fun specializableN {lvar=lv_f, regvars=[], tyvars, Type=ARROWtype(taus,_,taus_res,_),
                       constrs=[],
                       bind=FN{pat,body}} =
       if noinline_lvar lv_f then NONE
       else
       let exception Fail
           fun look (n, nil) = NONE
             | look (n, (lv_x,tau_x)::pat) =
               let fun app_f_x (APP(VAR{lvar=lv_f',...}, arg as PRIM(UB_RECORDprim,es),_)) =
                       if Lvars.eq(lv_f',lv_f) then
                         (case List.nth(es,n) of
                              VAR{lvar=lv_x',...} =>
                              if Lvars.eq(lv_x',lv_x)
                              then app_lamb app_f_x arg
                              else raise Fail
                            | _ => raise Fail)
                       else app_lamb app_f_x arg
                     | app_f_x (VAR{lvar,...}) = if Lvars.eq(lvar,lv_f) then raise Fail else ()
                     | app_f_x e = app_lamb app_f_x e
               in case tau_x of
                      ARROWtype _ => ((app_f_x body; SOME n) handle Fail => look (n+1,pat))
                    | _ => look (n+1,pat)
               end
           val taus' = map #2 pat
       in if length taus <= 1 orelse not(eq_Types(taus,taus')) then NONE
          else look (0,pat)
       end
     | specializableN _ = NONE

   fun subst_lvar_for_app lv (e as APP(lv_e as VAR{lvar,...},_,_)) =
        if Lvars.eq(lvar,lv) then lv_e
        else map_lamb (subst_lvar_for_app lv) e
     | subst_lvar_for_app lv e = map_lamb (subst_lvar_for_app lv) e

   fun subst_e_for_lvar lv e (e' as VAR{lvar,...}) = if Lvars.eq(lvar,lv) then e else e'
     | subst_e_for_lvar lv e e' = map_lamb (subst_e_for_lvar lv e) e'

   fun specialize_bind {lvar=lv_f, tyvars, Type=ARROWtype([tau_1],_,[ARROWtype([tau_2],rv0,[tau_3],rv)],_),
                        bind=FN{pat=[(lv_x,_)],body=FN{pat=[(lv_y,_)],body}}}
                       instances lamb' =
     let val S = mk_subst (fn () => "specialize_bind") (tyvars, instances)
         val tau_2' = on_Type S tau_2
         val tau_1' = on_Type S tau_1
         val tau = ARROWtype([tau_2'],rv0,[on_Type S tau_3],rv)
         val body' = subst_lvar_for_app lv_f body
         val body'' = on_LambdaExp S body'
         val scope = FIX{functions=[{lvar=lv_f,regvars=[],tyvars=[],Type=tau,constrs=[],
                                     bind=FN{pat=[(lv_y,tau_2')],body=body''}}],
                         scope=VAR{lvar=lv_f,instances=[],regvars=[]}}
         val e_0 = LET{pat=[(lv_x,[],tau_1')],bind=lamb',scope=scope}
     in new_instance e_0
     end
     | specialize_bind _ _ _ = die "specialize_bind"

   fun pick 0 (t::ts) = (t,ts)
     | pick n (t::ts) =
       let val (t',ts) = pick (n-1) ts
       in (t',t::ts)
       end
     | pick _ nil = die "pick"

   fun pick2 n ts =
       let fun loop 0 (t::ts) pre = (rev pre,t,ts)
             | loop n (t::ts) pre = loop (n-1) ts (t::pre)
             | loop n _ _ = die "pick2"
       in loop n ts nil
       end

   fun ubargs nil = die "ubargs"
     | ubargs [arg] = arg
     | ubargs args = PRIM(UB_RECORDprim,args)

   fun elim_app_arg lv n (e as APP(lv_e as VAR{lvar,...},PRIM(UB_RECORDprim,args),tailpos)) =
       if Lvars.eq(lvar,lv) then
         let val (_,args) = pick n args
             val args = map (elim_app_arg lv n) args
         in APP(lv_e,ubargs args,tailpos)
         end
       else map_lamb (elim_app_arg lv n) e
     | elim_app_arg lv n e = map_lamb (elim_app_arg lv n) e

   fun replaceLv lv lv' e =
       let fun f e =
               case e of
                   VAR{lvar,instances,regvars} =>
                   if Lvars.eq(lvar,lv)
                   then VAR{lvar=lv',instances=instances,regvars=regvars}
                   else e
                 | LET{pat,bind,scope} =>
                   LET{pat=pat,
                       bind=f bind,
                       scope= if List.exists (fn (v,_,_) => Lvars.eq(v,lv)) pat
                              then scope
                              else f scope}
                 | FN{pat,body} =>
                   FN{pat=pat,
                      body=if List.exists (fn (v,_) => Lvars.eq(v,lv)) pat
                           then body
                           else f body}
                 | FIX{functions,scope} =>
                   if List.exists (fn {lvar,...} => Lvars.eq(lvar,lv)) functions
                   then e
                   else map_lamb f e
                 | e => map_lamb f e
       in f e
       end


   fun specializeN_bind {lvar=lv_f, tyvars, Type=ARROWtype(taus,rv0,taus_res,rv),
                         bind=FN{pat,body}}
                        n (tailpos:bool option) instances (PRIM(UB_RECORDprim,args)) =
     let val S = mk_subst (fn () => "specializeN_bind") (tyvars, instances)
         val taus = on_Types S taus
         val taus_res = on_Types S taus_res
         val (taus1',tau', taus2') = pick2 n taus
         val taus' = taus1' @ taus2'
         val tau = ARROWtype(taus',rv0,taus_res,rv)
         val pat = map (fn (lv,t) => (lv,on_Type S t)) pat
         val ((p_lv,p_t),pat') = pick n pat
         val body' = elim_app_arg lv_f n body
         val lv_f' = Lvars.renew lv_f
         val body'' = on_LambdaExp S body'
         val body''' = replaceLv lv_f lv_f' body''
         fun fixF x = FIX{functions=[{lvar=lv_f',regvars=[],tyvars=[],Type=tau,constrs=[],
                                      bind=FN{pat=pat',body=body'''}}],
                          scope=x}
         val (args1,arg,args2) = pick2 n args
         val e = if safeLambdaExp arg orelse safeLambdaExps args1 then
                   LET{pat=[(p_lv,nil,p_t)],bind=arg,
                       scope=fixF (APP(VAR{lvar=lv_f',instances=[],regvars=[]},
                                       ubargs (args1 @ args2),
                                       tailpos))
                      }
                 else
                   let val lvts = map (fn t => (Lvars.newLvar(),t)) taus1'
                       fun lets nil nil s = s
                         | lets ((lv,t)::tvts) (e::es) s =
                           LET{pat=[(lv,[],t)],bind=e,scope=lets tvts es s}
                         | lets _ _ _ = die "specializeN_bind.lets"
                       val args1' = map (fn (lv,_) => VAR{lvar=lv,instances=[],regvars=[]}) lvts
                   in lets lvts args1
                           (LET{pat=[(p_lv,nil,p_t)],bind=arg,
                                scope=fixF (APP(VAR{lvar=lv_f',instances=[],regvars=[]},
                                                ubargs (args1' @ args2),
                                                tailpos))
                           })
                   end
     in new_instance e
     end
     | specializeN_bind _ _ _ _ _ = die "specializeN_bind"

   val tag_values = Flags.is_on0 "tag_values"

   fun simple_nonexpanding e =
       case e of
           VAR{instances=[],regvars=[],...} => true
         | INTEGER (_,t) => if tag_values() then eq_Type(t,int31Type) orelse eq_Type(t,int63Type) else true
         | F64 _ => true
         | LET{pat,bind,scope} => simple_nonexpanding bind andalso simple_nonexpanding scope
         | PRIM(SELECTprim _, [e]) => simple_nonexpanding e
         | PRIM(CCALLprim{name,...},[e]) =>
           (case name of
                "__real_to_f64" => true
              | "__neg_f64" => true
              | "__abs_f64" => true
              | "__sqrt_f64" => true
              | "__int_to_f64" => true
              | _ => false) andalso simple_nonexpanding e
         | PRIM(CCALLprim{name,...},[e1,e2]) =>
           (case name of
                "__mul_f64" => true
              | "__minus_f64" => true
              | "__plus_f64" => true
              | "__div_f64" => true
              | "__max_f64" => true
              | "__min_f64" => true
              | "__blockf64_sub_f64" => true
              | "__less_f64" => true
              | _ => false) andalso simple_nonexpanding e1 andalso simple_nonexpanding e2
         | _ => false

   (* =================================================================
    * A better contract
    * ================================================================= *)

    local

      (* -----------------------------------------------------------------
       * Liveness of exception constructors
       * ----------------------------------------------------------------- *)

      local
        val excon_bucket : excon list ref = ref []
        fun add_excon_bucket excon = excon_bucket := (excon :: !excon_bucket)
      in
        fun reset_excon_bucket () = excon_bucket := []
        fun is_live_excon excon = is_in_ex excon (!excon_bucket)
        fun mk_live_excon excon = if is_live_excon excon then () else add_excon_bucket excon
      end


      (* -----------------------------------------------------------------
       * Compile time values
       * ----------------------------------------------------------------- *)

      datatype cv = CVAR of {exp:LambdaExp}
                  | CRECORD of cv list
                  | CUNKNOWN
                  | CCONST of {exp:LambdaExp}
                  | CFN of {lexp: LambdaExp, large:bool}                             (* only to appear in env *)
                  | CFIX of {N:int option, Type: Type, bind: LambdaExp, large: bool} (* only to appear in env *)
                  | CBLKSZ of IntInf.int                                             (* statically sized block (e.g., array or string) *)
                  | CBLK2SZ of IntInf.int option * IntInf.int option                 (* statically sized 2d-block *)
                  | CRNG of {low: IntInf.int option, high: IntInf.int option}
                  | CCON1 of con * cv
                  | CCON0 of con

      fun eq_cv (cv1,cv2) =
        case (cv1,cv2)
          of (CVAR {exp=e1},CVAR {exp=e2}) => eq_lamb(e1,e2)
           | (CRECORD cvs1, CRECORD cvs2) => eq_cvs(cvs1,cvs2)
           | (CUNKNOWN, CUNKNOWN) => true
           | (CCONST {exp=e1}, CCONST {exp=e2}) => eq_lamb(e1,e2)
           | (CFN{lexp,large}, CFN{lexp=lexp2,large=large2}) => large = large2 andalso eq_lamb(lexp,lexp2)
           | (CFIX{N,bind,large,Type}, CFIX{N=N2,bind=bind2,large=large2,Type=Type2}) =>
             N=N2 andalso large = large2 andalso eq_Type(Type,Type2) andalso eq_lamb(bind,bind2)
           | (CBLKSZ i1, CBLKSZ i2) => i1 = i2
           | (CBLK2SZ i1, CBLK2SZ i2) => i1 = i2
           | (CRNG i1, CRNG i2) => i1 = i2
           | (CCON1 (c1,cv1), CCON1 (c2,cv2)) =>
             Con.eq(c1,c2) andalso eq_cv(cv1,cv2)
           | (CCON0 c1, CCON0 c2) => Con.eq(c1,c2)
           | _ => false
      and eq_cvs (cv1::cvs1,cv2::cvs2) = eq_cv(cv1,cv2) andalso eq_cvs(cvs1,cvs2)
        | eq_cvs (nil,nil) = true
        | eq_cvs _ = false

      fun closed_small_cv (lvars_free_ok,excons_free_ok,lvar,tyvars,cv) : bool =
        case cv
          of CVAR {exp=e1} => closed (lvars_free_ok, excons_free_ok,
                                      FN{pat=[(lvar,unitType)],body=e1})
           | CRECORD cvs => (List.foldl (fn (cv,acc) => acc
                                         andalso closed_small_cv(lvars_free_ok, excons_free_ok,lvar,tyvars,cv))
                             true cvs)
           | CUNKNOWN => true
           | CCONST _ => true
           | CFN{lexp,large} => (not large andalso
                                 closed (lvars_free_ok, excons_free_ok,
                                         FN{pat=[(lvar,unitType)],body=lexp}))
           | CFIX{N,bind,Type,large} => (not large andalso
                                         closed (lvars_free_ok, excons_free_ok,
                                                 FIX{functions=[{lvar=lvar,
                                                                 regvars=[],       (* memo:regvars *)
                                                                 tyvars=tyvars,
                                                                 Type=Type,
                                                                 constrs=[],       (* memo:constrs *)
                                                                 bind=bind}],
                                                     scope=STRING("",NONE)}))
           | CBLKSZ _ => true
           | CBLK2SZ _ => true
           | CRNG _ => true
           | CCON1 (_,cv) => closed_small_cv(lvars_free_ok,excons_free_ok,lvar,tyvars,cv)
           | CCON0 _ => true

      (* remove lvar from compiletimevalue, if it is there;
       * used when compiletimevalues are exported out of scope.
       *)
      fun remove lvar (CRECORD l) = CRECORD(map (remove lvar) l)
        | remove lvar (cv as (CVAR {exp=VAR{lvar =lvar',...}})) = if Lvars.eq(lvar,lvar') then CUNKNOWN else cv
        | remove _ (cv as (CCONST _)) = cv
        | remove _ (cv as (CBLKSZ _)) = cv
        | remove _ (cv as (CBLK2SZ _)) = cv
        | remove _ (cv as (CRNG _)) = cv
        | remove lvar (CCON1 (c,cv)) = CCON1(c,remove lvar cv)
        | remove _ (cv as (CCON0 _)) = cv
        | remove _ _ = CUNKNOWN

      fun removes [] cv = cv
        | removes (lv::lvs) cv = removes lvs (remove lv cv)

      fun pp_opti NONE = "_"
        | pp_opti (SOME i) = IntInf.toString i

      (* pretty printing *)
      fun show_cv (CVAR {exp=VAR x}) = " cvar " ^ Lvars.pr_lvar (#lvar x)
        | show_cv (CVAR _) = "<not possible>"
        | show_cv (CRECORD l) = concat ("[" :: (map show_cv l @ ["]"]))
        | show_cv (CCONST _) = "const"
        | show_cv (CFN {large=true,...}) = "(large fn)"
        | show_cv (CFN {large=false,...}) = "(small fn)"
        | show_cv (CFIX {large=true,...}) = "(large fix)"
        | show_cv (CFIX {large=false,...}) = "(small fix)"
        | show_cv (CUNKNOWN) = "(unknown)"
        | show_cv (CBLKSZ i) = "(cblksz " ^ IntInf.toString i ^ ")"
        | show_cv (CBLK2SZ (i0opt,i1opt)) = "(cblk2sz " ^ pp_opti i0opt ^ ", " ^ pp_opti i1opt ^ ")"
        | show_cv (CRNG {low,high}) = "(crng " ^ pp_opti low ^ "--" ^ pp_opti high ^ ")"
        | show_cv (CCON1 (c,cv)) = Con.pr_con c ^ "(" ^ show_cv cv ^ ")"
        | show_cv (CCON0 c) = Con.pr_con c

      (* substitution *)
      fun on_cv S cv =
        let fun on (CVAR {exp=lamb}) = CVAR {exp=on_LambdaExp S lamb}
              | on (cv as CCONST _) = cv
              | on (CRECORD cvs) = CRECORD (map on cvs)
              | on (CFN{lexp,large}) = CFN{lexp=on_LambdaExp S lexp,large=large}
              | on (CFIX{N,Type,bind,large}) = CFIX{N=N,Type=on_Type S Type,bind=on_LambdaExp S bind,large=large}
              | on (cv as CBLKSZ _) = cv
              | on (cv as CBLK2SZ _) = cv
              | on (cv as CRNG _) = cv
              | on (CCON1(c,cv)) = CCON1(c,on cv)
              | on (cv as CCON0 c) = cv
              | on _ = CUNKNOWN
        in on cv
        end

      fun eq_cv_scheme ((tvs1,cv1),(tvs2,cv2)) =
        length tvs1 = length tvs2 andalso
        let val S = mk_subst (fn () => die "eq_cv_scheme") (tvs1, map (fn tv => TYVARtype {tv=tv}) tvs2)
        in eq_cv(on_cv S cv1,cv2)
        end

      (* least upper bound *)
      fun lub (cv as CVAR {exp=e1},CVAR {exp=e2}) =
          if eq_lamb(e1,e2) then cv else CUNKNOWN
        | lub (CRECORD cvals,CRECORD cvals') =
          (CRECORD (map lub (BasisCompat.ListPair.zipEq(cvals,cvals')))
           handle BasisCompat.ListPair.UnequalLengths => die "lub")
        | lub (cv as CCONST {exp=e1},CCONST {exp=e2}) =
          if eq_lamb(e1,e2) then cv else CUNKNOWN
        | lub (CRNG{low=l1,high=h1},CRNG{low=l2,high=h2}) =
          let fun minopt (NONE,_) = NONE
                | minopt (_,NONE) = NONE
                | minopt (SOME i1,SOME i2) = SOME(IntInf.min(i1,i2))
              fun maxopt (NONE,_) = NONE
                | maxopt (_,NONE) = NONE
                | maxopt (SOME i1,SOME i2) = SOME(IntInf.max(i1,i2))
          in CRNG{low=minopt(l1,l2),high=maxopt(h1,h2)}
          end
        | lub (cv as CBLKSZ i1, CBLKSZ i2) =
          if i1 = i2 then cv else CUNKNOWN
        | lub (cv as CBLK2SZ i1, CBLK2SZ i2) =
          if i1 = i2 then cv else CUNKNOWN
        | lub (cv as CCON1(c1,cv1), CCON1(c2,cv2)) =
          if Con.eq(c1,c2) then
            if eq_cv(cv1,cv2) then cv
            else CCON1(c1,lub(cv1,cv2))
          else CUNKNOWN
        | lub (cv as CCON0 c1, CCON0 c2) =
          if Con.eq(c1,c2) then cv else CUNKNOWN
        | lub _ = CUNKNOWN

      fun lubList [] = CUNKNOWN
        | lubList (l::ls) =
          List.foldl (fn (cval,cval') => lub(cval, cval')) l ls


      (* -----------------------------------------------------------------
       * Compile time environment
       * ----------------------------------------------------------------- *)

      type env = (tyvar list * cv) LvarMap.map

      fun updateEnv [] [] env = env
        | updateEnv (lv::lvs) (p::ps) env = updateEnv lvs ps (LvarMap.add(lv,p,env))
        | updateEnv _ _ _ = die "updateEnv"

      fun lookup_lvar (env, lvar) = LvarMap.lookup env lvar

      fun layout_cv cv =
          case cv of
              CFN{large=false,lexp} =>
                  PP.NODE{start="(small fn == ", finish=")",
                          indent=2,childsep=PP.NOSEP,
                          children=[layoutLambdaExp lexp]}
            | CFIX{N=NONE,large=false,Type,bind} =>
                  PP.NODE{start="(small fix: ", finish=")",
                          indent=2,childsep=PP.RIGHT " == ",
                          children=[layoutType Type, layoutLambdaExp bind]}
            | CFIX{N=SOME n,large=false,Type,bind} =>
                  PP.NODE{start="(small fix[" ^ Int.toString n ^ "]: ", finish=")",
                          indent=2,childsep=PP.RIGHT " == ",
                          children=[layoutType Type, layoutLambdaExp bind]}
            | _ => PP.LEAF (show_cv cv)

      fun layout_tyvars tvs =
          PP.NODE{start="{", finish="}",indent=0,
                  childsep=PP.RIGHT",",
                  children=map (PP.LEAF o pr_tyvar) tvs}

      fun layout_cv_scheme (tyvars,cv) =
          PP.NODE{start="[\\/", finish="]", indent=0,
                  childsep=PP.RIGHT ".",
                  children=[layout_tyvars tyvars,layout_cv cv]}

      val layout_contract_env : env -> StringTree =
        LvarMap.layoutMap {start="ContractEnv={",eq="->", sep=", ", finish="}"}
        (PP.LEAF o Lvars.pr_lvar') layout_cv_scheme

      fun pr_contract_env e =
          PP.outputTree (print, layout_contract_env e, 200)

      fun cross_module_inline (lvars_free_ok, excons_free_ok) lvar (tyvars,cv) =
        cross_module_opt()
        andalso closed_small_cv(lvars_free_ok,excons_free_ok,lvar,tyvars,cv)

      val frame_contract_env : env option ref = ref NONE

        (* SOME empty              : an exception is raised for sure (bot)
           NONE                    : we're not certain that an exception
                                     is raised (top)
           SOME {lv1:CRNG rng1,...
                 lvn:CRNG rngn}    : an exception is raised if either of
                                     lvi is outside rngi
         *)
      fun exn_anti_env (e:LambdaExp) : env option =
          let fun join_rng (r1,r2) =
                  case (r1,r2) of
                      (CRNG {low=l1,high=h1}, CRNG {low=l2,high=h2}) =>    (* (SOME 0,NONE) v (NONE,SOME 128) => (SOME 0,SOME 128) *)
                      (case (l1,h1,l2,h2) of
                           (NONE,SOME h,SOME l,NONE) => if l < h then CRNG{low=l2,high=h1} else r1
                         | (SOME l,NONE,NONE,SOME h) => if l < h then CRNG{low=l1,high=h2} else r1
                         | _ => r1)
                    | (CRNG _, _) => r1
                    | (_, CRNG _) => r2
                    | _ => CUNKNOWN
              fun join_rng' ((_,r1),(_,r2)) = ([],join_rng(r1,r2))
              fun join (e1:env option,e2:env option) : env option =
                  case (e1,e2) of
                      (NONE,_) => e2
                    | (_,NONE) => e1
                    | (SOME e1',SOME e2') => if LvarMap.isEmpty e1' then e1
                                             else if LvarMap.isEmpty e2' then e2
                                             else SOME(LvarMap.mergeMap join_rng' e1' e2')
              fun exn e : env option =
                  case e of
                      RAISE _ => SOME LvarMap.empty
                    | PRIM (_,lambs) => List.foldl join NONE (map exn lambs)
                    | LET {pat,bind,scope} => join (exn bind, exn scope)
                    | APP (e1,e2,_) => join (exn e1, exn e2)
                    | FIX {functions,scope} => exn scope
                    | LETREGION {regvars,scope} => exn scope
                    | EXCEPTION (_,_,scope) => exn scope
                    | SWITCH_C (SWITCH(e,[((con,_),e_t)],SOME e_f)) =>
                      if not(Con.eq(con,Con.con_TRUE)) then NONE
                      else
                      let fun certain_gte lv i = SOME(LvarMap.singleton(lv,([],CRNG{low=SOME i,high=NONE})))
                          fun certain_lt lv i = SOME(LvarMap.singleton(lv,([],CRNG{low=NONE,high=SOME(i-1)})))
                          fun comp_env_f e =
                              case e of
                                  PRIM(CCALLprim{name="__less_int64ub",...},[VAR {lvar,...},INTEGER(i,_)]) => certain_gte lvar i
                                | PRIM(CCALLprim{name="__greatereq_int64ub",...},[VAR {lvar,...},INTEGER(i,_)]) => certain_lt lvar i
                                | SWITCH_C (SWITCH(e,[((con',_),PRIM(CONprim{con=con'',...},[]))],SOME e_f')) =>
                                  if Con.eq(con',Con.con_TRUE) andalso Con.eq(con'',Con.con_TRUE)
                                  then join (comp_env_f e, comp_env_f e_f')
                                  else NONE
                                | _ => NONE
(*
                          fun comp_env_t e =
                              case e of
                                  PRIM(CCALLprim{name="__less_int64ub",...},[VAR {lvar,...},INTEGER(i,_)]) => certain_lt lvar i
                                | _ => NONE
*)
                      in case (exn e_t, exn e_f) of
                             (NONE,NONE) => exn e
                           | (SOME env_t,NONE) => if LvarMap.isEmpty env_t then comp_env_f e
                                                  else NONE
(*
                           | (NONE,SOME env_f) => if LvarMap.isEmpty env_f then comp_env_t e
                                                  else NONE
*)
                           | _ => NONE
                      end
                    | SWITCH_C (SWITCH(e,_,_)) => exn e
                    | SWITCH_I {switch=SWITCH(e,_,_),...} => exn e
                    | SWITCH_W {switch=SWITCH(e,_,_),...} => exn e
                    | SWITCH_S (SWITCH(e,_,_)) => exn e
                    | SWITCH_E (SWITCH(e,_,_)) => exn e
                    | TYPED(e,_,_) => exn e
                    | FRAME _ => NONE
                    | VAR _ => NONE
                    | INTEGER _ => NONE
                    | WORD _ => NONE
                    | REAL _ => NONE
                    | F64 _ => NONE
                    | STRING _ => NONE
                    | FN _ => NONE
                    | HANDLE _ => NONE
          in exn e
          end

      (* -----------------------------------------------------------------
       * Usage counts
       * ----------------------------------------------------------------- *)

      (* Recursive occurrences of fix-bound variables are not uses!!!
       * This we model by marking fix-bound variables prior to the
       * recursive descent and then unmarking them prior to the
       * traversal of the scope of the FIX. Then, we do not
       * increase/decrease usage counts for variables that are
       * marked. *)

      fun incr_use lv = if is_marked_lvar lv then ()
                        else Lvars.incr_use lv

      fun decr_use lv = if is_marked_lvar lv then ()
                        else Lvars.decr_use lv

      fun zero_uses [] = true                                              (* Check if a list of variables all have zero uses. *)
        | zero_uses (lv::lvs) = Lvars.zero_use lv andalso zero_uses lvs

      fun incr_uses (VAR{lvar,...}) = incr_use lvar                        (* Increase uses in an expression. *)
        | incr_uses lamb = app_lamb incr_uses lamb

      fun decr_uses (VAR{lvar,...}) = decr_use lvar                        (* Decrease uses in an expression. *)
        | decr_uses lamb = app_lamb decr_uses lamb

      (* -----------------------------------------------------------------
       * Initialization of usage counts
       * ----------------------------------------------------------------- *)

      fun init e =
        case e
          of VAR{lvar,instances,regvars} => incr_use lvar
           | LET{pat,bind,scope} => (app (Lvars.reset_use o #1) pat; init bind; init scope)
           | FN{pat,body} => (app (Lvars.reset_use o #1) pat; init body)
           | FIX{functions,scope} => (app (Lvars.reset_use o #lvar) functions;
                                      app (mark_lvar o #lvar) functions;
                                      app (init o #bind) functions;
                                      app (unmark_lvar o # lvar) functions;
                                      init scope)
           | FRAME {declared_lvars,...} => app (fn {lvar,...} => (incr_use lvar; incr_use lvar)) declared_lvars
           | _ => app_lamb init e

      fun is_inlinable_fn lvar lamb =
          case lamb of
              FN _ => inline_lvar lvar orelse always_inline_function lvar orelse small_lamb (max_inline_size()) lamb
            | _ => false

      fun is_fn (FN _) = true
        | is_fn (LET{pat,bind,scope}) = is_fn bind andalso is_fn scope
        | is_fn (FIX{functions,scope}) = is_fn scope
        | is_fn _ = false

      (* inlining of unsafe bindings in safe contexts *)
      fun simpleContext lv e =
          if aggressive_opt() andalso Lvars.one_use lv then
            let fun build e =
                    case e of
                      VAR{lvar,instances,regvars} =>
                      if Lvars.eq(lv,lvar) then SOME(fn x => x, instances)
                      else NONE
                    | PRIM(p,args) =>
                      (case buildargs args of
                         SOME(f,instances) => SOME(fn x => PRIM(p,f x), instances)
                       | NONE => NONE)
                    | _ => NONE
              and buildargs args =
                  case args of
                    [] => NONE   (* hmmm - shouldn't happen *)
                  | arg::args =>
                    (case build arg of
                       SOME(f,instances) => SOME(fn x => f x :: args, instances)
                     | NONE =>
                       if safeLambdaExp arg then
                         (case buildargs args of
                            SOME (f,instances) => SOME(fn x => arg :: f x,instances)
                          | NONE => NONE)
                       else NONE)

            in build e
            end
          else NONE

      (* -----------------------------------------------------------------
       * Reduce on switch
       * ----------------------------------------------------------------- *)

      fun selectorCon env (e : LambdaExp) : ((con*lvar option)->bool)option =
          let fun selC con = SOME (fn (c,_) => Con.eq(c,con))
          in if aggressive_opt() then
               case e of
                   PRIM(CONprim {con,...}, args) =>
                   if List.all safeLambdaExp args then selC con
                   else NONE
                 | VAR {lvar,...} =>
                   (case lookup_lvar(env,lvar) of
                        SOME (_,CCON1 (con,_)) => selC con
                      | SOME (_,CCON0 con) => selC con
                      | _ => NONE)
                 | PRIM(SELECTprim {index}, [VAR{lvar,...}]) =>
                   (case lookup_lvar(env,lvar) of
                        SOME (_,CRECORD cvs) =>
                        (case (SOME(List.nth(cvs,index)) handle _ => NONE) of
                             SOME (CCON1 (con,_)) => selC con
                           | SOME (CCON0 con) => selC con
                           | _ => NONE)
                      | _ => NONE)
                 | _ => NONE
             else NONE
          end

      fun selectorNONE e = NONE

      fun searchSel (eq:'a->bool) (sels:('a*LambdaExp)list) (opt:LambdaExp option) : LambdaExp * LambdaExp list =
          let
            fun addOpt NONE es = es
              | addOpt (SOME e) es = e::es
            fun loop (nil, ps) =
                (case opt of
                   SOME e => (e,ps)
                 | NONE => die "searchSel.impossible")
              | loop ((c,e)::sels, es) =
                if eq c then
                  (e, addOpt opt (map #2 sels @ es))
                else loop(sels,e::es)
          in loop (sels,nil)
          end

      fun reduce_switch (reduce, env, (fail as (_,cv)), (SWITCH(arg, sel, opt)), selector) =
        let fun allEqual [] = true   (* If branches are equal and the selector *)
              | allEqual [x] = true  (* is safe then eliminate switch. *)
              | allEqual (x::(ys as y::_)) = eq_lamb(x,y) andalso allEqual ys
          fun constFold () =
              case selector arg of
                SOME sel_eq =>
                let val (e, others) = searchSel sel_eq sel opt
                in tick "reduce - switch constant fold";
                   app decr_uses others;
                   reduce (env, (e, cv))
                end
              | NONE => fail
        in case opt of
               SOME lamb =>
               if safeLambdaExp arg andalso allEqual (lamb::(map snd sel)) then
                 (tick "reduce - switch"; decr_uses arg; app (decr_uses o snd) sel; reduce (env, (lamb, cv)))
               else constFold()
             | NONE =>
               if safeLambdaExp arg andalso allEqual (map snd sel) then
                 case sel of
                     (_,lamb)::sel' => (tick "reduce - switch"; decr_uses arg;
                                        app (decr_uses o snd) sel'; reduce (env, (lamb, cv)))
                   | _ => die "reduce_switch"
               else constFold()
        end


      (* -----------------------------------------------------------------
       * Reduce
       * ----------------------------------------------------------------- *)

      fun single_arg_fn (FN{pat=[_],body}) = true
        | single_arg_fn _ = false

      fun is_boolean con =
        Con.eq(Con.con_TRUE, con) orelse Con.eq(Con.con_FALSE, con)

      fun is_unboxed_value lamb =
          case lamb of
              INTEGER (_,t) => if tag_values() then (eq_Type(t, int31Type) orelse
                                                     eq_Type(t, int63Type))
                               else true
            | WORD (_,t) => if tag_values() then (not(eq_Type(t, word32Type)) andalso
                                                  not(eq_Type(t, word64Type)))
                            else true
            | F64 _ => true
            | PRIM(CONprim {con,...},nil) => is_boolean con
            | _ => false

      fun constfold_f64 () = false

      fun constantFolding (env:env) lamb fail =
          let val opt =
                  if not(aggressive_opt()) then NONE
                  else
                    case lamb of
                        PRIM(CCALLprim{name,...},exps) =>
                        (case exps of
                             [STRING (s1,NONE),STRING (s2,NONE)] =>
                             let fun opp opr = SOME(if opr(s1,s2) then lexp_true else lexp_false)
                             in case name of
                                    "concatStringML" => SOME(STRING (s1 ^ s2, NONE))
                                  | "lessStringML" => opp (op <)
                                  | "greaterStringML" => opp (op >)
                                  | "lesseqStringML" => opp (op <=)
                                  | "greatereqStringML" => opp (op >=)
                                  | _ => NONE
                             end
                           | [INTEGER(v1,t),INTEGER(v2,_)] =>
                             let fun opp opr = SOME(if opr(v1,v2) then lexp_true else lexp_false)
                                 fun opp_ov opr = let val v = opr (v1,v2)
                                                  in if ((Int31.fromLarge v; true) handle _ => false)
                                                     then SOME (INTEGER(v,t))
                                                     else NONE
                                                  end
                             in case name of
                                    "__less_int31" => opp (op <)
                                  | "__less_int32b" => opp (op <)
                                  | "__less_int32ub" => opp (op <)
                                  | "__less_int63" => opp (op <)
                                  | "__less_int64b" => opp (op <)
                                  | "__less_int64ub" => opp (op <)
                                  | "__lesseq_int31" => opp (op <=)
                                  | "__lesseq_int32b" => opp (op <=)
                                  | "__lesseq_int32ub" => opp (op <=)
                                  | "__lesseq_int63" => opp (op <=)
                                  | "__lesseq_int64b" => opp (op <=)
                                  | "__lesseq_int64ub" => opp (op <=)
                                  | "__greater_int31" => opp (op >)
                                  | "__greater_int32b" => opp (op >)
                                  | "__greater_int32ub" => opp (op >)
                                  | "__greater_int63" => opp (op >)
                                  | "__greater_int64b" => opp (op >)
                                  | "__greater_int64ub" => opp (op >)
                                  | "__greatereq_int31" => opp (op >=)
                                  | "__greatereq_int32b" => opp (op >=)
                                  | "__greatereq_int32ub" => opp (op >=)
                                  | "__greatereq_int63" => opp (op >=)
                                  | "__greatereq_int64b" => opp (op >=)
                                  | "__greatereq_int64ub" => opp (op >=)
                                  | "__equal_int31" => opp (op =)
                                  | "__equal_int32b" => opp (op =)
                                  | "__equal_int32ub" => opp (op =)
                                  | "__equal_int63" => opp (op =)
                                  | "__equal_int64b" => opp (op =)
                                  | "__equal_int64ub" => opp (op =)
                                  | "__plus_int63" => opp_ov (op +)
                                  | "__plus_int64ub" => opp_ov (op +)
                                  | "__minus_int63" => opp_ov (op -)
                                  | "__minus_int64ub" => opp_ov (op -)
                                  | "__mul_int63" => opp_ov (op * )
                                  | "__mul_int64ub" => opp_ov (op * )
                                  | _ => NONE
                             end
                           | [INTEGER(v1,t),INTEGER(v2,_),_] =>
                             let fun opp_ov opr = let val v = opr (v1,v2)
                                                  in if ((Int31.fromLarge v; true) handle _ => false)
                                                     then SOME (INTEGER(v,t))
                                                     else NONE
                                                  end handle Div => NONE
                             in case name of
                                    "__div_int63" => opp_ov (op div)
                                  | "__div_int64ub" => opp_ov (op div)
                                  | _ => NONE
                             end
                           | [WORD(v1,t),WORD(v2,_)] =>
                             let fun opp opr = SOME(if opr(v1,v2) then lexp_true else lexp_false)
                             in case name of
                                    "__less_word31" => opp (op <)
                                  | "__less_word32b" => opp (op <)
                                  | "__less_word32ub" => opp (op <)
                                  | "__less_word63" => opp (op <)
                                  | "__less_word64b" => opp (op <)
                                  | "__less_word64ub" => opp (op <)
                                  | "__lesseq_word31" => opp (op <=)
                                  | "__lesseq_word32b" => opp (op <=)
                                  | "__lesseq_word32ub" => opp (op <=)
                                  | "__lesseq_word63" => opp (op <=)
                                  | "__lesseq_word64b" => opp (op <=)
                                  | "__lesseq_word64ub" => opp (op <=)
                                  | "__greater_word31" => opp (op >)
                                  | "__greater_word32b" => opp (op >)
                                  | "__greater_word32ub" => opp (op >)
                                  | "__greater_word63" => opp (op >)
                                  | "__greater_word64b" => opp (op >)
                                  | "__greater_word64ub" => opp (op >)
                                  | "__greatereq_word31" => opp (op >=)
                                  | "__greatereq_word32b" => opp (op >=)
                                  | "__greatereq_word32ub" => opp (op >=)
                                  | "__greatereq_word63" => opp (op >=)
                                  | "__greatereq_word64b" => opp (op >=)
                                  | "__greatereq_word64ub" => opp (op >=)
                                  | "__equal_word" => opp (op =)
                                  | "__equal_word31" => opp (op =)
                                  | "__equal_word32b" => opp (op =)
                                  | "__equal_word32ub" => opp (op =)
                                  | "__equal_word63" => opp (op =)
                                  | "__equal_word64b" => opp (op =)
                                  | "__equal_word64ub" => opp (op =)
                                  | "__andb_word" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word31" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word32b" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word32ub" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word63" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word64b" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__andb_word64ub" => SOME(WORD(IntInf.andb(v1,v2),t))
                                  | "__orb_word" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word31" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word32b" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word32ub" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word63" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word64b" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | "__orb_word64ub" => SOME(WORD(IntInf.orb(v1,v2),t))
                                  | _ => NONE
                             end
                           | [F64 s1,F64 s2] =>
                             if not(constfold_f64()) then NONE
                             else
                             let fun oppc opr =
                                     case (Real.fromString s1, Real.fromString s2) of
                                         (SOME r1, SOME r2) => SOME(if opr(r1,r2) then lexp_true else lexp_false)
                                       | _ => NONE
                                 fun opp opr =
                                     case (Real.fromString s1, Real.fromString s2) of
                                         (SOME r1, SOME r2) =>
                                         SOME(F64(Real.fmt (StringCvt.FIX(SOME 10)) (opr(r1,r2))))
                                       | _ => NONE
                             in case name of
                                    "__minus_f64" => opp Real.-
                                  | "__plus_f64" => opp Real.+
                                  | "__mul_f64" => opp Real.*
                                  | "__div_f64" => opp Real./
                                  | "__less_f64" => oppc Real.<
                                  | "__greater_f64" => oppc Real.>
                                  | "__lesseq_f64" => oppc Real.<=
                                  | "__greatereq_f64" => oppc Real.>=
                                  | _ => NONE
                             end
                           | [F64 s] =>
                             if not(constfold_f64()) then NONE
                             else
                             let fun opp opr =
                                     case Real.fromString s of
                                         SOME r => SOME(F64(Real.fmt (StringCvt.EXACT) (opr r)))
                                       | _ => NONE
                             in case name of
                                    "__neg_f64" => opp Real.~
                                  | "__abs_f64" => opp Real.abs
                                  | "__sqrt_f64" => opp Math.sqrt
                                  | _ => NONE
                             end
                           | [STRING(s,NONE)] =>
                             let
                             in case name of
                                    "__bytetable_size" =>
                                    SOME(INTEGER(Int.toLarge (String.size s), intDefaultType()))
                                  | _ => NONE
                             end
                           | [STRING(s,NONE),INTEGER(v,t)] =>
                             let
                             in case name of
                                    "__bytetable_sub" =>
                                    (let val c = String.sub(s,Int.fromLarge v)
                                     in SOME(WORD(Int.toLarge (Char.ord c), charType))
                                     end handle _ => NONE)
                                  | _ => NONE
                             end
                           | _ => NONE)
                      | _ => NONE
          in case opt of
                 SOME e => (tick "constant-folding"; (e,CCONST {exp=e}))
               | NONE =>
                 let datatype cmp = LT | LTE | GT | GTE
                     fun Not LT = GTE
                       | Not LTE = GT
                       | Not GT = LTE
                       | Not GTE = LT
                     fun rightlift LT i {low=SOME j,high=NONE} = i < j
                       | rightlift LTE i {low=SOME j,high=NONE} = i <= j
                       | rightlift GT i {low=NONE,high=SOME j} = i > j
                       | rightlift GTE i {low=NONE,high=SOME j} = i >= j
                       | rightlift _ _ _ = false
                     fun try (opr:cmp) xs =
                         case xs of
                             [INTEGER(i,_),VAR{lvar,...}] =>
                             (case lookup_lvar(env,lvar) of
                                  SOME(_,CRNG rng) =>
                                  if rightlift opr i rng then SOME lexp_true
                                  else if rightlift (Not opr) i rng then SOME lexp_false
                                  else NONE
                                | _ => NONE)
                           | [VAR{lvar,...},INTEGER(i,_)] =>
                             (case lookup_lvar(env,lvar) of
                                  SOME(_,CRNG rng) =>
                                  if rightlift (Not opr) i rng then SOME lexp_true
                                  else if rightlift opr i rng then SOME lexp_false
                                  else NONE
                                | _ => NONE)
                           | _ => NONE (* memo: more cases! *)
                     val opt2 =
                         case lamb of
                             PRIM(CCALLprim{name="__less_int64ub",...},xs) => try LT xs
                           | PRIM(CCALLprim{name="__less_int63",...},xs) => try LT xs
                           | PRIM(CCALLprim{name="__lesseq_int64ub",...},xs) => try LTE xs
                           | PRIM(CCALLprim{name="__lesseq_int63",...},xs) => try LTE xs
                           | PRIM(CCALLprim{name="__greater_int64ub",...},xs) => try GT xs
                           | PRIM(CCALLprim{name="__greater_int63",...},xs) => try GT xs
                           | PRIM(CCALLprim{name="__greatereq_int64ub",...},xs) => try GTE xs
                           | PRIM(CCALLprim{name="__greatereq_int63",...},xs) => try GTE xs
                           | _ => NONE
                 in case opt2 of
                        SOME e => (tick "range-folding"; (e,CCONST {exp=e}))
                      | NONE => fail
                 end
          end

      fun reduce_f64bin f64binop (e1,e2) =
          (tick "real_to_f64";
           (f64_to_real (f64binop (real_to_f64 e1, real_to_f64 e2)), CUNKNOWN))

      fun reduce_f64cmp f64cmp (e1,e2) =
          (tick "real_to_f64";
           (f64cmp (real_to_f64 e1, real_to_f64 e2), CUNKNOWN))

      fun reduce_f64uno f64unop x =
          (tick "real_to_f64";
           (f64_to_real (f64unop (real_to_f64 x)), CUNKNOWN))

      fun reduce (env, (fail as (lamb,cv))) =
          case lamb of VAR{lvar,instances,regvars=[]} =>
            ( (*output(!Flags.log, Lvars.pr_lvar lvar ^ ":" );*)
             case lookup_lvar(env,lvar)
               of SOME (tyvars,cv) =>
                  (case cv
                    of CFN {lexp=lamb',large} =>
                      if large andalso not(Lvars.one_use lvar) then (lamb, CVAR {exp=lamb})
                      else let val S = mk_subst (fn () => "reduce1") (tyvars, instances)
                               val _ = decr_use lvar
                               val lamb'' = new_instance lamb'
                               val _ = incr_uses lamb''
                               val _ = if large then tick "reduce - inline-largefn"
                                       else tick "reduce - inline-smallfn"
                           in (on_LambdaExp S lamb'', CVAR {exp=lamb})    (* reduce(env,...) *)
                           end
                     | CVAR {exp=lamb' as VAR{lvar=lvar',instances=instances',regvars=[]}} =>
                           let val S = mk_subst (fn () => "reduce2") (tyvars,instances)
                               val _ = decr_use lvar
                               val _ = incr_use lvar'
                               val lamb'' = on_LambdaExp S lamb'
                           in if Lvars.eq(lvar,lvar') then (lamb'', CVAR {exp=lamb''})
                              else (tick "reduce - inline-var"; (lamb'', CVAR {exp=lamb''})) (*reduce (env, (lamb'', CVAR lamb''))*)
                           end
                     | CCONST {exp=lamb'} =>
                           if is_unboxed_value lamb' orelse (aggressive_opt() andalso small_const lamb') then
                             (decr_use lvar; tick "reduce - inline-unboxed-value"; (lamb', cv))
                           else if Lvars.one_use lvar then
                             (decr_use lvar; tick "reduce - inline-const"; (lamb', cv))
                           else (lamb, CVAR {exp=lamb})
                     | CUNKNOWN => (lamb, CVAR {exp=lamb})
                     | _ => let val S = mk_subst (fn () => "reduce3") (tyvars,instances)
                            in (lamb, on_cv S cv)
                            end)
                | NONE => ((*output(!Flags.log, "none\n");*) (lamb, CVAR {exp=lamb})))
           | VAR _ => fail (* explicit region parameters *)
           | INTEGER _ => (lamb, CCONST {exp=lamb})
           | WORD _ => (lamb, CCONST {exp=lamb})
           | PRIM(CONprim {con,...},[]) => if is_boolean con orelse aggressive_opt() then (lamb, CCONST {exp=lamb})
                                           else fail
           | STRING _ => (lamb, CCONST {exp=lamb})
           | REAL _ => (lamb, CCONST {exp=lamb})
           | F64 _ => (lamb, CCONST {exp=lamb})
           | LET{pat=[(lvar,tyvars,tau)],bind,scope} =>
               let
                 (* maybe let-float f64-binding outwards to open up for other optimisations *)
                 fun default () = (lvar,tyvars,tau,bind,scope,fail)
                 fun hoist () =
                     case bind of
                         LET{pat=[(lv,[],tau')],bind=bind',scope=scope'} =>
                         if unbox_reals() (*andalso eq_Type(tau',f64Type)*) andalso simple_nonexpanding bind' then
                           (tick "reduce - let-floating";
                            let val scope'' = LET{pat=[(lvar,[],tau)],bind=scope',
                                                  scope=scope}
                            in (lv,[],tau',bind',scope'',
                                (LET{pat=[(lv,[],tau')],bind=bind',scope=scope''},
                                 CUNKNOWN))
                            end)
                         else default()
                       | _ => default()
                 val (lvar,tyvars,tau,bind,scope,fail) =
                     case (tyvars,tau) of
                         ([],RECORDtype (_ :: _ :: _,_)) => hoist()
                       | ([],CONStype([],tn,_)) => if TyName.eq(tn,TyName.tyName_STRING) orelse
                                                      TyName.eq(tn,TyName.tyName_CHARARRAY)
                                                   then hoist()
                                                   else default()
                       | _ => default()
                 (* maybe unbox real binding *)
                 val (tau,bind,scope,fail) =
                     if unbox_reals() andalso eq_Type(realType,tau) andalso real_lvar_f64_in_lamb lvar scope then
                       (tick "reduce - unbox_real";
                        let val (tau,bind,scope) = (f64Type,real_to_f64 bind,
                                                    subst_real_lvar_f64_in_lamb lvar scope)
                            val () = Lvars.set_ubf64 lvar
                        in (tau,bind,scope,(LET{pat=[(lvar,tyvars,tau)],bind=bind,scope=scope},
                                            CUNKNOWN))
                        end)
                     else (tau,bind,scope,fail)
                 fun do_sw SW (SWITCH(VAR{lvar=lvar',instances,regvars=[]},sel,opt_e)) =
                     if Lvars.eq(lvar,lvar') andalso Lvars.one_use lvar then
                       let val S = mk_subst (fn () => "let-switch") (tyvars, instances)
                       in tick "reduce - inline-switch";
                         (SW (SWITCH(on_LambdaExp S bind, sel, opt_e)), cv)
                       end
                     else fail
                     | do_sw _ _ = fail
               in if Lvars.zero_use lvar then
                    if safeLambdaExp bind then
                      (decr_uses bind; tick "reduce - dead-let"; reduce (env, (scope, cv)))
                    else (*case scope
                           of PRIM(RECORDprim,[]) => fail
                            | _ => if eq_Type(tau,unit_Type) then fail
                                   else let val pat'=[(Lvars.new_named_lvar "_not_used",[],unit_Type)]
                                            val bind' = LET{pat=pat,bind=bind,scope=PRIM(RECORDprim, [])}
                                            val e = LET{pat=pat',bind=bind',scope=scope}
                                        in tick "reduce - dead-type"; (e,cv)
                                        end*)
                      let val e = LET{pat=nil,bind=PRIM(DROPprim,[bind]),scope=scope}
                      in tick "reduce - wild"; (e,cv)
                      end
                  else case scope
                         of VAR{lvar=lvar',instances,regvars=[]} =>
                           if Lvars.eq(lvar,lvar') then   (* no need for decr_uses *)
                             let val S = mk_subst (fn () => "reduce.LET") (tyvars, instances)
                             in tick "reduce - let-var"; reduce (env, (on_LambdaExp S bind, cv))
                             end
                           else fail
                           | PRIM(CONprim {con,instances,regvar}, [VAR{lvar=lvar',instances=nil,regvars=[]}]) =>
                             if Lvars.eq(lvar,lvar')
                               andalso Con.eq(Con.con_CONS, con) then   (* no need for decr_uses *)
                               let val e = PRIM(CONprim {con=con,instances=instances,regvar=regvar}, [bind])
                               in tick "reduce - let-var-cons"; reduce (env, (e, cv))
                               end
                             else fail
                          | SWITCH_I {switch,precision} =>
                               do_sw (fn sw => SWITCH_I {switch=sw,precision=precision}) switch
                          | SWITCH_W {switch,precision,tyname} =>
                               do_sw (fn sw => SWITCH_W {switch=sw,precision=precision,tyname=tyname}) switch
                          | SWITCH_S sw => do_sw SWITCH_S sw
                          | SWITCH_C sw => do_sw SWITCH_C sw
                          | SWITCH_E sw => do_sw SWITCH_E sw
                          | _ => (case simpleContext lvar scope of
                                    SOME(f,instances) =>
                                    (* no need for decr_uses *)
                                    let val S = mk_subst (fn () => "reduce.LET2") (tyvars, instances)
                                    in tick "reduce - let-simpleContext"; reduce (env, (f(on_LambdaExp S bind), cv))
                                    end
                                  | NONE => fail)
               end
           | LET{pat=nil,bind,scope} =>
               if safeLambdaExp bind then
                 (decr_uses bind; tick "reduce - dead-let"; reduce (env, (scope, cv)))
               else fail
           | LET{pat,bind,scope} =>
                   (case bind of
                        PRIM(UB_RECORDprim, es) =>
                            if length pat <> length es then
                                die "LET.bind"
                            else
                                let val e : LambdaExp = List.foldl
                                      (fn ((p,e),scope) => LET{pat=[p],bind=e,scope=scope})
                                      scope (ListPair.zip(pat,es))
                                in  tick "reduce - let-split"
                                  ; reduce (env, (e,cv))
                                end
                      | _ => fail)
          | PRIM(SELECTprim {index=n},[lamb]) =>
               let fun do_select () = fail
                      (* case cv
                        of CRECORD cvs =>
                          let val nth_cv = List.nth(cvs,n)
                            handle Subscript => die "reduce4"
                          in case nth_cv
                               of CVAR {exp=var} => (tick "reduce - sel-var"; decr_uses lamb;
                                                     incr_uses var; reduce (env, (var,nth_cv)))
                                | CCONST {exp=e as INTEGER _} => (tick "reduce - sel-int";
                                                                  decr_uses lamb; (e, nth_cv))
                                | CCONST {exp=e as WORD _} => (tick "reduce - sel-word";
                                                               decr_uses lamb; (e, nth_cv))
                                | _ => (lamb, nth_cv)
                          end
                         | _ => fail *)
               in case lamb
                    of PRIM(RECORDprim _,lambs) =>
                      let val (lamb', lambs') = case removeNth n lambs of
                                                    SOME p => p
                                                  | NONE => die "reduce.impossible"
                      in if safeLambdaExps lambs' then
                           (tick "reduce - sel-record"; app decr_uses lambs';
                            reduce(env, (lamb', CUNKNOWN)))
                         else do_select()
                      end
                     | _ => do_select()
               end
          | PRIM(DECONprim {con,...}, [PRIM(CONprim{con=con',...},[e])]) =>
            if Con.eq(con,con') then (tick "reduce - decon-con"; reduce (env, (e,cv)))
            else constantFolding env lamb fail
          | FIX{functions,scope} =>
               let val lvs = map #lvar functions
               in if zero_uses lvs then (tick "reduce - dead-fix";
                                         app (decr_uses o #bind) functions;
                                         reduce (env, (scope,cv)))
                  else case functions
                         of [function as {lvar,regvars=[],tyvars,Type,constrs,bind}] =>
                           if single_arg_fn bind andalso not(lvar_in_lamb lvar bind) then
                             ((*tick "reduce - fix-let";*)
                              reduce (env, (LET{pat=[(lvar,tyvars,Type)],
                                                bind=bind,scope=scope},cv)))
                           else fail
                          | _ => fail
               end
(*mael
          | APP(LET{pat,bind,scope=FN{pat=pat',body}},arg) =>
               let val pat' = fn_to_let_pat pat'
               in tick "appletfn-fn";
                 reduce (env, (LET{pat=pat,bind=bind,
                                   scope=LET{pat=pat',bind=arg,scope=body}}, CUNKNOWN))
               end
*)
          | APP(FN{pat,body=scope},bind,_) =>
               let val pat' = fn_to_let_pat pat
               in tick "appfn-let"; reduce (env, (LET{pat=pat',bind=bind,scope=scope}, CUNKNOWN))
               end
          | APP(VAR{lvar,instances,regvars=[]}, lamb2, tailpos) =>
               (case lookup_lvar(env, lvar) of
                    SOME (tyvars, CFIX{N=NONE,Type,bind,large}) =>
                    if not(large) orelse Lvars.one_use lvar then
                      let val e = specialize_bind {lvar=lvar,tyvars=tyvars,Type=Type,bind=bind} instances lamb2
                      in decr_use lvar; decr_uses lamb2; incr_uses e;
                         tick ("reduce - fix-spec." ^ Lvars.pr_lvar lvar);
                         reduce (env, (e, CUNKNOWN))
                      end
                    else fail
                  | SOME (tyvars,CFIX{N=SOME n,Type,bind,large}) =>
                    if not(large) orelse Lvars.one_use lvar then
                      let val () = case lamb2 of PRIM(UB_RECORDprim,_) => ()
                                               | _ => die ("specializeN_bind.assumption:UB_RECORDprim: " ^ Lvars.pr_lvar lvar)
                          val e = specializeN_bind {lvar=lvar,tyvars=tyvars,Type=Type,bind=bind} n tailpos instances lamb2
                      in decr_use lvar; decr_uses lamb2; incr_uses e; tick ("reduce - fix-specN." ^ Lvars.pr_lvar lvar);
                         reduce (env, (e, CUNKNOWN))
                      end
                    else fail
                  | _ => fail)
          | APP(FIX{functions=functions as [{lvar,...}], scope=f as VAR{lvar=lv_f,...}}, e, _) =>
              if Lvars.eq(lvar,lv_f) then
                (tick "reduce - app-fix"; (FIX{functions=functions,scope=APP(f,e,NONE)}, CUNKNOWN))
              else fail
          | APP(exp1, exp2, _) =>
                let exception NoBetaReduction
                    fun seekFN (LET{pat,bind,scope}, f) = seekFN(scope, f o (fn sc => LET{pat=pat,bind=bind,scope=sc}))
                      | seekFN (FIX{functions,scope}, f) = seekFN(scope, f o (fn sc => FIX{functions=functions,scope=sc}))
                      | seekFN (FN{pat,body}, f) = {pat=pat,body=body,f=f}
                      | seekFN _ = raise NoBetaReduction
                in let val {pat, body, f} = seekFN (exp1, fn x => x)
                       val res = f (LET{pat=fn_to_let_pat pat, bind=exp2, scope=body})
                   in tick "appletfn-fn"; reduce (env, (res, CUNKNOWN))
                   end handle NoBetaReduction => fail
                end
          | SWITCH_I {switch,precision} => reduce_switch (reduce, env, fail, switch, selectorNONE)
          | SWITCH_W {switch,precision,tyname} => reduce_switch (reduce, env, fail, switch, selectorNONE)
          | SWITCH_S switch => reduce_switch (reduce, env, fail, switch, selectorNONE)
          | SWITCH_C switch => reduce_switch (reduce, env, fail, switch, selectorCon env)
          | SWITCH_E switch => reduce_switch (reduce, env, fail, switch, selectorNONE)
          | PRIM(CCALLprim{name="__real_to_f64",...}, [REAL(s,_)]) =>
            (tick "real immed to f64 immed";
             reduce (env,(F64 s,CUNKNOWN)))
          | PRIM(CCALLprim{name="__real_to_f64",...},[e]) =>
            let fun loop e f =
                    case e of
                        PRIM(CCALLprim{name="__f64_to_real",...},[e]) =>
                        (tick "real unbox o box elimination - let";
                         SOME (f e))
                      | LET{pat,bind,scope} => loop scope (f o (fn e => LET{pat=pat,bind=bind,scope=e}))
                      | _ => NONE
                fun default () =
                    case loop e (fn x => x) of
                        NONE => constantFolding env lamb fail
                      | SOME e' => reduce(env,(e',CUNKNOWN))
            in case e of
                   SWITCH_C(SWITCH(e,es,eopt)) =>
                   let fun is_f64_to_real e =
                           case e of
                               PRIM(CCALLprim{name="__f64_to_real",...},_) => true
                             | _ => false
                   in if simple_nonexpanding e
                         andalso let val es = case eopt of SOME e => e::map #2 es
                                                         | NONE => map #2 es
                                 in List.exists (is_f64_to_real) es
                                 end
                      then
                        ( tick "switch real_to_f64"
                        ; reduce(env,
                                 (SWITCH_C(SWITCH(e,
                                                  List.map (fn (c,e) => (c,real_to_f64 e)) es,
                                                  Option.map real_to_f64 eopt)),
                                  CUNKNOWN))
                        )
                      else default()
                   end
                 | _ => default()
            end
          | PRIM(CCALLprim{name="ord",...}, [WORD (i,t)]) =>
            (tick "ord immed"; (INTEGER(i,intDefaultType()), CUNKNOWN))
          | PRIM(CCALLprim{name,Type,...},xs) =>
            if unbox_reals() then
              case (name,xs) of
                  ("__plus_real",[x,y]) => reduce_f64bin f64_plus (x,y)
                | ("__minus_real",[x,y]) => reduce_f64bin f64_minus (x,y)
                | ("__mul_real",[x,y]) => reduce_f64bin f64_mul (x,y)
                | ("divFloat",[x,y]) => reduce_f64bin f64_div (x,y)
                | ("__max_real",[x,y]) => reduce_f64bin f64_max (x,y)
                | ("__min_real",[x,y]) => reduce_f64bin f64_min (x,y)
                | ("sqrtFloat",[x]) => reduce_f64uno f64_sqrt x
                | ("__neg_real",[x]) => reduce_f64uno f64_neg x
                | ("__abs_real",[x]) => reduce_f64uno f64_abs x
                | ("realInt",[x]) =>
                  (tick "real_to_f64";
                   (f64_to_real (PRIM(CCALLprim {name="__int_to_f64",instances=[],tyvars=[],
                                                 Type=ARROWtype([intDefaultType()],NONE,[f64Type],NONE)},
                                      [x])), CUNKNOWN))
                | ("__less_real",[x,y]) => reduce_f64cmp f64_less (x,y)
                | ("__lesseq_real",[x,y]) => reduce_f64cmp f64_lesseq (x,y)
                | ("__greater_real",[x,y]) => reduce_f64cmp f64_greater (x,y)
                | ("__greatereq_real",[x,y]) => reduce_f64cmp f64_greatereq (x,y)
                | ("__blockf64_sub_real",[t,i]) =>
                  let val argTypes =
                          case Type of
                              ARROWtype(argTypes, _, _, _) => argTypes
                            | _ => die "prim(__blockf64_sub_real): expecting arrow type"
                  in tick "real_to_f64";
                     (f64_to_real (PRIM(CCALLprim{name="__blockf64_sub_f64",instances=[],tyvars=[],
                                                  Type=ARROWtype(argTypes,NONE,[f64Type],NONE)},
                                        [t,i])),
                      CUNKNOWN)
                  end
                | ("__blockf64_update_real",[t,i,v]) =>
                  let val (bType,iType) =
                          case Type of
                              ARROWtype([bType,iType,_], _, _, _) => (bType,iType)
                            | _ => die "prim(__blockf64_update_real): expecting arrow type with three args"
                  in tick "real_to_f64";
                     (PRIM(CCALLprim{name="__blockf64_update_f64",instances=[],tyvars=[],
                                     Type=ARROWtype([bType,iType,f64Type],NONE,[unitType],NONE)},
                           [t,i,#1(reduce(env,(real_to_f64 v,CUNKNOWN)))]),
                      CUNKNOWN)
                  end
                | _ => constantFolding env lamb fail
            else constantFolding env lamb fail
          | _ => constantFolding env lamb fail


      (* -----------------------------------------------------------------
       * Contract on switch
       * ----------------------------------------------------------------- *)

      fun contr_switch (contr, reduce, env, SW, SWITCH(arg, sel, opt)) =
        let val arg' = fst (contr (env,arg))
            val mix = map (fn (a,e) => (a, contr (env,e))) sel
            val sel' = map (fn (a,(e,_)) => (a, e)) mix
            val cvs = map (fn (_, (_,cv)) => cv) mix
        in case opt
             of SOME lamb =>
               let val (lamb',cv') = contr (env,lamb)
                   val cv = lubList (cv'::cvs)
               in reduce (env, (SW(SWITCH(arg',sel',SOME lamb')), cv))
               end
              | NONE =>
               let val cv = lubList cvs
               in reduce (env, (SW(SWITCH(arg',sel',NONE)), cv))
               end
        end


      (* -----------------------------------------------------------------
       * Contract on expression
       * ----------------------------------------------------------------- *)

      fun contr (env:env, lamb:LambdaExp) : (LambdaExp * cv) =
        let val (lamb, cv) = reduce (env, (lamb, CUNKNOWN))
        in case lamb
             of FN{pat,body} =>
               let val lvars = lvars_fn_pat pat
                   val env' = updateEnv lvars
                               (map (fn lvar => ([], CVAR {exp=VAR{lvar=lvar,instances=[],regvars=[]}})) lvars) env
                   val (body',_) = contr (env', body)
               in (FN{pat=pat,body=body'},CUNKNOWN)
               end
              | LET{pat=(pat as [(lvar,tyvars,tau)]),bind,scope} =>
               let val (bind', cv) = contr (env, bind)
                   val cv' = if noinline_lvar lvar then CUNKNOWN
                             else if is_inlinable_fn lvar bind' then CFN{lexp=bind',large=false}
                             else if is_fn bind' then CFN{lexp=bind',large=true}
                             else if is_unboxed_value bind' then CCONST {exp=bind'}
                             else (case bind'
                                    of VAR _ => CVAR {exp=bind'}
                                   (*  | PRIM(CONprim {con,...}, nil) => CCONST {exp=bind'} *)
                                     | _ => cv)
                   val env' = LvarMap.add(lvar,(tyvars,cv'),env)

                   val env' = case exn_anti_env bind of  (* under which conditions does bind not raise an exception *)
                                  NONE => env'
                                | SOME env'' =>
                                  let (*val () = if LvarMap.isEmpty env'' then ()
                                               else pr_contract_env env''*)
                                  in LvarMap.plus(env',env'') (* if env'' = empty then, in principle bind
                                                                 is sure to raise an exception, but we
                                                                 will ignore this fact
                                                               *)
                                  end
                   val (scope',cv_scope) = contr (env', scope)
                   val cv_scope' = remove lvar cv_scope
               in reduce (env, (LET{pat=pat,bind=bind',scope=scope'}, cv_scope'))
               end
              | LET{pat=nil,bind,scope} =>  (* wild card *)
               let val (bind', cv) = contr (env, bind)
                   val (scope',cv_scope) = contr (env, scope)
               in reduce (env, (LET{pat=nil,bind=bind',scope=scope'}, cv_scope))
               end
              | PRIM(RECORDprim opt, lambs) =>
               let val lamb_cv = map (fn e => contr (env,e)) lambs
               in (PRIM(RECORDprim opt, map fst lamb_cv),CRECORD (map snd lamb_cv))
               end
              | PRIM(prim as EXCONprim excon, lambs) =>
               let val lambs' = map (fst o (fn e => contr (env, e))) lambs
               in (mk_live_excon excon; (PRIM(prim, lambs'), CUNKNOWN))
               end
              | PRIM(prim as DEEXCONprim excon, lambs) =>
               let val lambs' = map (fst o (fn e => contr (env,e))) lambs
               in (mk_live_excon excon; (PRIM(prim, lambs'), CUNKNOWN))
               end
              | PRIM(RESET_REGIONSprim _, [VAR _]) => (lamb, CUNKNOWN) (* Sweden: avoid inlining of variable *)
              | PRIM(p as CCALLprim{name="word_table_init",...},lambs) =>
                let val lambs' = map (fst o (fn e => contr (env, e))) lambs
                in case lambs' of
                       [INTEGER(v,_),e] => (PRIM(p,lambs'),CBLKSZ v)
                     | _ => (PRIM(p,lambs'),CUNKNOWN)
                end
              | PRIM(p as CCALLprim{name="word_table0",...},lambs) =>
                let val lambs' = map (fst o (fn e => contr (env, e))) lambs
                in case lambs' of
                       [INTEGER(v,_)] => (PRIM(p,lambs'),CBLKSZ v)
                     | _ => (PRIM(p,lambs'),CUNKNOWN)
                end
              | PRIM(p as CCALLprim{name="word_table2d0_init",...},lambs) =>
                let val lambs' = map (fst o (fn e => contr (env, e))) lambs
                in case lambs' of
                       [_,_,INTEGER(v0,_),INTEGER(v1,_)] => (PRIM(p,lambs'),CBLK2SZ (SOME v0,SOME v1))
                     | [_,_,INTEGER(v0,_),_] => (PRIM(p,lambs'),CBLK2SZ (SOME v0,NONE))
                     | [_,_,_,INTEGER(v1,_)] => (PRIM(p,lambs'),CBLK2SZ (NONE,SOME v1))
                     | _ => (PRIM(p,lambs'),CUNKNOWN)
                end
              | PRIM(p as CCALLprim{name="word_table2d0",...},lambs) =>
                let val lambs' = map (fst o (fn e => contr (env, e))) lambs
                in case lambs' of
                       [_,INTEGER(v0,_),INTEGER(v1,_)] => (PRIM(p,lambs'),CBLK2SZ (SOME v0,SOME v1))
                     | [_,INTEGER(v0,_),_] => (PRIM(p,lambs'),CBLK2SZ (SOME v0,NONE))
                     | [_,_,INTEGER(v1,_)] => (PRIM(p,lambs'),CBLK2SZ (NONE,SOME v1))
                     | _ => (PRIM(p,lambs'),CUNKNOWN)
                end
              | PRIM(p as CCALLprim{name="ord",...},[e]) =>
                let val (e',_) = contr (env, e)
                in (PRIM(p,[e']),CRNG{low=SOME (IntInf.fromInt 0),
                                      high=SOME (IntInf.fromInt 255)})
                end
              | PRIM(p as CCALLprim{name="table_size",...}, [e]) =>
                let val (e',cv) = contr (env,e)
                    fun fail () = (PRIM(p,[e']),CUNKNOWN)
                in case cv of
                       CBLKSZ i => if safeLambdaExp e' then
                                     let val e'' = INTEGER(i,intDefaultType())
                                     in tick "contr - table_size";
                                        decr_uses e';
                                        (e'', CCONST {exp=e''})
                                     end
                                   else fail()
                     | _ => fail()
                end
              | PRIM(p as CCALLprim{name="word_sub0",...}, [a,i as INTEGER(idx,_)]) =>
                let val (a',cv) = contr (env,a)
                    fun fail () = (PRIM(p,[a',i]),CUNKNOWN)
                    fun mk s i = let val e = INTEGER(i,intDefaultType())
                                 in tick ("contr - table2d_size" ^ s);
                                    decr_uses a'; (e, CCONST {exp=e})
                                 end
                in if safeLambdaExp a' then
                     case (idx,cv) of
                         (0, CBLK2SZ (SOME i,_)) => mk "0" i
                       | (1, CBLK2SZ (_,SOME i)) => mk "1" i
                       | _ => fail()
                   else fail()
                end
              | PRIM(prim as CONprim {con,...},[e]) =>
                let val (e',cv') = contr (env,e)
                in (PRIM(prim,[e']), CCON1(con,cv'))
                end
              | PRIM(prim as CONprim {con,...},[]) =>
                (PRIM(prim,[]), CCON0 con)
              | PRIM(prim as DECONprim {con,...},[e]) =>
                let val (e',cv') = contr (env,e)
                    fun default () = (PRIM(prim,[e']), CUNKNOWN)
                in case cv' of
                       CCON1(con',cv'') => if Con.eq(con,con') then
                                             (PRIM(prim,[e']), cv'')
                                           else default()
                     | _ => default()
                end
              | PRIM(prim as SELECTprim {index}, [e]) =>
                let val (e',cv') = contr (env,e)
                    val cv'' = case cv' of
                                   CRECORD cvs =>
                                   (List.nth(cvs,index)
                                    handle _ => CUNKNOWN)
                                 | _ => CUNKNOWN
                in (PRIM(prim,[e']), cv'')
                end
              | PRIM(prim,lambs) => (PRIM(prim,map (fst o (fn e => contr (env,e))) lambs),CUNKNOWN)
              | FIX{functions,scope} =>
               let val lvs = map #lvar functions
                   val env0 = updateEnv lvs (map (fn _ => ([],CUNKNOWN)) functions) env
                   val _ = app mark_lvar lvs
                   val functions' = map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                                         {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,constrs=constrs,
                                          bind=fst (contr (env0, bind))}) functions
                   val _ = app unmark_lvar lvs
                   val env' = case functions
                                of [function as {lvar,regvars=[],tyvars,Type,constrs=[],bind}] =>  (* memo:regvars *)
                                   let val cv =
                                           if specialize_recursive_functions() then
                                             if specializable function then
                                               CFIX{N=NONE,Type=Type,bind=bind,
                                                    large=not(small_lamb (max_specialise_size()) bind)}
                                             else case specializableN function of
                                                      SOME n => CFIX{N=SOME n,Type=Type,bind=bind,
                                                                     large=not(small_lamb (max_specialise_size()) bind)}
                                                    | NONE => CUNKNOWN
                                           else CUNKNOWN
                                   in updateEnv [lvar] [(tyvars,cv)] env
                                  end
                                 | _ => updateEnv lvs (map (fn {tyvars,...} => (tyvars,CUNKNOWN)) functions) env
                   val (scope', cv) = contr (env', scope)
                   val cv' = removes lvs cv
               in reduce (env, (FIX{functions=functions', scope=scope'}, cv'))
               end
              | APP(lamb1, lamb2, _) =>
               let val lamb1' = fst(contr (env, lamb1))
                   val lamb2' = fst(contr (env, lamb2))
               in reduce (env, (APP(lamb1',lamb2',NONE), CUNKNOWN))
               end
              | EXCEPTION(excon,tauOpt,lamb) =>
               let val (lamb', cv) = contr (env, lamb)
               in if is_live_excon excon then (EXCEPTION(excon,tauOpt, lamb'), cv)
                  else (tick "dead - excon"; (lamb', cv))
               end
              | RAISE(lamb,tl) => (RAISE(fst(contr (env, lamb)),tl),CUNKNOWN)
              | LETREGION {regvars,scope} => (LETREGION{regvars=regvars,scope=fst(contr (env, scope))},CUNKNOWN)
              | HANDLE(lamb1, lamb2) => (HANDLE(fst(contr (env, lamb1)), fst(contr (env, lamb2))),CUNKNOWN)
              | SWITCH_I {switch,precision} =>
               contr_switch (contr, reduce, env, fn sw => SWITCH_I {switch=sw, precision=precision}, switch)
              | SWITCH_W {switch,precision,tyname} =>
               contr_switch (contr, reduce, env, fn sw => SWITCH_W {switch=sw, precision=precision,tyname=tyname}, switch)
              | SWITCH_S switch => contr_switch (contr, reduce, env, SWITCH_S, switch)
              | SWITCH_C switch => contr_switch (contr, reduce, env, SWITCH_C, switch)
              | SWITCH_E switch =>
               let val res = contr_switch (contr, reduce, env, SWITCH_E, switch)
                   val SWITCH(_,sel,_) = switch
                   fun mklive [] = ()
                     | mklive (((excon,_),_)::rest) = (mk_live_excon excon; mklive rest)
               in mklive sel; res
               end
              | TYPED(lamb,t,cs) => (TYPED(fst(contr (env, lamb)),t,cs),CUNKNOWN)
              | FRAME{declared_excons,declared_lvars} =>
               let val lvars = map #lvar declared_lvars
                   val excons = map #1 declared_excons
                   val env' =
                     List.foldl (fn (lv,acc) =>
                                    case LvarMap.lookup env lv of
                                        SOME res =>
                                        let val xinl = cross_module_inline (lvars, [Excon.ex_SUBSCRIPT,Excon.ex_SIZE]) lv res
                                        in if xinl
                                           then LvarMap.add(lv,res,acc)
                                           else LvarMap.add(lv,(nil,CUNKNOWN),acc)
                                        end
                                      | NONE => LvarMap.add(lv,(nil,CUNKNOWN),acc))
                     LvarMap.empty lvars
               in  frame_contract_env := SOME env'
                 ; app mk_live_excon excons
                 ; (lamb, CUNKNOWN)
               end
              | _ => (lamb, cv)
        end
    in
      type contract_env = env

      val layout_contract_env = layout_contract_env

      fun unknown_env (e:env) : env =
          LvarMap.composemap (fn (tvs,_) => (tvs,CUNKNOWN)) e

      fun contract_env_dummy lamb : contract_env =
        let val lvars = #1(exports lamb)
        in List.foldl (fn (lv, acc) => LvarMap.add(lv,(nil,CUNKNOWN),acc))
           LvarMap.empty lvars
        end

      fun contract (ce:contract_env) lamb =
        if contract_p() then
          let val _ = log "contracting\n"
              val _ = reset_excon_bucket()
              val _ = init lamb
              val _ = frame_contract_env := NONE
              val lamb' = fst (contr (ce, lamb))
              val _ = reset_lvar_bucket ()
              val _ = reset_excon_bucket()
          in case !frame_contract_env
               of SOME ce' => (lamb',ce')
                | NONE => die "contract.no frame-env"
          end
        else
          (lamb, contract_env_dummy lamb)

      fun enrich_contract_env (ce1,ce2) =
          LvarMap.enrich eq_cv_scheme (ce1,ce2)

      (* Restrict *)

      fun free_exp (e,(lvs,cns,tns)) =
          let val (lvs',_) = LambdaBasics.freevars e
          in (lvs'@lvs,cns,tns)
          end

      fun free_cv (cv,acc) =
          case cv of
              CVAR {exp} => free_exp (exp,acc)
            | CRECORD cvs => List.foldl free_cv acc cvs
            | CUNKNOWN => acc
            | CCONST {exp} => free_exp (exp,acc)
            | CFN {lexp: LambdaExp, large:bool} => free_exp(lexp,acc)
            | CFIX {N,Type: Type, bind: LambdaExp, large: bool} => free_exp(bind,acc)
            | CBLKSZ _ => acc
            | CBLK2SZ _ => acc
            | CRNG _ => acc
            | CCON1 (cn,cv) =>
              let val (lvs,cns,tns) = free_cv (cv,acc)
              in (lvs,cn::cns,tns)
              end
            | CCON0 cn =>
              let val (lvs,cns,tns) = acc
              in (lvs,cn::cns,tns)
              end

      fun free_contract_env_res ((_,cv),acc) =
          free_cv(cv,acc)

      local
        fun restrict_contract_env0 (ce,lvs) =
            List.foldl (fn (lv,(e,acc)) =>
                           case LvarMap.lookup ce lv of
                               SOME res =>
                               let val acc = free_contract_env_res(res,acc)
                               in (LvarMap.add(lv,res,e),acc)
                               end
                             | NONE => die "restrict_contract_env.lv not in env")
                       (LvarMap.empty,(nil,nil,nil)) lvs
        fun clean () = ( reset_lvar_bucket(); reset_excon_bucket() )
      in
        fun restrict_contract_env (ce,lvs) =
            let val () = clean()
                val (e,(lvs,cns,tns)) = restrict_contract_env0 (ce,lvs)
            in clean() ; (e,lvs,cns,tns)
            end handle X => ( clean() ; raise X)
      end

      (* Warn on functions that are specified to be inlined but for
         which their definitions are not provided in the compiler
         environments.
       *)

      fun warn_on_failed_inlines ce : unit =
        let fun warn lv =
                print ("** WARNING: variable '" ^ Lvars.pr_lvar lv ^ "' is not inlined. Expressions\n\
                       \** that are effectful are not inlined and neither are functions that refer\n\
                       \** to unexported identifiers.\n")
            fun check cv k =
                case cv of
                    CFN _ => ()
                  | CCONST _ => ()
                  | _ => k()
        in LvarMap.Fold (fn ((lv,(_,cv)),()) =>
                            if inline_lvar lv then
                              check cv (fn () => warn lv)
                            else ()) () ce
        end

      (* Serialisation *)

      val pu_contract_env =
          let fun toInt (CVAR _) = 0
                | toInt (CRECORD _) = 1
                | toInt CUNKNOWN = 2
                | toInt (CCONST _) = 3
                | toInt (CFN _) = 4
                | toInt (CFIX _) = 5
                | toInt (CBLKSZ _) = 6
                | toInt (CRNG _) = 7
                | toInt (CBLK2SZ _) = 8
                | toInt (CCON1 _) = 9
                | toInt (CCON0 _) = 10

              fun fun_CVAR _ =
                  Pickle.con1 (fn e => CVAR {exp=e}) (fn CVAR {exp} => exp | _ => die "pu_contract_env.CVAR")
                  LambdaExp.pu_LambdaExp
              fun fun_CRECORD pu =
                  Pickle.con1 CRECORD (fn CRECORD a => a | _ => die "pu_contract_env.CRECORD")
                  (Pickle.listGen pu)
              val fun_CUNKNOWN = Pickle.con0 CUNKNOWN
              fun fun_CCONST _ =
                  Pickle.con1 (fn e => CCONST {exp=e}) (fn CCONST {exp} => exp | _ => die "pu_contract_env.CCONST")
                  LambdaExp.pu_LambdaExp
              fun fun_CFN _ =
                  Pickle.con1 CFN (fn CFN a => a | _ => die "pu_contract_env.CFN")
                  (Pickle.convert (fn (e,l) => {lexp=e,large=l}, fn {lexp=e,large=l} => (e,l))
                   (Pickle.pairGen0(LambdaExp.pu_LambdaExp,Pickle.bool)))
              fun fun_CFIX _ =
                  Pickle.con1 CFIX (fn CFIX a => a | _ => die "pu_contract_env.CFIX")
                              (Pickle.convert (fn (N,t,e,l) => {N=N,Type=t,bind=e,large=l},
                                               fn {N,Type=t,bind=e,large=l} => (N,t,e,l))
                                              (Pickle.tup4Gen0(Pickle.optionGen Pickle.int,
                                                               LambdaExp.pu_Type,
                                                               LambdaExp.pu_LambdaExp,
                                                               Pickle.bool)))
              fun fun_CBLKSZ _ =
                  Pickle.con1 CBLKSZ (fn CBLKSZ a => a | _ => die "pu_contract_env.CBLKSZ")
                  pu_intinf
              fun fun_CRNG _ =
                  Pickle.con1 (fn (l,h) => CRNG{low=l,high=h}) (fn CRNG{low,high} => (low,high)
                                                                 | _ => die "pu_contract_env.CRNG")
                  (Pickle.pairGen(Pickle.optionGen pu_intinf, Pickle.optionGen pu_intinf))
              fun fun_CBLK2SZ _ =
                  Pickle.con1 CBLK2SZ (fn CBLK2SZ a => a | _ => die "pu_contract_env.CBLK2SZ")
                              (Pickle.pairGen(Pickle.optionGen pu_intinf, Pickle.optionGen pu_intinf))
              fun fun_CCON1 pu =
                  Pickle.con1 CCON1 (fn CCON1 a => a | _ => die "pu_contract_env.CCON1")
                              (Pickle.pairGen(Con.pu,pu))
              fun fun_CCON0 pu = Pickle.con1 CCON0 (fn CCON0 c => c | _ => die "pu_contract_env.CCON0")
                                             Con.pu
              val pu_cv =
                  Pickle.dataGen("OptLambda.cv",toInt,[fun_CVAR,fun_CRECORD,fun_CUNKNOWN,
                                                       fun_CCONST,fun_CFN,fun_CFIX,fun_CBLKSZ,
                                                       fun_CRNG,fun_CBLK2SZ,fun_CCON1,fun_CCON0])
          in LvarMap.pu Lvars.pu
              (Pickle.pairGen(LambdaExp.pu_tyvars,pu_cv))
          end
    end


   (* -----------------------------------------------------------------
    * eliminate_explicit_records lamb - eliminate bindings of explicit
    *   records only used for selections. Transform
    *          let r = (e1,...,en) in ... #i r .. #j r ...
    *   into
    *          let x1=e1 in ... let xn=en in ... xi .. xj ...
    *
    *   We first traverse the expression top-down marking all variables
    *   bound to explicit records and unmarking all uses of variables
    *   not used in select contexts. Then we perform the transformation
    *   top-down. Information about fresh variables is kept in an
    *   environment.
    * ----------------------------------------------------------------- *)

   local
     fun traverse lamb =
       let fun f lamb =
             case lamb
               of LET{pat=[(lvar,[],_)],bind=PRIM(RECORDprim _,lambs),scope} =>
                 (mark_lvar lvar; app f lambs; f scope)
                | PRIM(SELECTprim _, [VAR{instances=[],...}]) => ()
                | VAR{lvar,...} => unmark_lvar lvar
                | FRAME{declared_lvars,...} => app (unmark_lvar o #lvar) declared_lvars
                | _ => app_lamb f lamb
       in app_lamb f lamb
       end

     type env = (lvar list) LvarMap.map

     fun transf env lamb =
        case lamb
          of PRIM(SELECTprim {index=i}, [VAR{lvar,instances=[],regvars=[]}]) =>
            (case LvarMap.lookup env lvar
               of SOME lvars =>
                 let val lvar' = List.nth(lvars, i)
                                 handle Subscript => die "eliminate_explicit_records"
                 in tick "eliminate explicit records - select";
                    VAR{lvar=lvar',instances=[],regvars=[]}
                 end
                | NONE => lamb)
           | LET{pat=[(lvar,[],Type)],bind=PRIM(RECORDprim _, lambs),scope} =>
              if is_marked_lvar lvar then
                let val lvars = map (fn _ => Lvars.newLvar()) lambs
                    val env' = LvarMap.add(lvar,lvars,env)
                    val taus = case Type
                                 of RECORDtype (taus,_) => taus
                                  | _ => die "eliminate_explicit_records2"
                    fun mk_lamb [] [] [] = transf env' scope
                      | mk_lamb (lv::lvs) (tau::taus) (lamb::lambs) =
                        ((if eq_Type(tau,f64Type) then Lvars.set_ubf64 lv else ());
                         LET{pat=[(lv,[],tau)],
                             bind=transf env lamb,
                             scope=mk_lamb lvs taus lambs})
                      | mk_lamb _ _ _ = die "eliminate_explicit_records3"
                in tick "eliminate explicit records - binding";
                   mk_lamb lvars taus lambs
                end
              else map_lamb (transf env) lamb
           | _ => map_lamb (transf env) lamb
   in
     fun eliminate_explicit_records lamb =
       if eliminate_explicit_records_p() then
         (log "eliminating explicit records\n";
(*        log " traversing\n"; *)
          traverse lamb;
(*        log " transformation\n"; *)
          let val lamb' = transf LvarMap.empty lamb
          in reset_lvar_bucket(); lamb'
          end)
       else lamb
   end

   (* -----------------------------------------------------------------
    * eliminate_explicit_blockf64_bindings lamb - eliminate bindings of
    * explicit blockf64 bindings only used for selections inside safe
    * contexts. Transform expressions of the form
    *
    *          let r = {e1,...,en} in ... (r sub i) .. (r sub j) ...
    *
    * into
    *
    *          let x1=f64_to_real(e1) in ...
    *          let xn=f64_to_real(en) in ...
    *             real_to_f64(xi) .. real_to_f64(xj) ...
    *
    * We first traverse the expression top-down marking all variables
    * bound to explicit blocks and unmarking all uses of variables
    * not used in subscripting contexts. Then we perform the transformation
    * top-down. Information about fresh variables is kept in an
    * environment.
    * ----------------------------------------------------------------- *)

   local
     fun traverse lamb =
       let fun f lamb =
             case lamb
               of LET{pat=[(lvar,[],_)],bind=PRIM(BLOCKF64prim,lambs),scope} =>
                 (mark_lvar lvar; app f lambs; f scope)
                | PRIM(CCALLprim{name="__blockf64_sub_f64",...}, [VAR{instances=[],...},INTEGER _]) => ()
                | VAR{lvar,...} => unmark_lvar lvar
                | FRAME{declared_lvars,...} => app (unmark_lvar o #lvar) declared_lvars
                | _ => app_lamb f lamb
       in app_lamb f lamb
       end

     type env = (lvar list) LvarMap.map

     fun transf env lamb =
        case lamb
          of PRIM(CCALLprim{name="__blockf64_sub_f64",...}, [VAR{lvar,instances=[],regvars=[],...},INTEGER (i32,_)]) =>
             (case LvarMap.lookup env lvar of
                  SOME lvars =>
                  let val i = IntInf.toInt i32
                              handle Overflow => die "eliminate_explicit_blockf64s: expecting small int"
                      val lvar' = List.nth(lvars, i)
                                  handle Subscript => die "eliminate_explicit_blockf64s: subscript error"
                  in tick "eliminate explicit blockf64s - sub";
                     real_to_f64(VAR{lvar=lvar',instances=[],regvars=[]})
                  end
                | NONE => lamb)
           | LET{pat=[(lvar,[],_)],bind=PRIM(BLOCKF64prim,lambs),scope} =>
             if is_marked_lvar lvar then
               let val lvars_and_lambs = map (fn e => (Lvars.newLvar(),e)) lambs
                   val lvars = map #1 lvars_and_lambs
                   val env' = LvarMap.add(lvar,lvars,env)
                   fun build [] = transf env' scope
                     | build ((lv,lamb)::pairs) =
                       LET{pat=[(lv,[],realType)],
                           bind=f64_to_real(transf env lamb),
                           scope=build pairs}
               in tick "eliminate explicit blockf64 - binding";
                  build lvars_and_lambs
               end
             else map_lamb (transf env) lamb
           | _ => map_lamb (transf env) lamb
   in
     fun eliminate_explicit_blockf64_bindings lamb =
       if eliminate_explicit_records_p() then
         (log "eliminating explicit blockf64 bindings\n";
(*        log " traversing\n"; *)
          traverse lamb;
(*        log " transformation\n"; *)
          let val lamb' = transf LvarMap.empty lamb
          in reset_lvar_bucket(); lamb'
          end)
       else lamb
   end


   (* -----------------------------------------------------------------
    * Common subexpression elimination
    *
    * Perform a simple kind of common subexpression elimination:
    *
    *
    *   1. Turn expressions of the form `fbinop(e,e)` into
    *      `let x = e in fbinop(x,x)` (provided e is non-expanding
    *      and not already a variable)
    *
    * --------------------------------------------------------------- *)

   fun cse e =
       case e of
           PRIM(p as CCALLprim{name,...}, es) =>
           let fun mkbin (e1,e2) =
                   let fun nonVAR (VAR _) = false
                         | nonVAR e = true
                   in if nonVAR e1 andalso eq_lamb (e1,e2) andalso simple_nonexpanding e1 then
                        let val lv = Lvars.newLvar()
                            val () = Lvars.set_ubf64 lv
                            val a = VAR{lvar=lv,instances=nil,regvars=nil}
                            val () = tick "CSE - f64"
                        in LET{pat=[(lv,[],f64Type)],bind=e1,scope=PRIM(p,[a,a])}
                        end
                      else PRIM(p,[e1,e2])
                   end
               val es = map cse es
           in case (name,es) of
                  ("__mul_f64", [e1,e2]) => mkbin (e1,e2)
                | _ => PRIM(p,es)
           end
         | _ => map_lamb cse e

   (* -----------------------------------------------------------------
    * Hoist blockf64 allocations
    *
    * Hoist blockf64 allocations to allow for unboxing of reals:
    *
    *   1. Convert `let x = f64_to_real(a) in let y = alloc(b) in e`
    *      into `let y = alloc(b) in let x = f64_to_real(a) in e`
    *      given x \not \in fv(b).
    *
    * This transformation will put the x binding closer to uses; in
    * particular, x can be allocated in an xmm register if the binding
    * and its uses do not cross a C function call...
    * --------------------------------------------------------------- *)

   fun hoist_blockf64_allocations e =
       let fun hoist e =
               case e of
                   LET{pat=pat as [(lv,_,_)],bind,scope} =>
                   let val bind = hoist bind
                       val scope = hoist scope
                   in case scope of
                          LET{pat=pat2,bind=bind2 as PRIM(CCALLprim{name="allocStringML",...},[e]),
                              scope=scope2} => (* yes *)
                          if not(lvar_in_lamb lv e) andalso (safeLambdaExp e orelse safeLambdaExp bind) then
                            LET{pat=pat2,bind=bind2,
                                scope=LET{pat=pat,bind=bind,scope=scope2}}
                          else
                            LET{pat=pat,bind=bind,
                                scope=LET{pat=pat2,bind=bind2,scope=scope2}}
                        | _ =>
                          LET{pat=pat,bind=bind,scope=scope}
                   end
                 | _ => map_lamb hoist e
       in hoist e
       end


   (* -----------------------------------------------------------------
    * Minimize fixs: split fix's into strongly connected components:
    *
    *     1) Build a graph, G, with the bound lambda variables of the
    *        fix as nodes and let there be an edge from g to f in G if
    *        g calls f.
    *     2) Compute the list of strongly connected components (a list
    *        of lists of nodes), such that a children in the scc-graph
    *        of G is listed before its parent.
    *     3) Perform substitutions to obtain correct instance-lists for
    *        variables no longer in the same fix.
    *     4) Rename bound type variables.
    *     5) Reconstruct expression.
    * ----------------------------------------------------------------- *)

   structure DG = LvarDiGraphScc
   local

     (* Build graph *)
     type fs = {lvar:lvar,regvars:RegVar.regvar list,tyvars:tyvar list,Type:Type,constrs:constr list,bind:LambdaExp}
     fun mk_nodes (functions,G) =
       let fun mn ([]:fs list) = ()
             | mn (f::fs) = (DG.addNode (DG.mkNode(#lvar f)) G; mn fs)
       in mn functions
       end
     fun mk_edges (functions,G) =
       let fun maybe_add_edge (lv,lv',G) =
             let val n = DG.findNode lv G
                 val n' = DG.findNode lv' G
             in DG.addEdge n n' ()
             end handle _ => ()
           fun me ([]:fs list) = ()
             | me ({lvar,bind,...}::fs) =
               let fun f (VAR{lvar=lvar',...}) = maybe_add_edge (lvar,lvar',G)
                     | f lamb = app_lamb f lamb
               in f bind; me fs
               end
       in me functions
       end
     fun build_graph functions =
       let val G = DG.mkGraph()
           val _ = mk_nodes(functions,G)
           val _ = mk_edges(functions,G)
       in G
       end

     fun find_lv_functions ((f as ({lvar,...} : fs))::fs) lv =
       if Lvars.eq(lv,lvar) then f else find_lv_functions fs lv
       | find_lv_functions _ _ = die "find_lv_functions"

     (* Compute SCC *)
     fun compute_scc (functions,G) =
       let val node_list_list = DG.scc G
           val lv_list_list = map (map (! o DG.getInfoNode)) node_list_list
           val f_list_list = map (map (find_lv_functions functions)) lv_list_list

(*         fun log s = output(!Flags.log, s)
           fun log' [] = ()
             | log' [lv] = log (Lvars.pr_lvar lv)
             | log' (lv::lvs) = (log (Lvars.pr_lvar lv); log ", "; log' lvs)
           fun logs [] = ()
             | logs (l::ls) = (log " c = ["; log' l; log "]\n"; logs ls)
           val _ = (log "\nscc:\n"; logs lv_list_list)
*)
       in f_list_list
       end

     (* Update instances of lambda variables *)
     fun update_instances scc =
       let fun on_f (IS : lvar -> Type list option) ({lvar,regvars,tyvars,Type,constrs,bind}:fs) : fs =
             let fun on_bind (lamb as VAR{lvar,instances=[],regvars=[]}) =
                   (case IS(lvar)
                      of SOME instances => VAR{lvar=lvar,instances=instances,regvars=[]}
                       | NONE => lamb)
                   | on_bind lamb = map_lamb on_bind lamb
             in {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,constrs=constrs,bind=on_bind bind}
             end
           fun extend_IS IS c =
             let fun ext [] lv = IS(lv)
                   | ext (({lvar,tyvars,...}:fs)::c) lv = if Lvars.eq(lvar,lv) then SOME (map (fn tv => TYVARtype {tv=tv}) tyvars)
                                                          else ext c lv
             in ext c
             end
           fun on_scc (IS : lvar -> Type list option) [] = []
             | on_scc IS (c::cs) =
             let val c' = map (on_f IS) c
               val IS' = extend_IS IS c'
               val cs' = on_scc IS' cs
             in c'::cs'
             end
       in on_scc (fn _ => NONE) scc
       end

     (* Rename bound type variables of strongly connected component *)
     fun rename_btvs c =
        let fun get_tyvars [] tyvars = tyvars
              | get_tyvars (({tyvars,...}:fs)::c) tyvars' =
                let fun add [] tyvars = tyvars
                      | add (tv::tvs) tyvars = if mem tv tyvars then add tvs tyvars
                                               else add tvs (tv::tyvars)
                in add tyvars (get_tyvars c tyvars')
                end
            fun fresh_tv tv = if equality_tyvar tv then fresh_eqtyvar ()
                              else fresh_tyvar ()

            fun on_tyvar S tv =
              case on_Type S (TYVARtype {tv=tv})
                of TYVARtype {tv=tv'} => tv'
                 | _ => die "on_tyvar"

            fun on_c S [] = []   (* memo:regvars *)
              | on_c S (({lvar,regvars,tyvars,Type,constrs,bind}:fs)::c) =
              let val tyvars' = map (on_tyvar S) tyvars
                  val Type' = on_Type S Type
                  val bind' = on_LambdaExp S bind
              in {lvar=lvar,regvars=regvars,tyvars=tyvars',Type=Type',constrs=constrs,bind=bind'}::on_c S c
              end

            val tyvars = get_tyvars c []
            val types = map (TYVARtype o (fn tv => {tv=tv}) o fresh_tv) tyvars
            val S = mk_subst (fn () => "rn_btvs_c") (tyvars, types)
        in on_c S c
        end

      (* Reconstruct lambda expression *)
      fun reconstruct_exp ([],scope) = {functions=[],scope=scope}
        | reconstruct_exp ([c],scope) = {functions=c,scope=scope}
        | reconstruct_exp (c::cs,scope) = {functions=c,scope=FIX(reconstruct_exp(cs,scope))}

      fun minimize_fix (fix as {functions=[],scope}) = fix
        | minimize_fix (fix as {functions=[f],scope}) = fix
        | minimize_fix (fix as {functions, scope}) =
        let val G = build_graph functions
            val scc = compute_scc (functions,G)
            val scc' = update_instances scc
            val scc'' = map rename_btvs scc'
            val fix = reconstruct_exp (scc'', scope)
        in fix
        end
   in
     fun do_minimize_fixs lamb =
       let fun maybe_FIX {functions=nil,scope} = scope
             | maybe_FIX f = FIX f
           fun min_fixs (FIX fs) = map_lamb min_fixs (maybe_FIX (minimize_fix fs))
             | min_fixs lamb = map_lamb min_fixs lamb
       in if minimize_fixs() then min_fixs lamb
          else lamb
       end
   end


   (* -----------------------------------------------------------------
    * fix_conversion - convert   let x = e' in e   to   fix x = e' in e
    *   when e' is a lambda abstraction, in order to avoid building
    *   of closures in order to increase region polymorphism. This
    *   optimisations must be performed as one of the last
    *   optimisations.
    * ----------------------------------------------------------------- *)

   fun fix_conversion lamb =
     let fun f (LET{pat=[(lvar,tyvars,Type)],bind=bind as FN _,scope=scope}) =
                 ((*tick "fix conversion";*)
                  FIX{functions=[{lvar=lvar,regvars=[],tyvars=tyvars,Type=Type,constrs=[],bind=bind}],
                      scope=scope})
           | f (FIX{functions=[],scope}) = (tick "fix conversion - empty"; f scope)
           | f lamb = lamb
     in
       if !fix_conversion_ref then (log "fix_conversion\n"; passTD f lamb)
       else lamb
     end

   (* ----------------------------------------------------------------------------
    * unfix_conversion - convert   fix f x = e in e'  to let f = fn x => e in e'
    *   when f does not occur in e, in order for the inliner to work properly.
    * ---------------------------------------------------------------------------- *)

   fun unfix_conversion lamb =
       let fun f (e as FIX{functions=[{lvar,regvars=[],tyvars,Type,constrs=[],bind}],
                           scope}) =
               if lvar_in_lamb lvar bind then e
               else LET{pat=[(lvar,tyvars,Type)],bind=bind,scope=scope}
             | f e = e
       in passTD f lamb
       end

   (* -----------------------------------------------------------------
    * functionalise_let let_env lamb
    *   ensure that all let-constructs, binding other things than
    *   lambdas are not polymorphic; this is done by translating
    *   polymorphic non-functional let-constructs into functional ones
    *   (this is ok. because polymorphism is allowed only for
    *   non-expansive expressions.
    * ----------------------------------------------------------------- *)

   local
     datatype let_env_res = DELAY_SIMPLE | IGNORE
     fun layout_let_env_res DELAY_SIMPLE = PP.LEAF "DELAY_SIMPLE"
       | layout_let_env_res IGNORE = PP.LEAF "IGNORE"

     type let_env = let_env_res LvarMap.map

     fun enrich_let_env (e1,e2) =
         LvarMap.enrich (op =) (e1,e2)

     fun restrict_let_env (e,lvs) =
         LvarMap.restrict (Lvars.pr_lvar,e,lvs)

     val layout_let_env = LvarMap.layoutMap {start="LetEnv={",eq="->", sep=", ", finish="}"}
      (PP.LEAF o Lvars.pr_lvar) layout_let_env_res

     fun lookup env lv = LvarMap.lookup env lv
     fun add_lv (lv,res,env) = LvarMap.add(lv,res,env)

     fun is_fn_or_var (FN _) = true
       | is_fn_or_var (VAR _) = true
       | is_fn_or_var _ = false

     val frame_let_env = ref (LvarMap.empty : let_env)

     fun f env lamb =
       case lamb
         of v as VAR{lvar,instances,...} =>
           (case LvarMap.lookup env lvar
              of SOME DELAY_SIMPLE => APP(v, PRIM(RECORDprim {regvar=NONE}, []), NONE)
               | _ => v)
          | LET{pat,bind,scope} =>
              (case pat
                 of [(lvar,tyvars,Type)] =>
                  if null(tyvars) then
                    LET{pat=pat,bind=f env bind, scope=f (add_lv(lvar,IGNORE,env)) scope}
                  else (case bind of
                          FN _ => (* already a lambda abstraction; make no new abstraction *)
                            LET{pat=pat,bind=f env bind, scope=f (add_lv(lvar,IGNORE,env)) scope}
                        | non_expansive_bind =>
                            (* make lambda abstraction *)
                            let val Type' = ARROWtype([unit_Type], NONE, [Type], NONE)
                                val pat' = [(lvar,tyvars,Type')]
                                val bind' = FN{pat=[(Lvars.newLvar(),unit_Type)],body=f env bind}
                                val scope' = f (LvarMap.add(lvar,DELAY_SIMPLE,env)) scope
                            in LET{pat=pat',bind=bind',scope=scope'}
                            end
                          )
                  | nil => LET{pat=pat,bind=f env bind, scope=f env scope}
                  | _ => die "functionalise_let. non-trivial patterns unimplemented.")
          | FIX{functions,scope} =>
            let val functions' =
                    map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                            {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,constrs=constrs,
                             bind=f env bind}) functions
                val lvars = map #lvar functions
                val env' = List.foldl (fn (lv,acc) => add_lv(lv,IGNORE,acc)) env lvars
            in FIX{functions=functions', scope=f env' scope}
            end
          | FRAME {declared_lvars,...} =>
              let val env' = List.foldr (fn ({lvar,...},env') =>
                                       case lookup env lvar
                                         of SOME p => add_lv (lvar,p,env')
                                          | NONE => die ("functionalise_let.FRAME.lvar " ^
                                                         Lvars.pr_lvar lvar ^ " not in env"))
                             LvarMap.empty declared_lvars
              in frame_let_env := env'; lamb
              end
          | _ => map_lamb (f env) lamb
   in
     type let_env = let_env
     val enrich_let_env = enrich_let_env
     val restrict_let_env = restrict_let_env
     val layout_let_env = layout_let_env
     fun functionalise_let env lamb =
       let val lamb = f env lamb
       in (lamb, !frame_let_env)
       end
     val pu_let_env =
         LvarMap.pu Lvars.pu (Pickle.enumGen ("OptLambda.let_env_res",[DELAY_SIMPLE,IGNORE]))
   end

   (* -------------------------------------------------------------------
    * Fix-flattening takes care of both argument unboxing (flattening),
    * uncurrying, and elimination of unused arguments. The technique
    * is based on the concept of compositional argument manipulators,
    * which are atomic manipulations that aim at either eliminating an
    * argument, adding a new unboxed argument, or uncurrying a
    * function.
    * ------------------------------------------------------------------- *)

   structure FixFlatten = struct

   (* Some utilities *)
   infix |>
   fun a |> f = f a

   fun dropi i nil = nil
     | dropi i (x::xs) = if i = 0 then xs
                         else x::dropi (i-1) xs

   (* Argument manipulators *)
   datatype t = Comp of t * t | Id | Newprj of int * int | Drop of int | Unc
   infix oo
   fun Id oo t = t
     | t oo Id = t
     | t oo t' = Comp(t,t')
   fun pp_t (t:t) =
       case t of
           Comp(t',t) => pp_t t' ^ " . " ^ pp_t t
         | Id => "id"
         | Newprj(i,j) => "newprj(" ^ Int.toString i ^ "," ^ Int.toString j ^ ")"
         | Drop i => "drop(" ^ Int.toString i ^ ")"
         | Unc => "unc"
   type arr = Type list * regvar option * Type list * regvar option
   fun on_arr (t:t) ((tys,eps,tys',rho):arr) : arr =
       let fun prj i ty =
               case ty of
                   RECORDtype (tys,_) => (List.nth(tys,i) handle _ => die "on_arr.prj.index err")
                 | _ => if i = ~1 andalso LambdaBasics.eq_Type(realType,ty)
                        then f64Type
                        else die "on_arr.prj.not tuple or real"
       in case t of
              Newprj(i,j) =>
              let val ty' = List.nth(tys,i)
                            handle _ => die "on_arr.newprj.index err"
              in (tys @ [prj j ty'],eps,tys',rho)
              end
            | Drop i =>
              if i >= length tys then die "on_arr.drop"
              else (dropi i tys, eps,tys',rho)
            | Comp(t',t) => on_arr t' (on_arr t (tys,eps,tys',rho))
            | Id => (tys,eps,tys',rho)
            | Unc =>
              case tys' of
                  [ARROWtype([ty1],_,tys',_)] => (tys @ [ty1],eps,tys',rho)
                | [ARROWtype _] => die "on_arr.expecting single arrow with one argument type"
                | [ty] => die ("on_arr.expecting single arrow - got " ^ pr_Type ty)
                | _ => die "on_arr.expecting single type"
       end

   fun on_ty t (ARROWtype arr) = ARROWtype(on_arr t arr)
     | on_ty _ _ = die "on_ty.not arrow"

   fun lams nil e = e
     | lams ((x,ty)::vtys) e = FN {pat=[(x,ty)],body=lams vtys e}

   fun flatten_args t tys es =
       case t of
           Unc =>
           let val x = Lvars.newLvar()
           in case tys of
                  [ARROWtype([ty],_,tys',_)] => (es @ [VAR{lvar=x,instances=nil,regvars=nil}], [(x,ty)], tys')
                | [ARROWtype _] => die "flatten_args.Unc.no single arg ARROWtype"
                | [t] => die ("flatten_args.Unc.no ARROWtype: " ^ pr_Type t)
                | _ => die ("flatten_args.Unc.no single type: " ^ pr_Types tys)
           end
         | Drop i =>
           if i > length es then die "flatten_args.Drop"
           else (dropi i es, [], tys)
         | Newprj(i,j) =>
           let val e = List.nth(es,i)
                       handle _ => die "flatten_args.Newprj"
           in (es @ [if j = ~1
                     then real_to_f64 e
                     else PRIM(SELECTprim {index=j},[e])],
               [], tys)
           end
         | Comp(t',t) =>
           let val (es,vtys,tys) = flatten_args t tys es
               val (es,vtys',tys) = flatten_args t' tys es
           in (es,vtys@vtys',tys)
           end
         | Id => (es, [], tys)

   datatype phi_res = FIXphi of t * tyvar list * Type | NOFIXphi
   type phi = phi_res Lvars.Map.map
   infix ++
   fun phi1 ++ phi2 = Lvars.Map.plus(phi1,phi2)

   type vty = lvar * Type
   type function = {lvar : lvar, regvars: regvar list, tyvars : tyvar list,
                    Type : Type, constrs: constr list, vtys:vty list, body : LambdaExp}
   fun to_function {lvar,regvars,tyvars,Type,constrs,bind=FN{pat,body}} : function =
       {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,constrs=constrs,vtys=pat,body=body}
     | to_function _ = die "to_function"
   fun from_function ({lvar,regvars,tyvars,Type,constrs,vtys,body} : function) =
       {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,constrs=constrs,bind=FN{pat=vtys,body=body}}

   type accessSubst = LambdaExp -> LambdaExp

   fun flatten (F: {phi:phi, function:function, mutrec:bool} -> function * t * accessSubst)
               (phi:phi) (e:LambdaExp) : LambdaExp * phi =
       let val framephi : phi option ref = ref NONE
           fun flt phi e =
               case e of
                   FIX {functions,scope} =>
                   let val functions = map to_function functions
                       val mutrec = length functions > 1
                       val functions' = map (fn f => (F {phi=phi,function=f,mutrec=mutrec},#Type f)) functions
                       val phi' = Lvars.Map.fromList (map (fn (({lvar,tyvars,...},t,_),oldType) =>
                                                              (lvar,FIXphi(t,nil,oldType)))
                                                          functions')
                       val phi'' = Lvars.Map.fromList (map (fn (({lvar,tyvars,...},t,_),oldType) =>
                                                               (lvar,FIXphi(t,tyvars,oldType)))
                                                           functions')
                   in FIX{functions=map (fn (({lvar,regvars,tyvars,Type,constrs,vtys,body},t,aS),_) =>
                                            let val phi'2 = Lvars.Map.fromList (map (fn (lv,_) => (lv,NOFIXphi)) vtys)
                                                val () = if t <> Id then tick "flatten - fix" else ()
                                                val body = flt (phi++phi'++phi'2) body
                                            in from_function {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                                                              constrs=constrs,vtys=vtys,body=aS body}
                                            end)
                                        functions',
                          scope=flt (phi++phi'') scope}
                   end
                 | APP(e1 as VAR{lvar,instances,regvars},e,bopt) =>
                   let val es = case e of
                                    PRIM(UB_RECORDprim,es) => es
                                  | _ => [e]
                       val es = map (flt phi) es
                       fun mkApp es =
                           APP(e1,case es of [e] => e | _ => PRIM(UB_RECORDprim,es),bopt)

                       fun binds tys es =
                           case (tys,es) of
                               (nil,nil) => (fn x => x, nil)
                             | (ty::tys, e::es) =>
                               let val (G,es') = binds tys es
                               in case e of
                                      VAR _ => (G, e::es')
                                    | _ =>
                                      let val x = Lvars.newLvar()
                                          val () = if LambdaBasics.eq_Type(ty,f64Type) then Lvars.set_ubf64 x
                                                   else ()
                                      in (fn X => LET{pat=[(x,nil,ty)],bind=e,scope=G X},
                                          VAR{lvar=x,instances=nil,regvars=nil}::es')
                                      end
                               end
                             | _ => die "flatten.binds"
                   in case Lvars.Map.lookup phi lvar of
                          NONE => die "flt.look"
                        | SOME NOFIXphi => mkApp es
                        | SOME (FIXphi(t,tvs,ARROWtype(tys0,_,tys,_))) =>
                          if t = Id then mkApp es
                          else let val S = LambdaBasics.mk_subst (fn () => "OptLambda.flatten") (tvs,instances)
                                   val tys = map (LambdaBasics.on_Type S) tys
                                   val tys0 = map (LambdaBasics.on_Type S) tys0
                                   val (G,es') = binds tys0 es
                                   (*val () = print ("tys=" ^ pr_Types tys ^ "\n")*)
                                   val (es,vtys,_) = flatten_args t tys es'
                                   val () = if t <> Id then tick "flatten - app" else ()
                               in lams vtys (G(mkApp es))
                               end
                        | SOME _ => die "flt.look.no Arr"
                   end
                 | FN {pat,body} =>
                   let val phi' = Lvars.Map.fromList (map (fn (x,_) => (x,NOFIXphi)) pat)
                   in FN{pat=pat,body=flt (phi ++ phi') body}
                   end
                 | LET {pat,bind,scope} =>
                   let val phi' = Lvars.Map.fromList (map (fn (x,_,_) => (x,NOFIXphi)) pat)
                   in LET{pat=pat,bind=flt phi bind,scope=flt (phi ++ phi') scope}
                   end
                 | VAR {lvar,instances,...} =>
                   (case Lvars.Map.lookup phi lvar of
                        SOME NOFIXphi => e
                      | SOME (FIXphi (_,tvs,ARROWtype([ty],_,_,_))) =>
                        let val S = LambdaBasics.mk_subst (fn () => "OptLambda.flatten") (tvs,instances)
                            val ty = LambdaBasics.on_Type S ty
                            val x = Lvars.newLvar()
                            val body = APP(e,VAR{lvar=x,instances=nil,regvars=nil},NONE)
                        in flt phi (FN{pat=[(x,ty)],body=body})
                        end
                      | SOME (FIXphi _) => die ("flatten.VAR1: " ^ Lvars.pr_lvar lvar)
                      | NONE => e (*die ("flatten.VAR: " ^ Lvars.pr_lvar lvar)*) )
                 | FRAME{declared_lvars, declared_excons} =>
                   let val declared_lvar_ress =
                           map (fn {lvar,tyvars,Type} =>
                                   case Lvars.Map.lookup phi lvar of
                                       SOME (res as FIXphi(t,tvs,Type)) =>
                                       ({lvar=lvar,tyvars=tvs,Type=Type}, res)
                                     | SOME NOFIXphi => ({lvar=lvar,tyvars=tyvars,Type=Type}, NOFIXphi)
                                     | NONE => die "OptLambda.flatten.FRAME")
                               declared_lvars
                       val phi' = Lvars.Map.fromList (map (fn ({lvar,...},res) => (lvar,res))
                                                          declared_lvar_ress)
                   in ( framephi := SOME phi'
                      ; FRAME{declared_lvars=map #1 declared_lvar_ress,
                              declared_excons=declared_excons}
                      )
                   end
                 | _ => LambdaBasics.map_lamb (flt phi) e
       in (flt phi e,
           case !framephi of
               SOME phi => phi
             | NONE => die "flatten:FRAME not set"
          )
       end

   fun dropOpt (phi:phi) (e:LambdaExp) : LambdaExp * phi =
       let fun F {phi:phi,function={lvar,regvars,tyvars,Type,constrs,vtys,body},mutrec} =
               let val vs0 = if mutrec
                             then #1 (freevars body)
                             else #1 (freevars_not_call_invariant (lvar,map #1 vtys) body)
(*
                   val () = print ("Freevars '" ^ Lvars.pr_lvar lvar ^ "': " ^ String.concatWith ", " (map Lvars.pr_lvar vs0) ^ "\n")
*)
                   val (_,t,vtys) =
                       List.foldl (fn ((x,ty),(i,t,vtys)) =>
                                      if List.exists (fn y => Lvars.eq(x,y)) vs0
                                      then (i+1,t,(x,ty)::vtys)
                                      else (tick "flatten-dropOpt";
                                            (i+1,t oo Drop i,vtys)))
                                  (0,Id,nil) vtys
                   val Type = on_ty t Type
(*
                   val tvs_set = tyvars_Type TyvarSet.empty Type TyvarSet.empty
                   val tyvars = List.filter (fn tv => TyvarSet.member tv tvs_set) tyvars
*)
               in ({lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                    constrs=constrs,vtys=List.rev vtys,body=body},t,fn e => e)
               end
       in flatten F phi e
       end

   fun uncOpt (phi:phi) (e:LambdaExp) : LambdaExp * phi =
       let fun F {phi,function={lvar,regvars,tyvars,Type,constrs,vtys,body},mutrec} =
               let fun unc t vtys e : t * (lvar*Type)list * LambdaExp =
                       case e of
                           FN{pat=[(x,ty')],body} =>
                           if uncurrying_p() then (tick "flatten-uncOpt";
                                                   unc (Unc oo t) (vtys@[(x,ty')]) body)
                           else (t,vtys,e)
                         | _ => (t,vtys,e)
                   val (t,vtys,e) = unc Id vtys body
               in ({lvar=lvar,regvars=regvars,tyvars=tyvars,Type=on_ty t Type,
                    constrs=constrs,vtys=vtys,body=e},t,fn e => e)
               end
       in flatten F phi e
       end

   fun unbOpt (phi:phi) (e:LambdaExp) : LambdaExp * phi =
       let fun F {phi,function={lvar,regvars,tyvars,Type,constrs,vtys,body},mutrec} =
               if not(unbox_function_arguments())
               then ({lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                      constrs=constrs,vtys=vtys,body=body}, Id, fn e => e)
               else
               let fun ins (x,ty,i,j) a =
                       if List.exists (fn (y,_,i',j') => Lvars.eq(y,x) andalso i=i' andalso j=j') a
                       then a else (x,ty,i,j)::a
                   fun looki (m:(lvar*'a)list) (x:lvar) : (int * 'a) option =
                       let fun loop i nil = NONE
                             | loop i ((k,v)::xs) =
                               if Lvars.eq(x,k) then SOME(i,v)
                               else loop (i+1) xs
                       in loop 0 m
                       end

                   (* First clear all variables *)
                   fun clear_vars (e:LambdaExp) : unit =
                       LambdaBasics.app_lamb
                           (fn VAR{lvar,...} => Lvars.is_inserted lvar := false
                             | e => clear_vars e) e

                   (* Then collect and mark all non-select variables, that is,
                    * variables that are used but not in select contexts. Notice
                    * that, for unboxing variables that appear only in
                    * __real_to_f64 contexts, we treat __real_to_f64 as "select ~1"
                    *
                    * We shouldn't mark/collect a variable for the very reason that
                    * it occurs as a direct argument (of type real) to a call to
                    * lvar!
                    *
                    * Also, perhaps we shouldn't mark/collect a real-argument variable
                    * for the very reason that it occurs in a return (i.e., tail)
                    * position. The function LambdaBasics.app_lamb_tail may be used for
                    * this, but currently it is not applied; mael 2024-10-25...
                    *)

                   val non_selects = ref nil
                   fun collect_non_selects (e:LambdaExp) : unit =
                       LambdaBasics.app_lamb
                           (fn PRIM(SELECTprim _, [VAR _]) => ()
                             | PRIM(CCALLprim{name="__real_to_f64",...}, [VAR _]) => ()
                             | APP(g as VAR{lvar=lvar',...},arg,_) =>
                               if Lvars.eq(lvar,lvar') (* identified call to the function lvar; don't
                                                        * mark/collect direct argument variables... *)
                               then
                                 let fun test e lv =
                                         case looki vtys lv of
                                             SOME (_,ty) => if true (*eq_Type(realType,ty)*) then ()
                                                            else collect_non_selects e
                                           | NONE => ()
                                 in case arg of
                                        PRIM(UB_RECORDprim,args) =>
                                        List.app (fn e as VAR {lvar=lv,...} => test e lv
                                                 | e => collect_non_selects e) args
                                      | VAR {lvar=lv,...} => test arg lv
                                      | _ => collect_non_selects arg
                                 end
                               else (collect_non_selects g; collect_non_selects arg)
                             | VAR{lvar,...} =>
                               if !(Lvars.is_inserted lvar) then ()
                               else (Lvars.is_inserted lvar := true;
                                     non_selects := lvar :: !non_selects)
                             | e => collect_non_selects e) e
                   val candidates = ref nil
                   fun collect_candidates (e:LambdaExp) : unit =
                       let fun look x j =
                               case looki vtys x of
                                   SOME (i,ty) => if !(Lvars.is_inserted x) then () (* there are non-selects! *)
                                                  else candidates := ins (x,ty,i,j) (!candidates)
                                 | NONE => ()
                       in LambdaBasics.app_lamb
                              (fn PRIM(SELECTprim {index=j}, [VAR{lvar=x,...}]) => look x j
                                | PRIM (CCALLprim{name="__real_to_f64",...}, [VAR{lvar=x,...}]) => look x ~1
                                | e => collect_candidates e) e
                       end
                   fun appS S e =
                       let fun find x i =
                               case List.find (fn (y,j,_) => Lvars.eq(y,x) andalso i=j) S of
                                   SOME (_,_,y) => VAR{lvar=y,instances=nil,regvars=nil}
                                 | NONE => e
                       in case e of
                              PRIM(SELECTprim {index=i},[VAR{lvar=x,...}]) => find x i
                            | PRIM (CCALLprim{name="__real_to_f64",...}, [VAR{lvar=x,...}]) => find x ~1
                            | _ => LambdaBasics.map_lamb (appS S) e
                       end
                   fun tySel ty j =
                       case ty of
                           RECORDtype (tys,_) => (List.nth(tys,j)
                                                  handle _ => die "tySel.no j")
                         | _ => if j = ~1 andalso LambdaBasics.eq_Type(realType,ty)
                                then f64Type
                                else die "tySel.no tuple"
                   val () = clear_vars body
                   val () = collect_non_selects body
                   val () = collect_candidates body
               in (case !candidates of
                       nil => ({lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                                constrs=constrs,vtys=vtys,body=body}, Id, fn e => e)
                     | xs =>
                       let val (t,vtys,S:(lvar*int*lvar)list) =
                               List.foldl (fn ((x,ty,i,j),(t,vtys,S)) =>
                                              let val ty' = tySel ty j
                                                  val y = if j = ~1 andalso LambdaBasics.eq_Type(ty',f64Type)
                                                          then let val y = (*Lvars.new_named_lvar (Lvars.pr_lvar x ^ "_f64")*)
                                                                       Lvars.newLvar()
                                                               in Lvars.set_ubf64 y
                                                                ; y
                                                               end
                                                          else Lvars.newLvar()
                                                  val () = tick "flatten-unbOpt"
                                              in (Newprj(i,j) oo t,
                                                  vtys@[(y,ty')],
                                                  (x,j,y)::S)
                                              end) (Id,vtys,nil) xs
                       in ({lvar=lvar,regvars=regvars,tyvars=tyvars,Type=on_ty t Type,
                            constrs=constrs,vtys=vtys,body=body}, t, appS S)
                       end) before List.app (fn x => Lvars.is_inserted x := false) (!non_selects)
               end
       in flatten F phi e
       end

   fun phi_mod (phi,phi') =
       Lvars.Map.ComposeMap
           (fn (lv,res) =>
               case Lvars.Map.lookup phi' lv of
                   SOME res' =>
                   (case (res, res') of
                        (FIXphi(t,tvs,ty),FIXphi(t',_,_)) => FIXphi(t' oo t,tvs,ty)
                      | (NOFIXphi, _) => res'
                      | (_, NOFIXphi) => res)
                 | NONE => die "phi_mod1")
           phi

   fun phi_nofix phi =
       Lvars.Map.composemap (fn NOFIXphi => NOFIXphi
                              | FIXphi (t,tvs,ty) => NOFIXphi) phi

   fun flattening (phi:phi) (e:LambdaExp) : LambdaExp * phi =
       let val () = log "flattening\n"
           (* val () = prLambdaExp "Before uncOpt" e *)
           val (e,phi1) = uncOpt phi e
           val phi0 = phi_nofix phi
           (* val () = prLambdaExp "Before unbOpt" e *)
           val (e,phi2) = unbOpt phi0 e
           (* val () = prLambdaExp "Before dropOpt" e *)
           val (e,phi3) = dropOpt phi0 e
           (* val () = prLambdaExp "After dropOpt" e *)
       in (e, phi_mod(phi_mod(phi1,phi2),phi3))
       end

   (* Pickling *)
   val pu_t =
       let fun toInt t =
               case t of
                   Comp _ => 0
                 | Id => 1
                 | Newprj _ => 2
                 | Drop _ => 3
                 | Unc => 4
           fun fun_Comp pu =
               Pickle.con1 Comp (fn Comp p => p | _ => die "pu_t.Comp")
                           (Pickle.pairGen(pu,pu))
           val fun_Id = Pickle.con0 Id
           fun fun_Newprj _ =
               Pickle.con1 Newprj (fn Newprj p => p | _ => die "pu_t.Newprj")
                           (Pickle.pairGen(Pickle.int,Pickle.int))
           fun fun_Drop _ =
               Pickle.con1 Drop (fn Drop i => i | _ => die "pu_t.Drop")
                           Pickle.int
           val fun_Unc = Pickle.con0 Unc
       in
         Pickle.dataGen("OptLambda.t",toInt,[fun_Comp,fun_Id,fun_Newprj,
                                             fun_Drop,fun_Unc])
       end

   val pu_phi_res =
       let fun toInt (FIXphi _) = 0
             | toInt NOFIXphi = 1
           fun fun_FIXphi _ =
               Pickle.con1 FIXphi (fn FIXphi x => x | _ => die "pu_phi_res.FIXphi")
                           (Pickle.tup3Gen0(pu_t,LambdaExp.pu_tyvars,LambdaExp.pu_Type))
           val fun_NOFIXphi = Pickle.con0 NOFIXphi
       in Pickle.dataGen("OptLambda.phi_res",toInt,[fun_FIXphi,fun_NOFIXphi])
       end

   val pu_phi = LvarMap.pu Lvars.pu pu_phi_res

   (* Layout *)
   fun layout_phi_res (FIXphi(t,tyvars,Type)) = PP.LEAF ("FIXphi(" ^ pp_t t ^ ")")
     | layout_phi_res NOFIXphi = PP.LEAF "NOFIXphi"

   val layout_phi =
       LvarMap.layoutMap {start="Phi={", eq="->", sep=", ", finish="}"}
                         (PP.LEAF o Lvars.pr_lvar) layout_phi_res

   (* Enrichment and Restriction *)
   fun eq_phi_res (x,y) =
       case (x,y) of
           (NOFIXphi, NOFIXphi) => true
         | (FIXphi (t1,tvs1,ty1), FIXphi (t2,tvs2,ty2)) =>
           t1 = t2 andalso eq_sigma((tvs1,ty1),(tvs2,ty2))
         | _ => false

   fun enrich_phi (phi1,phi2) =
       LvarMap.enrich eq_phi_res (phi1,phi2)

   fun restrict_phi (phi,lvars) =
       LvarMap.restrict (Lvars.pr_lvar,phi,lvars)

   fun dummy_phi (e:LambdaExp) : phi =
       exports e |> #1
                 |> List.map (fn lv => (lv,NOFIXphi))
                 |> Lvars.Map.fromList

   end (* structure FixFlatten *)

   (* -----------------------------------------------------------------
    * table2d_simplify lamb: Rewrite primitive calls to word_table2d0 and
    * word_table2d0_init in terms of word_table0 and word_table0_init where the
    * first two memory slots are assigned the sizes of the two dimensions;
    * client code respects this layout; see basis/Array2.sml...
    * ----------------------------------------------------------------- *)

   local
     fun exec (e: LambdaExp) (scope: LambdaExp) : LambdaExp =
         let val lv = Lvars.newLvar()
         in LET{pat=[(lv,[],unit_Type)],
                bind=e,scope=scope}
         end
     fun assign tyvars aType instances a (i:int) e =
         let val iType = intDefaultType()
         in PRIM(CCALLprim{name="word_update0",instances=instances,tyvars=tyvars,
                           Type=ARROWtype([aType,iType,iType],NONE,[unit_Type],NONE)},
                 [a,INTEGER(IntInf.fromInt i,iType),e])
         end
   in
     fun table2d_simplify lamb =
         case lamb of
             PRIM(CCALLprim{name="word_table2d0",instances,tyvars,
                            Type=ARROWtype([iType,_,_],rv0,[aType],rv)},lambs) =>
             (case map table2d_simplify lambs of
                  [n,nr,nc] =>
                  let val lv = Lvars.newLvar()
                      val a = VAR{lvar=lv,instances=[],regvars=nil}
                      val e0 = assign tyvars aType instances a 0 nr
                      val e1 = assign tyvars aType instances a 1 nc
                      val S = mk_subst (fn () => "table2d_simplify.word_table2d0") (tyvars,instances)
                      val aType' = on_Type S aType
                  in LET{pat=[(lv,nil,aType')],
                         bind=PRIM(CCALLprim{name="word_table0",instances=instances,
                                             tyvars=tyvars,
                                             Type=ARROWtype([iType],rv0,[aType],rv)},[n]),
                         scope=exec e0 (exec e1 a)}
                  end
                | _ => die "table2d_simplify: word_table2d0")
           | PRIM(CCALLprim{name="word_table2d0_init",instances,tyvars,
                            Type=ARROWtype([iType,eType,_,_],rv0,[aType],rv)},lambs) =>
             (case map table2d_simplify lambs of
                  [n,e,nr,nc] =>
                  let val lv = Lvars.newLvar()
                      val a = VAR{lvar=lv,instances=[],regvars=nil}
                      val e0 = assign tyvars aType instances a 0 nr
                      val e1 = assign tyvars aType instances a 1 nc
                      val S = mk_subst (fn () => "table2d_simplify.word_table2d0_init") (tyvars,instances)
                      val aType' = on_Type S aType
                  in LET{pat=[(lv,nil,aType')],
                         bind=PRIM(CCALLprim{name="word_table_init",instances=instances,
                                             tyvars=tyvars,
                                             Type=ARROWtype([iType,eType],rv0,[aType],rv)},[n,e]),
                         scope=exec e0 (exec e1 a)}
                  end
                | _ => die "table2d_simplify: word_table2d0_init")
           | _ => map_lamb table2d_simplify lamb
   end


   (* -----------------------------------------------------------------
    * inverse_eta_for_fix_bound_lvars lamb
    *   ensure that every fix-bound variable is always fully applied, i.e.
    *   convert   fix f = ... f ... in ... f ...
    *        to   fix f = ... fn x => f x ... in ... fn x => f x ...
    *   This is to simplify the compilation of the region annotated lambda code.
    *   (It will not lead to unnecessary closures: a non-applied occurrence of
    *   a FIX bound variable will after region inference become f [rho1,..,rhon]
    *   and this shall evaluate to a closure anyway according to the semantics.)
    * ----------------------------------------------------------------- *)

    datatype inveta_res = FIXBOUND of tyvar list * Type
                        | NOTFIXBOUND
    type inveta_env = inveta_res LvarMap.map

    fun restrict_inv_eta_env (e:inveta_env,lvs) =
        LvarMap.restrict (Lvars.pr_lvar,e,lvs)

    fun new_sigma ([],tau) = ([],tau)
      | new_sigma (tyvars,tau) =
      let fun new_tv tv = if equality_tyvar tv then fresh_eqtyvar()
                          else fresh_tyvar()
          val tyvars' = map new_tv tyvars
          val S = mk_subst (fn () => "new_sigma") (tyvars,map (fn tv => TYVARtype {tv=tv}) tyvars')
      in (tyvars', on_Type S tau)
      end

    fun eq_sigma (([],tau1),([],tau2)) = eq_Type(tau1,tau2)
      | eq_sigma (sigma1 as (tyvars1,_),sigma2 as (tyvars2,_)) =
      List.length tyvars1 = List.length tyvars2 andalso
      let val (tyvars1,tau1) = new_sigma sigma1
          val (tyvars2,tau2) = new_sigma sigma2
          val S = mk_subst (fn () => "eq_sigma") (tyvars1,map (fn tv => TYVARtype {tv=tv}) tyvars2)
          val tau1' = on_Type S tau1
      in eq_Type(tau1',tau2)
      end

    fun eq_inveta_res (FIXBOUND sigma1, FIXBOUND sigma2) = eq_sigma(sigma1,sigma2)
      | eq_inveta_res (NOTFIXBOUND, NOTFIXBOUND) = true
      | eq_inveta_res _ = false

    fun enrich_inv_eta_env (e1,e2) =
        LvarMap.enrich eq_inveta_res (e1,e2)

    type StringTree = PP.StringTree

    fun layout_tyvarsXtau (tyvars,tau) =
      PP.NODE {start="[",finish="]",indent=2,childsep=PP.RIGHT ",",
               children=map (PP.LEAF o pr_tyvar) tyvars @ [layoutType tau]}

    fun layout_inveta_res (FIXBOUND sigma) = layout_tyvarsXtau sigma
      | layout_inveta_res NOTFIXBOUND = PP.LEAF "NOTFIXBOUND"

    val layout_inveta_env = LvarMap.layoutMap {start="InvEtaEnv={",eq="->", sep=", ", finish="}"}
      (PP.LEAF o Lvars.pr_lvar) layout_inveta_res

    val frame_inveta_env = ref (LvarMap.empty : inveta_env)

    fun inverse_eta_for_fix_bound_lvars env lamb =
      let
        fun inverse_eta env lamb =
          case lamb
            of VAR {lvar=lvar',instances=instances',regvars} =>
              (case LvarMap.lookup env lvar'
                 of SOME (FIXBOUND(tyvars,Type)) =>
                   let val subst = case instances' of [] => mk_subst (fn () => "inverse_eta") ([],[])
                                      | _ => mk_subst (fn () => "inverse_eta") (tyvars,instances')
                        (* The above case analysis caters for the fact that the
                         * instances may be empty, if this occurrence of lvar' is
                         * on the rhs of a val rec which declares lvar' *)
                       val lv = Lvars.newLvar()
                       val lv_e = VAR{lvar=lv,instances=[],regvars=[]}
                       val (pat,arg) =
                           case on_Type subst Type of
                               ARROWtype([tau],_,_,_) => ([(lv, tau)], lv_e)
                             | ARROWtype(taus,_,_,_) => die "inverse_eta - multi-args"
                             | _ => die "inverse_eta -- fix bound lvar of non-function type"
                   in FN{pat=pat, body=APP(lamb,arg,NONE)}
                   end
                  | _ => lamb)
             | LET{pat,bind,scope} =>
                 let val bind' = inverse_eta env bind
                     val lvars = map #1 pat
                     val env' = List.foldl(fn (lv,acc) => LvarMap.add(lv,NOTFIXBOUND,acc)) env lvars
                 in LET{pat=pat,bind=bind',scope=inverse_eta env' scope}
                 end
             | FIX{functions,scope} =>
              let val env' = List.foldr (fn ({lvar, tyvars, Type, ...},env) =>
                                         LvarMap.add(lvar, FIXBOUND(tyvars, Type), env)) env functions
              in FIX{functions=map (fn {lvar,regvars,tyvars,Type,constrs,bind} =>
                                   {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
                                    constrs=constrs,bind=inverse_eta env' bind}) functions,
                     scope=inverse_eta env' scope}
              end
             | APP(x as VAR _,lamb,_) => APP(x,inverse_eta env lamb,NONE)
             | FRAME {declared_lvars,...} =>
              let val env' = List.foldr (fn ({lvar,...},env') =>
                                         case LvarMap.lookup env lvar
                                           of SOME res => LvarMap.add(lvar,res,env')
                                            | NONE => die "inverse_eta.FRAME.lv not in env")
                             LvarMap.empty declared_lvars
              in frame_inveta_env := env'; lamb
              end
             | _ => map_lamb (inverse_eta env) lamb
      in
        (inverse_eta env lamb handle _ => reportBadLambda("inverse_eta",lamb),
          !frame_inveta_env)
      end

   (* -----------------------------------------------------------------
    * The Optimiser Engine
    * ----------------------------------------------------------------- *)

    fun optimise (ce:contract_env) (phi:FixFlatten.phi) e =
      let
          fun loop_opt1 n ce e =
              let val _ = reset_tick()
                  val (e, ce1) = contract ce e
                  val e = eliminate_explicit_records e
                  val e = eliminate_explicit_blockf64_bindings e
                  val e = cse e
                  val e = hoist_blockf64_allocations e
                  val _ = end_round() (*stat*)
              in if test_tick() andalso n < max_optimise
                 then loop_opt1 (n+1) ce e
                 else (e, ce1)
              end

          fun loop_opt2 n ce phi e =
              let val _ = reset_tick()
                  val e = fix_conversion e
                  (*val _ = prLambdaExp "Before flattening" e*)
                  val (e, phi1) = FixFlatten.flattening phi e
                  val e = unfix_conversion e
                  val (e, _) = contract ce e
                  val e = eliminate_explicit_records e
                  val e = eliminate_explicit_blockf64_bindings e
                  val e = cse e
                  val e = hoist_blockf64_allocations e
                  val _ = end_round() (*stat*)
              in if test_tick() andalso n < max_optimise
                 then let val (e,phi2) = loop_opt2 (n+1) ce (FixFlatten.phi_nofix phi) e
                          val phi3 = FixFlatten.phi_mod(phi1,phi2)
                      in (e, phi3)
                      end
                 else (e, phi1)
              end

          val _ = reset_statistics()
          val e1 = do_minimize_fixs e
          val (e2, ce') = loop_opt1 0 ce e1
          val (e3, phi') = loop_opt2 0 (unknown_env ce) phi e2
          val _ = print_stat e e3
      in (e3,ce',phi')
      end


   (* -----------------------------------------------------------------
    * The lambda optimiser environment
    * ----------------------------------------------------------------- *)

    type env = inveta_env * let_env * FixFlatten.phi * contract_env

    val empty =  (LvarMap.empty, LvarMap.empty, LvarMap.empty, LvarMap.empty)
    val initial = empty
    fun plus ((e1, e2, e3, e4), (e1', e2', e3', e4')) =
      (LvarMap.plus (e1,e1'), LvarMap.plus (e2,e2'),
       LvarMap.plus (e3,e3'), LvarMap.plus (e4,e4'))

    fun restrict ((inv_eta_env,let_env,phi,cenv), lvars, cons, tns) =
        let val (e4,lvs1,cons1,tns1) = restrict_contract_env(cenv,lvars)
            val lvars = lvs1 @ lvars
            val e3 = FixFlatten.restrict_phi(phi,lvars)
            val e1 = restrict_inv_eta_env(inv_eta_env,lvars)
            val e2 = restrict_let_env(let_env,lvars)
        in
          ((e1, e2, e3, e4), lvs1, cons1, tns1)
        end

    val debug_man_enrich = Flags.is_on0 "debug_man_enrich"

    fun debug (s, b) = if debug_man_enrich() then
                         (if b then print("\n" ^ s ^ ": enrich succeeded.")
                          else print("\n" ^ s ^ ": enrich failed."); b)
                       else b

    fun enrich ((inv_eta_env1,let_env1,phi1,cenv1): env,
                (inv_eta_env2,let_env2,phi2,cenv2): env) : bool =
      debug("inv_eta_env", enrich_inv_eta_env(inv_eta_env1,inv_eta_env2)) andalso
      debug("let_env", enrich_let_env(let_env1,let_env2)) andalso
      debug("phi", FixFlatten.enrich_phi(phi1,phi2)) andalso
      debug("contract_env", enrich_contract_env(cenv1,cenv2))

    fun layout_env (e1,e2,e3,e4) =
      PP.NODE{start="",finish="",indent=0,childsep=PP.RIGHT ",",
              children=[layout_inveta_env e1, layout_let_env e2,
                        FixFlatten.layout_phi e3,
                        layout_contract_env e4]}

   (* -----------------------------------------------------------------
    * Rewriting: This rewriting shall always be performed, no matter
    * whether the lamb is optimised or not, and it shall always be
    * performed after possibly optimisation
    * ----------------------------------------------------------------- *)

    fun rewrite (inveta_env,let_env) lamb =
        let val (lamb,let_env) = functionalise_let let_env lamb
            val lamb = fix_conversion lamb
            val (lamb,inveta_env) = inverse_eta_for_fix_bound_lvars inveta_env lamb
            val lamb = table2d_simplify lamb
        in (lamb, (inveta_env, let_env))
        end


   (* -----------------------------------------------------------------
    * The Optimiser
    * ----------------------------------------------------------------- *)

    fun maybeoptimise cenv phi e =
        if optimise_p() then optimise cenv phi e
        else (e, contract_env_dummy e, FixFlatten.dummy_phi e)

    fun optimise (env, PGM(DATBINDS db,lamb)) =
        let val (env1,env2,phi,cenv) = env
            val (lamb, cenv, phi) = maybeoptimise cenv phi lamb
            (*val _ = prLambdaExp "Before rewriting" lamb *)
            val (lamb, (env1,env2)) = rewrite (env1,env2) lamb
            val env = (env1,env2,phi,cenv)
            val () = warn_on_failed_inlines cenv
        in (PGM(DATBINDS db, lamb), env)
        end

    (* Pickler *)
    val pu =
        let val pu_iee_res =
                let fun to (Option.SOME s) = FIXBOUND s
                      | to NONE = NOTFIXBOUND
                    fun from (FIXBOUND s) = SOME s
                      | from NOTFIXBOUND = NONE
                in Pickle.convert (to,from) (Pickle.optionGen LambdaExp.pu_TypeScheme)
                end
            val pu_iee = LvarMap.pu Lvars.pu pu_iee_res
            val pu_uce =
                LvarMap.pu Lvars.pu (Pickle.optionGen(Pickle.pairGen(Pickle.int,LambdaExp.pu_TypeScheme)))
        in
            Pickle.convert (fn ((a,b),(c,d)) => (a,b,c,d), fn (a,b,c,d) => ((a,b),(c,d)))
            (Pickle.pairGen0(Pickle.pairGen0(Pickle.comment "OptLambda.iee.pu" pu_iee,
                                             Pickle.comment "OptLambda.let_env.pu" pu_let_env),
                             Pickle.pairGen0(Pickle.comment "OptLambda.phi.pu" FixFlatten.pu_phi,
                                             Pickle.comment "OptLambda.contract_env.pu" pu_contract_env)))
        end
  end
