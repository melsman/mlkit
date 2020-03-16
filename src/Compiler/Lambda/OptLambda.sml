(* OptLambda - lambda code optimiser. *)

structure OptLambda: OPT_LAMBDA =
  struct

    structure LvarDiGraphScc =
      DiGraphScc(struct
		     type nodeId = Lvars.lvar
		     type info = Lvars.lvar
		     type edgeInfo = unit
		     val lt = fn a => fn b => Lvars.lt(a,b)
		     fun getId lv = lv
		     val pu = Lvars.pu
		 end)

    structure PP = PrettyPrint

    structure EdList = Edlib.List

    structure LvarMap = Lvars.Map

    open LambdaExp LambdaBasics
    type bound_lvar = {lvar:lvar, tyvars:tyvar list, Type: Type}


   (* -----------------------------------------------------------------
    * Some Optimisation Constants
    * ----------------------------------------------------------------- *)

    val max_optimise = 20  (* maximal number of times the entire term is traversed. *)


   (* -----------------------------------------------------------------
    * Dynamic flags
    * ----------------------------------------------------------------- *)

    val optimise_p = Flags.add_bool_entry
	{long="optimiser",short=SOME "opt", menu=["Control", "Optimiser", "optimiser"],
	 item=ref true, neg=true, desc=
	 "Enable optimisation of intermediate language code\n\
	  \(Lambda Expressions). Which optimisations are performed\n\
	  \is controlled by individual flags. The optimisations\n\
	  \include function inlining, function specialisation,\n\
	  \fix-minimization, unboxing of function arguments, and\n\
	  \elimination of unnecessary record constructions."}

    val statistics_after_optimisation = Flags.add_bool_entry
	{long="statistics_after_optimisation", short=NONE,
	 menu=["Control", "Optimiser", "statistics after optimisation"],
	 item=ref false, neg=false, desc=
	 "Report optimisation statistics after optimisation of\n\
	  \Lambda Expression."}

    val minimize_fixs = Flags.add_bool_entry
	 {long="minimize_fixs", short=NONE, menu=["Control", "Optimiser", "minimize fixs"],
	  item=ref true, neg=true, desc=
	  "Minimize fix constructs (Lambda Expression Optimiser)."}

    val fix_conversion_ref = ref true  (* users should not be able to disable fix_conversion
					* because polymorphism in regions must co-work
					* with polymorphism in type variables *)

    val contract_p = Flags.add_bool_entry
	 {long="contract", short=NONE, menu=["Control", "Optimiser", "contract"],
	  item=ref true, neg=true, desc=
	  "Contract is responsible for inlining, specialization,\n\
	   \elimination of dead code, and much else (Lambda\n\
	   \Expression Optimiser)."}

    val specialize_recursive_functions = Flags.add_bool_entry
	  {long="specialize_recursive_functions", short=NONE,
	   menu=["Control", "Optimiser", "specialize recursive functions"],
	   item=ref true, neg=true, desc=
	   "Specialise recursive functions. Use the option\n\
	    \maximum_specialise_size to control which functions\n\
	    \are specialised. If this flag is on, functions that are\n\
	    \applied only once are specialised, no matter the setting\n\
	    \of maximum_specialise_size (Lambda Expression Optimiser)."}

    val eliminate_explicit_records_p = Flags.add_bool_entry
	   {long="eliminate_explicit_records", short=NONE,
	    menu=["Control", "Optimiser", "eliminate explicit records"],
	    item=ref true, neg=true, desc=
	    "Eliminate bindings of explicit records only used for\n\
	     \selections. Transform\n\
	     \      let r = (e1,...,en) in ... #i r .. #j r ...\n\
	     \into\n\
	     \      let x1=e1 in ... let xn=en in ... xi .. xj ...\n\
	     \(Lambda Expression Optimiser)."}

    val unbox_function_arguments = Flags.add_bool_entry
	    {long="unbox_function_arguments", short=NONE,
	     menu=["Control", "Optimiser", "unbox function arguments"],
	     item=ref true, neg=true, desc=
	     "Unbox arguments to fix-bound functions, for which the\n\
	      \argument `a' is used only in contexts `#i a'. All call \n\
	      \sites are transformed to match the new function (Lambda\n\
	      \Expression Optimiser)."}

    val unbox_reals = Flags.add_bool_entry
	    {long="unbox_reals", short=NONE,
	     menu=["Control", "Optimiser", "unbox real values"],
	     item=ref true, neg=true, desc=
	     "Unbox real values and computations on real values inside\n\
             \functions. Real values stored in data structures and\n\
             \passed to functions are still boxed."}

    (* max size of recursive function defs. to be specialised. *)
    val max_specialise_size = Flags.add_int_entry
	{long="maximum_specialise_size",short=NONE,
	 menu=["Control", "Optimiser", "maximum specialise size"],
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
	 menu=["Control", "Optimiser", "maximum inline size"],
	 item=ref 50, desc=
	 "Functions smaller than this size (counted in abstract\n\
	  \syntax tree nodes) are inlined, even if they are used\n\
	  \more than once. Functions that are used only once are\n\
	  \always inlined."}

    (* names of functions to inline no-matter the setting of the flag
     * maximum_inline_size. *)
    val inline_names = Flags.add_stringlist_entry
	{long="inline_names",short=NONE,
	 menu=["Control", "Optimiser", "inline names"],
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
	 menu=["Control", "Optimiser", "cross module optimisation"],
	 item=ref true,neg=true,
	 desc=
	 "Enable cross-module optimisation including inlining\n\
	  \of small functions and specialisation of small\n\
	  \recursive functions. Which optimisations are performed\n\
	  \across modules is controlled by individual optimisation\n\
	  \flags."}

    val aggressive_opt = Flags.add_bool_entry
	{long="aggresive_opt",short=SOME "aopt",
	 menu=["Control", "Optimiser", "aggressive optimisation"],
	 item=ref true,neg=false,
	 desc=
	 "Enable aggressive optimisations, including constant\n\
         \folding and aggressive inlining. These\n\
         \optimisations are not guaranteed to be region\n\
         \safe. Turning off garbage collection automatically\n\
         \turns off this option."}

   (* -----------------------------------------------------------------
    * Some helpful functions
    * ----------------------------------------------------------------- *)

    val unit_Type = RECORDtype []

    fun log x = if statistics_after_optimisation() then TextIO.output(!Flags.log,x)
		else ()

    fun die s = Crash.impossible ("OptLambda." ^ s)

    fun reportBadLambda(msg,lamb) =
      (PP.outputTree((fn s => TextIO.output(!Flags.log, s)),
		     LambdaExp.layoutLambdaExp lamb,
		     !Flags.colwidth);
       die ("optimisation failed (" ^ msg ^ ")"))

    fun prLambdaExp s e =
	(print ("\n" ^ s ^ ": \n");
	 PP.outputTree(print, LambdaExp.layoutLambdaExp e, 200);
	 print "\n")

    fun filter p [] = []
      | filter p (x::xs) = if p x then x::filter p xs else filter p xs

    fun foldl f a []      = a
      | foldl f a (x::xs) = foldl f (f a x) xs

    fun fst (a,_) = a
    and snd (_,b) = b

    fun size_lamb lamb = foldTD (fn s => fn _ => s + 1) 0 lamb

    fun lvars_fn_pat pat = map (fn (lv,_) => lv) pat
    fun lvars_let_pat pat = map (fn (lv,_,_) => lv) pat

    fun fn_to_let_pat [] = []
      | fn_to_let_pat ((lv,tau)::rest) = (lv,[],tau) :: fn_to_let_pat rest

    fun is_in_lv lv lvs = List.exists (fn lv' => Lvars.eq(lv,lv')) lvs
    fun is_in_ex ex exs = List.exists (fn ex' => Excon.eq(ex,ex')) exs

    val lexp_true = PRIM(CONprim{con=Con.con_TRUE,instances=nil,regvar=NONE},nil)
    val lexp_false = PRIM(CONprim{con=Con.con_FALSE,instances=nil,regvar=NONE},nil)

    (* Operations on unboxed floats *)

    local
      type exp = LambdaExp
      fun ccall name argtypes restype =
          CCALLprim {name=name,instances=[],tyvars=[],
		     Type=ARROWtype(argtypes,[restype])}
      fun f64_bin opr (x:exp,y:exp) : exp =
          PRIM(ccall ("__" ^ opr ^ "_f64") [f64Type,f64Type] f64Type, [x,y])
      fun f64_uno opr (x:exp) : exp =
          PRIM(ccall ("__" ^ opr ^ "_f64") [f64Type] f64Type, [x])
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
    end

   (* -----------------------------------------------------------------
    * Statistical functions
    * ----------------------------------------------------------------- *)

    local
      type stat_map = (string, int) FinMap.map
      fun pad m =
        let val l = FinMap.list m
            val ss = map #1 l
            val max = List.foldl (fn (s,max) => if String.size s > max then String.size s else max) 0 ss
            fun space 0 = ""
              | space n = " " ^ space (n-1)
            fun add_space s = s ^ space (max - String.size s)
            val ss' = map add_space ss
            val l' = BasisCompat.ListPair.zipEq (ss', map #2 l) handle _ => die "pad"
	    fun fromList [] = FinMap.empty
	      | fromList ((a,b)::rest) = FinMap.add(a,b,fromList rest)
        in fromList l'
        end
      val stat_map : stat_map ref = ref FinMap.empty
      val layout_stat = (FinMap.layoutMap {start="Optimiser Statistics:", eq=" : ", sep=", ", finish =""}
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
	in log ("ticks pr. round: " ^ p (rev (!tick_count_list)))
	end

    in
      fun reset_statistics() = (stat_map := FinMap.empty; tick_count_list := [0])
      fun add_statistics (s:string) =
	let val map = !stat_map
	    val i = case FinMap.lookup map s
		      of SOME i => i
		       | NONE => 0
	    val new_stat_map = FinMap.add(s, i+1, map)
	    val _ = incr_tick_counter()
	in (* log ("  " ^ s ^ "\n"); *) stat_map := new_stat_map
	end
      fun print_stat lamb lamb' =
	if not (statistics_after_optimisation()) then ()
	else let val st = layout_stat (!stat_map)
	     in PP.outputTree (log, st, !Flags.colwidth);
	        print_size_difference (size_lamb lamb) (size_lamb lamb');
		print_tick_count_list ()
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
      fun tick (s : string) = (flag := true;
			       if statistics_after_optimisation() then add_statistics s
			       else ())
      fun reset_tick() = flag := false
      fun test_tick() = !flag
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
		(RECORDprim NONE, RECORDprim NONE) => true
	      | (RECORDprim (SOME rv1), RECORDprim (SOME rv2)) => RegVar.eq(rv1,rv2)
	      | (SELECTprim i,SELECTprim i') => i=i'
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
	      | _ => false

	fun eq_sw eq_lamb0m eq (SWITCH(e,es,eo),SWITCH(e',es',eo')) =
	    eq_lamb0m (e,e')
	    andalso eqAll (fn ((a,e),(a',e')) => eq(a,a') andalso eq_lamb0m (e,e')) (es,es')
	    andalso eqOpt (eq_lamb0m) (eo,eo')

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
	  | eq_lamb0 m (SWITCH_W {switch=sw,precision=p}, SWITCH_W {switch=sw',precision=p'}) =
	    p = p' andalso eq_sw (eq_lamb0 m) (op =) (sw,sw')
	  | eq_lamb0 m (SWITCH_S sw, SWITCH_S sw') = eq_sw (eq_lamb0 m) (op =) (sw,sw')
	  | eq_lamb0 m (SWITCH_C sw, SWITCH_C sw') =
	    (* optional lvars are only used with Barry, where they are used to make pretty-printing prettier *)
	    eq_sw (eq_lamb0 m) (fn((c,lvo),(c',lvo')) => Con.eq(c,c') (* andalso eqOpt (eqLvars m) (lvo,lvo') *) ) (sw,sw')
	  | eq_lamb0 m (SWITCH_E sw, SWITCH_E sw') =
	    eq_sw (eq_lamb0 m) (fn((c,lvo),(c',lvo')) => Excon.eq(c,c') (* andalso eqOpt (eqLvars m) (lvo,lvo') *) ) (sw,sw')
	  | eq_lamb0 m (FN{pat,body},FN{pat=pat',body=body'}) =
	    (case eq_pat m (pat,pat') of
		 SOME m => eq_lamb0 m (body,body')
	       | NONE => false)
	  | eq_lamb0 m (LET{pat=[(lv,tvs,t)],bind=b,scope=s},LET{pat=[(lv',tvs',t')],bind=b',scope=s'}) =
		 length tvs = length tvs'
		 andalso
		 let
		     val tv_taus = map (fn _ => TYVARtype(fresh_tyvar())) tvs
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
(*
	  | eq_lamb0 m (FIX{functions=[{lvar=lv,tyvars=tvs,Type=t,bind=b}],scope=s},
			FIX{functions=[{lvar=lv',tyvars=tvs',Type=t',bind=b'}],scope=s'}) =
		 length tvs = length tvs'
		 andalso
		 let
		     val tv_taus = map (fn _ => TYVARtype(fresh_tyvar())) tvs
		     val S = mk_subst (fn () => "eq_lamb01") (tvs,tv_taus)
		     val S' = mk_subst (fn () => "eq_lamb02") (tvs',tv_taus)
		     val t = on_Type S t
		     val t' = on_Type S' t'
		 in eq_Type(t,t')
		     andalso
		     let
			 val b = on_LambdaExp S b
			 val b' = on_LambdaExp S' b'
		     in eq_lamb0 (LvarMap.add(lv,lv',m)) (b,b')
		     end
		 end
		 andalso eq_lamb0 (LvarMap.add(lv,lv',m)) (s,s')
*)
	  | eq_lamb0 _ _ = false
    in
	fun eq_lamb p = eq_lamb0 LvarMap.empty p
    end


   (* -----------------------------------------------------------------
    * lvar_in_lamb lvar lamb - Returns true, if there are any free
    * occurrences of the lvar in the LambdaExp lamb.
    * ----------------------------------------------------------------- *)

   fun lvar_in_lamb (lvar:lvar) (lamb:LambdaExp) =
     let
       exception Occurs
       fun occurs acc lamb =
         case lamb
           of VAR{lvar=lv,...} => if Lvars.eq(lvar,lv) then raise Occurs else false
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
		   | add ({lvar,regvars,tyvars,Type,bind}::fcs) b = add fcs (lvar::b)
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
     in (c nil nil lamb; true) handle OPEN => false
     end

   (* Function for determining if a variable v (of type real) occurs in an
    * expression e only in contexts of the form __real_to_f64(v)
    * given that e is *simple* (contains no function applications,
    * no handlers, switches, or functions. *)

   fun real_lvar_f64_in_lamb (lv:lvar) (lamb:LambdaExp) : bool =
     let exception Bad
         fun ok n =
             case n of
                 "__plus_real" => true
               | "__minus_real" => true
               | "__mul_real" => true
               | "__div_real" => true
               | "__max_real" => true
               | "__min_real" => true
               | "__f64_to_real" => true
               | "__real_to_f64" => true
               | "__plus_f64" => true
               | "__minus_f64" => true
               | "__mul_f64" => true
               | "__div_f64" => true
               | "__sqrt_f64" => true
               | "__neg_f64" => true
               | "__abs_f64" => true
               | _ => false
         fun look e =
             case e of
                 REAL _ => ()
               | F64 _ => ()
               | WORD _ => ()
               | INTEGER _ => ()
               | VAR{lvar,...} => if Lvars.eq(lvar,lv) then raise Bad else ()
               | PRIM(CCALLprim{name="__real_to_f64",...},[VAR _]) => ()
               | PRIM(CCALLprim{name,...},es) =>
                 if not(ok name) then raise Bad
                 else app look es
	       | LET{pat,bind,scope} => (look bind; look scope)
               | PRIM(SELECTprim _, es) => app look es
               | PRIM(RECORDprim _, es) => app look es
               | _ => raise Bad
     in
       (look lamb; true) handle Bad => false
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
               | _ => die "subst_real_lvar_f64_in_lamb: impossible"
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


   (* -----------------------------------------------------------------
    * Specialization of recursive functions is performed during
    * contract. Here we give two functions; one which decides if a
    * function is specializable and one that specializes an occurrence
    * of a specializable function.
    * ----------------------------------------------------------------- *)

   fun specializable {lvar=lv_f, regvars=[], tyvars, Type=ARROWtype([tau_1'],[ARROWtype([tau_2'],_)]),
		      bind=FN{pat=[(lv_x,tau_1)],body=FN{pat=[(lv_y,tau_2)],body}}} =
     let exception Fail
         fun app_f_x (APP(VAR{lvar=lv_f',...}, VAR{lvar=lv_x',...}, _)) =
	     if Lvars.eq(lv_f',lv_f) then if Lvars.eq(lv_x',lv_x) then () else raise Fail
	     else if Lvars.eq(lv_x',lv_f) then raise Fail else ()
	   | app_f_x (VAR{lvar,...}) = if Lvars.eq(lvar,lv_f) then raise Fail else ()
	   | app_f_x e = app_lamb app_f_x e
     in	eq_Type(tau_1,tau_1') andalso eq_Type(tau_2,tau_2') andalso
       ((app_f_x body; true) handle Fail => false)
     end
     | specializable _ = false

   fun subst_lvar_for_app lv (e as APP(lv_e as VAR{lvar,...},_,_)) =
        if Lvars.eq(lvar,lv) then lv_e
	else map_lamb (subst_lvar_for_app lv) e
     | subst_lvar_for_app lv e = map_lamb (subst_lvar_for_app lv) e

   fun subst_e_for_lvar lv e (e' as VAR{lvar,...}) = if Lvars.eq(lvar,lv) then e else e'
     | subst_e_for_lvar lv e e' = map_lamb (subst_e_for_lvar lv e) e'

   fun specialize_bind {lvar=lv_f, tyvars, Type=ARROWtype([tau_1],[ARROWtype([tau_2],[tau_3])]),
			bind=FN{pat=[(lv_x,_)],body=FN{pat=[(lv_y,_)],body}}}
                       instances lamb' =
     let val S = mk_subst (fn () => "specialize_bind") (tyvars, instances)
         val tau_2' = on_Type S tau_2
	 val tau_1' = on_Type S tau_1
         val tau = ARROWtype([tau_2'],[on_Type S tau_3])
	 val body' = subst_lvar_for_app lv_f body
	 val body'' = on_LambdaExp S body'
	 val scope = FIX{functions=[{lvar=lv_f,regvars=[],tyvars=[],Type=tau,
				     bind=FN{pat=[(lv_y,tau_2')],body=body''}}],
			 scope=VAR{lvar=lv_f,instances=[],regvars=[]}}
	 val e_0 = LET{pat=[(lv_x,[],tau_1')],bind=lamb',scope=scope}
     in new_instance e_0
     end
     | specialize_bind _ _ _ = die "specialize_bind"

   val tag_values = Flags.is_on0 "tag_values"

   fun simple_nonexpanding e =
       case e of
           VAR{instances=[],regvars=[],...} => true
         | INTEGER (_,t) => if tag_values() then eq_Type(t,int31Type) else true
         | F64 _ => true
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
	fun is_live_excon excon = List.exists (fn excon' => Excon.eq(excon, excon')) (!excon_bucket)
	fun mk_live_excon excon = if is_live_excon excon then () else add_excon_bucket excon
      end


      (* -----------------------------------------------------------------
       * Compile time values
       * ----------------------------------------------------------------- *)

      datatype cv = CVAR of LambdaExp
                  | CRECORD of cv list
                  | CUNKNOWN
                  | CCONST of LambdaExp
                  | CFN of {lexp: LambdaExp, large:bool}  (* only to appear in env *)
	          | CFIX of {Type: Type, bind: LambdaExp, large: bool} (* only to appear in env *)

      fun eq_cv (cv1,cv2) =
	case (cv1,cv2)
	  of (CVAR e1,CVAR e2) => eq_lamb(e1,e2)
	   | (CRECORD cvs1, CRECORD cvs2) => eq_cvs(cvs1,cvs2)
	   | (CUNKNOWN, CUNKNOWN) => true
	   | (CCONST e1, CCONST e2) => eq_lamb(e1,e2)
	   | (CFN{lexp,large}, CFN{lexp=lexp2,large=large2}) => large = large2 andalso eq_lamb(lexp,lexp2)
	   | (CFIX{bind,large,Type}, CFIX{bind=bind2,large=large2,Type=Type2}) =>
	    large = large2 andalso eq_Type(Type,Type2) andalso eq_lamb(bind,bind2)
	   | _ => false
      and eq_cvs (cv1::cvs1,cv2::cvs2) = eq_cv(cv1,cv2) andalso eq_cvs(cvs1,cvs2)
	| eq_cvs (nil,nil) = true
	| eq_cvs _ = false

      fun closed_small_cv (lvars_free_ok,excons_free_ok,lvar,tyvars,cv) : bool =
	case cv
	  of CVAR e1 => closed (lvars_free_ok, excons_free_ok,
				FN{pat=[(lvar,unitType)],body=e1})
	   | CRECORD cvs => (List.foldl (fn (cv,acc) => acc
					 andalso closed_small_cv(lvars_free_ok, excons_free_ok,lvar,tyvars,cv))
			     true cvs)
	   | CUNKNOWN => true
	   | CCONST e1 => true
	   | CFN{lexp,large} => (not large andalso
				 closed (lvars_free_ok, excons_free_ok,
					 FN{pat=[(lvar,unitType)],body=lexp}))
	   | CFIX{bind,Type,large} => (not large andalso
				       closed (lvars_free_ok, excons_free_ok,
					       FIX{functions=[{lvar=lvar,
                                                               regvars=[],       (* memo:regvars *)
							       tyvars=tyvars,
							       Type=Type,
							       bind=bind}],
						   scope=STRING("",NONE)}))

      (* remove lvar from compiletimevalue, if it is there;
       * used when compiletimevalues are exported out of scope.
       *)
      fun remove lvar (CRECORD l) = CRECORD(map (remove lvar) l)
	| remove lvar (cval as (CVAR (VAR{lvar =lvar',...}))) = if Lvars.eq(lvar,lvar') then CUNKNOWN else cval
	| remove _ (cval as (CCONST _)) = cval
	| remove _ _ = CUNKNOWN

      fun removes [] cv = cv
	| removes (lv::lvs) cv = removes lvs (remove lv cv)

      (* pretty printing *)
      fun show_cv (CVAR (VAR x)) = " cvar " ^ Lvars.pr_lvar (#lvar x)
	| show_cv (CVAR _) = "<not possible>"
	| show_cv (CRECORD l) = concat ("[" :: (map show_cv l @ ["]"]))
	| show_cv (CCONST l) = "const"
	| show_cv (CFN {large=true,...}) = "(large fn)"
	| show_cv (CFN {large=false,...}) = "(small fn)"
	| show_cv (CFIX {large=true,...}) = "(large fix)"
	| show_cv (CFIX {large=false,...}) = "(small fix)"
	| show_cv (CUNKNOWN) = "(unknown)"

      (* substitution *)
      fun on_cv S cv =
	let fun on (CVAR lamb) = CVAR (on_LambdaExp S lamb)
	      | on (cv as CCONST _) = cv
	      | on (CRECORD cvs) = CRECORD (map on cvs)
	      | on (CFN{lexp,large}) = CFN{lexp=on_LambdaExp S lexp,large=large}
	      | on (CFIX{Type,bind,large}) = CFIX{Type=on_Type S Type,bind=on_LambdaExp S bind,large=large}
	      | on _ = CUNKNOWN
	in on cv
	end

      fun eq_cv_scheme ((tvs1,cv1),(tvs2,cv2)) =
	length tvs1 = length tvs2 andalso
	let val S = mk_subst (fn () => die "eq_cv_scheme") (tvs1, map TYVARtype tvs2)
	in eq_cv(on_cv S cv1,cv2)
	end

      (* least upper bound *)
      fun lub (cv as CVAR lamb,CVAR lamb') = if eq_lamb(lamb,lamb') then cv else CUNKNOWN
	| lub (CRECORD cvals,CRECORD cvals') = (CRECORD (map lub (BasisCompat.ListPair.zipEq(cvals,cvals')))
						handle BasisCompat.ListPair.UnequalLengths => die "lub")
	| lub (cv as CCONST lamb,CCONST lamb') = if eq_lamb(lamb,lamb') then cv else CUNKNOWN
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
	    | CFIX{large=false,Type,bind} =>
		  PP.NODE{start="(small fix: ", finish=")",
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


      fun cross_module_inline (lvars_free_ok, excons_free_ok) lvar (tyvars,cv) =
	cross_module_opt()
	andalso closed_small_cv(lvars_free_ok,excons_free_ok,lvar,tyvars,cv)

      val frame_contract_env : env option ref = ref NONE

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

(*
      fun is_small_closed_fn lamb =
	case lamb
	  of FN _ => small_lamb (!max_inline_size) lamb andalso closed lamb
	   | _ => false
*)
      fun is_inlinable_fn lvar lamb =
	  case lamb of
              FN _ => always_inline_function lvar orelse small_lamb (max_inline_size()) lamb
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
(*
                    | PRIM(p,arg::args) =>
                      (case build arg of
                         SOME(f,instances) => SOME(fn x => PRIM(p,f x :: args), instances)
                       | NONE => NONE)
*)
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

      fun selectorCon (e : LambdaExp) : ((con*lvar option)->bool)option =
          if aggressive_opt() then
            case e of
              PRIM(CONprim {con,...}, args) =>
              if List.all safeLambdaExp args then SOME (fn (c,_) => Con.eq(c,con))
              else NONE
            | _ => NONE
          else NONE

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

      fun reduce_switch (reduce, env, (fail as (_,cv)), (SWITCH(arg, sel, opt)), selector) =  (* If branches are equal and the selector *)
	let fun allEqual [] = true                                                            (* is safe then eliminate switch. *)
	      | allEqual [x] = true
	      | allEqual (x::(ys as y::_)) = eq_lamb(x,y) andalso allEqual ys
          fun constFold() =
              case selector arg of
                SOME sel_eq =>
                let val (e, others) = searchSel sel_eq sel opt
                in tick "reduce - switch constant fold";
                   app decr_uses others;
                   reduce (env, (e, cv))
                end
              | NONE => fail

	in case opt
	     of SOME lamb =>
	       if safeLambdaExp arg andalso allEqual (lamb::(map snd sel)) then
		 (tick "reduce - switch"; decr_uses arg; app (decr_uses o snd) sel; reduce (env, (lamb, cv)))
	       else constFold()
	      | NONE =>
		 if safeLambdaExp arg andalso allEqual (map snd sel) then
		   case sel
		     of (_,lamb)::sel' => (tick "reduce - switch"; decr_uses arg;
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
	case lamb
	  of INTEGER (v,t) => if tag_values() then not(eq_Type(t, int32Type))
			      else true
	   | WORD (v,t) => if tag_values() then not(eq_Type(t, word32Type))
			   else true
	   | PRIM(CONprim {con,...},nil) => is_boolean con
	   | _ => false

      fun constantFolding lamb fail =
          if not(aggressive_opt()) then fail
          else
            let val opt =
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
                       | [INTEGER(v1,_),INTEGER(v2,_)] =>
                         let fun opp opr = SOME(if opr(v1,v2) then lexp_true else lexp_false)
                         in case name of
                              "__less_int31" => opp (op <)
                            | "__less_int32b" => opp (op <)
                            | "__less_int32ub" => opp (op <)
                            | "__lesseq_int31" => opp (op <=)
                            | "__lesseq_int32b" => opp (op <=)
                            | "__lesseq_int32ub" => opp (op <=)
                            | "__greater_int31" => opp (op >)
                            | "__greater_int32b" => opp (op >)
                            | "__greater_int32ub" => opp (op >)
                            | "__greatereq_int31" => opp (op >=)
                            | "__greatereq_int32b" => opp (op >=)
                            | "__greatereq_int32ub" => opp (op >=)
                            | "__equal_int31" => opp (op =)
                            | "__equal_int32b" => opp (op =)
                            | "__equal_int32ub" => opp (op =)
                            | _ => NONE
                         end
                       | [WORD(v1,t),WORD(v2,_)] =>
                         let fun opp opr = SOME(if opr(v1,v2) then lexp_true else lexp_false)
                         in case name of
                              "__less_word31" => opp (op <)
                            | "__less_word32b" => opp (op <)
                            | "__less_word32ub" => opp (op <)
                            | "__lesseq_word31" => opp (op <=)
                            | "__lesseq_word32b" => opp (op <=)
                            | "__lesseq_word32ub" => opp (op <=)
                            | "__greater_word31" => opp (op >)
                            | "__greater_word32b" => opp (op >)
                            | "__greater_word32ub" => opp (op >)
                            | "__greatereq_word31" => opp (op >=)
                            | "__greatereq_word32b" => opp (op >=)
                            | "__greatereq_word32ub" => opp (op >=)
                            | "__equal_word" => opp (op =)
                            | "__equal_word31" => opp (op =)
                            | "__equal_word32b" => opp (op =)
                            | "__equal_word32ub" => opp (op =)
                            | "__andb_word" => SOME(WORD(Word32.andb(v1,v2),t))
                            | "__andb_word31" => SOME(WORD(Word32.andb(v1,v2),t))
                            | "__andb_word32b" => SOME(WORD(Word32.andb(v1,v2),t))
                            | "__andb_word32ub" => SOME(WORD(Word32.andb(v1,v2),t))
                            | "__orb_word" => SOME(WORD(Word32.orb(v1,v2),t))
                            | "__orb_word31" => SOME(WORD(Word32.orb(v1,v2),t))
                            | "__orb_word32b" => SOME(WORD(Word32.orb(v1,v2),t))
                            | "__orb_word32ub" => SOME(WORD(Word32.orb(v1,v2),t))
                            | _ => NONE
                         end
                       | _ => NONE)
                    | _ => NONE
            in case opt of
                 SOME lamb => (lamb,CCONST lamb)
               | NONE => fail
            end

      fun reduce_f64bin f64binop (e1,e2) =
          (tick "real_to_f64";
           (f64_to_real (f64binop (real_to_f64 e1, real_to_f64 e2)), CUNKNOWN))

      fun reduce_f64uno f64unop x =
          (tick "real_to_f64";
           (f64_to_real (f64unop (real_to_f64 x)), CUNKNOWN))

      fun reduce (env, (fail as (lamb,cv))) =
	case lamb
	  of VAR{lvar,instances,regvars=[]} =>
	    ((*output(!Flags.log, Lvars.pr_lvar lvar ^ ":" );*)
             case lookup_lvar(env,lvar)
	       of SOME (tyvars,cv) =>
		 ((*output(!Flags.log, show_cv cv ^ "\n");*)
                  case cv
		    of CFN {lexp=lamb',large} =>
		      if large andalso not(Lvars.one_use lvar) then (lamb, CVAR lamb)
		      else let val S = mk_subst (fn () => "reduce1") (tyvars, instances)
			       val _ = decr_use lvar
			       val lamb'' = new_instance lamb'
			       val _ = incr_uses lamb''
			       val _ = if large then tick "reduce - inline-largefn"
				       else tick "reduce - inline-smallfn"
			   in (on_LambdaExp S lamb'', CVAR lamb)    (* reduce(env,...) *)
			   end
		     | CVAR (lamb' as VAR{lvar=lvar',instances=instances',regvars=[]}) =>
			   let val S = mk_subst (fn () => "reduce2") (tyvars,instances)
			       val _ = decr_use lvar
			       val _ = incr_use lvar'
			       val lamb'' = on_LambdaExp S lamb'
			   in if Lvars.eq(lvar,lvar') then (lamb'', CVAR lamb'')
			      else (tick "reduce - inline-var"; (lamb'', CVAR lamb'')) (*reduce (env, (lamb'', CVAR lamb''))*)
			   end
		     | CCONST lamb' =>
			   if is_unboxed_value lamb' orelse (aggressive_opt() andalso small_const lamb') then
			     (decr_use lvar; tick "reduce - inline-unboxed-value"; (lamb', cv))
			   else if Lvars.one_use lvar then
			     (decr_use lvar; tick "reduce - inline-const"; (lamb', cv))
			   else (lamb, CVAR lamb)
		     | CUNKNOWN => (lamb, CVAR lamb)
		     | _ => let val S = mk_subst (fn () => "reduce3") (tyvars,instances)
			    in (lamb, on_cv S cv)
			    end)
		| NONE => ((*output(!Flags.log, "none\n");*) (lamb, CVAR lamb)))
	   | VAR _ => fail (* explicit region parameters *)
	   | INTEGER _ => (lamb, CCONST lamb)
	   | WORD _ => (lamb, CCONST lamb)
	   | PRIM(CONprim {con,...},[]) => if is_boolean con orelse aggressive_opt() then (lamb, CCONST lamb)
					   else fail
	   | STRING _ => (lamb, CCONST lamb)
	   | REAL _ => (lamb, CCONST lamb)
	   | F64 _ => (lamb, CCONST lamb)
	   | LET{pat=[(lvar,tyvars,tau)],bind,scope} =>
	       let
                 (* maybe let-float f64-binding outwards to open up for other optimisations *)
                 val (lvar,tyvars,tau,bind,scope,fail) =
                     case (tyvars,tau) of
                         ([],RECORDtype (_ :: _ :: _)) =>
                         (case bind of
                              LET{pat=[(lv,[],tau')],bind=bind',scope=scope'} =>
                                 if unbox_reals() (*andalso eq_Type(tau',f64Type)*) andalso simple_nonexpanding bind' then
                                   (tick "reduce - let-floating";
                                    let val scope'' = LET{pat=[(lvar,[],tau)],bind=scope',
                                                          scope=scope}
                                    in (lv,[],tau',bind',scope'',
                                        (LET{pat=[(lv,[],tau')],bind=bind',scope=scope''},
                                         CUNKNOWN))
                                    end)
                                 else (lvar,tyvars,tau,bind,scope,fail)
                            | _ => (lvar,tyvars,tau,bind,scope,fail))
                         | _ => (lvar,tyvars,tau,bind,scope,fail)
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
			  | SWITCH_W {switch,precision} =>
			       do_sw (fn sw => SWITCH_W {switch=sw,precision=precision}) switch
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
	  | PRIM(SELECTprim n,[lamb]) =>
	       let fun do_select () =
		      case cv
			of CRECORD cvs =>
			  let val nth_cv = List.nth(cvs,n)
			    handle Subscript => die "reduce4"
			  in case nth_cv
			       of CVAR var => (tick "reduce - sel-var"; decr_uses lamb;
					       incr_uses var; reduce (env, (var,nth_cv)))
				| CCONST(e as INTEGER _) => (tick "reduce - sel-int";
							     decr_uses lamb; (e, nth_cv))
				| CCONST(e as WORD _) => (tick "reduce - sel-word";
							  decr_uses lamb; (e, nth_cv))
				| _ => (lamb, nth_cv)
			  end
			 | _ => fail
	       in case lamb
		    of PRIM(RECORDprim _,lambs) =>
		      let val (lamb', lambs') = EdList.removeNth n lambs
		      in if safeLambdaExps lambs' then
			   (tick "reduce - sel-record"; app decr_uses lambs';
			    reduce(env, (lamb', CUNKNOWN)))
			 else do_select()
		      end
		     | _ => do_select()
	       end
	  | FIX{functions,scope} =>
	       let val lvs = map #lvar functions
	       in if zero_uses lvs then (tick "reduce - dead-fix";
					 app (decr_uses o #bind) functions;
					 reduce (env, (scope,cv)))
		  else case functions
			 of [function as {lvar,regvars=[],tyvars,Type,bind}] =>
			   if single_arg_fn bind andalso not(lvar_in_lamb lvar bind) then
			     (tick "reduce - fix-let";
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
	  | APP(VAR{lvar,instances,regvars=[]}, lamb2, _) =>
	       (case lookup_lvar(env, lvar)
		  of SOME (tyvars, CFIX{Type,bind,large}) =>
		    if not(large) orelse Lvars.one_use lvar then
		      let val e = specialize_bind {lvar=lvar,tyvars=tyvars,Type=Type,bind=bind} instances lamb2
		      in decr_use lvar; decr_uses lamb2; incr_uses e; tick ("reduce - fix-spec." ^ Lvars.pr_lvar lvar);
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
	  | SWITCH_W {switch,precision} => reduce_switch (reduce, env, fail, switch, selectorNONE)
	  | SWITCH_S switch => reduce_switch (reduce, env, fail, switch, selectorNONE)
	  | SWITCH_C switch => reduce_switch (reduce, env, fail, switch, selectorCon)
	  | SWITCH_E switch => reduce_switch (reduce, env, fail, switch, selectorNONE)
          | PRIM(CCALLprim{name="__real_to_f64",...}, [REAL(s,_)]) =>
            (tick "real immed to f64 immed";
             reduce (env,(F64 s,CUNKNOWN)))
          | PRIM(CCALLprim{name="__real_to_f64",...},
                 [PRIM(CCALLprim{name="__f64_to_real",...},[x])]) =>
            (tick "real unbox o box elimination";
             reduce (env,(x,CUNKNOWN)))
          | PRIM(CCALLprim{name="__real_to_f64",...},
                 [LET{pat,bind,scope=PRIM(CCALLprim{name="__f64_to_real",...},[scope])}]) =>
            (tick "real unbox o box elimination - let";
             reduce (env,(LET{pat=pat,bind=bind,scope=scope},CUNKNOWN)))
          | PRIM(CCALLprim{name,...},xs) =>
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
		                                 Type=ARROWtype([intDefaultType()],[f64Type])},
                                      [x])), CUNKNOWN))
                | _ => constantFolding lamb fail
            else constantFolding lamb fail
	  | _ => constantFolding lamb fail


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
		               (map (fn lvar => ([], CVAR (VAR{lvar=lvar,instances=[],regvars=[]}))) lvars) env
		   val (body',_) = contr (env', body)
	       in (FN{pat=pat,body=body'},CUNKNOWN)
	       end
	      | LET{pat=(pat as [(lvar,tyvars,tau)]),bind,scope} =>
	       let val (bind', cv) = contr (env, bind)
		   val cv' = if (*is_small_closed_fn*) is_inlinable_fn lvar bind' then CFN{lexp=bind',large=false}
			     else if is_fn bind' then CFN{lexp=bind',large=true}
                             else if is_unboxed_value bind' then CCONST bind'
			     else (case bind'
				     of VAR _ => CVAR bind'
				      | _ => cv)
		   val env' = LvarMap.add(lvar,(tyvars,cv'),env)
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
	      | PRIM(prim,lambs) => (PRIM(prim,map (fst o (fn e => contr (env,e))) lambs),CUNKNOWN)
	      | FIX{functions,scope} =>
	       let val lvs = map #lvar functions
		   val env0 = updateEnv lvs (map (fn _ => ([],CUNKNOWN)) functions) env
		   val _ = app mark_lvar lvs
		   val functions' = map (fn {lvar,regvars,tyvars,Type,bind} =>
					 {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
					  bind=fst (contr (env0, bind))}) functions
		   val _ = app unmark_lvar lvs
		   val env' = case functions
				of [function as {lvar,regvars=[],tyvars,Type,bind}] =>  (* memo:regvars *)
				   let val cv = if specializable function andalso specialize_recursive_functions() then
				                  CFIX{Type=Type,bind=bind,large=not(small_lamb (max_specialise_size()) bind)}
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
	      | SWITCH_W {switch,precision} =>
	       contr_switch (contr, reduce, env, fn sw => SWITCH_W {switch=sw, precision=precision}, switch)
	      | SWITCH_S switch => contr_switch (contr, reduce, env, SWITCH_S, switch)
	      | SWITCH_C switch => contr_switch (contr, reduce, env, SWITCH_C, switch)
	      | SWITCH_E switch =>
	       let val res = contr_switch (contr, reduce, env, SWITCH_E, switch)
		   val SWITCH(_,sel,_) = switch
		   fun mklive [] = ()
		     | mklive (((excon,_),_)::rest) = (mk_live_excon excon; mklive rest)
	       in mklive sel; res
	       end
	      | FRAME{declared_excons,declared_lvars} =>
	       let val lvars = map #lvar declared_lvars
		   val excons = map #1 declared_excons
		   val env' =
		     List.foldl (fn (lv,acc) =>
				 case LvarMap.lookup env lv
				   of SOME res =>
				     if cross_module_inline (lvars,excons) lv res
				       then LvarMap.add(lv,res,acc)
				     else LvarMap.add(lv,(nil,CUNKNOWN),acc)
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
	LvarMap.Fold (fn ((lv2,res2),b) => b andalso
		      case LvarMap.lookup ce1 lv2
			of SOME res1 => eq_cv_scheme(res1,res2)
			 | NONE => false) true ce2

      fun free_contract_env_res(res,cons,tns) = (cons,tns)

      fun restrict_contract_env (ce,lvars,cons,tns) =
	List.foldl (fn (lv,(e,cons,tns)) =>
		    case LvarMap.lookup ce lv
		      of SOME res =>
			let val (cons,tns) = free_contract_env_res(res,cons,tns)
			in (LvarMap.add(lv,res,e),cons,tns)
			end
		       | NONE => die "restrict_contract_env.lv not in env")
	(LvarMap.empty,cons,tns) lvars

      val layout_contract_env : contract_env -> StringTree =
	LvarMap.layoutMap {start="ContractEnv={",eq="->", sep=", ", finish="}"}
	(PP.LEAF o Lvars.pr_lvar') layout_cv_scheme

      val pu_contract_env =
	  let fun toInt (CVAR _) = 0
		| toInt (CRECORD _) = 1
		| toInt CUNKNOWN = 2
		| toInt (CCONST _) = 3
		| toInt (CFN _) = 4
		| toInt (CFIX _) = 5

	      fun fun_CVAR _ =
		  Pickle.con1 CVAR (fn CVAR a => a | _ => die "pu_contract_env.CVAR")
		  LambdaExp.pu_LambdaExp
	      fun fun_CRECORD pu =
		  Pickle.con1 CRECORD (fn CRECORD a => a | _ => die "pu_contract_env.CRECORD")
		  (Pickle.listGen pu)
	      val fun_CUNKNOWN = Pickle.con0 CUNKNOWN
	      fun fun_CCONST _ =
		  Pickle.con1 CCONST (fn CCONST a => a | _ => die "pu_contract_env.CCONST")
		  LambdaExp.pu_LambdaExp
	      fun fun_CFN _ =
		  Pickle.con1 CFN (fn CFN a => a | _ => die "pu_contract_env.CFN")
		  (Pickle.convert (fn (e,l) => {lexp=e,large=l}, fn {lexp=e,large=l} => (e,l))
		   (Pickle.pairGen0(LambdaExp.pu_LambdaExp,Pickle.bool)))
	      fun fun_CFIX _ =
		  Pickle.con1 CFIX (fn CFIX a => a | _ => die "pu_contract_env.CFIX")
		  (Pickle.convert (fn (t,e,l) => {Type=t,bind=e,large=l},
				   fn {Type=t,bind=e,large=l} => (t,e,l))
		   (Pickle.tup3Gen0(LambdaExp.pu_Type,LambdaExp.pu_LambdaExp,Pickle.bool)))
	      val pu_cv =
		  Pickle.dataGen("OptLambda.cv",toInt,[fun_CVAR,fun_CRECORD,fun_CUNKNOWN,
						       fun_CCONST,fun_CFN,fun_CFIX])
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
	  of PRIM(SELECTprim i, [VAR{lvar,instances=[],regvars=[]}]) =>
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
                                 of RECORDtype taus => taus
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
(*	  log " traversing\n"; *)
	  traverse lamb;
(*	  log " transformation\n"; *)
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
     type fs = {lvar:lvar,regvars:RegVar.regvar list,tyvars:tyvar list,Type:Type,bind:LambdaExp}
     fun mk_nodes(functions,G) =
       let fun mn ([]:fs list) = ()
	     | mn (f::fs) = (DG.addNode (DG.mkNode(#lvar f)) G; mn fs)
       in mn functions
       end
     fun mk_edges(functions,G) =
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
     fun compute_scc(functions,G) =
       let val node_list_list = DG.scc G
	   val lv_list_list = map (map (! o DG.getInfoNode)) node_list_list
	   val f_list_list = map (map (find_lv_functions functions)) lv_list_list

(*	   fun log s = output(!Flags.log, s)
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
       let fun on_f (IS : lvar -> Type list option) ({lvar,regvars,tyvars,Type,bind}:fs) : fs =
	     let fun on_bind (lamb as VAR{lvar,instances=[],regvars=[]}) =
	           (case IS(lvar)
		      of SOME instances => VAR{lvar=lvar,instances=instances,regvars=[]}
		       | NONE => lamb)
		   | on_bind lamb = map_lamb on_bind lamb
	     in {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,bind=on_bind bind}
	     end
	   fun extend_IS IS c =
	     let fun ext [] lv = IS(lv)
		   | ext (({lvar,tyvars,...}:fs)::c) lv = if Lvars.eq(lvar,lv) then SOME (map TYVARtype tyvars)
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
		      | add (tv::tvs) tyvars = if ListUtils.member tv tyvars then add tvs tyvars
					       else add tvs (tv::tyvars)
		in add tyvars (get_tyvars c tyvars')
		end
	    fun fresh_tv tv = if equality_tyvar tv then fresh_eqtyvar ()
			      else fresh_tyvar ()

	    fun on_tyvar S tv =
	      case on_Type S (TYVARtype tv)
		of TYVARtype tv' => tv'
		 | _ => die "on_tyvar"

	    fun on_c S [] = []   (* memo:regvars *)
	      | on_c S (({lvar,regvars,tyvars,Type,bind}:fs)::c) =
	      let val tyvars' = map (on_tyvar S) tyvars
		  val Type' = on_Type S Type
		  val bind' = on_LambdaExp S bind
	      in {lvar=lvar,regvars=regvars,tyvars=tyvars',Type=Type',bind=bind'}::on_c S c
	      end

	    val tyvars = get_tyvars c []
	    val types = map (TYVARtype o fresh_tv) tyvars
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
                 (tick "fix conversion";
		  FIX{functions=[{lvar=lvar,regvars=[],tyvars=tyvars,Type=Type,bind=bind}],
		      scope=scope})
	   | f lamb = lamb
     in
       if !fix_conversion_ref then (log "fix_conversion\n"; passTD f lamb)
       else lamb
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

     fun enrich_let_env(let_env1,let_env2) =
       LvarMap.Fold (fn ((lv2,res2),b) => b andalso
		       case LvarMap.lookup let_env1 lv2
			 of SOME res1 => res1=res2
			  | NONE => false) true let_env2

     fun restrict_let_env(let_env,lvars) =
       List.foldl (fn (lv,acc) =>
		   case LvarMap.lookup let_env lv
		     of SOME res => LvarMap.add(lv,res,acc)
		      | NONE => die "restrict_let_env.lv not in env") LvarMap.empty lvars

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
	      of SOME DELAY_SIMPLE => APP(v, PRIM(RECORDprim NONE, []), NONE)
	       | _ => v)
	  | LET{pat,bind,scope} =>
	      (case pat
		 of [(lvar,tyvars,Type)] =>
	          if null(tyvars)  then
		    LET{pat=pat,bind=f env bind, scope=f (add_lv(lvar,IGNORE,env)) scope}
		  else (case bind of
                          FN _ => (* already a lambda abstraction; make no new abstraction *)
                            LET{pat=pat,bind=f env bind, scope=f (add_lv(lvar,IGNORE,env)) scope}
                        | non_expansive_bind =>
                            (* make lambda abstraction *)
                            let val Type' = ARROWtype([unit_Type], [Type])
                                     val pat' = [(lvar,tyvars,Type')]
                                     val bind' = FN{pat=[(Lvars.newLvar(),unit_Type)],body=f env bind}
                                     val scope' = f (LvarMap.add(lvar,DELAY_SIMPLE,env)) scope
                            in LET{pat=pat',bind=bind',scope=scope'}
                            end
                          )
		  | nil => LET{pat=pat,bind=f env bind, scope=f env scope}
		  | _ => die "functionalise_let. non-trivial patterns unimplemented.")
	  | FIX{functions,scope} =>
		 let val functions' = map (fn {lvar,regvars,tyvars,Type,bind} =>
					   {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,bind=f env bind}) functions
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


   (* ----------------------------------------------------------------
    * unbox_fix_args; Unbox arguments to fix-bound functions, for which
    * the argument `a' is used only in contexts `#i a'. All call sites
    * are transformed to match the new function.
    * ---------------------------------------------------------------- *)
   local


     (* Given a lambda variable lv, see if there are any non-select
      * occurences of it in exp; if so, the function is not unboxable,
      * wrt. its arguments. *)

     fun unboxable lv exp : bool =
       let exception NonSelect
	 fun f lv exp =
	   case exp
	     of PRIM (SELECTprim i, [VAR _]) => ()
	      | VAR {lvar,...} => if Lvars.eq(lv,lvar) then raise NonSelect
				  else ()
	      | _ => app_lamb (f lv) exp
       in (f lv exp; true) handle NonSelect => false
       end

     (* The environment *)

     datatype fix_boxity = NORMAL_ARGS
       | UNBOXED_ARGS of tyvar list * Type (* sigma is the scheme of the function after unboxing *)
       | ARG_VARS of Lvars.lvar Vector.vector

     fun layout_fix_boxity NORMAL_ARGS = PP.LEAF "NORMAL_ARGS"
       | layout_fix_boxity (UNBOXED_ARGS sigma) = layoutTypeScheme sigma
       | layout_fix_boxity (ARG_VARS _) = PP.LEAF "ARG_VARS"

     fun eq_fix_boxity (NORMAL_ARGS,NORMAL_ARGS) = true
       | eq_fix_boxity (ARG_VARS _, _) = die "eq_fix_boxity; shouldn't get here"
       | eq_fix_boxity (_, ARG_VARS _) = die "eq_fix_boxity; shouldn't get here"
       | eq_fix_boxity (UNBOXED_ARGS sigma1, UNBOXED_ARGS sigma2) = eq_sigma(sigma1, sigma2)
       | eq_fix_boxity _ = false

     type unbox_fix_env = fix_boxity LvarMap.map

     fun enrich_unbox_fix_env(unbox_fix_env1, unbox_fix_env2) =
       LvarMap.Fold (fn ((lv2,res2),b) => b andalso
		       case LvarMap.lookup unbox_fix_env1 lv2
			 of SOME res1 => eq_fix_boxity(res1,res2)
			  | NONE => false) true unbox_fix_env2

     fun restrict_unbox_fix_env(unbox_fix_env,lvars) =
       List.foldl (fn (lv,acc) =>
		   case LvarMap.lookup unbox_fix_env lv
		     of SOME res => LvarMap.add(lv,res,acc)
		      | NONE => die "restrict_unbox_fix_env.lv not in env") LvarMap.empty lvars

     val layout_unbox_fix_env = LvarMap.layoutMap {start="UnboxFixEnv={",eq="->", sep=", ", finish="}"}
      (PP.LEAF o Lvars.pr_lvar) layout_fix_boxity

     fun lookup env lv = LvarMap.lookup env lv
     fun add_lv (lv,res,env) = LvarMap.add(lv,res,env)

     val frame_unbox_fix_env = ref (LvarMap.empty : unbox_fix_env)

     (* hoist bindings  `lvi = #i lv'  out of body
      * for 0 < i < sz *)
     fun hoist_lvars (body,lv,sz) =
       let
	 fun lookup (x:int) nil = NONE
	   | lookup x ((b,v)::xs) = if x = b then SOME v else lookup x xs

	 fun hoist (body, acc: (int * lvar) list) : LambdaExp * (int * lvar) list =
	   case body
	     of LET{pat,bind,scope} =>
	       (case (pat, bind)
		  of ([(lv1,nil,pt)], PRIM(SELECTprim n, [VAR{lvar,instances=[],regvars=[]}])) =>
		    if Lvars.eq(lvar,lv) then hoist(scope,(n,lv1)::acc)
		    else (body, acc)
		   | _ => (body, acc))
	      | _ => (body, acc)
	 val (body, lvar_map) = hoist (body, nil)

	 val vector = Vector.tabulate
	   (sz, fn i => case lookup i lvar_map
			  of SOME lv => lv
			   | NONE => Lvars.newLvar()) (*Lvars.new_named_lvar (Lvars.pr_lvar lv ^ "_"
							   ^ Int.toString i)) *)
       in (body, vector)
       end

     fun trans (env:unbox_fix_env) lamb =
       case lamb
	 of FIX {functions, scope} =>   (* memo:regvars *)
	  (let
	     fun add_env r ({lvar,regvars,tyvars,Type,bind=FN{pat=[(lv,pt)],body}}, env : unbox_fix_env) : unbox_fix_env =
	       let fun normal () = LvarMap.add (lvar, NORMAL_ARGS, env)
	       in (* interesting only if the function takes a tuple of arguments *)
		 case Type
		   of ARROWtype([RECORDtype nil],res) => normal()
		    | ARROWtype([rt as RECORDtype ts],res) =>
		     if optimise_p() andalso unbox_function_arguments() andalso unboxable lv body then
		       LvarMap.add(lvar,UNBOXED_ARGS (if r then nil else tyvars,ARROWtype(ts,res)),env)
		     else
		       normal()
		    | _ => normal()
	       end
	       | add_env _ _ = die "unbox_fix_args.f.add_env"
	     fun trans_function env {lvar,regvars,tyvars,Type,bind=FN{pat=[(lv,pt)],body}} =
	       let fun mk_fun Type argpat body = {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
						  bind=FN{pat=argpat, body=body}}
	       in case lookup env lvar
		    of SOME NORMAL_ARGS => mk_fun Type [(lv,pt)] (trans env body)
		     | SOME (UNBOXED_ARGS (_, Type' as ARROWtype(argTypes,_))) =>
		      let (* create argument env *)
			val sz = length argTypes
			val (body, vector) = hoist_lvars(body,lv,sz)
			val env' = LvarMap.add(lv, ARG_VARS vector, env)
			val body' = trans env' body
			val (i, argpat) = (List.foldr (fn (argType, (i, argpat)) =>
						      (i-1, (Vector.sub (vector, i), argType) :: argpat))
					   (sz-1,nil) argTypes)
			  handle _ => die "unbox_fix_args.trans.trans_function(subscript)"
			val _ = if i <> ~1 then die "unbox_fix_args.trans.trans_function(foldr)"
				else ()
		      in mk_fun Type' argpat body'
		      end
		     | _ => die "unbox_fix_args.trans.trans_function"
	       end
	       | trans_function _ _ = die "unbox_fix_args.f.do_fun"
	     val env_fix = List.foldl (add_env true) env functions
	     val env_scope = List.foldl (add_env false) env functions
	     val functions = map (trans_function env_fix) functions
	     val scope = trans env_scope scope
	   in
	     FIX{functions=functions,scope=scope}
	   end handle X =>
	     (print "Problem during processing of ";
	      app (fn {lvar,...} => print (Lvars.pr_lvar lvar ^ " ")) functions;
	      print "\n"; raise X))
	  | PRIM(SELECTprim i, [VAR{lvar,instances,regvars}]) =>
	   (case lookup env lvar
	      of SOME (ARG_VARS vector) =>
		if null instances andalso null regvars then
		  let val lv = Vector.sub (vector, i) handle _ => die "trans.select"
		  in VAR{lvar=lv,instances=[],regvars=[]}
		  end
		else die "trans.select.instances"
	       | _ => lamb)
	  | APP(lvexp as VAR{lvar,instances,regvars=[]}, arg, _) =>
	      let fun mk_app lv i =
		    APP(lvexp,
			PRIM(UB_RECORDprim,
			     List.tabulate (i, fn i =>
					    PRIM(SELECTprim i, [VAR{lvar=lv,instances=[],regvars=[]}])
					    )), NONE)
	      in
		case lookup env lvar
		  of SOME(UNBOXED_ARGS (sigma as (tyvars, ARROWtype(argTypes,res)))) =>
		    let val sz = length argTypes
		    in
		      case arg
			of PRIM(RECORDprim _, args) =>
			  if length args <> sz then die "unbox_fix_args.trans.app(length)"
			  else APP(lvexp, PRIM(UB_RECORDprim, map (trans env) args), NONE)
			 | VAR{lvar,instances=[],regvars=[]} => mk_app lvar sz
			 | _ =>
			    let val lv_tmp = Lvars.newLvar()
			      fun errFun () = "OptLambda.trans.app.lvar = " ^ Lvars.pr_lvar lvar
			      val S = mk_subst errFun (tyvars, instances)
			      val Type = on_Type S (RECORDtype argTypes)
			    in LET{pat=[(lv_tmp, nil, Type)], bind=trans env arg,
				   scope=mk_app lv_tmp sz}
			    end
		    end
		   | _ => APP(lvexp, trans env arg, NONE)
	      end
	  | VAR{lvar,instances,regvars=[]} =>
	      (case lookup env lvar of
		   SOME(UNBOXED_ARGS (sigma as (tyvars, ARROWtype(argTypes,res)))) =>
		       let val _ = tick "unbox - inverse-eta"
			   val lv = Lvars.newLvar()
			   val S = mk_subst (fn _ => "unbox.subst") (tyvars,instances)
			   val tau = on_Type S (RECORDtype argTypes)
			   fun sels (n, acc) =
			       if n < 0 then acc
			       else sels (n-1, PRIM(SELECTprim n, [VAR{lvar=lv,instances=[],regvars=[]}])::acc)
			   val args = PRIM(UB_RECORDprim, sels (length argTypes - 1, nil))
		       in FN{pat=[(lv,tau)],body=APP(lamb, args, NONE)}
		       end
		 | _ => lamb)
	  | FRAME{declared_lvars,...} =>
	      let val env' = restrict_unbox_fix_env (env, map #lvar declared_lvars)
	      in (frame_unbox_fix_env := env' ; lamb)
	      end
	  | LET {pat,bind,scope} =>
	      let val env' = List.foldl (fn ((lvar,_,_),e) => LvarMap.add(lvar,NORMAL_ARGS,e)) env pat
	      in LET{pat=pat,
		     bind=trans env bind,
		     scope=trans env' scope}
	      end
	  | _ => map_lamb (trans env) lamb
   in
     val restrict_unbox_fix_env = restrict_unbox_fix_env
     val layout_unbox_fix_env = layout_unbox_fix_env
     val enrich_unbox_fix_env = enrich_unbox_fix_env
     type unbox_fix_env = unbox_fix_env
     fun pr_env e =
       PP.outputTree (print, layout_unbox_fix_env e, 200)
     fun unbox_fix_args (env:unbox_fix_env) lamb : LambdaExp * unbox_fix_env =
       let
(*
	 val _ = print "Import unbox_fix_env:\n"
	 val _ = pr_env env
*)
	   val _ = frame_unbox_fix_env := LvarMap.empty
	   val lamb = trans env lamb
(*
	   val _ = print "\nExport unbox_fix_env:\n"
	   val _ = pr_env (!frame_unbox_fix_env)
	   val _ = print "\n"
*)
       in (lamb, !frame_unbox_fix_env)
       end

     val pu_unbox_fix_env =
	 let val pu_lvarVector = Pickle.vectorGen Lvars.pu
	     fun toInt (NORMAL_ARGS) = 0
	       | toInt (UNBOXED_ARGS _) = 1
	       | toInt (ARG_VARS _) = 2
	     val fun_NORMAL_ARGS = Pickle.con0 NORMAL_ARGS
	     fun fun_UNBOXED_ARGS _ =
		 Pickle.con1 UNBOXED_ARGS (fn UNBOXED_ARGS a => a | _ => die "pu.UNBOXED_ARGS")
		 LambdaExp.pu_TypeScheme
	     fun fun_ARG_VARS _ =
		 Pickle.con1 ARG_VARS (fn ARG_VARS a => a | _ => die "pu.ARG_VARS")
		 pu_lvarVector
	     val pu_fix_boxity =
		 Pickle.dataGen("OptLambda.fix_boxity",toInt,[fun_NORMAL_ARGS,fun_UNBOXED_ARGS,fun_ARG_VARS])
	 in LvarMap.pu Lvars.pu pu_fix_boxity
	 end
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

    fun restrict_inv_eta_env(inveta_env,lvars) =
      List.foldl(fn (lv,acc) =>
		 case LvarMap.lookup inveta_env lv
		   of SOME res => LvarMap.add(lv,res,acc)
		    | NONE => die "restrict_inv_eta_env.lv not in env") LvarMap.empty lvars

    fun new_sigma ([],tau) = ([],tau)
      | new_sigma (tyvars,tau) =
      let fun new_tv tv = if equality_tyvar tv then fresh_eqtyvar()
			  else fresh_tyvar()
	  val tyvars' = map new_tv tyvars
	  val S = mk_subst (fn () => "new_sigma") (tyvars,map TYVARtype tyvars')
      in (tyvars', on_Type S tau)
      end

    fun eq_sigma (([],tau1),([],tau2)) = eq_Type(tau1,tau2)
      | eq_sigma (sigma1 as (tyvars1,_),sigma2 as (tyvars2,_)) =
      List.length tyvars1 = List.length tyvars2 andalso
      let val (tyvars1,tau1) = new_sigma sigma1
	  val (tyvars2,tau2) = new_sigma sigma2
	  val S = mk_subst (fn () => "eq_sigma") (tyvars1,map TYVARtype tyvars2)
	  val tau1' = on_Type S tau1
      in eq_Type(tau1',tau2)
      end


    fun eq_inveta_res (FIXBOUND sigma1, FIXBOUND sigma2) = eq_sigma(sigma1,sigma2)
      | eq_inveta_res (NOTFIXBOUND, NOTFIXBOUND) = true
      | eq_inveta_res _ = false
    fun enrich_inv_eta_env(inveta_env1,inveta_env2) =
      LvarMap.Fold(fn ((lv2,res2),b) => b andalso
		   case LvarMap.lookup inveta_env1 lv2
		      of SOME res1 => eq_inveta_res(res1,res2)
		       | NONE => false) true inveta_env2

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
			       ARROWtype([tau],_) => ([(lv, tau)], lv_e)
			     | ARROWtype(taus,_) => die "inverse_eta - multi-args"
(*
			      let fun sels (n,acc) =
				    if n < 0 then acc
				    else sels(n-1,PRIM(SELECTprim n, [lv_e]) :: acc)
			      in
				  ([(lv, RECORDtype taus)],
				   PRIM(UB_RECORDprim, sels(length taus - 1, nil)))
			      end
*)
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
	      in FIX{functions=map (fn {lvar,regvars,tyvars,Type,bind} =>
				   {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
				    bind=inverse_eta env' bind}) functions,
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
    * Uncurrying of functions
    *
    *  Transform
    *
    *     fix f : sigma = \x1:t1...\xn:tn .
    *        e[f e1 ... en][f]
    *     in e'[f_il e1' ... en'][f_il]
    *     end
    *
    *  into
    *
    *     fix f : sigma' = \<x1:t1...xn:tn> .
    *        e[f <e1 ... en>][\x_1':t1...\xn':tn.f<x1'...xn'>]
    *     in e'[f_il <e1'...en'>][\x1'':t1'...\xn'':tn'.f_il<x1''...xn''>]
    *     end
    *
    *  where
    *
    *     sigma  = \/av.t1 -> ... -> tn -> t
    *     sigma' = \/av. <t1...tn> -> t
    *     av = a1...an
    *     ti' = ti[il/av]    i=1..n
    *     xi', xi'' fresh    i=1..n
    *
    *  E : Var -> N * TypeScheme
    * ----------------------------------------------------------------- *)

   val uncurrying = ref true

   val _ = Flags.add_bool_entry
       {long="uncurrying",short=SOME "uncurry",
	menu=["Control", "Optimiser", "uncurrying"],
	item=uncurrying,neg=true,
	desc=
	"Enable uncurrying of curried functions. The uncurried\n\
	 \function takes its arguments unboxed in registers or\n\
	 \on the stack. For partial applications and non-\n\
	 \application uses of the function, appropriate eta-\n\
	 \expansions are applied."}

   type TypeScheme = tyvar list * Type
   type uc_env = (int * TypeScheme) option LvarMap.map

   fun restrict_uc_env(uc_env:uc_env,lvars) =
	List.foldl (fn (lv, acc) =>
		    case LvarMap.lookup uc_env lv of
			SOME res => LvarMap.add(lv,res,acc)
		      | NONE => LvarMap.add(lv,NONE,acc)) LvarMap.empty lvars

   fun uc_eq (NONE, NONE) = true
     | uc_eq (SOME (n1,s1),SOME(n2,s2)) = n1=n2 andalso eq_sigma(s1,s2)
     | uc_eq _ = false

   fun enrich_uc_env(e1,e2) = LvarMap.enrich uc_eq (e1,e2)

   fun layoutUcPair NONE = PP.LEAF "NONE"
     | layoutUcPair (SOME(n,s)) = PP.HNODE{start="(" ^ Int.toString n ^ ", ",childsep=PP.NOSEP,
					   finish=")", children=[layoutTypeScheme s]}
   fun layout_uc_env e = LvarMap.layoutMap {start="UCMap= {", finish="}",
					    eq=" -> ", sep = ","}
       (PP.LEAF o Lvars.pr_lvar) layoutUcPair e

   fun uc_find_app (env, APP(e1,e2,_),acc,n) = uc_find_app(env,e1,e2::acc,n+1)
     | uc_find_app (env, v as VAR{lvar,instances=il,regvars=[]}, acc, n) =
       (case LvarMap.lookup env lvar of
	    SOME (SOME (N,sigma)) =>
		if n = N then
		    ((* print ("Uncurrying application of " ^ Lvars.pr_lvar lvar
			    ^ " (" ^ Int.toString n ^ ")\n"); *)
		     tick ("uncurry - app(" ^ Int.toString n ^ ")");
		     SOME (v, sigma, il, acc, n))
		else NONE
	  | _ => NONE)
     | uc_find_app _ = NONE

   fun uc_lambdas e : int =
       case e of
	   FN{pat=[_],body} => 1 + uc_lambdas body
	 | _ => 0

   fun uc_rem_lambdas 0 b = (nil,b)
     | uc_rem_lambdas n b =
       case b of
	   FN{pat=[p],body} =>
	       let val (pat,b) = uc_rem_lambdas (n-1) body
	       in (p::pat,b)
	       end
	 | FN _ => die "uc_rem_lambdas.FN"
	 | _ => die "uc_rem_lambdas.not FN"

   fun uc_tau 0 t = (nil,t)
     | uc_tau n t =
       case t of
	   ARROWtype([t1],[t2]) =>
	       let val (ts,t) = uc_tau (n-1) t2
	       in (t1::ts,t)
	       end
	 | ARROWtype _ => die "uc_tau.ARROWtype"
	 | _ => die "uc_tau.not ARROWtype"

   val uc_env_frame : uc_env ref = ref LvarMap.empty

   fun uc env e =
       case (*fixify env*) e of
	   FIX {functions,scope} =>
	       let val (functions',env') = uc_functions env functions
	       in FIX{functions=functions',
		      scope=uc (LvarMap.plus(env,env')) scope}
	       end
	 | LET{pat=[(lv,tyvars,tau)],bind=b as VAR{lvar,instances,regvars=[]},scope} =>
	       if !uncurrying then
	       (case LvarMap.lookup env lvar of
		    SOME (SOME (n,(tvs,ARROWtype(ts,_)))) =>
			if n <> length ts then die "uncurry.LET-VAR.length"
			else
			    let val S = mk_subst (fn _ => "uncurry.LET-VAR") (tvs,instances)
				val ts = map (on_Type S) ts
				val pat = map (fn t => (Lvars.newLvar(),t)) ts
				val args = PRIM(UB_RECORDprim, map (fn (lv,_) => VAR{lvar=lv,instances=[],regvars=[]}) pat)
				val (ts',t') = uc_tau n tau
				val tau' = ARROWtype(ts',[t'])
				val env' = LvarMap.add(lv,SOME(n,(tyvars,tau')),env)
				val function = {lvar=lv,regvars=[],tyvars=tyvars,Type=tau',
						bind=FN{pat=pat,body=APP(b,args,NONE)}}
			    in
				tick ("uncurry - let-var(" ^ Int.toString n ^ ")");
				FIX{functions=[function],scope=uc env' scope}
			    end
		  | _ => map_lamb (uc env) e)
	       else map_lamb (uc env) e
	 | FRAME{declared_lvars,...} =>
	       (uc_env_frame := restrict_uc_env (env, map #lvar declared_lvars); e)
	 | VAR {lvar,instances,regvars=[]} =>
	       (case LvarMap.lookup env lvar of
		    SOME (SOME (n,(tvs,ARROWtype(ts,_)))) =>
			let (* val _ = print ("Eta-expanding application of uncurried function "
					   ^ Lvars.pr_lvar lvar ^ "\n") *)
			    val _ = tick ("uncurry - eta-expand(" ^ Int.toString n ^ ")")
			    val S = mk_subst (fn _ => "uc.VAR") (tvs,instances)
			    val ts = map (on_Type S) ts
			    val _ = if length ts = 0 then die "uc.VAR - empty arg type list"
				    else ()
			    val lvts = map (fn t => (Lvars.newLvar(),t)) ts
			    val lves = map (fn (lv,_) => VAR{lvar=lv,instances=[],regvars=[]}) lvts
			in List.foldr (fn (lvt,e) => FN{pat=[lvt],body=e})
			    (APP(e,PRIM(UB_RECORDprim, lves),NONE)) lvts
			end
		  | _ => e)
	 | _ => case uc_find_app (env,e,nil,0) of
	       NONE => map_lamb (uc env) e
	     | SOME (v, sigma, il, es, n) => APP(v, PRIM(UB_RECORDprim, map (uc env) es),NONE)
   and uc_functions (env:uc_env) functions =
       let fun mk_envs (nil,env_b,env_s) = (env_b, env_s)
	     | mk_envs ({lvar:lvar,regvars,tyvars,Type:Type,bind:LambdaExp}::rest,env_b,env_s) =
	      let val n = uc_lambdas bind
		  val (env_b,env_s) =
		      if !uncurrying andalso n >= 2 then
			  let val (ts,t) = uc_tau n Type
			      val tau = ARROWtype(ts,[t])
			  in (LvarMap.add(lvar,SOME(n,(nil,tau)),env_b),
			      LvarMap.add(lvar,SOME(n,(tyvars,tau)),env_s))
			  end
		      else (LvarMap.add(lvar,NONE,env_b),
			    LvarMap.add(lvar,NONE,env_s))
	      in mk_envs (rest,env_b,env_s)
	      end
	   val (env_b,env_s) = mk_envs(functions,LvarMap.empty,LvarMap.empty)
       in (map (uc_function env_b env) functions,env_s)
       end
   and uc_function env_b env {lvar:lvar,regvars:RegVar.regvar list,tyvars:tyvar list,Type:Type,bind:LambdaExp} =
       case LvarMap.lookup env_b lvar of
	   NONE => die "fix-bound lvar not in uc-env"
	 | SOME NONE => {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=Type,
			 bind=uc (LvarMap.plus(env,env_b)) bind}
	 | SOME(SOME(n,(tvs,tau))) =>
	       ((* print ("Uncurrying function " ^ Lvars.pr_lvar lvar ^ " ("
		       ^ Int.toString n ^ ")\n"); *)
		tick ("uncurry - fix(" ^ Int.toString n ^ ")");
	       {lvar=lvar,regvars=regvars,tyvars=tyvars,Type=tau,
		bind= uc_function_bind (LvarMap.plus(env,env_b)) n bind})
   and uc_function_bind env n bind =
       let val (pat,e) = uc_rem_lambdas n bind
       in FN{pat=pat,body=uc env e}
       end
   and fixify env e =
       case e of
	   LET{pat=[(lv,tyvars,tau)],bind=b as VAR _, scope} =>
	       (case uc env b of
		    b as FN _ =>
			FIX{functions= [{lvar=lv,regvars=[],tyvars=tyvars,
					 Type=tau,bind=b}],
			    scope=scope}
		  | _ => e)
	 | _ => e


   (* disabling uncurrying is done by not transforming FIX'es and
    * not introducing uncurry information in the uncurry environment.
    * This way, disabling can be done on a per-file basis. *)
   fun uncurry env e =
       (uc_env_frame := LvarMap.empty;
	let val e' = uc env e
	in (e', !uc_env_frame)
	    before uc_env_frame := LvarMap.empty
	end)

   (* -----------------------------------------------------------------
    * The Optimiser Engine
    * ----------------------------------------------------------------- *)

    fun optimise (ce:contract_env) lamb =
      let
	  fun loop_opt ce lamb =
	      let val (lamb, ce) = contract ce lamb
                  val lamb = eliminate_explicit_records lamb
                  val lamb = cse lamb
	      in (lamb, ce)
	      end

	  fun loop n (lamb,ce') =
	    if n > max_optimise then (lamb,ce')
	    else
	      let val _ = reset_tick()
		  val _ = log ("Pass number " ^ Int.toString (n+1) ^ "\n")
		  val (lamb',ce') = loop_opt ce lamb
		  val _ = end_round() (*stat*)
	      in if test_tick() then loop (n+1) (lamb',ce')
		 else (lamb',ce')
	      end

	  val pre_opt = do_minimize_fixs

	  val _ = reset_statistics()
	  val lamb' = pre_opt lamb
	  val (lamb',ce') = loop 0 (lamb', contract_env_dummy lamb')
	  val _ = print_stat lamb lamb'
      in (lamb',ce')
      end


   (* -----------------------------------------------------------------
    * The lambda optimiser environment
    * ----------------------------------------------------------------- *)

    type env = inveta_env * let_env * unbox_fix_env * uc_env
	* contract_env * contract_env

    val empty =  (LvarMap.empty, LvarMap.empty, LvarMap.empty,
		  LvarMap.empty, LvarMap.empty, LvarMap.empty)
    val initial = empty
    fun plus ((e1, e2, e3, e4, e5, e6), (e1', e2', e3', e4', e5', e6')) =
      (LvarMap.plus (e1,e1'), LvarMap.plus (e2,e2'),
       LvarMap.plus (e3,e3'), LvarMap.plus (e4,e4'),
       LvarMap.plus (e5,e5'), LvarMap.plus (e6,e6'))

    fun restrict((inv_eta_env,let_env, unbox_fix_env,uc_env,cenv1,cenv2), lvars, cons, tns) =
      let val e1 = restrict_inv_eta_env(inv_eta_env,lvars)
	  val e2 = restrict_let_env(let_env,lvars)
	  val e3 = restrict_unbox_fix_env(unbox_fix_env,lvars)
	  val e4 = restrict_uc_env(uc_env,lvars)
	  val (e5,cons1,tns1) = restrict_contract_env(cenv1,lvars,cons,tns)
	  val (e6,cons2,tns2) = restrict_contract_env(cenv2,lvars,cons,tns)
      in
	((e1, e2, e3, e4, e5, e6), cons1 @ cons2, tns1 @ tns2)
      end

    val debug_man_enrich = Flags.is_on0 "debug_man_enrich"

    fun debug(s, b) = if debug_man_enrich() then
                         (if b then print("\n" ^ s ^ ": enrich succeeded.")
			  else print("\n" ^ s ^ ": enrich failed."); b)
		      else b

    fun enrich((inv_eta_env1,let_env1,unbox_fix_env1,uc_env1,cenv11,cenv21): env,
	       (inv_eta_env2,let_env2,unbox_fix_env2,uc_env2,cenv12,cenv22): env) : bool =
      debug("inv_eta_env", enrich_inv_eta_env(inv_eta_env1,inv_eta_env2)) andalso
      debug("let_env", enrich_let_env(let_env1,let_env2)) andalso
      debug("unbox_fix_env", enrich_unbox_fix_env(unbox_fix_env1,unbox_fix_env2)) andalso
      debug("contract_env", enrich_contract_env(cenv11,cenv12)) andalso
      debug("contract_env2", enrich_contract_env(cenv21,cenv22)) andalso
      debug("uc_env", enrich_uc_env(uc_env1,uc_env2))

    fun layout_env (e1,e2,e3,e4,e5,e6) =
      PP.NODE{start="",finish="",indent=0,childsep=PP.RIGHT ",",
	      children=[layout_inveta_env e1, layout_let_env e2,
			layout_unbox_fix_env e3,
			layout_uc_env e4, layout_contract_env e5,
			layout_contract_env e6]}


   (* -----------------------------------------------------------------
    * Rewriting: This rewriting shall always be performed, no matter
    * whether the lamb is optimised or not, and it shall always be
    * performed after possibly optimisation
    * ----------------------------------------------------------------- *)

    fun rewrite (inveta_env,let_env) lamb =
	let val (lamb,let_env) = functionalise_let let_env lamb
	    val lamb = fix_conversion lamb
	    val (lamb,inveta_env) = inverse_eta_for_fix_bound_lvars inveta_env lamb
	in (lamb, (inveta_env, let_env))
	end


   (* -----------------------------------------------------------------
    * The Optimiser
    * ----------------------------------------------------------------- *)

    fun maybeoptimise cenv e =
	if optimise_p() then optimise cenv e
	else (e, contract_env_dummy e)

    fun optimise (env, PGM(DATBINDS db,lamb)) =
	let
	    val (env1,env2,ubenv,ucenv,cenv,cenv2) = env
	    val (lamb, cenv) = maybeoptimise cenv lamb
	    val lamb = fix_conversion lamb
(*	    val _ = prLambdaExp "Before unbox" lamb *)
	    val (lamb, ubenv) = unbox_fix_args ubenv lamb
(*	    val _ = prLambdaExp "Before uncurry" lamb *)
	    val (lamb, ucenv) = uncurry ucenv lamb
(*	    val _ = prLambdaExp "Before 2nd optimize round" lamb *)
	    val (lamb, cenv2) = maybeoptimise cenv2 lamb
(*	    val _ = prLambdaExp "Before rewriting" lamb *)
	    val (lamb, (env1,env2)) = rewrite (env1,env2) lamb
	    val env = (env1,env2,ubenv,ucenv,cenv,cenv2)
	in
	    (PGM(DATBINDS db, lamb), env)
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
	    Pickle.convert (fn ((a,b,c),(d,e,f)) => (a,b,c,d,e,f), fn (a,b,c,d,e,f) => ((a,b,c),(d,e,f)))
	    (Pickle.pairGen0(Pickle.tup3Gen0(Pickle.comment "OptLambda.iee.pu" pu_iee,
					     Pickle.comment "OptLambda.let_env.pu" pu_let_env,
					     Pickle.comment "OptLambda.unbox_fix_env.pu" pu_unbox_fix_env),
			     Pickle.tup3Gen0(Pickle.comment "OptLambda.uce.pu" pu_uce,
					     Pickle.comment "OptLambda.contract_env.pu" pu_contract_env,
					     Pickle.comment "OptLambda.contract_env2..pu" pu_contract_env)))
	end
  end
