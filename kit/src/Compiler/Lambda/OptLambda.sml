(* OptLambda - lambda code optimiser. *)

(*$OptLambda: LVARS LAMBDA_EXP DIGRAPH_SCC LAMBDA_BASICS FINMAP
              MONO_FINMAP BASIC_IO CON EXCON TYNAME FLAGS CRASH
              PRETTYPRINT OPT_LAMBDA*)

functor OptLambda(structure Lvars: LVARS
		  structure LambdaExp: LAMBDA_EXP
		    sharing type LambdaExp.lvar = Lvars.lvar
		  structure LvarDiGraphScc : DIGRAPH_SCC
		    sharing type LvarDiGraphScc.nodeId = Lvars.lvar
		        and type LvarDiGraphScc.info = Lvars.lvar
		        and type LvarDiGraphScc.edgeInfo = unit
 	          structure LambdaBasics : LAMBDA_BASICS
		    sharing type LambdaBasics.LambdaExp = LambdaExp.LambdaExp
		        and type LambdaBasics.lvar = Lvars.lvar 
			and type LambdaBasics.excon = LambdaExp.excon
			and type LambdaBasics.Type = LambdaExp.Type
			and type LambdaBasics.tyvar = LambdaExp.tyvar
		  structure FinMap : FINMAP
		  structure LvarMap : MONO_FINMAP
		    sharing type LvarMap.dom = Lvars.lvar
		  structure BasicIO: BASIC_IO
                  structure Con : CON
                    sharing type Con.con = LambdaExp.con
                  structure Excon : EXCON
                    sharing type Excon.excon = LambdaExp.excon  
                  structure TyName : TYNAME
                    sharing type TyName.TyName = LambdaExp.TyName
		  structure Flags: FLAGS
		  structure Crash: CRASH
                  structure PP: PRETTYPRINT
                    sharing type PP.StringTree = LambdaExp.StringTree = 
		                 FinMap.StringTree = LvarMap.StringTree
		 ): OPT_LAMBDA =
  struct

    open LambdaExp LambdaBasics
    type bound_lvar = {lvar:lvar, tyvars:tyvar list, Type: Type}


   (* -----------------------------------------------------------------
    * Some Optimisation Constants
    * ----------------------------------------------------------------- *)

    val max_inline_size_fn = 20         (* max size of non-recursive function defs. to be inlined. *)
    val max_inline_size_fix = 200       (* max size of recursive function defs. to be specialised. *) 
    val max_optimise = 20               (* maximal number of times the entire term is traversed. *)


    
   (* -----------------------------------------------------------------
    * Dynamic flags
    * ----------------------------------------------------------------- *)

    val path = ["Control", "Optimiser"]
    val add_entry = fn (s, s', r) => Flags.add_flag_to_menu (path, s, s', r) 
    val statistics_after_optimisation =
          Flags.lookup_flag_entry "statistics_after_optimisation"
    val specialize_recursive_functions = ref true
    val entries = [("minimize_fixs", "minimize fix's", ref true),
		   ("fix_conversion", "fix conversion", ref true),
		   ("contract", "contract", ref true),
		   ("specialize_recursive_functions",
		    "  specialize recursive functions",
		    specialize_recursive_functions),
		   ("elim_explicit_records", "eliminate explicit records", ref true)]
    val _ = map add_entry entries
    val is_on = Flags.is_on


   (* -----------------------------------------------------------------
    * Some helpful functions
    * ----------------------------------------------------------------- *)

    fun app f [] = ()
      | app f (x::xs) = (f x; app f xs)

    val unit_Type = RECORDtype []

    fun log x = if !statistics_after_optimisation then output(!Flags.log,x)
		else ()

    fun die s = Crash.impossible ("OptLambda." ^ s)

    fun reportBadLambda(msg,lamb) = 
      (PP.outputTree((fn s => output(!Flags.log, s)),
		     LambdaExp.layoutLambdaExp lamb,
		     !Flags.colwidth);
       die ("optimisation failed (" ^ msg ^ ")"))

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

   (* -----------------------------------------------------------------
    * Statistical functions
    * ----------------------------------------------------------------- *)

    local
      type stat_map = (string, int) FinMap.map
      fun pad m =
        let val l = FinMap.list m
            val ss = map #1 l
            val max = List.foldL (fn s => fn max => if String.size s > max then String.size s else max) 0 ss
            fun space 0 = ""
              | space n = " " ^ space (n-1)
            fun add_space s = s ^ space (max - String.size s)
            val ss' = map add_space ss
            val l' = ListPair.zip (ss', map #2 l) handle _ => die "pad"
	    fun fromList [] = FinMap.empty
	      | fromList ((a,b)::rest) = FinMap.add(a,b,fromList rest)
        in fromList l'
        end 
      val stat_map : stat_map ref = ref FinMap.empty
      val layout_stat = (FinMap.layoutMap {start="Optimiser Statistics:", eq=" : ", sep=", ", finish =""}
	PP.LEAF (PP.LEAF o Int.string)) o pad
      fun print_size_difference size_before size_after =
	let
	  val per     = 100.0*(real (size_before - size_after))/(real size_before)
	  val dec     = 10.0
	  val rounded = (real (Real.round (dec*per)))/dec
	in 
	  log("\n------------------------------------------------\n" ^ 
	      "   Size before: " ^ (Int.string size_before) ^ ".\n" ^
	      "   Size after:  " ^ (Int.string size_after) ^ ".\n" ^ 
	      (if size_before > 0 then 
		 "   Reduced:     " ^ (Real.string rounded) ^ "%.\n"
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
	      | p [n] = Int.string n
	      | p (n::ns) = Int.string n ^ ", " ^ p ns
	in log ("ticks pr. round: " ^ p (rev (!tick_count_list)))
	end

    in
      fun reset_statistics() = (stat_map := FinMap.empty; tick_count_list := [0])
      fun add_statistics (s:string) =
	let val map = !stat_map
	    val i = case FinMap.lookup map s
		      of Some i => i
		       | None => 0
	    val new_stat_map = FinMap.add(s, i+1, map)
	    val _ = incr_tick_counter()
	in (* log ("  " ^ s ^ "\n"); *) stat_map := new_stat_map
	end
      fun print_stat lamb lamb' =
	if not (!statistics_after_optimisation) then ()
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
			       if !statistics_after_optimisation then add_statistics s
			       else ())
      fun reset_tick() = flag := false
      fun test_tick() = !flag
    end


   (* -----------------------------------------------------------------
    * Equality on types
    * ----------------------------------------------------------------- *)

    fun eq_tau (TYVARtype tv1, TYVARtype tv2) = tv1=tv2
      | eq_tau (ARROWtype (tl1, tl1'), ARROWtype (tl2,tl2')) = eq_taus(tl1,tl2) andalso eq_taus(tl1',tl2')
      | eq_tau (CONStype (tl1,tn1), CONStype (tl2,tn2)) = TyName.eq(tn1,tn2) andalso eq_taus(tl1,tl2)
      | eq_tau (RECORDtype tl1, RECORDtype tl2) = eq_taus(tl1,tl2)
      | eq_tau _ = false
    and eq_taus ([],[]) = true
      | eq_taus (tau1::taus1,tau2::taus2) = eq_tau(tau1,tau2) andalso eq_taus(taus1,taus2)
      | eq_taus _ = false


   (* -----------------------------------------------------------------
    * Equality on lambda expressions (conservative approximation)
    * ----------------------------------------------------------------- *)

    fun eq_lamb (INTEGER n, INTEGER n') = (n = n')
      | eq_lamb (REAL r, REAL r') = (r = r')
      | eq_lamb (STRING s, STRING s') = (s = s')
      | eq_lamb (VAR{lvar,instances=il},VAR{lvar=lvar',instances=il'}) = Lvars.eq(lvar,lvar') andalso eq_taus(il,il')
      | eq_lamb (PRIM(RECORDprim, lambs),PRIM(RECORDprim, lambs')) = eq_lambs(lambs,lambs')
      | eq_lamb _ = false
    and eq_lambs ([],[]) = true
      | eq_lambs (x::xs,x'::xs') = eq_lamb(x,x') andalso eq_lambs(xs,xs')
      | eq_lambs _ = false
      
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
    

   (* -----------------------------------------------------------------
    * safe_lamb - Determines whether a lambda expression can be moved
    * without changing the semantic of the program.
    * ----------------------------------------------------------------- *)
   
   local
     exception NotSafe

     fun safe_prim prim =
       case prim
	 of CONprim _          => ()
	  | DECONprim _        => ()
	  | EXCONprim _        => ()
	  | DEEXCONprim _      => ()
	  | RECORDprim         => ()
	  | SELECTprim _       => ()
	  | REALprim           => ()
	  | SINprim            => ()
	  | COSprim            => ()
	  | ARCTANprim         => ()
	  | SIZEprim           => ()
	  | EXPLODEprim        => ()
	  | IMPLODEprim        => ()
	  | EQUALprim _        => ()
	  | NOTEQUALprim _     => ()
	  | LESS_INTprim       => ()
	  | LESS_REALprim      => ()
	  | GREATER_INTprim    => ()
	  | GREATER_REALprim   => ()
	  | LESSEQ_INTprim     => ()
	  | LESSEQ_REALprim    => ()
	  | GREATEREQ_INTprim  => ()
	  | GREATEREQ_REALprim => ()
	  | STD_INprim         => ()
	  | STD_OUTprim        => ()
	       (* likewise for other primitives that do not perform side effects
		* and cannot raise exceptions *)
	  | _ => raise NotSafe 

     fun safe_sw safe (SWITCH(e,sel,opt_e)) =
       let fun safe_sel [] = ()
	     | safe_sel ((a,e)::rest) = (safe e; safe_sel rest)
	   fun safe_opt (Some e) = safe e
	     | safe_opt None = ()
       in (safe e; safe_sel sel; safe_opt opt_e)
       end

     fun safe lamb =
       case lamb
	 of VAR _	                => ()
	  | INTEGER _                   => ()
	  | STRING _	                => ()
	  | REAL _	                => ()
	  | FN _	                => ()
	  | LET {bind,scope,...}        => (safe bind; safe scope)
	  | FIX {scope,...}             => safe scope
	  | APP _	                => raise NotSafe
	  | EXCEPTION (_,_,scope)       => safe scope
	  | RAISE _                     => raise NotSafe
	  | HANDLE(lamb, _)             => safe lamb
	  (* if `lamb' is safe, then the actual handler can never be
	   * activated. If `lamb' is unsafe, then the entire expression
	   * is unsafe anyway. *)
	  | SWITCH_I sw                 => safe_sw safe sw
	  | SWITCH_S sw                 => safe_sw safe sw
	  | SWITCH_R sw                 => safe_sw safe sw
	  | SWITCH_C sw                 => safe_sw safe sw
	  | SWITCH_E sw                 => safe_sw safe sw
	  | PRIM(prim,lambs)            => (safe_prim prim; app safe lambs) 
	  | FRAME _                     => raise NotSafe
   in
     fun safe_lambs lambs = (app safe lambs; true) handle NotSafe => false
     fun safe_lamb lamb = safe_lambs [lamb]
   end   


   (* -----------------------------------------------------------------
    * Closedness of a lambda expression
    * ----------------------------------------------------------------- *)

   fun closed lamb =
     let exception OPEN
       fun c b e =
	 case e
	   of VAR{lvar,...} => if is_in_lv lvar b then () else raise OPEN
	    | LET{pat,bind,scope} =>
	     let fun add [] b = b
		   | add ((lv,_,_)::pat) b = add pat (lv::b)
	     in c b bind; c (add pat b) scope
	     end
	    | FN{pat,body} =>
	     let fun add [] b = b
		   | add ((lv,_)::pat) b = add pat (lv::b)
	     in c (add pat b) body
	     end
	    | FIX{functions,scope} =>
	     let fun add [] b = b
		   | add ({lvar,tyvars,Type,bind}::fcs) b = add fcs (lvar::b)
	         val b' = add functions b
	     in app ((c b') o #bind) functions; c b' scope
	     end
	    | FRAME _ => die "closed" 
	    | _ => app_lamb (c b) e
     in (c [] lamb; true) handle OPEN => false
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

   fun specializable {lvar=lv_f, tyvars, Type=ARROWtype([tau_1'],[ARROWtype([tau_2'],_)]), 
		      bind=FN{pat=[(lv_x,tau_1)],body=FN{pat=[(lv_y,tau_2)],body}}} =
     let exception Fail
         fun app_f_x (APP(VAR{lvar=lv_f',...}, VAR{lvar=lv_x',...})) =
	     if Lvars.eq(lv_f',lv_f) then if Lvars.eq(lv_x',lv_x) then () else raise Fail
	     else if Lvars.eq(lv_x',lv_f) then raise Fail else ()
	   | app_f_x (VAR{lvar,...}) = if Lvars.eq(lvar,lv_f) then raise Fail else ()
	   | app_f_x e = app_lamb app_f_x e
     in	eq_tau(tau_1,tau_1') andalso eq_tau(tau_2,tau_2') andalso 
       ((app_f_x body; true) handle Fail => false)
     end 
     | specializable _ = false

   fun subst_lvar_for_app lv (e as APP(lv_e as VAR{lvar,...},_)) = 
        if Lvars.eq(lvar,lv) then lv_e 
	else map_lamb (subst_lvar_for_app lv) e
     | subst_lvar_for_app lv e = map_lamb (subst_lvar_for_app lv) e 

   fun subst_e_for_lvar lv e (e' as VAR{lvar,...}) = if Lvars.eq(lvar,lv) then e else e'
     | subst_e_for_lvar lv e e' = map_lamb (subst_e_for_lvar lv e) e'

   fun specialize_bind {lvar=lv_f, tyvars, Type=ARROWtype([tau_1],[ARROWtype([tau_2],[tau_3])]), 
			bind=FN{pat=[(lv_x,_)],body=FN{pat=[(lv_y,_)],body}}} 
                       instances lamb' =
     let val S = mk_subst "specialize_bind" (tyvars, instances)
         val tau_2' = on_Type S tau_2
	 val tau_1' = on_Type S tau_1
         val tau = ARROWtype([tau_2'],[on_Type S tau_3])
	 val body' = subst_lvar_for_app lv_f body
	 val body'' = on_LambdaExp S body'
	 val scope = FIX{functions=[{lvar=lv_f,tyvars=[],Type=tau,
				     bind=FN{pat=[(lv_y,tau_2')],body=body''}}],
			 scope=VAR{lvar=lv_f,instances=[]}}
	 val e_0 = LET{pat=[(lv_x,[],tau_1')],bind=lamb',scope=scope}
     in new_instance e_0
     end
     | specialize_bind _ _ _ = die "specialize_bind"

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
	fun reset_excon_bucket() = excon_bucket := []
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
	| show_cv (CRECORD l) = implode ("[" :: (map show_cv l @ ["]"]))
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
	      | on _ = CUNKNOWN 
	in on cv
	end

      (* least upper bound *)
      fun lub (cv as CVAR lamb,CVAR lamb') = if eq_lamb(lamb,lamb') then cv else CUNKNOWN
	| lub (CRECORD cvals,CRECORD cvals') = (CRECORD (map lub (ListPair.zip(cvals,cvals')))
						handle ListPair.Zip => die "lub")
	| lub (cv as CCONST lamb,CCONST lamb') = if lamb=lamb' then cv else CUNKNOWN
	| lub _ = CUNKNOWN

      fun lubList [] = CUNKNOWN
	| lubList l = List.foldL' (fn cval => fn cval' => lub(cval, cval')) l


      (* -----------------------------------------------------------------
       * Compile time environment
       * ----------------------------------------------------------------- *)

      type env = (tyvar list * cv) LvarMap.map

      fun updateEnv [] [] env = env
	| updateEnv (lv::lvs) (p::ps) env = updateEnv lvs ps (LvarMap.add(lv,p,env))
	| updateEnv _ _ _ = die "updateEnv"

      fun lookup_lvar (env, lvar) = LvarMap.lookup env lvar


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
	  of VAR{lvar,instances} => incr_use lvar
	   | LET{pat,bind,scope} => (app (Lvars.reset_use o #1) pat; init bind; init scope)
	   | FN{pat,body} => (app (Lvars.reset_use o #1) pat; init body)
	   | FIX{functions,scope} => (app (Lvars.reset_use o #lvar) functions;
				      app (mark_lvar o #lvar) functions;
				      app (init o #bind) functions;
				      app (unmark_lvar o # lvar) functions;
				      init scope)
	   | FRAME {declared_lvars,...} => app (fn {lvar,...} => (incr_use lvar; incr_use lvar)) declared_lvars
	   | _ => app_lamb init e 


      fun is_small_closed_fn lamb =
	case lamb 
	  of FN _ => small_lamb max_inline_size_fn lamb andalso closed lamb 
	   | _ => false

      fun is_fn (FN _) = true
	| is_fn _ = false


      (* -----------------------------------------------------------------
       * Reduce on switch
       * ----------------------------------------------------------------- *)

      fun reduce_switch (reduce, env, (fail as (_,cv)), (SWITCH(arg, sel, opt))) =  (* If branches are equal and the selector *)
	let fun allEqual [] = true                                                  (* is safe then eliminate switch. *)
	      | allEqual [x] = true
	      | allEqual (x::(ys as y::_)) = eq_lamb(x,y) andalso allEqual ys
	in case opt
	     of Some lamb =>
	       if safe_lamb arg andalso allEqual (lamb::(map snd sel)) then
		 (tick "reduce - switch"; decr_uses arg; app (decr_uses o snd) sel; reduce (env, (lamb, cv)))
	       else fail
	      | None =>
		 if safe_lamb arg andalso allEqual (map snd sel) then
		   case sel
		     of (_,lamb)::sel' => (tick "reduce - switch"; decr_uses arg; 
					   app (decr_uses o snd) sel'; reduce (env, (lamb, cv)))
		      | _ => die "reduce_switch" 
		 else fail
	end


      (* -----------------------------------------------------------------
       * Reduce
       * ----------------------------------------------------------------- *)

      fun reduce (env, (fail as (lamb,cv))) =
	case lamb
	  of VAR{lvar,instances} =>
	    ((*output(!Flags.log, Lvars.pr_lvar lvar ^ ":" );*)
             case lookup_lvar(env,lvar)
	       of Some (tyvars,cv) =>
		 ((*output(!Flags.log, show_cv cv ^ "\n");*)
                  case cv
		    of CFN {lexp=lamb',large} =>
		      if large andalso not(Lvars.one_use lvar) then (lamb, CVAR lamb)
		      else let val S = mk_subst "reduce1" (tyvars, instances)
			       val _ = decr_use lvar
			       val lamb'' = new_instance lamb'
			       val _ = incr_uses lamb''
			       val _ = if large then tick "reduce - inline-largefn"
				       else tick "reduce - inline-smallfn"
			   in (on_LambdaExp S lamb'', CVAR lamb)    (* reduce(env,...) *)
			   end
		     | CVAR (lamb' as VAR{lvar=lvar',instances=instances'}) =>
			   let val S = mk_subst "reduce2" (tyvars,instances)
			       val _ = decr_use lvar
			       val _ = incr_use lvar'
			       val lamb'' = on_LambdaExp S lamb'
			   in if Lvars.eq(lvar,lvar') then (lamb'', CVAR lamb'') 
			      else (tick "reduce - inline-var"; (lamb'', CVAR lamb'')) (*reduce (env, (lamb'', CVAR lamb''))*)
			   end
		     | CCONST (lamb' as INTEGER _) => (decr_use lvar; tick "reduce - inline-int"; (lamb', cv))
		     | CCONST lamb' => if Lvars.one_use lvar then (decr_use lvar; tick "reduce - inline-const"; (lamb', cv))
				       else (lamb, CVAR lamb)
		     | CUNKNOWN => (lamb, CVAR lamb)
		     | _ => let val S = mk_subst "reduce3" (tyvars,instances)
			    in (lamb, on_cv S cv)
			    end)
		| None => ((*output(!Flags.log, "none\n");*) (lamb, CVAR lamb)))
	   | INTEGER _ => (lamb, CCONST lamb)
	   | STRING _ => (lamb, CCONST lamb)
	   | REAL _ => (lamb, CCONST lamb) 
	   | LET{pat=(pat as [(lvar,tyvars,tau)]),bind,scope} =>
	       let fun do_sw SW (SWITCH(VAR{lvar=lvar',instances},sel,opt_e)) =
		     if Lvars.eq(lvar,lvar') andalso Lvars.one_use lvar then
		       let val S = mk_subst "let-switch" (tyvars, instances)
		       in tick "reduce - inline-switch"; 
			 (SW (SWITCH(on_LambdaExp S bind, sel, opt_e)), cv)
		       end
		     else fail
		     | do_sw _ _ = fail
	       in if Lvars.zero_use lvar then
		    if safe_lamb bind then 
		      (decr_uses bind; tick "reduce - dead-let"; reduce (env, (scope, cv)))
		    else case scope
			   of PRIM(RECORDprim,[]) => fail
			    | _ => if tau = unit_Type then fail
				   else let val pat'=[(Lvars.new_named_lvar "_not_used",[],unit_Type)]
					    val bind' = LET{pat=pat,bind=bind,scope=PRIM(RECORDprim, [])}
					    val e = LET{pat=pat',bind=bind',scope=scope}
					in tick "reduce - dead-type"; (e,cv)
					end
		  else case scope
			 of VAR{lvar=lvar',instances} =>
			   if Lvars.eq(lvar,lvar') then   (* no need for decr_uses *)
			     let val S = mk_subst "reduce.LET" (tyvars, instances)
			     in tick "reduce - let-var"; reduce (env, (on_LambdaExp S bind, cv))
			     end
			   else fail
			  | SWITCH_I sw => do_sw SWITCH_I sw
			  | SWITCH_S sw => do_sw SWITCH_S sw
			  | SWITCH_R sw => do_sw SWITCH_R sw
			  | SWITCH_C sw => do_sw SWITCH_C sw
			  | SWITCH_E sw => do_sw SWITCH_E sw
			  | _ => fail
	       end
	  | PRIM(SELECTprim n,[lamb]) =>
	       let fun do_select () =
		      case cv 
			of CRECORD cvs => 
			  let val nth_cv = List.nth n cvs
			    handle List.Subscript _ => die "reduce4"
			  in case nth_cv 
			       of CVAR var => (tick "reduce - sel-var"; decr_uses lamb; incr_uses var; reduce (env, (var,nth_cv)))
				| CCONST(INTEGER n) => (tick "reduce - sel-int"; decr_uses lamb; (INTEGER n, nth_cv))
				| _ => (lamb, nth_cv)
			  end
			 | _ => fail
	       in case lamb
		    of PRIM(RECORDprim,lambs) =>
		      let val (lamb', lambs') = List.removeNth n lambs
		      in if List.foldL (fn a => fn b => safe_lamb a andalso b) true lambs' then
			   (tick "reduce - sel-record"; app decr_uses lambs'; reduce(env, (lamb', CUNKNOWN)))
			 else do_select()
		      end
		     | _ => do_select()
	       end 
	  | FIX{functions,scope} =>
	       let val lvs = map #lvar functions
	       in if zero_uses lvs then (tick "reduce - dead-fix"; app (decr_uses o #bind) functions; reduce (env, (scope,cv)))
		  else case functions
			 of [function as {lvar,tyvars,Type,bind}] =>
			   if not(lvar_in_lamb lvar bind) then 
			     (tick "reduce - fix-let"; reduce (env, (LET{pat=[(lvar,tyvars,Type)],bind=bind,scope=scope},cv)))
			   else fail
			  | _ => fail
	       end 
	  | APP(FN{pat,body=scope},bind) => 
	       let val pat' = fn_to_let_pat pat
	       in tick "appfn-let"; reduce (env, (LET{pat=pat',bind=bind,scope=scope}, CUNKNOWN))
	       end
	  | APP(VAR{lvar,instances}, lamb2) => 
	       (case lookup_lvar(env, lvar)
		  of Some (tyvars, CFIX{Type,bind,large}) =>
		    if not(large) orelse Lvars.one_use lvar then
		      let val e = specialize_bind {lvar=lvar,tyvars=tyvars,Type=Type,bind=bind} instances lamb2
		      in decr_use lvar; decr_uses lamb2; incr_uses e; tick ("reduce - fix-spec." ^ Lvars.pr_lvar lvar); 
			reduce (env, (e, CUNKNOWN))
		      end
		    else fail
		   | _ => fail)
	  | APP(FIX{functions=functions as [{lvar,...}], scope=f as VAR{lvar=lv_f,...}}, e) => 
	      if Lvars.eq(lvar,lv_f) then
		(tick "reduce - app-fix"; (FIX{functions=functions,scope=APP(f,e)}, CUNKNOWN))
	      else fail
	  | SWITCH_I switch => reduce_switch (reduce, env, fail, switch)
	  | SWITCH_S switch => reduce_switch (reduce, env, fail, switch)
	  | SWITCH_R switch => reduce_switch (reduce, env, fail, switch)
	  | SWITCH_C switch => reduce_switch (reduce, env, fail, switch)
	  | SWITCH_E switch => reduce_switch (reduce, env, fail, switch)
	  | _ => fail


      (* -----------------------------------------------------------------
       * Contract on switch
       * ----------------------------------------------------------------- *)

      fun contr_switch (contr, reduce, env, SW, SWITCH(arg, sel, opt)) =
	let val arg' = fst (contr (env,arg))
	    val mix = map (fn (a,e) => (a, contr (env,e))) sel 
	    val sel' = map (fn (a,(e,_)) => (a, e)) mix
	    val cvs = map (fn (_, (_,cv)) => cv) mix
	in case opt
	     of Some lamb =>
	       let val (lamb',cv') = contr (env,lamb)
		   val cv = lubList (cv'::cvs) 
	       in reduce (env, (SW(SWITCH(arg',sel',Some lamb')), cv))
	       end
	      | None =>
	       let val cv = lubList cvs
	       in reduce (env, (SW(SWITCH(arg',sel',None)), cv))
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
		               (map (fn lvar => ([], CVAR (VAR{lvar=lvar,instances=[]}))) lvars) env  
		   val (body',_) = contr (env', body)
	       in (FN{pat=pat,body=body'},CUNKNOWN)
	       end
	      | LET{pat=(pat as [(lvar,tyvars,tau)]),bind,scope} =>
	       let val (bind', cv) = contr (env, bind)
		   val cv' = if is_small_closed_fn bind' then CFN{lexp=bind',large=false}
			     else if is_fn bind' then CFN{lexp=bind',large=true}
                                  else (case bind' of VAR _ => CVAR bind' 
                                        | _ => cv)
		   val env' = LvarMap.add(lvar,(tyvars,cv'),env)
		   val (scope',cv_scope) = contr (env', scope)
		   val cv_scope' = remove lvar cv_scope
	       in reduce (env, (LET{pat=pat,bind=bind',scope=scope'}, cv_scope'))
	       end
	      | PRIM(RECORDprim, lambs) =>
	       let val lamb_cv = map (fn e => contr (env,e)) lambs
	       in (PRIM(RECORDprim, map fst lamb_cv),CRECORD (map snd lamb_cv))
	       end 
	      | PRIM(prim as EXCONprim excon, lambs) => 
	       let val lambs' = map (fst o (fn e => contr (env, e))) lambs
	       in (mk_live_excon excon; (PRIM(prim, lambs'), CUNKNOWN))
	       end
	      | PRIM(prim as DEEXCONprim excon, lambs) => 
	       let val lambs' = map (fst o (fn e => contr (env,e))) lambs
	       in (mk_live_excon excon; (PRIM(prim, lambs'), CUNKNOWN))
	       end
	      | PRIM(RESET_REGIONSprim _, [VAR _]) => (lamb, CUNKNOWN) (* Sweden: avoid in-lining of variable *)
	      | PRIM(prim,lambs) => (PRIM(prim,map (fst o (fn e => contr (env,e))) lambs),CUNKNOWN)
	      | FIX{functions,scope} =>
	       let val lvs = map #lvar functions
		   val env0 = updateEnv lvs (map (fn _ => ([],CUNKNOWN)) functions) env
		   val _ = app mark_lvar lvs
		   val functions' = map (fn {lvar,tyvars,Type,bind} =>
					 {lvar=lvar,tyvars=tyvars,Type=Type,
					  bind=fst (contr (env0, bind))}) functions
		   val _ = app unmark_lvar lvs
		   val env' = case functions
				of [function as {lvar,tyvars,Type,bind}] => 
				  let val cv = if !specialize_recursive_functions andalso specializable function then 
				                  CFIX{Type=Type,bind=bind,large=not(small_lamb max_inline_size_fix bind)} 
					       else CUNKNOWN
				  in updateEnv [lvar] [(tyvars,cv)] env
				  end
				 | _ => updateEnv lvs (map (fn {tyvars,...} => (tyvars,CUNKNOWN)) functions) env
		   val (scope', cv) = contr (env', scope)
		   val cv' = removes lvs cv
	       in reduce (env, (FIX{functions=functions', scope=scope'}, cv'))
	       end
	      | APP(lamb1, lamb2) => 
	       let val lamb1' = fst(contr (env, lamb1))
		   val lamb2' = fst(contr (env, lamb2))
	       in reduce (env, (APP(lamb1',lamb2'), CUNKNOWN))
	       end
	      | EXCEPTION(excon,tauOpt,lamb) => 
	       let val (lamb', cv) = contr (env, lamb)
	       in if is_live_excon excon then (EXCEPTION(excon,tauOpt, lamb'), cv)
		  else (tick "dead - excon"; (lamb', cv))
	       end
	      | RAISE(lamb,tl) => (RAISE(fst(contr (env, lamb)),tl),CUNKNOWN)
	      | HANDLE(lamb1, lamb2) => (HANDLE(fst(contr (env, lamb1)), fst(contr (env, lamb2))),CUNKNOWN)
	      | SWITCH_I switch => contr_switch (contr, reduce, env, SWITCH_I, switch)
	      | SWITCH_S switch => contr_switch (contr, reduce, env, SWITCH_S, switch)
	      | SWITCH_R switch => contr_switch (contr, reduce, env, SWITCH_R, switch)
	      | SWITCH_C switch => contr_switch (contr, reduce, env, SWITCH_C, switch)
	      | SWITCH_E switch => 
	       let val res = contr_switch (contr, reduce, env, SWITCH_E, switch)
		   val SWITCH(_,sel,_) = switch
		   fun mklive [] = ()
		     | mklive ((excon,_)::rest) = (mk_live_excon excon; mklive rest)
	       in mklive sel; res
	       end
	      | FRAME{declared_excons,...} => (app (mk_live_excon o #1) declared_excons; (lamb, CUNKNOWN)) 
	      | _ => (lamb, cv)
	end
    in 
      fun contract lamb =
	if is_on "contract" then
	  let val _ = log "contracting\n"
	      val _ = reset_excon_bucket()
	      val _ = init lamb
	      val lamb' = fst (contr (LvarMap.empty, lamb))
	      val _ = reset_lvar_bucket ()
	      val _ = reset_excon_bucket()
	  in lamb'
	  end
	else lamb
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
               of LET{pat=[(lvar,[],_)],bind=PRIM(RECORDprim,lambs),scope} => 
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
	  of PRIM(SELECTprim i, [VAR{lvar,instances=[]}]) =>
	    (case LvarMap.lookup env lvar
	       of Some lvars =>
		 let val lvar' = List.nth i lvars
                                 handle List.Subscript _ => die "eliminate_explicit_records"
		 in tick "eliminate explicit records - select"; 
                    VAR{lvar=lvar',instances=[]}
		 end
		| None => lamb)
	   | LET{pat=[(lvar,[],Type)],bind=PRIM(RECORDprim, lambs),scope} =>
              if is_marked_lvar lvar then
                let val lvars = map (fn _ => Lvars.newLvar()) lambs
                    val env' = LvarMap.add(lvar,lvars,env)
                    val taus = case Type
                                 of RECORDtype taus => taus
                                  | _ => die "eliminate_explicit_records2"
                    fun mk_lamb [] [] [] = transf env' scope
                      | mk_lamb (lv::lvs) (tau::taus) (lamb::lambs) =
                          LET{pat=[(lv,[],tau)],bind=transf env lamb,scope=mk_lamb lvs taus lambs}
                      | mk_lamb _ _ _ = die "eliminate_explicit_records3"
                in tick "eliminate explicit records - binding";
                   mk_lamb lvars taus lambs
                end
              else map_lamb (transf env) lamb
           | _ => map_lamb (transf env) lamb
   in
     fun eliminate_explicit_records lamb =
       if is_on "elim_explicit_records" then
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
     type fs = {lvar:lvar,tyvars:tyvar list,Type:Type,bind:LambdaExp}
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
       let fun on_f (IS : lvar -> Type list Option) ({lvar,tyvars,Type,bind}:fs) : fs =
	     let fun on_bind (lamb as VAR{lvar,instances=[]}) =
	           (case IS(lvar)
		      of Some instances => VAR{lvar=lvar,instances=instances}
		       | None => lamb)
		   | on_bind lamb = map_lamb on_bind lamb
	     in {lvar=lvar,tyvars=tyvars,Type=Type,bind=on_bind bind}
	     end
	   fun extend_IS IS c =
	     let fun ext [] lv = IS(lv)
		   | ext (({lvar,tyvars,...}:fs)::c) lv = if Lvars.eq(lvar,lv) then Some (map TYVARtype tyvars) 
							  else ext c lv
	     in ext c
	     end
	   fun on_scc (IS : lvar -> Type list Option) [] = []
	     | on_scc IS (c::cs) =
	     let val c' = map (on_f IS) c
	       val IS' = extend_IS IS c'
	       val cs' = on_scc IS' cs
	     in c'::cs'
	     end
       in on_scc (fn _ => None) scc
       end

     (* Rename bound type variables of strongly connected component *) 
     fun rename_btvs c =
        let fun get_tyvars [] tyvars = tyvars
	      | get_tyvars (({tyvars,...}:fs)::c) tyvars' = 
	        let fun add [] tyvars = tyvars
		      | add (tv::tvs) tyvars = if List.member tv tyvars then add tvs tyvars
					       else add tvs (tv::tyvars)
		in add tyvars (get_tyvars c tyvars')
		end
	    fun fresh_tv tv = if equality_tyvar tv then fresh_eqtyvar ()
			      else fresh_tyvar ()

	    fun on_tyvar S tv =
	      case on_Type S (TYVARtype tv)
		of TYVARtype tv' => tv'
		 | _ => die "on_tyvar"
 
	    fun on_c S [] = []
	      | on_c S (({lvar,tyvars,Type,bind}:fs)::c) =
	      let val tyvars' = map (on_tyvar S) tyvars
		  val Type' = on_Type S Type
		  val bind' = on_LambdaExp S bind
	      in {lvar=lvar,tyvars=tyvars',Type=Type',bind=bind'}::on_c S c
	      end

	    val tyvars = get_tyvars c []
	    val types = map (TYVARtype o fresh_tv) tyvars 
	    val S = mk_subst "rn_btvs_c" (tyvars, types)
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
     fun minimize_fixs lamb = 
       let fun min_fixs (FIX fs) = map_lamb min_fixs (FIX (minimize_fix fs))
	     | min_fixs lamb = map_lamb min_fixs lamb
       in if is_on "minimize_fixs" then min_fixs lamb
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
		  FIX{functions=[{lvar=lvar,tyvars=tyvars,Type=Type,bind=bind}],
		      scope=scope})
	   | f lamb = lamb
     in
       if is_on "fix_conversion" then (log "fix_conversion\n"; passTD f lamb)
       else lamb
     end


   (* -----------------------------------------------------------------
    * functionalise_let let_env lamb 
    *   ensure that all let-constructs, binding other things than
    *   lambdas are not polymorphic; this is done by translating
    *   polymorphic non-functional let-constructs into functional ones
    *   (this is ok. since polymorphism is only allowed for
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
		       of Some res1 => res1=res2
			| None => false) true let_env2

     fun restrict_let_env(let_env,lvars) = 
       List.foldL (fn lv => fn acc => 
		   case LvarMap.lookup let_env lv
		     of Some res => LvarMap.add(lv,res,acc)
		      | None => die "restrict_let_env.lv not in env") LvarMap.empty lvars 

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
	 of v as VAR{lvar,instances} =>
	   (case LvarMap.lookup env lvar
	      of Some DELAY_SIMPLE => APP(v, PRIM(RECORDprim, []))
	       | _ => v)
	  | LET{pat,bind,scope} => 
	      (case pat
		 of [(lvar,tyvars,Type)] =>
	          if tyvars = [] orelse is_fn_or_var bind then 
		    LET{pat=pat,bind=f env bind, scope=f (add_lv(lvar,IGNORE,env)) scope}
		  else let val Type' = ARROWtype([unit_Type], [Type])
			   val pat' = [(lvar,tyvars,Type')]
			   val bind' = FN{pat=[(Lvars.newLvar(),unit_Type)],body=f env bind}
			   val scope' = f (LvarMap.add(lvar,DELAY_SIMPLE,env)) scope
		       in LET{pat=pat',bind=bind',scope=scope'}
		       end
		  | _ => die "functionalise_let. non-trivial patterns unimplemented.") 
	  | FIX{functions,scope} =>
		 let val functions' = map (fn {lvar,tyvars,Type,bind} => 
					   {lvar=lvar,tyvars=tyvars,Type=Type,bind=f env bind}) functions
		     val lvars = map #lvar functions
		     val env' = List.foldL (fn lv => fn acc => add_lv(lv,IGNORE,acc)) env lvars
		 in FIX{functions=functions', scope=f env' scope}
		 end 
	  | FRAME {declared_lvars,...} => 
	      let val env' = List.foldR (fn {lvar,...} => fn env' =>
				       case lookup env lvar
					 of Some p => add_lv (lvar,p,env')
					  | None => die ("functionalise_let.FRAME.lvar " ^ 
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
      List.foldL(fn lv => fn acc => 
		 case LvarMap.lookup inveta_env lv
		   of Some res => LvarMap.add(lv,res,acc)
		    | None => die "restrict_inv_eta_env.lv not in env") LvarMap.empty lvars

    fun new_sigma ([],tau) = ([],tau)
      | new_sigma (tyvars,tau) =
      let fun new_tv tv = if equality_tyvar tv then fresh_eqtyvar()
			  else fresh_tyvar()
	  val tyvars' = map new_tv tyvars
	  val S = mk_subst "new_sigma" (tyvars,map TYVARtype tyvars')
      in (tyvars', on_Type S tau)
      end

    fun eq_sigma (([],tau1),([],tau2)) = eq_tau(tau1,tau2)
      | eq_sigma (sigma1 as (tyvars1,_),sigma2 as (tyvars2,_)) =
      List.size tyvars1 = List.size tyvars2 andalso
      let val (tyvars1,tau1) = new_sigma sigma1
	  val (tyvars2,tau2) = new_sigma sigma2
	  val S = mk_subst "eq_sigma" (tyvars1,map TYVARtype tyvars2)
	  val tau1' = on_Type S tau1
      in eq_tau(tau1',tau2)
      end
	    

    fun eq_inveta_res (FIXBOUND sigma1, FIXBOUND sigma2) = eq_sigma(sigma1,sigma2)
      | eq_inveta_res (NOTFIXBOUND, NOTFIXBOUND) = true
      | eq_inveta_res _ = false
    fun enrich_inv_eta_env(inveta_env1,inveta_env2) =
      LvarMap.Fold(fn ((lv2,res2),b) => b andalso
		   case LvarMap.lookup inveta_env1 lv2
		      of Some res1 => eq_inveta_res(res1,res2)
		       | None => false) true inveta_env2
 
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
            of VAR {lvar=lvar',instances=instances'} =>
	      (case LvarMap.lookup env lvar' 
	         of Some (FIXBOUND(tyvars,Type)) => 
		   let val lv = Lvars.newLvar()
		       val _ = if !Flags.DEBUG_OPTIMISER then
			         log ("inverse_eta: " ^ (Lvars.pr_lvar lvar'))
		               else ()
                       val subst = case instances' of [] => mk_subst "inverse_eta" ([],[]) 
                                      | _ => mk_subst "inverse_eta" (tyvars,instances')
                        (* The above case analysis caters for the fact that the 
                         * instances may be empty, if this occurrence of lvar' is
                         * on the rhs of a val rec which declares lvar' *)
		       val tau_lv = case on_Type subst Type 
                                      of ARROWtype([tau1],_) => tau1
	                               | _ => die "inverse_eta --- fix bound lvar of non-function type"
		   in FN{pat=[(lv,tau_lv)], body=APP(lamb,VAR{lvar=lv,instances=[]})}
		   end                 
		  | _ => lamb) 
	     | LET{pat,bind,scope} => 
		 let val bind' = inverse_eta env bind
		     val lvars = map #1 pat
		     val env' = List.foldL(fn lv => fn acc => LvarMap.add(lv,NOTFIXBOUND,acc)) env lvars
		 in LET{pat=pat,bind=bind',scope=inverse_eta env' scope}
		 end
	     | FIX{functions,scope} =>
	      let val env' = List.foldR (fn {lvar, tyvars, Type, ...} => fn env =>
				         LvarMap.add(lvar, FIXBOUND(tyvars, Type), env)) env functions
	      in FIX{functions=map (fn {lvar,tyvars,Type,bind} =>
				   {lvar=lvar,tyvars=tyvars,Type=Type,
				    bind=inverse_eta env' bind}) functions,
		     scope=inverse_eta env' scope}
	      end
	     | APP(x as VAR _,lamb) => APP(x,inverse_eta env lamb)
	     | FRAME {declared_lvars,...} => 
	      let val env' = List.foldR (fn {lvar,...} => fn env' =>
					 case LvarMap.lookup env lvar
					   of Some res => LvarMap.add(lvar,res,env')
					    | None => die "inverse_eta.FRAME.lv not in env")
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

    fun optimise lamb =
      let val loop_opt = eliminate_explicit_records o contract

	  fun loop n lamb = if n > max_optimise then lamb 
			    else let val _ = reset_tick()
				     val _ = log ("Pass number " ^ Int.string (n+1) ^ "\n")
				     val lamb' = loop_opt lamb
				     val _ = end_round() (*stat*)
				 in if test_tick() then loop (n+1) lamb'
				    else lamb'
				 end

	  val pre_opt = minimize_fixs

	  val _ = reset_statistics()
	  val lamb' = loop 0 (pre_opt lamb)
	  val _ = print_stat lamb lamb'
      in lamb'
      end


   (* -----------------------------------------------------------------
    * The lambda optimiser environment
    * ----------------------------------------------------------------- *)

    type env = inveta_env * let_env
    val empty_env =  (LvarMap.empty, LvarMap.empty)
    val initial_env = empty_env
    fun plus ((e1, e2), (e1', e2')) = (LvarMap.plus (e1,e1'), LvarMap.plus (e2,e2'))

    fun restrict((inv_eta_env,let_env), lvars) =
      (restrict_inv_eta_env(inv_eta_env,lvars),
       restrict_let_env(let_env,lvars))
    fun enrich((inv_eta_env1,let_env1),(inv_eta_env2,let_env2)) =
      enrich_inv_eta_env(inv_eta_env1,inv_eta_env2) andalso
      enrich_let_env(let_env1,let_env2)

    fun layout_env (e1,e2) = PP.NODE{start="",finish="",indent=0,childsep=PP.RIGHT ",",
				     children=[layout_inveta_env e1, layout_let_env e2]}



   (* -----------------------------------------------------------------
    * Rewriting: This rewriting shall always be performed, no matter
    * whether the lamb is optimised or not, and it shall always be
    * performed after possibly optimisation
    * ----------------------------------------------------------------- *)

    fun rewrite (inveta_env,let_env) lamb =
      let val (lamb1,let_env') = functionalise_let let_env lamb
	  val lamb2 = fix_conversion lamb1 
	  val (lamb3,inveta_env') = inverse_eta_for_fix_bound_lvars inveta_env lamb2
      in (lamb3, (inveta_env', let_env'))
      end


   (* -----------------------------------------------------------------
    * The Optimiser
    * ----------------------------------------------------------------- *)

    val optimise = fn (env, pgm as (PGM(DATBINDS db,lamb))) =>
          let
	    val (lamb', env') = 
	          rewrite env (if !Flags.optimiser then optimise lamb else lamb)
	  in
	    (PGM(DATBINDS db, lamb'), env')
	  end

  end;
