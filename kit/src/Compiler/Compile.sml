
(*$Compile: DEC_GRAMMAR TOPDEC_GRAMMAR EXCON LVARS COMPILE_BASIS
        REPORT FLAGS PRETTYPRINT FINMAP LAMBDA_EXP COMPILER_ENV
        COMPILE_DEC OPT_LAMBDA CRASH SPREAD_EXPRESSION COMP_LAMB
        KAM_BACKEND LAMBDA_STAT_SEM ELIMINATE_EQ COMPILE TIMING EFFECT
        RTYPE SPREAD_DATATYPE REGION_EXP REGINF MUL_INF MUL_EXP MUL
        AT_INF DROP_REGIONS PHYS_SIZE_INF NAME 
        REGION_FLOW_GRAPH_PROFILING*)

functor Compile(structure DecGrammar: DEC_GRAMMAR
		structure TopdecGrammar: TOPDEC_GRAMMAR
		  sharing type TopdecGrammar.dec = DecGrammar.dec
		      and type DecGrammar.info = TopdecGrammar.info

		structure Excon : EXCON
		structure FinMap: FINMAP

		structure Lvars : LVARS
		structure LambdaExp: LAMBDA_EXP
		  sharing type LambdaExp.lvar = Lvars.lvar

	        structure LambdaStatSem : LAMBDA_STAT_SEM
		  sharing type LambdaStatSem.LambdaPgm = LambdaExp.LambdaPgm

	        structure EliminateEq : ELIMINATE_EQ 
		  sharing type EliminateEq.LambdaPgm = LambdaExp.LambdaPgm

                structure RType : RTYPE

                structure RegionExp: REGION_EXP

		structure Effect: EFFECT
		  sharing type Effect.effect = RegionExp.effect

                structure SpreadExp: SPREAD_EXPRESSION
		  sharing SpreadExp.E = LambdaExp
		      and SpreadExp.E' = RegionExp
                      and type SpreadExp.place = Effect.effect = Effect.effect
                      and type SpreadExp.cone = Effect.cone = SpreadExp.RegionStatEnv.cone
		      and type SpreadExp.RegionStatEnv.TypeAndPlaceScheme = RegionExp.sigma
		      and type SpreadExp.RegionStatEnv.excon = RegionExp.excon
		      and type SpreadExp.RegionStatEnv.lvar = RegionExp.lvar
		      and type SpreadExp.RegionStatEnv.place = RegionExp.place
		      and type SpreadExp.RegionStatEnv.Type = RegionExp.Type

                structure RegInf: REGINF
		  sharing type RegInf.cone = Effect.cone
		      and type RegInf.rse = SpreadExp.RegionStatEnv.regionStatEnv
		      and type RegInf.trip = RegionExp.trip
		      and type RegInf.place = RType.place = SpreadExp.place = RegionExp.place

		structure Mul: MUL
		structure MulInf: MUL_INF
		  sharing type MulInf.place = Effect.place
		      and type MulInf.cone = RegInf.cone
		      and type MulInf.efenv = Mul.efenv
		      and type MulInf.mularefmap = Mul.mularefmap (*Psi*)
		      and type MulInf.LambdaPgm_phi = RegionExp.LambdaPgm

		structure MulExp: MUL_EXP
		  sharing type MulInf.LambdaPgm_psi = MulExp.LambdaPgm
		      and type MulExp.place = MulInf.place
		      and type MulExp.mul = MulInf.mul
                      and type MulExp.regionStatEnv= SpreadExp.RegionStatEnv.regionStatEnv

		structure AtInf: AT_INF
		  sharing type AtInf.LambdaPgm = MulExp.LambdaPgm
		      and type AtInf.place = Effect.place
		      and type AtInf.mul = MulExp.mul
		      and type AtInf.qmularefset = MulInf.qmularefset = MulExp.qmularefset
			      
		structure DropRegions: DROP_REGIONS
		  sharing type DropRegions.at = AtInf.at
		      and type DropRegions.place = Effect.place
		      and type DropRegions.mul = MulExp.mul
		      and type DropRegions.LambdaPgm = MulExp.LambdaPgm

		structure PhysSizeInf : PHYS_SIZE_INF
		  sharing type PhysSizeInf.at = AtInf.at
		      and type PhysSizeInf.place = Effect.place
		      and type PhysSizeInf.LambdaPgm = MulExp.LambdaPgm
		      and type PhysSizeInf.mul = MulExp.mul

		structure RegionFlowGraphProfiling : REGION_FLOW_GRAPH_PROFILING
		sharing type RegionFlowGraphProfiling.place = PhysSizeInf.place
		    and type RegionFlowGraphProfiling.at = AtInf.at
		    and type RegionFlowGraphProfiling.phsize = PhysSizeInf.phsize
		    and type RegionFlowGraphProfiling.pp = PhysSizeInf.pp

		structure CompLamb : COMP_LAMB
		  sharing type CompLamb.at = AtInf.at
		      and type CompLamb.LambdaPgm = PhysSizeInf.LambdaPgm
		      and type CompLamb.place = PhysSizeInf.place
		      and type CompLamb.phsize = PhysSizeInf.phsize
		      and type CompLamb.pp = PhysSizeInf.pp

		structure CompilerEnv: COMPILER_ENV
		  sharing type CompilerEnv.lvar = LambdaExp.lvar
                      and type CompilerEnv.excon = LambdaExp.excon = Excon.excon
                      and type CompilerEnv.Type = LambdaExp.Type

                structure CompileDec: COMPILE_DEC
		  sharing type CompileDec.dec = DecGrammar.dec
                      and type CompileDec.LambdaExp = LambdaExp.LambdaExp
                      and type CompileDec.LambdaPgm = LambdaExp.LambdaPgm
                      and type CompileDec.CEnv = CompilerEnv.CEnv

                structure OptLambda: OPT_LAMBDA
		  sharing type OptLambda.LambdaPgm = LambdaExp.LambdaPgm

                structure KAMBackend : KAM_BACKEND
		  sharing type CompLamb.EA = KAMBackend.EA
		      and type CompLamb.code = KAMBackend.code
		      and type CompLamb.linkinfo = KAMBackend.linkinfo

		structure CompileBasis: COMPILE_BASIS
		  sharing type CompileBasis.CEnv = CompilerEnv.CEnv
		      and type CompileBasis.EqEnv = EliminateEq.env
		      and type CompileBasis.OEnv = OptLambda.env
		      and type CompileBasis.TCEnv = LambdaStatSem.env 
                      and type CompileBasis.rse = SpreadExp.RegionStatEnv.regionStatEnv
		      and type CompileBasis.drop_env = DropRegions.env
		      and type CompileBasis.psi_env = PhysSizeInf.env
                      and type CompileBasis.l2kam_ce = CompLamb.env 
	       	      and type CompileBasis.mulenv = MulInf.efenv
                      and type CompileBasis.mularefmap = MulInf.mularefmap

                structure Report: REPORT
		structure Flags: FLAGS

		structure PP: PRETTYPRINT
		  sharing type CompileBasis.StringTree
                                     = LambdaExp.StringTree
			             = EliminateEq.StringTree
                                     = CompilerEnv.StringTree
                                     = PP.StringTree
                                     = DecGrammar.StringTree
                                     = SpreadExp.RegionStatEnv.StringTree
                                     = Effect.StringTree
                                     = RegionExp.StringTree
                                     = MulExp.StringTree
                                     = MulInf.StringTree
                                     = TopdecGrammar.StringTree
			             = AtInf.StringTree
			             = PhysSizeInf.StringTree
		                     = RegionFlowGraphProfiling.StringTree
                      and type PP.Report = Report.Report
			
	        structure Name : NAME

                structure Crash: CRASH
		structure Timing: TIMING
		  ): COMPILE =

  struct
    type CompileBasis = CompileBasis.CompileBasis
    type topdec = TopdecGrammar.topdec
    type linkinfo = KAMBackend.linkinfo
    type basisinfo = (int*KAMBackend.EA) list

    fun die s = Crash.impossible ("CompileAndRun."^s)

    (* ---------------------------------------------------------------------- *)
    (*  Dynamic Flags.                                                        *)
    (* ---------------------------------------------------------------------- *)

    val print_physical_size_inference_expression =
          Flags.lookup_flag_entry "print_physical_size_inference_expression"
    val print_call_explicit_expression =
          Flags.lookup_flag_entry "print_call_explicit_expression"
    val print_program_points = Flags.lookup_flag_entry "print_program_points"


    (* ---------------------------------------------------------------------- *)
    (*  Printing utilities                                                    *)
    (* ---------------------------------------------------------------------- *)

    fun pr0 st log = (Report.print' (PP.reportStringTree st) log; 
                      NonStandard.flush_out log)
    fun pr st = pr0 st (!Flags.log)

    fun length l = List.foldR (fn _ => fn n => n+1) 0 l

    fun msg(s: string) = (output(!Flags.log, s); NonStandard.flush_out (!Flags.log))

    fun chat(s: string) = if !Flags.chat then msg s else ()

    fun fast_pr stringtree = 
           (PP.outputTree ((fn s => output(!Flags.log, s)) , stringtree, !Flags.colwidth);
            output(!Flags.log, "\n"))

    fun display(title, tree) =
        fast_pr(PP.NODE{start=title ^ ": ",
                   finish="",
                   indent=3,
                   children=[tree],
                   childsep=PP.NONE
                  }
          )

    fun ifthen e f = if e then f() else ()
    fun footnote(x, y) = x
    infix footnote

    fun printCEnv ce =
      ifthen (!Flags.DEBUG_COMPILER)
      (fn _ => display("CompileAndRun.CEnv", CompilerEnv.layoutCEnv ce))


    (* ---------------------------------------------------------------------- *)
    (*  Abbreviations                                                         *)
    (* ---------------------------------------------------------------------- *)
      
    val layoutLambdaPgm = LambdaExp.layoutLambdaPgm 
    fun layoutRegionPgm  x = (RegionExp.layoutLambdaPgm 
                            (if !Flags.print_regions then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
                             else fn _ => None)
                            (fn _ => None)) x
    fun layoutRegionExp x = (RegionExp.layoutLambdaExp 
                            (if !Flags.print_regions then (fn rho => Some(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
                             else fn _ => None)
                             (fn _ => None)) x

    type arity = int


    (* -----------------------------------------
     * Effect `recounter'; for normalisation
     * ----------------------------------------- *)

    local 
      val effect_init = ref 6   (* there are six free variables (global_regions) in init_rse. *)
      val effect_count = ref (!effect_init)
    in
      fun effect_counter() = (effect_count := !effect_count + 1; !effect_count)
      fun reset_effect_count() = effect_count := !effect_init
      fun commit_effect_count() = effect_init := !effect_count
    end


    (* --------------------------------------------
     * Program point counter
     * -------------------------------------------- *)

    local 
      val pp_init = ref 1   (* ~1 and 0 are reserved *)
      val pp_count = ref (!pp_init)
    in
      fun pp_counter() = (pp_count := !pp_count + 1; !pp_count)
      fun reset_pp_count() = pp_count := !pp_init
      fun commit_pp_count() = pp_init := !pp_count
    end


    (* ---------------------------------------------------------------------- *)
    (*  Generative Names.                                                     *)
    (* ---------------------------------------------------------------------- *)

    type name = Name.name
    val generative_names = ref ([] : name list)


    (* ---------------------------------------------------------------------- *)
    (*  CompileAndRun RESET                                                   *)
    (* ---------------------------------------------------------------------- *)
    fun reset () = (CompLamb.reset();
		    LambdaExp.reset();
		    reset_pp_count();
		    reset_effect_count();
                    Effect.reset();       (* resets global cone *)
		    CompileDec.reset())

    fun commit () = (CompLamb.commit();
		     commit_pp_count();
		     LambdaExp.commit();
		     commit_effect_count();
		     Effect.commit())
		     

    (* ---------------------------------------------------------------------- *)
    (*  Compile the declaration using old compiler environment, ce            *)
    (* ---------------------------------------------------------------------- *)

    fun ast2lambda(ce, dec) =
    (ifthen (!Flags.DEBUG_COMPILER ) (fn () => (*  Display the elaborated declaration *)
       display("After Elaboration:", DecGrammar.layoutDec dec));
     ifthen (!Flags.chat) (fn _ => msg("\nCompiling into lambda language ..."));
     Timing.timing_begin();
     let         
        open LambdaExp
        val (ce1, lambFn) = CompileDec.compileDec ce dec 
                                footnote Timing.timing_end("Com. dec.")
                                (* val _ = printCEnv ce1 *)

        (* Determine the scope of the declaration using returned compiler env, ce1  *)
        (* declared_lvars and excons are those lvars and excons that are 
           in the _range_ of ce1 *)

        val declared_lvars : lvar list = CompilerEnv.lvarsOfCEnv ce1
        val typed_declared_lvars = 
              (* we associated the declared_lvars with dummy type schemes;
                 the real type schemes are put in later *)
            let val alpha = fresh_tyvar()
            in
               map (fn lv => {lvar  = lv,
                              tyvars = [alpha],    (* forall alpha. alpha *)
                              Type = TYVARtype(alpha)
                             })
                   declared_lvars
            end
        val declared_excons : (Excon.excon * Type Option) list =
                 map (fn excon => (excon, None)) (CompilerEnv.exconsOfCEnv ce1)  (*dummy-None*)
        val scope =  FRAME{declared_lvars = typed_declared_lvars,
                           declared_excons = declared_excons}
        (*   Apply the returned function to the scope to obtain the lambda pgm    *)
        val lamb = lambFn scope (* a lambda program *)
    in  
        ifthen (!Flags.DEBUG_COMPILER) (fn _ => 
           display("Report: UnOpt", layoutLambdaPgm lamb));
        (lamb,ce1, declared_lvars, declared_excons) 
    end)

    (* ------------------------------------ *)
    (* isEmptyLambdaPgm lamb  returns true  *)
    (* if lamb is a pair of an empty list   *)
    (* of datatype bindings and an empty    *)
    (* frame. Then we need not generate any *)
    (* code.                                *)
    (* ------------------------------------ *)
    fun isEmptyLambdaPgm lamb =
      let open LambdaExp
      in case lamb
	   of PGM(DATBINDS [[]], FRAME {declared_lvars=[], declared_excons=[]}) => true
	    | _ => false
      end

    (* ---------------------------------------------------------------------- *)
    (*  Type check the lambda code                                             *)
    (* ---------------------------------------------------------------------- *)

    fun type_check_lambda (a,b) =
      if Flags.is_on "type_check_lambda" then
	(chat "\nType checking lambda term begin...\n";
	 Timing.timing_begin();
	 let 
	   val env' = Timing.timing_end_res ("Check lam.",(LambdaStatSem.type_check {env = a,  letrec_polymorphism_only = false,
                  pgm =  b}))
	 in
	   chat "\nType checking lambda term end...\n";
	   env'
	 end)
      else LambdaStatSem.empty


    (* ---------------------------------------------------------------------- *)
    (*   Eliminate polymorphic equality in the lambda code                    *)
    (* ---------------------------------------------------------------------- *)

    fun elim_eq_lambda (env,lamb) =
      if Flags.is_on "eliminate_polymorphic_equality" then
	(chat "\nEliminating polymorphic equality begin...\n";
	 Timing.timing_begin();
	 let val (lamb', env') = 
	   Timing.timing_end_res ("Elim. eq.", EliminateEq.elim_eq (env, lamb))
	 in
	   chat "\nEliminating polymorphic equality end...\n";
	   if !Flags.DEBUG_COMPILER then 
	     (display("Lambda Program After Elimination of Pol. Eq.", 
		      layoutLambdaPgm lamb');
	      display("Pol. Eq. Environment", EliminateEq.layout_env env'))
	   else ();
	   (lamb', env')
	 end)
      else (lamb, EliminateEq.empty)


    (* ---------------------------------------------------------------------- *)
    (*   Optimise the lambda code                                             *)
    (* ---------------------------------------------------------------------- *)

    fun optlambda (env, lamb) =
          (if !Flags.chat then 
             msg (if !Flags.optimiser
		  then "\nOptimising lambda term ..."
		  else "\nRewriting lambda term ...")
	   else ();
	   Timing.timing_begin();
	   let 
	     val (lamb_opt, env') = 
	           Timing.timing_end_res ("Opt. lam.", OptLambda.optimise(env,lamb))
	   in
	     if !Flags.DEBUG_COMPILER
	     then display("Report: Opt", layoutLambdaPgm lamb_opt) else () ;
	     (lamb_opt, env')
	   end)

    (* ---------------------------------------------------------------------- *)
    (*   Spread the optimised lambda code                                     *)
    (* ---------------------------------------------------------------------- *)

    fun spread(cone,rse, lamb_opt)=
        (chat "\nSpreading regions and effects (NEW)...";
         Timing.timing_begin();
         (*Profile.reset();
         Profile.profileOn();*)
         let val (cone,rse_con,spread_lamb) = SpreadExp.spreadPgm(cone,rse, lamb_opt)
         in Timing.timing_end("Spread exp. (NEW)");
            (*Profile.profileOff();
            output(!Flags.log, "\n PROFILING OF S\n\n");
            Profile.report(!Flags.log);*)
            if !Flags.DEBUG_COMPILER 
            then (display("\nReport: Spread; program", layoutRegionPgm spread_lamb) ;
                  display("\nReport: Spread; entire cone after Spreading", Effect.layoutCone cone) )
            else ();
            
            (cone,rse_con, spread_lamb)
         end) 


    (* ---------------------------------------------------------------------- *)
    (*   Do the region inference on the spread optimised lambda code  (NEW)   *)
    (* ---------------------------------------------------------------------- *)

    fun inferRegions(cone,rse, rse_con,  spread_lamb as RegionExp.PGM 
                        {expression = spread_lamb_exp,
                         export_datbinds = datbinds,
                         export_basis=export_basis  (* list of region variables and arrow effects *)
                        }) = 
    let
        val _ = (chat "\nInferring regions and effects ...(NEW)";
		 Timing.timing_begin()
                 (*;Profile.reset()
                 ;Profile.profileOn()*))
        val rse_with_con = SpreadExp.RegionStatEnv.plus(rse,rse_con)
        val cone = RegInf.inferEffects
                   (fn s => (output(!Flags.log, s); flush_out(!Flags.log)))
                   (cone,rse_with_con, spread_lamb_exp)
        val new_layer = Effect.topLayer cone (* to get back to level of "cone" *)

	val toplevel = Effect.level Effect.initCone
	val cone = List.foldL (fn effect => fn cone =>
			       Effect.lower toplevel effect cone) cone new_layer

(*	val _ = print "\n*** Unifying toplevel regions and effects ***\n" *)
	val new_layer = (Effect.unify_with_toplevel_rhos_eps new_layer; [])

        val _ = Timing.timing_end("Reg. Inf. (NEW)")
     (*   val _ = (Profile.profileOff();
                output(!Flags.log, "\n PROFILING OF R\n\n");
                Profile.report(!Flags.log));
     *)

        val pgm' = RegionExp.PGM{expression = spread_lamb_exp, (*side-effected*)
                      export_datbinds = datbinds, (*unchanged*)
                      export_basis= new_layer  (* list of region variables and arrow effects *)}

	(* call of normPgm no longer commented out; mads *)
        val _ = if Flags.is_on "region_profiling" then
	          ()
		else
		  (reset_effect_count();      (* inserted; mads *)
		   RegionExp.normPgm(pgm',effect_counter) 
		   )

	val rse' =
	  case spread_lamb_exp
	    of RegionExp.TR(_,RegionExp.Frame{declared_lvars,declared_excons},_) =>
	      (let val rse_temp = 
		 List.foldL (fn {lvar,compound,create_region_record,sigma, place} => fn rse =>
			     SpreadExp.RegionStatEnv.declareLvar(lvar,(compound, 
							   create_region_record, !sigma, place, 
							   Some(ref[]) (* reset occurrences *), None
							 ), rse)) rse_con declared_lvars
	       in
		 List.foldL (fn (excon, Some(Type, place)) => (fn rse =>
							       SpreadExp.RegionStatEnv.declareExcon(excon,(Type,place),rse))
			      | _ => die "rse.excon") rse_temp declared_excons

	       end handle _ => die "cannot form rse'")
	     | _ => die "program does not have type frame"
    in 
        if !Flags.DEBUG_COMPILER 
            then display("\nReport: After Region Inference (NEW)", layoutRegionPgm pgm') 
        else ();
       (cone,rse',pgm')       (* rse' contains rse_con *)
    end

    (* ---------------------------------------------------------------------- *)
    (*   Multiplicity Inference                                       (NEW)   *)
    (* ---------------------------------------------------------------------- *)

    fun mulInf(program_after_R:(Effect.place,unit)RegionExp.LambdaPgm, Psi, cone, mulenv) =
    let

        val _ = (chat "\nInferring multiplicities ...(NEW)";
                Timing.timing_begin()
                (*;Profile.reset()
                ;Profile.profileOn()*) )
        val (pgm', mulenv',Psi') = MulInf.mulInf(program_after_R,Psi,cone,mulenv)

        val _ = Timing.timing_end("Multiplicity Inference (NEW)")

(*        val _ = ( Profile.profileOff()(*;
                output(!Flags.log, "\n PROFILING OF MulInf\n\n");
                Profile.report(!Flags.log)*));
*)
    in 
        if !Flags.DEBUG_COMPILER 
            then (display("\nReport: After Multiplicity Inference (NEW), the program is:\n",
                   	    MulInf.layoutLambdaPgm pgm'))
            else ();

        (pgm', mulenv', Psi')
    end
    


    (* ---------------------------------------------------------------------- *)
    (*  Spread Expression, Region Inference and Multiplicity Inference (NEW)  *)
    (* We now connect the three phases above. No cone is needed in the        *)
    (* compiler (dynamic) basis. Instead, a cone is built from the initial    *)
    (* rse, and lives throughout all three phases. All generated, free nodes  *)
    (* of a lambda program are lowered to have level toplevel; i.e. 1.        *)
    (* ---------------------------------------------------------------------- *)

    fun SpreadRegMul(rse, Psi, mulenv, opt_pgm) =
      let val cone = Effect.push (SpreadExp.RegionStatEnv.mkConeToplevel rse)
	  val (cone, rse_con, spread_pgm) = spread(cone,rse,opt_pgm)
	  val (cone, rse', reginf_pgm) = inferRegions(cone,rse,rse_con,spread_pgm)
	  val (mul_pgm, mulenv', Psi') = mulInf(reginf_pgm,Psi,cone,mulenv)
      in (mul_pgm, rse', mulenv', Psi')
      end


    (* ---------------------------------------------------------------------- *)
    (*   Perform K-normalisation                                       (NEW)  *)
    (* ---------------------------------------------------------------------- *)

    local open MulInf
    in fun k_norm(pgm: (place,place*mul,qmularefset ref)LambdaPgm_psi) 
	: (place,place*mul,qmularefset ref)LambdaPgm_psi =
	(chat "\nK-normalisation (NEW)...\n";
         Timing.timing_begin();
         let val pgm' = MulInf.k_normPgm pgm
	 in Timing.timing_end("K-norm (NEW)");
(*KILL 29/03/1997 19:29. tho.:
	    if Flags.is_on "print_K_normal_forms" 
               orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: K-normalised program:", layoutLambdaPgm pgm')
	    else ();
*)
	    pgm'
	 end)
    end	

    (* ---------------------------------------------------------------------- *)
    (*   Do attop/atbot analysis                                       (NEW)  *)
    (* ---------------------------------------------------------------------- *)

    local open AtInf
    in fun storagemodeanalysis(pgm: (place,place*mul,qmularefset ref)LambdaPgm) 
	: (place at,place*mul,unit)LambdaPgm =
         (* chatting and timing done in AtInf*)
	 let val pgm' = sma pgm
	 in 
	    if Flags.is_on "print_attop_atbot_expression" 
               orelse !Flags.DEBUG_COMPILER then 
	          display("\nReport: ATTOP/ATBOT:", layout_pgm pgm')
	    else ();
	    pgm'
	 end
    end	


    (* ---------------------------------------------------------------------- *)
    (*   Do drop regions                                               (NEW)  *)
    (* ---------------------------------------------------------------------- *)
  
    local open DropRegions
    in
      val drop_regions : env*(place at,place*mul,unit)LambdaPgm -> (place at,place*mul,unit)LambdaPgm * env =
	fn (env, pgm) =>
	(chat "\nDrop Regions (NEW)...\n";
         Timing.timing_begin();
         let val (pgm',env') = drop_regions(env, pgm)
	 in Timing.timing_end("DROP (NEW)");
	    if Flags.is_on "print_drop_regions_expression" orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER DROP REGIONS:", AtInf.layout_pgm_brief pgm')
	    else ();
	    (pgm',env')
	 end)
    end


    (* ---------------------------------------------------------------------- *)
    (*   Do physical size inference                                    (NEW)  *)
    (* ---------------------------------------------------------------------- *)
  
    local open PhysSizeInf
   in
      fun phys_size_inf (env: env, pgm:(place at,place*mul,unit)LambdaPgm) 
	: ((place*pp)at,place*phsize,unit)LambdaPgm * env =
	(chat "\nPhysical Size Inference (NEW)...\n";
         Timing.timing_begin();
         let val (pgm',env') = psi(pp_counter, env, pgm)
	 in Timing.timing_end("PSI (NEW)");
	    if !print_physical_size_inference_expression orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER PHYSICAL SIZE INFERENCE (NEW):", layout_pgm pgm')
	    else ();
	    (pgm',env')
	 end)
    end


    (* ---------------------------------------------------------------------- *)
    (*   Do application conversion                                     (NEW)  *)
    (* ---------------------------------------------------------------------- *)
  
    local open PhysSizeInf
    in
      fun appConvert (pgm:((place*pp)at,place*phsize,unit)LambdaPgm): 
                          ((place*pp)at,place*phsize,unit)LambdaPgm =
	(chat "\nApplication Conversion (NEW)...\n";
         Timing.timing_begin();
         let val pgm' = PhysSizeInf.appConvert(pgm)
	 in Timing.timing_end("App Conv (NEW)");
	    if !print_call_explicit_expression orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER APPLICATION CONVERSION (NEW):", layout_pgm pgm')
	    else ();
	    pgm'
	 end)
    end

    (* ---------------------------------------------------------------------- *)
    (*   Compile region annotated code to KAM code (NEW)                      *)
    (* ---------------------------------------------------------------------- *)
    fun comp_lamb(l2kam_ce, pgm) = 
      let val _ = chat "\nCompiling region annotated lambda language (NEW)..."
	  val _ = Timing.timing_begin()
	  val (linkinfo, code, l2kam_ce') = CompLamb.comp_lamb(l2kam_ce, pgm)
	  val _ = Timing.timing_end("Com. Lam. (NEW)")
      in (linkinfo, code, l2kam_ce')
      end


    (* ===================================
     * Compile with the NEW backend
     * =================================== *)

    type target = KAMBackend.target
    fun comp_with_new_backend(rse, Psi, mulenv, drop_env, psi_env, l2kam_ce, lamb_opt, vcg_file) =
      let
	val (mul_pgm, rse1, mulenv1, Psi1) = SpreadRegMul(rse, Psi, mulenv, lamb_opt)
        val _ = MulExp.warn_puts(rse, mul_pgm)
        val k_mul_pgm = k_norm mul_pgm
	val sma_pgm = storagemodeanalysis k_mul_pgm
	val (drop_pgm, drop_env1) = drop_regions(drop_env,sma_pgm)
	val (psi_pgm, psi_env1) = phys_size_inf(psi_env, drop_pgm)
        val app_conv_psi_pgm = appConvert psi_pgm
	val _ = RegionFlowGraphProfiling.reset_graph ()
	val (linkinfo, code, l2kam_ce1) = comp_lamb(l2kam_ce, app_conv_psi_pgm) 

	(* Generate lambda code file with program points *)
	val old_setting = !print_program_points
	val _ = print_program_points := true
	val old_setting2 = !Flags.print_regions
	val _ = Flags.print_regions := true
	val _ = 
	  if Flags.is_on "generate_lambda_code_with_program_points" then
	    (display("\nReport: LAMBDA CODE WITH PROGRAM POINTS:", PhysSizeInf.layout_pgm psi_pgm);
	     display("\nReport: REGION FLOW GRAPH FOR PROFILING:", RegionFlowGraphProfiling.layout_graph()))
	  else ()
	val _ = print_program_points := old_setting
	val _ = Flags.print_regions := old_setting2

	(* Generate VCG graph *)
	val _ = if (Flags.is_on "generate_vcg_graph") then
                  let
	            val outStreamVCG = open_out vcg_file
		  in
		    (chat "\nGenerate VCG region flow graph for profiling...";
		     RegionFlowGraphProfiling.export_graph outStreamVCG;
		     close_out(outStreamVCG))
		  end
		else
		  ()

	val target = KAMBackend.generate_target_code(linkinfo, code)
      in
	(rse1, Psi1, mulenv1, drop_env1, psi_env1, l2kam_ce1, target, linkinfo)
      end


    (************************************************************************)
    (* This is the main function; It invokes all the passes of the back end *)
    (************************************************************************)

    fun compileDecFromBasis(Basis, dec, vcg_file) =
      let
	(* It is necessary to reset the timer because only 
	 * one timer may be activated at the time. *)

	(* There is only space in the basis for one lambdastat-env.
	 * If we want more checks, we should insert more components
	 * in bases. For now, we do type checking after optlambda, only. *)

(*	val _ = Timing.reset_timings() 28/04/1997, Niels*)
        val _ = RegionExp.printcount:=1;
	val {CEnv,TCEnv,EqEnv,OEnv,rse,mulenv, mularefmap,drop_env,psi_env,l2kam_ce} =
	  CompileBasis.de_CompileBasis Basis

        val (lamb,CEnv1, declared_lvars, declared_excons) = ast2lambda(CEnv, dec)
	val (lamb',EqEnv1) = elim_eq_lambda (EqEnv, lamb)
        val (lamb_opt,OEnv1) = optlambda (OEnv, lamb')
	val TCEnv1 = type_check_lambda (TCEnv, lamb_opt)
      in
	if isEmptyLambdaPgm lamb_opt then None
	else
	  let val (rse1, mularefmap1, mulenv1, drop_env1, psi_env1, l2kam_ce1, target, linkinfo) = 
	       comp_with_new_backend(rse, mularefmap, mulenv, drop_env, psi_env, l2kam_ce, lamb_opt, vcg_file)
	      val Basis' = CompileBasis.mk_CompileBasis {CEnv=CEnv1,TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,
							 rse=rse1,mulenv=mulenv1,mularefmap=mularefmap1,
							 drop_env=drop_env1,psi_env=psi_env1,l2kam_ce=l2kam_ce1}
	  in Some (Basis', target, linkinfo)
	  end
      end


    local
      open DecGrammar
      open TopdecGrammar
      open CompileBasis
    in

     (* compile is a hack - it interprets sequential/local declarations in a
        rather ad-hoc way. This is because we need to deal with sequential
        and local declarations for the core language, and we haven't done the
        modules compiler yet (the parser treats them as module syntax). *)

     (* The technique we use (assuming it works...) is to push LOCAL and
        SEQ constructs from the modules level of the abstract syntax to the
        core level. *)

      fun dec_from_strdec strdec =
	case strdec
	  of LOCALstrdec(i, strdec1, strdec2) =>
               let val dec1 = dec_from_strdec strdec1
                   val dec2 = dec_from_strdec strdec2
               in LOCALdec(i, dec1, dec2)
               end
           | SEQstrdec(i, strdec1, strdec2) =>
               let val dec1 = dec_from_strdec strdec1
                   val dec2 = dec_from_strdec strdec2
               in SEQdec(i, dec1, dec2)
               end
           | DECstrdec(_, dec) => dec
	   | _ => Crash.unimplemented "CompileAndRun.dec_from_strdec" 

      fun dec_from_topdec topdec =
	case topdec
          of STRtopdec(_, strdec, None) => dec_from_strdec strdec
           | STRtopdec(i, strdec1, Some topdec2) =>
	    let val dec1 = dec_from_strdec strdec1
	        val dec2 = dec_from_topdec topdec2
	    in SEQdec(i, dec1, dec2)
	    end
           | SIGtopdec _ => Crash.unimplemented "Compile.compile(SIGtopdec)"
           | FUNtopdec _ => Crash.unimplemented "Compile.compile(FUNtopdec)"

      fun compile(basis, topdec, vcg_file) = compileDecFromBasis(basis, dec_from_topdec topdec, vcg_file)

    end

    val generate_link_code = 
      let
	(* Global regions for all modules. *)
	val basis_info : basisinfo = 
	  [(Effect.key_of_eps_or_rho Effect.toplevel_region_withtype_top, KAMBackend.KAM.toplevel_region_withtype_topEA),
	   (Effect.key_of_eps_or_rho Effect.toplevel_region_withtype_string, KAMBackend.KAM.toplevel_region_withtype_stringEA),
	   (Effect.key_of_eps_or_rho Effect.toplevel_region_withtype_real, KAMBackend.KAM.toplevel_region_withtype_realEA)]
      in
	KAMBackend.generate_link_code basis_info
      end
    val emit = KAMBackend.emit


  end;
