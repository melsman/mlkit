
functor Compile(structure Excon : EXCON
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
		  sharing SpreadExp.E' = RegionExp
                  sharing type SpreadExp.place = Effect.effect
                  sharing type SpreadExp.cone = Effect.cone = SpreadExp.RegionStatEnv.cone
		  sharing type SpreadExp.RegionStatEnv.TypeAndPlaceScheme = RegionExp.sigma
		  sharing type SpreadExp.RegionStatEnv.excon = RegionExp.excon
		  sharing type SpreadExp.RegionStatEnv.lvar = RegionExp.lvar
		  sharing type SpreadExp.RegionStatEnv.place = RegionExp.place
		  sharing type SpreadExp.RegionStatEnv.Type = RegionExp.Type

                structure RegInf: REGINF
		  sharing type RegInf.cone = Effect.cone
		  sharing type RegInf.rse = SpreadExp.RegionStatEnv.regionStatEnv
		  sharing type RegInf.trip = RegionExp.trip
		  sharing type RegInf.place = RType.place = SpreadExp.place = RegionExp.place

		structure Mul: MUL
		structure MulInf: MUL_INF
		  sharing type MulInf.place = RegionExp.effect
		  sharing type MulInf.cone = RegInf.cone
		  sharing type MulInf.efenv = Mul.efenv
		  sharing type MulInf.mularefmap = Mul.mularefmap (*Psi*)
		  sharing type MulInf.LambdaPgm_phi = RegionExp.LambdaPgm

		structure MulExp: MUL_EXP
		  sharing type MulInf.LambdaPgm_psi = MulExp.LambdaPgm
		  sharing type MulExp.place = MulInf.place
		  sharing type MulExp.mul = MulInf.mul
                  sharing type MulExp.regionStatEnv= SpreadExp.RegionStatEnv.regionStatEnv

		structure AtInf: AT_INF
		  sharing type AtInf.LambdaPgm = MulExp.LambdaPgm
		  sharing type AtInf.place = Effect.place
		  sharing type AtInf.mul = MulExp.mul
		  sharing type AtInf.qmularefset = MulInf.qmularefset = MulExp.qmularefset
			      
		structure DropRegions: DROP_REGIONS
		  sharing type DropRegions.at = AtInf.at
		  sharing type DropRegions.place = Effect.place
		  sharing type DropRegions.mul = MulExp.mul
		  sharing type DropRegions.LambdaPgm = MulExp.LambdaPgm

		structure PhysSizeInf : PHYS_SIZE_INF
		  sharing type PhysSizeInf.at = AtInf.at
		  sharing type PhysSizeInf.place = Effect.place
		  sharing type PhysSizeInf.LambdaPgm = MulExp.LambdaPgm
		  sharing type PhysSizeInf.mul = MulExp.mul

		structure ClosExp : CLOS_EXP
		  sharing type ClosExp.place = PhysSizeInf.place
 (*                 sharing type ClosExp.pp = PhysSizeInf.pp *)
                  sharing type ClosExp.phsize = PhysSizeInf.phsize
                  sharing type ClosExp.at = AtInf.at
                  sharing type ClosExp.LambdaPgm = PhysSizeInf.LambdaPgm

	        structure LineStmt : LINE_STMT
                    where type ClosPrg = ClosExp.ClosPrg
		  sharing type LineStmt.place = PhysSizeInf.place
(*                  sharing type LineStmt.pp = PhysSizeInf.pp *)
                  sharing type LineStmt.phsize = PhysSizeInf.phsize
                  sharing type LineStmt.label = ClosExp.label

	        structure RegAlloc : REG_ALLOC
                  where type ('a, 'b, 'c)LinePrg = ('a, 'b, 'c)LineStmt.LinePrg
		  sharing type RegAlloc.place = LineStmt.place
                  sharing type RegAlloc.phsize = LineStmt.phsize
                  sharing type RegAlloc.label = LineStmt.label
                  sharing type RegAlloc.lvar = LineStmt.lvar
                  sharing type RegAlloc.Atom = LineStmt.Atom
                  sharing type RegAlloc.StoreTypeLI = LineStmt.StoreType

	        structure FetchAndFlush : FETCH_AND_FLUSH
                  where type ('a, 'b, 'c)LinePrg = ('a, 'b, 'c)LineStmt.LinePrg
		  sharing type FetchAndFlush.place = LineStmt.place
                  sharing type FetchAndFlush.phsize = LineStmt.phsize
                  sharing type FetchAndFlush.label = LineStmt.label
                  sharing type FetchAndFlush.lvar = LineStmt.lvar
                  sharing type FetchAndFlush.StoreTypeRA = RegAlloc.StoreType
                  sharing type FetchAndFlush.Atom = LineStmt.Atom

	        structure CalcOffset : CALC_OFFSET
                  where type ('a, 'b, 'c)LinePrg = ('a, 'b, 'c)LineStmt.LinePrg
		  sharing type CalcOffset.place = LineStmt.place
                  sharing type CalcOffset.phsize = LineStmt.phsize
                  sharing type CalcOffset.label = LineStmt.label
                  sharing type CalcOffset.lvar = LineStmt.lvar
                  sharing type CalcOffset.StoreTypeIFF = FetchAndFlush.StoreType
                  sharing type CalcOffset.Atom = LineStmt.Atom

	        structure SubstAndSimplify : SUBST_AND_SIMPLIFY
                  where type ('a, 'b, 'c)LinePrg = ('a, 'b, 'c)LineStmt.LinePrg
		  sharing type SubstAndSimplify.place = LineStmt.place
                  sharing type SubstAndSimplify.phsize = LineStmt.phsize
                  sharing type SubstAndSimplify.label = LineStmt.label
                  sharing type SubstAndSimplify.lvar = LineStmt.lvar
                  sharing type SubstAndSimplify.StoreTypeCO = CalcOffset.StoreType
                  sharing type SubstAndSimplify.AtomCO = CalcOffset.Atom

		structure RegionFlowGraphProfiling : REGION_FLOW_GRAPH_PROFILING
		  sharing type RegionFlowGraphProfiling.place = PhysSizeInf.place
		  sharing type RegionFlowGraphProfiling.at = AtInf.at
		  sharing type RegionFlowGraphProfiling.phsize = PhysSizeInf.phsize

		structure CompilerEnv: COMPILER_ENV
		  sharing type CompilerEnv.lvar = LambdaExp.lvar
                  sharing type CompilerEnv.excon = LambdaExp.excon = Excon.excon
                  sharing type CompilerEnv.Type = LambdaExp.Type

                structure CompileDec: COMPILE_DEC
		  sharing type CompileDec.LambdaPgm = LambdaExp.LambdaPgm
                  sharing type CompileDec.CEnv = CompilerEnv.CEnv

                structure OptLambda: OPT_LAMBDA
		  sharing type OptLambda.LambdaPgm = LambdaExp.LambdaPgm

		structure CompileBasis: COMPILE_BASIS
		  sharing type CompileBasis.EqEnv = EliminateEq.env
		  sharing type CompileBasis.OEnv = OptLambda.env
		  sharing type CompileBasis.TCEnv = LambdaStatSem.env 
                  sharing type CompileBasis.rse = SpreadExp.RegionStatEnv.regionStatEnv
		  sharing type CompileBasis.drop_env = DropRegions.env
		  sharing type CompileBasis.psi_env = PhysSizeInf.env
	       	  sharing type CompileBasis.mulenv = MulInf.efenv
                  sharing type CompileBasis.mularefmap = MulInf.mularefmap
                  sharing type CompileBasis.clos_env = ClosExp.env

                structure Report: REPORT
		structure Flags: FLAGS

		structure PP: PRETTYPRINT
		  sharing type CompileBasis.StringTree
                                     = LambdaExp.StringTree
			             = EliminateEq.StringTree
                                     = CompilerEnv.StringTree
                                     = PP.StringTree
                                     = SpreadExp.RegionStatEnv.StringTree
                                     = Effect.StringTree
                                     = RegionExp.StringTree
                                     = MulExp.StringTree
                                     = MulInf.StringTree
			             = AtInf.StringTree
			             = PhysSizeInf.StringTree
		                     = RegionFlowGraphProfiling.StringTree
                  sharing type PP.Report = Report.Report
			
	        structure Name : NAME

                structure Crash: CRASH
		structure Timing: TIMING
		  ): COMPILE =

  struct

    structure CE = CompilerEnv

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type strdec = CompileDec.strdec

    fun die s = Crash.impossible ("Compile." ^ s)

    (* ---------------------------------------------------------------------- *)
    (*  Dynamic Flags.                                                        *)
    (* ---------------------------------------------------------------------- *)

    val print_opt_lambda_expression = 
      Flags.lookup_flag_entry "print_opt_lambda_expression" 
    val print_drop_regions_expression_with_storage_modes = 
      Flags.lookup_flag_entry "print_drop_regions_expression_with_storage_modes"
    val print_physical_size_inference_expression =
      Flags.lookup_flag_entry "print_physical_size_inference_expression"
    val print_call_explicit_expression =
      Flags.lookup_flag_entry "print_call_explicit_expression"
    val print_program_points = Flags.lookup_flag_entry "print_program_points"

    val gc_flag              = Flags.lookup_flag_entry "garbage_collection"
    val tag_values_flag      = Flags.lookup_flag_entry "tag_values"
    val tag_integers_flag    = Flags.lookup_flag_entry "tag_integers"
    val unbox_datatypes_flag = Flags.lookup_flag_entry "unbox_datatypes"
    fun set_tag_flags() =
      if !gc_flag  then
	(tag_values_flag := true;
	 tag_integers_flag := true;
	 unbox_datatypes_flag := false)
      else
	(tag_values_flag := false;
	 tag_integers_flag := false;
	 unbox_datatypes_flag := true)

    (* ---------------------------------------------------------------------- *)
    (*  Printing utilities                                                    *)
    (* ---------------------------------------------------------------------- *)

    fun out_layer (stl:PP.StringTree list) =
      PP.outputTree((fn s => TextIO.output(TextIO.stdOut, s)), 
         PP.NODE{start = "[", finish = "]", childsep = PP.RIGHT ",", indent = 1, 
                 children = stl}, !Flags.colwidth)

    fun pr0 st log = (Report.print' (PP.reportStringTree st) log; 
                      TextIO.flushOut log)
    fun pr st = pr0 st (!Flags.log)

    fun length l = foldr (fn (_, n) => n+1) 0 l

    local
      fun msg(s: string) = (TextIO.output(!Flags.log, s); TextIO.flushOut (!Flags.log)
      (*; TextIO.output(TextIO.stdOut, s)*))
    in
      fun chat(s: string) = if !Flags.chat then msg (s^"\n") else ()
    end

    fun fast_pr stringtree = 
           (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
            TextIO.output(!Flags.log, "\n"))

    fun display(title, tree) =
        fast_pr(PP.NODE{start=title ^ ": ",
                   finish="",
                   indent=3,
                   children=[tree],
                   childsep=PP.NOSEP
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
                            (if !Flags.print_regions then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
                             else fn _ => NONE)
                            (fn _ => NONE)) x
    fun layoutRegionExp x = (RegionExp.layoutLambdaExp 
                            (if !Flags.print_regions then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
                             else fn _ => NONE)
                             (fn _ => NONE)) x

    fun say x = TextIO.output(!Flags.log, x)
    fun sayenv rse = PP.outputTree(say, SpreadExp.RegionStatEnv.layout rse, !Flags.colwidth)

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
    (*  Compile RESET                                                         *)
    (* ---------------------------------------------------------------------- *)

(*
    (* The following reset functions are never called *)
    fun reset () = (LambdaExp.reset();  (* counters only *)
		    reset_pp_count();
		    reset_effect_count();
                    Effect.reset())       (* resets global cone *)

    fun commit () = (commit_pp_count();
		     LambdaExp.commit();
		     commit_effect_count();
		     Effect.commit())
*)		     

    (* ---------------------------------------------------------------------- *)
    (*  Compile the declaration using old compiler environment, ce            *)
    (* ---------------------------------------------------------------------- *)

    fun ast2lambda(ce, strdecs) =
      (chat "Compiling abstract syntax tree into lambda language begin ...";
       Timing.timing_begin();
       let val _ = LambdaExp.reset()  (* Reset type variable counter to improve pretty printing; The generated
				       * Lambda programs are closed w.r.t. type variables, because code 
				       * generation of the strdecs is done after an entire top-level 
				       * declaration is elaborated. ME 1998-09-04 *)
	   val (ce1, lamb) =  Timing.timing_end_res 
	        ("ToLam", CompileDec.compileStrdecs ce strdecs)
	   val declared_lvars = CompilerEnv.lvarsOfCEnv ce1
	   val declared_excons = CompilerEnv.exconsOfCEnv ce1
       in  
	 chat "Compiling abstract syntax tree into lambda language end.";
	 ifthen (!Flags.DEBUG_COMPILER) (fn _ => display("Report: UnOpt", layoutLambdaPgm lamb));
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
	    | PGM(DATBINDS [], FRAME {declared_lvars=[], declared_excons=[]}) => true
	    | _ => false
      end

    (* ---------------------------------------------------------------------- *)
    (*  Type check the lambda code                                             *)
    (* ---------------------------------------------------------------------- *)

    fun type_check_lambda (a,b) =
      if Flags.is_on "type_check_lambda" then
	(chat "Type checking lambda term begin ...";
	 Timing.timing_begin();
	 let 
	   val env' = Timing.timing_end_res ("CheckLam",(LambdaStatSem.type_check {env = a,  letrec_polymorphism_only = false,
                  pgm =  b}))
	 in
	   chat "Type checking lambda term end.";
	   env'
	 end)
      else LambdaStatSem.empty


    (* ---------------------------------------------------------------------- *)
    (*   Eliminate polymorphic equality in the lambda code                    *)
    (* ---------------------------------------------------------------------- *)

    fun elim_eq_lambda (env,lamb) =
      if Flags.is_on "eliminate_polymorphic_equality" then
	(chat "Eliminating polymorphic equality begin ...";
	 Timing.timing_begin();
	 let val (lamb', env') = 
	   Timing.timing_end_res ("ElimEq", EliminateEq.elim_eq (env, lamb))
	 in
	   chat "Eliminating polymorphic equality end.";
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
          ((if !Flags.optimiser then chat "Optimising lambda term ..."
	    else chat "Rewriting lambda term ...");
	   Timing.timing_begin();
	   let 
	     val (lamb_opt, env') = 
	           Timing.timing_end_res ("OptLam", OptLambda.optimise(env,lamb))
	   in
	     if !Flags.DEBUG_COMPILER orelse !print_opt_lambda_expression
	     then display("Report: Opt", layoutLambdaPgm lamb_opt) else () ;
	     (lamb_opt, env')
	   end)

    (* ---------------------------------------------------------------------- *)
    (*   Spread the optimised lambda code                                     *)
    (* ---------------------------------------------------------------------- *)

    fun spread(cone,rse, lamb_opt)=
        (chat "Spreading regions and effects ...";
         Timing.timing_begin();
         (*Profile.reset();
         Profile.profileOn();*)
         let val (cone,rse_con,spread_lamb) = SpreadExp.spreadPgm(cone,rse, lamb_opt)
         in Timing.timing_end("SpreadExp");
            (*Profile.profileOff();
            TextIO.output(!Flags.log, "\n PROFILING OF S\n\n");
            Profile.report(!Flags.log);*)
            if !Flags.DEBUG_COMPILER 
            then (display("\nReport: Spread; program", layoutRegionPgm spread_lamb) ;
                  display("\nReport: Spread; entire cone after Spreading", Effect.layoutCone cone) )
            else ();
            
            (cone,rse_con, spread_lamb)
         end) 


    (* ---------------------------------------------------------------------- *)
    (*   Do the region inference on the spread optimised lambda code          *)
    (* ---------------------------------------------------------------------- *)

    fun inferRegions(cone,rse, rse_con,  spread_lamb as RegionExp.PGM 
                        {expression = spread_lamb_exp,
                         export_datbinds = datbinds,
                         export_basis=export_basis  (* list of region variables and arrow effects *)
                        }) = 
    let
        val _ = (chat "Inferring regions and effects ...";
		 Timing.timing_begin())
(*
	val _ = if !profRegInf.b then (Compiler.Profile.reset(); Compiler.Profile.setTimingMode true) else ()
*)
        val rse_with_con = SpreadExp.RegionStatEnv.plus(rse,rse_con)
(*	val _ = print "RegInf.inferEffects ... \n" *)
        val cone = RegInf.inferEffects
                   (fn s => (TextIO.output(!Flags.log, s); TextIO.flushOut(!Flags.log)))
                   (cone,rse_with_con, spread_lamb_exp)
(*	val _ = print "RegInf.Creating new layer ...\n" *)
        val new_layer = Effect.topLayer cone (* to get back to level of "cone" *)
(*
        val _ = print "new_layer before lowering:\n"
        val _ = out_layer(Effect.layoutEtas new_layer)
*)
(*	val _ = print "RegInf.Creating Cone ...\n" *)
	val toplevel = Effect.level Effect.initCone
	val cone = foldl (fn (effect, cone) =>
			       Effect.lower toplevel effect cone) cone new_layer
(*
        val _ = print "new_layer after lowering:\n"
        val _ = out_layer(Effect.layoutEtas new_layer)
*)
(*	val _ = print "RegInf.Unifying toplevel regions and effects ...\n" *)
        val cone = Effect.unify_with_toplevel_rhos_eps(cone,new_layer)

	val new_layer = []

        val _ = Timing.timing_end("RegInf")

        val pgm' = RegionExp.PGM{expression = spread_lamb_exp, (*side-effected*)
                      export_datbinds = datbinds, (*unchanged*)
                      export_basis= new_layer  (* list of region variables and arrow effects *)}

	(* call of normPgm no longer commented out; mads *)
        val _ = if Flags.is_on "region_profiling" then
	          ()
		else
		  ((*print "RegInf.Normalising program ...\n";*)
		   reset_effect_count();      (* inserted; mads *)
		   RegionExp.normPgm(pgm',effect_counter) 
		   )
(*	val _ = print "RegInf.Computing rse' ...\n"  *)
	val rse' =
	  case spread_lamb_exp
	    of RegionExp.TR(_,RegionExp.Frame{declared_lvars,declared_excons},_) =>
	      (let val rse_temp = 
		 foldl (fn ({lvar,compound,create_region_record,sigma, place}, rse) =>
			     SpreadExp.RegionStatEnv.declareLvar(lvar,(compound, 
							   create_region_record, !sigma, place, 
							   SOME(ref[]) (* reset occurrences *), NONE
							 ), rse)) rse_con declared_lvars
	       in
		 foldl (fn ((excon, SOME(Type, place)), rse) => SpreadExp.RegionStatEnv.declareExcon(excon,(Type,place),rse)
	                 | _ => die "rse.excon") rse_temp declared_excons

	       end handle _ => die "cannot form rse'")
	     | _ => die "program does not have type frame"
(*
	val _ =
	  if !profRegInf.b then
	    let
	      val tempfile = OS.FileSys.tmpName()
	      val os = TextIO.openOut tempfile
	      val _ = Compiler.Profile.report os
	      val _ = TextIO.closeOut os   
	    in print ("RegInf.Exported profile to file " ^ tempfile ^ "\n")
	    end
	  else ()
*)
    in 
      if !Flags.DEBUG_COMPILER then
        (say "Resulting region-static environment:\n";
	 sayenv(rse');
	 display("\nReport: After Region Inference", layoutRegionPgm pgm'))
      else ();
      (cone,rse',pgm')       (* rse' contains rse_con *)
    end

    (* ---------------------------------------------------------------------- *)
    (*   Multiplicity Inference                                               *)
    (* ---------------------------------------------------------------------- *)

    fun mulInf(program_after_R:(Effect.place,unit)RegionExp.LambdaPgm, Psi, cone, mulenv) =
    let

        val _ = (chat "Inferring multiplicities ...";
                Timing.timing_begin()
                (*;Profile.reset()
                ;Profile.profileOn()*) )
        val (pgm', mulenv',Psi') = MulInf.mulInf(program_after_R,Psi,cone,mulenv)

        val _ = Timing.timing_end("MulInf")

(*        val _ = ( Profile.profileOff()(*;
                TextIO.output(!Flags.log, "\n PROFILING OF MulInf\n\n");
                Profile.report(!Flags.log)*));
*)
    in 
        if !Flags.DEBUG_COMPILER 
            then (display("\nReport: After Multiplicity Inference, the program is:\n",
                   	    MulInf.layoutLambdaPgm pgm'))
            else ();

        (pgm', mulenv', Psi')
    end
    


    (* ---------------------------------------------------------------------- *)
    (*  Spread Expression, Region Inference and Multiplicity Inference        *)
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
    (*   Perform K-normalisation                                              *)
    (* ---------------------------------------------------------------------- *)

    local open MulInf
    in fun k_norm(pgm: (place,place*mul,qmularefset ref)LambdaPgm_psi) 
	: (place,place*mul,qmularefset ref)LambdaPgm_psi =
	(chat "K-normalisation ...";
         Timing.timing_begin();
         let val pgm' = MulInf.k_normPgm pgm
	 in Timing.timing_end("Knorm");
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
    (*   Do attop/atbot analysis                                              *)
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
    (*   Do drop regions                                                      *)
    (* ---------------------------------------------------------------------- *)
  
    local open DropRegions
    in
      val drop_regions : env*(place at,place*mul,unit)LambdaPgm -> (place at,place*mul,unit)LambdaPgm * env =
	fn (env, pgm) =>
	(chat "Drop Regions ...";
         Timing.timing_begin();
         let val (pgm',env') = drop_regions(env, pgm)
	 in Timing.timing_end("Drop");
	    if Flags.is_on "print_drop_regions_expression" then 
	      display("\nReport: AFTER DROP REGIONS:", AtInf.layout_pgm_brief pgm')
	    else ();
	    if Flags.is_on "print_drop_regions_expression_with_storage_modes" orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER DROP REGIONS (with storage modes):", AtInf.layout_pgm pgm')
	    else ();
	    (pgm',env')
	 end)
    end


    (* ---------------------------------------------------------------------- *)
    (*   Do physical size inference                                           *)
    (* ---------------------------------------------------------------------- *)
  
    local open PhysSizeInf
   in
      fun phys_size_inf (env: env, pgm:(place at,place*mul,unit)LambdaPgm) 
	: ((place*pp)at,place*phsize,unit)LambdaPgm * env =
	(chat "Physical Size Inference ...";
         Timing.timing_begin();
         let val (pgm',env') = psi(pp_counter, env, pgm)
	 in Timing.timing_end("PSI");
	    if !print_physical_size_inference_expression orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER PHYSICAL SIZE INFERENCE:", layout_pgm pgm')
	    else ();
	    (pgm',env')
	 end)
    end


    (* ---------------------------------------------------------------------- *)
    (*   Warn against dangling pointers (when Garbage Collection is on)       *)
    (* ---------------------------------------------------------------------- *)

       fun warn_dangling_pointers(rse, psi_pgm) = 
        let (* warn against dangling references *)
            fun get_place_at(AtInf.ATTOP(rho,pp)) = rho
              | get_place_at(AtInf.ATBOT(rho,pp)) = rho
              | get_place_at(AtInf.SAT(rho,pp)) = rho
              | get_place_at(AtInf.IGNORE) = Effect.toplevel_region_withtype_top
        in
            chat "Checking whether there are dangling pointers ...";
            Timing.timing_begin();
            MulExp.warn_dangling_pointers(rse, psi_pgm, get_place_at);
            Timing.timing_end("Dangle")
        end


    (* ---------------------------------------------------------------------- *)
    (*   Do application conversion                                            *)
    (* ---------------------------------------------------------------------- *)
  
    local open PhysSizeInf
    in
      fun appConvert (pgm:((place*pp)at,place*phsize,unit)LambdaPgm): 
                          ((place*pp)at,place*phsize,unit)LambdaPgm =
	(chat "Application Conversion ...";
         Timing.timing_begin();
         let val pgm' = PhysSizeInf.appConvert(pgm)
	 in Timing.timing_end("AppConv");
	    if !print_call_explicit_expression orelse !Flags.DEBUG_COMPILER then 
	      display("\nReport: AFTER APPLICATION CONVERSION:", layout_pgm pgm')
	    else ();
	    pgm'
	 end)
    end

    (* ---------------------------------------------------------------------- *)
    (*   New Lambda Backend                                                   *)
    (* ---------------------------------------------------------------------- *)

    type label = SubstAndSimplify.label
    type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) SubstAndSimplify.LinePrg 
    type offset = SubstAndSimplify.offset
    type StoreTypeCO = SubstAndSimplify.StoreTypeCO
    type AtySS = SubstAndSimplify.Aty

    type target_new = {main_lab: label,
		       code: (StoreTypeCO,offset,AtySS) LinePrg,
		       imports: label list * label list,
		       exports: label list * label list,
		       safe: bool}     (* true if the fragment has no side-effects;
					* for dead code elimination. *)

    fun new_lambda_backend(clos_env,pgm,safe) : ClosExp.env * 
      {main_lab: label, code:(StoreTypeCO,offset,AtySS) LinePrg,
       imports: label list * label list, exports: label list * label list, safe:bool}  =
      let
	val _ = Timing.timing_begin()
	val all_clos_exp = Timing.timing_end_res("ClosConv",ClosExp.cc (clos_env, pgm))
	val clos_env' = #env all_clos_exp


	val _ = Timing.timing_begin()
	val all_line_stmt = 
	  Timing.timing_end_res("LineStmt",LineStmt.L {main_lab= #main_lab all_clos_exp,
						       code= #code all_clos_exp,
						       imports= #imports all_clos_exp,
						       exports= #exports all_clos_exp})

	val _ = Timing.timing_begin()
	val all_reg_alloc = 
	  Timing.timing_end_res("RegAlloc",
				if Flags.is_on "perform_register_allocation" then 
				  RegAlloc.ra all_line_stmt
				else 
				  RegAlloc.ra_dummy all_line_stmt)

	val _ = Timing.timing_begin()
	val all_fetch_flush = Timing.timing_end_res("FetchFlush",FetchAndFlush.IFF all_reg_alloc)

	val _ = Timing.timing_begin()
	val all_calc_offset = Timing.timing_end_res("CalcOffset",CalcOffset.CO all_fetch_flush)

	val _ = Timing.timing_begin()
	val all_calc_offset_with_bv = 
	  Timing.timing_end_res("CBV",
				if !gc_flag then
				  CalcOffset.CBV all_calc_offset
				else
				  all_calc_offset)

	val _ = Timing.timing_begin()
	val all_subst_and_simplify = Timing.timing_end_res("SS", SubstAndSimplify.SS all_calc_offset_with_bv)
      in
	(clos_env',
	 {main_lab = #main_lab all_subst_and_simplify,
	  code = #code all_subst_and_simplify,
	  imports = #imports all_subst_and_simplify,
	  exports = #exports all_subst_and_simplify,
	  safe = safe})
      end


    (* ===================================
     * Compile with the NEW backend
     * =================================== *)

    fun comp_with_new_backend(rse, Psi, mulenv, drop_env, psi_env, clos_env, lamb_opt, vcg_file) =
      let
	val safe = LambdaExp.safeLambdaPgm lamb_opt
	val (mul_pgm, rse1, mulenv1, Psi1) = SpreadRegMul(rse, Psi, mulenv, lamb_opt)
        val _ = MulExp.warn_puts(rse, mul_pgm)
        val k_mul_pgm = k_norm mul_pgm
	val sma_pgm = storagemodeanalysis k_mul_pgm
	val (drop_pgm, drop_env1) = drop_regions(drop_env,sma_pgm)
	val (psi_pgm, psi_env1) = phys_size_inf(psi_env, drop_pgm)
        val _ = warn_dangling_pointers(rse, psi_pgm)
        val app_conv_psi_pgm = appConvert psi_pgm
	val _ = RegionFlowGraphProfiling.reset_graph ()
	  
	val (clos_env1, target_new) = new_lambda_backend(clos_env,app_conv_psi_pgm, safe)
(*
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
*)

	(* Show region flow graph and generate .vcg file *)
	val _ = if Flags.is_on "show_region_flow_graph" then
	           (display("\nReport: REGION FLOW GRAPH FOR PROFILING:", 
			    RegionFlowGraphProfiling.layout_graph());
		    let val outStreamVCG = TextIO.openOut vcg_file
		    in chat "Generating region flow graph for profiling (.vcg file) ...";
		      RegionFlowGraphProfiling.export_graph outStreamVCG;
		      TextIO.closeOut(outStreamVCG)
		    end)
		else ()

      in
	(rse1, Psi1, mulenv1, drop_env1, psi_env1, clos_env1, target_new)
      end


    (************************************************************************)
    (* This is the main function; It invokes all the passes of the back end *)
    (************************************************************************)

    datatype res = CodeRes of CEnv * CompileBasis * target_new
                 | CEnvOnlyRes of CEnv

    fun compile(CEnv, Basis, strdecs, vcg_file) : res =
      let

	(* Make sure that tag flags are set appropriately. *)
	val _ = set_tag_flags()

	(* There is only space in the basis for one lambdastat-env.
	 * If we want more checks, we should insert more components
	 * in bases. For now, we do type checking after optlambda, only. *)

        val _ = RegionExp.printcount:=1;
	val {TCEnv,EqEnv,OEnv,rse,mulenv, mularefmap,drop_env,psi_env,clos_env} =
	  CompileBasis.de_CompileBasis Basis

        val (lamb,CEnv1, declared_lvars, declared_excons) = ast2lambda(CEnv, strdecs)
	val (lamb',EqEnv1) = elim_eq_lambda (EqEnv, lamb)
        val (lamb_opt,OEnv1) = optlambda (OEnv, lamb')
	val TCEnv1 = type_check_lambda (TCEnv, lamb_opt)
      in
	if isEmptyLambdaPgm lamb_opt 
          then (chat "Empty lambda program; skipping code generation.";
                CEnvOnlyRes CEnv1)
	else
	  let val (rse1, mularefmap1, mulenv1, drop_env1, psi_env1, 
		   clos_env1, target_new) = 
	              comp_with_new_backend(rse, mularefmap, mulenv, drop_env, psi_env, 
					    clos_env, lamb_opt, vcg_file)
	      val Basis' = CompileBasis.mk_CompileBasis {TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,
							 rse=rse1,mulenv=mulenv1,mularefmap=mularefmap1,
							 drop_env=drop_env1,psi_env=psi_env1,
							 clos_env=clos_env1}
	  in CodeRes (CEnv1, Basis', target_new)
	  end
      end

  end;
