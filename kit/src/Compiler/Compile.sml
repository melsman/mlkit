
structure Compile: COMPILE =
  struct
    structure SpreadExp = SpreadExpression
    structure PP = PrettyPrint
    structure CE = CompilerEnv

    type CompBasis = CompBasis.CompBasis
    type CEnv = CompileToLamb.CEnv
    type Env = CompileToLamb.Env
    type strdec = CompileToLamb.strdec
    type strexp = CompileToLamb.strexp
    type funid = CompileToLamb.funid
    type strid = CompileToLamb.strid

    fun die s = Crash.impossible ("Compile." ^ s)

    (* ---------------------------------------------------------------------- *)
    (*  Dynamic Flags.                                                        *)
    (* ---------------------------------------------------------------------- *)

    val print_regions = Flags.is_on0 "print_regions"

    val region_profiling_p = Flags.is_on0 "region_profiling"

    val rse_0 = Flags.add_bool_entry 
	{long="print_region_static_env0", short=SOME "Prse0", 
	 menu=["Printing of RSE0",
	       "print imported region static environment"],
	 item=ref false, neg=false, desc=
	 "Print imported region static environment prior to\n\
	  \region inference."}

    val print_storage_mode_expression = Flags.add_bool_entry 
	{long="print_storage_mode_expression", short=SOME "Psme", 
	 menu=["Printing of intermediate forms",
	       "print storage mode expression"],
	 item=ref false, neg=false, desc=
	 "Print Region Expression after storage mode analysis"}

    val print_drop_regions_expression_with_storage_modes = Flags.add_bool_entry 
	{long="print_drop_regions_expression_with_storage_modes", short=SOME "Pdresm", 
	 menu=["Printing of intermediate forms",
	       "print drop regions expression with storage modes"],
	 item=ref false, neg=false, desc=
	 "Print Region Expression after dropping word regions and\n\
	  \regions arguments with only get-effects. Also print\n\
	  \atbot and attop annotations resulting from storage mode\n\
	  \analysis."}

    val print_drop_regions_expression = Flags.add_bool_entry 
	{long="print_drop_regions_expression", short=SOME "Pdre", 
	 menu=["Printing of intermediate forms","print drop regions expression"],
	 item=ref false, neg=false, desc=
	 "Print Region Expression after dropping word regions and\n\
	  \regions arguments with only get-effects."}

    val print_physical_size_inference_expression = Flags.add_bool_entry 
	 {long="print_physical_size_inference_expression", short=SOME "Ppse", 
	  menu=["Printing of intermediate forms",
		"print physical size inference expression"],
	  item=ref false, neg=false, desc=
	  "Print Region Expression after physical size inference."}

    val print_call_explicit_expression = Flags.add_bool_entry 
	 {long="print_call_explicit_expression", short=SOME "Pcee", 
	  menu=["Printing of intermediate forms","print call-explicit expression"],
	  item=ref false, neg=false, desc=
	  "Print Region Expression with call annotations."}

    (* ---------------------------------------------------------------------- *)
    (*  Printing utilities                                                    *)
    (* ---------------------------------------------------------------------- *)

    fun out_layer (stl:PP.StringTree list) =
      PP.outputTree((fn s => TextIO.output(TextIO.stdOut, s)), 
         PP.NODE{start = "[", finish = "]", childsep = PP.RIGHT ",", indent = 1, 
                 children = stl}, !Flags.colwidth)

    local
      fun msg(s: string) = 
	  (TextIO.output(!Flags.log, s); TextIO.flushOut (!Flags.log))
    in
      fun chat(s: string) = if !Flags.chat then msg (s^"\n") else ()
    end

    fun fast_pr stringtree = 
	(PP.outputTree ((fn s => TextIO.output(!Flags.log, s)),
			stringtree, !Flags.colwidth);
	 TextIO.output(!Flags.log, "\n\n"))
	
    fun display(title, tree) =
        fast_pr(PP.NODE{start=title ^ ": ",
                   finish="",
                   indent=3,
                   children=[tree],
                   childsep=PP.NOSEP
                  }
          )

    fun ifthen e f = if e then f() else ()

    (* ---------------------------------------------------------------------- *)
    (*  Abbreviations                                                         *)
    (* ---------------------------------------------------------------------- *)
      
    fun layoutRegionPgm x = 
	(RegionExp.layoutLambdaPgm 
	 (if print_regions() then 
	      (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
	  else fn _ => NONE)
                            (fn _ => NONE)) x
    fun layoutRegionExp x = 
	(RegionExp.layoutLambdaExp 
	 (if print_regions() then 
	      (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho))))
	  else fn _ => NONE)
	      (fn _ => NONE)) x

    fun say x = TextIO.output(!Flags.log, x)
    fun sayenv rse = 
	PP.outputTree(say, SpreadExp.RegionStatEnv.layout rse, !Flags.colwidth)

    type arity = int

    (* --------------------------------------------
     * Program point counter
     * -------------------------------------------- *)

    local 
      val pp_init = 1   (* ~1 and 0 are reserved *)
      val pp_count = ref (pp_init)
    in
      fun pp_counter() = (pp_count := !pp_count + 1; !pp_count)
      fun reset_pp_count() = pp_count := pp_init
    end

    (* ---------------------------------------------------------------------- *)
    (*   Spread the optimised lambda code                                     *)
    (* ---------------------------------------------------------------------- *)

    fun spread(cone,rse, lamb_opt)=
        (chat "[Spreading regions and effects...";
         Timing.timing_begin();
         (*Profile.reset();
         Profile.profileOn();*)
         let 
(*	     val _ = display ("Region static environment 0",SpreadExp.RegionStatEnv.layout rse) *)
	     val effects_rse = 
		 SpreadExp.RegionStatEnv.FoldLvar 
		 (fn ((lv,(_,_,s,r,_,_)),acc) => 
		  if Lvars.pr_lvar lv = "revAcc" then 
		      r :: RType.frv_sigma s @ acc 
		  else acc) nil rse
(*	     val _ = out_layer (Effect.layoutEtas effects_rse) *)
	     val (cone,rse_con,spread_lamb) = SpreadExp.spreadPgm(cone,rse, lamb_opt)
         in 
	   Timing.timing_end("SpreadExp");
	   chat "]\n";
	   (*Profile.profileOff();
            TextIO.output(!Flags.log, "\n PROFILING OF S\n\n");
            Profile.report(!Flags.log);*)
	   if !Flags.DEBUG_COMPILER 
	     then (display("\nReport: Spread; program", 
			   layoutRegionPgm spread_lamb) ;
		   display("\nReport: Spread; entire cone after Spreading", 
			   Effect.layoutCone cone) )
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
        val _ = (chat "[Inferring regions and effects...";
		 Timing.timing_begin())
(*
	val _ = if !profRegInf.b then (Compiler.Profile.reset(); Compiler.Profile.setTimingMode true) else ()
*)
        val rse_with_con = SpreadExp.RegionStatEnv.plus(rse,rse_con)

	val _ = if rse_0() then display ("Region static environment 0",SpreadExp.RegionStatEnv.layout(rse_with_con))
		else ()
(*
	val effects_rse0 = SpreadExp.RegionStatEnv.FoldLvar (fn ((lv,(_,_,s,r,_,_)),acc) => if Lvars.pr_lvar lv = "revAcc" then r :: RType.frv_sigma s @ acc else acc) nil rse_with_con
	val _ = print ("Checking effects; size = " ^ Int.toString(List.length effects_rse0) ^ "\n")
	val _ = out_layer (Effect.layoutEtas effects_rse0)
	val _ = Effect.check_effects "Compile" effects_rse0
*)
(*	val _ = print "RegInf.inferEffects ... \n" *)
        val cone = RegInf.inferEffects
                   (fn s => (TextIO.output(!Flags.log, s); TextIO.flushOut(!Flags.log)))
                   (cone,rse_with_con, spread_lamb_exp)
(*	val _ = print "RegInf.Creating new layer ...\n" *)
        val new_layer = Effect.topLayer cone (* to get back to level of "cone" *)
(*
        val _ = print "new_layer before lowering:\n"
        val _ = out_layer(Effect.layoutEtas new_layer)

	val _ = print "RegInf.Creating Cone ...\n"
*)
	val toplevel = Effect.level Effect.initCone
	val cone = foldl (fn (effect, cone) =>
			       Effect.lower toplevel effect cone) cone new_layer
(*
        val _ = print "new_layer after lowering:\n"
        val _ = out_layer(Effect.layoutEtas new_layer)
*)
	(* all variables in cone with toplevel: *)
(*	  
	val _ = print "RegInf.Unifying toplevel regions and effects ...\n"
*)
        val cone = Effect.unify_with_toplevel_rhos_eps(cone,new_layer)

	val new_layer = []

        val _ = Timing.timing_end("RegInf")
	val _ = chat "]\n"

        val pgm' = RegionExp.PGM{expression = spread_lamb_exp, (*side-effected*)
                      export_datbinds = datbinds, (*unchanged*)
                      export_basis= new_layer  (* list of region variables and arrow effects *)}

	(* call of normPgm no longer commented out; mads *)
(*
        val _ = if region_profiling_p() then ()
		else
		  ((*print "RegInf.Normalising program ...\n";*)
		   reset_effect_count();      (* inserted; mads *)
		   RegionExp.normPgm(pgm',effect_counter) 
		   )
*)
(*	val _ = print "RegInf.Computing rse' ...\n"  *)
	val rse' =
	  case spread_lamb_exp
	    of RegionExp.TR(_,RegionExp.Frame{declared_lvars,declared_excons},_) =>
	      (let val rse_temp = 
		 foldl (fn ({lvar,compound,create_region_record,sigma, place}, rse) =>
			     SpreadExp.RegionStatEnv.declareLvar(lvar,(compound, 
							   create_region_record, !sigma, place, 
							   NONE (*SOME(ref[])*) (* reset occurrences *), NONE
							 ), rse)) rse_con declared_lvars
	       in
		 foldl (fn ((excon, SOME(Type, place)), rse) => SpreadExp.RegionStatEnv.declareExcon(excon,(Type,place),rse)
	                 | _ => die "rse.excon") rse_temp declared_excons

	       end handle _ => die "cannot form rse'")
	     | _ => die "program does not have type frame"

	val (rhos_rse',epss_rse') = SpreadExp.RegionStatEnv.places_effectvarsRSE rse'
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

        val _ = (chat "[Inferring multiplicities...";
                Timing.timing_begin()
                (*;Profile.reset()
                ;Profile.profileOn()*) )
        val (pgm', mulenv',Psi') = MulInf.mulInf(program_after_R,Psi,cone,mulenv)

        val _ = Timing.timing_end("MulInf")
	val _ = chat "]\n"

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
      let (* regionvar id is initialized by call in Manager.sml *)
	  val cone = Effect.push (SpreadExp.RegionStatEnv.mkConeToplevel rse)
	  val (cone, rse_con, spread_pgm) = spread(cone,rse,opt_pgm)
	  val (cone, rse', reginf_pgm) = inferRegions(cone,rse,rse_con,spread_pgm)
	  val (mul_pgm, mulenv', Psi') = mulInf(reginf_pgm,Psi,cone,mulenv)
	  val _ = MulInf.contract mul_pgm
      in (mul_pgm, rse', mulenv', Psi')
      end


    (* ---------------------------------------------------------------------- *)
    (*   Perform K-normalisation                                              *)
    (* ---------------------------------------------------------------------- *)

    local open MulInf
    in fun k_norm(pgm: (place,place*mul,qmularefset ref)LambdaPgm_psi) 
	: (place,place*mul,qmularefset ref)LambdaPgm_psi =
	(chat "[K-normalisation...";
         Timing.timing_begin();
         let (* val _ = display("\nReport: just before K-normalisation:", layoutLambdaPgm pgm) *)
	     val pgm' = MulInf.k_normPgm pgm
	 in Timing.timing_end("Knorm");
	     chat "]\n";
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
	    if print_storage_mode_expression() 
               orelse !Flags.DEBUG_COMPILER then 
	          display("\nReport: Storage Mode Expression:", layout_pgm pgm')
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
	(chat "[Drop Regions...";
         Timing.timing_begin();
         let val (pgm',env') = drop_regions(env, pgm)
	 in Timing.timing_end("Drop");
	   chat "]\n";
	    if print_drop_regions_expression() then 
	      display("Report: AFTER DROP REGIONS:", AtInf.layout_pgm_brief pgm')
	    else ();
	    if print_drop_regions_expression_with_storage_modes() orelse !Flags.DEBUG_COMPILER then 
	      display("Report: AFTER DROP REGIONS (with storage modes):", AtInf.layout_pgm pgm')
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
	(chat "[Physical Size Inference...";
         Timing.timing_begin();
         let val (pgm',env') = psi(pp_counter, env, pgm)
	 in Timing.timing_end("PSI");
	   chat "]\n";
	    if print_physical_size_inference_expression() orelse !Flags.DEBUG_COMPILER then 
	      display("Report: AFTER PHYSICAL SIZE INFERENCE:", layout_pgm pgm')
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
            chat "[Checking for dangling pointers...";
            Timing.timing_begin();
            MulExp.warn_dangling_pointers(rse, psi_pgm, get_place_at);
            Timing.timing_end("Dangle");
	    chat "]\n"
        end


    (* ---------------------------------------------------------------------- *)
    (*   Do application conversion                                            *)
    (* ---------------------------------------------------------------------- *)
  
    local open PhysSizeInf
    in
      fun appConvert (pgm:((place*pp)at,place*phsize,unit)LambdaPgm): 
                          ((place*pp)at,place*phsize,unit)LambdaPgm =
	(chat "[Application Conversion...";
         Timing.timing_begin();
         let val pgm' = PhysSizeInf.appConvert(pgm)
	 in Timing.timing_end("AppConv");
	   chat "]\n";
	    if print_call_explicit_expression() orelse !Flags.DEBUG_COMPILER then 
	      display("Report: AFTER APPLICATION CONVERSION:", layout_pgm pgm')
	    else ();
	    pgm'
	 end)
    end

    type place = PhysSizeInf.place
    type 'a at = 'a PhysSizeInf.at
    type phsize = PhysSizeInf.phsize
    type pp = PhysSizeInf.pp
    type ('a, 'b, 'c) LambdaPgm = ('a, 'b, 'c) PhysSizeInf.LambdaPgm

    (************************************************************************)
    (* This is the main function; It invokes all the passes of the back end *)
    (************************************************************************)

    type target = ((place*pp)at,place*phsize,unit) LambdaPgm
    datatype res = CodeRes of CEnv * CompBasis * target * bool
                 | CEnvOnlyRes of CEnv

    fun compile fe (CEnv, Basis, strdecs) : res =
      let
        val _ = RegionExp.printcount:=1;
	val {NEnv,TCEnv,EqEnv,OEnv,rse,mulenv, mularefmap=Psi,drop_env,psi_env} =
	  CompBasis.de_CompBasis Basis
        val BtoLamb = CompBasisToLamb.mk_CompBasis{NEnv=NEnv,TCEnv=TCEnv,EqEnv=EqEnv,OEnv=OEnv}
      in
        case CompileToLamb.compile fe (CEnv, BtoLamb, strdecs) of
          CompileToLamb.CEnvOnlyRes CEnv1 => CEnvOnlyRes CEnv1
        | CompileToLamb.CodeRes (CEnv1, BtoLamb1, lamb_opt, safe) => 
          let 
            val {NEnv=NEnv1,TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1} = CompBasisToLamb.de_CompBasis BtoLamb1
            val (mul_pgm, rse1, mulenv1, Psi1) = SpreadRegMul(rse, Psi, mulenv, lamb_opt)
	    val _ = MulExp.warn_puts(rse, mul_pgm)
	    val k_mul_pgm = k_norm mul_pgm
	    val sma_pgm = storagemodeanalysis k_mul_pgm
	    val (drop_pgm, drop_env1) = drop_regions(drop_env,sma_pgm)
	    val (psi_pgm, psi_env1) = phys_size_inf(psi_env, drop_pgm)
	    val _ = warn_dangling_pointers(rse, psi_pgm)
	    val app_conv_psi_pgm = appConvert psi_pgm
	    val Basis' = CompBasis.mk_CompBasis {NEnv=NEnv1,TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1,
						 rse=rse1,mulenv=mulenv1,mularefmap=Psi1,
						 drop_env=drop_env1,psi_env=psi_env1}
	  in CodeRes (CEnv1, Basis', app_conv_psi_pgm, safe)
	  end
      end

    (* Hook to be run before any compilation. 
     * Overwrite hook for CompileToLamb, which is a noop. *)
    fun preHook():unit =
	let (* val _ = print ("In preHook\n") *)
	in
	    Effect.resetCount() (* if "-regionvar n" is provided, 
				 * the first effectvar/regionvar 
				 * gets id n. *)
(*	    before print "[Exiting preHook]\n" *)
	end
    
    fun pairToFile (a,b) file =
	let
	    fun outString {file:string,s:string} =
		let (* val _ = print ("[Trying to write string to file " ^ file ^ "]\n") *)
		    val os = TextIO.openOut file
		in TextIO.output(os,s) before TextIO.closeOut os
		    handle X => (TextIO.closeOut os ; raise X)
		end
	in
	    outString {file=file, s=Int.toString a ^ " " ^ Int.toString b}
	end

    (* Hook to be run after all compilations (for one compilation unit)
     * Overwrite hook for CompileToLamb, which is a noop. *)
    fun postHook {unitname:string} : unit =
	if !(Flags.lookup_int_entry "regionvar") = ~1 then ()
	else
	let (* val _ = print "[In postHook]\n" *)
	    val pair = Effect.getCountFirstLast()
	    val file = unitname ^ ".rev" (* region/effect variable *)
	in pairToFile pair file
	end
  end;
