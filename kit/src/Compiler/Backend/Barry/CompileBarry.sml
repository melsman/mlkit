(* Standard ML Barifyer *)
      
signature COMPILE_BARRY =
  sig

    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CompBasis
    type CEnv 
    type strdec

    type LambdaPgm

    datatype res = CodeRes of CEnv * CompBasis * LambdaPgm * bool
                 | CEnvOnlyRes of CEnv      (* the boolean is true (safe) if the code has no side-effects *)

    (* emit: returns the filename for the generated .sml file *)
    val emit : {target: LambdaPgm, filename: string} -> string  

    val compile : CEnv * CompBasis * strdec list -> res
  end 


functor CompileBarry
               (structure LambdaExp: LAMBDA_EXP

	        structure LambdaStatSem : LAMBDA_STAT_SEM
		  sharing type LambdaStatSem.LambdaPgm = LambdaExp.LambdaPgm

	        structure EliminateEq : ELIMINATE_EQ 
		  sharing type EliminateEq.LambdaPgm = LambdaExp.LambdaPgm

		structure CompilerEnv: COMPILER_ENV
		  sharing type CompilerEnv.lvar = LambdaExp.lvar
                  sharing type CompilerEnv.excon = LambdaExp.excon
                  sharing type CompilerEnv.Type = LambdaExp.Type

                structure CompileDec: COMPILE_DEC
		  sharing type CompileDec.LambdaPgm = LambdaExp.LambdaPgm
                  sharing type CompileDec.CEnv = CompilerEnv.CEnv

                structure OptLambda: OPT_LAMBDA
		  sharing type OptLambda.LambdaPgm = LambdaExp.LambdaPgm

		structure CompBasis: COMP_BASIS_BARRY
		  sharing type CompBasis.EqEnv = EliminateEq.env
		  sharing type CompBasis.OEnv = OptLambda.env
		  sharing type CompBasis.TCEnv = LambdaStatSem.env 

                structure Report: REPORT
		structure Flags: FLAGS

		structure PP: PRETTYPRINT
		  sharing type CompBasis.StringTree
                                     = LambdaExp.StringTree
			             = EliminateEq.StringTree
                                     = CompilerEnv.StringTree
                                     = PP.StringTree

                  sharing type PP.Report = Report.Report
			
	        structure Name : NAME

                structure Crash: CRASH
		structure Timing: TIMING
		  ): COMPILE_BARRY =

  struct

    structure CE = CompilerEnv

    type CompBasis = CompBasis.CompBasis
    type CEnv = CompilerEnv.CEnv
    type strdec = CompileDec.strdec
    type LambdaPgm = LambdaExp.LambdaPgm

    fun die s = Crash.impossible ("Compile." ^ s)

    (* ---------------------------------------------------------------------- *)
    (*  Dynamic Flags.                                                        *)
    (* ---------------------------------------------------------------------- *)

    val print_opt_lambda_expression = 
      Flags.lookup_flag_entry "print_opt_lambda_expression" 

    (* ---------------------------------------------------------------------- *)
    (*  Printing utilities                                                    *)
    (* ---------------------------------------------------------------------- *)

    local
      fun msg(s: string) = (TextIO.output(!Flags.log, s); 
			    TextIO.flushOut (!Flags.log))
    in
      fun chat(s: string) = if !Flags.chat then msg (s^"\n") else ()
    end

    fun fast_pr stringtree = 
           (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
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
      
    val layoutLambdaPgm = LambdaExp.layoutLambdaPgm 

    (* ---------------------------------------------------------------------- *)
    (*  Compile the declaration using old compiler environment, ce            *)
    (* ---------------------------------------------------------------------- *)

    fun ast2lambda(ce, strdecs) =
      (chat "[Compiling abstract syntax tree into lambda language...";
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
	 chat "]\n";
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
	(chat "[Type checking lambda term...";
	 Timing.timing_begin();
	 let 
	   val env' = Timing.timing_end_res 
	       ("CheckLam", LambdaStatSem.type_check {env = a,  
						      letrec_polymorphism_only = false,
						      pgm = b})
	 in
	   chat "]\n";
	   env'
	 end)
      else LambdaStatSem.empty


    (* ---------------------------------------------------------------------- *)
    (*   Eliminate polymorphic equality in the lambda code                    *)
    (* ---------------------------------------------------------------------- *)

    fun elim_eq_lambda (env,lamb) =
      if Flags.is_on "eliminate_polymorphic_equality" then
	(chat "[Eliminating polymorphic equality...";
	 Timing.timing_begin();
	 let val (lamb', env') = 
	   Timing.timing_end_res ("ElimEq", EliminateEq.elim_eq (env, lamb))
	 in
	   chat "]\n";
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
          ((if !Flags.optimiser then chat "[Optimising lambda term..."
	    else chat "[Rewriting lambda term...");
	   Timing.timing_begin();
	   let 
	     val (lamb_opt, env') = 
	           Timing.timing_end_res ("OptLam", OptLambda.optimise(env,lamb))
	   in
	     chat "]\n";
	     if !Flags.DEBUG_COMPILER orelse !print_opt_lambda_expression
	     then display("Report: Opt", layoutLambdaPgm lamb_opt) else () ;
	     (lamb_opt, env')
	   end)

    (************************************************************************)
    (* This is the main function; It invokes all the passes of the back end *)
    (************************************************************************)

    datatype res = CodeRes of CEnv * CompBasis * LambdaPgm * bool
                 | CEnvOnlyRes of CEnv

    fun compile(CEnv, Basis, strdecs) : res =
      let

	(* There is only space in the basis for one lambdastat-env.
	 * If we want more checks, we should insert more components
	 * in bases. For now, we do type checking after optlambda, only. *)

	val {TCEnv,EqEnv,OEnv} = CompBasis.de_CompBasis Basis

        val (lamb,CEnv1, declared_lvars, declared_excons) = ast2lambda(CEnv, strdecs)
	val (lamb',EqEnv1) = elim_eq_lambda (EqEnv, lamb)
        val (lamb_opt,OEnv1) = optlambda (OEnv, lamb')
	val TCEnv1 = type_check_lambda (TCEnv, lamb_opt)
      in
	if isEmptyLambdaPgm lamb_opt 
          then (chat "Empty lambda program; skipping code generation.";
                CEnvOnlyRes CEnv1)
	else
	  let val safe = LambdaExp.safeLambdaPgm lamb_opt
	      val Basis' = CompBasis.mk_CompBasis {TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1}
	  in CodeRes (CEnv1, Basis', lamb_opt, safe)
	  end
      end

    fun emit {target: LambdaPgm, filename} : string =
	let val filename = filename ^ ".sml"
	    val colwidth = 100
	    val st = LambdaExp.barify target
	    val os = TextIO.openOut filename
	in  (TextIO.output(os, "(* Generated by Barry - the Standard ML barifier *)\n\n");
	     PP.outputTree (fn s => TextIO.output(os,s), st, colwidth);
	     TextIO.output(os, "\n\n");	     
	     TextIO.closeOut os;
	     print ("[wrote ML file:\t" ^ filename ^ "]\n");
	     filename) handle X => (TextIO.closeOut os; raise X)
	end
	
  end
