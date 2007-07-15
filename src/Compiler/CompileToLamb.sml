
structure CompileToLamb: COMPILE_TO_LAMB =
  struct
    structure PP = PrettyPrint
    structure CE = CompilerEnv

    type CompBasis = CompBasisToLamb.CompBasis
    type CEnv = CompilerEnv.CEnv
    type Env = CompileDec.Env
    type strdec = CompileDec.strdec
    type strexp = CompileDec.strexp
    type funid = CompileDec.funid
    type strid = CompileDec.strid

    fun die s = Crash.impossible ("CompileToLamb." ^ s)

    (* ---------------------------------------------------------------------- *)
    (*  Dynamic Flags.                                                        *)
    (* ---------------------------------------------------------------------- *)

    val eliminate_polymorphic_equality_p = Flags.is_on0 "eliminate_polymorphic_equality"

    val type_check_lambda_p = Flags.is_on0 "type_check_lambda"

    val print_opt_lambda_expression = Flags.is_on0 "print_opt_lambda_expression" 

    val safeLinkTimeElimination = Flags.add_bool_entry 
	{long="safeLinkTimeElimination", short=NONE, 
	 menu=["Control",
	       "safeLinkTimeElimination"],
	 item=ref false, neg=false, desc=
	 "Threat this module as a library in the sense that\n\
	  \the code can be eliminated if it is not used."}

    (* ---------------------------------------------------------------------- *)
    (*  Printing utilities                                                    *)
    (* ---------------------------------------------------------------------- *)

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
      
    val layoutLambdaPgm = LambdaExp.layoutLambdaPgm 

    (* ---------------------------------------------------------------------- *)
    (*  Compile the declaration using old compiler environment, ce            *)
    (* ---------------------------------------------------------------------- *)

    fun ast2lambda fe (ce, strdecs) =
      (chat "[Compiling abstract syntax tree into lambda language...";

(*       Timing.timing_begin(); *)  

       (* timing does not work with functor inlining because it triggers reelaboration,
	* which is also timed. mael 2003-02-18 *)

       let val _ = LambdaExp.reset()  (* Reset type variable counter to improve pretty printing; The generated
				       * Lambda programs are closed w.r.t. type variables, because code 
				       * generation of the strdecs is done after an entire top-level 
				       * declaration is elaborated. ME 1998-09-04 *)
(*
	   val (ce1, lamb) =  Timing.timing_end_res 
	        ("ToLam", CompileDec.compileStrdecs fe ce strdecs)
*)
	   val (ce1, lamb) = CompileDec.compileStrdecs fe ce strdecs
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
      if type_check_lambda_p() then
	(chat "[Type checking lambda term...";
	 Timing.timing_begin();
	 let 
	   val env' = Timing.timing_end_res ("CheckLam",(LambdaStatSem.type_check {env = a,  letrec_polymorphism_only = false,
                  pgm =  b}))
	 in
	   chat "]\n";
	   env'
	 end)
      else LambdaStatSem.empty


    (* ---------------------------------------------------------------------- *)
    (*   Eliminate polymorphic equality in the lambda code                    *)
    (* ---------------------------------------------------------------------- *)

    fun elim_eq_lambda (env,lamb) =
      if eliminate_polymorphic_equality_p() then
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

    val optimise_p = Flags.is_on0 "optimiser"

    fun optlambda (env, lamb) =
          ((if optimise_p() then chat "[Optimising lambda term..."
	    else chat "[Rewriting lambda term...");
	   Timing.timing_begin();
	   let 
	     val (lamb_opt, env') = 
	           Timing.timing_end_res ("OptLam", OptLambda.optimise(env,lamb))
	   in
	     chat "]\n";
	     if !Flags.DEBUG_COMPILER orelse print_opt_lambda_expression()
	     then display("Report: Opt", layoutLambdaPgm lamb_opt) else () ;
	     (lamb_opt, env')
	   end)


    (****************************************************)
    (* This is the main function; It invokes all passes *)
    (****************************************************)

    type target = LambdaExp.LambdaPgm
    datatype res = CodeRes of CEnv * CompBasis * target * bool
                 | CEnvOnlyRes of CEnv

    fun compile fe (CEnv, Basis, strdecs) : res =
      let

	(* There is only space in the basis for one lambdastat-env.
	 * If we want more checks, we should insert more components
	 * in bases. For now, we do type checking after optlambda, only. *)

	val {TCEnv,EqEnv,OEnv} = CompBasisToLamb.de_CompBasis Basis

        val (lamb,CEnv1, declared_lvars, declared_excons) = ast2lambda fe (CEnv, strdecs)
	val (lamb',EqEnv1) = elim_eq_lambda (EqEnv, lamb)
        val (lamb_opt,OEnv1) = optlambda (OEnv, lamb')
	val TCEnv1 = type_check_lambda (TCEnv, lamb_opt)
      in
	if isEmptyLambdaPgm lamb_opt 
          then (chat "Empty lambda program; skipping code generation.";
                CEnvOnlyRes CEnv1)
	else
	  let
	    val safe = LambdaExp.safeLambdaPgm lamb_opt
	    val Basis1 = CompBasisToLamb.mk_CompBasis{TCEnv=TCEnv1,EqEnv=EqEnv1,OEnv=OEnv1}
	  in CodeRes (CEnv1, Basis1, lamb_opt, safe orelse safeLinkTimeElimination())
	  end
      end

    (* Hook to be run before any compilation *)
    fun preHook():unit = ()
    
    (* Hook to be run after all compilations (for one compilation unit) *)
    fun postHook {unitname:string} : unit = ()

  end
