functor CodeGenKAM(structure PhysSizeInf : PHYS_SIZE_INF
		   structure Con : CON
		   structure Excon : EXCON
		   structure Lvars : LVARS
		   structure Effect : EFFECT
		   structure Labels : ADDRESS_LABELS
		   structure RegvarFinMap : MONO_FINMAP
                     sharing type RegvarFinMap.dom =
		                  Effect.effect =
				  Effect.place =
				  PhysSizeInf.place
		   structure CallConv: CALL_CONV
		   structure ClosExp: CLOS_EXP
 	             sharing type Con.con = ClosExp.con
		     sharing type Excon.excon = ClosExp.excon
                     sharing type Lvars.lvar = ClosExp.lvar = CallConv.lvar
                     sharing type Effect.effect = Effect.place = ClosExp.place
                     sharing type Labels.label = ClosExp.label
                     sharing type CallConv.cc = ClosExp.cc
		     sharing type ClosExp.phsize = PhysSizeInf.phsize
		   structure BI : BACKEND_INFO
                   structure JumpTables : JUMP_TABLES
		   structure Lvarset: LVARSET
		     sharing type Lvarset.lvar = Lvars.lvar
		   structure Kam: KAM
                     sharing type Kam.label = Labels.label
		   structure BuiltInCFunctions : BUILT_IN_C_FUNCTIONS_KAM
		   structure PP : PRETTYPRINT
		     sharing type PP.StringTree = 
		                  Effect.StringTree = 
				  ClosExp.StringTree =
                                  Kam.StringTree =
				  RegvarFinMap.StringTree =
				  Lvars.Map.StringTree
                   structure Flags : FLAGS
		   structure Report : REPORT
		     sharing type Report.Report = Flags.Report
		   structure Crash : CRASH) : CODE_GEN_KAM (* : sig end *) =

struct
  structure LvarFinMap = Lvars.Map

  open Kam

  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  datatype phsize = datatype PhysSizeInf.phsize
  type pp = PhysSizeInf.pp
  type cc = CallConv.cc
  type label = Labels.label
  type ClosPrg = ClosExp.ClosPrg

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CodeGenKAM." ^ s)

  fun fast_pr stringtree = 
    (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
     TextIO.output(!Flags.log, "\n"))

  fun display(title, tree) =
    fast_pr(PP.NODE{start=title ^ ": ",
		    finish="",
		    indent=3,
		    children=[tree],
		    childsep=PP.NOSEP
		    })

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_kam_program", "print kam program", ref false)]

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("comments_in_kam_code", "comments in kam code", ref true)]

  val comments_in_kam_code = Flags.lookup_flag_entry "comments_in_kam_code"
  val jump_tables = Flags.lookup_flag_entry "jump_tables"

  (*************)
  (* Utilities *)
  (*************)
  fun zip ([],[]) = []
    | zip ((x::xs),(y::ys)) = (x,y) :: (zip (xs,ys))
    | zip _ = die "zip: Cannot zip two lists."

  fun is_region_real place =
    (case Effect.get_place_ty place
       of NONE => die "LETREGION.alloc.regvar has no runtype."
	| SOME Effect.REAL_RT => true
	| SOME Effect.STRING_RT => false
	| SOME _ => false)

  (* Check to inforce that datalabels that are exported are indeed defined *)

  local 
      val export_labs : label list ref = ref nil
      fun member l =
	let fun mem nil = false
	      | mem (x::xs) = Labels.eq(l,x) orelse mem xs
	in mem (!export_labs)
	end
  in 
      fun setExportLabs ls = export_labs := ls
      fun storeData l =
	if member l then StoreData l
	else die ("Label " ^ Labels.pr_label l ^ " is not defined")
  end

  (***************************)
  (* Compiler Environment CE *)
  (***************************)
  structure LvarFinMap = Lvars.Map
  datatype access_type =
      REG_I of int
    | REG_F of int
    | STACK of int
    | ENV of int
    | ENV_REG

  type VarEnv     = access_type LvarFinMap.map
  type RhoEnv     = access_type RegvarFinMap.map
  type env = {VarEnv    : VarEnv,
	      RhoEnv    : RhoEnv}

  val initialVarEnv : VarEnv = LvarFinMap.empty
  val initialRhoEnv : RhoEnv = RegvarFinMap.empty
  val initialEnv = {VarEnv     = initialVarEnv,
		    RhoEnv     = initialRhoEnv}

  fun plus ({VarEnv,RhoEnv}, {VarEnv=VarEnv',RhoEnv=RhoEnv'}) =
    {VarEnv     = LvarFinMap.plus(VarEnv,VarEnv'),
     RhoEnv     = RegvarFinMap.plus(RhoEnv,RhoEnv')}

  fun declareLvar (lvar,access_type,{VarEnv,RhoEnv}) =
    {VarEnv     = LvarFinMap.add(lvar,access_type,VarEnv),
     RhoEnv     = RhoEnv}

  fun declareRho (place,access_type,{VarEnv,RhoEnv}) =
    {VarEnv     = VarEnv,
     RhoEnv     = RegvarFinMap.add(place,access_type,RhoEnv)}

  fun lookupVar ({VarEnv,...} : env) lvar = 
    case LvarFinMap.lookup VarEnv lvar of
      SOME access_type => access_type
    | NONE  => die ("lookupVar(" ^ (Lvars.pr_lvar lvar) ^ ")")

  fun lookupVarOpt ({VarEnv,...} : env) lvar = LvarFinMap.lookup VarEnv lvar

  fun lookupRho ({RhoEnv,...} : env) place =
    case RegvarFinMap.lookup RhoEnv place of
      SOME access_type => access_type
    | NONE  => die ("lookupRho(" ^ (PP.flatten1(Effect.layout_effect place)) ^ ")")
(*
  fun lookupRhoOpt ({RhoEnv,...} : env) place = RegvarFinMap.lookup RhoEnv place
*)
  (* --------------------------------------------------------------------- *)
  (*                          Pretty Printing                              *)
  (* --------------------------------------------------------------------- *)

  type StringTree = PP.StringTree
  val rec layoutEnv : env -> StringTree = fn {VarEnv,RhoEnv} =>
    PP.NODE{start="CodeGenKamEnv(",finish=")",indent=2,
	    children=[layoutVarEnv VarEnv,layoutRhoEnv RhoEnv],
	    childsep=PP.RIGHT ","}

  and layoutVarEnv = fn VarEnv =>
    PP.NODE{start="VarEnv = ",finish="",indent=2,childsep=PP.NOSEP,
	    children=[LvarFinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
		      (PP.layoutAtom Lvars.pr_lvar)
		      layout_access_type
		      VarEnv]}

  and layoutRhoEnv = fn RhoEnv =>
    PP.NODE{start="RhoEnv = ",finish="",indent=2,childsep=PP.NOSEP,
	    children=[RegvarFinMap.layoutMap {start="{",eq=" -> ", sep=", ", finish="}"}
		      (PP.layoutAtom (PP.flatten1 o Effect.layout_effect))
		      layout_access_type
		      RhoEnv]}

  and layout_access_type =
    fn REG_I i => PP.LEAF("REG_I(" ^ (Int.toString i) ^ ")")
     | REG_F i => PP.LEAF("REG_F(" ^ (Int.toString i) ^ ")")
     | STACK i => PP.LEAF("STACK(" ^ (Int.toString i) ^ ")")
     | ENV i => PP.LEAF("ENV(" ^ (Int.toString i) ^ ")")
     | ENV_REG => PP.LEAF("ENVREG")

  and pr_access_type = 
    fn acc_ty => PP.flatten1(layout_access_type acc_ty)


  (* ----------------------------------------------------------------------------
   * Dead code elimination; during code generation we eliminate code that is non-
   * reachable by eliminating code from the continuation---down to a label---when 
   * a jump or a return is generated.
   * ---------------------------------------------------------------------------- *)

  fun dead_code_elim continuation =
    case continuation
      of Label _ :: _ => continuation
       | DotLabel _ :: _ => continuation
       | _ :: rest => dead_code_elim rest
       | nil => continuation

  (* -----------------------------------------------------------------
   * Peep hole optimization: we define functions here that takes the
   * continuation as an extra parameter, which can be inspected and
   * merged with the instruction proper.
   * ----------------------------------------------------------------- *)

  fun pop (i, acc) =
    if i > 0 then
      case acc 
	of Pop n :: Push :: acc => PopPush(n+i) :: acc
	 | PopPush n :: acc => PopPush(n+i) :: acc
	 | Push :: acc => PopPush i :: acc
	 | Pop n :: acc => Pop(n+i) :: acc
	 | _ => Pop i :: acc
    else if i < 0 then die "pop(i). i < 0"
	 else (*i=0*) acc

  fun push acc =
    case acc
      of Pop i :: acc =>
	if i > 0 then pop(i-1, acc)
	else if i < 0 then die "push"
	     else Push :: acc
       | _ => Push :: acc

  fun selectEnv(i, s, acc) =
    case acc
      of ClearAtbotBit :: Push :: acc => SelectEnvClearAtbotBitPush i :: acc
       | Push :: acc => SelectEnvPush i :: acc
       | _ => SelectEnv (i,s) :: acc

  fun select(i, acc) =
    case acc
      of Push :: acc => SelectPush i :: acc
       | _ => Select i :: acc

  fun immedInt (i, acc) =
    case acc
      of Push :: acc => ImmedIntPush i :: acc
       | _ => ImmedInt i :: acc

  fun stackOffset(i, acc) =
    case acc 
      of StackOffset n :: acc => StackOffset(n+i)::acc
       | _ => StackOffset i :: acc

  (***********************)
  (* Code Generation     *)
  (***********************)

  local
    fun selectStack(i, s, acc) =
      case acc
	of Push :: acc => SelectStackPush i :: acc
	 | _ => SelectStack(i,s) :: acc

    fun envToAcc acc =
      case acc
	of Push :: acc => EnvPush :: acc
	 | _ => EnvToAcc :: acc

    fun stackAddrInfBit (i, s, acc) =
      case acc
	of SetAtbotBit :: Push :: acc => StackAddrInfBitAtbotBitPush i :: acc
	 | _ => StackAddrInfBit (i, s) :: acc

    fun stackAddr (i, s, acc) =
      case acc
	of Push :: acc => StackAddrPush (i,s) :: acc
	 | _ => StackAddr (i, s) :: acc

    fun access_lv(lv,env,sp,acc) =
      case lookupVar env lv of
	STACK i => selectStack(0-sp+i, Lvars.pr_lvar lv, acc)
      | ENV i => selectEnv(i, Lvars.pr_lvar lv, acc)
      |	REG_I i => stackAddrInfBit(0-sp+i, Lvars.pr_lvar lv, acc)
      | REG_F i => stackAddr(0-sp+i, Lvars.pr_lvar lv, acc)
      | ENV_REG => envToAcc acc

    fun access_rho(rho,env,sp,acc) =
      case lookupRho env rho of
	STACK i => selectStack(0-sp+i, ClosExp.pr_rhos [rho], acc)
      | ENV i => selectEnv(i, ClosExp.pr_rhos [rho], acc)
      |	REG_I i => stackAddrInfBit(0-sp+i, ClosExp.pr_rhos [rho], acc)
      | REG_F i => stackAddr(0-sp+i, ClosExp.pr_rhos [rho], acc)
      | ENV_REG => envToAcc acc

    fun num_args_cc(cc) =
      let
	val decomp_cc = CallConv.decompose_cc cc
      in
	List.length (#reg_args(decomp_cc)) +
	List.length (#args(decomp_cc))
      end

    fun comment(str,C) = 
      if !comments_in_kam_code then Comment str :: C
      else C

    (* Compile Switch Statements *)
    local
      fun new_label str = Labels.new_named str
      fun label(lab,C) = Label lab :: C
      fun jmp(lab,C) = JmpRel lab :: dead_code_elim C
    in
      fun linear_search(sels,
			default,
			if_no_match_go_lab_sel: label * 'sel * KamInst list -> KamInst list,
			compile_insts: ClosExp.ClosExp * KamInst list -> KamInst list,
			C) =
	  JumpTables.linear_search_new(sels,
				       default,
				       comment,
				       new_label,
				       if_no_match_go_lab_sel,
				       compile_insts,
				       label,
				       jmp,
				       C)
	  
      fun binary_search(sels,
			default,
			if_not_equal_go_lab_sel: label * int * KamInst list -> KamInst list,
			if_less_than_go_lab_sel: label * int * KamInst list -> KamInst list,
			if_greater_than_go_lab_sel: label * int * KamInst list -> KamInst list,
			compile_insts: ClosExp.ClosExp * KamInst list -> KamInst list,
			C) =
	if !jump_tables then
	  JumpTables.binary_search_new(sels,
				       default,
				       comment,
				       new_label,
				       if_not_equal_go_lab_sel,
				       if_less_than_go_lab_sel,
				       if_greater_than_go_lab_sel,
				       compile_insts,
				       label,
				       jmp,
				       fn (sel1,sel2) => Int.abs(sel1-sel2),
				       fn (lab,sel,C) => JmpVector(lab,sel)::C,
				       fn (lab,C) => DotLabel(lab) :: C, (* add_label_to_jump_tab  *)
				       eq_lab,C)
	else
	  linear_search(sels,
			default,
			if_not_equal_go_lab_sel,
			compile_insts,
			C)
    end

(*    fun num_args_cc cc =
      let
	val decomp_cc = CallConv.decompose_cc cc
      in
	List.length (#reg_args(decomp_cc)) +
	List.length (#args(decomp_cc))
      end not used anyway 2000-10-15, Niels *)


    fun CG_ce(ClosExp.VAR lv,env,sp,cc,acc)             = access_lv(lv,env,sp,acc)
      | CG_ce(ClosExp.RVAR place,env,sp,cc,acc)         = access_rho(place,env,sp,acc)
      | CG_ce(ClosExp.DROPPED_RVAR place,env,sp,cc,acc) = die "DROPPED_RVAR not implemented"
      | CG_ce(ClosExp.FETCH lab,env,sp,cc,acc)          = FetchData lab :: acc
      | CG_ce(ClosExp.STORE(ce,lab),env,sp,cc,acc)      = CG_ce(ce,env,sp,cc, storeData lab :: acc)
      | CG_ce(ClosExp.INTEGER i,env,sp,cc,acc)          = immedInt (i, acc)
      | CG_ce(ClosExp.STRING s,env,sp,cc,acc)           = ImmedString s :: acc
      | CG_ce(ClosExp.REAL s,env,sp,cc,acc)             = ImmedReal s :: acc
      | CG_ce(ClosExp.PASS_PTR_TO_MEM(sma,i),env,sp,cc,acc) = alloc(sma,i,env,sp,cc,acc)
      | CG_ce(ClosExp.PASS_PTR_TO_RHO sma,env,sp,cc,acc) = set_sm(sma,env,sp,cc,acc)
      | CG_ce(ClosExp.UB_RECORD ces,env,sp,cc,acc) = comp_ces(ces,env,sp,cc,acc)
      (* Layout of closure [label,rho1,...,rhon,excon1,...exconm,lv1,...,lvo], see build_clos_env in ClosExp *)
      | CG_ce(ClosExp.CLOS_RECORD{label,elems=(lvs,excons,rhos),alloc},env,sp,cc,acc) =
      PushLbl(label) :: (comp_ces_to_block(rhos @ excons @ lvs,1,env,sp+1,cc,alloc,acc))
      (* Layout of closure [rho1,...,rhon,excon1,...exconm,lv1,...,lvo], see build_clos_env in ClosExp *)
      | CG_ce(ClosExp.SCLOS_RECORD{elems=(lvs,excons,rhos),alloc},env,sp,cc,acc) = 
      comp_ces_to_block(rhos @ excons @ lvs,0,env,sp,cc,alloc,acc)
      | CG_ce(ClosExp.REGVEC_RECORD{elems,alloc},env,sp,cc,acc) = die "REGVEC_RECORD not used in this back end"
      | CG_ce(ClosExp.RECORD{elems,alloc,tag},env,sp,cc,acc) = comp_ces_to_block(elems,0,env,sp,cc,alloc,acc)
      | CG_ce(ClosExp.SELECT(i,ce as ClosExp.VAR lv),env,sp,cc,acc) = 
      (* This may be a SelectEnv? *)
      if Lvars.eq(lv,Lvars.env_lvar) then
	selectEnv(i, Lvars.pr_lvar lv,acc)
      else
	CG_ce(ce,env,sp,cc, select(i,acc))
      | CG_ce(ClosExp.SELECT(i,ce),env,sp,cc,acc) = CG_ce(ce,env,sp,cc, select(i,acc))
      | CG_ce(ClosExp.FNJMP{opr,args,clos=NONE,free=[]},env,sp,cc,acc) = 
      CG_ce(opr,env,sp,cc,
	    push (comp_ces(args,env,sp+1,cc, 
			   ApplyFnJmp(List.length args, sp) :: 
			   dead_code_elim acc)))
      | CG_ce(ClosExp.FNJMP{opr,args,clos,free},env,sp,cc,acc) = die "FNJMP: either clos or free are non empty."
      | CG_ce(ClosExp.FNCALL{opr,args,clos=NONE,free=[]},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(opr,env,sp+1,cc,
	      push (comp_ces(args,env,sp+2,cc,
			     ApplyFnCall(List.length args) :: Label(return_lbl) :: acc)))
      end
      | CG_ce(ClosExp.FNCALL{opr,args,clos,free},env,sp,cc,acc) = 
      die "FNCALL: either clos or free are non empty."      
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=NONE,free=[]},env,sp,cc,acc) =
      ImmedIntPush 0 ::        (* is it always all the region arguments that are reused? *)
      comp_ces(args,env,sp+1,cc,
	       ApplyFunJmp(opr,List.length args,sp - (List.length reg_args)) :: 
	       dead_code_elim acc)
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce,free=[]},env,sp,cc,acc) =
      CG_ce(clos_ce,env,sp,cc,
	    push (comp_ces(args,env,sp+1,cc,
			   ApplyFunJmp(opr,List.length args,sp - (List.length reg_args)) :: 
			   dead_code_elim acc)))
      | CG_ce(ClosExp.JMP{opr,args,reg_vec,reg_args,clos,free},env,sp,cc,acc) = die "JMP either reg_vec or free are non empty."
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos=NONE,free=[]},env,sp,cc,acc) =
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	ImmedIntPush 0 ::
	comp_ces(reg_args @ args,env,sp+2,cc,
		 ApplyFunCall(opr,List.length args + List.length reg_args) :: 
		 Label(return_lbl) :: acc)
      end
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce,free=[]},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(clos_ce,env,sp+1,cc, 
	      push (comp_ces(reg_args @ args,env,sp+2,cc,
			     ApplyFunCall(opr,List.length args + List.length reg_args) :: 
			     Label(return_lbl) :: acc)))
      end
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec,reg_args,clos,free},env,sp,cc,acc) = die "FUNCALL: either reg_vec or free are non empty."
      | CG_ce(ClosExp.LETREGION{rhos,body},env,sp,cc,acc) = 
      let
	fun comp_alloc_rhos([],env,sp,cc,fn_acc) = fn_acc(env,sp)
	  | comp_alloc_rhos((place,PhysSizeInf.INF)::rs,env,sp,cc,fn_acc) = 
	  LetregionInf ::
	  comp_alloc_rhos(rs,declareRho(place,REG_I(sp),env),sp+(BI.size_of_reg_desc()),cc,fn_acc)
	  | comp_alloc_rhos((place,PhysSizeInf.WORDS 0)::rs,env,sp,cc,fn_acc) = 
	  (* it seems that finite rhos of size 0 actually exists in env? 2000-10-08, Niels 
	   * and code is actually generated when passing arguments in region polymorphic functions??? *)
	  comp_alloc_rhos(rs,declareRho(place,REG_F(sp),env),sp,cc,fn_acc) 
	  | comp_alloc_rhos((place,PhysSizeInf.WORDS i)::rs,env,sp,cc,fn_acc) = 
	  stackOffset(i,
	  comp_alloc_rhos(rs,declareRho(place,REG_F(sp),env),sp+i,cc,fn_acc))

	fun comp_dealloc_rho((place,PhysSizeInf.INF), acc) =  EndregionInf :: acc
	  | comp_dealloc_rho((place,PhysSizeInf.WORDS 0), acc) = acc
	  | comp_dealloc_rho((place,PhysSizeInf.WORDS i), acc) = pop(i, acc)
      in
	comment ("Letregion <" ^ (ClosExp.pr_rhos (List.map #1 rhos)) ^ ">",
	comp_alloc_rhos(rhos,env,sp,cc,
			fn (env,sp) => CG_ce(body,env,sp,cc,
					     (List.foldl (fn (rho,acc) => 
							  comp_dealloc_rho (rho,acc)) acc rhos))))
      end
      | CG_ce(ClosExp.LET{pat=[],bind,scope},env,sp,cc,acc) = die "LET with pat = []."
      | CG_ce(ClosExp.LET{pat,bind,scope},env,sp,cc,acc) = 
      let
	val n = List.length pat
	fun declareLvars([],sp,env) = env
	  | declareLvars(lv::lvs,sp,env) = declareLvars(lvs,sp+1,declareLvar(lv,STACK(sp),env))
      in
	comment ("Let <" ^ (ClosExp.pr_lvars pat) ^ ">",
	CG_ce(bind,env,sp,cc, 
	      push (CG_ce(scope,declareLvars(pat,sp,env),sp+n,cc, pop(n, acc)))))
      end
      | CG_ce(ClosExp.RAISE ce,env,sp,cc,acc) = CG_ce(ce,env,sp,cc,Raise::acc)
      | CG_ce(ClosExp.HANDLE(ce1,ce2),env,sp,cc,acc) =
      (* An exception handler on the stack contains the following fields: *)
      (* sp[offset+2] = pointer to previous exception handler used when updating exnPtr. *)
      (* sp[offset+1] = pointer to handle closure.                          *)
      (* sp[offset] = label for handl_return code.                      *)
      (* Note that we call deallocate_regions_until to the address above the exception handler, *)
      (* when an exception is raised.  *)
      (* We must store the environment for the surrounding function in the handle to be restored when *)
      (* returning from the handle function. Just some thoughts. 2000-12-10, Niels *)
(* original, 22.18 2000-12-10, Niels     let
	val return_lbl = Labels.new_named "return_handle"
      in
	CG_ce(ce2,env,sp,cc, PushLbl return_lbl :: Push :: PushExnPtr ::	 
	      CG_ce(ce1,env,sp+3,cc, PopExnPtr :: Pop(2) :: Label return_lbl :: acc))
      end*)

      let
	val return_lbl = Labels.new_named "return_handle"
      in
	CG_ce(ce2,env,sp,cc, PushLbl return_lbl :: EnvPush :: Push :: PushExnPtr ::	 
	      CG_ce(ce1,env,sp+4,cc, PopExnPtr :: Pop(3) :: Label return_lbl :: acc))
      end

      | CG_ce(ClosExp.SWITCH_I (ClosExp.SWITCH(ce,sels,default)),env,sp,cc,acc) = 
      CG_ce(ce,env,sp,cc, 
            binary_search(sels,
			  default,
			  fn (lab,i,C) => IfNotEqJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfLessThanJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfGreaterThanJmpRelImmed (lab,i) :: C,
			  fn (ce,C) => CG_ce(ce,env,sp,cc,C),
			  acc))
      | CG_ce(ClosExp.SWITCH_S sw,env,sp,cc,acc) = die "SWITCH_S is unfolded in ClosExp"
      | CG_ce(ClosExp.SWITCH_C (ClosExp.SWITCH(ce,sels,default)),env,sp,cc,acc) =
      let (* NOTE: selectors in sels are tagged in ClosExp but the operand is tagged here! *)
	val con_kind = 
	  (case sels of
	     [] => ClosExp.ENUM 1 (*necessary to compile non-optimized programs (OptLambda off) *)
	   | ((con,con_kind),_)::rest => con_kind)
	val sels' = map (fn ((con,con_kind),sel_ce) => 
			 case con_kind of
			   ClosExp.ENUM i => (i,sel_ce)
			 | ClosExp.UNBOXED i => (i,sel_ce)
			 | ClosExp.BOXED i => (i,sel_ce)) sels
      in
	CG_ce(ce,env,sp,cc,
	      (case con_kind of
		 ClosExp.ENUM _ => (fn C => C)
	       | ClosExp.UNBOXED _ => (fn C => UbTagCon :: C)
	       | ClosExp.BOXED _ => fn C => select(0,C)) 
		(binary_search(sels',
			       default,
			       fn (lab,i,C) => IfNotEqJmpRelImmed(lab,i) :: C,
			       fn (lab,i,C) => IfLessThanJmpRelImmed(lab,i) :: C,
			       fn (lab,i,C) => IfGreaterThanJmpRelImmed(lab,i) :: C,
			       fn (ce,C) => CG_ce(ce,env,sp,cc,C),
			       acc)))
      end
      | CG_ce(ClosExp.SWITCH_E sw,env,sp,cc,acc) = die "SWITCH_E is unfolded in ClosExp"
      | CG_ce(ClosExp.CON0{con,con_kind,aux_regions,alloc},env,sp,cc,acc) =
      let 
	fun reset_regions C =
	  foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,env,sp,cc,C)) C aux_regions
      in
	case con_kind of
	  ClosExp.ENUM i => 
	    let 
	      val tag = 
		if !BI.tag_values orelse (*hack to treat booleans tagged*)
		  Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 
		  2*i+1 
		else i
	    in
	      immedInt (tag, acc)
	    end
	| ClosExp.UNBOXED i => 
	    let val tag = 4*i+3 
	    in reset_regions(immedInt (tag, acc))
	    end
	| ClosExp.BOXED i => 
	    let val tag = Word32.toInt(BI.tag_con0(false,i))
	    in reset_regions(ImmedIntPush tag :: alloc_block(alloc,1,env,sp+1,cc,acc))
	    end
      end
      | CG_ce(ClosExp.CON1{con,con_kind,alloc,arg},env,sp,cc,acc) =
	 (case con_kind of
	    ClosExp.UNBOXED 0 => CG_ce(arg,env,sp,cc,acc)
	  | ClosExp.UNBOXED i => 
	      (case i of
		 1 => CG_ce(arg,env,sp,cc,SetBit31 :: acc)
	       | 2 => CG_ce(arg,env,sp,cc,SetBit30 :: acc)
	       | _ => die "CG_ce: UNBOXED CON1 with i > 2")
	  | ClosExp.BOXED i => 
		 let
		   val tag = Word32.toInt(BI.tag_con1(false,i))
		 in
		   ImmedIntPush tag :: 
		   CG_ce(arg,env,sp+1,cc,             (*mael fix: sp -> sp+1 *)
			 push (alloc_block(alloc,2,env,sp+2,cc,acc)))
		 end
	  | _ => die "CG_ce: CON1.con not unary in env.")
      | CG_ce(ClosExp.DECON{con,con_kind,con_exp},env,sp,cc,acc) =
	    (case con_kind of
	       ClosExp.UNBOXED 0 => CG_ce(con_exp,env,sp,cc,acc)
	     | ClosExp.UNBOXED _ => CG_ce(con_exp,env,sp,cc,ClearBit30And31 :: acc)
	     | ClosExp.BOXED _ => CG_ce(con_exp,env,sp,cc, select(1,acc))
	     | _ => die "CG_ce: DECON used with con_kind ENUM")
      | CG_ce(ClosExp.DEREF ce,env,sp,cc,acc) = CG_ce(ce,env,sp,cc, select(0,acc))
      | CG_ce(ClosExp.REF(sma,ce),env,sp,cc,acc) = 
	       CG_ce(ce,env,sp,cc,
		     push (alloc_block(sma,1,env,sp+1,cc,acc)))
      | CG_ce(ClosExp.ASSIGN(sma,ce1,ce2),env,sp,cc,acc) = 
	       CG_ce(ce1,env,sp,cc, 
		     push (CG_ce(ce2,env,sp+1,cc,Store(0) :: acc)))
      | CG_ce(ClosExp.RESET_REGIONS{force=false,regions_for_resetting},env,sp,cc,acc) =
	  foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,env,sp,cc,C)) acc regions_for_resetting
      | CG_ce(ClosExp.RESET_REGIONS{force=true,regions_for_resetting},env,sp,cc,acc) =
	  foldr (fn (alloc,C) => force_reset_aux_region(alloc,env,sp,cc,C)) acc regions_for_resetting
      | CG_ce(ClosExp.CCALL{name,rhos_for_result,args},env,sp,cc,acc) =
	  let
	    (* Note that the prim names are defined in BackendInfo! *)
	    fun prim_name_to_KAM name =
	      (case name
		 of "__equal_int"         => PrimEquali
	       | "__minus_int"            => PrimSubi
	       | "__plus_int"             => PrimAddi
	       | "__mul_int"              => PrimMuli
	       | "__neg_int"              => PrimNegi
	       | "__abs_int"              => PrimAbsi
	       | "__less_int"             => PrimLessThan
	       | "__lesseq_int"           => PrimLessEqual
	       | "__greater_int"          => PrimGreaterThan
	       | "__greatereq_int"        => PrimGreaterEqual
	       | "__plus_float"           => PrimAddf
	       | "__minus_float"          => PrimSubf
	       | "__mul_float"            => PrimMulf
	       | "__div_float"            => PrimDivf
	       | "__neg_float"            => PrimNegf
	       | "__abs_float"            => PrimAbsf
	       | "__less_float"           => PrimLessThanFloat
	       | "__lesseq_float"         => PrimLessEqualFloat
	       | "__greater_float"        => PrimGreaterThanFloat
	       | "__greatereq_float"      => PrimGreaterEqualFloat
		       
	       | "less_word__"            => PrimLessThanUnsigned
	       | "greater_word__"         => PrimGreaterThanUnsigned
	       | "lesseq_word__"          => PrimLessEqualUnsigned
	       | "greatereq_word__"       => PrimGreaterEqualUnsigned
		       
	       | "plus_word8__"           => PrimAddw8
	       | "minus_word8__"          => PrimSubw8
	       | "mul_word8__"            => PrimMulw8
		       
	       | "and__"                  => PrimAndi
	       | "or__"                   => PrimOri
	       | "xor__"                  => PrimXori
	       | "shift_left__"           => PrimShiftLefti
	       | "shift_right_signed__"   => PrimShiftRightSignedi
	       | "shift_right_unsigned__" => PrimShiftRightUnsignedi
		       
	       | "plus_word__"            => PrimAddw
	       | "minus_word__"           => PrimSubw
	       | "mul_word__"             => PrimMulw
		       
	       | "__fresh_exname"         => PrimFreshExname
	       | _ => die ("PRIM(" ^ name ^ ") not implemented"))

	  in
	    if BI.is_prim name then 
	      (* rhos_for_result comes after args so that the accumulator holds the *)
	      (* pointer to allocated memory. *)
	      comp_ces(args @ rhos_for_result,env,sp,cc,
		       prim_name_to_KAM name :: acc)
	    else
	      let
	      (* rhos_for_result comes before args, because that is what the C *)
	      (* functions expects. *)
		val all_args = rhos_for_result @ args
		val i = BuiltInCFunctions.name_to_built_in_C_function_index name
	      in
		if i >= 0 then
		  comp_ces(all_args,env,sp,cc,
			   Ccall(i, List.length all_args) :: acc)
		else die ("Couldn't generate code for a C-call to " ^ name)
	      end
	  end
      | CG_ce(ClosExp.FRAME{declared_lvars,declared_excons},env,sp,cc,acc) = 
	  comment ("FRAME - this is a nop", acc)

    and force_reset_aux_region(sma,env,sp,cc,acc) = 
      let
	fun comp_ce(ce,acc) = CG_ce(ce,env,sp,cc,acc)
      in
	case sma 
	  of ClosExp.ATBOT_LI(ce,pp) => comp_ce(ce, ResetRegion :: acc)
	| ClosExp.SAT_FI(ce,pp) => comp_ce(ce, ResetRegion :: acc)
	| ClosExp.SAT_FF(ce,pp) => comp_ce(ce, ResetRegionIfInf :: acc)
	| _ => acc
      end

    and maybe_reset_aux_region(sma,env,sp,cc,acc) = 
      let
	fun comp_ce(ce,acc) = CG_ce(ce,env,sp,cc,acc)
      in
	case sma 
	  of ClosExp.ATBOT_LI(ce,pp) => comp_ce(ce, ResetRegion :: acc)
	| ClosExp.SAT_FI(ce,pp) => comp_ce(ce, MaybeResetRegion :: acc)
	| ClosExp.SAT_FF(ce,pp) => comp_ce(ce, MaybeResetRegion :: acc)
	| _ => acc
      end

    and set_sm(sma,env,sp,cc,acc) =
      let
	fun comp_ce(ce,acc) = CG_ce(ce,env,sp,cc,acc)
      in
	case sma of
	  ClosExp.ATTOP_LI(ce,pp) => comp_ce(ce,acc)
	| ClosExp.ATTOP_LF(ce,pp) => comp_ce(ce,acc)
	| ClosExp.ATTOP_FF(ce,pp) => comp_ce(ce,ClearAtbotBit :: acc)
	| ClosExp.ATTOP_FI(ce,pp) => comp_ce(ce,ClearAtbotBit :: acc)
	| ClosExp.SAT_FI(ce,pp)   => comp_ce(ce,acc)
	| ClosExp.SAT_FF(ce,pp)   => comp_ce(ce,acc)
	| ClosExp.ATBOT_LI(ce,pp) => comp_ce(ce,SetAtbotBit :: acc)
	| ClosExp.ATBOT_LF(ce,pp) => comp_ce(ce,acc)
	| ClosExp.IGNORE => die "CodeGenKAM.set_sm: sma = Ignore"
      end

    and alloc_block(sma,n,env,sp,cc,acc) =
      let
	fun comp_ce(ce,acc) = CG_ce(ce,env,sp,cc,acc)
      in
	case sma of
	  ClosExp.ATTOP_LI(ce,pp) => comp_ce(ce,BlockAlloc(n) :: acc)
	| ClosExp.ATTOP_LF(ce,pp) => comp_ce(ce,Block(n) :: acc)
	| ClosExp.ATTOP_FF(ce,pp) => comp_ce(ce,BlockAllocIfInf(n) :: acc)
	| ClosExp.ATTOP_FI(ce,pp) => comp_ce(ce,BlockAlloc(n) :: acc)
	| ClosExp.SAT_FI(ce,pp)   => comp_ce(ce,BlockAllocSatInf(n) :: acc)
	| ClosExp.SAT_FF(ce,pp)   => comp_ce(ce,BlockAllocSatIfInf(n) :: acc)
	| ClosExp.ATBOT_LI(ce,pp) => comp_ce(ce,BlockAllocAtbot(n) :: acc)
	| ClosExp.ATBOT_LF(ce,pp) => comp_ce(ce,Block(n) :: acc)
	| ClosExp.IGNORE => acc (*die "CodeGenKAM.alloc_block: sma = Ignore" 05/10-2000, Niels *)
      end

    and alloc(sma,n,env,sp,cc,acc) =
      let
	fun comp_ce(ce,acc) = CG_ce(ce,env,sp,cc,acc)
      in
	case sma of
	  ClosExp.ATTOP_LI(ce,pp) => comp_ce(ce,Alloc(n) :: acc)
	| ClosExp.ATTOP_LF(ce,pp) => comp_ce(ce,acc)
	| ClosExp.ATTOP_FF(ce,pp) => comp_ce(ce,AllocIfInf(n) :: acc)
	| ClosExp.ATTOP_FI(ce,pp) => comp_ce(ce,Alloc(n) :: acc)
	| ClosExp.SAT_FI(ce,pp)   => comp_ce(ce,AllocSatInf(n) :: acc)
	| ClosExp.SAT_FF(ce,pp)   => comp_ce(ce,AllocSatIfInf(n) :: acc)
	| ClosExp.ATBOT_LI(ce,pp) => comp_ce(ce,AllocAtbot(n) :: acc)
	| ClosExp.ATBOT_LF(ce,pp) => comp_ce(ce,acc)
	| ClosExp.IGNORE => die "CodeGenKAM.alloc: sma = Ignore"
      end

    and comp_ces_to_block ([],n,env,sp,cc,alloc,acc) = alloc_block(alloc,n,env,sp,cc,acc)
      | comp_ces_to_block (ce::ces,n,env,sp,cc,alloc,acc) = 
      CG_ce(ce,env,sp,cc, push (comp_ces_to_block(ces,n+1,env,sp+1,cc,alloc,acc)))

    and comp_ces ([],env,sp,cc,acc) = acc
      | comp_ces ([ce],env,sp,cc,acc) = CG_ce(ce,env,sp,cc,acc)
      | comp_ces (ce::ces,env,sp,cc,acc) = 
      CG_ce(ce,env,sp,cc, push (comp_ces(ces,env,sp+1,cc,acc)))

    local
      fun mk_fun f_fun (lab,cc,ce) =
	(* Region arguments start at offset 0 *)
	(* cc.res contains one pseudo lvar for each value returned, see LiftTrip in ClosExp *)
	(* I don't know what a ``pseudo lvar'' is?? ME 2000-11-04 *)
	let
	  val decomp_cc = CallConv.decompose_cc cc
	  fun add_lvar (lv,(offset,env)) = (offset+1,declareLvar(lv,STACK(offset),env))
	  fun add_clos_opt (NONE,env) = (env, Return)
	    | add_clos_opt (SOME clos_lv, env) = (declareLvar(clos_lv,ENV_REG,env), Return)
(*
	  val _ = print "Regvars formals:\n"
	  val _ = app (fn lv => print (Lvars.pr_lvar lv ^ ", ")) (#reg_args(decomp_cc))
	  val _ = print "\n"
*)
          val (offset,env) = List.foldl add_lvar (0,initialEnv) (#reg_args(decomp_cc))
	  val (offset,env) = List.foldl add_lvar (offset,env) (#args(decomp_cc))
	  val (env,return_inst) = add_clos_opt(#clos(decomp_cc),env)

	  val returns = Int.max(1, List.length (#res(decomp_cc)))  (* the return_inst instruction assumes 
								    * that there is at least one result 
								    * to return *)
	in
	  f_fun(lab,CG_ce(ce,env,offset,cc,[return_inst(offset,returns)]))
	end
    in
      fun CG_top_decl(ClosExp.FUN(lab,cc,ce)) = mk_fun FUN (lab,cc,ce)
	| CG_top_decl(ClosExp.FN(lab,cc,ce)) = mk_fun FN (lab,cc,ce)
    end
  in
    fun CG_clos_prg funcs =
      List.foldr (fn (func,acc) => CG_top_decl func :: acc) [] funcs
  end

  fun pp_labels s ls = 
    let fun loop nil = ()
	  | loop (l::ls) = (print (Labels.pr_label l); print ","; loop ls)
    in print (s ^ " = ["); loop ls; print "]\n"
    end

  (******************************)
  (* Code Generation -- KAM     *)
  (******************************)
  fun CG {main_lab_opt:label option,
	  code=clos_prg:ClosPrg,
	  imports=(imports_code:label list, imports_data:label list),
	  exports=(exports_code:label list, exports_data:label list)} =
    let
      val _ = chat "[CodeGeneration for the KAM..."


      val exports_code = case main_lab_opt
			   of SOME l => l :: exports_code
			    | NONE => exports_code
(*mael
      val _ = pp_labels "data labels" exports_data
      val _ = pp_labels "code labels" exports_code
*)
      val _ = setExportLabs exports_data
      val asm_prg = {top_decls=CG_clos_prg clos_prg,
		     main_lab_opt=main_lab_opt,
		     imports_code=imports_code,
		     imports_data=imports_data,
		     exports_code=exports_code,
		     exports_data=exports_data}
      val _ = 
	if Flags.is_on "print_kam_program" then
	  display("\nReport: AFTER CodeGeneration for the KAM:", 
		  layout_AsmPrg asm_prg)
	else
	  ()
      val _ = chat "]\n"
    in
      asm_prg
    end

    (* ------------------------------------------------------------------------------ *)
    (*              Generate Link Code for Incremental Compilation                    *)
    (* ------------------------------------------------------------------------------ *)
    fun generate_link_code (linkinfos:label list) = {top_decls=[], (* not done 05/10-2000, Niels *)
						     main_lab_opt=NONE,
						     imports_code=nil,
						     imports_data=nil,
						     exports_code=nil,
						     exports_data=nil}
end