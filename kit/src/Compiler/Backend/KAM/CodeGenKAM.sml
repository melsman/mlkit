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
                     sharing type BI.lvar = Lvars.lvar
		   structure Lvarset: LVARSET
		     sharing type Lvarset.lvar = Lvars.lvar
		   structure Kam: KAM
                     sharing type Kam.label = Labels.label
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
		   structure Crash : CRASH) : sig end =

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
    [("print_KAM_program", "print KAM program", ref false)]

  val print_KAM_flag = Flags.lookup_flag_entry "print_KAM_program"

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

  fun lookupRhoOpt ({RhoEnv,...} : env) place = RegvarFinMap.lookup RhoEnv place

  (* --------------------------------------------------------------------- *)
  (*                          Pretty printing                              *)
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

  (***********************)
  (* Code Generation     *)
  (***********************)
  local
    fun access_lv(lv,env,sp) =
      case lookupVar env lv of
	STACK i => SelectStack(0-sp+i)
      | ENV i => SelectEnv(i)
      |	REG_I i => StackAddrInfBit(0-sp+i)
      | REG_F i => StackAddr(0-sp+i)
      | ENV_REG => EnvToAcc

    fun access_rho(rho,env,sp) =
      case lookupRho env rho of
	STACK i => SelectStack(0-sp+i)
      | ENV i => SelectEnv(i)
      |	REG_I i => StackAddrInfBit(0-sp+i)
      | REG_F i => StackAddr(0-sp+i)
      | ENV_REG => EnvToAcc

    fun num_args_cc(cc) =
      let
	val decomp_cc = CallConv.decompose_cc cc
      in
	List.length (#reg_args(decomp_cc)) +
	List.length (#args(decomp_cc))
      end

    fun CG_ce_sw(ClosExp.SWITCH(ce,sels,default),f_L,f_sel) = Comment "Switch not implemented"

    fun CG_ce(ClosExp.VAR lv,env,sp,cc,acc)             = access_lv(lv,env,sp) :: acc
      | CG_ce(ClosExp.RVAR place,env,sp,cc,acc)         = access_rho(place,env,sp) :: acc
      | CG_ce(ClosExp.DROPPED_RVAR place,env,sp,cc,acc) = die "DROPPED_RVAR not implemented"
      | CG_ce(ClosExp.FETCH lab,env,sp,cc,acc)          = die "FETCH not implemented"
      | CG_ce(ClosExp.STORE(ce,lab),env,sp,cc,acc)      = die "STORE not implemented"
      | CG_ce(ClosExp.INTEGER i,env,sp,cc,acc)          = ImmedInt i :: acc
      | CG_ce(ClosExp.STRING s,env,sp,cc,acc)           = ImmedString s :: acc
      | CG_ce(ClosExp.REAL s,env,sp,cc,acc)             = ImmedReal s :: acc
      | CG_ce(ClosExp.PASS_PTR_TO_MEM(sma,i),env,sp,cc,acc) = die "PASS_PTR_TO_MEM not implemented"
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
      | CG_ce(ClosExp.SELECT(i,ce),env,sp,cc,acc) = CG_ce(ce,env,sp,cc,Select(i)::acc)
      | CG_ce(ClosExp.FNJMP{opr,args,clos=NONE,free=[]},env,sp,cc,acc) = 
      CG_ce(opr,env,sp,cc,
	    Push :: (comp_ces(args,env,sp+1,cc, ApplyFnJmp(List.length (#args(CallConv.decompose_cc cc)),List.length args) :: acc)))
      | CG_ce(ClosExp.FNJMP{opr,args,clos,free},env,sp,cc,acc) = die "FNJMP: either clos or free are non empty."
      | CG_ce(ClosExp.FNCALL{opr,args,clos=NONE,free=[]},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(opr,env,sp+1,cc,
	      Push ::
	      comp_ces(args,env,sp+2,cc,ApplyFnCall(List.length args) :: Label(return_lbl) :: acc))
      end
      | CG_ce(ClosExp.FNCALL{opr,args,clos,free},env,sp,cc,acc) = die "FNCALL: either clos or free are non empty."      
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=NONE,free=[]},env,sp,cc,acc) =
      comp_ces(args,env,sp,cc,
	       ApplyFunJmpNoClos(opr,List.length (#args(CallConv.decompose_cc cc)) - (List.length reg_args),List.length args) :: acc)
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce,free=[]},env,sp,cc,acc) =
      CG_ce(clos_ce,env,sp,cc,
	    Push ::
	    comp_ces(args,env,sp+1,cc,
		     ApplyFunJmp(opr,List.length (#args(CallConv.decompose_cc cc)) - (List.length reg_args),List.length args) :: acc))
      | CG_ce(ClosExp.JMP{opr,args,reg_vec,reg_args,clos,free},env,sp,cc,acc) = die "JMP either reg_vec or free are non empty."
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos=NONE,free=[]},env,sp,cc,acc) =
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	comp_ces(reg_args,env,sp+1,cc,
		 comp_ces(args,env,sp+1+(List.length reg_args),cc,
			  ApplyFunCallNoClos(opr,List.length args + List.length reg_args) :: 
			  Label(return_lbl) :: acc))
      end
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce,free=[]},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(clos_ce,env,sp+1,cc, Push ::
	      comp_ces(reg_args,env,sp+2,cc,
		       comp_ces(args,env,sp+2+(List.length reg_args),cc,
				ApplyFunCallNoClos(opr,List.length args + List.length reg_args) :: 
				Label(return_lbl) :: acc)))
      end
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec,reg_args,clos,free},env,sp,cc,acc) = die "FUNCALL: either reg_vec or free are non empty."
      | CG_ce(ClosExp.LETREGION{rhos,body},env,sp,cc,acc) = 
      let
	fun comp_alloc_rhos([],env,sp,cc,fn_acc) = fn_acc(env,sp)
	  | comp_alloc_rhos((place,PhysSizeInf.INF)::rs,env,sp,cc,fn_acc) = 
	  LetregionInf ::
	  comp_alloc_rhos(rs,declareRho(place,REG_I(sp),env),sp+(BI.size_of_reg_desc()),cc,fn_acc)
	  | comp_alloc_rhos((place,PhysSizeInf.WORDS i)::rs,env,sp,cc,fn_acc) = 
	  LetregionFin(i) ::
	  comp_alloc_rhos(rs,declareRho(place,REG_F(sp),env),sp+i,cc,fn_acc)
	fun comp_dealloc_rho(place,PhysSizeInf.INF) =  EndregionInf
	  | comp_dealloc_rho(place,PhysSizeInf.WORDS i) = Pop(i)
      in
	comp_alloc_rhos(rhos,env,sp,cc,
			fn (env,sp) => CG_ce(body,env,sp,cc,
					     (List.foldl (fn (rho,acc) => 
							  comp_dealloc_rho rho :: acc) acc rhos)))
      end
      | CG_ce(ClosExp.LET{pat=[],bind,scope},env,sp,cc,acc) = die "LET with pat = []."
      | CG_ce(ClosExp.LET{pat,bind,scope},env,sp,cc,acc) = 
      let
	val n = List.length pat
	fun declareLvars([],sp,env) = env
	  | declareLvars(lv::lvs,sp,env) = declareLvars(lvs,sp+1,declareLvar(lv,STACK(sp),env))
      in
	CG_ce(bind,env,sp,cc, Push ::
	      CG_ce(scope,declareLvars(pat,sp,env),sp+n,cc,Pop(n) :: acc))
      end
      | CG_ce(ClosExp.RAISE ce,env,sp,cc,acc) = CG_ce(ce,env,sp,cc,Raise::acc)
      | CG_ce(ClosExp.HANDLE(ce1,ce2),env,sp,cc,acc) =
      (* An exception handler on the stack contains the following fields: *)
      (* sp[offset] = pointer to handle closure.                          *)
      (* sp[offset+1] = label for handl_return code.                      *)
      (* sp[offset+2] = pointer to previous exception handler used when updating expPtr. *)
      (* Note that we call deallocate_regions_until to the address above the exception handler, *)
      (* when an exception is raised.  *)
      let
	val return_lbl = Labels.new_named "return_handle"
      in
	CG_ce(ce2,env,sp,cc, Push :: PushLbl return_lbl :: PushExnPtr ::	 
	      CG_ce(ce1,env,sp+3,cc, PopExnPtr :: Pop(2) :: Label return_lbl :: acc))
      end
      | CG_ce(ClosExp.SWITCH_I sw,env,sp,cc,acc) = die "SWITCH_I not implemented"
      | CG_ce(ClosExp.SWITCH_S sw,env,sp,cc,acc) = die "SWITCH_S not implemented"
      | CG_ce(ClosExp.SWITCH_C sw,env,sp,cc,acc) = die "SWITCH_C not implemented"
      | CG_ce(ClosExp.SWITCH_E sw,env,sp,cc,acc) = die "SWITCH_E not implemented"
      | CG_ce(ClosExp.CON0{con,con_kind,aux_regions,alloc},env,sp,cc,acc) =
      (case con_kind of
	 ClosExp.ENUM i => 
	   let 
	     val tag = 
	       if !BI.tag_values orelse (*hack to treat booleans tagged*)
		 Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 
		 2*i+1 
	       else i
	   in
	     ImmedInt tag :: acc 
	   end
       | ClosExp.UNBOXED i => 
	   let
	     val tag = 4*i+3 
	     fun reset_regions C =
	       foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,env,sp,cc,C)) C aux_regions
	   in
	     reset_regions(ImmedInt tag :: acc)
	   end
       | ClosExp.BOXED i => 
	   let 
	     val tag = Word32.toInt(BI.tag_con0(false,i))
	     fun reset_regions C =
	       List.foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,env,sp,cc,C)) C aux_regions
	   in  
	     reset_regions(ImmedInt tag :: alloc_block(alloc,1,env,sp,cc,acc))
	   end)
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
		   CG_ce(arg,env,sp,cc,Push :: ImmedInt tag :: 
			 alloc_block(alloc,2,env,sp+2,cc,acc))
		 end
	  | _ => die "CG_ce: CON1.con not unary in env.")
      | CG_ce(ClosExp.DECON{con,con_kind,con_exp},env,sp,cc,acc) =
	    (case con_kind of
	       ClosExp.UNBOXED 0 => CG_ce(con_exp,env,sp,cc,acc)
	     | ClosExp.UNBOXED _ => CG_ce(con_exp,env,sp,cc,ClearBit30And31 :: acc)
	     | ClosExp.BOXED _ => CG_ce(con_exp,env,sp,cc, Select(1) :: acc)
	     | _ => die "CG_ce: DECON used with con_kind ENUM")
      | CG_ce(ClosExp.DEREF ce,env,sp,cc,acc) = die "DEREF not implemented"
      | CG_ce(ClosExp.REF(sma,ce),env,sp,cc,acc) = die "REF not implemented"
      | CG_ce(ClosExp.ASSIGN(sma,ce1,ce2),env,sp,cc,acc) = die "ASSIGN not implemented"
      | CG_ce(ClosExp.RESET_REGIONS{force,regions_for_resetting},env,sp,cc,acc) = die "RESET_REGIONS not implemented"
      | CG_ce(ClosExp.CCALL{name,rhos_for_result,args},env,sp,cc,acc) = die "CCALL not implemented"
      | CG_ce(ClosExp.FRAME{declared_lvars,declared_excons},env,sp,cc,acc) = die "FRAME not implemented"

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
	| ClosExp.ATBOT_LF(ce,pp) => comp_ce(ce,acc)
	| ClosExp.IGNORE => die "CodeGenKAM.alloc_block: sma = Ignore"
      end

    and comp_ces_to_block ([],n,env,sp,cc,alloc,acc) = alloc_block(alloc,n,env,sp,cc,acc)
      | comp_ces_to_block (ce::ces,n,env,sp,cc,alloc,acc) = CG_ce(ce,env,sp,cc,Push::comp_ces_to_block(ces,n+1,env,sp+1,cc,alloc,acc))

    and comp_ces ([],env,sp,cc,acc) = die "CG_ce.comp_ces: empty ces list"
      | comp_ces ([ce],env,sp,cc,acc) = CG_ce(ce,env,sp,cc,acc)
      | comp_ces (ce::ces,env,sp,cc,acc) = CG_ce(ce,env,sp,cc,Push::comp_ces(ces,env,sp+1,cc,acc))

    local
      fun mk_fun f_fun (lab,cc,ce) =
	(* Region arguments start a offset 0 *)
	(* cc.res contains one pseudo lvar for each value returned, see LiftTrip in ClosExp *)
	let
	  val decomp_cc = CallConv.decompose_cc cc
	  fun add_lvar (lv,(offset,env)) = (offset+1,declareLvar(lv,STACK(offset),env))
	  val (offset,env) = List.foldl add_lvar (List.foldl add_lvar 
						  (0,initialEnv) (#reg_args(decomp_cc))) (#args(decomp_cc))
	in
	  f_fun(lab,CG_ce(ce,env,offset,cc,[Return(offset,List.length (#res(decomp_cc)))]))
	end
    in
      fun CG_top_decl(ClosExp.FUN(lab,cc,ce)) = mk_fun FUN (lab,cc,ce)
	| CG_top_decl(ClosExp.FN(lab,cc,ce)) = mk_fun FN (lab,cc,ce)
    end
  in
    fun CG_clos_prg funcs =
      List.foldr (fn (func,acc) => CG_top_decl func :: acc) [] funcs
  end

  (******************************)
  (* Code Generation -- KAM     *)
  (******************************)
  fun CG {main_lab:label,
	  code=clos_prg:ClosPrg,
	  imports:label list * label list,
	  exports:label list * label list} =
    let
      val _ = chat "[CodeGeneration for the KAM..."
      val asm_prg = {top_decls=CG_clos_prg clos_prg,
		     init_code=[],
		     exit_code=[],
		     static_data=[]}
      val _ = 
	if Flags.is_on "print_KAM_program" then
	  display("\nReport: AFTER CodeGeneration for the KAM:", 
		  layout_AsmPrg asm_prg)
	else
	  ()
      val _ = chat "]\n"
    in
      asm_prg
    end
end;