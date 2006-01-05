functor CodeGenKAM(structure CallConv: CALL_CONV
                     where type lvar = Lvars.lvar
		   structure ClosExp: CLOS_EXP
 	             where type con = Con.con
		     where type excon = Excon.excon
                     where type lvar = Lvars.lvar
                     where type place = Effect.place
                     where type label = AddressLabels.label
		     where type phsize = PhysSizeInf.phsize
		   sharing type CallConv.cc = ClosExp.cc
		   structure BI : BACKEND_INFO
                   structure JumpTables : JUMP_TABLES
		       ) : CODE_GEN_KAM (* : sig end *) =

struct
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  structure LvarFinMap = Lvars.Map
  structure RegvarFinMap = EffVarEnv
  structure BuiltInCFunctions = BuiltInCFunctionsKAM
  structure Opcodes = OpcodesKAM

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

  val _ = Flags.add_bool_entry 
    {long="print_kam_program", short=NONE, item=ref false, neg=false,
     menu=["Printing of intermediate forms", "print KAM program"],
     desc="Print Kit Abstract Machine code."}
				  

  val _ = Flags.add_bool_entry 
    {long="comments_in_kam_code", short=NONE, item=ref false, neg=false,
     menu=["Printing of intermediate forms", "comments in KAM code"],
     desc=""}

  val comments_in_kam_code = Flags.lookup_flag_entry "comments_in_kam_code"
  val jump_tables = true

  (*************)
  (* Utilities *)
  (*************)
  fun zip ([],[]) = []
    | zip ((x::xs),(y::ys)) = (x,y) :: (zip (xs,ys))
    | zip _ = die "zip: Cannot zip two lists."

(*
  fun is_region_real place =
    (case Effect.get_place_ty place
       of NONE => die "LETREGION.alloc.regvar has no runtype."
	| SOME Effect.REAL_RT => true
	| SOME Effect.STRING_RT => false
	| SOME _ => false)
*)

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



  (* Convert ~n to -n; works for all int32 values including Int32.minInt *)
  fun intToStr (i : Int32.int) : string = 
    let fun tr s = case explode s
		     of #"~"::rest => implode (#"-"::rest)
		      | _ => s
    in tr (Int32.toString i)
    end

  fun wordToStr (w : Word32.word) : string =
    "0x" ^ Word32.toString w

    fun maybeTagInt {value: Int32.int, precision:int} : Int32.int =
      case precision
	of 31 => ((2 * value + 1)         (* use tagged-unboxed representation *)
		  handle Overflow => die "maybeTagInt.Overflow")
	 | 32 => value                    (* use untagged representation - maybe boxed *)
	 | _ => die "maybeTagInt"

    fun maybeTagWord {value: Word32.word, precision:int} : Word32.word =
      case precision
	of 31 =>                            (* use tagged representation *)
	  let val w = 0w2 * value + 0w1   
	  in if w < value then die "maybeTagWord.Overflow"
	     else w
	  end
	 | 32 => value                      (* use untagged representation - maybe boxed *)
	 | _ => die "maybeTagWord"

    (* formatting of immediate integer and word values *)
    fun fmtInt a : string = intToStr(maybeTagInt a)
    fun fmtWord a : string = wordToStr(maybeTagWord a)


  (* ----------------------------------------------------------------------------
   * Dead code elimination; during code generation we eliminate code that is non-
   * reachable by eliminating code from the continuation---down to a label---when 
   * a jump, a return, or a raise is generated.
   * ---------------------------------------------------------------------------- *)

  fun dead_code_elim C =
    case C
      of Label _ :: _ => C
       | DotLabel _ :: _ => C
       | (i as StoreData _) :: C => i :: dead_code_elim C (* necessary for linking; problem is 
							   * Raise instruct *)
       | (i as FetchData _) :: C => i :: dead_code_elim C (* necessary for linking; problem is 
							   * Raise instruct *)
       | _ :: rest => dead_code_elim rest
       | nil => C

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
       | ImmedInt 1 :: PrimSubi :: acc => PrimSubi1 :: acc
       | ImmedInt 2 :: PrimSubi :: acc => PrimSubi2 :: acc
       | ImmedInt 1 :: PrimAddi :: acc => PrimAddi1 :: acc
       | ImmedInt 2 :: PrimAddi :: acc => PrimAddi2 :: acc
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

  fun immedInt (i : Int32.int, acc) =
    case acc
      of Push :: acc => ImmedIntPush i :: acc
       | _ => ImmedInt i :: acc

  fun immedWord (w : Word32.word, acc) =
    let val i = Int32.fromLarge (Word32.toLargeIntX w)
    in case acc
	 of Push :: acc => ImmedIntPush i :: acc
	  | _ => ImmedInt i :: acc
    end

  fun immedIntMaybeTag (a, acc) = immedInt (maybeTagInt a, acc)
  fun immedWordMaybeTag (a, acc) = immedWord (maybeTagWord a, acc)


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

    fun comment(str,C) = 
      if !comments_in_kam_code then Comment str :: C
      else C

    fun comment_fn (f, C) =
      if !comments_in_kam_code then Comment (f()) :: C
      else C

    (* Compile Switch Statements *)
    local
      fun new_label str = Labels.new_named str
      fun label(lab,C) = Label lab :: C
      fun jmp(lab,C) = JmpRel lab :: dead_code_elim C
      fun inline_cont C =
	case C 
	  of (i as JmpRel lab) :: _ => SOME (fn C => i :: C)
	   | (i as Return _) :: _ => SOME (fn C => i :: C)
	   | (i1 as Pop _) :: (i2 as Return _) :: _ => SOME (fn C => i1 :: i2 :: C)
	   | _ => NONE
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
				       inline_cont,
				       C)
	  
      fun binary_search(sels,
			default,
			if_not_equal_go_lab_sel: label * Int32.int * KamInst list -> KamInst list,
			if_less_than_go_lab_sel: label * Int32.int * KamInst list -> KamInst list,
			if_greater_than_go_lab_sel: label * Int32.int * KamInst list -> KamInst list,
			compile_insts: ClosExp.ClosExp * KamInst list -> KamInst list,
			precision,
			toInt,
			C) =
	let
 	  fun maybe_tag (i : Int32.int) : Int32.int = 
	    if precision < 32 then 2*i+1
	    else i
	  val sels = map (fn (i,e) => (maybe_tag(toInt i), e)) sels
	in
	  if jump_tables then
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
					 fn (sel1,sel2) => Int32.abs(sel1-sel2),
					 fn (lab,sel,length,C) => JmpVector(lab,sel,length)::C,
					 fn (lab,C) => DotLabel(lab) :: C, (* add_label_to_jump_tab  *)
					 eq_lab,
					 inline_cont,
					 C)
	  else
	    linear_search(sels,
			  default,
			  if_not_equal_go_lab_sel,
			  compile_insts,
			  C)
	end
    end

    fun toCString acc = PrimAddi2 :: PrimAddi2 :: acc
    fun untagBool acc = Primi31Toi :: acc
    fun tagBool acc = PrimiToi31 :: acc
    fun cconvert_arg ft acc =
      case ft
	of ClosExp.CharArray => toCString acc
	 | ClosExp.Bool => untagBool acc
	 | ClosExp.Int => acc
	 | ClosExp.ForeignPtr => acc
	 | ClosExp.Unit => acc

    fun cconvert_res ft acc =
      case ft
	of ClosExp.CharArray => die "cconvert_res.CharArray not allowed in C result"
	 | ClosExp.Bool => tagBool acc
	 | ClosExp.Int => acc
	 | ClosExp.ForeignPtr => acc
	 | ClosExp.Unit => acc

    fun name_to_built_in_C_function_index name = 
      if !Flags.SMLserver 
      then BuiltInCFunctions.name_to_built_in_C_function_index_apsml name
      else BuiltInCFunctions.name_to_built_in_C_function_index name

    fun CG_ce(ClosExp.VAR lv,env,sp,cc,acc)             = access_lv(lv,env,sp,acc)
      | CG_ce(ClosExp.RVAR place,env,sp,cc,acc)         = access_rho(place,env,sp,acc)
      | CG_ce(ClosExp.DROPPED_RVAR place,env,sp,cc,acc) = acc (* die "DROPPED_RVAR not implemented" *)
      | CG_ce(ClosExp.FETCH lab,env,sp,cc,acc)          = FetchData lab :: acc
      | CG_ce(ClosExp.STORE(ce,lab),env,sp,cc,acc)      = CG_ce(ce,env,sp,cc, storeData lab :: acc)
      | CG_ce(ClosExp.INTEGER i,env,sp,cc,acc)          = immedIntMaybeTag (i, acc)
      | CG_ce(ClosExp.WORD w,env,sp,cc,acc)             = immedWordMaybeTag (w, acc)
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
      | CG_ce(ClosExp.RECORD{elems,alloc,tag,maybeuntag},env,sp,cc,acc) = comp_ces_to_block(elems,0,env,sp,cc,alloc,acc)
      | CG_ce(ClosExp.SELECT(i,ce as ClosExp.VAR lv),env,sp,cc,acc) = 
      (* This may be a SelectEnv? *)
      if Lvars.eq(lv,Lvars.env_lvar) then
	selectEnv(i, Lvars.pr_lvar lv,acc)
      else
	CG_ce(ce,env,sp,cc, select(i,acc))
      | CG_ce(ClosExp.SELECT(i,ce),env,sp,cc,acc) = CG_ce(ce,env,sp,cc, select(i,acc))
      | CG_ce(ClosExp.FNJMP{opr,args,clos=NONE},env,sp,cc,acc) = 
      CG_ce(opr,env,sp,cc,
	    push (comp_ces(args,env,sp+1,cc, 
			   ApplyFnJmp(List.length args, sp) :: 
			   dead_code_elim acc)))
      | CG_ce(ClosExp.FNJMP{opr,args,clos},env,sp,cc,acc) = die "FNJMP: clos is non-empty."
      | CG_ce(ClosExp.FNCALL{opr,args,clos=NONE},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(opr,env,sp+1,cc,
	      push (comp_ces(args,env,sp+2,cc,
			     ApplyFnCall(List.length args) :: Label(return_lbl) :: acc)))
      end
      | CG_ce(ClosExp.FNCALL{opr,args,clos},env,sp,cc,acc) = 
      die "FNCALL: clos is non-empty."      
(*
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=NONE},env,sp,cc,acc) =
      ImmedIntPush "0" ::        (* is it always all the region arguments that are reused? *)
      comp_ces(args,env,sp+1,cc,
	       ApplyFunJmp(opr,List.length args,sp - (List.length reg_args)) :: 
	       dead_code_elim acc)
      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce},env,sp,cc,acc) =
      CG_ce(clos_ce,env,sp,cc,
	    push (comp_ces(args,env,sp+1,cc,
			   ApplyFunJmp(opr,List.length args,sp - (List.length reg_args)) :: 
			   dead_code_elim acc)))
*)
(*      | CG_ce(ClosExp.JMP a,env,sp,cc,acc) = CG_ce(ClosExp.FUNCALL a,env,sp,cc,acc)*)

      | CG_ce(ClosExp.JMP{opr,args,reg_vec=NONE,reg_args,clos},env,sp,cc,acc) =
      let 
	val allargs = reg_args @ args
	fun push_clos NONE C = ImmedIntPush 0 :: C
	  | push_clos (SOME clos_ce) C = CG_ce(clos_ce,env,sp,cc, push C)
      in push_clos clos (comp_ces(allargs, env, sp+1, cc,
				  ApplyFunJmp(opr, List.length allargs, sp) :: 
				  dead_code_elim acc))
      end
      | CG_ce(ClosExp.JMP{opr,args,reg_vec,reg_args,clos},env,sp,cc,acc) = die "JMP reg_vec is non-empty."
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos},env,sp,cc,acc) =
      let
	val allargs = reg_args @ args
	val return_lbl = Labels.new_named "return_from_app"
	fun push_clos NONE C = ImmedIntPush 0 :: C
	  | push_clos (SOME clos_ce) C = CG_ce(clos_ce,env,sp+1,cc, push C)
      in
	PushLbl(return_lbl) ::
	push_clos clos (comp_ces(allargs,env,sp+2,cc,
				 ApplyFunCall(opr,List.length allargs) :: 
				 Label(return_lbl) :: acc))
      end
(*
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec=NONE,reg_args,clos=SOME clos_ce},env,sp,cc,acc) = 
      let
	val return_lbl = Labels.new_named "return_from_app"
      in
	PushLbl(return_lbl) ::
	CG_ce(clos_ce,env,sp+1,cc, 
	      push (comp_ces(reg_args @ args,env,sp+2,cc,
			     ApplyFunCall(opr,List.length args + List.length reg_args) :: 
			     Label(return_lbl) :: acc)))
      end
*)
      | CG_ce(ClosExp.FUNCALL{opr,args,reg_vec,reg_args,clos},env,sp,cc,acc) = die "FUNCALL: reg_vec is non-empty."
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
	comment_fn (fn () => "Letregion <" ^ (ClosExp.pr_rhos (List.map #1 rhos)) ^ ">",
	comp_alloc_rhos(rhos,env,sp,cc,
			fn (env,sp) => CG_ce(body,env,sp,cc,
					     (List.foldl (fn (rho,acc) => 
							  comp_dealloc_rho (rho,acc)) acc rhos))))
      end
      | CG_ce(ClosExp.LET{pat=[],bind,scope},env,sp,cc,acc) =
	comment ("Let _",
	CG_ce(bind,env,sp,cc,  
	  push (CG_ce(scope,env,sp+1,cc, pop(1,acc)))))
      
      | CG_ce(ClosExp.LET{pat,bind,scope},env,sp,cc,acc) = 
      let
	val n = List.length pat
	fun declareLvars([],sp,env) = env
	  | declareLvars(lv::lvs,sp,env) = declareLvars(lvs,sp+1,declareLvar(lv,STACK(sp),env))
      in
	comment_fn (fn () => "Let <" ^ (ClosExp.pr_lvars pat) ^ ">",
	CG_ce(bind,env,sp,cc, 
	      push (CG_ce(scope,declareLvars(pat,sp,env),sp+n,cc, pop(n, acc)))))
      end

      | CG_ce(ClosExp.RAISE ce,env,sp,cc,acc) = CG_ce(ce,env,sp,cc,Raise :: dead_code_elim acc)  

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

      | CG_ce(ClosExp.SWITCH_I {switch=ClosExp.SWITCH(ce,sels,default),
				precision},env,sp,cc,acc) = 
      CG_ce(ce,env,sp,cc, 
            binary_search(sels,
			  default,
			  fn (lab,i,C) => IfNotEqJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfLessThanJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfGreaterThanJmpRelImmed (lab,i) :: C,
			  fn (ce,C) => CG_ce(ce,env,sp,cc,C),
			  precision,
			  fn i => i,
			  acc))
      | CG_ce(ClosExp.SWITCH_W {switch=ClosExp.SWITCH(ce,sels,default),
				precision},env,sp,cc,acc) = 
      CG_ce(ce,env,sp,cc, 
            binary_search(sels,
			  default,
			  fn (lab,i,C) => IfNotEqJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfLessThanJmpRelImmed (lab,i) :: C,
			  fn (lab,i,C) => IfGreaterThanJmpRelImmed (lab,i) :: C,
			  fn (ce,C) => CG_ce(ce,env,sp,cc,C),
			  precision,
			  Int32.fromLarge o Word32.toLargeIntX,
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
			   ClosExp.ENUM i => (Int32.fromInt i,sel_ce)
			 | ClosExp.UNBOXED i => (Int32.fromInt i,sel_ce)
			 | ClosExp.BOXED i => (Int32.fromInt i,sel_ce)) sels
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
			       BI.defaultIntPrecision(),
			       fn i => i,
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
		if BI.tag_values() orelse (*hack to treat booleans tagged*)
		  Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 
		  2*i+1 
		else i
	    in
	      immedInt (Int32.fromInt tag, acc)
	    end
	| ClosExp.UNBOXED i => 
	    let val tag = 4*i+3 
	    in reset_regions(immedInt (Int32.fromInt tag, acc))
	    end
	| ClosExp.BOXED i => 
	    let val tag = Word32.toInt(BI.tag_con0(false,i))
	    in reset_regions(ImmedIntPush (Int32.fromInt tag) :: alloc_block(alloc,1,env,sp+1,cc,acc))
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
		   ImmedIntPush (Int32.fromInt tag) :: 
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
      | CG_ce(ClosExp.DROP ce,env,sp,cc,acc) = CG_ce(ce,env,sp,cc,acc)  (* dropping type *)
      | CG_ce(ClosExp.RESET_REGIONS{force=false,regions_for_resetting},env,sp,cc,acc) =
	  foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,env,sp,cc,C)) acc regions_for_resetting
      | CG_ce(ClosExp.RESET_REGIONS{force=true,regions_for_resetting},env,sp,cc,acc) =
	  foldr (fn (alloc,C) => force_reset_aux_region(alloc,env,sp,cc,C)) acc regions_for_resetting
      | CG_ce(ClosExp.CCALL{name,rhos_for_result,args},env,sp,cc,acc) =
	  let
	    fun not_impl n = die ("Prim(" ^ n ^ ") is not yet implemented!")

	    (* Note that the prim names are defined in BackendInfo! *)
	    fun prim_name_to_KAM name =
	       case name
		 of "__equal_int32ub"     => PrimEquali
		  | "__equal_int32b"      => not_impl name
		  | "__equal_int31"       => PrimEquali
		  | "__equal_word31"      => PrimEquali
		  | "__equal_word32ub"    => PrimEquali
		  | "__equal_word32b"     => not_impl name

		  | "__plus_int32ub"      => PrimAddi
		  | "__plus_int32b"       => not_impl name
		  | "__plus_int31"        => PrimAddi31
		  | "__plus_word31"       => PrimAddw31
		  | "__plus_word32ub"     => PrimAddw
		  | "__plus_word32b"      => not_impl name
		  | "__plus_real"         => PrimAddf

		  | "__minus_int32ub"     => PrimSubi
		  | "__minus_int32b"      => not_impl name
		  | "__minus_int31"       => PrimSubi31
		  | "__minus_word31"      => PrimSubw31
		  | "__minus_word32ub"    => PrimSubw
		  | "__minus_word32b"     => not_impl name
		  | "__minus_real"        => PrimSubf

		  | "__mul_int32ub"       => PrimMuli
		  | "__mul_int32b"        => not_impl name
		  | "__mul_int31"         => PrimMuli31
		  | "__mul_word31"        => PrimMulw31
		  | "__mul_word32ub"      => PrimMulw
		  | "__mul_word32b"       => not_impl name
		  | "__mul_real"          => PrimMulf

		  | "__div_real"          => PrimDivf

		  | "__neg_int32ub"       => PrimNegi
		  | "__neg_int32b"        => not_impl name
		  | "__neg_int31"         => PrimNegi31
		  | "__neg_real"          => PrimNegf

		  | "__abs_int32ub"       => PrimAbsi
		  | "__abs_int32b"        => not_impl name
		  | "__abs_int31"         => PrimAbsi31
		  | "__abs_real"          => PrimAbsf

		  | "__less_int32ub"      => PrimLessThan
		  | "__less_int32b"       => not_impl name
		  | "__less_int31"        => PrimLessThan
		  | "__less_word31"       => PrimLessThanUnsigned
		  | "__less_word32ub"     => PrimLessThanUnsigned
		  | "__less_word32b"      => not_impl name
		  | "__less_real"         => PrimLessThanFloat

		  | "__lesseq_int32ub"    => PrimLessEqual
		  | "__lesseq_int32b"     => not_impl name
		  | "__lesseq_int31"      => PrimLessEqual
		  | "__lesseq_word31"     => PrimLessEqualUnsigned
		  | "__lesseq_word32ub"   => PrimLessEqualUnsigned
		  | "__lesseq_word32b"    => not_impl name
		  | "__lesseq_real"       => PrimLessEqualFloat

		  | "__greater_int32ub"   => PrimGreaterThan
		  | "__greater_int32b"    => not_impl name
		  | "__greater_int31"     => PrimGreaterThan
		  | "__greater_word31"    => PrimGreaterThanUnsigned
		  | "__greater_word32ub"  => PrimGreaterThanUnsigned
		  | "__greater_word32b"   => not_impl name
		  | "__greater_real"      => PrimGreaterThanFloat

		  | "__greatereq_int32ub" => PrimGreaterEqual
		  | "__greatereq_int32b"  => not_impl name
		  | "__greatereq_int31"   => PrimGreaterEqual
		  | "__greatereq_word31"  => PrimGreaterEqualUnsigned
		  | "__greatereq_word32ub" => PrimGreaterEqualUnsigned
		  | "__greatereq_word32b" => not_impl name
		  | "__greatereq_real"    => PrimGreaterEqualFloat

		  | "__andb_word31"       => PrimAndw
		  | "__andb_word32ub"     => PrimAndw
		  | "__andb_word32b"      => not_impl name

		  | "__orb_word31"        => PrimOrw
		  | "__orb_word32ub"      => PrimOrw
		  | "__orb_word32b"       => not_impl name

		  | "__xorb_word31"       => PrimXorw31
		  | "__xorb_word32ub"     => PrimXorw
		  | "__xorb_word32b"      => not_impl name

		  | "__shift_left_word31"             => PrimShiftLeftw31
		  | "__shift_left_word32ub"           => PrimShiftLeftw
		  | "__shift_left_word32b"            => not_impl name

		  | "__shift_right_signed_word31"     => PrimShiftRightSignedw31
		  | "__shift_right_signed_word32ub"   => PrimShiftRightSignedw
		  | "__shift_right_signed_word32b"    => not_impl name

		  | "__shift_right_unsigned_word31"   => PrimShiftRightUnsignedw31
		  | "__shift_right_unsigned_word32ub" => PrimShiftRightUnsignedw
		  | "__shift_right_unsigned_word32b"  => not_impl name
		   
		  | "__int31_to_int32b"    => not_impl name
		  | "__int31_to_int32ub"   => Primi31Toi
		  | "__int32b_to_int31"    => not_impl name
		  | "__int32ub_to_int31"   => PrimiToi31

		  | "__word31_to_word32b"  => not_impl name
		  | "__word31_to_word32ub" => Primw31Tow
		  | "__word32b_to_word31"  => not_impl name
		  | "__word32ub_to_word31" => PrimwTow31

		  | "__word31_to_word32ub_X" => Primw31TowX
		  | "__word31_to_word32b_X"  => not_impl name

		  | "__word32b_to_int32b"   => not_impl name
		  | "__word32b_to_int32b_X" => not_impl name
		  | "__int32b_to_word32b"   => not_impl name
		  | "__word32ub_to_int32ub" => PrimwToi
		  | "__word32b_to_int31"    => not_impl name
		  | "__int32b_to_word31"    => not_impl name
		  | "__word32b_to_int31_X"  => not_impl name
		   
		  | "__fresh_exname"       => PrimFreshExname

		  | "__bytetable_sub"      => PrimByteTableSub
		  | "__bytetable_update"   => PrimByteTableUpdate
		  | "__bytetable_size"     => PrimTableSize

		  | "word_sub0"            => PrimWordTableSub
		  | "word_update0"         => PrimWordTableUpdate
		  | "table_size"           => PrimTableSize

		  | "__is_null"            => PrimIsNull

		  | "terminateML"          => Halt

		  | "__serverGetCtx"       => GetContext

		  | _ => die ("PRIM(" ^ name ^ ") not implemented")
	  in	    
	    if BI.is_prim name orelse name = "terminateML" then 
	      (* rhos_for_result comes after args so that the accumulator holds the *)
	      (* pointer to allocated memory. *)
	      comp_ces(args @ rhos_for_result,env,sp,cc,
		       prim_name_to_KAM name :: acc)
	    else
	      let
	      (* rhos_for_result comes before args, because that is what the C *)
	      (* functions expects. *)
		datatype StaDyn = Dyn | Sta
		val (i,k) = case name of ":" => (0,Dyn)
		                       | _ => (name_to_built_in_C_function_index name,Sta)
		val all_args = case k 
		               of Dyn => (let val (a1,ar) = Option.valOf (List.getItem args)
					              in rhos_for_result @ ar @ [a1]
								  end handle Option.Option => 
								    die ("You must give the function to call as the first"^
									     "arguemnt to :"))
		                | Sta => rhos_for_result @ args
	      in
		if i >= 0 then 
        comp_ces(all_args,env,sp,cc,
           (case name 
		    of ":" => DCcall(1, (List.length all_args)-1)
		       | _ =>  Ccall(i, List.length all_args)) :: acc)
		else die ("Couldn't generate code for a C-call to " ^ name ^
			  "; you probably need to insert the function name in the " ^
			  "file BuiltInCFunctions.spec or BuiltInCFunctionsNsSml.spec")
	      end
	  end
      | CG_ce(ClosExp.CCALL_AUTO{name,args,res}, env,sp,cc,acc) =
	  let 
		datatype StaDyn = Dyn | Sta
		val (i,k) = case name of ":" => (0,Dyn)
		                       | _ => (name_to_built_in_C_function_index name,Sta)
		val args = 
		  case k 
		  of Dyn => 
			  let val (a1,ar) = Option.valOf (List.getItem args)  handle Option.Option => 
										die ("You must give the function to call as the first"^
											 "arguemnt to :")
			  in ar @ [a1]
			  end
		   | Sta => args 
	  in 
	    if i >= 0 then 
	      (comp_ces_ccall_auto(args,env,sp,cc,
		      (case k of Sta => Ccall(i,List.length args)
			           | Dyn => DCcall(2, List.length args - 1)) ::
				  cconvert_res res acc))
	    else die ("Couldn't generate code for a C-autocall to " ^ name ^
		      "; you probably need to insert the function name in the " ^
		      "file BuiltInCFunctions.spec or BuiltInCFunctionsNsSml.spec")
	  end
      | CG_ce(ClosExp.EXPORT {name, arg = (aty, ft1, ft2)},env,sp,cc,acc) =
         let
         in
           die "_export unsupported in the KAM backend"
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

    and comp_ces_ccall_auto ([],env,sp,cc,acc) = acc
      | comp_ces_ccall_auto ([(ce,ft)],env,sp,cc,acc) = CG_ce(ce,env,sp,cc, cconvert_arg ft acc)
      | comp_ces_ccall_auto ((ce,ft)::ces,env,sp,cc,acc) = 
      CG_ce(ce,env,sp,cc, 
	    cconvert_arg ft (push (comp_ces_ccall_auto(ces,env,sp+1,cc,acc))))

    local
      fun mk_fun f_fun (lab,cc,ce) =
	(* Region arguments start at offset 0 *)
	(* cc.res contains one pseudo lvar for each value returned, see LiftTrip in ClosExp *)
	(* I don't know what a ``pseudo lvar'' is?? ME 2000-11-04 *)
	let
	  val decomp_cc = CallConv.decompose_cc cc
	  fun add_lvar (lv,(offset,env)) = (offset+1,declareLvar(lv,STACK(offset),env))
	  fun add_clos_opt (NONE,env) = env
	    | add_clos_opt (SOME clos_lv, env) = declareLvar(clos_lv,ENV_REG,env)
(*
	  val _ = print "Regvars formals:\n"
	  val _ = app (fn lv => print (Lvars.pr_lvar lv ^ ", ")) (#reg_args(decomp_cc))
	  val _ = print "\n"
*)
          val (offset,env) = List.foldl add_lvar (0,initialEnv) (#reg_args(decomp_cc))
	  val (offset,env) = List.foldl add_lvar (offset,env) (#args(decomp_cc))
	  val env = add_clos_opt(#clos(decomp_cc),env)

	  val returns = Int.max(1, List.length (#res(decomp_cc)))  (* the Return instruction assumes 
								    * that there is at least one result 
								    * to return *)
	in
	  f_fun(lab,CG_ce(ce,env,offset,cc,[Return(offset,returns)]))
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
end
