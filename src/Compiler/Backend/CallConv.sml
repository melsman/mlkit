(* This module implements the call convention used by all the back ends *)
(* Call conventions are build and resolved. Some functions depend on    *)
(* register information. We do not give that information here in a      *)
(* structure RegisterInfo because not all back ends actually use that   *)
(* functionality (e.g., the KAM machine). The functions using register  *)
(* information are therefore parametrized over register information     *)
(* (e.g., resolve_cc, resolve_ccall, handl_arg_phreg,                   *)
(*        handl_return_phreg, resolve_act_cc)                           *)

functor CallConv(structure Lvars : LVARS
                 structure BI : BACKEND_INFO
		 structure PP : PRETTYPRINT
		 structure Flags : FLAGS
                 structure Report : REPORT
		   sharing type Report.Report = Flags.Report
		 structure Crash : CRASH) : CALL_CONV = 
  struct
    type lvar = Lvars.lvar
    type offset = int

    (***********)
    (* Logging *)
    (***********)
    fun log s = TextIO.output(!Flags.log,s ^ "\n")
    fun msg s = TextIO.output(TextIO.stdOut, s ^ "\n")
    fun die s  = Crash.impossible ("CallConv." ^ s)

    datatype CC_STY =
        CC_NO_STY of lvar
      | CC_STACK of lvar * offset
      | CC_PHREG of lvar * lvar

    type cc = {clos:       CC_STY option,
	       args:       CC_STY list,
	       reg_vec:    CC_STY option,
	       reg_args:   CC_STY list,
	       res:        CC_STY list,
	       frame_size: int option}

    (*************************)
    (* Build Call Convention *)
    (*************************)
    fun mk_sty_opt(SOME lv) = SOME(CC_NO_STY lv)
      | mk_sty_opt(NONE) = NONE

    fun mk_cc_fn(args,clos,ress) =
      {clos = mk_sty_opt clos,
       args = map CC_NO_STY args,
       reg_vec = NONE,
       reg_args = [],
       res=map CC_NO_STY ress,
       frame_size = NONE}

    fun mk_cc_fun(args,clos,reg_vec,reg_args,ress) =
      {clos = mk_sty_opt clos,
       args = map CC_NO_STY args,
       reg_vec = mk_sty_opt reg_vec,
       reg_args = map CC_NO_STY reg_args,
       res= map CC_NO_STY ress,
       frame_size = NONE}

    fun get_lvar_sty(CC_NO_STY lv) = lv
      | get_lvar_sty(CC_STACK(lv,_)) = lv
      | get_lvar_sty(CC_PHREG(lv,phreg)) = lv

    fun get_lvar_stys'(stys,C) = foldl (fn (sty,C) => get_lvar_sty sty :: C) C stys
    fun get_lvar_sty_opt'(NONE,C) = C
      | get_lvar_sty_opt'(SOME sty,C) = get_lvar_sty sty :: C

    fun get_lvar_sty_opt(NONE) = NONE
      | get_lvar_sty_opt(SOME sty) = SOME (get_lvar_sty sty)

    fun get_res_lvars({res,...}:cc) = map get_lvar_sty res

    fun get_arg_lvars({clos, args, reg_vec, reg_args, ...}:cc) = 
      get_lvar_sty_opt'(clos,
      get_lvar_stys'(args,
      get_lvar_sty_opt'(reg_vec,
      get_lvar_stys'(reg_args,[]))))

    fun decompose_cc({clos, args, reg_vec,reg_args, res, ...}:cc) : 
      {clos : lvar option, args : lvar list, 
       reg_vec : lvar option,reg_args : lvar list, res : lvar list} =
      {clos = get_lvar_sty_opt clos,
       args = map get_lvar_sty args,
       reg_vec = get_lvar_sty_opt reg_vec,
       reg_args = map get_lvar_sty reg_args,
       res = map get_lvar_sty res}

    fun get_frame_size({frame_size = NONE,...}:cc) = die "get_frame_size: frame_size does not exists"
      | get_frame_size({frame_size = SOME f_size,...}:cc) = f_size

    fun filter_out_phreg([]) = []
      | filter_out_phreg(CC_NO_STY lv::rest) = die "filter_out_phreg: stack and machine registers not annotated yet"
      | filter_out_phreg(CC_STACK a::rest) = CC_STACK a :: (filter_out_phreg rest)
      | filter_out_phreg(CC_PHREG _::rest) = filter_out_phreg rest

    fun filter_out_phreg_opt(NONE) = []
      | filter_out_phreg_opt(SOME a) = filter_out_phreg([a])

    fun filter_out_stack([],C) = C
      | filter_out_stack(CC_NO_STY lv::rest,C) = die "filter_out_stack: stack and machine registers not annotated yet"
      | filter_out_stack(CC_STACK a::rest,C) = filter_out_stack(rest,C)
      | filter_out_stack(CC_PHREG (lv,phreg)::rest,C) = filter_out_stack(rest,phreg::C)

    fun filter_out_stack_opt(NONE,C) = C
      | filter_out_stack_opt(SOME a,C) = filter_out_stack([a],C)

    fun get_rcf_size{clos,args,reg_vec,reg_args,res,frame_size} = List.length(filter_out_phreg res)
    fun get_ccf_size{clos,args,reg_vec,reg_args,res,frame_size} =
      List.length(filter_out_phreg_opt clos) +
      List.length(filter_out_phreg args) +
      List.length(filter_out_phreg_opt reg_vec)

    fun get_cc_size cc =
      get_rcf_size cc +
      get_ccf_size cc +
      1 (* The return label occupies one word on the stack. *)

    fun add_frame_size({clos,args,reg_vec,reg_args,res,frame_size},f_size) =
      {clos = clos,
       args = args,
       reg_vec = reg_vec,
       reg_args = reg_args,
       res = res,
       frame_size = SOME f_size}

    (***************************)
    (* Resolve Call Convention *)
    (***************************)
    local
      local
	val next_offset = ref 0
      in
	fun reset_offset() = next_offset := 0
	fun get_next_offset() = (next_offset := !next_offset - 1; !next_offset)
      end

      fun get_spilled_sty(CC_NO_STY lv,acc) = die "get_spilled_sty: STY not resolved yet."
	| get_spilled_sty(CC_STACK(lv,offset),acc) = (lv,offset)::acc
	| get_spilled_sty(CC_PHREG(lv,phreg),acc) = acc

      fun get_spilled_sty_opt(NONE,acc) = acc
	| get_spilled_sty_opt(SOME sty,acc) = get_spilled_sty(sty,acc)

      fun get_spilled_stys(stys,acc) = foldr (fn (sty,acc) => get_spilled_sty(sty,acc)) acc stys

      fun assign_phreg(CC_NO_STY lv, phreg) = (CC_PHREG(lv,phreg),(lv,phreg))
	| assign_phreg(CC_STACK _,_) = die "assign_phreg: sty is CC_STACK and not CC_NO_STY."
	| assign_phreg(CC_PHREG _,_) = die "assign_phreg: sty is CC_PHREG and not CC_NO_STY."

      fun assign_stack(CC_NO_STY lv) = CC_STACK(lv,get_next_offset())
	| assign_stack(CC_STACK _) = die "assign_stack: sty is CC_STACK and not CC_NO_STY."
	| assign_stack(CC_PHREG _) = die "assign_stack: sty is CC_PHREG and not CC_NO_STY."

      fun resolve_stys([],acc,ph_regs) = ([],acc,ph_regs)
	| resolve_stys(stys,acc,[]) = (map assign_stack stys,acc,[])
	| resolve_stys(sty::stys,acc,ph_reg::ph_regs) =
	let
	  val (sty_list,lv_phreg_list,ph_regs') = resolve_stys(stys,acc,ph_regs)
	  val (sty',lv_phreg') = assign_phreg(sty,ph_reg)
	in
	  (sty'::sty_list,lv_phreg'::lv_phreg_list,ph_regs')
	end

      (* for use with multiple arguments ; on the x86, we store the arguments on the stack in
       * reverse order!! *)
      fun resolve_stys_args([],acc,ph_regs) = ([],acc,ph_regs)
	| resolve_stys_args(stys,acc,[]) = 
	let val stys = if BI.down_growing_stack then rev stys else stys
	in (map assign_stack stys,acc,[])
	end
	| resolve_stys_args(sty::stys,acc,ph_reg::ph_regs) =
	let
	  val (sty_list,lv_phreg_list,ph_regs') = resolve_stys_args(stys,acc,ph_regs)
	  val (sty',lv_phreg') = assign_phreg(sty,ph_reg)
	in
	  (sty'::sty_list,lv_phreg'::lv_phreg_list,ph_regs')
	end


      fun resolve_sty_opt(SOME sty,acc,[]) = (SOME(assign_stack sty),acc,[])
	| resolve_sty_opt(SOME sty,acc,phreg::phregs) = 
	let
	  val (sty',lv_phreg') = assign_phreg(sty,phreg)
	in
	  (SOME sty',lv_phreg'::acc,phregs)
	end
        | resolve_sty_opt(NONE,acc,phregs) = (NONE,acc,phregs)

      fun resolve_list phreg_to_alpha ([],assign_list,phregs) = ([],assign_list,phregs)
	| resolve_list phreg_to_alpha (alpha,assign_list,[]) = (alpha,assign_list,[])
	| resolve_list phreg_to_alpha (alpha::alphas,assign_list,phreg::phregs) =
	let
	  val (alpha_list,assign_list,phregs') = resolve_list phreg_to_alpha (alphas,assign_list,phregs)
	  val phreg' = phreg_to_alpha phreg
	in
	  (phreg'::alpha_list,(alpha,phreg)::assign_list,phregs')
	end

      fun resolve_opt phreg_to_alpha (SOME alpha,assign_list,[]) = (SOME alpha,assign_list,[])
	| resolve_opt phreg_to_alpha (SOME alpha,assign_list,phreg::phregs) = 
	let
	  val phreg' = phreg_to_alpha phreg
	in
	  (SOME phreg',(alpha,phreg)::assign_list,phregs)
	end
	| resolve_opt phreg_to_alpha (NONE,assign_list,phregs) = (NONE,assign_list,phregs)
    in
      fun resolve_ccall args_phreg_ccall res_phreg_ccall (phreg_to_alpha: lvar  -> 'a)
	{args: 'a list, rhos_for_result: 'a list, res: 'a list} =
	let
	  val (rhos_for_result',assign_list_args,phregs) = resolve_list phreg_to_alpha (rhos_for_result,[],args_phreg_ccall)
	  val (args',assign_list_args,_) = resolve_list phreg_to_alpha (args,assign_list_args,phregs)

	  val (res',assign_list_res,_) = resolve_list phreg_to_alpha (res,[],res_phreg_ccall)
	in
	  ({args=args',rhos_for_result=rhos_for_result',res=res'},assign_list_args,assign_list_res)
	end
      fun resolve_app args_phreg res_phreg (phreg_to_alpha: lvar -> 'a)
	{clos: 'a option, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} =
	let
	  val (clos',assign_list_args,phregs) = resolve_opt phreg_to_alpha (clos,[],args_phreg)
	  val (reg_vec',assign_list_args,phregs) = resolve_opt phreg_to_alpha (reg_vec,assign_list_args,phregs)
	  val (args',assign_list_args,phregs) = resolve_list phreg_to_alpha (args,assign_list_args,phregs)
	  val (reg_args',assign_list_args,phregs) = resolve_list phreg_to_alpha (reg_args,assign_list_args,phregs)

	  val (res',assign_list_res,_) = resolve_list phreg_to_alpha (res,[],res_phreg)
	in
	  ({clos = clos',
	    args = args',
	    reg_vec = reg_vec',
	    reg_args = reg_args',
	    res = res'},assign_list_args,assign_list_res)
	end

      fun resolve_cc args_phreg res_phreg {clos,args,reg_vec,reg_args,res,frame_size} =
	let
	  val _ = reset_offset()
	  val (clos_sty_opt,lv_phreg_args,phregs) = resolve_sty_opt(clos,[],args_phreg)
	  val (reg_vec_sty_opt,lv_phreg_args,phregs) = resolve_sty_opt(reg_vec,lv_phreg_args,phregs)
	  val (args_stys,lv_phreg_args,phregs) = resolve_stys_args(args,lv_phreg_args,phregs)
	  val (reg_args_stys,lv_phreg_args,_) = resolve_stys(reg_args,lv_phreg_args,phregs)

	  val _ = get_next_offset() (* The next offset is for the return address *)
	  val (res_stys,lv_phreg_res,_) = resolve_stys(res,[],res_phreg)
	in
	  ({clos=clos_sty_opt,
	    args=args_stys,
	    reg_vec=reg_vec_sty_opt,
	    reg_args=reg_args_stys,
	    res = res_stys,
	    frame_size=frame_size},
	   lv_phreg_args,
	   lv_phreg_res)
	end

      fun get_spilled_args {clos,args,reg_vec,reg_args,res,frame_size} =
	map #1 (get_spilled_sty_opt(clos,get_spilled_stys(args,get_spilled_sty_opt(reg_vec,get_spilled_stys(reg_args,[])))))

      fun get_spilled_args_with_offsets{clos,args,reg_vec,reg_args,res,frame_size} =
	get_spilled_sty_opt(clos,get_spilled_stys(args,get_spilled_sty_opt(reg_vec,get_spilled_stys(reg_args,[]))))

      fun get_spilled_res {clos,args,reg_vec,reg_args,res,frame_size} =
	map #1 (get_spilled_stys(res,[]))

      fun get_spilled_res_with_offsets {clos,args,reg_vec,reg_args,res,frame_size} =
	get_spilled_stys(res,[])

      fun resolve_act_cc args_phreg res_phreg {clos: 'a option, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} =
	let
	  fun append_to_list_opt(NONE,l) = l
	    | append_to_list_opt(SOME e,l) = e::l
	  fun calc_offset([],offset,l) = (offset,List.rev l)
	    | calc_offset(a::aa,offset,l) = calc_offset(aa,offset+1,(a,offset)::l)
	  val res' = List.drop(res,List.length res_phreg) handle General.Subscript => []
	  val args_list = append_to_list_opt(clos,append_to_list_opt(reg_vec,args@reg_args))
	  val args' = List.drop(args_list,List.length args_phreg) handle General.Subscript => []
	  val (o_res,aty_res) = calc_offset(res',0,[])
	  val return_lab_offset = o_res
	  val (_,aty_args) = calc_offset(args',o_res+1,[])
	in
	  (aty_args,aty_res,return_lab_offset)
	end
    end

    fun get_register_args {clos,args,reg_vec,reg_args,res,frame_size} =
      filter_out_stack_opt(clos,filter_out_stack(args,filter_out_stack_opt(reg_vec,filter_out_stack(reg_args,[]))))

    (* The Call Convention supports one return register for handle functions *)
    fun handl_return_phreg res_phreg = 
      case res_phreg of
	phreg::rest => phreg
      | _ => die "handl_return_phreg needs at least one machine register for the result"

    fun handl_arg_phreg args_phreg =
      case args_phreg of
	phreg1::phreg2::rest => (phreg1,phreg2)
      | _ => die "handl function needs at least two machine registers for arguments"
	
    (******************)
    (* PrettyPrinting *)
    (******************)
    fun pr_seq [] pp = ""
      | pr_seq [e] pp = pp e
      | pr_seq (e::rest) pp = pp e ^ ", " ^ (pr_seq rest pp)

    fun pr_sty(CC_NO_STY lv) = Lvars.pr_lvar lv
      | pr_sty(CC_STACK(lv,offset)) = Lvars.pr_lvar lv ^ ":stack(" ^ Int.toString offset ^ ")"
      | pr_sty(CC_PHREG(lv,phreg)) = (*Lvars.pr_lvar lv ^ ":" ^*) Lvars.pr_lvar phreg

    fun pr_sty_opt(SOME sty) = pr_sty sty
      | pr_sty_opt(NONE) = ""

    fun pr_frame_size(NONE) = ""
      | pr_frame_size(SOME f_size) = ",f_size: " ^ Int.toString f_size

    fun pr_stys stys = pr_seq stys pr_sty

    fun pr_cc{clos,args,reg_vec,reg_args,res,frame_size} =
      "args=<" ^ pr_stys args ^
      ">,reg_vec=<" ^ pr_sty_opt reg_vec ^ 
      ">,reg_args=<" ^ pr_stys reg_args ^
      ">,clos=<" ^ pr_sty_opt clos ^ 
      ">,res=<" ^ pr_stys res ^ ">" ^
      pr_frame_size frame_size

  end
