functor CallConv(structure Lvars : LVARS
                 structure BI : BACKEND_INFO
                   sharing type BI.lvar = Lvars.lvar
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
	       free:       CC_STY list,
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

    fun mk_cc_fn(args,clos,free,ress) =
      {clos = mk_sty_opt clos,
       free = map CC_NO_STY free,
       args = map CC_NO_STY args,
       reg_vec = NONE,
       reg_args = [],
       res=map CC_NO_STY ress,
       frame_size = NONE}

    fun mk_cc_fun(args,clos,free,reg_vec,reg_args,ress) =
      {clos = mk_sty_opt clos,
       free = map CC_NO_STY free,
       args = map CC_NO_STY args,
       reg_vec = mk_sty_opt reg_vec,
       reg_args = map CC_NO_STY reg_args,
       res= map CC_NO_STY ress,
       frame_size = NONE}

    fun get_lvar_sty(CC_NO_STY lv) = lv
      | get_lvar_sty(CC_STACK(lv,_)) = lv
      | get_lvar_sty(CC_PHREG(lv,phreg)) = lv

    fun get_res_lvars({res,...}:cc) = map get_lvar_sty res

    fun get_frame_size({frame_size = NONE,...}:cc) = die "get_frame_size: frame_size does not exists"
      | get_frame_size({frame_size = SOME f_size,...}:cc) = f_size

    fun filter_out_phreg([]) = []
      | filter_out_phreg(CC_NO_STY lv::rest) = die "filter_out_phreg: stack and machine registers not annotated yet"
      | filter_out_phreg(CC_STACK a::rest) = CC_STACK a :: (filter_out_phreg rest)
      | filter_out_phreg(CC_PHREG _::rest) = filter_out_phreg rest

    fun filter_out_phreg_opt(NONE) = []
      | filter_out_phreg_opt(SOME a) = filter_out_phreg([a])

    fun get_cc_size{clos,free,args,reg_vec,reg_args,res,frame_size} =
      List.length(filter_out_phreg_opt clos) +
      List.length(filter_out_phreg free) +
      List.length(filter_out_phreg args) +
      List.length(filter_out_phreg_opt reg_vec) +
      List.length(filter_out_phreg res) +
      1 (* The return label occupies one word on the stack. *)

    fun add_frame_size({clos,free,args,reg_vec,reg_args,res,frame_size},f_size) =
      {clos = clos,
       free = free,
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
      fun resolve_ccall(phreg_to_alpha: lvar  -> 'a)
	{args: 'a list, rhos_for_result: 'a list, res: 'a list} =
	let
	  val (args',assign_list_args,phregs) = resolve_list phreg_to_alpha (args,[],BI.args_phreg_ccall)
	  val (rhos_for_result',assign_list_args,_) = resolve_list phreg_to_alpha (rhos_for_result,assign_list_args,phregs)

	  val (res',assign_list_res,_) = resolve_list phreg_to_alpha (res,[],BI.res_phreg_ccall)
	in
	  ({args=args',rhos_for_result=rhos_for_result',res=res'},assign_list_args,assign_list_res)
	end
      fun resolve_app (phreg_to_alpha: lvar -> 'a)
	{clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} =
	let
	  val (clos',assign_list_args,phregs) = resolve_opt phreg_to_alpha (clos,[],BI.args_phreg)
	  val (reg_vec',assign_list_args,phregs) = resolve_opt phreg_to_alpha (reg_vec,assign_list_args,phregs)
	  val (args',assign_list_args,phregs) = resolve_list phreg_to_alpha (args,assign_list_args,phregs)
	  val (free',assign_list_args,phregs) = resolve_list phreg_to_alpha (free,assign_list_args,phregs)
	  val (reg_args',assign_list_args,phregs) = resolve_list phreg_to_alpha (reg_args,assign_list_args,phregs)

	  val (res',assign_list_res,_) = resolve_list phreg_to_alpha (res,[],BI.res_phreg)
	in
	  ({clos = clos',
	    free = free',
	    args = args',
	    reg_vec = reg_vec',
	    reg_args = reg_args',
	    res = res'},assign_list_args,assign_list_res)
	end

      fun resolve_cc {clos,free,args,reg_vec,reg_args,res,frame_size} =
	let
	  val _ = reset_offset()
	  val (clos_sty_opt,lv_phreg_args,phregs) = resolve_sty_opt(clos,[],BI.args_phreg)
	  val (reg_vec_sty_opt,lv_phreg_args,phregs) = resolve_sty_opt(reg_vec,lv_phreg_args,phregs)
	  val (args_stys,lv_phreg_args,phregs) = resolve_stys(args,lv_phreg_args,phregs)
	  val (free_stys,lv_phreg_args,phregs) = resolve_stys(free,lv_phreg_args,phregs)
	  val (reg_args_stys,lv_phreg_args,_) = resolve_stys(reg_args,lv_phreg_args,phregs)

	  val _ = get_next_offset() (* The next offset is for the return address *)
	  val (res_stys,lv_phreg_res,_) = resolve_stys(res,[],BI.res_phreg)
	in
	  ({clos=clos_sty_opt,
	    free=free_stys,
	    args=args_stys,
	    reg_vec=reg_vec_sty_opt,
	    reg_args=reg_args_stys,
	    res = res_stys,
	    frame_size=frame_size},
	   lv_phreg_args,
	   lv_phreg_res)
	end

      fun get_spilled_args {clos,free,args,reg_vec,reg_args,res,frame_size} =
	map #1 (get_spilled_sty_opt(clos,get_spilled_stys(free,get_spilled_stys(args,get_spilled_sty_opt(reg_vec,get_spilled_stys(reg_args,[]))))))

      fun get_spilled_args_with_offsets{clos,free,args,reg_vec,reg_args,res,frame_size} =
	get_spilled_sty_opt(clos,get_spilled_stys(free,get_spilled_stys(args,get_spilled_sty_opt(reg_vec,get_spilled_stys(reg_args,[])))))

      fun get_spilled_res {clos,free,args,reg_vec,reg_args,res,frame_size} =
	map #1 (get_spilled_stys(res,[]))

      fun get_spilled_res_with_offsets {clos,free,args,reg_vec,reg_args,res,frame_size} =
	get_spilled_stys(res,[])
    end
	
    (******************)
    (* PrettyPrinting *)
    (******************)
    fun pr_seq [] pp = ""
      | pr_seq [e] pp = pp e
      | pr_seq (e::rest) pp = pp e ^ ", " ^ (pr_seq rest pp)

    fun pr_sty(CC_NO_STY lv) = Lvars.pr_lvar lv
      | pr_sty(CC_STACK(lv,offset)) = Lvars.pr_lvar lv ^ ":stack(" ^ Int.toString offset ^ ")"
      | pr_sty(CC_PHREG(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ Lvars.pr_lvar phreg

    fun pr_sty_opt(SOME sty) = pr_sty sty
      | pr_sty_opt(NONE) = ""

    fun pr_frame_size(NONE) = ""
      | pr_frame_size(SOME f_size) = ",f_size: " ^ Int.toString f_size

    fun pr_stys stys = pr_seq stys pr_sty

    fun pr_cc{clos,free,args,reg_vec,reg_args,res,frame_size} =
      "args=<" ^ pr_stys args ^
      ">,reg_vec=<" ^ pr_sty_opt reg_vec ^ 
      ">,reg_args=<" ^ pr_stys reg_args ^
      ">,clos=<" ^ pr_sty_opt clos ^ 
      ">,free=<" ^ pr_stys free ^ 
      ">,res=<" ^ pr_stys res ^ ">" ^
      pr_frame_size frame_size

  end
