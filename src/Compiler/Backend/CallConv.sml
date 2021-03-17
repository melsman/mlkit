(* This module implements the call convention used by all the back ends *)
(* Call conventions are build and resolved. Some functions depend on    *)
(* register information. We do not give that information here in a      *)
(* structure RegisterInfo because not all back ends actually use that   *)
(* functionality (e.g., the KAM machine). The functions using register  *)
(* information are therefore parametrized over register information     *)
(* (e.g., resolve_cc, resolve_ccall, handl_arg_phreg,                   *)
(*        handl_return_phreg, resolve_act_cc)                           *)

functor CallConv(BI : BACKEND_INFO) : CALL_CONV =
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
               reg_args:   CC_STY list,
               fargs:      CC_STY list,
               res:        CC_STY list,
               frame_size: int option}

    type 'a cc0 = {clos: 'a option, args: 'a list, reg_args: 'a list, fargs: 'a list, res: 'a list}

    (*************************)
    (* Build Call Convention *)
    (*************************)

    fun mk_sty_opt (SOME lv) = SOME(CC_NO_STY lv)
      | mk_sty_opt NONE = NONE

    fun mk_cc ({clos,args,reg_args,fargs,res}:lvar cc0) : cc =
        {clos = mk_sty_opt clos,
         args = map CC_NO_STY args,
         reg_args = map CC_NO_STY reg_args,
         fargs = map CC_NO_STY fargs,
         res=map CC_NO_STY res,
         frame_size = NONE}

    fun mk_cc_fn (args,clos,res) =
        mk_cc {clos=clos,args=args,reg_args=[],fargs=[],res=res}

    fun mk_cc_fun (args,clos,reg_args,fargs,res) =
        mk_cc {clos=clos,args=args,reg_args=reg_args,fargs=fargs,res=res}

    fun get_lvar_sty (CC_NO_STY lv) = lv
      | get_lvar_sty (CC_STACK(lv,_)) = lv
      | get_lvar_sty (CC_PHREG(lv,phreg)) = lv

    fun get_lvar_stys' (stys,C) = foldl (fn (sty,C) => get_lvar_sty sty :: C) C stys

    fun get_lvar_sty_opt' (NONE,C) = C
      | get_lvar_sty_opt' (SOME sty,C) = get_lvar_sty sty :: C

    fun get_lvar_sty_opt NONE = NONE
      | get_lvar_sty_opt (SOME sty) = SOME (get_lvar_sty sty)

    fun get_res_lvars ({res,...}:cc) = map get_lvar_sty res

    fun get_arg_lvars ({clos, args, reg_args, ...}:cc) =
        get_lvar_sty_opt'(clos,
        get_lvar_stys'(args,
        get_lvar_stys'(reg_args,[])))

    fun decompose_cc ({clos, args, reg_args, fargs, res, ...}:cc) : lvar cc0 =
        {clos = get_lvar_sty_opt clos,
         args = map get_lvar_sty args,
         reg_args = map get_lvar_sty reg_args,
         fargs = map get_lvar_sty fargs,
         res = map get_lvar_sty res}

    fun get_frame_size ({frame_size = NONE,...}:cc) = die "get_frame_size: frame_size does not exists"
      | get_frame_size ({frame_size = SOME f_size,...}:cc) = f_size

    fun filter_out_phreg [] = []
      | filter_out_phreg (CC_NO_STY lv::rest) = die "filter_out_phreg: stack and machine registers not annotated yet"
      | filter_out_phreg (CC_STACK a::rest) = CC_STACK a :: (filter_out_phreg rest)
      | filter_out_phreg (CC_PHREG _::rest) = filter_out_phreg rest

    fun filter_out_phreg_opt NONE = []
      | filter_out_phreg_opt (SOME a) = filter_out_phreg([a])

    fun filter_out_stack ([],C) = C
      | filter_out_stack (CC_NO_STY lv::rest,C) = die "filter_out_stack: stack and machine registers not annotated yet"
      | filter_out_stack (CC_STACK a::rest,C) = filter_out_stack(rest,C)
      | filter_out_stack (CC_PHREG (lv,phreg)::rest,C) = filter_out_stack(rest,phreg::C)

    fun filter_out_stack_opt (NONE,C) = C
      | filter_out_stack_opt (SOME a,C) = filter_out_stack([a],C)

    fun get_rcf_size {clos,args,reg_args,res,fargs,frame_size} = List.length(filter_out_phreg res)

    fun get_ccf_size {clos,args,reg_args,fargs,res,frame_size} =
        List.length(filter_out_phreg_opt clos) +
        List.length(filter_out_phreg args) +
        List.length(filter_out_phreg reg_args) +
        List.length(filter_out_phreg fargs)

    fun get_cc_size cc =
        get_rcf_size cc +
        get_ccf_size cc +
        1 (* The return label occupies one word on the stack. *)

    fun add_frame_size ({clos,args,reg_args,fargs,res,frame_size},f_size) =
        {clos = clos,
         args = args,
         reg_args = reg_args,
         fargs = fargs,
         res = res,
         frame_size = SOME f_size}

    (***************************)
    (* Resolve Call Convention *)
    (***************************)
    local
      local val next_offset = ref 0
      in fun reset_offset () = next_offset := 0
         fun get_next_offset () = (next_offset := !next_offset - 1; !next_offset)
      end

      fun get_spilled_sty (CC_NO_STY lv,acc) = die "get_spilled_sty: STY not resolved yet."
        | get_spilled_sty (CC_STACK(lv,offset),acc) = (lv,offset)::acc
        | get_spilled_sty (CC_PHREG(lv,phreg),acc) = acc

      fun get_spilled_sty_opt (NONE,acc) = acc
        | get_spilled_sty_opt (SOME sty,acc) = get_spilled_sty(sty,acc)

      fun get_spilled_stys (stys,acc) = foldr (fn (sty,acc) => get_spilled_sty(sty,acc)) acc stys

      fun assign_phreg (CC_NO_STY lv, phreg) = (CC_PHREG(lv,phreg),(lv,phreg))
        | assign_phreg (CC_STACK _,_) = die "assign_phreg: sty is CC_STACK and not CC_NO_STY."
        | assign_phreg (CC_PHREG _,_) = die "assign_phreg: sty is CC_PHREG and not CC_NO_STY."

      fun assign_stack (CC_NO_STY lv) = CC_STACK(lv,get_next_offset())
        | assign_stack (CC_STACK _) = die "assign_stack: sty is CC_STACK and not CC_NO_STY."
        | assign_stack (CC_PHREG _) = die "assign_stack: sty is CC_PHREG and not CC_NO_STY."

      fun resolve_stys ([], (acc,ph_regs)) = ([], (acc,ph_regs))
        | resolve_stys (stys, (acc,[])) = (map assign_stack stys, (acc,[]))
        | resolve_stys (sty::stys, (acc,ph_reg::ph_regs)) =
          let val (sty_list, (lv_phreg_list,ph_regs')) = resolve_stys (stys, (acc,ph_regs))
              val (sty', lv_phreg') = assign_phreg (sty, ph_reg)
          in (sty'::sty_list, (lv_phreg'::lv_phreg_list,ph_regs'))
          end

      (* for use with multiple arguments ; on the x86, we store the arguments on the stack in
       * reverse order!! *)
      fun resolve_stys_args (stys, rstys, fstys, (ps,regs,fregs)) =  (* rstys: region stys; stys: general purpose; fstys: float registers *)
          let fun resolv (nil, (ps,regs)) = ((nil,nil),(ps,regs))    (* no more use for resources *)
                | resolv (stys, (ps,nil)) = ((nil,stys),(ps,nil))    (* no more resources *)
                | resolv (s::ss, (ps,r::rs)) =
                  let val ((ss',ss),(ps,rs)) = resolv (ss, (ps,rs))
                      val (s',p) = assign_phreg (s,r)
                  in ((s'::ss',ss),(p::ps, rs))
                  end
              val ((astys,stys), (ps,regs)) = resolv (stys,(ps,regs))
              val ((arstys,rstys), (ps,_)) = resolv (rstys,(ps,regs))
              val ((afstys,fstys), (ps,_)) = resolv (fstys,(ps,fregs))
              val (astys',arstys',afstys') =
                  if BI.down_growing_stack then
                    let val afstys' = map assign_stack (rev fstys)
                        val arstys' = map assign_stack (rev rstys)
                        val astys' = map assign_stack (rev stys)
                    in (astys',arstys',afstys')
                    end
                  else
                    let val astys' = map assign_stack stys
                        val arstys' = map assign_stack rstys
                        val afstys' = map assign_stack fstys
                    in (astys', arstys',afstys')
                    end
          in (astys@astys', arstys@arstys', afstys@afstys', ps)
          end

(*
      fun resolve_stys_args ([], [], (acc,ph_regs)) = ([], [], (acc,ph_regs))
        | resolve_stys_args (args_stys, reg_args_stys, (acc,[])) =      (* no more phregs *)
        if BI.down_growing_stack then
          let val reg_args = map assign_stack (rev reg_args_stys)
              val args = map assign_stack (rev args_stys)
          in (args, reg_args, (acc, []))
          end
        else
          let val args = map assign_stack args_stys
              val reg_args = map assign_stack reg_args_stys
          in (args, reg_args, (acc, []))
          end
        | resolve_stys_args (asty::astys, rastys, (acc,ph_reg::ph_regs)) =
        let val (astys', rastys', (lv_phreg_list,ph_regs')) = resolve_stys_args (astys, rastys, (acc,ph_regs))
            val (asty', lv_phreg') = assign_phreg (asty, ph_reg)
        in (asty'::astys', rastys', (lv_phreg'::lv_phreg_list,ph_regs'))
        end
        | resolve_stys_args ([], rasty::rastys, (acc,ph_reg::ph_regs)) =
        let val (_,rastys', (lv_phreg_list,ph_regs')) = resolve_stys_args ([], rastys, (acc,ph_regs))
            val (rasty', lv_phreg') = assign_phreg (rasty, ph_reg)
        in ([], rasty'::rastys', (lv_phreg'::lv_phreg_list,ph_regs'))
        end
*)

      fun resolve_sty_opt (SOME sty,(ps,[])) = (SOME(assign_stack sty),(ps,[]))
        | resolve_sty_opt (SOME sty,(ps,r::rs)) =
          let val (sty,p) = assign_phreg(sty,r)
          in (SOME sty, (p::ps,rs))
          end
        | resolve_sty_opt (NONE,(ps,rs)) = (NONE,(ps,rs))

      fun resolve_list f ([], alist, phregs) = ([], alist, phregs)
        | resolve_list f (alphas, alist, []) = (alphas, alist, [])
        | resolve_list f (alpha::alphas, alist, phreg::phregs) =
          let val (alphas, alist, phregs) = resolve_list f (alphas, alist, phregs)
          in (f phreg::alphas, (alpha,phreg)::alist, phregs)
          end

      fun resolve_args (f:lvar->'a) (args:'a list, rargs:'a list, fargs: 'a list, alist, regs, fregs) =
          let val (args', alist, regs) = resolve_list f (args, alist, regs)
              val (rargs', alist, _) = resolve_list f (rargs, alist, regs)
              val (fargs', alist, _) = resolve_list f (fargs, alist, fregs)
          in (args', rargs', fargs', alist)
          end

      fun resolve_list_auto f ([], alist, phregs) = ([], alist, phregs)
        | resolve_list_auto f (alpha, alist, []) = (alpha, alist, [])
        | resolve_list_auto f ((alpha,ft)::alphas, alist, phreg::phregs) =
          let val (alphas, alist, phregs) = resolve_list_auto f (alphas, alist, phregs)
          in ((f phreg,ft)::alphas, (alpha,phreg)::alist, phregs)
          end

      fun resolve_opt f (SOME alpha, alist, []) = (SOME alpha, alist, [])
        | resolve_opt f (SOME alpha, alist, phreg::phregs) =
          (SOME (f phreg), (alpha,phreg)::alist, phregs)
        | resolve_opt f (NONE, alist, phregs) = (NONE, alist, phregs)

      fun resolve_auto f (alpha, alist, []) = (alpha, alist, [])
        | resolve_auto f ((alpha,ft), alist, phreg::phregs) =
          ((f phreg,ft), (alpha,phreg)::alist, phregs)

    in
      fun resolve_ccall args_phreg_ccall res_phreg_ccall (f: lvar  -> 'a)
          {args: 'a list, rhos_for_result: 'a list, res: 'a list} =
          let val (rhos_for_result',alist_args,phregs) = resolve_list f (rhos_for_result,[],args_phreg_ccall)
              val (args',alist_args,_) = resolve_list f (args,alist_args,phregs)
              val (res',alist_res,_) = resolve_list f (res,[],res_phreg_ccall)
          in ({args=args',rhos_for_result=rhos_for_result',res=res'},
              alist_args, alist_res)
          end

      fun resolve_ccall_auto args_phreg_ccall res_phreg_ccall (f: lvar  -> 'a)
          {args: ('a*'b) list, res: 'a*'b} =
          let val (args', alist_args, _) = resolve_list_auto f (args, nil, args_phreg_ccall)
              val (res', alist_res, _) = resolve_auto f (res, nil, res_phreg_ccall)
          in ({args=args',res=res'},
              alist_args, alist_res)
          end

      fun resolve_app {arg_regs, arg_fregs, res_regs} (f: lvar -> 'a)
          {clos: 'a option, args: 'a list, reg_args: 'a list, fargs: 'a list, res: 'a list} =
          let val (clos', alist_args, regs) = resolve_opt f (clos, [], arg_regs)
              val (args', rargs', fargs', alist_args) = resolve_args f (args, reg_args, fargs, alist_args, regs, arg_fregs)
              val (res', alist_res, _) = resolve_list f (res, [], res_regs)
          in ({clos = clos',
               args = args',
               fargs=fargs',
               reg_args = rargs',
               res = res'},
              alist_args, alist_res)  (* return assignment lists *)
          end

      fun resolve_cc {arg_regs, arg_fregs, res_regs} {clos,args,reg_args,fargs,res,frame_size} =
          let val _ = reset_offset()
              val (clos_sty_opt, (acc,regs)) = resolve_sty_opt (clos, ([], arg_regs))
              val (args_stys, reg_args_stys, fargs_stys, lv_phreg_args) =
                  resolve_stys_args (args, reg_args, fargs, (acc,regs,arg_fregs))
              val _ = get_next_offset() (* The next offset is for the return address *)
              val (res_stys, (lv_phreg_res,_)) = resolve_stys (res,([],res_regs))    (*memo: is this right on the x86?*)
          in ({clos=clos_sty_opt,
               args=args_stys,
               reg_args=reg_args_stys,
               fargs=fargs_stys,
               res = res_stys,
               frame_size=frame_size},
              lv_phreg_args,
              lv_phreg_res)
          end

      fun get_spilled_region_args {clos,args,reg_args,fargs,res,frame_size} =
          map #1 (get_spilled_stys(reg_args,[]))

      fun get_spilled_region_and_float_args {clos,args,reg_args,fargs,res,frame_size} =
          map #1 (get_spilled_stys(reg_args,get_spilled_stys(fargs,[])))

      fun get_spilled_args_with_offsets {clos,args,reg_args,fargs,res,frame_size} =
          get_spilled_sty_opt(clos,get_spilled_stys(args,get_spilled_stys(reg_args,get_spilled_stys(fargs,[]))))

      fun get_spilled_args cc = map #1 (get_spilled_args_with_offsets cc)

      fun get_spilled_res_with_offsets {clos,args,reg_args,fargs,res,frame_size} =
          get_spilled_stys(res,[])

      fun get_spilled_res cc = map #1 (get_spilled_res_with_offsets cc)

      fun resolve_act_cc {arg_regs, arg_fregs, res_regs}
                         ({clos: 'a option, args: 'a list, reg_args: 'a list,
                           fargs: 'a list, res: 'a list}: 'a cc0) =
        let fun cons_list_opt (NONE,l) = l
              | cons_list_opt (SOME e,l) = e::l
            fun calc_offset ([],offset,l) = (offset,List.rev l)
              | calc_offset (a::aa,offset,l) = calc_offset(aa,offset+1,(a,offset)::l)
            val res_stack = List.drop(res,List.length res_regs) handle General.Subscript => []
            val args_gpr = cons_list_opt(clos,args@reg_args)      (* general purpose registers *)
            val args_stack = List.drop(args_gpr, List.length arg_regs) handle General.Subscript => []
            val fargs_stack = List.drop(fargs,List.length arg_fregs) handle General.Subscript => []
            val (o_res,aty_res) = calc_offset(res_stack,0,[])
            val return_lab_offset = o_res
            val (_,aty_args) = calc_offset(args_stack@fargs_stack,o_res+1,[])
        in (aty_args,aty_res(*,return_lab_offset*))
        end
    end

    fun get_register_args {clos,args,reg_args,fargs,res,frame_size} =
        filter_out_stack_opt(clos,filter_out_stack(args,filter_out_stack(reg_args,filter_out_stack(fargs,[]))))

    fun get_register_args_excluding_region_and_float_args {clos,args,reg_args,fargs,res,frame_size} =
        filter_out_stack_opt(clos,filter_out_stack(args,[]))

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

    fun pr_sty (CC_NO_STY lv) = Lvars.pr_lvar lv
      | pr_sty (CC_STACK(lv,offset)) = Lvars.pr_lvar lv ^ ":stack(" ^ Int.toString offset ^ ")"
      | pr_sty (CC_PHREG(lv,phreg)) = (*Lvars.pr_lvar lv ^ ":" ^*) Lvars.pr_lvar phreg

    fun pr_sty_opt (SOME sty) = pr_sty sty
      | pr_sty_opt NONE = ""

    fun pr_frame_size NONE = ""
      | pr_frame_size (SOME f_size) = ",f_size: " ^ Int.toString f_size

    fun pr_stys stys = pr_seq stys pr_sty

    fun pr_cc {clos,args,reg_args,fargs,res,frame_size} =
        "args=<" ^ pr_stys args ^
        ">,reg_args=<" ^ pr_stys reg_args ^
        ">,fargs=<" ^ pr_stys fargs ^
        ">,clos=<" ^ pr_sty_opt clos ^
        ">,res=<" ^ pr_stys res ^ ">" ^
        pr_frame_size frame_size

  end
