functor FetchAndFlush(structure CallConv: CALL_CONV
			where type lvar = Lvars.lvar
		      structure LineStmt: LINE_STMT
		        where type con = Con.con
		        where type excon = Excon.excon
		        where type lvar = Lvars.lvar
                        where type place = Effect.effect
                        where type label = AddressLabels.label
		        where type phsize = PhysSizeInf.phsize
                        where type StringTree = PrettyPrint.StringTree
                        sharing type CallConv.cc = LineStmt.cc
		      structure RegAlloc: REG_ALLOC
                        where type lvar = Lvars.lvar
                        where type label = AddressLabels.label
			sharing type RegAlloc.Atom = LineStmt.Atom
		      structure RI : REGISTER_INFO
                        where type lvar = Lvars.lvar)
    : FETCH_AND_FLUSH =
struct
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  type phsize = PhysSizeInf.phsize
  type pp = PhysSizeInf.pp
  type cc = CallConv.cc
  type label = Labels.label
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
  type StoreTypeRA = RegAlloc.StoreType
  type Atom = RegAlloc.Atom

  datatype StoreType =
    STACK_STY of lvar
  | PHREG_STY of lvar * lvar
  | FLUSHED_CALLEE_STY of lvar
  | FLUSHED_CALLER_STY of lvar * lvar
  | FV_STY             of lvar * label * label

  fun pr_sty(STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
    | pr_sty(PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ LineStmt.pr_phreg phreg
    | pr_sty(FLUSHED_CALLEE_STY phreg) = LineStmt.pr_phreg phreg ^ ":flushed_callee"
    | pr_sty(FLUSHED_CALLER_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":flushed " ^ LineStmt.pr_phreg phreg
    | pr_sty(FV_STY(lv,l1,l2)) = Lvars.pr_lvar lv ^ ":FV(" ^ Labels.pr_label l1 ^ "," ^ Labels.pr_label l2 ^ ")"

  fun pr_atom atom = RegAlloc.pr_atom atom

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then print (s ^ "\n") else ()
  fun die s  = Crash.impossible ("FetchAndFlush." ^ s)
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

  local
    structure LS = LineStmt
    structure RA = RegAlloc

    val _ = Flags.add_bool_entry
      {long="print_fetch_and_flush_program", short=NONE, item=ref false, neg=false,
       menu=["Printing of intermediate forms", "print program with fetch and flush (LineStmt)"],
       desc="Print program with instructions for activation\n\
	\record fetching and flushing."}

    val region_profiling = Flags.is_on0 "region_profiling"

    fun lvset_difference(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.delete(set,lv)) lv_set lv_list
    fun lvset_add(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.add(set,lv)) lv_set lv_list
    fun lvset_delete(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.delete(set,lv)) lv_set lv_list
    fun lvset_intersection(lv_set,lv_list) = Lvarset.intersection(lv_set,Lvarset.lvarsetof lv_list)

    (****************************************************************)
    (* Statistics                                                   *)
    (****************************************************************)

    (* We record statistics for the entire program module.                     *)
    local
      val no_of_fetch    = ref 0
      val no_of_flush    = ref 0
    in
      fun fix_int i = StringCvt.padLeft #" " 7 (Int.toString i)
      fun pp_stat() =
	(chat ("Number of fetch.....: " ^ fix_int(!no_of_fetch));
	 chat ("Number of flush.....: " ^ fix_int(!no_of_flush)))
      fun reset_stat() =
	(no_of_fetch := 0;
	 no_of_flush := 0)
      fun inc_flush() = no_of_flush := !no_of_flush+1
      fun inc_fetch() = no_of_fetch := !no_of_fetch+1
    end

    (*****************************************)
    (* Insert SCOPE on Callee Save Registers *)
    (*****************************************)
    fun mk_flushed_callee([]) = []
      | mk_flushed_callee(phreg::phregs) = FLUSHED_CALLEE_STY phreg :: mk_flushed_callee phregs

    fun insert_flush_callee([],lss) = lss
      | insert_flush_callee(phreg::phregs,lss) = (inc_flush(); insert_flush_callee(phregs,LS.FLUSH(LS.PHREG phreg,())::lss))

    fun insert_fetch_callee([],lss) = lss
      | insert_fetch_callee(phreg::phregs,lss) = (inc_fetch(); insert_fetch_callee(phregs,LS.FETCH(LS.PHREG phreg,())::lss))

    fun only_finite_rhos nil = true
      | only_finite_rhos (((_,LS.WORDS _),_)::rest) = only_finite_rhos rest
      | only_finite_rhos _ = false

    fun only_null_rhos nil = true
      | only_null_rhos (((_,LS.WORDS 0),_)::rest) = only_null_rhos rest
      | only_null_rhos _ = false

    (***************************************)
    (* Calculate Set of Variables to Flush *)
    (***************************************)
    fun F_sw(F_lss,LS.SWITCH(atom,sels,default),L_set,F_set,C_set) =
      let
	val (L_set_sels, F_set_sels, C_set_sels) =
	  foldr (fn ((sel,lss), (L_set_acc, F_set, C_set)) =>
		 let val (L_set', F_set', C_set') = F_lss(lss, L_set, F_set, C_set)
		 in (Lvarset.union(L_set_acc, L_set'), F_set', C_set')
		 end) (Lvarset.empty, F_set, C_set) sels
	val (L_set_def, F_set_def, C_set_def) = F_lss(default, L_set, F_set_sels, C_set_sels)
      in
	(lvset_add(Lvarset.union(L_set_def,L_set_sels),LS.get_lvar_atom(atom,[])),
	 F_set_def,
	 C_set_def)
      end

    fun do_non_tail_call (ls,L_set,F_set,C_set) =
      let
	val (def,use) = LS.def_use_lvar_ls ls
	val lvars_to_flush = lvset_difference(L_set,def)
      in
	(lvset_add(lvars_to_flush,use),
	 Lvarset.union(F_set,lvars_to_flush),
	 C_set)
      end

    fun do_non_tail_ccall (ls,L_set,F_set,C_set) =
      let
	val (def,use) = LS.def_use_lvar_ls ls
	val lvars_to_flush = lvset_difference(L_set,def)
      in
	(lvset_add(lvars_to_flush,use),
	 F_set,
	 Lvarset.union(C_set,lvars_to_flush))
      end

    fun do_tail_call (ls,L_set,F_set,C_set) =
      let val (def,use) = LS.def_use_lvar_ls ls
      in (Lvarset.lvarsetof use, F_set, C_set)
      end

    fun F_ls(ls,L_set,F_set,C_set) =
      (case ls of
	 LS.FNCALL cc => do_non_tail_call(ls,L_set,F_set,C_set)
       | LS.FNJMP cc => do_tail_call(ls,L_set,F_set,C_set)
       | LS.FUNCALL cc => do_non_tail_call(ls,L_set,F_set,C_set)
       | LS.JMP cc => do_tail_call(ls,L_set,F_set,C_set)
       | LS.LETREGION{rhos,body} =>
	   if only_null_rhos rhos
	     orelse ( not(region_profiling()) andalso only_finite_rhos rhos ) then
	     F_lss(body,L_set,F_set,C_set)
	   else
	     let
	       (* A letregion calls a C function at entry (allocateRegion)
		* and at end (deallocateRegion), therefore the two unions below. *)
	       val (L_set',F_set',C_set') = F_lss(body,L_set,F_set,Lvarset.union(C_set,L_set))
	     in (* No variables are defined in a letregion construct. *)
	       (L_set',F_set',Lvarset.union(C_set',L_set'))
	     end
       | LS.SCOPE{pat,scope} => (* do also remove from C_set using RI.is_callee_save_c_call. 17/02/1999, Niels *)
	   let
	     val (L_set',F_set',C_set') = F_lss(scope,L_set,F_set,C_set)
	     val lvs_to_remove_C =
	       let
		 fun lv_to_remove_C(RA.STACK_STY lv,acc) = lv::acc
		   | lv_to_remove_C(RA.PHREG_STY (lv,phreg),acc) =
		   if RI.is_callee_save_ccall phreg then lv::acc
		   else acc
		   | lv_to_remove_C(RA.FV_STY lv,acc) = acc
	       in foldr lv_to_remove_C [] pat
	       end
	     val lvs_to_remove_F =
	       let
		 fun lv_to_remove_F(RA.STACK_STY lv,acc) = lv::acc
		   | lv_to_remove_F(RA.PHREG_STY (lv,phreg),acc) = acc
		   | lv_to_remove_F(RA.FV_STY lv,acc) = acc  (* hmmm;  mael 2001-03-21*)
	       in foldr lv_to_remove_F [] pat
	       end
	   in
	     (L_set',
	      lvset_difference(F_set',lvs_to_remove_F),
	      lvset_difference(C_set',lvs_to_remove_C))
	   end
       | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset} =>
	   let
	     val (L_set1,F_set1,C_set1) = F_lss(default,L_set,F_set,C_set)
	     val (L_set2,F_set2,C_set2) = F_lss(handl,L_set1,F_set1,C_set1)
	     val handl_return_lvar = LS.get_var_atom (handl_return_lv,nil)
	     val F_set3 = Lvarset.union(F_set2,lvset_difference(L_set,handl_return_lvar)) (* We must flush all caller save registers that are live *)
                                                                                          (* after the handle. We define handl_return_lv in the    *)
                                                                                          (* handle construct. 17/02/1999, Niels                   *)
	   in
	     (L_set2,F_set3,C_set2)
	   end
       | LS.HANDLE{default,handl,handl_return,offset} => die "F_ls: handl_return in HANDLE not empty"
       | LS.SWITCH_I {switch,precision} => F_sw(F_lss,switch,L_set,F_set,C_set)
       | LS.SWITCH_W {switch,precision} => F_sw(F_lss,switch,L_set,F_set,C_set)
       | LS.SWITCH_S sw => F_sw(F_lss,sw,L_set,F_set,C_set)
       | LS.SWITCH_C sw => F_sw(F_lss,sw,L_set,F_set,C_set)
       | LS.SWITCH_E sw => F_sw(F_lss,sw,L_set,F_set,C_set)
       | LS.CCALL{name,args,rhos_for_result,res} => do_non_tail_ccall(ls,L_set,F_set,C_set)
       | LS.CCALL_AUTO{name,args,rhos_for_result,res} => do_non_tail_ccall(ls,L_set,F_set,C_set)
       | _ =>
	   let
	     val (def,use) = LS.def_use_lvar_ls ls
	   in
	     (lvset_add(lvset_difference(L_set,def),use),
	      F_set,
	      C_set)
	   end)
    and F_lss(lss,L_set,F_set,C_set) =
      foldr
      (fn (ls,(L_set,F_set,C_set)) => F_ls(ls,L_set,F_set,C_set))
      (L_set,F_set,C_set) lss

    (*****************************************************)
    (* Insert Flushes and Scope on Callee Save Registers *)
    (*****************************************************)
    fun assign_sty F (RA.STACK_STY lv) = STACK_STY lv
      | assign_sty F (RA.PHREG_STY(lv,i)) =
      if Lvarset.member(lv,F) then
	FLUSHED_CALLER_STY(lv,i)
      else
	PHREG_STY(lv,i)
      | assign_sty F (RA.FV_STY lv) = FV_STY lv

    fun atom_in_F(LS.VAR lv,F) = Lvarset.member(lv,F)
      | atom_in_F(_,F) = false

    fun insert_flush([],C) = C
      | insert_flush((atom::atoms),C) = (inc_flush(); insert_flush (atoms,(LS.FLUSH(atom,())::C)))

    fun IFF_sw IFF_lss (LS.SWITCH(atom_arg,sels,default)) =
      LS.SWITCH(atom_arg,map (fn (s,lss) => (s,IFF_lss lss)) sels, IFF_lss default)

    fun IFF_lss(lss,F,acc) =
      let
	fun IFF_lss'([]) = acc
	  | IFF_lss'(LS.ASSIGN{pat,bind}::lss) =
	  if atom_in_F(pat,F) then
	    (inc_flush();
	     LS.ASSIGN{pat=pat,bind=bind}::(LS.FLUSH(pat,()))::IFF_lss' lss)
	  else
	    LS.ASSIGN{pat=pat,bind=bind} :: IFF_lss' lss
	  | IFF_lss'(LS.FLUSH _::lss) = die "IFF_ls: FLUSH not inserted yet!"
	  | IFF_lss'(LS.FETCH _::lss) = die "IFF_ls: FETCH not inserted yet!"
	  | IFF_lss'(LS.FNJMP cc::lss) = LS.FNJMP cc :: IFF_lss' lss
	  | IFF_lss'(LS.FNCALL(cc as {res,...})::lss) =
	    let
	      val flushed_lvars = List.filter (fn atom => atom_in_F(atom,F)) res
	    in
	      LS.FNCALL cc :: (insert_flush(flushed_lvars,IFF_lss' lss))
	    end
	  | IFF_lss'(LS.JMP cc::lss) = LS.JMP cc :: IFF_lss' lss
	  | IFF_lss'(LS.FUNCALL(cc as {res,...})::lss) =
	    let
	      val flushed_lvars = List.filter (fn atom => atom_in_F(atom,F)) res
	    in
	      LS.FUNCALL cc :: (insert_flush(flushed_lvars,IFF_lss' lss))
	    end
	  | IFF_lss'(LS.LETREGION{rhos,body}::lss) = LS.LETREGION{rhos=rhos,body=IFF_lss(body,F,[])} :: IFF_lss' lss
	  | IFF_lss'(LS.SCOPE{pat,scope}::lss) = LS.SCOPE{pat=map (assign_sty F) pat,scope=IFF_lss(scope,F,[])} :: IFF_lss' lss
	  | IFF_lss'(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset}::lss) =
	    if atom_in_F(handl_return_lv,F) then (* handl_return_lv is defined and therefore flushed if necessary *)
	      (inc_flush();
	       LS.HANDLE{default=IFF_lss(default,F,[]),handl=(IFF_lss(handl,F,[]),handl_lv),
			 handl_return=([LS.FLUSH(handl_return_lv,())],handl_return_lv,bv),offset=offset} :: IFF_lss' lss)
	    else
	      LS.HANDLE{default=IFF_lss(default,F,[]),handl=(IFF_lss(handl,F,[]),handl_lv),
			handl_return=([],handl_return_lv,bv),offset=offset} :: IFF_lss' lss
	  | IFF_lss'(LS.HANDLE{default,handl,handl_return,offset}::lss) = die "IFF_lss': handle_return in HANDLE not empty"
	  | IFF_lss'(LS.RAISE{arg,defined_atys}::lss) = LS.RAISE{arg=arg,defined_atys=defined_atys} :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_I {switch,precision}::lss) =
	      LS.SWITCH_I{switch=IFF_sw (fn lss => IFF_lss(lss,F,[])) switch,
			  precision=precision} :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_W {switch,precision}::lss) =
	      LS.SWITCH_W{switch=IFF_sw (fn lss => IFF_lss(lss,F,[])) switch,
			  precision=precision} :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_S sw::lss) = LS.SWITCH_S(IFF_sw (fn lss => IFF_lss(lss,F,[])) sw) :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_C sw::lss) = LS.SWITCH_C(IFF_sw (fn lss => IFF_lss(lss,F,[])) sw) :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_E sw::lss) = LS.SWITCH_E(IFF_sw (fn lss => IFF_lss(lss,F,[])) sw) :: IFF_lss' lss
	  | IFF_lss'(LS.RESET_REGIONS a::lss) = LS.RESET_REGIONS a :: IFF_lss' lss
	  | IFF_lss'(LS.PRIM{name,args,res}::lss) =
	    let val flushed_lvars = List.filter (fn atom => atom_in_F(atom,F)) res
	    in LS.PRIM{name=name,args=args,res=res} :: insert_flush(flushed_lvars,IFF_lss' lss)
	    end
	  | IFF_lss'(LS.CCALL{name,args,rhos_for_result,res}::lss) =
	    let val flushed_lvars = List.filter (fn atom => atom_in_F(atom,F)) res
	    in LS.CCALL{name=name,args=args,rhos_for_result=rhos_for_result,res=res} :: insert_flush(flushed_lvars,IFF_lss' lss)
	    end
	  | IFF_lss'(LS.CCALL_AUTO{name,args,rhos_for_result,res}::lss) =
	    let val flushed_lvars =
	          case res
		    of (a,ft) => if atom_in_F(a,F) then [a] else nil
	    in LS.CCALL_AUTO{name=name,args=args,rhos_for_result=rhos_for_result,res=res} :: insert_flush(flushed_lvars,IFF_lss' lss)
	    end
	  | IFF_lss'(LS.EXPORT{name,clos_lab,arg}::lss) =
	    LS.EXPORT{name=name,clos_lab=clos_lab,arg=arg} :: IFF_lss' lss
      in
	IFF_lss' lss
      end

    (******************)
    (* Insert Fetches *)
    (******************)
    fun IF_sw(IF_lss,switch_con,LS.SWITCH(atom,sels,default),U_set,lss) =
      let
	val (acc,U_set_acc) = IF_lss(lss,U_set)
	val (sels',U_set_sels) =
	  foldr (fn ((sel,lss),(sels_acc,U_set_acum)) =>
		 let
		   val (lss',U_set_sel) = IF_lss(lss,U_set_acc)
		 in
		   ((sel,lss')::sels_acc,Lvarset.union(U_set_acum,U_set_sel))
		 end) ([],Lvarset.empty) sels
	val (default',U_set_default) = IF_lss(default,U_set_acc)
      in
	(switch_con(LS.SWITCH(atom,sels',default'))::acc,
	 lvset_add(Lvarset.union(U_set_sels,U_set_default),LS.get_lvar_atom(atom,[])))
      end

    fun insert_fetch_if([],acc) = acc
      | insert_fetch_if(lv::lvs,acc) = (inc_fetch(); LS.FETCH(LS.VAR lv,())::insert_fetch_if(lvs,acc))

    fun do_non_tail_call_if(ls,F_set,(acc,U_set)) =
      let
	val (def,use) = LS.def_use_lvar_ls ls
	val lvars_used = lvset_difference(U_set,def)
	val lvars_to_fetch = Lvarset.intersection(F_set,lvars_used)
      in
	(ls :: insert_fetch_if(Lvarset.members lvars_to_fetch,acc),
	 lvset_add(Lvarset.difference(lvars_used,lvars_to_fetch),use))
      end

    fun IF_lss(lss,F_set,C_set) =
      let
	fun IF_lss'([],U_set) = ([],U_set)
	  | IF_lss'((ls as LS.FNCALL cc)::lss,U_set) = do_non_tail_call_if(ls,F_set,IF_lss'(lss,U_set))
	  | IF_lss'((ls as LS.FUNCALL cc)::lss,U_set) = do_non_tail_call_if(ls,F_set,IF_lss'(lss,U_set))
	  | IF_lss'(LS.LETREGION{rhos,body}::lss,U_set) =
	  (* If we have any infinite regions, then we perform a C-call at entry (allocRegion) and at
	   * exit (deallocateRegion). We must then fetch caller save registers! When profiling is enabled,
	   * we perform C calls at entry and exit also for finite regions. *)
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val ccalls_in_and_out : bool =
	      only_null_rhos rhos orelse ( not(region_profiling())
					  andalso only_finite_rhos rhos )
	    val lv_fetch2 =
	      if ccalls_in_and_out then	Lvarset.empty
	      else Lvarset.intersection(C_set,U_set_acc)

	    val (body,U_set_body) = IF_lss'(body,Lvarset.difference(U_set_acc,lv_fetch2))

	    val lv_fetch1 =
	      if ccalls_in_and_out then Lvarset.empty
	      else Lvarset.intersection(C_set,U_set_body)
	  in
	    (LS.LETREGION{rhos=rhos,body=insert_fetch_if(Lvarset.members lv_fetch1,body)}::
	     insert_fetch_if(Lvarset.members lv_fetch2,acc),Lvarset.difference(U_set_body,lv_fetch1))
	  end
	  | IF_lss'(LS.SCOPE{pat,scope}::lss,U_set) =
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val (scope,U_set_scope) = IF_lss'(scope,U_set_acc)
	  in
	    (LS.SCOPE{pat=pat,scope=scope}::acc,U_set_scope)
	  end
	  | IF_lss'(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_lv,bv),offset}::lss,U_set) =
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val (lss1',U_set1) = IF_lss'(default,U_set_acc)
	    val (lss2',U_set2) = IF_lss'(handl,U_set1)
	    val lvars_used = lvset_difference(U_set_acc,LS.get_lvar_atom(handl_return_lv,[]))
	    val lvars_to_fetch = Lvarset.members (Lvarset.intersection(F_set,lvars_used))
	  in
	    case handl_return of
	      [] => (LS.HANDLE{default=lss1',handl=(lss2',handl_lv),
			       handl_return=(insert_fetch_if(lvars_to_fetch,[]),handl_return_lv,bv),
			       offset=offset}::acc,U_set2)
	    | [flush_ls] => (LS.HANDLE{default=lss1',handl=(lss2',handl_lv),
				       handl_return=(flush_ls :: insert_fetch_if(lvars_to_fetch,[]),handl_return_lv,bv),
				       offset=offset}::acc,U_set2)
	    | _ => die "IF_lss': handl_return contains more than one statement."
	  end
	  | IF_lss'(LS.SWITCH_I {switch,precision}::lss,U_set) =
	  IF_sw(IF_lss',fn sw => LS.SWITCH_I {switch=sw, precision=precision},
		switch,U_set,lss)
	  | IF_lss'(LS.SWITCH_W {switch,precision}::lss,U_set) =
	  IF_sw(IF_lss',fn sw => LS.SWITCH_W {switch=sw, precision=precision},
		switch,U_set,lss)
	  | IF_lss'(LS.SWITCH_S sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_S,sw,U_set,lss)
	  | IF_lss'(LS.SWITCH_C sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_C,sw,U_set,lss)
	  | IF_lss'(LS.SWITCH_E sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_E,sw,U_set,lss)
	  | IF_lss'((ls as LS.CCALL{name,args,rhos_for_result,res})::lss,U_set) =
	  do_non_tail_call_if(ls,C_set,IF_lss'(lss,U_set)) (* Note, we use C_set and not F_set *)
	  | IF_lss'((ls as LS.CCALL_AUTO{name,args,rhos_for_result,res})::lss,U_set) =
	  do_non_tail_call_if(ls,C_set,IF_lss'(lss,U_set)) (* Note, we use C_set and not F_set *)
	  | IF_lss'(ls::lss,U_set) =
	  let
	    val (def,use) = LS.def_use_lvar_ls ls
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	  in
	    (ls::acc,
	     lvset_add(lvset_difference(U_set_acc,def),use))
	  end
      in
	IF_lss'(lss,Lvarset.empty)
      end

    (*********************************)
    (* IFF on Top level Declarations *)
    (*********************************)
    fun do_top_decl gen_fn (lab,cc,lss) =
      let
	val (_,F_set,C_set) = F_lss(lss,Lvarset.empty,Lvarset.empty,Lvarset.empty)

	(* remove variables from F and C that are not assigned
	 * to registers... mael 2001-03-21 *)
	val F = lvset_delete(F_set,CallConv.get_spilled_args cc)
	val C = lvset_delete(C_set,CallConv.get_spilled_args cc)
	val lss_iff = IFF_lss(lss,Lvarset.union(F,C),nil)
	val (lss_if,_) = IF_lss(lss_iff,F,C)
      in
	gen_fn(lab,cc,lss_if)
      end
    fun IFF_top_decl(LS.FUN(lab,cc,lss)) = do_top_decl LS.FUN (lab,cc,lss)
      | IFF_top_decl(LS.FN(lab,cc,lss)) = do_top_decl LS.FN (lab,cc,lss)

    val print_fetch_and_flush_program = Flags.is_on0 "print_fetch_and_flush_program"
  in
    fun IFF {main_lab:label,
	     code=ra_prg: (StoreTypeRA,unit,Atom) LinePrg,
	     imports:label list * label list,
	     exports:label list * label list} =
      let
	val _ = chat "[Insert Fetch and Flush..."
	val _ = reset_stat()
	val line_prg_iff = foldr (fn (func,acc) => IFF_top_decl func :: acc) [] ra_prg
	val _ =
	  if print_fetch_and_flush_program() then
	    display("\nReport: AFTER INSERT FETCH AND FLUSH:",
		    LS.layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_iff)
	  else ()
(*	val _ = pp_stat() *)
	val _ = chat "]\n"
      in
	{main_lab=main_lab,code=line_prg_iff: (StoreType,unit,Atom) LinePrg,imports=imports,exports=exports}
      end

  end

end;
