functor FetchAndFlush(structure PhysSizeInf : PHYS_SIZE_INF
		      structure Con : CON
		      structure Excon : EXCON
		      structure Lvars : LVARS
		      structure Effect : EFFECT
		      structure Labels : ADDRESS_LABELS
		      structure CallConv: CALL_CONV
		      structure LineStmt: LINE_STMT
		        sharing type Con.con = LineStmt.con
		        sharing type Excon.excon = LineStmt.excon
		        sharing type Lvars.lvar = LineStmt.lvar = CallConv.lvar
                        sharing type Effect.effect = Effect.place = LineStmt.place
                        sharing type Labels.label = LineStmt.label
                        sharing type CallConv.cc = LineStmt.cc
		        sharing type LineStmt.phsize = PhysSizeInf.phsize
		      structure RegAlloc: REG_ALLOC
                        sharing type RegAlloc.lvar = LineStmt.lvar
			sharing type RegAlloc.Atom = LineStmt.Atom
		      structure BI : BACKEND_INFO
                        sharing type BI.lvar = Lvars.lvar
		      structure Lvarset: LVARSET
		        sharing type Lvarset.lvar = LineStmt.lvar
			sharing type Lvarset.lvarset= BI.lvarset
		      structure PP : PRETTYPRINT
		        sharing type PP.StringTree = 
                                   Effect.StringTree = 
				   LineStmt.StringTree
                      structure Flags : FLAGS
		      structure Report : REPORT
		        sharing type Report.Report = Flags.Report
		      structure Crash : CRASH) : FETCH_AND_FLUSH =
struct

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

  fun pr_sty(STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
    | pr_sty(PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ LineStmt.pr_phreg phreg
    | pr_sty(FLUSHED_CALLEE_STY phreg) = LineStmt.pr_phreg phreg ^ ":flushed_callee"
    | pr_sty(FLUSHED_CALLER_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":flushed " ^ LineStmt.pr_phreg phreg

  fun pr_atom atom = RegAlloc.pr_atom atom

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
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

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_fetch_and_flush_program", "print program with fetch and flush (LineStmt)", ref false)]

  local
    structure LS = LineStmt
    structure RA = RegAlloc

    fun lvset_difference(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.delete(set,lv)) lv_set lv_list
    fun lvset_add(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.add(set,lv)) lv_set lv_list
    fun lvset_delete(lv_set,lv_list) = foldr (fn (lv,set) => Lvarset.delete(set,lv)) lv_set lv_list
    fun lvset_intersection(lv_set,lv_list) = Lvarset.intersection(lv_set,Lvarset.lvarsetof lv_list)

    (*****************************************)
    (* Insert SCOPE on Callee Save Registers *)    
    (*****************************************)
    fun mk_flushed_callee([]) = []
      | mk_flushed_callee(phreg::phregs) = FLUSHED_CALLEE_STY phreg :: mk_flushed_callee phregs

    fun insert_flush_callee([],lss) = lss
      | insert_flush_callee(phreg::phregs,lss) = insert_flush_callee(phregs,LS.FLUSH(LS.PHREG phreg,())::lss)

    fun insert_fetch_callee([],lss) = lss
      | insert_fetch_callee(phreg::phregs,lss) = insert_fetch_callee(phregs,LS.FETCH(LS.PHREG phreg,())::lss)

    (***************************************)
    (* Calculate Set of Variables to Flush *)
    (***************************************)
    fun F_sw(F_lss,LS.SWITCH(atom,sels,default),L_set,F_set,R_set) =
      let
	val (L_set_sels,F_set_sels,R_set_sels) = 
	  foldr 
	  (fn ((sel,lss),(L_set_acc,F_set,R_set)) => 
	   let 
	     val (L_set',F_set',R_set') = F_lss(lss,L_set,F_set,R_set)
	   in 
	     (Lvarset.union(L_set_acc,L_set'),F_set',R_set') 
	   end) (Lvarset.empty(*L_set*),F_set,R_set) sels
	val (L_set_def,F_set_def,R_set_def) = F_lss(default,L_set,F_set_sels,R_set_sels)
      in
	(lvset_add(Lvarset.union(L_set_def,L_set_sels),LS.get_lvar_atom(atom,[])),
	 F_set_def,
	 R_set_def)
      end

    fun do_non_tail_call (ls,L_set,F_set,R_set) =
      let
	val (def,use) = LineStmt.def_use_lvar_ls ls
	val lvars_to_flush = lvset_difference(L_set,def)
      in
	(lvset_add(lvars_to_flush,use),
	 Lvarset.union(F_set,lvars_to_flush),
	 lvset_add(R_set,LS.get_phreg_ls ls))
      end

    fun F_ls(ls,L_set,F_set,R_set) = 
      (case ls of
	 LS.FNCALL cc => do_non_tail_call(ls,L_set,F_set,R_set)
       | LS.FUNCALL cc => do_non_tail_call(ls,L_set,F_set,R_set)
       | LS.LETREGION{rhos,body} => F_lss(body,L_set,F_set,R_set)
       | LS.SCOPE{pat,scope} =>
	   let
	     val (L_set',F_set',R_set') = F_lss(scope,L_set,F_set,R_set)
	     fun lv_to_remove(RA.STACK_STY lv,acc) = lv::acc
	       | lv_to_remove(RA.PHREG_STY (lv,phreg),acc) = 
	       if BI.is_callee_save(phreg) then
		 lv::acc
	       else
		 acc
	     val lvs_to_remove = foldr (fn (sty,acc) => lv_to_remove(sty,acc)) [] pat
	     fun add_phreg(RA.STACK_STY lv,acc) = acc
	       | add_phreg(RA.PHREG_STY (lv,phreg),acc) = phreg::acc
	     val phregs_to_add = foldr (fn (sty,acc) => add_phreg(sty,acc)) [] pat
	   in
	     (L_set',lvset_difference(F_set',lvs_to_remove),lvset_add(R_set',phregs_to_add))
	   end
       | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv),offset} =>
	   let
	     val (L_set1,F_set1,R_set1) = F_lss(default,L_set,F_set,R_set)
	     val (L_set2,F_set2,R_set2) = F_lss(handl,L_set1,F_set1,R_set1) (* was L_set *)
	     val R_set_all = Lvarset.union(R_set2,BI.callee_save_phregset)  (* We must save ALL callee save registers across a handle! *)
	   in
	     (L_set2(*Lvarset.union(L_set1,L_set2)*),F_set2,R_set_all)  (* It should be possible to make L_set1 and L_set2 in sequence? *)
	   end
       | LS.HANDLE{default,handl,handl_return,offset} => die "F_ls: handl_return in HANDLE not empty"
       | LS.SWITCH_I sw => F_sw(F_lss,sw,L_set,F_set,R_set)
       | LS.SWITCH_S sw => F_sw(F_lss,sw,L_set,F_set,R_set)
       | LS.SWITCH_C sw => F_sw(F_lss,sw,L_set,F_set,R_set)
       | LS.SWITCH_E sw => F_sw(F_lss,sw,L_set,F_set,R_set)
       | LS.CCALL{name,args,rhos_for_result,res} => do_non_tail_call(ls,L_set,F_set,R_set)
       | _ =>
	   let
	     val (def,use) = LineStmt.def_use_lvar_ls ls
	   in
	     (lvset_add(lvset_difference(L_set,def),use),
	      F_set,
	      lvset_add(R_set,LS.get_phreg_ls ls))
	   end)
    and F_lss(lss,L_set,F_set,R_set) = 
      foldr 
      (fn (ls,(L_set,F_set,R_set)) => F_ls(ls,L_set,F_set,R_set))
      (L_set,F_set,R_set) lss

    (*****************************************************)
    (* Insert Flushes and Scope on Callee Save Registers *)
    (*****************************************************)
    fun assign_sty F (RA.STACK_STY lv) = STACK_STY lv
      | assign_sty F (RA.PHREG_STY(lv,i)) = 
      if Lvarset.member(lv,F) then
	FLUSHED_CALLER_STY(lv,i)
      else
	PHREG_STY(lv,i)

    fun atom_in_F(LS.VAR lv,F) = Lvarset.member(lv,F)
      | atom_in_F(_,F) = false

    fun insert_flush([],C) = C
      | insert_flush((atom::atoms),C) = insert_flush (atoms,(LS.FLUSH(atom,())::C))
	 
    fun IFF_sw IFF_lss (LS.SWITCH(atom_arg,sels,default)) =
      LS.SWITCH(atom_arg,map (fn (s,lss) => (s,IFF_lss lss)) sels, IFF_lss default)

    fun IFF_lss(lss,F,acc) =
      let 
	fun IFF_lss'([]) = acc
	  | IFF_lss'(LS.ASSIGN{pat,bind}::lss) =
	  if atom_in_F(pat,F) then
	    LS.ASSIGN{pat=pat,bind=bind}::(LS.FLUSH(pat,()))::IFF_lss' lss
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
	  | IFF_lss'(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv),offset}::lss) = 
	    LS.HANDLE{default=IFF_lss(default,F,[]),handl=(IFF_lss(handl,F,[]),handl_lv),handl_return=([],handl_return_lv),offset=offset} :: IFF_lss' lss
	  | IFF_lss'(LS.HANDLE{default,handl,handl_return,offset}::lss) = die "IFF_lss': handle_return in HANDLE not empty"
	  | IFF_lss'(LS.RAISE{arg,defined_atys}::lss) = LS.RAISE{arg=arg,defined_atys=defined_atys} :: IFF_lss' lss
	  | IFF_lss'(LS.SWITCH_I sw::lss) = LS.SWITCH_I(IFF_sw (fn lss => IFF_lss(lss,F,[])) sw) :: IFF_lss' lss
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

    fun do_non_tail_call_if(ls,F_set,(acc,U_set)) =
      let
	val (def,use) = LineStmt.def_use_lvar_ls ls
	val lvars_used = lvset_difference(U_set,def)
	val lvars_to_fetch = Lvarset.intersection(F_set,lvars_used)
	fun insert_fetch([],acc) = acc
	  | insert_fetch(lv::lvs,acc) = LS.FETCH(LS.VAR lv,())::insert_fetch(lvs,acc)
      in
	(ls :: insert_fetch(Lvarset.members lvars_to_fetch,acc),
	 Lvarset.lvarsetof use (*lvset_add(lvars_used,use)04/01/1999, Niels*))
      end

    fun IF_lss(lss,F_set) =
      let
	fun IF_lss'([],U_set) = ([],U_set)
	  | IF_lss'((ls as LS.FNCALL cc)::lss,U_set) = do_non_tail_call_if(ls,F_set,IF_lss'(lss,U_set))
	  | IF_lss'((ls as LS.FUNCALL cc)::lss,U_set) = do_non_tail_call_if(ls,F_set,IF_lss'(lss,U_set))
	  | IF_lss'(LS.LETREGION{rhos,body}::lss,U_set) = 
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val (body,U_set_body) = IF_lss'(body,U_set_acc)
	  in
	    (LS.LETREGION{rhos=rhos,body=body}::acc,U_set_body)
	  end
	  | IF_lss'(LS.SCOPE{pat,scope}::lss,U_set) =
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val (scope,U_set_scope) = IF_lss'(scope,U_set_acc)
	  in
	    (LS.SCOPE{pat=pat,scope=scope}::acc,U_set_scope)
	  end
	  | IF_lss'(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv),offset}::lss,U_set) =
	  let
	    val (acc,U_set_acc) = IF_lss'(lss,U_set)
	    val (lss1',U_set1) = IF_lss'(default,U_set_acc)
	    val (lss2',U_set2) = IF_lss'(handl,U_set_acc)
	  in
	    (LS.HANDLE{default=lss1',handl=(lss2',handl_lv),handl_return=(insert_fetch_callee(BI.callee_save_phregs,[]),handl_return_lv),
		       offset=offset}::acc,Lvarset.union(U_set1,U_set2))
	  end
	  | IF_lss'(LS.HANDLE{default,handl,handl_return,offset}::lss,U_set) = die "IF_lss': handl_return in HANDLE not empty"
	  | IF_lss'(LS.SWITCH_I sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_I,sw,U_set,lss)
	  | IF_lss'(LS.SWITCH_S sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_S,sw,U_set,lss)
	  | IF_lss'(LS.SWITCH_C sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_C,sw,U_set,lss)
	  | IF_lss'(LS.SWITCH_E sw::lss,U_set) = IF_sw(IF_lss',LS.SWITCH_E,sw,U_set,lss)
	  | IF_lss'((ls as LS.CCALL{name,args,rhos_for_result,res})::lss,U_set) = do_non_tail_call_if(ls,F_set,IF_lss'(lss,U_set))
	  | IF_lss'(ls::lss,U_set) =
	  let
	    val (def,use) = LineStmt.def_use_lvar_ls ls
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
	val (_,F_set,R_set) = F_lss(lss,Lvarset.empty,Lvarset.empty,Lvarset.empty)
	val F = lvset_delete(F_set,CallConv.get_spilled_args cc)
	val R = Lvarset.intersection(R_set,BI.callee_save_phregset)
	val R_list = Lvarset.members R
	val lss_iff = [LS.SCOPE{pat = mk_flushed_callee R_list,
				scope = insert_flush_callee(R_list,
							    IFF_lss(lss,F,insert_fetch_callee(R_list,[])))}]
	val (lss_if,_) = IF_lss(lss_iff,F)
      in
	gen_fn(lab,cc,lss_if)
      end
    fun IFF_top_decl(LineStmt.FUN(lab,cc,lss)) = do_top_decl LineStmt.FUN (lab,cc,lss)
      | IFF_top_decl(LineStmt.FN(lab,cc,lss)) = do_top_decl LineStmt.FN (lab,cc,lss)
  in
    fun IFF {main_lab:label,
	     code=ra_prg: (StoreTypeRA,unit,Atom) LinePrg,
	     imports:label list * label list,
	     exports:label list * label list} =
      let
	val _ = chat "[Insert Fetch and Flush..."
	val line_prg_iff = foldr (fn (func,acc) => IFF_top_decl func :: acc) [] ra_prg
	val _ = 
	  if Flags.is_on "print_fetch_and_flush_program" then
	    display("\nReport: AFTER INSERT FETCH AND FLUSH:", LineStmt.layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_iff)
	  else
	    ()
	val _ = chat "]\n"
      in
	{main_lab=main_lab,code=line_prg_iff: (StoreType,unit,Atom) LinePrg,imports=imports,exports=exports}
      end

  end

end;
