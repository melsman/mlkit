(* Doubles are not aligned yet! 18/12/1998, Niels *)

functor CalcOffset(structure PhysSizeInf : PHYS_SIZE_INF
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
		      structure FetchAndFlush: FETCH_AND_FLUSH
                        sharing type FetchAndFlush.lvar = LineStmt.lvar
                        sharing type FetchAndFlush.Atom = LineStmt.Atom
		      structure BI : BACKEND_INFO
		      structure IntSet: KIT_MONO_SET where type elt = int
		      structure PP : PRETTYPRINT
		        sharing type PP.StringTree = 
                                   Effect.StringTree = 
				   LineStmt.StringTree
                      structure Flags : FLAGS
		      structure Report : REPORT
		        sharing type Report.Report = Flags.Report
		      structure Crash : CRASH) : CALC_OFFSET =
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
  type StoreTypeIFF = FetchAndFlush.StoreType
  type offset = int
  type Atom = LineStmt.Atom

  datatype StoreType =
    STACK_STY of lvar * offset
  | PHREG_STY of lvar * lvar 
  | FLUSHED_CALLEE_STY of lvar * offset
  | FLUSHED_CALLER_STY of lvar * lvar * offset

  fun pr_sty(STACK_STY(lv,offset)) = Lvars.pr_lvar lv ^ ":stack(" ^ Int.toString offset ^ ")"
    | pr_sty(PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ LineStmt.pr_phreg phreg
    | pr_sty(FLUSHED_CALLEE_STY(phreg,offset)) = LineStmt.pr_phreg phreg ^ ":flushed_callee(" ^ Int.toString offset ^ ")"
    | pr_sty(FLUSHED_CALLER_STY(lv,phreg,offset)) = Lvars.pr_lvar lv ^ ":flushed " ^ LineStmt.pr_phreg phreg ^ ":stack(" ^ Int.toString offset ^ ")"

  fun pr_offset offset = "stack(" ^ Int.toString offset ^ ")"
  fun pr_atom atom = LineStmt.pr_atom atom

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CalcOffset." ^ s)
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
    [("print_calc_offset_program", "print program with offsets in activation records inserted (LineStmt)", ref false)]

  (*********************)
  (* Calculate Offsets *)
  (*********************)
  local
    structure LS = LineStmt
    structure IFF = FetchAndFlush

    structure LvarFinMap = Lvars.Map

    local
      val max_offset = ref 0
      val is_frame_db_aligned = ref true
    in
      fun reset_max_offset() = (max_offset := BI.init_frame_offset)
      fun set_frame_db_alignment t = is_frame_db_aligned := t
      infix ++
      fun offset ++ inc =
	if offset+inc < !max_offset then
	  offset+inc
	else
	  (max_offset := offset+inc;
	   offset+inc)
      fun get_max_offset() = 
	(case !is_frame_db_aligned of
	   true =>
	     (if !max_offset mod 2 = 0 then
		!max_offset
	      else
		!max_offset+1)
	 | false => 
	     (if !max_offset mod 2 = 0 then
		!max_offset+1
	      else
		!max_offset))
      fun double_align_offset offset =
	(case !is_frame_db_aligned of
	   true =>
	     (if offset mod 2 = 0 then
		offset
	      else
		offset++1)
	 | false => 
	     (if offset mod 2 = 0 then
		offset++1
	      else
		offset))
    end

    fun lookup_lv(lv_map,lv) =
      case LvarFinMap.lookup lv_map lv of
	SOME r => r
      | NONE  => die ("lookup_lv(" ^ (Lvars.pr_lvar lv) ^ ")")
    fun add_lv(lv_map,r,lv) = LvarFinMap.add(lv,r,lv_map)

    fun assign_offset_atom(LS.VAR lv,LVmap,PHmap) = lookup_lv(LVmap,lv)  
      | assign_offset_atom(LS.PHREG phreg,LVmap,PHmap) = lookup_lv(PHmap,phreg)
      | assign_offset_atom _ = die "assign_offset_atom: not a VAR or PHREG."

    fun assign_binders(binders,offset) =
      let
	fun assign_binder(((place,phsize as PhysSizeInf.INF),_),offset) = (((place,phsize),offset),offset++BI.size_of_reg_desc())
	  | assign_binder(((place,phsize as PhysSizeInf.WORDS i),_),offset) = 
	  if LS.is_region_real place then
	    let
	      val offset' = double_align_offset offset
	    in
	      (((place,phsize),offset'),offset'++(i)) (* Double Align finite regions with runtime type REAL *) (*+2 necessary for tagging hack 11/01/1999, Niels hmm we must correct PSI such that this isn't necessary, function psi_tr *)
	    end
	  else
	    (((place,phsize),offset),offset++(i+1)) (* +1 necessary for tagging hack 11/01/1999, Niels we should correct this in PSI. function psi_tr *)
      in
	foldr (fn (binder,(acc,offset)) => 
	       let
		 val (binder',offset') = assign_binder(binder,offset)
	       in
		 (binder'::acc,offset')
	       end) ([],offset) binders
      end

    fun assign_stys(stys,LVmap,PHmap,offset) =
      let
	fun assign_sty(IFF.STACK_STY lv,LVmap,PHmap,offset) = (STACK_STY(lv,offset),add_lv(LVmap,offset,lv),PHmap,offset++1)
	  | assign_sty(IFF.PHREG_STY(lv,phreg),LVmap,PHmap,offset) = (PHREG_STY(lv,phreg),LVmap,PHmap,offset)
	  | assign_sty(IFF.FLUSHED_CALLEE_STY(phreg),LVmap,PHmap,offset) = (FLUSHED_CALLEE_STY(phreg,offset),LVmap,add_lv(PHmap,offset,phreg),offset++1)
	  | assign_sty(IFF.FLUSHED_CALLER_STY(lv,phreg),LVmap,PHmap,offset) = (FLUSHED_CALLER_STY(lv,phreg,offset),add_lv(LVmap,offset,lv),PHmap,offset++1)
      in
	foldl (fn (sty,(stys_acc,LVmap,PHmap,offset)) =>
	       let
		 val (sty',LVmap',PHmap',offset') = assign_sty(sty,LVmap,PHmap,offset)
	       in
		 (sty'::stys_acc,LVmap',PHmap',offset')
	       end) ([],LVmap,PHmap,offset) stys
      end

    fun CO_sw(CO_lss,switch_con,LS.SWITCH(atom,sels,default),LVmap,PHmap,offset) =
      let
	val sels' =
	  foldr (fn ((sel,lss),sels_acum) => (sel,CO_lss(lss,LVmap,PHmap,offset,[]))::sels_acum) [] sels
	val default' = CO_lss(default,LVmap,PHmap,offset,[])
      in
	switch_con(LS.SWITCH(atom,sels',default'))
      end

    fun CO_lss([],LVmap,PHmap,offset,acc) = acc
      | CO_lss(LS.ASSIGN a::lss,LVmap,PHmap,offset,acc) = LS.ASSIGN a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.FLUSH(atom,_)::lss,LVmap,PHmap,offset,acc) = LS.FLUSH(atom,assign_offset_atom(atom,LVmap,PHmap))::CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.FETCH(atom,_)::lss,LVmap,PHmap,offset,acc) = LS.FETCH(atom,assign_offset_atom(atom,LVmap,PHmap))::CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.FNJMP a::lss,LVmap,PHmap,offset,acc) = LS.FNJMP a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.FNCALL a::lss,LVmap,PHmap,offset,acc) = LS.FNCALL a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.JMP a::lss,LVmap,PHmap,offset,acc) = LS.JMP a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.FUNCALL a::lss,LVmap,PHmap,offset,acc) = LS.FUNCALL a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.LETREGION{rhos,body}::lss,LVmap,PHmap,offset,acc) = 
      let
	val (binders',offset') = assign_binders(rhos,offset)
      in
	LS.LETREGION{rhos=binders',body=CO_lss(body,LVmap,PHmap,offset',[])} :: CO_lss(lss,LVmap,PHmap,offset,acc)
      end
      | CO_lss(LS.SCOPE{pat,scope}::lss,LVmap,PHmap,offset,acc) = 
      let
	val (pat',LVmap',PHmap',offset') = assign_stys(pat,LVmap,PHmap,offset)
      in
	LS.SCOPE{pat=pat',scope=CO_lss(scope,LVmap',PHmap',offset',[])} :: CO_lss(lss,LVmap,PHmap,offset,acc)
      end
      | CO_lss(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_lv,bv),...}::lss,LVmap,PHmap,offset,acc) = 
      let
	val (handl',size_handl) = (CO_lss(handl,LVmap,PHmap,offset++(BI.size_of_handle()),[]),0)
	val (default',size_default) = (CO_lss(default,LVmap,PHmap,offset++(BI.size_of_handle()),[]),0)
	val handl_return' = CO_lss(handl_return,LVmap,PHmap,offset(*++(BI.size_of_handle())*),[])
      in
	LS.HANDLE{default=default',handl=(handl',handl_lv),handl_return=(handl_return',handl_return_lv,bv),offset=offset}::CO_lss(lss,LVmap,PHmap,offset,acc)
      end
      | CO_lss(LS.RAISE a::lss,LVmap,PHmap,offset,acc) = LS.RAISE a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_I sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_I,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_S sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_S,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_C sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_C,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_E sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_E,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.RESET_REGIONS a::lss,LVmap,PHmap,offset,acc) = LS.RESET_REGIONS a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.PRIM a::lss,LVmap,PHmap,offset,acc) = LS.PRIM a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.CCALL a::lss,LVmap,PHmap,offset,acc) = LS.CCALL a :: CO_lss(lss,LVmap,PHmap,offset,acc)

    (********************************)
    (* CO on Top level Declarations *)
    (********************************)
    fun do_top_decl gen_fn (lab,cc,lss) =
      (* We preserve the invariant that sp is always double aligned after an activation record. *)
      let
	val _ = reset_max_offset()
	val size_cc = CallConv.get_cc_size cc
	(* If size_cc is odd then the frame is _not_ double aligned *)
	val _ = 
	  if size_cc mod 2 = 0 then
	    set_frame_db_alignment true
	  else
	    set_frame_db_alignment false
	val LVmap_args = LvarFinMap.fromList (CallConv.get_spilled_args_with_offsets cc)
	val LVmap_res = LvarFinMap.addList (CallConv.get_spilled_res_with_offsets cc) LVmap_args
	val lss_co = CO_lss(lss,LVmap_res,LvarFinMap.empty,BI.init_frame_offset,[])
	val cc' = CallConv.add_frame_size(cc,get_max_offset())
      in
	gen_fn(lab,cc',lss_co)
      end

    fun CO_top_decl(LineStmt.FUN(lab,cc,lss)) = do_top_decl LineStmt.FUN (lab,cc,lss)
      | CO_top_decl(LineStmt.FN(lab,cc,lss)) = do_top_decl LineStmt.FN (lab,cc,lss)
  in
    fun CO {main_lab:label,
	     code=iff_prg: (StoreTypeIFF,unit,Atom) LinePrg,
	     imports:label list * label list,
	     exports:label list * label list} =
      let
	val _ = chat "[Calculate Offsets..."
	val line_prg_co = foldr (fn (func,acc) => CO_top_decl func :: acc) [] iff_prg
	val _ = 
	  if Flags.is_on "print_calc_offset_program" then
	    display("\nReport: AFTER CALCULATING OFFSETS IN ACTIVATION RECORDS:", LineStmt.layout_line_prg pr_sty pr_offset pr_atom false line_prg_co)
	  else
	    ()
	val _ = chat "]\n"
      in
	{main_lab=main_lab,code=line_prg_co: (StoreType,offset,Atom) LinePrg,imports=imports,exports=exports}
      end
  end

  (************************)
  (* Calculate BitVectors *)
  (************************)
  local
    structure LS = LineStmt
    structure LvarFinMap = Lvars.Map

    local 
      val local_fun_nr = ref 0
    in
      fun reset_fun_nr() = local_fun_nr := 0
      fun new_fun_nr() = (local_fun_nr := !local_fun_nr+1; Word32.fromInt (!local_fun_nr-1))
    end

    (* We have an environment LVenv mapping variables to their offsets in the activation record          *)
    (* -- A spilled variable may be live at an application                                               *)
    (* -- A variable mapped into a caller save register and never flused is never live at an application *)
    (* -- A variable mapped into a caller save register and flused may be live at an application         *)
    (* -- A variable mapped into a callee save register is not considered yet!                           *)
    fun add_sty(STACK_STY(lv,offset),LVmap) = LvarFinMap.add(lv,offset,LVmap) 
      | add_sty(PHREG_STY(lv,phreg),LVmap) = LVmap 
      | add_sty(FLUSHED_CALLEE_STY(phreg,offset),LVmap) = LVmap
      | add_sty(FLUSHED_CALLER_STY(lv,phreg,offset),LVmap) = LvarFinMap.add(lv,offset,LVmap)

    fun add_stys([],LVmap) = LVmap
      | add_stys(sty::rest,LVmap) = add_stys(rest,add_sty(sty,LVmap))

    fun lookup_lvs(LVmap,[]) = []
      | lookup_lvs(LVmap,lv::lvs) =
      case LvarFinMap.lookup LVmap lv of
	SOME r => r::lookup_lvs(LVmap,lvs)
      | NONE  => lookup_lvs(LVmap,lvs)

    fun lvset_difference(lv_set,lv_list,LVmap) = foldr (fn (offset,set) => IntSet.remove offset set) lv_set (lookup_lvs(LVmap,lv_list))
    fun lvset_add(lv_set,lv_list,LVmap) = foldr (fn (offset,set) => IntSet.insert offset set) lv_set (lookup_lvs(LVmap,lv_list))

    fun gen_bitvector(L_set,size_ff) =
      let
	val w0 = Word32.fromInt 0
	fun pw w = print ("Word is " ^ (Word32.fmt StringCvt.BIN w) ^ "\n")
	fun pws ws = app pw ws
	fun set_bit(bit_no,w) = Word32.orb(w,Word32.<<(Word32.fromInt 1,Word.fromInt bit_no))

	val offsets_in_bitvector_sorted = ListSort.sort (fn i1 => (fn i2 => i1 < i2)) (IntSet.list L_set)

	fun gen_words([],adjust,word) = [word]
	  | gen_words(offset::offsets,adjust,word) =
	  if (offset-adjust<32) then
	    gen_words(offsets,adjust,set_bit(offset-adjust,word))
	  else
	    word::gen_words(offset::offsets,adjust+32,Word32.fromInt 0)

	val num_words = 
	  if size_ff mod 32 = 0 then
	    size_ff div 32
	  else
	    (size_ff div 32)+1
	fun postfix_words l =
	  if length l < num_words then
	    postfix_words(l@[Word32.fromInt 0])
	  else
	    l

	val ws = postfix_words(gen_words(offsets_in_bitvector_sorted,0,w0))
(*	val _ = app (fn off => print ("Offset " ^ Int.toString off ^ "\n")) offsets_in_bitvector_sorted
	val _ = pws ws
	val _ = print ("size_ff is " ^ Int.toString size_ff ^ " and num_words is " ^ Int.toString num_words ^ "\n")*)
      in
        new_fun_nr() :: (Word32.fromInt size_ff) :: ws
      end

    fun CBV_sw(CBV_lss,gen_sw,LS.SWITCH(atom,sels,default),L_set,LVenv,lss) =
      let
	val (L_set',lss') = CBV_lss(lss,LVenv,L_set)
	val (sels',L_set_sels) = 
	  foldr 
	  (fn ((sel,lss),(sels_acc,L_set_acc)) => 
	   let 
	     val (L_set_lss',lss') = CBV_lss(lss,LVenv,L_set)
	   in 
	     ((sel,lss')::sels_acc,IntSet.union L_set_acc L_set_lss')
	   end) ([],IntSet.empty) sels

	val (L_set_def,default') = CBV_lss(default,LVenv,L_set)
      in
	(lvset_add(IntSet.union L_set_def L_set_sels,LS.get_lvar_atom(atom,[]),LVenv),
	 gen_sw(LS.SWITCH(atom,sels',default'))::lss')
      end

    fun CBV_lss (lss,size_ff) =
      let 
	fun CBV_lss'([],LVenv,L_set) = (L_set,[])
	  | CBV_lss'(ls::lss,LVenv,L_set) =
	  (case ls of
	     LS.FNJMP(cc as {opr,args,clos,free,res,bv}) => 
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = LineStmt.def_use_lvar_ls ls
		 val lvset_kill_def = lvset_difference(L_set',def,LVenv)
		 val bit_vector = gen_bitvector(lvset_kill_def,size_ff)
	       in
		(lvset_add(lvset_kill_def,use,LVenv),
		 LS.FNJMP{opr=opr,args=args,clos=clos,free=free,res=res,bv=bit_vector}::lss')
	       end
	   | LS.FNCALL(cc as {opr,args,clos,free,res,bv}) =>
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = LineStmt.def_use_lvar_ls ls
		 val lvset_kill_def = lvset_difference(L_set',def,LVenv)

(*		 val _ = print (LS.pr_line_stmt pr_sty pr_offset pr_atom false ls)
		 val _ = print "\n"*)
		 val bit_vector = gen_bitvector(lvset_kill_def,size_ff)
	       in
		(lvset_add(lvset_kill_def,use,LVenv),
		 LS.FNCALL{opr=opr,args=args,clos=clos,free=free,res=res,bv=bit_vector}::lss')
	       end
	  | LS.JMP(cc as {opr,args,reg_vec,reg_args,clos,free,res,bv}) =>
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = LineStmt.def_use_lvar_ls ls
		 val lvset_kill_def = lvset_difference(L_set',def,LVenv)
		 val bit_vector = gen_bitvector(lvset_kill_def,size_ff)
	       in
		(lvset_add(lvset_kill_def,use,LVenv),
		 LS.JMP{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,free=free,res=res,bv=bit_vector}::lss')
	       end
	   | LS.FUNCALL(cc as {opr,args,reg_vec,reg_args,clos,free,res,bv}) =>
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = LineStmt.def_use_lvar_ls ls
		 val lvset_kill_def = lvset_difference(L_set',def,LVenv)
		 val bit_vector = gen_bitvector(lvset_kill_def,size_ff)
	       in
		(lvset_add(lvset_kill_def,use,LVenv),
		 LS.FUNCALL{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,free=free,res=res,bv=bit_vector}::lss')
	       end
	   | LS.LETREGION{rhos,body} => 
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (L_set_letregion,body') = CBV_lss'(body,LVenv,L_set')
	       in
		 (L_set_letregion,
		  LS.LETREGION{rhos=rhos,body=body'}::lss')
	       end
	   | LS.SCOPE{pat,scope} =>
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (L_set_scope,scope') = CBV_lss'(scope,add_stys(pat,LVenv),L_set')
	       in
		 (L_set_scope,
		  LS.SCOPE{pat=pat,scope=scope'}::lss')
	       end
	   | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_lv,bv),offset} =>
	       (* Pointer to handle closure is at offset+1, see CodeGen.sml *)
	       let
		 val (L_set',lss') = CBV_lss'(lss,LVenv,L_set)
		 val (L_set_handl_return,handl_return') = CBV_lss'(handl_return,LVenv,
								   lvset_difference(L_set',LS.get_lvar_atom(handl_return_lv,[]),LVenv))            (* Handler is dead in handlreturn code *)
		 val bv_handl_return = gen_bitvector(L_set_handl_return,size_ff)
		 val (L_set_default,default') = CBV_lss'(default,LVenv,IntSet.insert (offset+1) L_set')  (* Handler is live in default code *)
		 val (L_set_handl,handl') = CBV_lss'(handl,LVenv,IntSet.remove (offset+1) L_set_default) (* Handler is dead in handl code *)
	       in
		 ((*IntSet.insert (offset+1)*) L_set_handl ,
		  LS.HANDLE{default=default',
			    handl=(handl',
				   handl_lv),
			    handl_return=(handl_return',
					  handl_return_lv,
					  bv_handl_return),
			    offset=offset}::lss')
	       end
	   | LS.SWITCH_I sw => CBV_sw(CBV_lss',LS.SWITCH_I,sw,L_set,LVenv,lss)
	   | LS.SWITCH_S sw => CBV_sw(CBV_lss',LS.SWITCH_S,sw,L_set,LVenv,lss)
	   | LS.SWITCH_C sw => CBV_sw(CBV_lss',LS.SWITCH_C,sw,L_set,LVenv,lss)
	   | LS.SWITCH_E sw => CBV_sw(CBV_lss',LS.SWITCH_E,sw,L_set,LVenv,lss)
	   | LS.FLUSH(atom,_) => 
	       let (* We define the stack slot *)
		 val (L_set,lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = (LineStmt.get_var_atom(atom,[]),[])
	       in
		 (lvset_add(lvset_difference(L_set,def,LVenv),use,LVenv),
		  ls::lss')
	       end

	   | LS.FETCH(atom,_) =>
	       let (* We use the stack slot (i.e., the stack slot is live) *)
		 val (L_set,lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = ([],LineStmt.get_var_atom(atom,[]))
	       in
		 (lvset_add(lvset_difference(L_set,def,LVenv),use,LVenv),
		  ls::lss')
	       end
	   | _ => 
	       let
		 val (L_set,lss') = CBV_lss'(lss,LVenv,L_set)
		 val (def,use) = LineStmt.def_use_lvar_ls ls
	       in
		 (lvset_add(lvset_difference(L_set,def,LVenv),use,LVenv),
		  ls::lss')
	       end)
      in
	#2(CBV_lss'(lss,LvarFinMap.empty,IntSet.empty))
      end

    (*********************************)
    (* CBV on Top level Declarations *)
    (*********************************)
    fun do_top_decl gen_fn (lab,cc,lss) =
      let
	val size_cc = CallConv.get_cc_size cc
	val _ = if size_cc <> 1 then die "do_top_decl: CC not of size 1" else () (* For now we assume all variables are passed in machine registers *)
	val size_ff = CallConv.get_frame_size cc
	val lss_cbv = CBV_lss(lss,size_ff)
      in
	gen_fn(lab,cc,lss_cbv)
      end

    fun CBV_top_decl(LineStmt.FUN(lab,cc,lss)) = do_top_decl LineStmt.FUN (lab,cc,lss)
      | CBV_top_decl(LineStmt.FN(lab,cc,lss)) = do_top_decl LineStmt.FN (lab,cc,lss)

  in
    fun CBV {main_lab:label,
	     code=co_prg: (StoreType,offset,Atom) LinePrg,
	     imports:label list * label list,
	     exports:label list * label list} =
      let
	val _ = chat "[Calculate BitVectors..."
	val _ = reset_fun_nr()
	val line_prg_cbv = foldr (fn (func,acc) => CBV_top_decl func :: acc) [] co_prg
	val _ = chat "]\n"
      in
	{main_lab=main_lab,code=line_prg_cbv: (StoreType,offset,Atom) LinePrg,imports=imports,exports=exports}
      end

  end

end;


(*    fun lvset_add(lv_set,lv_list) = (app (fn lv => Array.update(lv_set,lv,true)) lv_list;lv_set)
    fun lvset_delete(lv_set,lv_list) = (app (fn lv => Array.update(lv_set,lv,false)) lv_list;lv_set)
    fun lvset_difference(lv_set,lv_list) = (app (fn lv => Array.update(lv_set,lv,false)) lv_list;lv_set)
*)

