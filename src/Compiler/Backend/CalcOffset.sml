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
                        sharing type FetchAndFlush.phreg = LineStmt.phreg = Lvars.lvar
                        sharing type FetchAndFlush.Atom = LineStmt.Atom
		      structure BI : BACKEND_INFO
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
  type phreg = FetchAndFlush.phreg
  type offset = int
  type Atom = LineStmt.Atom

  datatype StoreType =
    STACK_STY of lvar * offset
  | PHREG_STY of lvar * phreg 
  | FLUSHED_CALLEE_STY of phreg * offset
  | FLUSHED_CALLER_STY of lvar * phreg * offset

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

  local
    structure LS = LineStmt
    structure IFF = FetchAndFlush

    structure LvarFinMap = Lvars.Map

    local
      val max_offset = ref 0
    in
      fun reset_max_offset() = (max_offset := BI.init_frame_offset)
      infix ++
      fun offset ++ inc =
	if offset+inc < !max_offset then
	  offset+inc
	else
	  (max_offset := offset+inc;
	   offset+inc)
      fun get_max_offset() = !max_offset
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
	  | assign_binder(((place,phsize as PhysSizeInf.WORDS i),_),offset) = (((place,phsize),offset),offset++i)
      in
	foldr (fn (binder,(acc,offset)) => 
	       let
		 val (binder',offset') = assign_binder(binder,offset)
	       in
		 (binder'::acc,offset')
	       end) ([],0) binders
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
      | CO_lss(LS.HANDLE{default,handl,handl_return,...}::lss,LVmap,PHmap,offset,acc) = 
      let
	val lss1' = CO_lss(default,LVmap,PHmap,offset++(BI.size_of_handle()),[])
	val lss2' = CO_lss(handl,LVmap,PHmap,offset++(BI.size_of_handle()),[])
	val handl_return' = CO_lss(handl_return,LVmap,PHmap,offset++(BI.size_of_handle()),[])
      in
	LS.HANDLE{default=lss1',handl=lss2',handl_return=handl_return',offset=offset++(BI.size_of_handle())}::CO_lss(lss,LVmap,PHmap,offset,acc)
      end
      | CO_lss(LS.RAISE a::lss,LVmap,PHmap,offset,acc) = LS.RAISE a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_I sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_I,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_S sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_S,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_C sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_C,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.SWITCH_E sw::lss,LVmap,PHmap,offset,acc) = CO_sw(CO_lss,LS.SWITCH_E,sw,LVmap,PHmap,offset) :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.RESET_REGIONS a::lss,LVmap,PHmap,offset,acc) = LS.RESET_REGIONS a :: CO_lss(lss,LVmap,PHmap,offset,acc)
      | CO_lss(LS.CCALL a::lss,LVmap,PHmap,offset,acc) = LS.CCALL a :: CO_lss(lss,LVmap,PHmap,offset,acc)

    (********************************)
    (* CO on Top level Declarations *)
    (********************************)
    fun CO_top_decl(LineStmt.FUN(lab,cc,lss)) = 
      let
	val _ = reset_max_offset()
	val LVmap_args = LvarFinMap.fromList (CallConv.get_spilled_args_with_offsets cc)
	val LVmap_res = LvarFinMap.addList (CallConv.get_spilled_res_with_offsets cc) LVmap_args
	val lss_co = CO_lss(lss,LVmap_res,LvarFinMap.empty,BI.init_frame_offset,[])
	val cc' = CallConv.add_frame_size(cc,get_max_offset())
      in
	LineStmt.FUN(lab,cc',lss_co)
      end
      | CO_top_decl(LineStmt.FN(lab,cc,lss)) = 
      let
	val _ = reset_max_offset()
	val LVmap_args = LvarFinMap.fromList (CallConv.get_spilled_args_with_offsets cc)
	val LVmap_res = LvarFinMap.addList (CallConv.get_spilled_res_with_offsets cc) LVmap_args
	val lss_co = CO_lss(lss,LVmap_res,LvarFinMap.empty,BI.init_frame_offset,[])
	val cc' = CallConv.add_frame_size(cc,get_max_offset())
      in
	LineStmt.FN(lab,cc',lss_co)
      end
  in
    fun CO {main_lab:label,
	     code=iff_prg: (StoreTypeIFF,unit,Atom) LinePrg,
	     imports:label list,
	     exports:label list} =
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

end;
