functor CodeGen(structure PhysSizeInf : PHYS_SIZE_INF
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
	        structure SubstAndSimplify: SUBST_AND_SIMPLIFY
                  sharing type SubstAndSimplify.lvar = LineStmt.lvar
                  sharing type SubstAndSimplify.place = LineStmt.place
                  sharing type SubstAndSimplify.LinePrg = LineStmt.LinePrg
	        structure HpPaRisc : HP_PA_RISC
                  sharing type HpPaRisc.label = Labels.label
	        structure BI : BACKEND_INFO
                  sharing type BI.reg = SubstAndSimplify.reg = HpPaRisc.reg
	        structure PP : PRETTYPRINT
		  sharing type PP.StringTree = 
		               Effect.StringTree = 
			       LineStmt.StringTree =
                               HpPaRisc.StringTree
		structure Flags : FLAGS
	        structure Report : REPORT
		  sharing type Report.Report = Flags.Report
		structure Crash : CRASH) : CODE_GEN =
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
  type StoreTypeSS = SubstAndSimplify.StoreTypeCO
  type AtySS = SubstAndSimplify.Aty
  type reg = HpPaRisc.reg
  type offset = int
  type RiscPrg = HpPaRisc.RiscPrg

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CodeGen(HP-PARISC)." ^ s)
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
    [("print_HP-PARISC_program", "print HP-PARISC program", ref false)]

  (********************************)
  (* CG on Top Level Declarations *)
  (********************************)
  local
    open HpPaRisc
    structure SS = SubstAndSimplify
    structure LS = LineStmt

    (* Labels Local To This Compilation Unit *)
    fun new_local_lab name = LocalLab (Labels.new_named name)
    local
      val counter = ref 0
      fun incr() = (counter := !counter + 1; !counter)
    in
      fun new_string_lab() : lab = NameLab("StringLab" ^ Int.toString(incr()))
      fun new_float_lab() : lab = NameLab("FloatLab" ^ Int.toString(incr()))
      fun reset_label_counter() = counter := 0
    end

    (* Static Data *)
    local
      val static_data : RiscInst list ref = ref []
    in
      fun add_static_data (insts) = (static_data := insts @ !static_data)
      fun reset_static_data () = static_data := []
      fun get_static_data() = !static_data
    end

    (* Convert ~n to -n *)
    fun int_to_string i =
      if i >= 0 then Int.toString i
      else "-" ^ Int.toString (~i)

    (* We make the offset base explicit in the following functions *)
    datatype Offset = 
        WORDS of int 
      | BYTES of int
      | IMMED of int

    (* Can be used to load from the stack or from a record *)     
    (* dst = base[x]                                       *)
    (* Kills Gen 1                                         *)
    fun load_indexed(dst_reg:reg,base_reg:reg,offset:Offset,C) =
      let
	val x = 
	  case offset of
	    BYTES x => x
	  | WORDS x => x*4
	  | _ => die "load_indexed: offset not in BYTES or WORDS"
      in
	if is_im14 x then
	  LDW{d=int_to_string x,s=Space 0,b=base_reg,t=dst_reg} :: C
	else
	  ADDIL{i="L'" ^ int_to_string x,r=base_reg} ::
	  LDW{d="R'" ^ int_to_string x,s=Space 0,b=Gen 1,t=dst_reg} :: C
      end

    (* Can be used to update the stack or store in a record *)
    (* base[x] = src                                        *)
    (* Kills Gen 1                                          *)
    fun store_indexed(base_reg:reg,offset:Offset,src_reg:reg,C) =
      let
	val x =
	  case offset of
	    BYTES x => x
	  | WORDS x => x*4
	  | _ => die "store_indexed: offset not in BYTES or WORDS"
      in
	if is_im14 x then
	  STW {r=src_reg,d=int_to_string x,s=Space 0,b=base_reg} :: C
	else
	  ADDIL {i="L'" ^ int_to_string x,r=base_reg} ::
	  STW {r=src_reg,d="R'" ^ int_to_string x,s=Space 0,b=Gen 1} :: C
      end

    (* Calculate an addres given a base and an offset *)
    (* dst = base + x                                 *)
    (* Kills Gen 1                                    *)
    fun base_plus_offset(base_reg:reg,offset:Offset,dst_reg:reg,C) =
      let
	val x = 
	  case offset of
	    BYTES x => x
	  | WORDS x => x*4
	  | _ => die "base_plus_offset: offset not in BYTES or WORDS"
      in
	if is_im14 x then
	  LDO {d=int_to_string x,b=base_reg,t=dst_reg} :: C
	else
	  ADDIL {i="L'" ^ int_to_string x,r=base_reg} ::
	  LDO {d="R'" ^ int_to_string x,b=Gen 1,t=dst_reg} :: C
      end

    (* Load a constant *)
    (* dst = x         *)
    (* Kills no regs.  *)
    fun load_immed(IMMED x,dst_reg:reg,C) =
      if is_im14 x then 
	LDI {i=int_to_string x, t=dst_reg} :: C
      else 
	LDIL {i="L'" ^ int_to_string x, t=dst_reg} ::
	LDO {d="R'" ^ int_to_string x,b=dst_reg,t=dst_reg} :: C
      | load_immed _ = die "load_immed: immed not in IMMED"

    (* Find a register for aty and generate code to store into the aty *)
    fun resolve_aty_def(SS.STACK_ATY offset,t:reg,size_ff,C) = (t,store_indexed(sp,WORDS(~size_ff+offset),t,C))
      | resolve_aty_def(SS.PHREG_ATY phreg,t:reg,size_ff,C)  = (phreg,C)
      | resolve_aty_def _ = die "resolve_aty_def: ATY cannot be defined"

    (* Make sure that the aty ends up in register dst_reg *)
    fun move_aty_into_reg(SS.REG_I_ATY offset,dst_reg,size_ff,C) = base_plus_offset(sp,BYTES(~size_ff*4+offset*4+BI.inf_bit),dst_reg,C)
      | move_aty_into_reg(SS.REG_F_ATY offset,dst_reg,size_ff,C) = base_plus_offset(sp,WORDS(~size_ff+offset),dst_reg,C)
      | move_aty_into_reg(SS.STACK_ATY offset,dst_reg,size_ff,C) = load_indexed(dst_reg,sp,WORDS(~size_ff+offset),C)
      | move_aty_into_reg(SS.DROPPED_RVAR_ATY,dst_reg,size_ff,C) = C
      | move_aty_into_reg(SS.PHREG_ATY phreg,dst_reg,size_ff,C)  = if dst_reg = phreg then C else COPY{r=phreg,t=dst_reg} :: C
      | move_aty_into_reg(SS.INTEGER_ATY i,dst_reg,size_ff,C)    = load_immed(IMMED i,dst_reg,C)
      | move_aty_into_reg(SS.UNIT_ATY,dst_reg,size_ff,C)         = load_immed(IMMED BI.ml_unit,dst_reg,C)

    (* dst_aty = src_reg *)
    fun move_reg_into_aty(src_reg:reg,dst_aty,size_ff,C) =
      case dst_aty of
	SS.PHREG_ATY dst_reg => 
	  if src_reg = dst_reg then
	    C
	  else 
	    COPY{r=src_reg,t=dst_reg} :: C
      | SS.STACK_ATY offset => store_indexed(sp,WORDS(~size_ff+offset),src_reg,C) 
      | _ => die "move_reg_into_aty: ATY not recognized"

    (* dst_aty = src_aty *)
    fun move_aty_to_aty(SS.PHREG_ATY src_reg,dst_aty,size_ff,C) = move_reg_into_aty(src_reg,dst_aty,size_ff,C)
      | move_aty_to_aty(src_aty,SS.PHREG_ATY dst_reg,size_ff,C) = move_aty_into_reg(src_aty,dst_reg,size_ff,C)
      | move_aty_to_aty(src_aty,dst_aty,size_ff,C) = 
      let
	val (reg_for_result,C') = resolve_aty_def(dst_aty,tmp_reg1,size_ff,C)
      in
	move_aty_into_reg(src_aty,reg_for_result,size_ff,C')
      end

    (* dst_aty = src_aty[offset] *)
    fun move_index_aty_to_aty(SS.PHREG_ATY src_reg,SS.PHREG_ATY dst_reg,offset:Offset,size_ff,C) = 
      load_indexed(dst_reg,src_reg,offset,C)
      | move_index_aty_to_aty(SS.PHREG_ATY src_reg,dst_aty,offset:Offset,size_ff,C) = 
      load_indexed(tmp_reg1,src_reg,offset,
		   move_reg_into_aty(tmp_reg1,dst_aty,size_ff,C))
      | move_index_aty_to_aty(src_aty,dst_aty,offset,size_ff,C) =
      move_aty_into_reg(src_aty,tmp_reg1,size_ff,
			load_indexed(tmp_reg1,tmp_reg1,offset,
				     move_reg_into_aty(tmp_reg1,dst_aty,size_ff,C)))
		   
    (* dst_aty = &lab *)
    (* Kills Gen 1    *)
    fun load_label_addr(lab,dst_aty,size_ff,C) = 
      let
	val (reg_for_result,C') = resolve_aty_def(dst_aty,tmp_reg1,size_ff,C)
      in
	ADDIL{i="L'" ^ pp_lab lab ^ "-$global$",r=dp} ::
	LDO{d="R'" ^ pp_lab lab ^ "-$global$",b=Gen 1,t=reg_for_result} :: C'
      end

    (* dst_aty = lab[0] *)
    (* Kills Gen 1      *)
    fun load_from_label(lab,dst_aty,size_ff,C) =
      let
	val (reg_for_result,C') = resolve_aty_def(dst_aty,tmp_reg1,size_ff,C)
      in
	ADDIL{i="L'" ^ Labels.pr_label lab ^ "-$global$",r=dp} ::
	LDO{d="R'" ^ Labels.pr_label lab ^ "-$global$",b=Gen 1,t=reg_for_result} :: 
	LDW{d="0",s=Space 0,b=reg_for_result,t=reg_for_result} :: C'
      end

    (* lab[0] = src_aty *)
    (* Kills Gen 1      *)
    fun store_in_label(SS.PHREG_ATY src_reg,label,size_ff,C) =
      ADDIL{i="L'" ^ Labels.pr_label label ^ "-$global$",r=dp} ::
      LDO{d="R'" ^ Labels.pr_label label ^ "-$global$",b=Gen 1,t=tmp_reg1} :: 
      STW{r=src_reg,d="0",s=Space 0,b=tmp_reg1} :: C
      | store_in_label(src_aty,label,size_ff,C) =
      move_aty_into_reg(src_aty,tmp_reg2,size_ff,
			ADDIL{i="L'" ^ Labels.pr_label label ^ "-$global$",r=dp} ::
			LDO{d="R'" ^ Labels.pr_label label ^ "-$global$",b=Gen 1,t=tmp_reg1} :: 
			STW{r=tmp_reg2,d="0",s=Space 0,b=tmp_reg1} :: C)


    (* Can be used to update the stack or a record when the argument is an ATY *)
    (* base_reg[offset] = src_aty *)
    fun store_aty_in_reg_record(SS.PHREG_ATY src_reg,tmp_reg:reg,base_reg,offset:Offset,size_ff,C) =
      store_indexed(base_reg,offset,src_reg,C)
      | store_aty_in_reg_record(src_aty,tmp_reg:reg,base_reg,offset:Offset,size_ff,C) =
      move_aty_into_reg(src_aty,tmp_reg,size_ff,
		  store_indexed(base_reg,offset,tmp_reg,C))

    (* Can be used to load form the stack or a record when destination is an ATY *)
    (* dst_aty = base_reg[offset] *)
    fun load_aty_from_reg_record(SS.PHREG_ATY dst_reg,tmp_reg:reg,base_reg,offset:Offset,size_ff,C) =
      load_indexed(dst_reg,base_reg,offset,C)
      | load_aty_from_reg_record(dst_aty,tmp_reg:reg,base_reg,offset:Offset,size_ff,C) =
      load_indexed(tmp_reg,base_reg,offset,
		   move_reg_into_aty(tmp_reg,dst_aty,size_ff,C))

    (* base_aty[offset] = src_aty *)
    fun store_aty_in_aty_record(SS.PHREG_ATY src_reg,SS.PHREG_ATY base_reg,offset:Offset,tmp_reg1,tmp_reg2,size_ff,C) =
      store_indexed(base_reg,offset,src_reg,C)
      | store_aty_in_aty_record(SS.PHREG_ATY src_reg,base_aty,offset:Offset,tmp_reg1,tmp_reg2,size_ff,C) =
      move_aty_into_reg(base_aty,tmp_reg2,size_ff,
			store_indexed(tmp_reg2,offset,src_reg,C))
      | store_aty_in_aty_record(src_aty,SS.PHREG_ATY base_reg,offset:Offset,tmp_reg1,tmp_reg2,size_ff,C) =
      move_aty_into_reg(src_aty,tmp_reg1,size_ff,
			store_indexed(base_reg,offset,tmp_reg1,C))
      | store_aty_in_aty_record(src_aty,base_aty,offset:Offset,tmp_reg1,tmp_reg2,size_ff,C) =
      move_aty_into_reg(src_aty,tmp_reg1,size_ff,
			move_aty_into_reg(base_aty,tmp_reg2,size_ff,
					  store_indexed(tmp_reg2,offset,tmp_reg1,C)))

    (***********************)
    (* Calling C Functions *)
    (***********************)
    (* Kills tmp_reg1 and tmp_reg2 *)
    fun align_stack C = (* MEGA HACK *)
      COPY {r=sp, t=tmp_reg1} ::
      load_immed(IMMED 60,tmp_reg2,
		 ANDCM{cond=NEVER,r1=tmp_reg2,r2=sp,t=tmp_reg2} ::
		 ADD{cond=NEVER,r1=tmp_reg2,r2=sp,t=sp} ::
		 STWM {r=tmp_reg1,d="1028",s=Space 0,b=sp} :: C)

    (* Kills no registers. *)      
    fun restore_stack C = LDW {d="-1028",s=Space 0,b=sp,t=sp} :: C

    fun compile_c_call_prim(name: string, args: SS.Aty list,opt_ret: SS.Aty option,size_ff: int,C) =
      let
	val (convert: bool,name: string) =
	  (case explode name of
	     #"@" :: rest => (!BI.tag_values, implode rest)
	   | _ => (false, name))

	fun convert_int_to_c(reg,C) =
	  if convert then 
	    SHD {cond=NEVER, r1=Gen 0, r2=reg, p="1" , t=reg} :: C
	  else 
	    C

	fun convert_int_to_ml(reg,C) =
	  if convert then 
	    SH1ADD {cond=NEVER, r1=reg, r2=Gen 0, t=reg} ::
	    LDO {d="1", b=reg, t=reg} :: C
	  else 
	    C

	fun arg_str(n,[]) = ""
	  | arg_str(n,[a]) = "ARGW" ^ Int.toString n ^ "=GR"
	  | arg_str(n,a::rest) = 
	  if n<3 then 
	    arg_str(n,[a]) ^ ", " ^ arg_str(n+1,rest)
	  else 
	    arg_str(n,[a]) 

	val call_str = arg_str(0,args) ^ 
	  (case opt_ret 
	     of SOME _ => (if length args > 0 then ", " else "") ^ "RTNVAL=GR" 
	      | NONE => "")

	fun fetch_args_ext([],_,C) = C
	  | fetch_args_ext(r::rs,offset,C) = 
	  move_aty_into_reg(r,tmp_reg1,size_ff,
			    convert_int_to_c(tmp_reg1,
					     STW{r=tmp_reg1,d="-" ^ Int.toString offset,s=Space 0,b=sp} :: fetch_args_ext(rs,offset+4,C)))

	fun fetch_args([],_,C) = align_stack C
	  | fetch_args(r::rs,ar::ars,C) = 
	  move_aty_into_reg(r,ar,size_ff,
			    convert_int_to_c(ar,
					     fetch_args(rs,ars,C)))
	  | fetch_args(rs,[],C) = fetch_args_ext(rs,52,align_stack C) (* arg4 is at offset sp-52 *)

	fun store_ret(SOME d,C) = 
	  convert_int_to_ml(ret0,
			    move_reg_into_aty(ret0,d,size_ff,C))
	  | store_ret(NONE,C) = C
      in
	fetch_args(args,[arg0, arg1, arg2, arg3],
		   META_BL{n=false,target=NameLab name,rpLink=rp,callStr=call_str} ::
		   restore_stack(store_ret(opt_ret,C)))
      end

    (*********************)
    (* Allocation Points *)
    (*********************)

    (* Status Bits Are Not Cleared *)
    fun reset_region(t:reg,size_ff,C) = compile_c_call_prim("resetRegion",[SS.PHREG_ATY t],NONE,size_ff,C)

    (* Status Bits Are Not Cleared *)
    fun alloc(t:reg,n:int,size_ff,C) = compile_c_call_prim("alloc",[SS.PHREG_ATY t,SS.INTEGER_ATY n],SOME(SS.PHREG_ATY t),size_ff,C)

    fun clear_status_bits(t,C) = DEPI{cond=NEVER,i="0",p="31",len="2",t=t}::C

    (* move_aty_into_reg_ap differs from move_aty_into_reg in the case where aty is a phreg! *)
    (* We must always make a copy of phreg because we may overwrite status bits in phreg.    *) 
    fun move_aty_into_reg_ap(SS.REG_I_ATY offset,dst_reg,size_ff,C) = base_plus_offset(sp,BYTES(~size_ff*4+offset*4+BI.inf_bit),dst_reg,C)
      | move_aty_into_reg_ap(SS.REG_F_ATY offset,dst_reg,size_ff,C) = base_plus_offset(sp,WORDS(~size_ff+offset),dst_reg,C)
      | move_aty_into_reg_ap(SS.STACK_ATY offset,dst_reg,size_ff,C) = load_indexed(dst_reg,sp,WORDS(~size_ff+offset),C)
      | move_aty_into_reg_ap(SS.PHREG_ATY phreg,dst_reg,size_ff,C)  = COPY {r=phreg,t=dst_reg} :: C
      | move_aty_into_reg_ap _ = die "move_aty_into_reg_ap: ATY cannot be used to allocate memory"

    fun alloc_ap(LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,n,size_ff,C) = C
      | alloc_ap(LS.IGNORE,dst_reg:reg,n,size_ff,C) = C

      | alloc_ap(LS.ATTOP_LI(aty,pp),dst_reg:reg,n,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,alloc(dst_reg,n,size_ff,C))
      | alloc_ap(LS.ATTOP_LF(aty,pp),dst_reg:reg,n,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C) (* status bits are not set *)
      | alloc_ap(LS.ATTOP_FI(aty,pp),dst_reg:reg,n,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,alloc(dst_reg,n,size_ff,C))
      | alloc_ap(LS.ATTOP_FF(aty,pp),dst_reg:reg,n,size_ff,C) = 
      let
	val default_lab = new_local_lab "no_alloc"
      in
	move_aty_into_reg_ap(aty,dst_reg,size_ff,
			     META_IF_BIT{r=dst_reg,bitNo=31,target=default_lab} ::
			     alloc(dst_reg,n,size_ff,LABEL default_lab :: C))
      end
      | alloc_ap(LS.ATBOT_LI(aty,pp),dst_reg:reg,n,size_ff,C) = 
      move_aty_into_reg_ap(aty,dst_reg,size_ff,
			   reset_region(dst_reg,size_ff,
					alloc(dst_reg,n,size_ff,C)))
      | alloc_ap(LS.ATBOT_LF(aty,pp),dst_reg:reg,n,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C) (* status bits are not set *)
      | alloc_ap(LS.SAT_FI(aty,pp),dst_reg:reg,n,size_ff,C) = 
      let
	val default_lab = new_local_lab "no_reset"
      in
	move_aty_into_reg_ap(aty,dst_reg,size_ff,
			     META_IF_BIT{r=dst_reg,bitNo=30,target=default_lab} ::
			     reset_region(dst_reg,size_ff,LABEL default_lab :: 
					  alloc(dst_reg,n,size_ff,C)))
      end
      | alloc_ap(LS.SAT_FF(aty,pp),dst_reg:reg,n,size_ff,C) = 
      let
	val default_lab = new_local_lab "no_reset"
      in
	move_aty_into_reg_ap(aty,dst_reg,size_ff,
			     META_IF_BIT{r=dst_reg,bitNo=31,target=default_lab} ::
			     META_IF_BIT{r=dst_reg,bitNo=30,target=default_lab} ::
			     reset_region(dst_reg,size_ff,LABEL default_lab :: 
					  alloc(dst_reg,n,size_ff,C)))
      end

    fun set_atbot_bit(dst_reg:reg,C) = DEPI{cond=NEVER, i="1", p="30", len="1", t=dst_reg} :: C
    fun clear_atbot_bit(dst_reg:reg,C) = DEPI{cond=NEVER, i="0", p="30", len="1", t=dst_reg} :: C

    fun store_sm_in_record(LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp),tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
      | store_sm_in_record(LS.IGNORE,tmp:reg,base_reg,offset,size_ff,C) = die "store_sm_in_record: IGNORE not implemented."
      | store_sm_in_record(LS.ATTOP_LI(SS.PHREG_ATY phreg,pp),tmp:reg,base_reg,offset,size_ff,C) = store_indexed(base_reg,offset,phreg,C)
      | store_sm_in_record(LS.ATTOP_LI(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      move_aty_into_reg_ap(aty,tmp,size_ff,
			   store_indexed(base_reg,offset,tmp,C))
      | store_sm_in_record(LS.ATTOP_LF(SS.PHREG_ATY phreg,pp),tmp:reg,base_reg,offset,size_ff,C) = store_indexed(base_reg,offset,phreg,C)
      | store_sm_in_record(LS.ATTOP_LF(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      move_aty_into_reg_ap(aty,tmp,size_ff,
			   store_indexed(base_reg,offset,tmp,C))
      | store_sm_in_record(LS.ATTOP_FI(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      move_aty_into_reg_ap(aty,tmp,size_ff,
			   clear_atbot_bit(tmp,
					   store_indexed(base_reg,offset,tmp,C)))
      | store_sm_in_record(LS.ATTOP_FF(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      move_aty_into_reg_ap(aty,tmp,size_ff,
			   clear_atbot_bit(tmp,
					   store_indexed(base_reg,offset,tmp,C)))
      | store_sm_in_record(LS.ATBOT_LI(SS.REG_I_ATY offset_reg_i,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      base_plus_offset(sp,BYTES(~size_ff*4+offset_reg_i*4+BI.inf_bit+BI.atbot_bit),tmp,
		       store_indexed(base_reg,offset,tmp,C))
      | store_sm_in_record(LS.ATBOT_LI(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = move_aty_into_reg_ap(aty,tmp,size_ff,set_atbot_bit(tmp,C))
      | store_sm_in_record(LS.ATBOT_LF(SS.PHREG_ATY phreg,pp),tmp:reg,base_reg,offset,size_ff,C) = store_indexed(base_reg,offset,phreg,C)
      | store_sm_in_record(LS.ATBOT_LF(aty,pp),tmp:reg,base_reg,offset,size_ff,C) = 
      move_aty_into_reg_ap(aty,tmp,size_ff,store_indexed(base_reg,offset,tmp,C))
      | store_sm_in_record(LS.SAT_FI(SS.PHREG_ATY phreg,pp),tmp:reg,base_reg,offset,size_ff,C) = store_indexed(base_reg,offset,phreg,C)
      | store_sm_in_record(LS.SAT_FI(aty,pp),tmp:reg,base_reg,offset,size_ff,C) =
      move_aty_into_reg_ap(aty,tmp,size_ff,store_indexed(base_reg,offset,tmp,C))
      | store_sm_in_record(LS.SAT_FF(SS.PHREG_ATY phreg,pp),tmp:reg,base_reg,offset,size_ff,C) = store_indexed(base_reg,offset,phreg,C)
      | store_sm_in_record(LS.SAT_FF(aty,pp),tmp:reg,base_reg,offset,size_ff,C) =
      move_aty_into_reg_ap(aty,tmp,size_ff,store_indexed(base_reg,offset,tmp,C))

      fun maybe_reset_region(LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp),dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.IGNORE,dst_reg:reg,size_ff,C) = C
	| maybe_reset_region(LS.ATBOT_LI(aty,pp),dst_reg:reg,size_ff,C) = 
	move_aty_into_reg_ap(aty,dst_reg,size_ff,
			     reset_region(dst_reg,size_ff,C))
	| maybe_reset_region(LS.SAT_FI(aty,pp),dst_reg:reg,size_ff,C) = 
	let
	  val default_lab = new_local_lab "no_reset"
	in
	  move_aty_into_reg_ap(aty,dst_reg,size_ff,
			       META_IF_BIT{r=dst_reg,bitNo=30,target=default_lab} ::
			       reset_region(dst_reg,size_ff,LABEL default_lab :: C))
	end
	| maybe_reset_region(LS.SAT_FF(aty,pp),dst_reg:reg,size_ff,C) = 
	let
	  val default_lab = new_local_lab "no_reset"
	in
	  move_aty_into_reg_ap(aty,dst_reg,size_ff,
			       META_IF_BIT{r=dst_reg,bitNo=31,target=default_lab} ::
			       META_IF_BIT{r=dst_reg,bitNo=30,target=default_lab} ::
			       reset_region(dst_reg,size_ff,LABEL default_lab :: C))
	end
	| maybe_reset_region(LS.ATTOP_LI(aty,pp),dst_reg:reg,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
	| maybe_reset_region(LS.ATTOP_LF(aty,pp),dst_reg:reg,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C) (* status bits are not set *)
	| maybe_reset_region(LS.ATTOP_FI(aty,pp),dst_reg:reg,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
	| maybe_reset_region(LS.ATTOP_FF(aty,pp),dst_reg:reg,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
        | maybe_reset_region(LS.ATBOT_LF(aty,pp),dst_reg:reg,size_ff,C) = move_aty_into_reg_ap(aty,dst_reg,size_ff,C) (* status bits are not set *)

      fun maybe_reset_aux_region(LS.ATBOT_LI(aty,pp),tmp_reg:reg,size_ff,C) = 
	move_aty_into_reg_ap(aty,tmp_reg,size_ff,
					       reset_region(tmp_reg,size_ff,C))
	| maybe_reset_aux_region(LS.SAT_FI(aty,pp),tmp_reg:reg,size_ff,C) = 
	let
	  val default_lab = new_local_lab "no_reset"
	in
	  move_aty_into_reg_ap(aty,tmp_reg,size_ff,
			       META_IF_BIT{r=tmp_reg,bitNo=30,target=default_lab} ::
						 reset_region(tmp_reg,size_ff,LABEL default_lab :: C))
	end
	| maybe_reset_aux_region(LS.SAT_FF(aty,pp),dst_reg:reg,size_ff,C) = 
	let
	  val default_lab = new_local_lab "no_reset"
	in
	  move_aty_into_reg_ap(aty,dst_reg,size_ff,
			       META_IF_BIT{r=dst_reg,bitNo=31,target=default_lab} ::
			       META_IF_BIT{r=dst_reg,bitNo=30,target=default_lab} ::
						 reset_region(dst_reg,size_ff,LABEL default_lab :: C))
	end
	| maybe_reset_aux_region(_,dst_reg:reg,size_ff,C) = C

    (*******************)
    (* Code Generation *)
    (*******************)
    fun CG_lss(lss,size_ff,size_cc,C) =
      let
	fun not_impl(s,C) = COMMENT s :: C
	fun CG_ls(LS.ASSIGN{pat,bind},C) = 
	  (case bind of
	     LS.ATOM src_aty => move_aty_to_aty(src_aty,pat,size_ff,C)
	   | LS.LOAD label => load_from_label(label,pat,size_ff,C)
	   | LS.STORE(src_aty,label) => store_in_label(src_aty,label,size_ff,C)
	   | LS.STRING str =>
	       let
		 val string_lab = new_string_lab()
		 val _ = add_static_data [DOT_DATA,
					  DOT_ALIGN 4,
					  LABEL string_lab,
					  DOT_WORD(Int.toString(size(str)*8+BI.value_tag_string)),
					  DOT_WORD (Int.toString(size(str))),
					  DOT_WORD "0", (* NULL pointer to next fragment. *)
					  DOT_STRINGZ str]
	       in
		 load_label_addr(string_lab,pat,size_ff,C)
	       end
	   | LS.REAL str => 
	       let
		 val float_lab = new_float_lab()
		 val _ = 
		   if !BI.tag_values then 
		     add_static_data [DOT_DATA,
				      DOT_ALIGN 8,
				      LABEL float_lab,
				      DOT_WORD (Int.toString BI.value_tag_real),
				      DOT_WORD "0", (* dummy *)
				      DOT_DOUBLE str]
		   else
		     add_static_data [DOT_DATA,
				      DOT_ALIGN 8,
				      LABEL float_lab,
				      DOT_DOUBLE str]
	       in
		 load_label_addr(float_lab,pat,size_ff,C)
	       end
	   | LS.CLOS_RECORD{label,elems,alloc} => 
	       let
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	       in
		 alloc_ap(alloc,reg_for_result,List.length elems + 1,size_ff,
			  load_label_addr(NameLab (Labels.pr_label label),SS.PHREG_ATY tmp_reg2,size_ff,
					  store_indexed(reg_for_result,WORDS 0,tmp_reg2,
							#2(foldr (fn (aty,(offset,C)) => 
								  (offset+1,store_aty_in_reg_record(aty,tmp_reg2,reg_for_result,WORDS offset,size_ff,C))) (1,C') elems))))
	       end
	   | LS.REGVEC_RECORD{elems,alloc} =>
	       let
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	       in
		 alloc_ap(alloc,reg_for_result,List.length elems,size_ff,
			  #2(foldr (fn (sma,(offset,C)) => (offset+1,store_sm_in_record(sma,tmp_reg2,reg_for_result,WORDS offset,size_ff,C))) (0,C') elems))
	       end
	   | LS.SCLOS_RECORD{elems,alloc} => 
	       let
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	       in
		 alloc_ap(alloc,reg_for_result,List.length elems,size_ff,
			    #2(foldr (fn (aty,(offset,C)) => (offset+1,store_aty_in_reg_record(aty,tmp_reg2,reg_for_result,WORDS offset,size_ff,C))) (0,C') elems))
	       end
	   | LS.RECORD{elems,alloc} =>
	       let
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	       in
		 alloc_ap(alloc,reg_for_result,List.length elems,size_ff,
			    #2(foldr (fn (aty,(offset,C)) => (offset+1,store_aty_in_reg_record(aty,tmp_reg2,reg_for_result,WORDS offset,size_ff,C))) (0,C') elems))
	       end
	   | LS.SELECT(i,aty) => move_index_aty_to_aty(pat,aty,WORDS i,size_ff,C)
	   | LS.CON0{con,con_kind,aux_regions,alloc} =>
	       (case con_kind of
		  LS.ENUM i => 
		    let 
		      val tag = 
			if !BI.tag_values orelse (*hack to treat booleans tagged*)
			  Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 
			  2*i+1 
			else i
		      val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		    in
		      load_immed(IMMED tag,reg_for_result,C')
		    end
		| LS.UNBOXED i => 
		    let
		      val tag = 4*i+3 
		      val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		    in
		      load_immed(IMMED tag,reg_for_result,
				 foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,tmp_reg2,size_ff,C)) C' aux_regions)
		    end
		| LS.BOXED i => 
		    let 
		      val tag = 8*i + BI.value_tag_con0 
		      val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		    in  
		      List.foldr (fn (alloc,C) => maybe_reset_aux_region(alloc,tmp_reg2,size_ff,C))
		       (alloc_ap(alloc,reg_for_result,1,size_ff,
				 load_immed(IMMED tag,tmp_reg2,
					    store_indexed(reg_for_result,WORDS 0,tmp_reg2,C')))) aux_regions
		    end)
	   | LS.CON1{con,con_kind,alloc,arg} => 
	     (case con_kind of
		LS.UNBOXED 0 => move_aty_to_aty(arg,pat,size_ff,C) 
	      | LS.UNBOXED i => 
		  let
		    val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		  in
		    (case i of
		       1 => move_aty_into_reg(arg,reg_for_result,size_ff,
					      DEPI{cond=NEVER, i="1", p="31", len="1", t=reg_for_result} :: C')
		     | 2 => move_aty_into_reg(arg,reg_for_result,size_ff,
					      DEPI{cond=NEVER, i="1", p="30", len="1", t=reg_for_result} :: C')
		     | _ => die "CG_ls: UNBOXED CON1 with i > 2")
		  end
	      | LS.BOXED i => 
		  let
		    val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		    val tag = 8*i + BI.value_tag_con1
		  in
		    alloc_ap(alloc,reg_for_result,2,size_ff,
			     load_immed(IMMED tag,tmp_reg2,
					store_indexed(reg_for_result,WORDS 0,tmp_reg2,
						      store_aty_in_reg_record(arg,tmp_reg2,reg_for_result,WORDS 1,size_ff,C'))))
		  end
	      | _ => die "CON1.con not unary in env.")
	   | LS.DECON{con,con_kind,con_aty} =>
		(case con_kind of
		   LS.UNBOXED 0 => move_aty_to_aty(con_aty,pat,size_ff,C)
		 | LS.UNBOXED _ => 
		     let
		       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		     in
		       move_aty_into_reg(con_aty,reg_for_result,size_ff,
					 DEPI{cond=NEVER, i="0", p="31", len="2", t=reg_for_result} :: C')
		     end
		 | LS.BOXED _ => move_index_aty_to_aty(con_aty,pat,WORDS 1,size_ff,C)
		 | _ => die "CG_ls: DECON used with con_kind ENUM")
	   | LS.DEREF aty =>
	       let
		 val offset = if !BI.tag_values then 1 else 0
	       in
		 move_index_aty_to_aty(pat,aty,WORDS offset,size_ff,C)
	       end
	   | LS.REF(alloc,aty) =>
	       let
		 val offset = if !BI.tag_values then 1 else 0
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		 fun maybe_tag_value C =
		   if !BI.tag_values then
		     load_immed(IMMED BI.value_tag_ref,tmp_reg2,
				store_indexed(reg_for_result,WORDS 0,tmp_reg2,C))
		   else C
	       in
		 alloc_ap(alloc,reg_for_result,BI.size_of_ref(),size_ff,
			  store_aty_in_reg_record(aty,tmp_reg2,reg_for_result,WORDS offset,size_ff,
						  maybe_tag_value C'))
	       end
	   | LS.ASSIGNREF(alloc,aty1,aty2) =>
	       let 
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
		 val offset = if !BI.tag_values then 1 else 0
	       in
		 store_aty_in_aty_record(aty2,aty1,WORDS offset,tmp_reg1,tmp_reg2,size_ff,
					 load_immed(IMMED BI.ml_unit,reg_for_result,C'))
	       end
	   | LS.PASS_PTR_TO_MEM(alloc,i) =>
	       let
		 val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	       in
		 alloc_ap(alloc,reg_for_result,i,size_ff,C')
	       end
	   | LS.PASS_PTR_TO_RHO(alloc) =>
	     let
	       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
	     in 
	       maybe_reset_region(alloc,reg_for_result,size_ff,C')
	     end)
	  | CG_ls(LS.FLUSH(aty,offset),C) = store_aty_in_reg_record(aty,tmp_reg1,sp,WORDS offset,size_ff,C)
	  | CG_ls(LS.FETCH(aty,offset),C) = load_aty_from_reg_record(aty,tmp_reg1,sp,WORDS offset,size_ff,C)
	  | CG_ls(LS.FNJMP a,C) = not_impl("FNJMP",C)
	  | CG_ls(LS.FNCALL a,C) = not_impl("FNCALL",C)
	  | CG_ls(LS.JMP a,C) = not_impl("JMP",C)
	  | CG_ls(LS.FUNCALL a,C) = not_impl("FUNCALL",C)
	  | CG_ls(LS.LETREGION{rhos,body},C) = 
	     let
	       fun alloc_region_prim((_,offset),C) = 
		 base_plus_offset(sp,WORDS(~size_ff+offset),arg1,
				  compile_c_call_prim("allocateRegion",[SS.PHREG_ATY arg1],NONE,size_ff,C))
	       fun dealloc_region_prim C =
		 compile_c_call_prim("deallocateRegionNew",[],NONE,size_ff,C)
	       fun remove_finite_rhos([]) = []
		 | remove_finite_rhos(((place,PhysSizeInf.WORDS i),offset)::rest) = remove_finite_rhos rest
		 | remove_finite_rhos(rho::rest) = rho :: remove_finite_rhos rest
	       val rhos_to_allocate = remove_finite_rhos rhos
	     in
	       foldr alloc_region_prim 
	       (CG_lss(body,size_ff,size_cc,
		       foldl (fn (_,C) => dealloc_region_prim C) C rhos_to_allocate)) rhos_to_allocate
	     end
	  | CG_ls(LS.SCOPE{pat,scope},C) = CG_lss(scope,size_ff,size_cc,C)
	  | CG_ls(LS.HANDLE{default,handl,handl_return,offset},C) = not_impl("HANDLE",C)
	  | CG_ls(LS.RAISE{arg,defined_atys},C) = not_impl("RAISE",C)
	  | CG_ls(LS.SWITCH_I sw,C) = not_impl("SWITCH_I",C)
	  | CG_ls(LS.SWITCH_S sw,C) = not_impl("SWITCH_S",C)
	  | CG_ls(LS.SWITCH_C sw,C) = not_impl("SWITCH_C",C)
	  | CG_ls(LS.SWITCH_E sw,C) = not_impl("SWITCH_E",C)
	  | CG_ls(LS.RESET_REGIONS{force,regions_for_resetting},C) = not_impl("RESET_REGIONS",C)
	  | CG_ls(LS.CCALL{name,args,rhos_for_result,res},C) = not_impl("CCALL",C)
      in
	foldr (fn (ls,C) => CG_ls(ls,C)) C lss
      end

    fun CG_top_decl(LS.FUN(lab,cc,lss)) = FUN(lab,CG_lss(lss,CallConv.get_frame_size cc,CallConv.get_cc_size cc,[]))
      | CG_top_decl(LS.FN(lab,cc,lss)) = FN(lab,CG_lss(lss,CallConv.get_frame_size cc,CallConv.get_cc_size cc,[]))
  in
    fun CG {main_lab:label,
	    code=ss_prg: (StoreTypeSS,offset,AtySS) LinePrg,
	    imports:label list,
	    exports:label list} =
      let
	val _ = chat "[Code Generation..."
	val _ = reset_static_data()
	val _ = reset_label_counter()
	val hp_parisc_prg = {top_decls = foldr (fn (func,acc) => CG_top_decl func :: acc) [] ss_prg,
			     init_code = [],
			     exit_code = [],
			     static_data = get_static_data()}
	val _ = 
	  if Flags.is_on "print_HP-PARISC_program" then
	    display("\nReport: AFTER CODE GENERATION(HP-PARISC):", HpPaRisc.layout_RiscPrg hp_parisc_prg)
	  else
	    ()
	val _ = chat "]\n"
      in
	hp_parisc_prg
      end
  end

end;



(*    fun in_bytes(BYTES x) = x
      | in_bytes(WORDS x) = x*4
      | in_bytes(IMMED x) =
      if Int.mod(x,8) <> 0 then
	die ("in_bytes: " ^ int_to_string x ^ " cannot be converted into bytes")
      else
	Int.div(x,8)*)

(*    fun in_words(BYTES x) =
      if Int.mod(x,4) <> 0 then
	die ("in_words: Bytes " ^ int_to_string x ^ " cannot be converted into words")
      else
	Int.div(x,4)
      | in_words(WORDS x) = x
      | in_words(IMMED x) =
	if Int.mod(x,32) <> 0 then
	  die ("in_words: Immed " ^  int_to_string x ^ " cannot be converted into words")
	else
	  Int.div(x,32)*)
	  
(*    fun in_immeds(BYTES x) = x * 8
      | in_immeds(WORDS x) = x * 32
      | in_immeds(IMMED x) = x*)

(*    fun resolve_aty_use(SS.REG_I_ATY offset,t,size_ff,C) = (t,calc_offset(sp,BYTES(~size_ff*4+offset*4+inf_bit),t,C))
      | resolve_aty_use(SS.REG_F_ATY offset,t,size_ff,C) = (t,calc_offset(sp,BYTES(~size_ff*4+offset*4),t,C))
      | resolve_aty_use(SS.STACK_ATY offset,t,size_ff,C) = (t,load_word(t,sp,BYTES(~size_ff*4+offset*4),C))
      | resolve_aty_use(SS.DROPPED_RVAR_ATY,t,size_ff,C) = (t,C)
      | resolve_aty_use(SS.PHREG_ATY phreg,t,size_ff,C)  = (phreg,C)      
      | resolve_aty_use(SS.INTEGER_ATY i,t,size_ff,C)    = (t,load_immed(IMMED i,t,C))
      | resolve_aty_use(SS.UNIT_ATY,t,size_ff,C)         = (t,load_immed(IMMED ml_unit,t,C))*)
