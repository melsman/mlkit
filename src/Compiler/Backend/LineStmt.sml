functor LineStmt(structure PhysSizeInf : PHYS_SIZE_INF
                 structure Con : CON
		 structure Excon : EXCON
		 structure Lvars : LVARS
		 structure Effect : EFFECT
		 structure Labels : ADDRESS_LABELS
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
	         structure RI : REGISTER_INFO
                   sharing type RI.lvar = Lvars.lvar
		 structure Lvarset: LVARSET
		   sharing type Lvarset.lvar = Lvars.lvar
		 structure PP : PRETTYPRINT
		   sharing type PP.StringTree = 
                                Effect.StringTree = 
				ClosExp.StringTree
                 structure Flags : FLAGS
		 structure Report : REPORT
		   sharing type Report.Report = Flags.Report
		 structure Crash : CRASH) : LINE_STMT = 
struct

  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  datatype phsize = datatype PhysSizeInf.phsize
  type pp = PhysSizeInf.pp
  type cc = CallConv.cc
  type label = Labels.label
  type ClosPrg = ClosExp.ClosPrg

  structure LvarFinMap = Lvars.Map

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun log_st st = PP.outputTree(fn s => TextIO.output(!Flags.log,s), st, 70)
  fun pr_st st = PP.outputTree(fn s => TextIO.output(TextIO.stdOut,s), st, 70)
  fun die s  = Crash.impossible ("LineStmt." ^ s)
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

  (************)
  (* LineStmt *)
  (************)

  datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | BOXED of int

  type binder = place * phsize

  datatype Atom =
      VAR           of lvar
    | FLOW_VAR      of lvar * label * label
    | RVAR          of place
    | DROPPED_RVAR  of place
    | PHREG         of lvar
    | INTEGER       of int 
    | UNIT

  datatype StoreType =
      V of lvar
    | FV of lvar * label * label

  datatype 'aty SimpleExp =
      ATOM            of 'aty
    | LOAD            of label
    | STORE           of 'aty * label
    | STRING          of string
    | REAL            of string
    | CLOS_RECORD     of {label: label, elems: 'aty list*'aty list*'aty list, alloc: 'aty sma}
    | REGVEC_RECORD   of {elems: 'aty sma list, alloc: 'aty sma}
    | SCLOS_RECORD    of {elems: 'aty list*'aty list*'aty list, alloc: 'aty sma}
    | RECORD          of {elems: 'aty list, alloc: 'aty sma, tag: Word32.word}
    | SELECT          of int * 'aty
    | CON0            of {con: con, con_kind: con_kind, aux_regions: 'aty sma list, alloc: 'aty sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: 'aty sma, arg: 'aty}
    | DECON           of {con: con, con_kind: con_kind, con_aty: 'aty}
    | DEREF           of 'aty
    | REF             of 'aty sma * 'aty
    | ASSIGNREF       of 'aty sma * 'aty * 'aty
    | PASS_PTR_TO_MEM of 'aty sma * int (* Used only by CCALL *)
    | PASS_PTR_TO_RHO of 'aty sma       (* Used only by CCALL *)

  and ('sty,'offset,'aty) LineStmt = 
      ASSIGN        of {pat: 'aty, bind: 'aty SimpleExp}
    | FLUSH         of 'aty * 'offset
    | FETCH         of 'aty * 'offset
    | FNJMP         of {opr: 'aty, args: 'aty list, clos: 'aty option, 
			free: 'aty list, res: 'aty list, bv: Word32.word list}
    | FNCALL        of {opr: 'aty, args: 'aty list, clos: 'aty option, 
			free: 'aty list, res: 'aty list, bv: Word32.word list}
    | JMP           of {opr: label, args: 'aty list, reg_vec: 'aty option, 
			reg_args: 'aty list, clos: 'aty option, free: 'aty list, res: 'aty list, bv: Word32.word list}
    | FUNCALL       of {opr: label, args: 'aty list, reg_vec: 'aty option, 
			reg_args: 'aty list, clos: 'aty option, free: 'aty list, res: 'aty list, bv: Word32.word list}
    | LETREGION     of {rhos: (binder*'offset) list, body: ('sty,'offset,'aty) LineStmt list}
    | SCOPE         of {pat: 'sty list, scope: ('sty,'offset,'aty) LineStmt list}
    | HANDLE        of {default: ('sty,'offset,'aty) LineStmt list, 
			handl: ('sty,'offset,'aty) LineStmt list * 'aty, 
			handl_return: ('sty,'offset,'aty) LineStmt list * 'aty * (Word32.word list), 
			offset: 'offset}
    | RAISE         of {arg: 'aty,defined_atys: 'aty list}
    | SWITCH_I      of (int,'sty,'offset,'aty) Switch
    | SWITCH_S      of (string,'sty,'offset,'aty) Switch
    | SWITCH_C      of ((con*con_kind),'sty,'offset,'aty) Switch
    | SWITCH_E      of (excon,'sty,'offset,'aty) Switch
    | RESET_REGIONS of {force: bool, 
			regions_for_resetting: 'aty sma list}
    | PRIM          of {name: string, args: 'aty list, res: 'aty list}
    | CCALL         of {name: string, args: 'aty list,
			rhos_for_result : 'aty list, res: 'aty list}

  and ('a,'sty,'offset,'aty) Switch = SWITCH of 'aty * ('a * (('sty,'offset,'aty) LineStmt list)) list * (('sty,'offset,'aty) LineStmt list)

  and 'aty sma = 
      ATTOP_LI of 'aty * pp
    | ATTOP_LF of 'aty * pp
    | ATTOP_FI of 'aty * pp
    | ATTOP_FF of 'aty * pp
    | ATBOT_LI of 'aty * pp
    | ATBOT_LF of 'aty * pp
    | SAT_FI   of 'aty * pp
    | SAT_FF   of 'aty * pp
    | IGNORE

  datatype ('sty,'offset,'aty) TopDecl = 
      FUN of label * cc * ('sty,'offset,'aty) LineStmt list
    | FN of label * cc * ('sty,'offset,'aty) LineStmt list
  
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) TopDecl list

  fun smash_free (lvs,excons,rhos) = rhos@excons@lvs

  (************************)
  (* PrettyPrint LineStmt *)
  (************************)
  type StringTree = PP.StringTree

  fun pr_phreg phreg = Lvars.pr_lvar phreg

  fun remove_finite_rhos([]) = []
    | remove_finite_rhos(((place,PhysSizeInf.WORDS i),offset)::rest) = remove_finite_rhos rest
    | remove_finite_rhos(rho::rest) = rho :: remove_finite_rhos rest

  fun pr_atom(VAR lv) = Lvars.pr_lvar lv
    | pr_atom(FLOW_VAR(lv,l1,l2)) = Lvars.pr_lvar lv
    | pr_atom(RVAR place) = PP.flatten1(Effect.layout_effect place)
    | pr_atom(DROPPED_RVAR place) = "D" ^ PP.flatten1(Effect.layout_effect place)
    | pr_atom(PHREG phreg) = pr_phreg phreg
    | pr_atom(INTEGER i) = Int.toString i
    | pr_atom(UNIT) = "()"

  fun pr_sty(V lv) = Lvars.pr_lvar lv
    | pr_sty(FV(lv,l1,l2)) = Lvars.pr_lvar lv ^ ":FV(" ^ Labels.pr_label l1 ^ "," ^ Labels.pr_label l2 ^ ")"

  (* simplify is a bool used to not print non operative constructs *)
  (* like scope and letregion on finite regions.                   *)
  local
    open PP

    fun pr_phsize(PhysSizeInf.INF)     = "inf"
      | pr_phsize(PhysSizeInf.WORDS i) = Int.toString i

    fun pr_binder(place,phsize) = 
      (PP.flatten1(Effect.layout_effect place) ^ ":" ^ pr_phsize phsize)

    fun pr_con_kind(ENUM i)    = "enum " ^ Int.toString i
      | pr_con_kind(UNBOXED i) = "unboxed " ^ Int.toString i
      | pr_con_kind(BOXED i)   = "boxed " ^ Int.toString i

    fun pr_pp pp = "pp" ^ Int.toString pp

    fun layout_aty pr_aty aty = LEAF(pr_aty aty)
      
    fun layout_aty_opt pr_aty (SOME aty) = layout_aty pr_aty aty
      | layout_aty_opt pr_aty (NONE) = LEAF ""

    fun pr_bv([]) = ""
      | pr_bv([mark]) = die "pr_bv:Bit Vector with only one element"
      | pr_bv([mark,offsetToReturn]) = die "pr_bv:Bit Vector with only two elements"
      | pr_bv([mark,offsetToReturn,size]) = die "pr_bv:Bit Vector with only three elements"
      | pr_bv(mark::offsetToReturn::size::bvs) = 
      (foldl (fn (w,C) => (Word32.fmt StringCvt.BIN w) ^ ":" ^  C) "" bvs) ^ 
      (Word32.fmt StringCvt.DEC size) ^ ":" ^ (Word32.fmt StringCvt.DEC offsetToReturn) ^ ":" ^
      (Word32.fmt StringCvt.DEC mark)

    fun layout_switch pr_aty layout_lss pr_const (SWITCH(aty_arg,sels,default)) =
      let
	fun layout_sels(const,ls_sel) =
	  NODE{start="",finish="",indent=0,
	       children=[LEAF (pr_const const),layout_lss ls_sel],
	       childsep=RIGHT " => "}
	val t1 = NODE{start="(case ",finish=" ",indent=2, childsep = NOSEP, 
		      children=[layout_aty pr_aty aty_arg]}
	val t2 = NODE{start="of " ,finish="",indent=6,childsep=LEFT " | ",
		      children=(map layout_sels sels) @ 
		      [NODE{start="",finish="",indent=0,
			    children=[LEAF "_",layout_lss default],
			    childsep=RIGHT " => "}]}
	val t3 = NODE{start="",finish=") (*case*) ",indent=3,childsep=NOSEP,children=[t2]}
      in 
	NODE{start = "", finish = "", indent=0, childsep=NOSEP,children=[t1,t3]}
      end

      fun layout_se pr_aty se =
	(case se of
	   ATOM aty => layout_aty pr_aty aty
	 | LOAD lab => LEAF("load(" ^ Labels.pr_label lab ^ ")")
	 | STORE(aty,lab) => LEAF("store(" ^ pr_aty aty ^ "," ^ Labels.pr_label lab ^ ")")
	 | STRING s  => LEAF("\"" ^ String.toString s ^ "\"")
	 | REAL s    => LEAF(s)
	 | CLOS_RECORD{label,elems=elems as (lvs,excons,rhos),alloc} => HNODE{start="[",
									      finish="]clos " ^ pr_sma pr_aty alloc,
									      childsep=RIGHT ",",
									      children=LEAF(Labels.pr_label label)::
									      map (layout_aty pr_aty) (smash_free elems)}
	 | REGVEC_RECORD{elems,alloc} => HNODE{start="[",
					       finish="]regvec " ^ pr_sma pr_aty alloc,
					       childsep=RIGHT ",",
					       children=map (layout_sma pr_aty) elems}
	 | SCLOS_RECORD{elems=elems as (lvs,excons,rhos),alloc} => HNODE{start="[",
									 finish="]sclos " ^ pr_sma pr_aty alloc,
									 childsep=RIGHT ",",
									 children= map (layout_aty pr_aty) (smash_free elems)}
	 | RECORD{elems,alloc,tag} => HNODE{start="[",
					    finish="] " ^ pr_sma pr_aty alloc,
					    childsep=RIGHT ",",
					    children= map (layout_aty pr_aty) elems}
	 | SELECT(i,aty) => HNODE{start="#" ^ Int.toString i ^ "(",
				  finish=")",
				  childsep=NOSEP,
				  children=[layout_aty pr_aty aty]}
	 | CON0{con,con_kind,aux_regions,alloc} =>
	     HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ") [",
		   finish="]aux " ^ pr_sma pr_aty alloc,
		   childsep=RIGHT ",",
		   children=map (layout_sma pr_aty) aux_regions}
	 | CON1{con,con_kind,alloc,arg} => 
	     HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ") ",
		   finish="" ^ pr_sma pr_aty alloc,
		   childsep=NOSEP,
		   children=[layout_aty pr_aty arg]}
	 | DECON{con,con_kind,con_aty} =>
	     LEAF("decon(" ^ Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")," ^ pr_aty con_aty ^ ")")
	 | DEREF(aty) => LEAF("!" ^ pr_aty aty)
	 | REF(sma,aty) => LEAF("ref " ^ pr_aty aty ^ " " ^ pr_sma pr_aty sma)
	 | ASSIGNREF(sma,aty1,aty2) => HNODE{start="",
					     finish="",
					     childsep=RIGHT " := ",
					     children=[layout_aty pr_aty aty1,layout_aty pr_aty aty2]}
	 | PASS_PTR_TO_MEM(sma,i) => LEAF("MEM(" ^ pr_sma pr_aty sma ^ "," ^ Int.toString i ^ ")")
	 | PASS_PTR_TO_RHO(sma) => LEAF("PTR(" ^ pr_sma pr_aty sma ^ ")"))
	
      and layout_ls pr_sty pr_offset pr_aty simplify ls =
	let 
	  fun layout_lss_local lss = layout_lss pr_sty pr_offset pr_aty simplify lss
	in
	  (case ls of
	     ASSIGN{pat,bind} => HNODE{start="",
				       finish="",
				       childsep=RIGHT " = ",
				       children=[LEAF(pr_aty pat),layout_se pr_aty bind]}
	   | FLUSH(aty,offset) => LEAF("flush(" ^ pr_aty aty ^ "," ^ pr_offset offset ^ ")")
	   | FETCH(aty,offset) => LEAF("fetch(" ^ pr_aty aty ^ "," ^ pr_offset offset ^ ")")
	   | FNJMP{opr,args,clos,free,res,bv} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ pr_aty opr ^ "_fnjmp ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t2,t3]}
	       end
	   | FNCALL{opr,args,clos,free,res,bv=[]} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ pr_aty opr ^ "_fncall ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t2,t3]}
	       end
	   | FNCALL{opr,args,clos,free,res,bv} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
		 val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[PP.LEAF(pr_bv bv)]}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ pr_aty opr ^ "_fncall ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t2,t3,t4]}
	       end
	   | JMP{opr,args,reg_vec,reg_args,clos,free,res,bv} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
		 val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) reg_args}
		 val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty reg_vec]}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ Labels.pr_label opr ^ "_funjmp ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t5,t4,t2,t3]}
	       end
	   | FUNCALL{opr,args,reg_vec,reg_args,clos,free,res,bv=[]} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
		 val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) reg_args}
		 val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty reg_vec]}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ Labels.pr_label opr ^ "_funcall ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t5,t4,t2,t3]}
	       end
	   | FUNCALL{opr,args,reg_vec,reg_args,clos,free,res,bv} =>
	       let
		 val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) args}
		 val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty clos]}
		 val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) free}
		 val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (layout_aty pr_aty) reg_args}
		 val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_aty_opt pr_aty reg_vec]}
		 val t6 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[PP.LEAF(pr_bv bv)]}
	       in
		 HNODE{start=flatten1(t0) ^ " = " ^ Labels.pr_label opr ^ "_funcall ",
		       finish="", childsep=RIGHT " ",
		       children=[t1,t5,t4,t2,t3,t6]}
	       end
	   | LETREGION{rhos,body} =>
	       let
		 val rhos = if simplify then remove_finite_rhos rhos else rhos
		 val binders = HNODE{start = "", 
				     finish = "", 
				     childsep = RIGHT", ", 
				     children = map (fn (b,offset) => LEAF(pr_binder b ^ pr_offset offset)) rhos}
	       in 
		 (case rhos of
		    [] => layout_lss_local body
		  | _ => NODE{start= "letregion " ^ flatten1(binders) ^ " in ",
			      finish= "end (*" ^ flatten1(binders) ^ "*)",
			      childsep= NOSEP,
			      indent=2,
			      children= [layout_lss_local body]})
	       end
	   | SCOPE{pat=[],scope} => layout_lss_local scope
	   | SCOPE{pat,scope} =>
	       if simplify then
		 layout_lss_local scope
	       else
		 let
		   val lay_pat = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (fn sty => LEAF(pr_sty sty)) pat}
		 in
		   PP.NODE{start= "scope " ^ flatten1(lay_pat) ^ " in ",
			   finish=" end ",
			   indent=2,
			   childsep=NOSEP,
			   children=[layout_lss_local scope]}
		 end
	   | HANDLE{default,handl=(handl,handl_aty),handl_return=(handl_return,handl_return_aty,[]),offset} =>
		 let
		   val node_exn = NODE{start="[",finish="]",childsep=RIGHT("](" ^ pr_aty handl_aty ^ 
				       ") handlereturn(" ^ pr_aty handl_return_aty ^ ") ["), 
				       indent=2,children=[layout_lss_local handl,layout_lss_local handl_return]}
		 in
		   NODE{start="[",finish="",childsep=RIGHT("] handle " ^ pr_offset offset ^ " "),indent=2,children=[layout_lss_local default,node_exn]}
		 end
	   | HANDLE{default,handl=(handl,handl_aty),handl_return=(handl_return,handl_return_aty,bv),offset} =>
		 let
		   val hnode_bv = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[PP.LEAF(pr_bv bv)]}
		   val node_exn = NODE{start="[",finish="](bv: " ^ flatten1(hnode_bv) ^ ")",childsep=RIGHT("](" ^ pr_aty handl_aty ^ 
				       ") handlereturn(" ^ pr_aty handl_return_aty ^ ") ["), 
				       indent=2,children=[layout_lss_local handl,layout_lss_local handl_return]}
		 in
		   NODE{start="[",finish="",childsep=RIGHT("] handle " ^ pr_offset offset ^ " "),indent=2,children=[layout_lss_local default,node_exn]}
		 end

	   | RAISE{arg,defined_atys} => 
		 let
		   val lay_stys = flatten1(HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (fn aty => LEAF(pr_aty aty)) defined_atys})
		 in
		   PP.LEAF("raise " ^ pr_aty arg ^ "(defined: " ^ lay_stys ^ ")") (* Defined atys not written 08/12/1998, Niels*)
		 end
	   | SWITCH_I sw => layout_switch pr_aty layout_lss_local (Int.toString) sw
	   | SWITCH_S sw => layout_switch pr_aty layout_lss_local (fn s => s) sw
	   | SWITCH_C sw =>
		 layout_switch pr_aty layout_lss_local (fn (con,con_kind) => Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")") sw
           | SWITCH_E sw => layout_switch pr_aty layout_lss_local Excon.pr_excon sw
	   | RESET_REGIONS{force=true,regions_for_resetting} =>
		 HNODE{start="force reset regions(",
		       finish=")",
		       childsep=RIGHT ",",
		       children=map (layout_sma pr_aty) regions_for_resetting}
	   | RESET_REGIONS{force=false,regions_for_resetting} =>
		 HNODE{start="reset regions(",
		       finish=")",
		       childsep=RIGHT ",",
		       children=map (layout_sma pr_aty) regions_for_resetting}
	   | PRIM{name,args,res} =>
		 let
		   val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 in
		   HNODE{start=flatten1(t0) ^ " = prim(\"" ^ name ^ "\", <", 
			 finish=">)",
			 childsep=RIGHT ",",
			 children=map (layout_aty pr_aty) args}
		 end
	   | CCALL{name,args,rhos_for_result,res} =>
		 let
		   val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map (layout_aty pr_aty) res}
		 in
		   HNODE{start=flatten1(t0) ^ " = ccall(\"" ^ name ^ "\", <", 
			 finish=">)",
			 childsep=RIGHT ",",
			 children=(map (layout_aty pr_aty) rhos_for_result) @ (map (layout_aty pr_aty) args)}
		 end)
	end
      
      and layout_lss pr_sty pr_offset pr_aty simplify lss = 
	NODE{start="",
	     finish= "",
	     indent= 0,
	     childsep= RIGHT "; ",
	     children= map (layout_ls pr_sty pr_offset pr_aty simplify) lss}

      and pr_sma pr_aty sma =
	(case sma of
	   ATTOP_LI(aty,pp) => "attop_li " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | ATTOP_LF(aty,pp) => "attop_lf " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | ATTOP_FI(aty,pp) => "attop_fi " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | ATTOP_FF(aty,pp) => "attop_ff " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | ATBOT_LI(aty,pp) => "atbot_li " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | ATBOT_LF(aty,pp) => "atbot_lf " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | SAT_FI(aty,pp)   => "sat_fi " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | SAT_FF(aty,pp)   => "sat_ff " ^ pr_aty aty ^ " " ^ pr_pp pp
	 | IGNORE            => "ignore ")

      and layout_sma pr_aty sma = LEAF(pr_sma pr_aty sma)

      fun layout_top_decl pr_sty pr_offset pr_aty simplify (FUN(lab,cc,lss)) =
          NODE{start = "FUN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = NOSEP, 
	       children = [layout_lss pr_sty pr_offset pr_aty simplify lss]}
      | layout_top_decl pr_sty pr_offset pr_aty simplify (FN(lab,cc,lss)) =
	  NODE{start = "FN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = NOSEP,
	       children = [layout_lss pr_sty pr_offset pr_aty simplify lss]}

      fun layout_line_stmt' pr_sty pr_offset pr_aty simplify ls = layout_ls pr_sty pr_offset pr_aty simplify ls
      fun layout_line_prg' pr_sty pr_offset pr_aty simplify top_decls =
	NODE{start="LineStmt program begin",
	     finish="LineStmt program end",
	     indent=2,
	     childsep=NOSEP,
	     children = map (layout_top_decl pr_sty pr_offset pr_aty simplify) top_decls}
  in
    fun layout_line_prg pr_sty pr_offset pr_aty simplify top_decls = 
      layout_line_prg' pr_sty pr_offset pr_aty simplify top_decls
    fun layout_line_stmt pr_sty pr_offset pr_aty simplify ls =
      layout_line_stmt' pr_sty pr_offset pr_aty simplify ls
    fun pr_line_stmt pr_sty pr_offset pr_aty simplify ls =
      PP.flatten1(layout_line_stmt' pr_sty pr_offset pr_aty simplify ls)
  end

  val flow_var_flag = Flags.lookup_flag_entry "use_flow_variables"

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

  (***********************)
  (* Linearization: L_ce *)
  (***********************)
  local
    fun binder_to_binder(place,phsize) = ((place,phsize),())  (* for now, offset is unit *)

    fun one_lvar([]) = die "one_lvar: zero lvars."
      | one_lvar([lv]) = lv
      | one_lvar(lvars) = die "one_lvar: more than one lvar."

    fun ce_to_atom(ClosExp.VAR lv) = VAR lv
      | ce_to_atom(ClosExp.RVAR place) = RVAR place
      | ce_to_atom(ClosExp.DROPPED_RVAR place) = DROPPED_RVAR place
      | ce_to_atom(ClosExp.INTEGER i) = INTEGER i
      | ce_to_atom(ClosExp.RECORD{elems=[],alloc=ClosExp.IGNORE,tag}) = UNIT
      | ce_to_atom ce = die ("ce_to_atom: expression not an atom:" ^ PP.flatten1(ClosExp.layout_clos_exp ce))

    fun ce_to_atom_opt(NONE) = NONE
      | ce_to_atom_opt(SOME ce) = SOME(ce_to_atom ce)

    fun ces_to_atoms([]) = []
      | ces_to_atoms(ce::ces) = ce_to_atom(ce)::ces_to_atoms(ces)

    fun sma_to_sma(ClosExp.ATTOP_LI(ce,pp)) = ATTOP_LI(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.ATTOP_LF(ce,pp)) = ATTOP_LF(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.ATTOP_FI(ce,pp)) = ATTOP_FI(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.ATTOP_FF(ce,pp)) = ATTOP_FF(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.ATBOT_LI(ce,pp)) = ATBOT_LI(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.ATBOT_LF(ce,pp)) = ATBOT_LF(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.SAT_FI(ce,pp))   =   SAT_FI(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.SAT_FF(ce,pp))   =   SAT_FF(ce_to_atom ce,pp)
      | sma_to_sma(ClosExp.IGNORE)          = IGNORE

    fun smas_to_smas([]) = []
      | smas_to_smas(sma::smas) = sma_to_sma(sma)::smas_to_smas(smas)

    fun con_kind_to_con_kind(ClosExp.ENUM i) = ENUM i
      | con_kind_to_con_kind(ClosExp.UNBOXED i) = UNBOXED i
      | con_kind_to_con_kind(ClosExp.BOXED i) = BOXED i

    fun mk_sty lv = V lv (* Flow variables are annotated later *)

    fun L_ce_sw(ClosExp.SWITCH(ce,sels,default),f_L,f_sel) =
      SWITCH(ce_to_atom ce,
	     map (fn (sel,ce) => (f_sel sel,f_L (ce,[]))) sels,
	     f_L (default,[]))

    fun L_ce(ClosExp.VAR lv,lvars_res,acc)             = ASSIGN{pat=VAR(one_lvar lvars_res),bind=ATOM(VAR lv)}::acc
      | L_ce(ClosExp.RVAR place,lvars_res,acc)         = die "RVAR not implemented"
      | L_ce(ClosExp.DROPPED_RVAR place,lvars_res,acc) = die "DROPPED_RVAR not implemented"
      | L_ce(ClosExp.FETCH lab,lvars_res,acc)          = ASSIGN{pat=VAR(one_lvar lvars_res),bind=LOAD lab}::acc
      | L_ce(ClosExp.STORE(ce,lab),lvars_res,acc)      = ASSIGN{pat=VAR(one_lvar lvars_res),bind=STORE(ce_to_atom ce,lab)}::acc
      | L_ce(ClosExp.INTEGER i,lvars_res,acc)          = ASSIGN{pat=VAR(one_lvar lvars_res),bind=ATOM(INTEGER i)}::acc
      | L_ce(ClosExp.STRING s,lvars_res,acc)           = ASSIGN{pat=VAR(one_lvar lvars_res),bind=STRING s}::acc
      | L_ce(ClosExp.REAL s,lvars_res,acc)             = ASSIGN{pat=VAR(one_lvar lvars_res),bind=REAL s}::acc
      | L_ce(ClosExp.PASS_PTR_TO_MEM(sma,i),lvars_res,acc) = ASSIGN{pat=VAR(one_lvar lvars_res),bind=PASS_PTR_TO_MEM(sma_to_sma sma,i)}::acc
      | L_ce(ClosExp.PASS_PTR_TO_RHO sma,lvars_res,acc) = ASSIGN{pat=VAR(one_lvar lvars_res),bind=PASS_PTR_TO_RHO(sma_to_sma sma)}::acc
      | L_ce(ClosExp.UB_RECORD ces,lvars_res,acc)      = 
          List.foldr (fn ((ce,lv_res),acc) => L_ce(ce,[lv_res],acc)) acc (zip(ces,lvars_res))
      | L_ce(ClosExp.CLOS_RECORD{label,elems=(lvs,excons,rhos),alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=CLOS_RECORD{label=label,
							      elems=(ces_to_atoms lvs,ces_to_atoms excons,ces_to_atoms rhos),
							      alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.REGVEC_RECORD{elems,alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=REGVEC_RECORD{elems=smas_to_smas elems,alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.SCLOS_RECORD{elems=(lvs,excons,rhos),alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=SCLOS_RECORD{elems=(ces_to_atoms lvs,ces_to_atoms excons,ces_to_atoms rhos),alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.RECORD{elems,alloc,tag},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=RECORD{elems=ces_to_atoms elems,alloc=sma_to_sma alloc,tag=tag}}::acc
      | L_ce(ClosExp.SELECT(i,ce),lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=SELECT(i,ce_to_atom ce)}::acc
      | L_ce(ClosExp.FNJMP{opr,args,clos,free},lvars_res,acc) = 
	  FNJMP{opr=ce_to_atom opr,args=ces_to_atoms args,clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res, bv=[]}::acc
      | L_ce(ClosExp.FNCALL{opr,args,clos,free},lvars_res,acc) =
	  FNCALL{opr=ce_to_atom opr,args=ces_to_atoms args,clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res, bv=[]}::acc
      | L_ce(ClosExp.JMP{opr,args,reg_vec,reg_args,clos,free},lvars_res,acc) =
	  JMP{opr=opr,args=ces_to_atoms args,reg_vec=ce_to_atom_opt reg_vec,reg_args=ces_to_atoms reg_args,
	      clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res,bv=[]}::acc
      | L_ce(ClosExp.FUNCALL{opr,args,reg_vec,reg_args,clos,free},lvars_res,acc) =
	  FUNCALL{opr=opr,args=ces_to_atoms args,reg_vec=ce_to_atom_opt reg_vec,reg_args=ces_to_atoms reg_args,
		  clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res,bv=[]}::acc
      | L_ce(ClosExp.LETREGION{rhos,body},lvars_res,acc) =
	  LETREGION{rhos=map binder_to_binder rhos,body=L_ce(body,lvars_res,[])}::acc
      | L_ce(ClosExp.LET{pat,bind,scope},lvars_res,acc) =
	  SCOPE{pat=map mk_sty pat,scope=L_ce(bind,pat,L_ce(scope,lvars_res,[]))}::acc
      | L_ce(ClosExp.RAISE ce,lvars_res,acc) = RAISE{arg=ce_to_atom ce,defined_atys=map VAR lvars_res}::acc
      | L_ce(ClosExp.HANDLE(ce1,ce2),[lv_res],C) =
	  let
	    val clos_lv = Lvars.new_named_lvar "handleCloslv"
	  in
	    HANDLE{default=L_ce(ce1,[lv_res],[]),
		   handl=([SCOPE{pat=[mk_sty clos_lv],scope=L_ce(ce2,[clos_lv],[])}],VAR clos_lv),
		   handl_return=([],VAR lv_res,[]),offset=()}::C (* for now, offset is unit *)
	  end
      | L_ce(ClosExp.HANDLE(ce1,ce2),lvars_res,C) = die "L_ce: HANDLE with more than one lvars_res"
      | L_ce(ClosExp.SWITCH_I sw,lvars_res,acc) = SWITCH_I(L_ce_sw(sw,fn (ce,acc) => L_ce(ce,lvars_res,acc),fn i => i))::acc
      | L_ce(ClosExp.SWITCH_S sw,lvars_res,acc) = SWITCH_S(L_ce_sw(sw,fn (ce,acc) => L_ce(ce,lvars_res,acc),fn s => s))::acc
      | L_ce(ClosExp.SWITCH_C sw,lvars_res,acc) = SWITCH_C(L_ce_sw(sw,fn (ce,acc) => L_ce(ce,lvars_res,acc),
							       fn (con,con_kind) => (con,con_kind_to_con_kind con_kind)))::acc
      | L_ce(ClosExp.SWITCH_E sw,lvars_res,acc) = SWITCH_E(L_ce_sw(sw,fn (ce,acc) => L_ce(ce,lvars_res,acc),fn e => e))::acc
      | L_ce(ClosExp.CON0{con,con_kind,aux_regions,alloc},lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=CON0{con=con,con_kind=con_kind_to_con_kind con_kind,
						       aux_regions=smas_to_smas aux_regions,
						       alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.CON1{con,con_kind,alloc,arg},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=CON1{con=con,con_kind=con_kind_to_con_kind con_kind,
						       alloc=sma_to_sma alloc,arg=ce_to_atom arg}}::acc
      | L_ce(ClosExp.DECON{con,con_kind,con_exp},lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=DECON{con=con,con_kind=con_kind_to_con_kind con_kind,
							con_aty=ce_to_atom con_exp}}::acc
      | L_ce(ClosExp.DEREF ce,lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=DEREF(ce_to_atom ce)}::acc
      | L_ce(ClosExp.REF(sma,ce),lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=REF(sma_to_sma sma,ce_to_atom ce)}::acc
      | L_ce(ClosExp.ASSIGN(sma,ce1,ce2),lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=ASSIGNREF(sma_to_sma sma,ce_to_atom ce1,ce_to_atom ce2)}::acc
      | L_ce(ClosExp.RESET_REGIONS{force,regions_for_resetting},lvars_res,acc) = 
	  (* We must have RESET_REGIONS return unit. *)
	  RESET_REGIONS{force=force,regions_for_resetting=smas_to_smas regions_for_resetting}::
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=ATOM UNIT}::acc
      | L_ce(ClosExp.CCALL{name,rhos_for_result,args},lvars_res,acc) = 
	  if BI.is_prim name then PRIM{name=name,args=ces_to_atoms rhos_for_result @ ces_to_atoms args,
				       res=map VAR lvars_res}::acc
	  else CCALL{name=name,args=ces_to_atoms args,
		     rhos_for_result=ces_to_atoms rhos_for_result,
		     res=map VAR lvars_res}::acc
      | L_ce(ClosExp.FRAME{declared_lvars,declared_excons},[],acc) = acc
      | L_ce(ClosExp.FRAME{declared_lvars,declared_excons},_,_) = die "L_ce.FRAME: lvars_res not empty."

    fun L_top_decl(ClosExp.FUN(lab,cc,ce)) =
      let
	val lvars_res = CallConv.get_res_lvars(cc)
      in
	FUN(lab,cc,L_ce(ce,lvars_res,[]))
      end	    
      | L_top_decl(ClosExp.FN(lab,cc,ce)) =
      let
	val lvars_res = CallConv.get_res_lvars(cc)
      in
	FN(lab,cc,L_ce(ce,lvars_res,[]))
      end
  in
    fun L_clos_prg funcs =
      List.foldr (fn (func,acc) => L_top_decl func :: acc) [] funcs
  end

  (***********************************************)
  (* Calculate Number of Applications in Program *)
  (***********************************************)
  local
    val count_tail_calls = false

    fun NA_sw (SWITCH(atom_arg,sels,default)) NA_lss =
      (foldl (fn ((s,lss),n) => n + NA_lss lss) 0 sels) + NA_lss default

    fun NA_lss lss =
      let
	fun NA_ls ls =
	  case ls
	    of FNJMP a => if count_tail_calls then 1 else 0
	     | FNCALL a => 1
	     | JMP a => if count_tail_calls then 1 else 0
	     | FUNCALL a => 1 
	     | LETREGION{rhos,body} => NA_lss body
	     | SCOPE{pat,scope} => NA_lss scope
	     | HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset} => NA_lss default + NA_lss handl
	     | HANDLE{default,handl,handl_return,offset} => die "NA_lss: handl_return in HANDLE not empty"
	     | SWITCH_I sw => NA_sw sw NA_lss
	     | SWITCH_S sw => NA_sw sw NA_lss
	     | SWITCH_C sw => NA_sw sw NA_lss
	     | SWITCH_E sw => NA_sw sw NA_lss
	     | _ => 0
    in 
      foldr (fn (ls,n) => n + NA_ls ls) 0 lss
    end
  in
    fun NA_prg top_decls =
    let 
      fun NA_top_decl func = 
	case func 
	  of FUN(lab,cc,lss) => NA_lss lss
	   | FN(lab,cc,lss) => NA_lss lss
    in 
      foldr (fn (func,n) => n + NA_top_decl func) 0 top_decls
    end
  end

  (*****************************************)
  (* Get Machine Registers from a LineStmt *)
  (*****************************************)
  fun get_phreg_atom(PHREG phreg,acc) = phreg::acc
    | get_phreg_atom(_,acc) = acc

  fun get_phreg_atoms(atoms,acc) = foldr (fn (atom,acc) => get_phreg_atom(atom,acc)) acc atoms

  fun get_phreg_atom_opt(NONE,acc) = acc
    | get_phreg_atom_opt(SOME atom,acc) = get_phreg_atom(atom,acc)

  fun get_phreg_sma(ATTOP_LI(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(ATTOP_LF(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(ATTOP_FI(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(ATTOP_FF(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(ATBOT_LI(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(ATBOT_LF(atom,pp),acc) = get_phreg_atom(atom,acc)
    | get_phreg_sma(SAT_FI(atom,pp),acc)   = get_phreg_atom(atom,acc)
    | get_phreg_sma(SAT_FF(atom,pp),acc)   = get_phreg_atom(atom,acc)
    | get_phreg_sma(IGNORE,acc)            = acc
    
  fun get_phreg_smas(smas,acc) = foldr (fn (sma,acc) => get_phreg_sma(sma,acc)) acc smas

  fun get_phreg_se(ATOM atom,acc) = get_phreg_atom(atom,acc)
    | get_phreg_se(LOAD lab,acc) = acc
    | get_phreg_se(STORE(atom,lab),acc) = get_phreg_atom(atom,acc)
    | get_phreg_se(STRING str,acc) = acc
    | get_phreg_se(REAL str,acc) = acc
    | get_phreg_se(CLOS_RECORD{label,elems,alloc},acc) = get_phreg_sma(alloc, get_phreg_atoms(smash_free elems,acc))
    | get_phreg_se(REGVEC_RECORD{elems,alloc},acc) = get_phreg_sma(alloc, get_phreg_smas(elems,acc))
    | get_phreg_se(SCLOS_RECORD{elems,alloc},acc) = get_phreg_sma(alloc, get_phreg_atoms(smash_free elems,acc))
    | get_phreg_se(RECORD{elems,alloc,tag},acc) = get_phreg_sma(alloc, get_phreg_atoms(elems,acc))
    | get_phreg_se(SELECT(i,atom),acc) = get_phreg_atom(atom,acc)
    | get_phreg_se(CON0{con,con_kind,aux_regions,alloc},acc) = get_phreg_sma(alloc, get_phreg_smas(aux_regions,acc))
    | get_phreg_se(CON1{con,con_kind,alloc,arg},acc) = get_phreg_sma(alloc,get_phreg_atom(arg,acc))
    | get_phreg_se(DECON{con,con_kind,con_aty},acc) = get_phreg_atom(con_aty,acc)
    | get_phreg_se(DEREF atom,acc) = get_phreg_atom(atom,acc)
    | get_phreg_se(REF(sma,atom),acc) = get_phreg_sma(sma,get_phreg_atom(atom,acc))
    | get_phreg_se(ASSIGNREF(sma,atom1,atom2),acc) = get_phreg_sma(sma,get_phreg_atom(atom1,get_phreg_atom(atom2,acc)))
    | get_phreg_se(PASS_PTR_TO_MEM(sma,i),acc) = get_phreg_sma(sma,acc)
    | get_phreg_se(PASS_PTR_TO_RHO sma,acc) = get_phreg_sma(sma,acc)

  fun get_phreg_in_fun{opr,args,reg_vec,reg_args,clos,free,res,bv} = (* Operand is always a label *)
       get_phreg_atoms(args,get_phreg_atom_opt(reg_vec,
	 get_phreg_atoms(reg_args,get_phreg_atom_opt(clos,get_phreg_atoms(free,
          get_phreg_atoms(res,[]))))))

  fun get_phreg_in_fn{opr,args,clos,free,res,bv} =
       get_phreg_atoms(args,get_phreg_atom_opt(clos,get_phreg_atoms(free,
        get_phreg_atom(opr,get_phreg_atoms(res,[])))))

  fun get_phreg_ls(ASSIGN{pat,bind}) = get_phreg_se(bind,[])
    | get_phreg_ls(FLUSH(atom,_)) = get_phreg_atom(atom,[])
    | get_phreg_ls(FETCH(atom,_)) = get_phreg_atom(atom,[])
    | get_phreg_ls(FNJMP cc) = get_phreg_in_fn cc
    | get_phreg_ls(FNCALL cc) = get_phreg_in_fn cc
    | get_phreg_ls(JMP cc) = get_phreg_in_fun cc
    | get_phreg_ls(FUNCALL cc) = get_phreg_in_fun cc
    | get_phreg_ls(RAISE{arg,defined_atys}) = get_phreg_atom(arg,[])
    | get_phreg_ls(RESET_REGIONS{force,regions_for_resetting}) = get_phreg_smas(regions_for_resetting,[])
    | get_phreg_ls(PRIM{name,args,res}) = get_phreg_atoms(args,[])
    | get_phreg_ls(CCALL{name,args,rhos_for_result,res}) = get_phreg_atoms(args,get_phreg_atoms(rhos_for_result,[]))
    | get_phreg_ls _ = die "get_phreg_ls: statement contains statements itself."

  (**************************************************************)
  (* Def and Use sets for LineStmt RETURN BOTH lvars and phregs *)
  (**************************************************************)
  fun get_var_atom(VAR lv,acc) = lv::acc
    | get_var_atom(PHREG phreg,acc) = phreg::acc
    | get_var_atom(_,acc) = acc

  fun get_var_atoms(atoms,acc) = foldr (fn (atom,acc) => get_var_atom(atom,acc)) acc atoms

  fun get_var_atom_opt(NONE,acc) = acc
    | get_var_atom_opt(SOME atom,acc) = get_var_atom(atom,acc)

  fun get_var_sma(ATTOP_LI(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(ATTOP_LF(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(ATTOP_FI(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(ATTOP_FF(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(ATBOT_LI(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(ATBOT_LF(atom,pp),acc) = get_var_atom(atom,acc)
    | get_var_sma(SAT_FI(atom,pp),acc)   = get_var_atom(atom,acc)
    | get_var_sma(SAT_FF(atom,pp),acc)   = get_var_atom(atom,acc)
    | get_var_sma(IGNORE,acc)            = acc

  fun get_var_smas(smas,acc) = foldr (fn (sma,acc) => get_var_sma(sma,acc)) acc smas

  fun def_var_se (se: Atom SimpleExp,acc:lvar list) = acc

  fun use_var_se(ATOM atom,acc) = get_var_atom(atom,acc)
    | use_var_se(LOAD lab,acc) = acc
    | use_var_se(STORE(atom,lab),acc) = get_var_atom(atom,acc)
    | use_var_se(STRING str,acc) = acc
    | use_var_se(REAL str,acc) = acc
    | use_var_se(CLOS_RECORD{label,elems,alloc},acc) = get_var_sma(alloc, get_var_atoms(smash_free elems,acc))
    | use_var_se(REGVEC_RECORD{elems,alloc},acc) = get_var_sma(alloc, get_var_smas(elems,acc))
    | use_var_se(SCLOS_RECORD{elems,alloc},acc) = get_var_sma(alloc, get_var_atoms(smash_free elems,acc))
    | use_var_se(RECORD{elems,alloc,tag},acc) = get_var_sma(alloc, get_var_atoms(elems,acc))
    | use_var_se(SELECT(i,atom),acc) = get_var_atom(atom,acc)
    | use_var_se(CON0{con,con_kind,aux_regions,alloc},acc) = get_var_sma(alloc, get_var_smas(aux_regions,acc))
    | use_var_se(CON1{con,con_kind,alloc,arg},acc) = get_var_sma(alloc,get_var_atom(arg,acc))
    | use_var_se(DECON{con,con_kind,con_aty},acc) = get_var_atom(con_aty,acc)
    | use_var_se(DEREF atom,acc) = get_var_atom(atom,acc)
    | use_var_se(REF(sma,atom),acc) = get_var_sma(sma,get_var_atom(atom,acc))
    | use_var_se(ASSIGNREF(sma,atom1,atom2),acc) = get_var_sma(sma,get_var_atom(atom1,get_var_atom(atom2,acc)))
    | use_var_se(PASS_PTR_TO_MEM(sma,i),acc) = get_var_sma(sma,acc)
    | use_var_se(PASS_PTR_TO_RHO sma,acc) = get_var_sma(sma,acc)

  fun use_var_on_fun{opr,args,reg_vec,reg_args,clos,free,res,bv} = (* Operand is always a label *)
    get_var_atoms(args,get_var_atom_opt(reg_vec,
					get_var_atoms(reg_args,get_var_atom_opt(clos,get_var_atoms(free,[])))))

  fun def_var_on_fun{opr,args,reg_vec,reg_args,clos,free,res,bv} = get_var_atoms(res,[])

  fun use_var_on_fn{opr,args,clos,free,res,bv} =
    get_var_atoms(args,get_var_atom_opt(clos,get_var_atoms(free,get_var_atom(opr,[]))))

  fun def_var_on_fn{opr,args,clos,free,res,bv} = get_var_atoms(res,[])

  fun use_var_ls(ASSIGN{pat,bind}) = use_var_se(bind,[])
    | use_var_ls(FLUSH(atom,_)) = get_var_atom(atom,[])
    | use_var_ls(FETCH(atom,_)) = []
    | use_var_ls(FNJMP cc) = use_var_on_fn cc
    | use_var_ls(FNCALL cc) = use_var_on_fn cc
    | use_var_ls(JMP cc) = use_var_on_fun cc
    | use_var_ls(FUNCALL cc) = use_var_on_fun cc
    | use_var_ls(RAISE{arg,defined_atys}) = get_var_atom(arg,[])
    | use_var_ls(RESET_REGIONS{force,regions_for_resetting}) = get_var_smas(regions_for_resetting,[])
    | use_var_ls(PRIM{name,args,res}) = get_var_atoms(args,[])
    | use_var_ls(CCALL{name,args,rhos_for_result,res}) = get_var_atoms(args,get_var_atoms(rhos_for_result,[]))
    | use_var_ls _ = die "use_var_ls: statement contains statements itself."

  fun def_var_ls(ASSIGN{pat,bind}) = get_var_atom(pat,[]) 
    | def_var_ls(FLUSH(atom,_)) = []
    | def_var_ls(FETCH(atom,_)) = get_var_atom(atom,[])
    | def_var_ls(FNJMP cc) = def_var_on_fn cc
    | def_var_ls(FNCALL cc) = def_var_on_fn cc
    | def_var_ls(JMP cc) = def_var_on_fun cc
    | def_var_ls(FUNCALL cc) = def_var_on_fun cc
    | def_var_ls(RAISE{arg,defined_atys}) = get_var_atoms(defined_atys,[])
    | def_var_ls(RESET_REGIONS{force,regions_for_resetting}) = []
    | def_var_ls(PRIM{res,...}) = get_var_atoms(res,[])
    | def_var_ls(CCALL{res,...}) = get_var_atoms(res,[])
    | def_var_ls _ = die "def_var_ls: statement contains statements itself."

  fun def_use_var_ls ls = (def_var_ls ls,use_var_ls ls)

  (***************************************************)
  (* Def and Use sets for LineStmt RETURN ONLY lvars *)
  (***************************************************)
  fun filter_out_phregs lvs = List.filter (fn lvar => not (RI.is_reg lvar)) lvs

  fun get_lvar_atom(atom,acc) = filter_out_phregs (get_var_atom(atom,acc))
  fun get_lvar_atoms(atoms,acc) = filter_out_phregs (get_var_atoms(atoms,acc))
  fun get_lvar_atom_opt(atom_opt,acc) = filter_out_phregs (get_var_atom_opt(atom_opt,acc))

  fun get_lvar_sma(sma,acc) = filter_out_phregs (get_var_sma(sma,acc))    
  fun get_lvar_smas(smas,acc) = filter_out_phregs(get_var_smas(smas,acc))

  fun def_lvar_se (se:Atom SimpleExp,acc:lvar list) = filter_out_phregs acc
  fun use_lvar_se(se,acc) = filter_out_phregs(use_var_se(se,acc))

  fun use_lvar_on_fun cc = filter_out_phregs(use_var_on_fun cc)
  fun def_lvar_on_fun cc = filter_out_phregs(def_var_on_fun cc)

  fun use_lvar_on_fn cc = filter_out_phregs(use_var_on_fn cc)
  fun def_lvar_on_fn cc = filter_out_phregs(def_var_on_fn cc)

  fun use_lvar_ls ls = filter_out_phregs(use_var_ls ls)
  fun def_lvar_ls ls = filter_out_phregs(def_var_ls ls)
  fun def_use_lvar_ls ls = (filter_out_phregs(def_var_ls ls),filter_out_phregs(use_var_ls ls))

  (*****************************************************)
  (* Map f_aty, f_offset and f_sty on LineStmt program *)
  (*****************************************************)
  local
    fun map_lss f_aty f_offset f_sty lss =
      let
	fun map_sma sma =
	  (case sma of
	     ATTOP_LI(aty,pp) => ATTOP_LI(f_aty aty,pp)
	   | ATTOP_LF(aty,pp) => ATTOP_LF(f_aty aty,pp)
	   | ATTOP_FI(aty,pp) => ATTOP_FI(f_aty aty,pp)
	   | ATTOP_FF(aty,pp) => ATTOP_FF(f_aty aty,pp)
	   | ATBOT_LI(aty,pp) => ATBOT_LI(f_aty aty,pp)
	   | ATBOT_LF(aty,pp) => ATBOT_LF(f_aty aty,pp)
	   | SAT_FI(aty,pp) => SAT_FI(f_aty aty,pp)
	   | SAT_FF(aty,pp) => SAT_FF(f_aty aty,pp)
	   | IGNORE => IGNORE)
	fun map_smas smas = map map_sma smas
	fun map_sty sty = f_sty sty
	fun map_stys stys = map f_sty stys
	fun map_aty aty = f_aty aty
	fun map_aty_opt(NONE) = NONE
	  | map_aty_opt(SOME aty) = SOME(f_aty aty)
	fun map_atys atys = map f_aty atys
	fun map_rho(binder,offset) = (binder,f_offset offset)
	fun map_rhos rhos = map map_rho rhos

	fun map_sw(map_lss,switch_con,SWITCH(atom,sels,default)) =
	  let
	    val sels' =
	      foldr (fn ((sel,lss),sels_acum) => (sel,map_lss lss)::sels_acum) [] sels
	    val default' = map_lss default
	  in
	    switch_con(SWITCH(map_aty atom,sels',default'))
	  end 

	fun map_fn_app{opr,args,clos,free,res,bv} =
	  {opr=map_aty opr,
	   args=map_atys args,
	   clos=map_aty_opt clos,
	   free=map_atys free,
	   res=map_atys res,
	   bv=bv}

	fun map_fun_app{opr,args,clos,free,res,reg_vec,reg_args,bv} =
	  {opr=opr,
	   args=map_atys args,
	   clos=map_aty_opt clos,
	   free=map_atys free,
	   res=map_atys res,
	   reg_vec=map_aty_opt reg_vec,
	   reg_args=map_atys reg_args,
	   bv=bv}

	fun map_se(ATOM aty) = ATOM (map_aty aty)
	  | map_se(LOAD label) = LOAD label
	  | map_se(STORE(aty,label)) = STORE(map_aty aty,label)
	  | map_se(STRING str) = STRING str
	  | map_se(REAL str) = REAL str
	  | map_se(CLOS_RECORD{label,elems=(lvs,excons,rhos),alloc}) = 
	  CLOS_RECORD{label=label,
		      elems=(map_atys lvs,map_atys excons,map_atys rhos),
		      alloc= map_sma alloc}
	  | map_se(REGVEC_RECORD{elems,alloc}) = REGVEC_RECORD{elems=map_smas elems,alloc=map_sma alloc}
	  | map_se(SCLOS_RECORD{elems=(lvs,excons,rhos),alloc}) = 
	  SCLOS_RECORD{elems=(map_atys lvs,map_atys excons,map_atys rhos),
		       alloc = map_sma alloc}
	  | map_se(RECORD{elems,alloc,tag}) = RECORD{elems=map_atys elems,alloc=map_sma alloc,tag=tag}
	  | map_se(SELECT(i,aty)) = SELECT(i,map_aty aty)
	  | map_se(CON0{con,con_kind,aux_regions,alloc}) = CON0{con=con,con_kind=con_kind,aux_regions=map_smas aux_regions,alloc=map_sma alloc}
	  | map_se(CON1{con,con_kind,alloc,arg}) = CON1{con=con,con_kind=con_kind,alloc=map_sma alloc,arg=map_aty arg}
	  | map_se(DECON{con,con_kind,con_aty}) = DECON{con=con,con_kind=con_kind,con_aty=map_aty con_aty}
	  | map_se(DEREF aty) = DEREF(map_aty aty)
	  | map_se(REF(sma,aty)) = REF(map_sma sma,map_aty aty)
	  | map_se(ASSIGNREF(sma,aty1,aty2)) = ASSIGNREF(map_sma sma,map_aty aty1,map_aty aty2)
	  | map_se(PASS_PTR_TO_MEM(sma,i)) = PASS_PTR_TO_MEM(map_sma sma,i)
	  | map_se(PASS_PTR_TO_RHO(sma)) = PASS_PTR_TO_RHO(map_sma sma)

	fun map_lss'([]) = []
	  | map_lss'(ASSIGN{pat,bind}::lss) = ASSIGN{pat=map_aty pat,bind=map_se bind} :: map_lss' lss
	  | map_lss'(FLUSH(aty,offset)::lss) = FLUSH(map_aty aty,f_offset offset) :: map_lss' lss
	  | map_lss'(FETCH(aty,offset)::lss) = FETCH(map_aty aty,f_offset offset) :: map_lss' lss
	  | map_lss'(FNJMP a::lss) = FNJMP(map_fn_app a) :: map_lss' lss
	  | map_lss'(FNCALL a::lss) = FNCALL(map_fn_app a) :: map_lss' lss
	  | map_lss'(JMP a::lss) = JMP(map_fun_app a) :: map_lss' lss
	  | map_lss'(FUNCALL a::lss) = FUNCALL(map_fun_app a) :: map_lss' lss
	  | map_lss'(LETREGION{rhos,body}::lss) = LETREGION{rhos=map_rhos rhos,body=map_lss' body} :: map_lss' lss
	  | map_lss'(SCOPE{pat,scope}::lss) = SCOPE{pat=map_stys pat,scope=map_lss' scope} :: map_lss' lss
	  | map_lss'(HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_lv,bv),offset}::lss) =
	  HANDLE{default=map_lss' default,
		 handl=(map_lss' handl,map_aty handl_lv),
		 handl_return=(map_lss' handl_return,map_aty handl_return_lv,bv),
		 offset=f_offset offset} :: map_lss' lss
	  | map_lss'(RAISE{arg,defined_atys}::lss) = RAISE{arg=map_aty arg,defined_atys=map_atys defined_atys} :: map_lss' lss
	  | map_lss'(SWITCH_I sw::lss) = map_sw(map_lss',SWITCH_I,sw) :: map_lss' lss
	  | map_lss'(SWITCH_S sw::lss) = map_sw(map_lss',SWITCH_S,sw) :: map_lss' lss
	  | map_lss'(SWITCH_C sw::lss) = map_sw(map_lss',SWITCH_C,sw) :: map_lss' lss
	  | map_lss'(SWITCH_E sw::lss) = map_sw(map_lss',SWITCH_E,sw) :: map_lss' lss
	  | map_lss'(RESET_REGIONS{force,regions_for_resetting}::lss) =
	  RESET_REGIONS{force=force,regions_for_resetting=map_smas regions_for_resetting} :: map_lss' lss
	  | map_lss'(PRIM{name,args,res}::lss) = 
	  PRIM{name=name,args=map_atys args,res=map_atys res} :: map_lss' lss
	  | map_lss'(CCALL{name,args,rhos_for_result,res}::lss) = 
	  CCALL{name=name,args=map_atys args,rhos_for_result=map_atys rhos_for_result,res=map_atys res} :: map_lss' lss
      in
	map_lss' lss
      end
  in
    val map_lss = map_lss
    fun map_prg f_aty f_offset f_sty {main_lab:label,
				      code=top_decls:('sty1,'offset1,'aty1) LinePrg,
				      imports:label list * label list,
				      exports:label list * label list} =
    let 
      fun map_top_decl func = 
	case func 
	  of FUN(lab,cc,lss) => FUN(lab,cc,map_lss f_aty f_offset f_sty lss)
	   | FN(lab,cc,lss) => FN(lab,cc,map_lss f_aty f_offset f_sty lss)
    in 
      {main_lab = main_lab,
       code = foldr (fn (func,acc) => map_top_decl func :: acc) [] top_decls,
       imports = imports,
       exports = exports}
    end
  end

  (**************************************************)
  (* Calculate Flow Variables in Linearised Program *)
  (**************************************************)
  local
    val no_of_flow_var = ref 0
  in
    fun reset_flow_var_stat() = no_of_flow_var := 0;
    fun inc_flow_var n = no_of_flow_var := !no_of_flow_var + n
    fun get_no_of_flow_var() = !no_of_flow_var
  end

  local
    fun add_ok_use(lv,(OKset,notOKset,_)) = 
      if Lvarset.member(lv,OKset) then
	 (Lvarset.delete(OKset,lv),Lvarset.add(notOKset,lv),NONE)
      else
	if Lvarset.member(lv,notOKset) then
	  (OKset,notOKset,NONE)
	else
	  (Lvarset.add(OKset,lv),notOKset,SOME lv)
    fun add_not_ok_use(lvs,(OKset,notOKset,_)) =
      foldl (fn (lv,(OKset,notOKset,next_prev_use_lv)) => 
	     (Lvarset.delete(OKset,lv),Lvarset.add(notOKset,lv),next_prev_use_lv)) (OKset,notOKset,NONE) lvs
    fun add_not_ok_def(lvs,(OKset,notOKset,_)) =
      foldl (fn (lv,(OKset,notOKset,next_prev_use_lv)) => 
	     (Lvarset.delete(OKset,lv),Lvarset.add(notOKset,lv),next_prev_use_lv)) (OKset,notOKset,NONE) lvs
    fun add_ok_def(lv,NONE,(OKset,notOKset,_)) = add_not_ok_def([lv],(OKset,notOKset,NONE))
      | add_ok_def(lv,SOME lv_to_match,(OKset,notOKset,_)) = 
      if Lvars.eq(lv,lv_to_match) then
	(OKset,notOKset,NONE)
      else
	(Lvarset.delete(OKset,lv),Lvarset.add(notOKset,lv),NONE)


    (********************************)
    (* Calculate OKset and notOKset *)
    (********************************)
    fun FV_CalcSets_sw(FV_CalcSets_lss,SWITCH(atom,sels,default),OKset,notOKset,prev_use_lv) =
      (* Note, that we pass the newest sets to FV_CalcSets_lss for each branch! *)
      (* It is wrong to apply union on notOKset and intersection on OKset       *)
      (* because we may only have ONE use of each flow variable.                *)
      let
	val (OKset_sels,notOKset_sels,_) =
	  foldr (fn ((sel,lss),(OKset,notOKset,_)) => 
		 FV_CalcSets_lss(lss,(OKset,notOKset,prev_use_lv))) (OKset,notOKset,NONE) sels
      in
	add_not_ok_use(get_lvar_atom(atom,[]),FV_CalcSets_lss(default,(OKset_sels,notOKset_sels,prev_use_lv)))
      end

    fun pr_prev(NONE) = "none"
      | pr_prev(SOME lv) = Lvars.pr_lvar lv 

    fun FV_CalcSets_ls(ls,OKset,notOKset,prev_use_lv) =
      (case ls of
	 (* Pattern: lv := TRUE *)
         (* Pattern: lv := FALSE *)
	 ASSIGN{pat=VAR lv,bind=CON0{con,con_kind,aux_regions=[],alloc=IGNORE}} =>
	   if Con.eq(con,Con.con_TRUE) orelse
	     Con.eq(con,Con.con_FALSE) then
	     add_ok_def(lv,prev_use_lv,(OKset,notOKset,NONE))
	   else
	     add_not_ok_use(use_var_ls ls,add_not_ok_def(def_var_ls ls,(OKset,notOKset,NONE)))
         (* Pattern: lv := prim(cond) *)
       | PRIM{name,args=[aty1,aty2],res=[VAR lv_res]} =>
	   if name = BI.EQUAL_INT orelse
	     name = BI.LESS_INT orelse
	     name = BI.LESSEQ_INT orelse
	     name = BI.GREATER_INT orelse
	     name = BI.GREATEREQ_INT then
	     add_ok_def(lv_res,prev_use_lv,add_not_ok_use(use_var_ls ls,(OKset,notOKset,NONE)))
	   else
	     add_not_ok_def([lv_res],add_not_ok_use(use_var_ls ls,(OKset,notOKset,NONE)))
         (* Pattern: case lv of true => lss | _ => lss *)
         (* Pattern: case lv of false => lss | _ => lss *)
       | SWITCH_C(sw as SWITCH(VAR lv,[((con,con_kind),lss)],default)) => 
	   if Con.eq(con,Con.con_TRUE) orelse
	     Con.eq(con,Con.con_FALSE) then
	     let
	       val (OKset',notOKset',_) = FV_CalcSets_lss(default,(OKset,notOKset,prev_use_lv))
	     in
	       add_ok_use(lv,FV_CalcSets_lss(lss,(OKset',notOKset',prev_use_lv)))
	     end
	   else
	     FV_CalcSets_sw(FV_CalcSets_lss,sw,OKset,notOKset,prev_use_lv)
         (* Pattern: case lv of 3 => lss | _ => lss *)
         (* Pattern: case lv of 1 => lss | _ => lss *)
       | SWITCH_I(sw as SWITCH(VAR lv,[(sel_val,lss)],default)) => 
	   if sel_val = BI.ml_true orelse
	     sel_val = BI.ml_false then
	     let
	       val (OKset',notOKset',_) = FV_CalcSets_lss(default,(OKset,notOKset,prev_use_lv))
	     in
	       add_ok_use(lv,FV_CalcSets_lss(lss,(OKset',notOKset',prev_use_lv)))
	     end
	   else
	     FV_CalcSets_sw(FV_CalcSets_lss,sw,OKset,notOKset,prev_use_lv)
       | LETREGION{rhos,body} => (* only infinite regions execute code *)
	     (case remove_finite_rhos rhos of
		[] => FV_CalcSets_lss(body,(OKset,notOKset,prev_use_lv))
	      | _ => 
		  let
		    val (OKset',notOKset',_) = FV_CalcSets_lss(body,(OKset,notOKset,NONE))
		  in
		    (OKset',notOKset',NONE)
		  end)
       | SCOPE{pat,scope} => FV_CalcSets_lss(scope,(OKset,notOKset,prev_use_lv))

       | HANDLE{default,handl=(handl,VAR handl_lv),handl_return=([],VAR handl_return_lv,bv),offset} =>
	     let
	       val (OKset_d,notOKset_d,_) = FV_CalcSets_lss(default,(OKset,notOKset,NONE))
	       val (OKset_h,notOKset_h,_) = FV_CalcSets_lss(handl,(OKset_d,notOKset_d,NONE))
	     in
	       add_not_ok_def([handl_return_lv,handl_lv],(OKset_h,notOKset_h,NONE))
	     end
       | HANDLE{default,handl,handl_return,offset} => die "FV_CalcSets_ls: Handle"

       | SWITCH_I sw => FV_CalcSets_sw(FV_CalcSets_lss,sw,OKset,notOKset,prev_use_lv)
       | SWITCH_S sw => die "FV_CalcSets_ls: SWITCH_S"
       | SWITCH_C sw => FV_CalcSets_sw(FV_CalcSets_lss,sw,OKset,notOKset,prev_use_lv)
       | SWITCH_E sw => die "FV_CalcSets_ls: SWITCH_E"
       | _ => add_not_ok_use(use_var_ls ls,add_not_ok_def(def_var_ls ls,(OKset,notOKset,NONE))))
    and FV_CalcSets_lss(lss,(OKset,notOKset,prev_use_lv)) =
      foldr (fn (ls,(OKset,notOKset,prev_use_lv)) => FV_CalcSets_ls(ls,OKset,notOKset,prev_use_lv)) (OKset,notOKset,prev_use_lv) lss

    (********************************)
    (* Annotate Flow Variables      *)
    (********************************)
    fun ann_aty (VAR lv,OKmap) =
      (case LvarFinMap.lookup OKmap lv of
	 NONE => VAR lv
       | SOME(lab1,lab2) => FLOW_VAR(lv,lab1,lab2))
      | ann_aty (FLOW_VAR(lv,lab1,lab2),OKmap) = die "ann_aty: aty is FLOW_VAR"
      | ann_aty (atom,OKmap) = atom

    fun ann_sty (V lv,OKmap) =
      (case LvarFinMap.lookup OKmap lv of
	 NONE => V lv
       | SOME(lab1,lab2) => FV(lv,lab1,lab2))
      | ann_sty (FV _,_) = die "ann_sty: sty is FV"

    fun FV_top_decl' gen_top_decl (cc,lss) =
      let
	(* Add cc to nonOKset *)
	val notOKset = Lvarset.lvarsetof (CallConv.get_res_lvars cc @ CallConv.get_arg_lvars cc)
	val (OKset,_,_) = FV_CalcSets_lss(lss,(Lvarset.empty,notOKset,NONE)) 
	val _ = inc_flow_var (List.length(Lvarset.members OKset))
	val OKmap = 
	  Lvarset.foldset 
	  (fn (OKmap,lv) => 
	   LvarFinMap.add (lv,
			   (Labels.new_named (Lvars.pr_lvar lv ^ "T"), 
			    Labels.new_named (Lvars.pr_lvar lv ^ "F")),
			   OKmap)) (LvarFinMap.empty,OKset)
	val lss' = map_lss (fn aty => ann_aty(aty,OKmap)) (fn i => i) (fn sty => ann_sty(sty,OKmap)) lss
      in
	gen_top_decl lss'
      end
  in
    fun FV_prg top_decls =
      let 
	fun FV_top_decl func = 
	  case func 
	    of FUN(lab,cc,lss) => FV_top_decl' (fn lss => FUN(lab,cc,lss)) (cc,lss)
	     | FN(lab,cc,lss) => FV_top_decl' (fn lss => FN(lab,cc,lss)) (cc,lss)
      in 
	foldr (fn (func,acc) => FV_top_decl func :: acc) [] top_decls
      end
  end

  (******************************)
  (* Linearise ClosExp          *)
  (******************************)
  fun L {main_lab:label,
	 code=clos_prg:ClosPrg,
	 imports:label list * label list,
	 exports:label list * label list} =
    let
      val _ = chat "[Linearisation..."
      val _ = reset_flow_var_stat()
      val line_prg = L_clos_prg clos_prg	
      val line_prg_flow_var = 
	if !flow_var_flag then
	  FV_prg line_prg
	else
	  line_prg
      val _ = 
	if Flags.is_on "print_linearised_program" then
	  (print ("Number of functions:      " ^ (Int.toString(length(line_prg))) ^ "\n");
	   print ("Number of applications:   " ^ (Int.toString(NA_prg line_prg)) ^ "\n");
	   print ("Number of flow variables: " ^ (Int.toString(get_no_of_flow_var())) ^ "\n");
	  display("\nReport: AFTER LINEARISATION:", layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_flow_var))
	else
	  ()
      val _ = chat "]\n"
    in
      {main_lab=main_lab,code=line_prg_flow_var,imports=imports,exports=exports}
    end
end;