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
  type phsize = PhysSizeInf.phsize
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

  datatype StoreType =
      STACK_STY of lvar
    | PHREG_STY of lvar * int
    | NO_STY of lvar  (* NO_STY says that no sty has been assigned yet. *)

  datatype Atom =
      VAR           of lvar
    | RVAR          of place
    | DROPPED_RVAR  of place
    | PHREG         of int
    | INTEGER       of int 
    | UNIT

  and SimpleExp =
      ATOM            of Atom
    | FETCH           of label
    | STORE           of Atom * label
    | STRING          of string
    | REAL            of string
    | CLOS_RECORD     of {label: label, elems: Atom list, alloc: sma}
    | REGVEC_RECORD   of {elems: sma list, alloc: sma}
    | SCLOS_RECORD    of {elems: Atom list, alloc: sma}
    | RECORD          of {elems: Atom list, alloc: sma}
    | SELECT          of int * Atom
    | CON0            of {con: con, con_kind: con_kind, aux_regions: sma list, alloc: sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: sma, arg: Atom}
    | DECON           of {con: con, con_kind: con_kind, con_atom: Atom}
    | DEREF           of Atom
    | REF             of sma * Atom
    | ASSIGNREF       of sma * Atom * Atom
    | PASS_PTR_TO_MEM of sma * int
    | PASS_PTR_TO_RHO of sma

  and LineStmt = 
      ASSIGN        of {pat: Atom, bind: SimpleExp}
    | FNJMP         of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | FNCALL        of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | JMP           of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | FUNCALL       of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | LETREGION     of {rhos: binder list, body: LineStmt list}
    | SCOPE         of {pat: StoreType list, scope: LineStmt list}
    | HANDLE        of LineStmt list * LineStmt list
    | RAISE         of Atom
    | SWITCH_I      of int Switch
    | SWITCH_S      of string Switch
    | SWITCH_C      of (con*con_kind) Switch
    | SWITCH_E      of excon Switch
    | RESET_REGIONS of {force: bool, 
			regions_for_resetting: sma list}
    | CCALL         of {name: string,  
			args: Atom list,
			rhos_for_result : Atom list,
			res: Atom list}
    | FRAME         of {declared_lvars: {lvar: lvar, label: label} list,
			declared_excons: {excon: excon, label: label} list}

  and 'a Switch = SWITCH of Atom * ('a * (LineStmt list)) list * (LineStmt list)

  and sma = 
      ATTOP_LI of Atom * pp
    | ATTOP_LF of Atom * pp
    | ATTOP_FI of Atom * pp
    | ATTOP_FF of Atom * pp
    | ATBOT_LI of Atom * pp
    | ATBOT_LF of Atom * pp
    | SAT_FI   of Atom * pp
    | SAT_FF   of Atom * pp
    | IGNORE

  datatype TopDecl = 
      FUN of label * cc * LineStmt list
    | FN of label * cc * LineStmt list
  
  type LinePrg = TopDecl list

  (************************)
  (* PrettyPrint LineStmt *)
  (************************)
  type StringTree = PP.StringTree

  local
    open PP

    fun pr_con_kind(ENUM i)    = "enum " ^ Int.toString i
      | pr_con_kind(UNBOXED i) = "unboxed " ^ Int.toString i
      | pr_con_kind(BOXED i)   = "boxed " ^ Int.toString i

    fun pr_phsize(PhysSizeInf.INF)     = "inf"
      | pr_phsize(PhysSizeInf.WORDS i) = Int.toString i

    fun pr_binder(place,phsize) = 
      (flatten1(Effect.layout_effect place) ^ ":" ^ pr_phsize phsize)

    fun pr_pp pp = "pp" ^ Int.toString pp

    fun pr_sty(STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
      | pr_sty(PHREG_STY(lv,i)) = Lvars.pr_lvar lv ^ ":phreg"^ Int.toString i
      | pr_sty(NO_STY lv) = Lvars.pr_lvar lv

    fun pr_atom(VAR lv) = Lvars.pr_lvar lv
      | pr_atom(RVAR place) = flatten1(Effect.layout_effect place)
      | pr_atom(DROPPED_RVAR place) = "D" ^ flatten1(Effect.layout_effect place)
      | pr_atom(PHREG i) = "phreg" ^ Int.toString i
      | pr_atom(INTEGER i) = Int.toString i
      | pr_atom(UNIT) = "()"

    fun layout_atom atom = LEAF(pr_atom atom)
      
    fun layout_atom_opt(SOME atom) = layout_atom atom
      | layout_atom_opt(NONE) = LEAF ""

    fun layout_switch layout_lss pr_const (SWITCH(atom_arg,sels,default)) =
	  let
	    fun layout_sels(const,ls_sel) =
		  NODE{start="",finish="",indent=0,
		       children=[LEAF (pr_const const),layout_lss ls_sel],
		       childsep=RIGHT " => "}
	    val t1 = NODE{start="(case ",finish=" ",indent=2, childsep = NOSEP, 
			  children=[layout_atom atom_arg]}
	    val t2 = NODE{start="of " ,finish="",indent=6,childsep=LEFT " | ",
			  children=(map layout_sels sels) @ 
			           [NODE{start="",finish="",indent=0,
					 children=[LEAF "_",layout_lss default],
					 childsep=RIGHT " => "}]}
	    val t3 = NODE{start="",finish=") (*case*) ",indent=3,childsep=NOSEP,children=[t2]}
	  in 
	    NODE{start = "", finish = "", indent=0, childsep=NOSEP,children=[t1,t3]}
	  end

    fun layout_se(ATOM atom) = layout_atom atom
      | layout_se(FETCH lab) = LEAF("fetch(" ^ Labels.pr_label lab ^ ")")
      | layout_se(STORE(atom,lab)) = LEAF("store(" ^ pr_atom atom ^ "," ^ Labels.pr_label lab ^ ")")
      | layout_se(STRING s)  = LEAF("\"" ^ s ^ "\"")
      | layout_se(REAL s)    = LEAF(s)
      | layout_se(CLOS_RECORD{label,elems,alloc}) = HNODE{start="[",
							  finish="]clos " ^ pr_sma alloc,
							  childsep=RIGHT ",",
							  children=LEAF(Labels.pr_label label)::
							  map layout_atom elems}
      | layout_se(REGVEC_RECORD{elems,alloc}) = HNODE{start="[",
						      finish="]regvec " ^ pr_sma alloc,
						      childsep=RIGHT ",",
						      children=map (fn sma => layout_sma sma) elems}
      | layout_se(SCLOS_RECORD{elems,alloc}) = HNODE{start="[",
						     finish="]sclos " ^ pr_sma alloc,
						     childsep=RIGHT ",",
						     children= map layout_atom elems}
      | layout_se(RECORD{elems,alloc}) = HNODE{start="[",
					       finish="] " ^ pr_sma alloc,
					       childsep=RIGHT ",",
					       children= map layout_atom elems}
      | layout_se(SELECT(i,atom)) = HNODE{start="#" ^ Int.toString i ^ "(",
					  finish=")",
					  childsep=NOSEP,
					  children=[layout_atom atom]}
      | layout_se(CON0{con,con_kind,aux_regions,alloc}) =
          HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ") [",
		finish="]aux " ^ pr_sma alloc,
		childsep=RIGHT ",",
		children=map (fn sma => layout_sma sma) aux_regions}
      | layout_se(CON1{con,con_kind,alloc,arg}) = 
	  HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ") ",
		finish="" ^ pr_sma alloc,
		childsep=NOSEP,
		children=[layout_atom arg]}
      | layout_se(DECON{con,con_kind,con_atom}) =
	  LEAF("decon(" ^ Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")," ^ pr_atom con_atom ^ ")")
      | layout_se(DEREF(atom)) = LEAF("!" ^ pr_atom atom)
      | layout_se(REF(sma,atom)) = LEAF("ref " ^ pr_atom atom ^ " " ^ pr_sma sma)
      | layout_se(ASSIGNREF(sma,atom1,atom2)) = HNODE{start="",
						      finish="",
						      childsep=RIGHT " := ",
						      children=[layout_atom atom1,layout_atom atom2]}
      | layout_se(PASS_PTR_TO_MEM(sma,i)) = LEAF("MEM(" ^ pr_sma sma ^ "," ^ Int.toString i ^ ")")
      | layout_se(PASS_PTR_TO_RHO(sma)) = LEAF("PTR(" ^ pr_sma sma ^ ")")

    and layout_ls(ASSIGN{pat,bind}) = HNODE{start="",
					    finish="",
					    childsep=RIGHT " = ",
					    children=[LEAF(pr_atom pat),layout_se bind]}
      | layout_ls(FNJMP{opr,args,clos,free,res}) =
          let
	    val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map layout_atom res}
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom free}
	  in
	    HNODE{start=flatten1(t0) ^ " = " ^ pr_atom opr ^ "_fnjmp ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3]}
	  end
      | layout_ls(FNCALL{opr,args,clos,free,res}) =
          let
	    val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map layout_atom res}
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom free}
	  in
	    HNODE{start=flatten1(t0) ^ " = " ^ pr_atom opr ^ "_fncall ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3]}
	  end
      | layout_ls(JMP{opr,args,reg_vec,reg_args,clos,free,res}) =
          let
	    val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map layout_atom res}
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom free}
	    val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom reg_args}
	    val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt reg_vec]}
	  in
	    HNODE{start=flatten1(t0) ^ " = " ^ Labels.pr_label opr ^ "_funjmp ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t5,t4,t2,t3]}
	  end
      | layout_ls(FUNCALL{opr,args,reg_vec,reg_args,clos,free,res}) =
          let
	    val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map layout_atom res}
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom free}
	    val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_atom reg_args}
	    val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_atom_opt reg_vec]}
	  in
	    HNODE{start=flatten1(t0) ^ " = " ^ Labels.pr_label opr ^ "_funcall ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t5,t4,t2,t3]}
	  end
      | layout_ls(LETREGION{rhos=[],body}) = layout_lss body
      | layout_ls(LETREGION{rhos,body}) = 
	  let
	    val binders = HNODE{start = "", 
				finish = "", 
				childsep = RIGHT", ", 
				children = map (fn b => LEAF(pr_binder b)) rhos}
	  in 
	    NODE{start= "letregion " ^ flatten1(binders) ^ " in ",
		 finish= "end (*" ^ flatten1(binders) ^ "*)",
		 childsep= NOSEP,
		 indent=2,
		 children= [layout_lss body]}
	  end
      | layout_ls(SCOPE{pat=[],scope}) = layout_lss scope
      | layout_ls(SCOPE{pat,scope}) =
	  let
	    val lay_pat = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (fn sty => LEAF(pr_sty sty)) pat}
          in
            PP.NODE{start= "scope " ^ flatten1(lay_pat) ^ " in ",
                    finish=" end ",
                    indent=2,
                    childsep=NOSEP,
                    children=[layout_lss scope]}
          end
      | layout_ls(HANDLE(ls1,ls2)) =
	  NODE{start="",finish="",childsep=RIGHT " handle ",indent=1,children=[layout_lss ls1,layout_lss ls2]}
      | layout_ls(RAISE atom) = PP.LEAF("raise " ^ pr_atom atom)
      | layout_ls(SWITCH_I sw) = layout_switch layout_lss (Int.toString) sw
      | layout_ls(SWITCH_S sw) = layout_switch layout_lss (fn s => s) sw
      | layout_ls(SWITCH_C sw) = 
	  layout_switch layout_lss (fn (con,con_kind) => Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")") sw
      | layout_ls(SWITCH_E sw) = layout_switch layout_lss Excon.pr_excon sw
      | layout_ls(RESET_REGIONS{force=true,regions_for_resetting}) =
	  HNODE{start="force reset regions",
		finish="",
		childsep=RIGHT ",",
		children=map (fn sma => layout_sma sma) regions_for_resetting}
      | layout_ls(RESET_REGIONS{force=false,regions_for_resetting}) =
	  HNODE{start="reset regions",
		finish="",
		childsep=RIGHT ",",
		children=map (fn sma => layout_sma sma) regions_for_resetting}
      | layout_ls(CCALL{name,args,rhos_for_result,res}) =
          let
	    val t0 = HNODE{start="<",finish=">",childsep=RIGHT ",",children= map layout_atom res}
	  in
	    HNODE{start=flatten1(t0) ^ " = ccall(\"" ^ name ^ "\", <", 
		  finish=">)",
		  childsep=RIGHT ",",
		  children=(map layout_atom args) @ (map layout_atom rhos_for_result)}
	  end
      | layout_ls(FRAME{declared_lvars,declared_excons}) =
	  NODE{start="{|",
	       finish="|}",
	       indent=0,
	       childsep=RIGHT ",",
	       children=(map (fn {lvar,label} => LEAF("(" ^ Lvars.pr_lvar lvar ^ "," ^ 
						      Labels.pr_label label ^ ")")) declared_lvars) @
	       (map (fn {excon,label} => LEAF("(" ^ Excon.pr_excon excon ^ "," ^ 
					      Labels.pr_label label ^ ")")) declared_excons)}

    and layout_lss lss = NODE{start="",
			      finish= "",
			      indent= 0,
			      childsep= RIGHT ";",
			      children= map layout_ls lss}

    and pr_sma(ATTOP_LI(atom,pp)) = "attop_li " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(ATTOP_LF(atom,pp)) = "attop_lf " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(ATTOP_FI(atom,pp)) = "attop_fi " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(ATTOP_FF(atom,pp)) = "attop_ff " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(ATBOT_LI(atom,pp)) = "atbot_li " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(ATBOT_LF(atom,pp)) = "atbot_lf " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(SAT_FI(atom,pp))   = "sat_fi " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(SAT_FF(atom,pp))   = "sat_ff " ^ pr_atom atom ^ " " ^ pr_pp pp
      | pr_sma(IGNORE)            = "ignore "

    and layout_sma sma = LEAF(pr_sma sma)

    fun layout_top_decl'(FUN(lab,cc,lss)) =
          NODE{start = "FUN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = NOSEP, 
	       children = [layout_lss lss]}
      | layout_top_decl'(FN(lab,cc,lss)) =
	  NODE{start = "FN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = RIGHT ";", 
	       children = map layout_ls lss}
  in
    fun layout_top_decl top_decl = layout_top_decl' top_decl
    fun layout_line_stmt ls = layout_ls ls
    fun layout_line_prg top_decls = NODE{start="LineStmt program begin",
					 finish="LineStmt program end",
					 indent=2,
					 childsep=NOSEP,
					 children = map layout_top_decl top_decls}
  end



  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_linearised_program", "print linearised program (LineStmt)", ref false)]

  (*************)
  (* Utilities *)
  (*************)
  fun zip ([],[]) = []
    | zip ((x::xs),(y::ys)) = (x,y) :: (zip (xs,ys))
    | zip _ = die "zip: Cannot zip two lists."

  (***********************)
  (* Linearization: L_ce *)
  (***********************)
  local
    fun binder_to_binder(place,phsize) = (place,phsize)

    fun one_lvar([]) = die "one_lvar: zero lvars."
      | one_lvar([lv]) = lv
      | one_lvar(lvars) = die "one_lvar: more than one lvar."

    fun ce_to_atom(ClosExp.VAR lv) = VAR lv
      | ce_to_atom(ClosExp.RVAR place) = RVAR place
      | ce_to_atom(ClosExp.DROPPED_RVAR place) = DROPPED_RVAR place
      | ce_to_atom(ClosExp.INTEGER i) = INTEGER i
      | ce_to_atom(ClosExp.RECORD{elems=[],alloc=ClosExp.IGNORE}) = UNIT
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

    fun mk_sty lv = NO_STY lv

    fun L_ce_sw(ClosExp.SWITCH(ce,sels,default),f_L,f_sel) =
      SWITCH(ce_to_atom ce,
	     map (fn (sel,ce) => (f_sel sel,f_L (ce,[]))) sels,
	     f_L (default,[]))

    fun L_ce(ClosExp.VAR lv,lvars_res,acc)             = ASSIGN{pat=VAR(one_lvar lvars_res),bind=ATOM(VAR lv)}::acc
      | L_ce(ClosExp.RVAR place,lvars_res,acc)         = die "RVAR not implemented"
      | L_ce(ClosExp.DROPPED_RVAR place,lvars_res,acc) = die "DROPPED_RVAR not implemented"
      | L_ce(ClosExp.FETCH lab,lvars_res,acc)          = ASSIGN{pat=VAR(one_lvar lvars_res),bind=FETCH lab}::acc
      | L_ce(ClosExp.STORE(ce,lab),lvars_res,acc)      = ASSIGN{pat=VAR(one_lvar lvars_res),bind=STORE(ce_to_atom ce,lab)}::acc
      | L_ce(ClosExp.INTEGER i,lvars_res,acc)          = ASSIGN{pat=VAR(one_lvar lvars_res),bind=ATOM(INTEGER i)}::acc
      | L_ce(ClosExp.STRING s,lvars_res,acc)           = ASSIGN{pat=VAR(one_lvar lvars_res),bind=STRING s}::acc
      | L_ce(ClosExp.REAL s,lvars_res,acc)             = ASSIGN{pat=VAR(one_lvar lvars_res),bind=REAL s}::acc
      | L_ce(ClosExp.PASS_PTR_TO_MEM(sma,i),lvars_res,acc) = ASSIGN{pat=VAR(one_lvar lvars_res),bind=PASS_PTR_TO_MEM(sma_to_sma sma,i)}::acc
      | L_ce(ClosExp.PASS_PTR_TO_RHO sma,lvars_res,acc) = ASSIGN{pat=VAR(one_lvar lvars_res),bind=PASS_PTR_TO_RHO(sma_to_sma sma)}::acc
      | L_ce(ClosExp.UB_RECORD ces,lvars_res,acc)      = 
          List.foldr (fn ((ce,lv_res),acc) => L_ce(ce,[lv_res],acc)) acc (zip(ces,lvars_res))
      | L_ce(ClosExp.CLOS_RECORD{label,elems,alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=CLOS_RECORD{label=label,elems=ces_to_atoms elems,alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.REGVEC_RECORD{elems,alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=REGVEC_RECORD{elems=smas_to_smas elems,alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.SCLOS_RECORD{elems,alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=SCLOS_RECORD{elems=ces_to_atoms elems,alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.RECORD{elems,alloc},lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=RECORD{elems=ces_to_atoms elems,alloc=sma_to_sma alloc}}::acc
      | L_ce(ClosExp.SELECT(i,ce),lvars_res,acc) = 
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=SELECT(i,ce_to_atom ce)}::acc
      | L_ce(ClosExp.FNJMP{opr,args,clos,free},lvars_res,acc) = 
	  FNJMP{opr=ce_to_atom opr,args=ces_to_atoms args,clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res}::acc
      | L_ce(ClosExp.FNCALL{opr,args,clos,free},lvars_res,acc) =
	  FNCALL{opr=ce_to_atom opr,args=ces_to_atoms args,clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res}::acc
      | L_ce(ClosExp.JMP{opr,args,reg_vec,reg_args,clos,free},lvars_res,acc) =
	  JMP{opr=opr,args=ces_to_atoms args,reg_vec=ce_to_atom_opt reg_vec,reg_args=ces_to_atoms reg_args,
	      clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res}::acc
      | L_ce(ClosExp.FUNCALL{opr,args,reg_vec,reg_args,clos,free},lvars_res,acc) =
	  FUNCALL{opr=opr,args=ces_to_atoms args,reg_vec=ce_to_atom_opt reg_vec,reg_args=ces_to_atoms reg_args,
		  clos=ce_to_atom_opt clos,free=ces_to_atoms free,res=map VAR lvars_res}::acc
      | L_ce(ClosExp.LETREGION{rhos,body},lvars_res,acc) =
	  LETREGION{rhos=map binder_to_binder rhos,body=L_ce(body,lvars_res,[])}::acc
      | L_ce(ClosExp.LET{pat,bind,scope},lvars_res,acc) =
	  SCOPE{pat=map mk_sty pat,scope=L_ce(bind,pat,L_ce(scope,lvars_res,[]))}::acc
      | L_ce(ClosExp.RAISE ce,lvars_res,acc) = RAISE(ce_to_atom ce)::acc
      | L_ce(ClosExp.HANDLE(ce1,ce2),lvars_res,acc) =
	  HANDLE(L_ce(ce1,lvars_res,[]),L_ce(ce2,lvars_res,[]))::acc
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
							con_atom=ce_to_atom con_exp}}::acc
      | L_ce(ClosExp.DEREF ce,lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=DEREF(ce_to_atom ce)}::acc
      | L_ce(ClosExp.REF(sma,ce),lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=REF(sma_to_sma sma,ce_to_atom ce)}::acc
      | L_ce(ClosExp.ASSIGN(sma,ce1,ce2),lvars_res,acc) =
	  ASSIGN{pat=VAR(one_lvar lvars_res),bind=ASSIGNREF(sma_to_sma sma,ce_to_atom ce1,ce_to_atom ce2)}::acc
      | L_ce(ClosExp.RESET_REGIONS{force,regions_for_resetting},lvars_res,acc) = 
	  RESET_REGIONS{force=force,regions_for_resetting=smas_to_smas regions_for_resetting}::acc
      | L_ce(ClosExp.CCALL{name,args,rhos_for_result},lvars_res,acc) = 
	  CCALL{name=name,args=ces_to_atoms args,
		rhos_for_result=ces_to_atoms rhos_for_result,
		res=map VAR lvars_res}::acc
      | L_ce(ClosExp.FRAME{declared_lvars,declared_excons},[],acc) = 
	  FRAME{declared_lvars=declared_lvars,
		declared_excons=declared_excons}::acc
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

  (*********************************)
  (* Def and Use sets for LineStmt *)
  (*********************************)
  local
    fun get_lvar_atom(VAR lv,acc) = lv::acc
      | get_lvar_atom(_,acc) = acc

    fun get_lvar_atoms(atoms,acc) = foldr (fn (atom,acc) => get_lvar_atom(atom,acc)) acc atoms

    fun get_lvar_atom_opt(NONE,acc) = acc
      | get_lvar_atom_opt(SOME atom,acc) = get_lvar_atom(atom,acc)

    fun get_lvar_sma(ATTOP_LI(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(ATTOP_LF(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(ATTOP_FI(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(ATTOP_FF(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(ATBOT_LI(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(ATBOT_LF(atom,pp),acc) = get_lvar_atom(atom,acc)
      | get_lvar_sma(SAT_FI(atom,pp),acc)   = get_lvar_atom(atom,acc)
      | get_lvar_sma(SAT_FF(atom,pp),acc)   = get_lvar_atom(atom,acc)
      | get_lvar_sma(IGNORE,acc)            = acc
    
    fun get_lvar_smas(smas,acc) = foldr (fn (sma,acc) => get_lvar_sma(sma,acc)) acc smas

    fun def_se (se:SimpleExp,acc:lvar list) = acc

    fun use_se(ATOM atom,acc) = get_lvar_atom(atom,acc)
      | use_se(FETCH lab,acc) = acc
      | use_se(STORE(atom,lab),acc) = get_lvar_atom(atom,acc)
      | use_se(STRING str,acc) = acc
      | use_se(REAL str,acc) = acc
      | use_se(CLOS_RECORD{label,elems,alloc},acc) = get_lvar_sma(alloc, get_lvar_atoms(elems,acc))
      | use_se(REGVEC_RECORD{elems,alloc},acc) = get_lvar_sma(alloc, get_lvar_smas(elems,acc))
      | use_se(SCLOS_RECORD{elems,alloc},acc) = get_lvar_sma(alloc, get_lvar_atoms(elems,acc))
      | use_se(RECORD{elems,alloc},acc) = get_lvar_sma(alloc, get_lvar_atoms(elems,acc))
      | use_se(SELECT(i,atom),acc) = get_lvar_atom(atom,acc)
      | use_se(CON0{con,con_kind,aux_regions,alloc},acc) = get_lvar_sma(alloc, get_lvar_smas(aux_regions,acc))
      | use_se(CON1{con,con_kind,alloc,arg},acc) = get_lvar_sma(alloc,get_lvar_atom(arg,acc))
      | use_se(DECON{con,con_kind,con_atom},acc) = get_lvar_atom(con_atom,acc)
      | use_se(DEREF atom,acc) = get_lvar_atom(atom,acc)
      | use_se(REF(sma,atom),acc) = get_lvar_sma(sma,get_lvar_atom(atom,acc))
      | use_se(ASSIGNREF(sma,atom1,atom2),acc) = 
	 get_lvar_sma(sma,get_lvar_atom(atom1,get_lvar_atom(atom2,acc)))
      | use_se(PASS_PTR_TO_MEM(sma,i),acc) = get_lvar_sma(sma,acc)
      | use_se(PASS_PTR_TO_RHO sma,acc) = get_lvar_sma(sma,acc)

    fun use_on_fun{opr,args,reg_vec,reg_args,clos,free,res} = (* Operand is always a label *)
	get_lvar_atoms(args,get_lvar_atom_opt(reg_vec,
	 get_lvar_atoms(reg_args,get_lvar_atom_opt(clos,get_lvar_atoms(free,[])))))

    fun use_on_fn{opr,args,clos,free,res} =
	get_lvar_atoms(args,get_lvar_atom_opt(clos,get_lvar_atoms(free,get_lvar_atom(opr,[]))))

    fun use_ls(ASSIGN{pat,bind}) = use_se(bind,[])
      | use_ls(FNJMP cc) = use_on_fn cc
      | use_ls(FNCALL cc) = use_on_fn cc
      | use_ls(JMP cc) = use_on_fun cc
      | use_ls(FUNCALL cc) = use_on_fun cc
      | use_ls(RAISE atom) = get_lvar_atom(atom,[])
      | use_ls(RESET_REGIONS{force,regions_for_resetting}) = get_lvar_smas(regions_for_resetting,[])
      | use_ls(CCALL{name,args,rhos_for_result,res}) = get_lvar_atoms(args,get_lvar_atoms(rhos_for_result,[]))
      | use_ls _ = die "use_ls: statement contains statements itself."

    fun def_ls(ASSIGN{pat,bind}) = get_lvar_atom(pat,[])
      | def_ls(FNJMP{res,...}) = get_lvar_atoms(res,[])
      | def_ls(FNCALL{res,...}) = get_lvar_atoms(res,[])
      | def_ls(JMP{res,...}) = get_lvar_atoms(res,[])
      | def_ls(FUNCALL{res,...}) = get_lvar_atoms(res,[])
      | def_ls(RAISE atom) = []
      | def_ls(RESET_REGIONS{force,regions_for_resetting}) = []
      | def_ls(CCALL{res,...}) = get_lvar_atoms(res,[])
      | def_ls _ = die "def_ls: statement contains statements itself."
  in
    fun def_use_ls ls = (def_ls ls, use_ls ls)
  end


  (******************************)
  (* Linearise ClosExp          *)
  (******************************)
  fun L {main_lab:label,
	 code=clos_prg:ClosPrg,
	 imports:label list,
	 exports:label list} =
    let
      val _ = chat "[Linearisation..."
      val line_prg = L_clos_prg clos_prg
      val _ = 
	if Flags.is_on "print_linearised_program" then
	  display("\nReport: AFTER LINEARISATION:", layout_line_prg line_prg)
	else
	  ()
      val _ = chat "]\n"
    in
      {main_lab=main_lab,code=line_prg,imports=imports,exports=exports}
    end
end;