functor ClosExp(structure Con : CON
		structure Excon : EXCON
		structure Lvars : LVARS
		structure TyName : TYNAME
		structure Effect : EFFECT
		structure RType : RTYPE
		  sharing type RType.effect = Effect.effect
		  sharing type RType.tyname = TyName.TyName
                structure RegionExp: REGION_EXP
                 sharing type RType.Type = RegionExp.Type
                structure Mul : MUL
                  sharing type Mul.effectvar = Effect.effect = RegionExp.effect
                structure MulExp : MUL_EXP
		  sharing type MulExp.con = Con.con = MulExp.RegionExp.con
		  sharing type MulExp.Type = RType.Type = MulExp.RegionExp.Type
		  sharing type MulExp.effect = Effect.effect
		  sharing type MulExp.lvar = Lvars.lvar = RegionExp.lvar
		  sharing type MulExp.excon = Excon.excon
		  sharing type MulExp.datbinds = MulExp.RegionExp.datbinds
		  sharing type MulExp.metaType = MulExp.RegionExp.metaType
                  sharing type MulExp.il = RType.il
		  sharing type MulExp.qmularefset = Mul.qmularefset
                  sharing type MulExp.mulef = Mul.mulef
                  sharing type MulExp.lvar = Lvars.lvar = Mul.lvar
                  sharing type MulExp.metaType = RegionExp.metaType = MulExp.RegionExp.metaType
                  sharing MulExp.RegionExp = RegionExp
	        structure PhysSizeInf : PHYS_SIZE_INF
		  sharing type PhysSizeInf.LambdaPgm = MulExp.LambdaPgm
                structure AtInf : AT_INF
                  sharing type AtInf.place = Effect.effect = MulExp.place = Mul.place = 
                               RegionExp.place = RType.place = Effect.place = MulExp.RegionExp.place =
			       PhysSizeInf.place
		  sharing type PhysSizeInf.at = AtInf.at
		structure Labels : ADDRESS_LABELS
	        structure CallConv: CALL_CONV
		  sharing type CallConv.lvar = Lvars.lvar
		structure ClosConvEnv: CLOS_CONV_ENV
                  sharing type ClosConvEnv.con = Con.con
                  sharing type ClosConvEnv.place = AtInf.place
                  sharing type ClosConvEnv.excon = Excon.excon
                  sharing type ClosConvEnv.lvar = Lvars.lvar
                  sharing type ClosConvEnv.label = Labels.label
		structure BI : BACKEND_INFO
	        structure PP : PRETTYPRINT
		  sharing type PP.StringTree = Effect.StringTree = AtInf.StringTree = PhysSizeInf.StringTree =
		               ClosConvEnv.StringTree
                structure Flags : FLAGS
		structure Report : REPORT
		  sharing type Report.Report = Flags.Report
		structure Crash : CRASH) : CLOS_EXP = 
struct

  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  type phsize = PhysSizeInf.phsize
  type 'a at = 'a AtInf.at
  type pp = PhysSizeInf.pp
  type env = ClosConvEnv.env
  type cc = CallConv.cc
  type label = Labels.label
  type ('a,'b,'c)LambdaPgm = ('a,'b,'c)MulExp.LambdaPgm

  fun die s  = Crash.impossible ("ClosExp." ^ s)

  (***********)
  (* ClosExp *)
  (***********)

  datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | BOXED of int

  type binder = place * phsize

  datatype ClosExp = 
      VAR             of lvar
    | RVAR            of place
    | DROPPED_RVAR    of place
    | FETCH           of label
    | STORE           of ClosExp * label
    | INTEGER         of int 
    | STRING          of string
    | REAL            of string
    | PASS_PTR_TO_MEM of sma * int
    | PASS_PTR_TO_RHO of sma
    | UB_RECORD       of ClosExp list
    | CLOS_RECORD     of {label: label, elems: ClosExp list, alloc: sma}
    | REGVEC_RECORD   of {elems: sma list, alloc: sma}
    | SCLOS_RECORD    of {elems: ClosExp list, alloc: sma}
    | RECORD          of {elems: ClosExp list, alloc: sma}
    | SELECT          of int * ClosExp
    | FNJMP           of {opr: ClosExp, args: ClosExp list, clos: ClosExp option, free: ClosExp list}
    | FNCALL          of {opr: ClosExp, args: ClosExp list, clos: ClosExp option, free: ClosExp list}
    | JMP             of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list, 
			  clos: ClosExp option, free: ClosExp list}
    | FUNCALL         of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list, 
			  clos: ClosExp option, free: ClosExp list}
    | LETREGION       of {rhos: binder list, body: ClosExp}
    | LET             of {pat: lvar list, bind: ClosExp, scope: ClosExp}
    | RAISE           of ClosExp
    | HANDLE          of ClosExp * ClosExp
    | SWITCH_I        of int Switch
    | SWITCH_S        of string Switch
    | SWITCH_C        of (con*con_kind) Switch
    | SWITCH_E        of excon Switch
    | CON0            of {con: con, con_kind: con_kind, aux_regions: sma list, alloc: sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: sma, arg: ClosExp}
    | DECON           of {con: con, con_kind: con_kind, con_exp: ClosExp}
    | DEREF           of ClosExp
    | REF             of sma * ClosExp
    | ASSIGN          of sma * ClosExp * ClosExp
    | RESET_REGIONS   of {force: bool, 
			  regions_for_resetting: sma list}
    | CCALL           of {name: string,  
			  args: ClosExp list,
			  rhos_for_result : ClosExp list}
    | FRAME           of {declared_lvars: {lvar: lvar, label: label} list,
			  declared_excons: {excon: excon, label: label} list}

  and 'a Switch = SWITCH of ClosExp * ('a * ClosExp) list * ClosExp

  and sma = 
      ATTOP_LI of ClosExp * pp
    | ATTOP_LF of ClosExp * pp
    | ATTOP_FI of ClosExp * pp
    | ATTOP_FF of ClosExp * pp
    | ATBOT_LI of ClosExp * pp
    | ATBOT_LF of ClosExp * pp
    | SAT_FI   of ClosExp * pp
    | SAT_FF   of ClosExp * pp
    | IGNORE

  datatype TopDecl = 
      FUN of label * cc * ClosExp
    | FN of label * cc * ClosExp
  
  type ClosPrg = TopDecl list

  (***********************)
  (* PrettyPrint ClosExp *)
  (***********************)
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

    fun layout_switch layout_ce pr_const (SWITCH(ce_arg,sels,default)) =
	  let
	    fun layout_sels(const,ce_sel) =
		  NODE{start="",finish="",indent=0,
		       children=[LEAF (pr_const const), layout_ce ce_sel],
		       childsep=RIGHT " => "}
	    val t1 = NODE{start="(case ",finish=" ",indent=2, childsep = NOSEP, 
			  children=[layout_ce ce_arg]}
	    val t2 = NODE{start="of " ,finish="",indent=4,childsep=LEFT " | ",
			  children=(map layout_sels sels) @ 
			           [NODE{start="",finish="",indent=0,
					 children=[LEAF "_",layout_ce default],
					 childsep=RIGHT " => "}]}
	    val t3 = NODE{start="",finish=") (*case*) ",indent=3,childsep=NOSEP,children=[t2]}
	  in 
	    NODE{start = "", finish = "", indent=0, childsep=NOSEP,children=[t1,t3]}
	  end
      
    fun layout_ce(VAR lv)             = LEAF(Lvars.pr_lvar lv)
      | layout_ce(RVAR place)         = Effect.layout_effect place
      | layout_ce(DROPPED_RVAR place) = LEAF("D" ^ flatten1(Effect.layout_effect place))
      | layout_ce(FETCH lab)          = LEAF("fetch(" ^ Labels.pr_label lab ^ ")")
      | layout_ce(STORE(ce,lab))      = LEAF("store(" ^ flatten1(layout_ce ce) ^ "," ^ Labels.pr_label lab ^ ")")
      | layout_ce(INTEGER i)          = LEAF(Int.toString i)
      | layout_ce(STRING s)           = LEAF("\"" ^ String.toString s ^ "\"")
      | layout_ce(REAL s)             = LEAF(s)
      | layout_ce(PASS_PTR_TO_MEM(sma,i)) = LEAF("MEM(" ^ (flatten1(pr_sma sma)) ^ "," ^ Int.toString i ^ ")")
      | layout_ce(PASS_PTR_TO_RHO(sma))   = LEAF("PTR(" ^ (flatten1(pr_sma sma)) ^ ")")
      | layout_ce(UB_RECORD ces)      = HNODE{start="<",
					      finish=">",
					      childsep=RIGHT ",",
					      children=map layout_ce ces}
      | layout_ce(CLOS_RECORD{label,elems,alloc}) = HNODE{start="[",
							  finish="]clos " ^ (flatten1(pr_sma alloc)),
							  childsep=RIGHT ",",
							  children=LEAF(Labels.pr_label label)::
							  map layout_ce elems}
      | layout_ce(REGVEC_RECORD{elems,alloc}) = HNODE{start="[",
						      finish="]regvec " ^ (flatten1(pr_sma alloc)),
						      childsep=RIGHT ",",
						      children=map (fn sma => pr_sma sma) elems}
      | layout_ce(SCLOS_RECORD{elems,alloc}) = HNODE{start="[",
						     finish="]sclos " ^ (flatten1(pr_sma alloc)),
						     childsep=RIGHT ",",
						     children= map layout_ce elems}
      | layout_ce(RECORD{elems,alloc}) = HNODE{start="[",
					       finish="] " ^ (flatten1(pr_sma alloc)),
					       childsep=RIGHT ",",
					       children= map layout_ce elems}
      | layout_ce(SELECT(i,ce)) = HNODE{start="#" ^ Int.toString i ^ "(",
					finish=")",
					childsep=NOSEP,
					children=[layout_ce ce]}
      | layout_ce(FNJMP{opr,args,clos,free}) = 
          let
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce free}
	  in
	    HNODE{start=flatten1(layout_ce opr) ^ "_fnjmp ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3]}
	  end
      | layout_ce(FNCALL{opr,args,clos,free}) = 
          let
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt clos]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce free}
	  in
	    HNODE{start=flatten1(layout_ce opr) ^ "_fncall ",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3]}
	  end
      | layout_ce(JMP{opr,args,reg_vec,reg_args,clos,free}) = 
          let
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt reg_vec]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce reg_args}
	    val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt clos]}
	    val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce free}
	  in
	    HNODE{start=Labels.pr_label opr ^ "_funjmp",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3,t4,t5]}
	  end
      | layout_ce(FUNCALL{opr,args,reg_vec,reg_args,clos,free}) = 
          let
	    val t1 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce args}
	    val t2 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt reg_vec]}
	    val t3 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce reg_args}
	    val t4 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=[layout_ce_opt clos]}
	    val t5 = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map layout_ce free}
	  in
	    HNODE{start=Labels.pr_label opr ^ "_funcall",
		  finish="", childsep=RIGHT " ",
		  children=[t1,t2,t3,t4,t5]}
	  end
      | layout_ce(LETREGION{rhos=[],body}) = layout_ce body
      | layout_ce(LETREGION{rhos,body}) =
          let 
	    val binders = HNODE{start = "", 
				finish = "", 
				childsep = RIGHT", ", 
				children = map (fn b => LEAF(pr_binder b)) rhos}
	    val t1 = NODE{start="letregion ",
			  finish="",
			  childsep=NOSEP,
			  indent=10,
			  children=[binders]}
	    val t2 = NODE{start="in ",
			  finish="", 
			  childsep=NOSEP,
			  indent=3,
			  children=[layout_ce body]}
	    val t3 = NODE{start="end (*",
			  finish="*)",
			  childsep=NOSEP, 
			  indent=6,
			  children=[HNODE{start="",
					  finish="",
					  childsep=RIGHT", ", 
					  children=[binders]}]}
	  in 
	    NODE{start="",finish="",indent=0,childsep=RIGHT " ",children=[t1,t2,t3]}
	  end
      | layout_ce(ce as LET{pat: lvar list, bind: ClosExp, scope: ClosExp}) = 
	  let
	    fun layout_rec(LET{pat,bind,scope}) =
	      let
		val lay_pat = HNODE{start="<",finish=">",childsep=RIGHT ",",children=map (fn lv => LEAF(Lvars.pr_lvar lv)) pat}
		val (binds, body) = layout_rec scope
		val bind = NODE{start="val ",finish="",childsep=RIGHT " = ",indent=4,children=[lay_pat,layout_ce bind]}
	      in
                (bind::binds,body)
	      end
	      | layout_rec ce = ([],layout_ce ce)

           val (l, body) = layout_rec ce
           val bindings =  NODE{start="",finish="",childsep=RIGHT "; ",indent = 0,children = l}
          in
            PP.NODE{start= "let ",
                    finish=" end ",
                    indent=4,
                    children=[bindings,body],
                    childsep=LEFT (" in ")}
          end
      | layout_ce(RAISE ce) = PP.LEAF("raise " ^ (flatten1(layout_ce ce)))
      | layout_ce(HANDLE(ce1,ce2)) = NODE{start="",finish="",childsep=RIGHT " handle ",indent=1,
					  children=[layout_ce ce1,layout_ce ce2]}
      | layout_ce(SWITCH_I sw) = layout_switch layout_ce (Int.toString) sw
      | layout_ce(SWITCH_S sw) = layout_switch layout_ce (fn s => s) sw
      | layout_ce(SWITCH_C sw) = layout_switch layout_ce (fn (con,con_kind) => Con.pr_con con ^ "(" ^ 
							  pr_con_kind con_kind ^ ")") sw
      | layout_ce(SWITCH_E sw) = layout_switch layout_ce Excon.pr_excon sw
      | layout_ce(CON0{con,con_kind,aux_regions,alloc}) = 
	  HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")",
		finish="] " ^ (flatten1(pr_sma alloc)),
		childsep=RIGHT ",",
		children=map (fn sma => pr_sma sma) aux_regions}
      | layout_ce(CON1{con,con_kind,alloc,arg}) =
	  HNODE{start=Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ") ",
		finish="" ^ (flatten1(pr_sma alloc)),
		childsep=NOSEP,
		children=[layout_ce arg]}
      | layout_ce(DECON{con,con_kind,con_exp}) =
	  LEAF("decon(" ^ Con.pr_con con ^ "(" ^ pr_con_kind con_kind ^ ")," ^ (flatten1(layout_ce con_exp)) ^ ")")
      | layout_ce(DEREF ce) = LEAF("!" ^ (flatten1(layout_ce ce)))
      | layout_ce(REF(sma,ce)) = LEAF("ref " ^ (flatten1(layout_ce ce)) ^ " " ^ (flatten1(pr_sma sma)))
      | layout_ce(ASSIGN(sma,ce1,ce2)) = HNODE{start="",
					       finish="",
					       childsep=RIGHT ":=",
					       children=[layout_ce ce1,layout_ce ce2]}
      | layout_ce(RESET_REGIONS{force=true,regions_for_resetting}) =
	  HNODE{start="force reset regions",
		finish="",
		childsep=RIGHT ",",
		children=map (fn sma => pr_sma sma) regions_for_resetting}
      | layout_ce(RESET_REGIONS{force=false,regions_for_resetting}) =
	  HNODE{start="reset regions",
		finish="",
		childsep=RIGHT ",",
		children=map (fn sma => pr_sma sma) regions_for_resetting}
      | layout_ce(CCALL{name,args,rhos_for_result}) =
	  HNODE{start="ccall(\"" ^ name ^ "\", <",
		finish=">)",
		childsep=RIGHT ",",
		children=(map layout_ce args) @ (map layout_ce rhos_for_result)}
      | layout_ce(FRAME{declared_lvars,declared_excons}) =
		  NODE{start="{|",
		       finish="|}",
		       indent=0,
		       childsep=RIGHT ",",
		       children=(map (fn {lvar,label} => LEAF("(" ^ Lvars.pr_lvar lvar ^ "," ^ 
							      Labels.pr_label label ^ ")")) declared_lvars) @
		                (map (fn {excon,label} => LEAF("(" ^ Excon.pr_excon excon ^ "," ^ 
							       Labels.pr_label label ^ ")")) declared_excons)}
		  
    and layout_ce_opt (NONE)    = LEAF ""
      | layout_ce_opt (SOME ce) = layout_ce ce

    and pr_sma(ATTOP_LI(ce,pp)) = NODE{start="attop_li ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(ATTOP_LF(ce,pp)) = NODE{start="attop_lf ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(ATTOP_FI(ce,pp)) = NODE{start="attop_fi ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(ATTOP_FF(ce,pp)) = NODE{start="attop_ff ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(ATBOT_LI(ce,pp)) = NODE{start="atbot_li ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(ATBOT_LF(ce,pp)) = NODE{start="atbot_lf ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(SAT_FI(ce,pp))   = NODE{start="sat_fi ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(SAT_FF(ce,pp))   = NODE{start="sat_ff ",finish=" " ^ pr_pp pp,indent=0,
				       childsep=NOSEP,children=[layout_ce ce]}
      | pr_sma(IGNORE)          = NODE{start="ignore ",finish="",indent=0,childsep=NOSEP,children=[]}

    fun layout_top_decl'(FUN(lab,cc,ce)) =
          NODE{start = "FUN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = NOSEP, 
	       children = [layout_ce ce]}
      | layout_top_decl'(FN(lab,cc,ce)) =
	  NODE{start = "FN " ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=", 
	       finish = "", 
	       indent = 2, 
	       childsep = NOSEP, 
	       children = [layout_ce ce]}
  in
    fun layout_top_decl top_decl = layout_top_decl' top_decl
    fun layout_clos_exp ce = layout_ce ce
    fun layout_clos_prg top_decls = NODE{start="ClosExp program begin",
					 finish="ClosExp program end",
					 indent=2,
					 childsep=NOSEP,
					 children = map layout_top_decl top_decls}
  end

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_normalized_program", "print normalized expression (MulExp)", ref false),
     ("print_clos_conv_program", "print closure converted expression (ClosExp)", ref false)]

  (*****************************************************************************************)
  (*****************************************************************************************)
  (* Compilation Functions Starts Here                                                     *)
  (*****************************************************************************************)
  (*****************************************************************************************)

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s = Crash.impossible ("ClosExp:"^s)
  fun log_st st = PP.outputTree(fn s => TextIO.output(!Flags.log,s), st, 70)
  fun pr_st st = PP.outputTree(fn s => TextIO.output(TextIO.stdOut,s), st, 70)

  (*************)
  (* Utilities *)
  (*************)
  fun zip ([],[]) = []
    | zip ((x::xs),(y::ys)) = (x,y) :: (zip (xs,ys))
    | zip _ = die "zip: Cannot zip two lists."

  fun zip5 ([],[],[],[],[]) = []
    | zip5 ((x::xs),(y::ys),(z::zs),(v::vs),(w::ws)) = (x,y,z,v,w) :: (zip5 (xs,ys,zs,vs,ws))
    | zip5 _ = die "zip4: Cannot zip four lists."

  fun unzip l = List.foldr (fn ((x,y),(xs,ys)) => (x::xs,y::ys)) ([],[]) l

  fun one_in_list([]) = die "one_in_list: list has zero elements."
    | one_in_list([x]) = x
    | one_in_list _ = die "one_in_list: list has more than one element."

  fun split_in_hd_and_tl (x::xs) = (x,xs)
    | split_in_hd_and_tl _ = die "split_in_hd_and_tl: Can't split list with zero elements"

  fun concat_lists l = List.foldl (op @) [] l

  fun size3(l1,l2,l3) = List.length l1 + List.length l2 + List.length l3

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

  (****************************)
  (* Normalize MulExp Program *)
  (****************************)

  (* Normalize inserts extra let bindings in FN, FIX and HANDLE          *)
  (* expressions such that all value creating expressions are bound to a *)
  (* lambda variable.                                                    *)
  (* The input term is K-normalized so we only have to insert lets       *)
  (* around FN terms.                                                    *)
  (* The boolean insert_let is true if we do not have a let expression   *)
  (* or a fix around a function. Note, that we also insert a let in the  *)
  (* handle expression.                                                  *)
  (* The new variables are names kn# and the flag k_let is set true.     *)

  local 
    open MulExp
    val r = ref 0
    fun fresh _ = (r:= !r + 1; Lvars.new_named_lvar ("kn" ^ Int.toString(!r)))
  in
    fun N(prog as MulExp.PGM{expression = tr,
			     export_datbinds,
			     import_vars,
			     export_vars,
			     export_basis,
			     export_Psi}) = 
      let
	fun NTrip ((MulExp.TR(e,metaType,ateffects,mulef))) insert_let =
	  let
	    fun e_to_t e = MulExp.TR(e,metaType,ateffects,mulef)
	    local 
	      val il0 = RType.mk_il([],[],[])
	      val dummy_'c = ()
	    in
	      fun lvar_as_term(x,mu) =
		TR(VAR{lvar=x,il=il0,plain_arreffs=[],
		       alloc=NONE,rhos_actuals=ref [],other=dummy_'c},mu,[],ref Mul.empty_psi)
	      fun mk_pat(lvar,mu) =
		(case mu of
		   RegionExp.Mus[(ty,place)] =>
		     [(lvar,ref([]:RType.il ref list),[],ref([]:effect list),ty,place,dummy_'c)]
		 | _ => die "mk_pat: metatype not (tau,rho)")
	    end

	    fun Nsw (tr, choices, opt) =
	      MulExp.SWITCH(NTrip tr true, 
			    map (fn(match,tr) => (match,NTrip tr true)) choices, 
			    (case opt of
			       SOME tr => SOME (NTrip tr true)
			     | NONE => NONE))

	    fun NExp e =
	      (case e of
		 MulExp.VAR{lvar,il, plain_arreffs,alloc,rhos_actuals,other} => e
	       | MulExp.INTEGER(i,alloc) => e
	       | MulExp.STRING(s,alloc) => e
	       | MulExp.REAL(r,alloc) => e
	       | MulExp.UB_RECORD trs => MulExp.UB_RECORD (map (fn tr => NTrip tr true) trs)
	       | MulExp.FN{pat,body,free,alloc} => 
		   if insert_let then
		     let
		       val x = fresh()
		     in
		       MulExp.LET{k_let = true,
				  pat = mk_pat(x,metaType),
				  bind = e_to_t(MulExp.FN{pat=pat,
							  body=NTrip body true,
							  free=free,
							  alloc=alloc}),
				  scope = lvar_as_term(x,metaType)}
		     end
		   else
		     MulExp.FN{pat=pat,body=NTrip body true,free=free,alloc=alloc}
	       | MulExp.LETREGION{B, rhos, body} => MulExp.LETREGION{B=B,rhos=rhos,body=NTrip body true}
	       | MulExp.LET{k_let,pat,bind,scope} => MulExp.LET{k_let=k_let,
								pat=pat,
								bind=NTrip bind false,
								scope=NTrip scope true}
	       | MulExp.FIX{free,shared_clos,functions,scope} => 
		   MulExp.FIX{free=free,
			      shared_clos=shared_clos,
			      functions=
			      map (fn {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
				       bound_but_never_written_into,other,bind} =>
				   {lvar=lvar,occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,
				    Type=Type,rhos_formals=rhos_formals,other=other,
				    bound_but_never_written_into=bound_but_never_written_into,
				    bind=NTrip bind false}) functions,
			      scope=NTrip scope true}
	       | MulExp.APP(callKind,saveRestore,operator,operand) =>
		   MulExp.APP(callKind,saveRestore,NTrip operator true,NTrip operand true)
	       | MulExp.EXCEPTION(excon,bool,typePlace,alloc,scope) => 
		   MulExp.EXCEPTION(excon,bool,typePlace,alloc,NTrip scope true)
	       | MulExp.RAISE tr => MulExp.RAISE (NTrip tr true)
	       | MulExp.HANDLE(tr1,tr2) => MulExp.HANDLE(NTrip tr1 true,NTrip tr2 true)
	       | MulExp.SWITCH_I(MulExp.SWITCH(tr,choices,opt)) => MulExp.SWITCH_I(Nsw(tr, choices, opt))
	       | MulExp.SWITCH_S(MulExp.SWITCH(tr,choices,opt)) => MulExp.SWITCH_S(Nsw(tr, choices, opt))
	       | MulExp.SWITCH_C(MulExp.SWITCH(tr,choices,opt)) => MulExp.SWITCH_C(Nsw(tr, choices, opt))
	       | MulExp.SWITCH_E(MulExp.SWITCH(tr,choices,opt)) => MulExp.SWITCH_E(Nsw(tr, choices, opt))
	       | MulExp.CON0{con,il,aux_regions,alloc} => e
	       | MulExp.CON1({con,il,alloc},tr) => MulExp.CON1({con=con,il=il,alloc=alloc},NTrip tr true)
	       | MulExp.DECON({con,il},tr) => MulExp.DECON({con=con,il=il}, NTrip tr true)
	       | MulExp.EXCON(excon,NONE) => e
	       | MulExp.EXCON(excon,SOME(alloc,tr)) => MulExp.EXCON(excon,SOME(alloc, NTrip tr true))
	       | MulExp.DEEXCON(excon,tr) => MulExp.DEEXCON(excon, NTrip tr true)
	       | MulExp.RECORD(alloc, trs) => MulExp.RECORD(alloc, map (fn tr => NTrip tr true) trs)
	       | MulExp.SELECT(i,tr) => MulExp.SELECT(i,NTrip tr true)
	       | MulExp.DEREF tr => MulExp.DEREF (NTrip tr true)
	       | MulExp.REF(a,tr) => MulExp.REF(a,NTrip tr true)
	       | MulExp.ASSIGN(alloc,tr1,tr2) => MulExp.ASSIGN(alloc,NTrip tr1 true,NTrip tr2 true)
	       | MulExp.EQUAL({mu_of_arg1,mu_of_arg2,alloc},tr1, tr2) => 
		   MulExp.EQUAL({mu_of_arg1=mu_of_arg1,mu_of_arg2=mu_of_arg2,alloc=alloc},
				NTrip tr1 true,
				NTrip tr2 true)
	       | MulExp.CCALL({name, mu_result, rhos_for_result}, trs) =>
		   MulExp.CCALL({name=name,mu_result=mu_result,rhos_for_result=rhos_for_result},
				map (fn tr => NTrip tr true) trs)
	       | MulExp.RESET_REGIONS({force, alloc, regions_for_resetting},tr) =>
		   MulExp.RESET_REGIONS({force=force,alloc=alloc,regions_for_resetting=regions_for_resetting},
					NTrip tr true)
	       | MulExp.FRAME{declared_lvars, declared_excons} => e)
	  in
	    MulExp.TR(NExp e,metaType,ateffects,mulef)
	  end
      in
	MulExp.PGM{expression = NTrip tr false,
		   export_datbinds = export_datbinds,
		   import_vars = import_vars,
		   export_vars = export_vars,
		   export_basis = export_basis,
		   export_Psi = export_Psi}
      end
  end


  (*************************************)
  (* Calculating The Need For Closures *)
  (*************************************)

  local
    open MulExp

    (***************************************)
    (* Calculating and PrettyPrinting FEnv *)
    (***************************************)
    (* We define FEnv to be a finite map from lvars to either          *)
    (* an ordinary function or a fix-bound function and the free       *)
    (* variables.                                                      *)
    (* The domain is the lvar bound to the function                    *)

    type free = lvar list * excon list * place list

    datatype fenv =
      FN of (lvar * free)
    | FIX of (lvar * free)

    structure FuncEnv =
      OrderFinMap(structure Order =
		    struct
		      type T = lvar
		      fun lt(l1: T) l2 = Lvars.lt(l1,l2)
		    end
		  structure PP =PP
		  structure Report = Report)

    local
      fun pp_dom lvar = PP.LEAF (Lvars.pr_lvar lvar)
      fun pp_regvar rho =  PP.flatten1(Effect.layout_effect rho)
      fun pr_seq [] pp = ""
	| pr_seq [e] pp = pp e
	| pr_seq (e::rest) pp = pp e ^ ", " ^ (pr_seq rest pp)
      fun pr_rhos rhos = pr_seq rhos pp_regvar
      fun pr_lvars lvars = pr_seq lvars Lvars.pr_lvar
      fun pr_excons excons = pr_seq excons Excon.pr_excon
      fun pr_free (lvars, excons, rhos) =
	"(["^(pr_lvars lvars)^"],["^(pr_excons excons)^"],["^(pr_rhos rhos)^"])"
      fun pp_ran (FN(arg,free)) = 
	PP.LEAF ("FN[Arg:"^(Lvars.pr_lvar arg)^
		 ",Free:"^(pr_free free)^"]")
	| pp_ran (FIX(arg,free)) = 
	PP.LEAF ("FIX[Arg:"^(Lvars.pr_lvar arg)^
		 ",Free:"^(pr_free free)^"]")
    in
      fun pp_fenv fenv = pr_st(pp_ran fenv);
      fun pp_Fenv Fenv =
	let
	  val init = {start="Fenv[",eq="-->",sep="",finish="]"}
	in
	  FuncEnv.layoutMap init pp_dom pp_ran Fenv
	end
    end
  
    fun is_in_dom_Fenv Fenv lvar = 
      case FuncEnv.lookup Fenv lvar of
	NONE   => false
      | SOME r => true

    fun lookup_Fenv Fenv lvar = FuncEnv.lookup Fenv lvar

    fun rem_Fenv Fenv lvar = 
      case FuncEnv.remove (lvar, Fenv) of
	SOME Fenv => Fenv
      | NONE => die "Remove lvar fra Fenv."

    fun add_Fenv Fenv lvar v = FuncEnv.add (lvar, v, Fenv)

    (*******)
    (* Env *)
    (*******)
    structure EnvLvar =
      OrderSet(structure Order =
		 struct
		   type T = lvar
		   fun lt(l1: T) l2 = Lvars.lt(l1,l2)
		 end
	       structure PP =PP
	       structure Report = Report)

    structure EnvExCon =
      OrderSet(structure Order =
		 struct
		   type T = excon
		   fun lt(e1: T) e2 = Excon.< (e1,e2)
		 end
	       structure PP =PP
	       structure Report = Report)

    structure EnvRho =
      OrderSet(structure Order =
		 struct
		   type T = place
		   fun lt(r1: T) r2 = Effect.lt_eps_or_rho(r1,r2)
		 end
	       structure PP =PP
	       structure Report = Report)

    fun add_Env (Lvar, ExCon, Rho) (lvars, excons, rhos) =
      (EnvLvar.addList lvars Lvar, EnvExCon.addList excons ExCon, EnvRho.addList rhos Rho)

    fun fresh_Env (lvars1, excons1, rhos1) (lvars2, excons2, rhos2) =
      (EnvLvar.fromList (lvars1@lvars2), 
       EnvExCon.fromList (excons1@excons2),
       EnvRho.fromList (rhos1@rhos2))

    fun free_in_Env (LvarEnv, ExConEnv, RhoEnv) (lvars, excons, rhos) =
      (List.foldl (fn (lvar,base) => 
		   if EnvLvar.member lvar LvarEnv andalso  base=true then true else false) true lvars) andalso
      (List.foldl (fn (excon,base) => 
		   if EnvExCon.member excon ExConEnv andalso base=true then true else false) true excons) andalso
      (List.foldl (fn (rho,base) => 
		   if EnvRho.member rho RhoEnv andalso base=true then true else false) true rhos)

    (***************)
    (* Return Type *)
    (***************)
    datatype rtn_type =
      FUNC of (lvar * free)
    | OTHER

  in
    fun F(prog as MulExp.PGM{expression = tr,
			     export_datbinds,
			     import_vars,
			     export_vars,
			     export_basis,
			     export_Psi}) = 
      let
	val import_vars = 
	  case import_vars
	    of ref (SOME vars) => vars
	  | _ => die "comp_lamb.no import vars info"

	fun FTrip ((MulExp.TR(e,metaType,ateffects,mulef))) Fenv Env =
	  let
	    fun FExp e Fenv (Env as (EnvLvar, EnvExCon, EnvRho)) =
	      (case e of
		 MulExp.VAR{lvar,il, plain_arreffs,alloc,rhos_actuals,other} => 
		   if is_in_dom_Fenv Fenv lvar then
		     (rem_Fenv Fenv lvar, [OTHER])
		   else
		     (Fenv, [OTHER])
	       | MulExp.INTEGER(i,alloc) => (Fenv, [OTHER])
	       | MulExp.STRING(s,alloc) => (Fenv, [OTHER])
	       | MulExp.REAL(r,alloc) => (Fenv, [OTHER]) 
	       | MulExp.UB_RECORD trs => 
		   List.foldr (fn (tr,(Fenv',types')) => 
				(case FTrip tr Fenv' Env
				   of (Fenv_t, [t]) => (Fenv_t,t::types')
				 | _ => die "UB_RECORD")) (Fenv, []) trs
	       | MulExp.FN{pat as [(lvar,_)],body,free,alloc} => 
		   let
		     val free_vars = 
		       case (!free) of
			 SOME free => free
		       | NONE => ([], [], [])

		     val (Fenv',_) = FTrip body Fenv (add_Env (fresh_Env free_vars import_vars) ([lvar],[],[]))
		 in
		   (Fenv', [FUNC(lvar,free_vars)])
		 end
	       | MulExp.FN{pat,body,free,alloc} => die "FN with not only one argument"
	       | MulExp.LETREGION{B, rhos, body} => 
		   FTrip body Fenv (add_Env Env ([],[],List.map #1 (!rhos)))
	       | MulExp.LET{k_let,pat,bind,scope} => 
		   let
		     val lvars = List.map #1 pat
		     val (Fenv', types) = FTrip bind Fenv Env
		     val types' = zip (lvars,types)
		     val Fenv_scope = List.foldl (fn ((lvar',type'),base) => 
						   case type' of
						     FUNC(arg,free) => add_Fenv base lvar' (FN(arg, free))
						   | OTHER => base) Fenv' types'
		   in
		     FTrip scope Fenv_scope (add_Env Env (lvars,[],[]))
		 end
	     | MulExp.FIX{free,shared_clos,functions,scope} =>
		 let
		   (* funcs : (lvar, args, formals, free, body) list *)
		   fun f {lvar, occ, tyvars, rhos, epss, Type, rhos_formals: (place*phsize) list ref, other,
			  bound_but_never_written_into,
			  bind = MulExp.TR(MulExp.FN{pat=[(arg,_)],body,free,alloc},_,_,_)} =
		         (case (!free)
			    of NONE => (lvar, arg, List.map #1 (!rhos_formals), ([],[],[]), body)
			     | SOME free => (lvar, arg, List.map #1 (!rhos_formals), free, body))
		     | f _ = die "Functions not in expected shape."
		   val funcs = List.map f functions
		     
		   val Fenv1 = List.foldl (fn ((lvar,arg,_,free,_),base) => 
					   add_Fenv base lvar (FIX(arg, free))) Fenv funcs 

		   val FenvN = List.foldl (fn ((_,arg,rhos_formals,free,body),base) => 
					   #1(FTrip body base 
					      (add_Env (fresh_Env free import_vars) 
					       ([arg],[],rhos_formals)))) Fenv1 funcs

		   val all_exists = List.foldl (fn ((lvar,_,_,_,_),base) => 
						if is_in_dom_Fenv FenvN lvar andalso base = true then true else false) true funcs

		   val Fenv_scope = 
		     if all_exists then 
		       FenvN
		     else (* Remove all FIX bound functions. *)
		       List.foldl (fn ((lvar,_,_,_,_),base) => 
				    if is_in_dom_Fenv base lvar then
				      rem_Fenv base lvar
				    else
				      base) FenvN funcs
		 in
		   FTrip scope Fenv_scope (add_Env Env (List.map #1 funcs,[],[]))
		 end
	     | MulExp.APP(callKind,saveRestore,operator,operand) =>
		 (case operator 
		    of MulExp.TR(MulExp.VAR{lvar,il, plain_arreffs,alloc=NONE,rhos_actuals,other},_,_,_) =>
		      (* Ordinary function call or a primitive *)
		      (case Lvars.primitive lvar
			 of NONE => (* Ordinary function call *)
			   let
			     val Fenv' = (case lookup_Fenv Fenv lvar
					    of NONE => Fenv
					     | SOME (FN(arg_fn,free_fn)) =>
					      if free_in_Env Env free_fn then 
						Fenv
					      else 
						rem_Fenv Fenv lvar
					     | SOME (FIX(lvar,free)) => die "Function should be FN but is recorded as FIX")
			     val (Fenv_res, _) = FTrip operand Fenv' Env
			   in
			     (Fenv_res, [OTHER])
			   end
			  | SOME prim => (* Primitive call *)
			   let
			     val (Fenv_res, _) = FTrip operand Fenv Env (* We traverse all arguments *)
			   in
			     (Fenv_res, [OTHER])
			   end)
		     | MulExp.TR(MulExp.VAR{lvar,il, plain_arreffs,alloc=SOME atp,rhos_actuals,other},_,_,_) =>
		       (* Region Polymorphic call *)
			 let
			   val Fenv' = (case lookup_Fenv Fenv lvar
					  of NONE => Fenv
					   | SOME (FIX(arg_fix,free_fix)) =>
					    if free_in_Env Env free_fix then
					      Fenv
					    else 
					      rem_Fenv Fenv lvar
					   | SOME (FN(lvar,free)) => die "Function should be a FIX but is recorded as FN")
			   val (Fenv_res,_) = FTrip operand Fenv' Env
			 in
			   (Fenv_res, [OTHER])
			 end
		     | _ => die "First argument in application not as expected.")
	     | MulExp.EXCEPTION(excon,bool,typePlace,alloc,scope) => 
		 FTrip scope Fenv (add_Env Env ([],[excon],[]))
	     | MulExp.RAISE tr => FTrip tr Fenv Env
	     | MulExp.HANDLE(tr1,tr2) =>
		 let
		   val (Fenv1, _) = FTrip tr1 Fenv Env
		   val (Fenv2, _) = FTrip tr2 Fenv1 Env
		 in
		   (Fenv2, [OTHER])
		 end
	     | MulExp.SWITCH_I(MulExp.SWITCH(tr,choices,opt)) =>
		 let
		   val (Fenv_tr,_) = FTrip tr Fenv Env
		   val Fenv_ch = List.foldl (fn ((_,tr),base) => #1(FTrip tr base Env)) Fenv_tr choices
		  val (Fenv_res) = (case opt of SOME tr => #1(FTrip tr Fenv_ch Env) | NONE => Fenv_ch)
		 in
		   (Fenv_res,[OTHER])
		 end
	     | MulExp.SWITCH_S(MulExp.SWITCH(tr,choices,opt)) =>
		 let
		   val (Fenv_tr,_) = FTrip tr Fenv Env
		   val Fenv_ch = List.foldl (fn ((_,tr),base) => #1(FTrip tr base Env)) Fenv_tr choices
		   val (Fenv_res) = (case opt of SOME tr => #1(FTrip tr Fenv_ch Env) | NONE => Fenv_ch)
		 in
		   (Fenv_res,[OTHER])
		 end
	     | MulExp.SWITCH_C(MulExp.SWITCH(tr,choices,opt)) =>
		 let
		   val (Fenv_tr,_) = FTrip tr Fenv Env
		   val Fenv_ch = List.foldl (fn ((_,tr),base) => #1(FTrip tr base Env)) Fenv_tr choices
		   val (Fenv_res) = (case opt of SOME tr => #1(FTrip tr Fenv_ch Env) | NONE => Fenv_ch)
		 in
		   (Fenv_res,[OTHER])
		 end
	     | MulExp.SWITCH_E(MulExp.SWITCH(tr,choices,opt)) =>
		 let
		   val (Fenv_tr,_) = FTrip tr Fenv Env
		   val Fenv_ch = List.foldl (fn ((_,tr),base) => #1(FTrip tr base Env)) Fenv_tr choices
		   val (Fenv_res) = (case opt of SOME tr => #1(FTrip tr Fenv_ch Env) | NONE => Fenv_ch)
		 in
		   (Fenv_res,[OTHER])
		 end
	     | MulExp.CON0{con,il,aux_regions,alloc} => (Fenv, [OTHER])
	     | MulExp.CON1({con,il,alloc},tr) => (Fenv, [OTHER])
	     | MulExp.DECON({con,il},tr) => FTrip tr Fenv Env
	     | MulExp.EXCON(excon,NONE) => (Fenv, [OTHER])
	     | MulExp.EXCON(excon,SOME(alloc,tr)) => FTrip tr Fenv Env
	     | MulExp.DEEXCON(excon,tr) => FTrip tr Fenv Env
	     | MulExp.RECORD(alloc, trs) => 
		 let
		   val Fenv_res = List.foldl (fn (tr,base) => #1(FTrip tr base Env)) Fenv trs
		 in
		   (Fenv_res, [OTHER])
		 end
	     | MulExp.SELECT(i,tr) => FTrip tr Fenv Env
	     | MulExp.DEREF tr => FTrip tr Fenv Env
	     | MulExp.REF(a,tr) => FTrip tr Fenv Env
	     | MulExp.ASSIGN(alloc,tr1,tr2) => 
		 let
		   val (Fenv1,_) = FTrip tr1 Fenv Env
		   val (Fenv2,_) = FTrip tr2 Fenv1 Env
		 in
		   (Fenv2, [OTHER])
		 end
	     | MulExp.EQUAL({mu_of_arg1,mu_of_arg2,alloc},tr1, tr2) =>
		 let
		   val (Fenv1,_) = FTrip tr1 Fenv Env
		   val (Fenv2,_) = FTrip tr2 Fenv1 Env
		 in
		   (Fenv2, [OTHER])
		 end
	     | MulExp.CCALL({name, mu_result, rhos_for_result}, trs) =>
		 let
		   val Fenv_res = List.foldl (fn (tr,base) => #1(FTrip tr base Env)) Fenv trs
		 in
		   (Fenv_res, [OTHER])
		 end
	     | MulExp.RESET_REGIONS({force, alloc, regions_for_resetting},tr) => FTrip tr Fenv Env
	     | MulExp.FRAME{declared_lvars, declared_excons} => 
		 (List.foldl (fn ({lvar,...},base) => if is_in_dom_Fenv base lvar then
			                    rem_Fenv base lvar
					  else 
					    base) Fenv declared_lvars, [OTHER]))
	  in
	    FExp e Fenv Env
	  end
	      
	val (Fenv', _) = FTrip tr FuncEnv.empty (fresh_Env import_vars ([],[],[]))

	(* Remove all export_vars from Fenv'. They must be closure implemented. *)
	val Fenv_res = List.foldl (fn (lvar,base) => 
				    if is_in_dom_Fenv base lvar then
				      rem_Fenv base lvar
				    else
				      base) Fenv' (#1(export_vars))

(*	val _ = pr_st (pp_Fenv Fenv_res) Debug *)
      in
	Fenv_res
      end
  end

  (**********************)
  (* Closure Conversion *)
  (**********************)

  local
    structure CE = ClosConvEnv

    local 
      val lv_no = ref 0
      val lab_no = ref 0
    in
      fun reset_lvars() = lv_no := 0
      fun reset_labs() = lab_no := 0
      fun fresh_lvar name = (lv_no := !lv_no+1; Lvars.new_named_lvar ("cc" ^ Int.toString(!lv_no)))
      fun fresh_lab name = (lab_no := !lab_no+1; Labels.new_named (name ^ Int.toString(!lab_no)))
    end

    (* ------------------------------------------------------- *)
    (*    Utility Functions on Storage Mode Annotations        *)
    (* ------------------------------------------------------- *)
    val dummy_sma = IGNORE

    fun get_ce(ATTOP_LI (ce,pp)) = ce
      | get_ce(ATTOP_LF (ce,pp)) = ce
      | get_ce(ATTOP_FI (ce,pp)) = ce
      | get_ce(ATTOP_FF (ce,pp)) = ce
      | get_ce(ATBOT_LI (ce,pp)) = ce
      | get_ce(ATBOT_LF (ce,pp)) = ce
      | get_ce(  SAT_FI (ce,pp)) = ce
      | get_ce(  SAT_FF (ce,pp)) = ce
      | get_ce(IGNORE) = die "get_ce: combination not recognized."

    fun insert_ce_in_sma(ce',ATTOP_LI (ce,pp)) = ATTOP_LI (ce',pp)
      | insert_ce_in_sma(ce',ATTOP_LF (ce,pp)) = ATTOP_LF (ce',pp)
      | insert_ce_in_sma(ce',ATTOP_FI (ce,pp)) = ATTOP_FI (ce',pp)
      | insert_ce_in_sma(ce',ATTOP_FF (ce,pp)) = ATTOP_FF (ce',pp)
      | insert_ce_in_sma(ce',ATBOT_LI (ce,pp)) = ATBOT_LI (ce',pp)
      | insert_ce_in_sma(ce',ATBOT_LF (ce,pp)) = ATBOT_LF (ce',pp)
      | insert_ce_in_sma(ce',  SAT_FI (ce,pp)) =   SAT_FI (ce',pp)
      | insert_ce_in_sma(ce',  SAT_FF (ce,pp)) =   SAT_FF (ce',pp)
      | insert_ce_in_sma(ce',IGNORE)= die "insert_ce_in_sma: combination not recognized."

    fun convert_sma (AtInf.ATTOP(rho,pp),CE.LI,ce) = ATTOP_LI (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.LF,ce) = ATTOP_LF (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.FI,ce) = ATTOP_FI (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.FF,ce) = ATTOP_FF (ce,pp)
      | convert_sma (AtInf.ATBOT(rho,pp),CE.LI,ce) = ATBOT_LI (ce,pp)
      | convert_sma (AtInf.ATBOT(rho,pp),CE.LF,ce) = ATBOT_LF (ce,pp)
      | convert_sma (AtInf.SAT(rho,pp),CE.FI,ce) = SAT_FI (ce,pp)
      | convert_sma (AtInf.SAT(rho,pp),CE.FF,ce) = SAT_FF (ce,pp)    
      | convert_sma  _ = die "convert_sma: sma combination not recognized."

    (* ----------------------------------------------- *)
    (*    Utility Functions on Select Expressions      *)
    (* ----------------------------------------------- *)
    datatype select_exp =
        SELECT_SE of lvar * int * lvar (* Value bound in closure: lv1 = #i(lv2) *)
      | FETCH_SE of lvar * label       (* Global defined value: lv = fetch(lab) *)
      | NONE_SE                        (* No select expression                  *)

    fun lt_se(NONE_SE, _) = true
      | lt_se(FETCH_SE _ ,NONE_SE) = false
      | lt_se(FETCH_SE(_,lab1),FETCH_SE(_,lab2)) =
          if Labels.lt(lab1,lab2) then
	    true
	  else
	    false
      | lt_se(FETCH_SE _, SELECT_SE _) = true
      | lt_se(SELECT_SE _, NONE_SE) = false
      | lt_se(SELECT_SE _, FETCH_SE _) = false
      | lt_se(SELECT_SE(_,i1,lv1), SELECT_SE(_,i2,lv2)) =
	    if (i1<i2) then
	      true
	    else 
	      if (i1=i2) andalso Lvars.lt(lv1,lv2) then
		true
	      else
		false

    structure SEMap = (* The map (se --> ce) is used temporarily by unify_ce_se and unify_sma_se only *)
      OrderFinMap(structure Order =
		    struct
		      type T = select_exp
		      fun lt(se1: T) se2 = lt_se(se1,se2)
		    end
		  structure PP =PP
		  structure Report = Report)

    fun unify_ce_se ces_and_ses se_map =
      let
	fun resolve((ce,NONE_SE),(ces,ses,se_map)) = (ce::ces,ses,se_map) 
	  | resolve((ce,se),(ces,ses,se_map)) =
	  (case SEMap.lookup se_map se of
	       NONE => (ce::ces,se::ses,SEMap.add (se,ce,se_map))
	     | SOME(ce') => (ce'::ces,ses,se_map))
      in
        List.foldr resolve ([],[],se_map) ces_and_ses
      end

    (* Must keep the sma information and only exchange the expression inside the sma *)
    fun unify_sma_se smas_and_ses se_map =
      let
	fun resolve((sma,NONE_SE),(smas,ses,se_map)) = (sma::smas,ses,se_map) 
	  | resolve((sma,se),(smas,ses,se_map)) =
	  (case SEMap.lookup se_map se of
	       NONE => (sma::smas,se::ses,SEMap.add (se,get_ce(sma),se_map))
	     | SOME(ce') => (insert_ce_in_sma(ce',sma)::smas,ses,se_map))
      in
	List.foldr resolve ([],[],se_map) smas_and_ses
      end

    fun unify_smas_ces_and_ses (smas_and_ses,ces_and_ses) =
      let
	val (smas,ses_smas,se_map) = unify_sma_se smas_and_ses SEMap.empty
	val (ces,ses,_) = unify_ce_se ces_and_ses se_map
      in
	(smas,ces,ses_smas@ses)
      end

    fun insert_se(ce,NONE_SE) = ce
      | insert_se(ce,SELECT_SE (lv1,i,lv2)) =
          LET{pat=[lv1],
	      bind=SELECT(i,VAR lv2),
	      scope=ce}
      | insert_se(ce,FETCH_SE(lv,lab)) =
	  LET{pat=[lv],
	      bind=FETCH lab,
	      scope=ce}

(*    fun insert_ses(ce,ses) = List.foldr (fn (se,ce) => insert_se(ce,se)) ce ses Now we use unboxed records 03/12/1998, Niels *)

    fun insert_ses(ce,ses) = 
      let
	fun filter(acc,NONE_SE) = acc
	  | filter((lvs,ces),SELECT_SE (lv1,i,lv2)) = (lv1::lvs,SELECT(i,VAR lv2)::ces)
	  | filter((lvs,ces),FETCH_SE(lv,lab)) = (lv::lvs,FETCH lab::ces)
	val (lvs,ces) = List.foldr (fn (se,acc) => filter(acc,se)) ([],[]) ses
      in
	(case lvs of
	   [] => ce
	 | _ => LET{pat=lvs,
		    bind=UB_RECORD ces,
		    scope=ce})
      end


    (* ----------------------------------------- *)
    (*    Utility Functions on Environments      *)
    (* ----------------------------------------- *)
    infix plus_decl_with
    fun (env plus_decl_with declare) pairs =
      List.foldr (fn ((x,y),env) => declare(x,y,env)) env pairs

    (* global_env is the initial environment at entry to functions excluding *)
    (* the call convention.                                                  *)
    local 
      val global_env = ref ClosConvEnv.empty
      val frame_env = ref ClosConvEnv.empty
    in
      fun set_global_env env = global_env := env
      fun get_global_env () = !global_env
      fun set_frame_env env = global_env := env
      fun get_frame_env () = !global_env
    end

    fun lookup_ve env lv =
      let
	fun resolve_se(CE.LVAR lv') = (VAR lv',NONE_SE)
	  | resolve_se(CE.SELECT(lv',i)) =
	      let
		val lv'' = fresh_lvar("lookup_ve")
	      in
		(VAR lv'',SELECT_SE(lv'',i,lv'))
	      end
	  | resolve_se(CE.LABEL lab) =
	      let
		val lv' = fresh_lvar("lookup_ve")
	      in
		(VAR lv',FETCH_SE (lv',lab))
	      end
	  | resolve_se _ = die "resolve_se: wrong FIX or RVAR binding in VE"
      in
	case CE.lookupVarOpt env lv of
	  SOME(CE.FIX(_,SOME a,_)) => resolve_se(a)
	| SOME(CE.FIX(_,NONE,_)) => die "lookup_ve: this case should be caught in APP."
	| SOME(a) => resolve_se(a)               
	| NONE  => die ("lookup_ve: lvar(" ^ (Lvars.pr_lvar lv) ^ ") not bound in env.")
      end

    fun lookup_fun env lv =
      case CE.lookupVarOpt env lv of
	  SOME(CE.FIX(lab,ce,size)) => (lab,size)
	| _ => die ("lookup_fun: function(" ^ Lvars.pr_lvar lv ^ ") does not exists")

    fun lookup_excon env excon =
      (case CE.lookupExconOpt env excon of
	 SOME(CE.LVAR lv') => (VAR lv',NONE_SE)
       | SOME(CE.SELECT(lv',i)) =>
	   let
	     val lv'' = fresh_lvar("lookup_excon")
	   in
	     (VAR lv'',SELECT_SE(lv'',i,lv'))
	   end
       | SOME(CE.LABEL lab) =>
	   let
	     val lv' = fresh_lvar("lookup_excon")
	   in
	     (VAR lv',FETCH_SE (lv',lab))
	   end
       | SOME _ => die "lookup_excon: excon bound to FIX or RVAR"
       | NONE  => die ("lookup_excon: excon(" ^ (Excon.pr_excon excon) ^ ") not bound"))

    fun lookup_rho env place =
      (case CE.lookupRhoOpt env place of
	 SOME(CE.LVAR lv') => (VAR lv',NONE_SE)
       | SOME(CE.RVAR place) => (RVAR place, NONE_SE)
       | SOME(CE.DROPPED_RVAR place) => (DROPPED_RVAR place, NONE_SE)
       | SOME(CE.SELECT(lv',i)) =>
	   let
	     val lv'' = fresh_lvar("lookup_rho")
	   in
	     (VAR lv'',SELECT_SE(lv'',i,lv'))
	   end
      | SOME(CE.LABEL lab) =>
	   let
	     val lv' = fresh_lvar("lookup_rho")
	   in
	     (VAR lv',FETCH_SE (lv',lab))
	   end
       | SOME _ => die "lookup_rho: rho bound to FIX"
       | NONE  => die ("lookup_rho: rho(" ^ PP.flatten1(Effect.layout_effect place) ^ ") not bound"))

    fun convert_alloc (alloc,env) =
      (case alloc of
	   AtInf.ATBOT(rho,pp) => 
	     let
	       val (ce,se) = lookup_rho env rho
	     in
	       (convert_sma(AtInf.ATBOT(rho,pp),CE.lookupRhoKind env rho,ce),se)
	     end
         | AtInf.SAT(rho,pp) =>
	     let
	       val (ce,se) = lookup_rho env rho
	     in
	       (convert_sma(AtInf.SAT(rho,pp),CE.lookupRhoKind env rho,ce),se)
	     end
	 | AtInf.ATTOP(rho,pp) =>
	     let
	       val (ce,se) = lookup_rho env rho
	     in
	       (convert_sma(AtInf.ATTOP(rho,pp),CE.lookupRhoKind env rho,ce),se)
	     end
	 | AtInf.IGNORE => (IGNORE,NONE_SE))

    fun mult("f",PhysSizeInf.INF) = CE.FI
      | mult("f",PhysSizeInf.WORDS n) = CE.FF
      | mult("l",PhysSizeInf.INF) = CE.LI
      | mult("l",PhysSizeInf.WORDS n) = CE.LF
      | mult _ = die "mult: Wrong binding or phsize"

    fun lookup_con env con = 
      (case CE.lookupCon env con of
	 CE.ENUM i    => ENUM i
       | CE.BOXED i   => BOXED i
       | CE.UNBOXED i => UNBOXED i)

    (*------------------------------------------------------------------*)
    (* Analyse the datatype bindings and return an environment mapping  *)
    (* all constructors to representation information. Unary            *)
    (* constructors are enumerated from zero to the number of unary     *)
    (* constructors in the datatype. Similarly for nullary constructors.*)
    (* Note, that booleans and lists are allready defined,              *)
    (* see ClosConvEnv.sml, and lists are the only unboxed datatypes    *)
    (* that we have.                                                    *) 
    (*------------------------------------------------------------------*)
    fun add_datbinds_to_env (RegionExp.DATBINDS dbs) l2clos_exp_env : CE.env =
      let
	fun tags n0 n1 [] = []
	  | tags n0 n1 ((con,RegionExp.VALUE_CARRYING,_)::binds) = (con,CE.BOXED n1) :: tags n0 (n1+1) binds
	  | tags n0 n1 ((con,RegionExp.CONSTANT,_)::binds) = (con,CE.BOXED n0) :: tags (n0+1) n1 binds
	fun analyse_datbind (tyname,binds: (con * RegionExp.constructorKind * 'a) list) : (con * CE.con_kind) list =
	  tags 0 0 binds
    in
      List.foldl (fn (datbind,env) =>
		  (env plus_decl_with CE.declareCon) (analyse_datbind datbind))
                 l2clos_exp_env (concat_lists dbs)
    end

    (* -------------------------------- *)
    (*    Add Top Level Functions       *)
    (* -------------------------------- *)
    local
      val top_decl = ref []
    in
      fun reset_top_decls() = top_decl := []
      fun add_new_fn(lab,cc,e)  = top_decl := FN(lab,cc,e)::(!top_decl)
      fun add_new_fun(lab,cc,e) = top_decl := FUN(lab,cc,e)::(!top_decl)
      fun get_top_decls() = !top_decl
    end

    (* ------------------------------ *)  
    (*   General Utility Functions    *)
    (* ------------------------------ *)  

    (* remove_zero_sized_region_closure_lvars: remove lvars associated with zero sized *)
    (* region closures. No code is introduced for zero sized region closures and hence *)
    (* registers associated with zero sized region closures are never defined.         *)
    fun remove_zero_sized_region_closure_lvars env (lvs,rhos,excons) =
      let
	fun remove [] = []
	  | remove (lv::lvs) = 
	      (case CE.lookupVar env lv of 
	          CE.FIX(lab,NONE,0) => remove lvs
		| CE.FIX(lab,NONE,i) => die "remove_zero_sized_region_closure_lvars: FIX messed up"
		| CE.FIX(lab,SOME _,0) => die "remove_zero_sized_region_closure_lvars: FIX messed up"
		| _ => lv :: remove lvs)
      in
	(remove lvs,rhos,excons)
      end

    (* Determine the free variables for an ordinary or shared closure. *)
    (* org_env is the lexical environment in which this function is    *)
    (* declared --- used to look up the region sizes for the free      *)
    (* variables bound to letrec functions. new_env is used as base    *)
    (* when building the new environment.                              *)
    fun build_clos_env org_env new_env lv_clos base_offset (free_lv,free_excon,free_rho) =
      let 
	(* When computing offsets we do not increase the offset counter when meeting *)
	(* a lambda variable bound to a zero sized shared region closure, since code *)
	(* is not constructed to put such region closures into the actual closure.   *)
	fun add_free_lv (lv,(env,i)) = 
	  (case CE.lookupVar org_env lv of 
	     CE.FIX(lab,NONE,0)   => (CE.declareLvar(lv,CE.FIX(lab,NONE,0),env),i)
	   | CE.FIX(lab,NONE,s)   => die "add_free_lv: CE.FIX messed up."
	   | CE.FIX(lab,SOME _,0) => die "add_free_lv: CE.FIX messed up."
	   | CE.FIX(lab,SOME _,s) => (CE.declareLvar(lv,CE.FIX(lab,SOME(CE.SELECT(lv_clos,i)),s),env),i+1)
	   | _                    => (CE.declareLvar(lv,CE.SELECT(lv_clos,i),env),i+1))
	fun add_free_excon (excon,(env,i)) = (CE.declareExcon(excon,CE.SELECT(lv_clos,i),env),i+1)
	fun add_free_rho (place,(env,i)) = 
	  (CE.declareRhoKind(place,CE.lookupRhoKind org_env place,
			     CE.declareRho(place,CE.SELECT(lv_clos,i),env)),i+1)
	val (env',_)  = 
	  List.foldl add_free_rho 
	  (List.foldl add_free_excon 
	   (List.foldl add_free_lv (new_env, base_offset) free_lv) free_excon) free_rho
      in
	env'
      end

    (* Returns (ces,ses) corresponding to accessing the free variables *)
    fun gen_ces_and_ses_free env (free_lv,free_excon,free_rho) =
      let
	val lvs_and_ses = List.map (fn lv => lookup_ve env lv) free_lv
	val excons_and_ses = List.map (fn excon => lookup_excon env excon) free_excon 
	val rhos_and_ses = List.map (fn place => lookup_rho env place) free_rho
      in
	lvs_and_ses @ excons_and_ses @ rhos_and_ses
      end

    (* drop_rho rho: replace rho by a global region with the same runtime type; *)
    (* used when rho is letrec-bound, but never written into.                    *)
    fun drop_rho rho = 
      (case Effect.get_place_ty rho of
	 SOME Effect.WORD_RT   => Effect.toplevel_region_withtype_word
       | SOME Effect.STRING_RT => Effect.toplevel_region_withtype_string
       | SOME Effect.REAL_RT   => Effect.toplevel_region_withtype_real
       | SOME Effect.TOP_RT    => Effect.toplevel_region_withtype_top
       | SOME Effect.BOT_RT    => Effect.toplevel_region_withtype_bot
       | _ => die "drop_rho: no runtime-type")

    (* ces_and_ses contains the arguments. The function compiles the closure argument if exists. *)
    fun compile_letrec_app env lvar ces_and_ses =
      let
	val (lab_f,size_clos) = lookup_fun env lvar
      in
	if size_clos = 0 then
	  let
	    val (ces,ses,_) = unify_ce_se ces_and_ses SEMap.empty
	  in
	    (NONE,ces,ses,lab_f)
	  end
	else
	  let
	    val (ce_clos,se_clos) = lookup_ve env lvar
	    val (ces,ses,_) = unify_ce_se ((ce_clos,se_clos)::ces_and_ses) SEMap.empty
	    val (ce_clos',ces') = split_in_hd_and_tl ces
	  in
	    (SOME ce_clos',ces',ses,lab_f)
	  end
      end

    fun compile_sels_and_default sels default f_match ccTrip =
      let
	val sels' =
	  List.foldr (fn ((m,tr),sels_acc) => 
		      (f_match m, insert_se(ccTrip tr))::sels_acc) [] sels
      in
	case default of
	  SOME tr => (sels', insert_se(ccTrip tr))
	| NONE => 
	    (case rev sels' of
	       ((_,ce)::rev_sels') => (rev rev_sels',ce)
	     | _ => die "compile_sels_and_default: no selections.")
      end

    fun find_globals_in_env (lvars, excons, regvars) env =
      let
	fun lookup lv f_lookup =
	  (case f_lookup env lv of
	     SOME(CE.FIX(lab,_,_))    => lab
	   | SOME(CE.LVAR _)          => die "find_globals_in_env: global bound to lvar."
	   | SOME(CE.RVAR _)          => die "find_globals_in_env: global bound to rvar."
	   | SOME(CE.DROPPED_RVAR _)  => die "find_globals_in_env: global bound to dropped rvar."
	   | SOME(CE.SELECT _)        => die "find_globals_in_env: global bound to select expression."
	   | SOME(CE.LABEL lab)       => lab
	   | NONE                     => die ("find_globals_in_env: lvar not bound in env."))
	val lvar_labs = map (fn lv => lookup lv CE.lookupVarOpt) lvars
      in
	List.foldr (fn (excon,labs) => lookup excon CE.lookupExconOpt::labs) lvar_labs excons
      end

    fun gen_fresh_res_lvars(RegionExp.Mus type_and_places) =
      (case type_and_places of
	  [(RType.FUN(mus1,arroweffect,mus2),_)] => List.map (fn _ => fresh_lvar("res")) mus2
        | _ => die "gen_fresh_res: not a function type.")
      | gen_fresh_res_lvars(RegionExp.Frame _) = []
      | gen_fresh_res_lvars(RegionExp.RaisedExnBind) = []

    (* ------------------------ *)
    (*    Closure Conversion    *)
    (* ------------------------ *)
    fun ccTrip (MulExp.TR(e,metaType,ateffects,mulef)) env lab cur_rv =
      let
	fun ccExp e =
	  (case e of
	     MulExp.VAR{lvar,il, plain_arreffs,alloc,rhos_actuals,other} => lookup_ve env lvar 
	   | MulExp.INTEGER(i,alloc) => (INTEGER i, NONE_SE)
	   | MulExp.STRING(s,alloc) => (STRING s,NONE_SE)
	   | MulExp.REAL(r,alloc) => 
	       let
		 fun convert r =    (* Translate a real constant into C notation: *)
		   let fun conv #"~" = #"-"
			 | conv #"E" = #"e"
			 | conv c = c
		   in 
		     (implode o (map conv) o explode) r
		   end
	       in
		 (REAL (convert r),NONE_SE)
	       end

	   | MulExp.UB_RECORD trs => 
	       let
		 val ces_and_ses = List.map (fn tr => ccTrip tr env lab cur_rv) trs
		 val (ces,ses,_) = unify_ce_se ces_and_ses SEMap.empty
	       in
		 (insert_ses(UB_RECORD ces,ses),NONE_SE)
	       end
	   | MulExp.FN{pat,body,free=ref (SOME free_vars_all),alloc} => 
	       (* For now, the function is closure implemented. *)
	       (* Free variables must go into the closure. All free variables  *)
	       (* (free_vars_all) must be bound in the closure environment,    *)
	       (* while we do not store region closures with no free variables *) 
               (* in the actual closure.                                       *)
	       let
		 val free_vars = remove_zero_sized_region_closure_lvars env free_vars_all

		 val new_lab = fresh_lab (Labels.pr_label lab ^ ".anon")
		 val lv_clos = fresh_lvar("clos")
		 val args = List.map #1 pat
		 val ress = gen_fresh_res_lvars metaType (* Result variables are not bound in env as they only exists in cc *)
		 val cc = CallConv.mk_cc_fn(args,SOME lv_clos,[],ress)

		 val env_body = build_clos_env env (get_global_env()) lv_clos BI.init_clos_offset free_vars_all
		 val env_with_args = (env_body plus_decl_with CE.declareLvar) (map (fn lv => (lv, CE.LVAR lv)) args)

		 val ces_and_ses = gen_ces_and_ses_free env free_vars

		 val _ = add_new_fn(new_lab, cc, insert_se(ccTrip body env_with_args new_lab NONE))
		 val (sma,se_sma) = convert_alloc(alloc,env)
		 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_sma)],ces_and_ses)
	       in
		 (insert_ses(CLOS_RECORD{label=new_lab, elems=ces, alloc=one_in_list(smas)},ses),NONE_SE)
	       end
	   | MulExp.FN _ => die "ccExp: FN with no free vars info"
	   | MulExp.FIX{free=ref (SOME free_vars_all),shared_clos=alloc,functions,scope} =>
	       (* For now, the functions are closure implemented *)
	       (* Note, that we may pass a shared closure to a function even though it isn't used by the function. *)
	       (* It is not necessary to pass a shared closure to a FIX bound function f iff:                      *)
	       (*   1- f has no free variables except FIX bound functions.                                         *)
	       (*   2- f does not call another FIX bound function g using the shared closure.                      *)
	       let
		 val free_vars_in_shared_clos = remove_zero_sized_region_closure_lvars env free_vars_all
		 val shared_clos_size = size3 free_vars_in_shared_clos

		 val lv_sclos = fresh_lvar("sclos")
		 val ces_and_ses = gen_ces_and_ses_free env free_vars_in_shared_clos

		 val lvars = map #lvar functions
		 val binds = map #bind functions
		 val formalss = map (! o #rhos_formals) functions (* place*phsize *)
		 val dropss = map (valOf o #bound_but_never_written_into) functions
		   handle Option => die "FIX.dropps: bound but never written was None"

		 val labels = map (fn lv => fresh_lab(Lvars.pr_lvar lv)) lvars
		 val lvars_and_labels = zip(lvars,labels)

		 val env_scope =
		   if shared_clos_size = 0 then
		     (env plus_decl_with CE.declareLvar)
		     (map (fn (lv,lab) => (lv,CE.FIX(lab,NONE,0))) lvars_and_labels)
		   else
		     (env plus_decl_with CE.declareLvar)
		     (map (fn (lv,lab) => (lv,CE.FIX(lab,SOME(CE.LVAR lv_sclos),shared_clos_size))) lvars_and_labels)
		   
		 fun compile_fn (lvar,bind,formals,drops,lab) =
		   let
		     val (args,body,metaType) = case bind of
		       MulExp.TR(MulExp.FN{pat,body,...},metaType,_,_) => (List.map #1 pat, body,metaType)
		     | _ => die "compile_fn: bind is not a FN"
		     val ress = gen_fresh_res_lvars metaType (* Result variables are not bound in env as they only exists in cc *)
		     val lv_sclos_fn = fresh_lvar("sclos")
		     val env_bodies = build_clos_env env (get_global_env()) lv_sclos_fn BI.init_sclos_offset free_vars_all

		     val env_with_funs =
		       if shared_clos_size = 0 then
			 (env_bodies plus_decl_with CE.declareLvar)
			 (map (fn (lv,lab) => (lv,CE.FIX(lab,NONE,0))) lvars_and_labels)
		       else
			 (env_bodies plus_decl_with CE.declareLvar)
			 (map (fn (lv,lab) => (lv,CE.FIX(lab,SOME(CE.LVAR lv_sclos_fn),shared_clos_size))) lvars_and_labels)
		     val lv_rv = fresh_lvar("rv")
		     val (env_with_rv,_) =
		       List.foldl (fn ((place,_),(env,i)) =>
				    (CE.declareRho(place,CE.SELECT(lv_rv,i),env),i+1)) (env_with_funs, BI.init_regvec_offset) formals
		     val env_with_rho_kind =
			  (env_with_rv plus_decl_with CE.declareRhoKind)
			  (map (fn (place,phsize) => (place,mult("f",phsize))) formals)
		     val env_with_rho_drop = 
			   (env_with_rho_kind plus_decl_with CE.declareRho)
			   (map (fn (place,_) => (place,CE.DROPPED_RVAR(drop_rho place))) drops)
		     val env_with_rho_drop_kind =
		           (env_with_rho_drop plus_decl_with CE.declareRhoKind)
			   (map (fn(place,phsize) => (place,mult("f",phsize))) drops) (* new 23/11/1998, Niels*)
		     val env_with_args =
		           (env_with_rho_drop_kind plus_decl_with CE.declareLvar)
			   (map (fn lv => (lv, CE.LVAR lv)) args)
		     val cc = CallConv.mk_cc_fun(args,SOME lv_sclos_fn,[],SOME lv_rv,[],ress)
		   in
		     add_new_fun(lab,cc,insert_se(ccTrip body env_with_args lab (SOME (VAR lv_rv))))
		   end
		 val _ = List.app compile_fn (zip5 (lvars,binds,formalss,dropss,labels))
	       in
		 if shared_clos_size = 0 then
		   (insert_se(ccTrip scope env_scope lab cur_rv),NONE_SE)
		 else
		   let
		     val (sma,se_a) = convert_alloc(alloc,env)
		     val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],ces_and_ses)
		   in
		     (insert_ses(LET{pat=[lv_sclos],
				     bind= SCLOS_RECORD{elems=ces,alloc=one_in_list(smas)},
				     scope=insert_se(ccTrip scope env_scope lab cur_rv)},
				 ses),NONE_SE)
		   end
	       end
	   | MulExp.FIX{free=_,shared_clos,functions,scope} => die "ccExp: No free variables in FIX"

	   | MulExp.APP(SOME MulExp.JMP, _, tr1 as MulExp.TR(MulExp.VAR{lvar,alloc,rhos_actuals,...}, _, _, _), tr2) =>
	       (* Poly tail call so we reuse the region vector stored in cur_rv *)
	       let
		 val ces_and_ses = (* We remove the unboxed record. *)
		   case tr2 of
		     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
		   | _ => [ccTrip tr2 env lab cur_rv]

		 val (ce_clos,ces_arg,ses,lab_f) = compile_letrec_app env lvar ces_and_ses
	       in
		 (insert_ses(JMP{opr=lab_f,args=ces_arg,reg_vec=cur_rv,reg_args=[],clos= ce_clos,free=[]},
			     ses),NONE_SE)
	       end
	   | MulExp.APP(SOME MulExp.JMP, _, tr1 (*not lvar: error *), tr2) => die "JMP to other than lvar"
	   | MulExp.APP(SOME MulExp.FUNCALL, _,
			tr1 as MulExp.TR(MulExp.VAR{lvar,alloc as (SOME atp), rhos_actuals=ref rhos_actuals,...},_,_,_), 
			tr2) =>
	       let
		 val ces_and_ses = (* We remove the unboxed record. *)
		   case tr2 of
		     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
		   | _ => [ccTrip tr2 env lab cur_rv]

		 val (ce_clos,ces_arg,ses,lab_f) = compile_letrec_app env lvar ces_and_ses

		 val actual_region_vector_size = List.length rhos_actuals
		 val (sma,smas,ses_sma) =
		   if actual_region_vector_size = 0 then
		     (dummy_sma,[],[]) (* sma is dummy and never used *)
		   else
		     let
		       val (sma,se_sma) = convert_alloc(atp,env)
		       val smas_regvec_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) rhos_actuals

		       val (smas,ses_sma,_) = unify_sma_se ((sma,se_sma)::smas_regvec_and_ses) SEMap.empty
		       val (sma,smas_regvec) = split_in_hd_and_tl smas
		     in
		       (sma,smas_regvec,ses_sma)
		     end
		 val rv = fresh_lvar ("rv")
	       in
		 if actual_region_vector_size = 0 then
		   (insert_ses(FUNCALL{opr=lab_f,args=ces_arg,reg_vec=NONE,reg_args=[],clos=ce_clos,free=[]},
			       ses),NONE_SE)
		 else
		    (insert_ses(LET{pat=[rv],
				    bind=REGVEC_RECORD{elems=smas,alloc=sma},
				    scope=insert_ses(FUNCALL{opr=lab_f,args=ces_arg,reg_vec=SOME (VAR rv),
							     reg_args=[],clos=ce_clos,free=[]},
						     ses)},ses_sma),NONE_SE)
	       end
	  | MulExp.APP(SOME MulExp.FNJMP,_, tr1,tr2) =>
	       let
		 val ces_and_ses = 
		   case tr2 of
		     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
		   | _ => [ccTrip tr2 env lab cur_rv]
		 val (ce_opr,se_opr) = ccTrip tr1 env lab cur_rv
		 val (ces1,ses',_) = unify_ce_se ((ce_opr,se_opr)::ces_and_ses) SEMap.empty
		 val (ce_opr',ces') = split_in_hd_and_tl ces1
	       in
		 (insert_ses(FNJMP{opr=ce_opr',args=ces',clos=SOME ce_opr', free=[]},
			     ses'),NONE_SE)
	       end
	   | MulExp.APP(NONE,_, (*  primitive *)
			tr1 as MulExp.TR(MulExp.VAR{lvar,alloc as NONE, rhos_actuals=ref rhos_actuals,...},_,_,_), 
			tr2) =>
	       let
		 val ces_and_ses = 
		   (case tr2 of
		      MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => 
			List.map (fn tr => ccTrip tr env lab cur_rv) trs    (* all primitives have *)
		      | _ => die "APP.lvar.prim.args not UB_RECORD")        (* unboxed arguments.  *)

		 val prim_name =
		   (case (Lvars.primitive lvar, rhos_actuals) of
		      (NONE, []) => die ("APP.expected primitive: " ^ Lvars.pr_lvar lvar)
		    | (NONE, _) => die ("APP.non-primitive with unboxed region parameters: lvar = " ^ Lvars.pr_lvar lvar)
		    | (SOME prim, _) =>
			(case prim of
			   Lvars.PLUS_INT        => "PLUS_INT"
			 | Lvars.MINUS_INT       => "MINUS_INT"
			 | Lvars.MUL_INT         => "MUL_INT"
			 | Lvars.NEG_INT         => "NEG_INT"
			 | Lvars.ABS_INT         => "ABS_INT"
			 | Lvars.LESS_INT        => "LESS_INT"
			 | Lvars.LESSEQ_INT      => "LESSEQ_INT"
			 | Lvars.GREATER_INT     => "GREATER_INT"
			 | Lvars.GREATEREQ_INT   => "GREATEREQ_INT"
			 | Lvars.PLUS_FLOAT      => "PLUS_FLOAT"
			 | Lvars.MINUS_FLOAT     => "MINUS_FLOAT"
			 | Lvars.MUL_FLOAT       => "MUL_FLOAT"
			 | Lvars.DIV_FLOAT       => "DIV_FLOAT"
			 | Lvars.NEG_FLOAT       => "NEG_FLOAT"
			 | Lvars.ABS_FLOAT       => "ABS_FLOAT"
			 | Lvars.LESS_FLOAT      => "LESS_FLOAT"
			 | Lvars.LESSEQ_FLOAT    => "LESSEQ_FLOAT"
			 | Lvars.GREATER_FLOAT   => "GREATER_FLOAT"
			 | Lvars.GREATEREQ_FLOAT => "GREATEREQ_FLOAT"))

		 val smas_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) rhos_actuals

		 val (smas,ces,ses) = unify_smas_ces_and_ses (smas_and_ses,ces_and_ses)
		 (* Only real primitives allocate and only one time. *)
		 val smas_ccall = map (fn sma => PASS_PTR_TO_MEM(sma,BI.size_of_real())) smas
		 val fresh_lvs = map (fn _ => fresh_lvar "sma") smas_ccall
		 fun maybe_insert_smas([],[],ce) = ce
		   | maybe_insert_smas(fresh_lvs,smas,ce) =
		   LET{pat=fresh_lvs,bind=UB_RECORD smas,scope=ce}

	       in
		 (insert_ses(maybe_insert_smas(fresh_lvs,smas_ccall,
					       CCALL{name=prim_name,
						     args=ces,
						     rhos_for_result=map VAR fresh_lvs}),ses),NONE_SE)
	       end
	   | MulExp.APP(NONE,_, (*  primitive *)
			tr1, (* not lvar: error *)
			tr2) =>  die "expected primitive operation"
	   | MulExp.APP(SOME MulExp.FNCALL,_, tr1, tr2) =>
	       let
		 val ces_and_ses = 
		   case tr2 of
		     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
		   | _ => [ccTrip tr2 env lab cur_rv]
		 val (ce_opr,se_opr) = ccTrip tr1 env lab cur_rv
		 val (ces1,ses',_) = unify_ce_se ((ce_opr,se_opr)::ces_and_ses) SEMap.empty
		 val (ce_opr',ces') = split_in_hd_and_tl ces1
	       in
		 (insert_ses(FNCALL{opr=ce_opr',args=ces',clos=SOME ce_opr', free=[]},
			     ses'),NONE_SE)
	       end
	   | MulExp.APP _ => die "application form not recognised"

	   | MulExp.LETREGION{B,rhos=ref bound_regvars,body} => 
	       let
		 val env_with_kind =
		   (env plus_decl_with CE.declareRhoKind)
		   (map (fn (place,phsize) => (place,mult("l",phsize))) bound_regvars)
		 val env_body =
		    (env_with_kind plus_decl_with CE.declareRho)
		    (map (fn (place,_) => (place,CE.RVAR place)) bound_regvars)
	       in
		 (LETREGION{rhos=bound_regvars,
			    body=insert_se(ccTrip body env_body lab cur_rv)},NONE_SE)
	       end
	   | MulExp.LET{k_let,pat,bind,scope} =>
	       let
		 val lvars = List.map #1 pat
		 val env_with_lvar =
		   (env plus_decl_with CE.declareLvar)
		   (map (fn lv => (lv,CE.LVAR lv)) lvars)
	       in
		 (LET{pat=lvars,
		      bind=insert_se(ccTrip bind env lab cur_rv),
		      scope=insert_se(ccTrip scope env_with_lvar lab cur_rv)},NONE_SE)
	       end
	   | MulExp.EXCEPTION(excon,true,typePlace,alloc,scope) => (* Nullary exception constructor *)
	       let
		 val lv1 = fresh_lvar "exn0-1"
		 val lv2 = fresh_lvar "exn0-2"
		 val lv3 = fresh_lvar "exn0-3"
		 val lv4 = fresh_lvar "exn0-4"
		 val env' = CE.declareExcon(excon,CE.LVAR lv4,env)
		 val (sma,se_a) = convert_alloc(alloc,env)
	       in
		 (LET{pat=[lv1], 
		      bind=CCALL{name="fresh_exname",
				 args=[],
				 rhos_for_result=[]},
		      scope=insert_se(LET{pat=[lv2], 
					  bind=STRING (Excon.pr_excon excon),
					  scope=LET{pat=[lv3],
						    bind=RECORD{elems=[VAR lv1,VAR lv2],
								alloc=sma},
						    scope=LET{pat=[lv4],
							      bind=RECORD{elems=[VAR lv3],
									  alloc=sma},
							      scope=insert_se (ccTrip scope env' lab cur_rv)}}},
				      se_a)},NONE_SE)
	       end
	   | MulExp.EXCEPTION(excon,false,typePlace,alloc,scope) => (* Unary exception constructor *)
	       let
		 val lv1 = fresh_lvar "exn0-1"
		 val lv2 = fresh_lvar "exn0-2"
		 val lv3 = fresh_lvar "exn0-3"
		 val env' = CE.declareExcon(excon,CE.LVAR lv3,env)
		 val (sma,se_a) = convert_alloc(alloc,env)
	       in
		 (LET{pat=[lv1], 
		      bind=CCALL{name="fresh_exname",
				 args=[],
				 rhos_for_result=[]},
		      scope=LET{pat=[lv2], 
				bind=STRING (Excon.pr_excon excon),
				scope=insert_se(LET{pat=[lv3],
						    bind=RECORD{elems=[VAR lv1,VAR lv2],
								alloc=sma},
						    scope=insert_se (ccTrip scope env' lab cur_rv)},se_a)}},NONE_SE)
	       end
	   | MulExp.RAISE tr => 
	       let
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(RAISE ce,se),NONE_SE)
	       end
	   | MulExp.HANDLE(tr1,tr2) => (HANDLE (insert_se(ccTrip tr1 env lab cur_rv),
						insert_se(ccTrip tr2 env lab cur_rv)),NONE_SE)
	   | MulExp.SWITCH_I(MulExp.SWITCH(tr,selections,opt)) =>
	       let
		 val (selections,opt) = 
		   compile_sels_and_default selections opt (fn m=>m) (fn tr => ccTrip tr env lab cur_rv)
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(SWITCH_I(SWITCH(ce,selections,opt)),se),NONE_SE)
	       end
	   | MulExp.SWITCH_S(MulExp.SWITCH(tr,selections,opt)) =>
	       let 
		 val (selections,opt) =
		   compile_sels_and_default selections opt (fn m=>m) (fn tr => ccTrip tr env lab cur_rv)
		 val (ce,se) = ccTrip tr env lab cur_rv
		 fun compile_seq_switch(ce,[],default) = default
		   | compile_seq_switch(ce,(s,ce')::rest,default) =
		   let
		     val lv_sw = fresh_lvar("sw")
		     val lv_s = fresh_lvar("str")
		   in
		     LET{pat=[lv_s],
			 bind=STRING s,
			 scope=LET{pat=[lv_sw],
				   bind=CCALL{name="equalString",args=[ce,VAR lv_s],rhos_for_result=[]},
				   scope=SWITCH_I(SWITCH(VAR lv_sw,[(BI.ml_true,ce')],
							 compile_seq_switch(ce,rest,default)))}}
		   end
	       in
		 (insert_se(compile_seq_switch(ce,selections,opt),se),NONE_SE)
	       end

	   | MulExp.SWITCH_C(MulExp.SWITCH(tr,selections,opt)) =>
	       let
		 val (selections,opt) =
		   compile_sels_and_default selections opt (fn m=>(m,lookup_con env m)) 
		                            (fn tr => ccTrip tr env lab cur_rv)
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(SWITCH_C(SWITCH(ce,selections,opt)),se),NONE_SE)
	       end
	   | MulExp.SWITCH_E(MulExp.SWITCH(tr,selections,opt)) =>
	       let
		 val (selections,opt) =
		   compile_sels_and_default selections opt (fn m=>lookup_excon env m) (fn tr => ccTrip tr env lab cur_rv)
		 val (ce,se) = ccTrip tr env lab cur_rv
		 fun compile_seq_switch(ce,[],default) = default
		   | compile_seq_switch(ce,((ce_e,se_e),ce')::rest,default) =
		   let
		     val lv_sw = fresh_lvar("sw")
		     val lv_exn1 = fresh_lvar("exn1")
		     val lv_exn2 = fresh_lvar("exn2")
		   in
		     LET{pat=[lv_exn1],
			 bind=insert_se(SELECT(0,ce_e),se_e),
			 scope=LET{pat=[lv_exn2],
				   bind=SELECT(0,VAR lv_exn1),
				   scope=LET{pat=[lv_sw],
					     bind=CCALL{name="EQUAL_INT",args=[ce,VAR lv_exn2],rhos_for_result=[]},
					     scope=SWITCH_I(SWITCH(VAR lv_sw,[(BI.ml_true,ce')],
								   compile_seq_switch(ce,rest,default)))}}}
		   end
		 val lv_exn_arg1 = fresh_lvar("exn_arg1")
		 val lv_exn_arg2 = fresh_lvar("exn_arg2")
		 val ce_res =
		   LET{pat=[lv_exn_arg1],
			 bind=SELECT(0,ce),
			 scope=LET{pat=[lv_exn_arg2],
				   bind=SELECT(0,VAR lv_exn_arg1),
				   scope=insert_se(compile_seq_switch(ce,selections,opt),se)}}
	       in
		 (ce_res,NONE_SE)
	       end
	   | MulExp.CON0{con,il,aux_regions,alloc} => 
	       let
		 val (sma,se_a) = convert_alloc(alloc,env)

		 val smas_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) aux_regions

		 val (smas,ses',_) = unify_sma_se ((sma,se_a)::smas_and_ses) SEMap.empty
		 val (sma',smas') = split_in_hd_and_tl smas
	       in
		 (insert_ses(CON0{con=con,
				  con_kind=lookup_con env con,
				  aux_regions=smas',
				  alloc=sma'},ses'),NONE_SE)
	       end
	   | MulExp.CON1({con,il,alloc},tr) =>
	       let
		 val (sma,se_a) = convert_alloc(alloc,env)
		 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
		 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],[(ce_arg,se_arg)])
	       in
		 (insert_ses(CON1{con=con,
				 con_kind=lookup_con env con,
				 alloc=one_in_list(smas),
				 arg=one_in_list(ces)},ses),NONE_SE)
	       end
	   | MulExp.DECON({con,il},tr) =>
	       let
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(DECON{con=con,
				  con_kind=lookup_con env con,
				  con_exp = ce},se),NONE_SE)
	       end
	   | MulExp.EXCON(excon,NONE) => lookup_excon env excon
	   | MulExp.EXCON(excon,SOME(alloc,tr)) => 
	       let
		 val (ce_excon,se_excon) = lookup_excon env excon
		 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
		 val (sma,se_a) = convert_alloc(alloc,env)
		 val (smas,ces,ses) = unify_smas_ces_and_ses ([(sma,se_a)],[(ce_excon,se_arg),(ce_arg,se_excon)])
	       in
		 (insert_ses(RECORD{elems=ces,
				    alloc=one_in_list(smas)},ses),NONE_SE)
	       end
	   | MulExp.DEEXCON(excon,tr) =>
	       let
		 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(SELECT(1,ce_arg),se_arg),NONE_SE)
	       end
	   | MulExp.RECORD(alloc, trs) => 
	       let
		 val ces_and_ses = List.foldr (fn (tr,b) => ccTrip tr env lab cur_rv::b) [] trs
		 val (sma,se_a) = convert_alloc(alloc,env)
		 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],ces_and_ses)
	       in
		 (insert_ses(RECORD{elems=ces,
				    alloc=one_in_list(smas)},ses),NONE_SE)
	       end
	   | MulExp.SELECT(i,tr) => 
	       let
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(SELECT(i,ce),se),NONE_SE)
	       end
	   | MulExp.REF(a,tr) =>
	       let
		 val (ce,se) = ccTrip tr env lab cur_rv
		 val (sma,se_sma) = convert_alloc(a,env)
		 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_sma)],[(ce,se)])
	       in
		 (insert_ses(REF(one_in_list(smas),one_in_list(ces)),ses),NONE_SE)
	       end
	   | MulExp.DEREF tr => 
	       let
		 val (ce,se) = ccTrip tr env lab cur_rv
	       in
		 (insert_se(DEREF ce,se),NONE_SE)
	       end
	   | MulExp.ASSIGN(alloc,tr1,tr2) =>
	       let
		 val (sma,se_sma) = convert_alloc(alloc,env)
		 val (sma,ce1,ce2,ses) = 
		   case unify_smas_ces_and_ses([(sma,se_sma)],[ccTrip tr1 env lab cur_rv,ccTrip tr2 env lab cur_rv]) of
		     ([sma],[ce1,ce2],ses) => (sma,ce1,ce2,ses)
		   | _ => die "ASSIGN: error in unify."
	       in
		 (insert_ses(ASSIGN(sma,ce1,ce2),ses),NONE_SE)
	       end
	   | MulExp.EQUAL({mu_of_arg1,mu_of_arg2,alloc},tr1,tr2) =>
	       let 
		 val tau = 
		   (case tr1 of 
		      MulExp.TR(_,RegionExp.Mus[(tau,_)],_,_) => tau
		    | _ => die "EQUAL.metaType not Mus.")

		 val (ce1,ce2,ses) = 
		   (case unify_ce_se [ccTrip tr1 env lab cur_rv,
				      ccTrip tr2 env lab cur_rv] SEMap.empty of
		      ([ce1,ce2],ses,_) => (ce1,ce2,ses)
		    | _ => die "EQUAL: error in unify.")
		      
		 val ce =
		   (case tau of
		      RType.CONSTYPE(tn,_,_,_) =>
			if TyName.eq(tn,TyName.tyName_INT) orelse TyName.eq(tn,TyName.tyName_BOOL) orelse TyName.eq(tn,TyName.tyName_REF) then
			  CCALL{name="EQUAL_INT",args=[ce1,ce2],rhos_for_result=[]}
			else if TyName.eq(tn,TyName.tyName_STRING) then
			  CCALL{name="equalString",args=[ce1,ce2],rhos_for_result=[]}
			     else if TyName.eq(tn,TyName.tyName_WORD_TABLE) then
			       die "`=' on word_tables! EliminateEq should have dealt with this"
				  (*TODO 11/02/1998 13:47. tho.  You can delete these two
				   die's when EliminateEq has been changed.*)
				  else CCALL{name="equalPoly",args=[ce1,ce2],rhos_for_result=[]}
		    | RType.RECORD [] => CCALL{name="EQUAL_INT",args=[ce1,ce2],rhos_for_result=[]} 
		    | _ => CCALL{name="equalPoly",args=[ce1,ce2],rhos_for_result=[]})
	       in
		 (insert_ses(ce,ses),NONE_SE)
	       end
	   | MulExp.CCALL({name, mu_result, rhos_for_result}, trs) =>
	       (* Regions in mu_result must be passed to the C-function for storing  *)
  	       (* the result of the call.  Regions are passed in two ways, dependent *)
	       (* on whether the size of the allocation in the region can be         *)
	       (* determined statically.  Either, (1) a pointer to the region is     *)
	       (* passed, or (2) a pointer to already allocated space is passed.     *)
	       (* Regions occurring in mu_result paired with a string type or occur  *)
	       (* in a type (tau list,rho) in mu_result, are passed by passing a     *)
	       (* pointer to the region.  For other regions we allocate space        *)
	       (* statically and pass a pointer to the allocated space.  Regions     *)
	       (* passed as infinite also have to get the storage mode set for the   *)
	       (* case that the C function calls resetRegion.  See also the chapter  *)
	       (* `Calling C Functions' in the documentation.                        *)
	       let
		 fun comp_region_args_sma [] = []
		   | comp_region_args_sma ((sma, i_opt)::rest) = 
		       (case i_opt of
			  SOME 0 => die "ccExp (CCALL ...): argument region with size 0"
			| SOME i => PASS_PTR_TO_MEM(sma,i) :: (comp_region_args_sma rest)
			| NONE   => PASS_PTR_TO_RHO(sma) :: (comp_region_args_sma rest))
		 val smas_and_ses = List.map (fn (alloc,_) => convert_alloc(alloc,env)) rhos_for_result
		 val i_opts = List.map #2 rhos_for_result

		 val ces_and_ses = List.map (fn tr => ccTrip tr env lab cur_rv) trs
		 val (smas',ces,ses) = unify_smas_ces_and_ses(smas_and_ses,ces_and_ses)
		 val smas = comp_region_args_sma (zip(smas',i_opts))       
		 val maybe_return_unit =
		   (case mu_result of
		      (RType.RECORD [], _) => (fn ce => LET{pat=[fresh_lvar("ccall")],bind=ce,scope=RECORD{elems=[],alloc=IGNORE}})
		    | _ => (fn ce => ce))
		 val fresh_lvs = map (fn _ => fresh_lvar "sma") smas
		 fun maybe_insert_smas([],[],ce) = ce
		   | maybe_insert_smas(fresh_lvs,smas,ce) =
		   LET{pat=fresh_lvs,bind=UB_RECORD smas,scope=ce}
	       in
		 (maybe_return_unit(
		    insert_ses(maybe_insert_smas(fresh_lvs,smas,CCALL{name=name,
								      args=ces,
								      rhos_for_result=map VAR fresh_lvs}),ses)),NONE_SE)
	       end
	   | MulExp.RESET_REGIONS({force,alloc,regions_for_resetting},tr) => 
	       let
		 val regions_for_resetting = List.filter (fn alloc => 
							  case alloc of
							    AtInf.IGNORE => false | _ => true) regions_for_resetting
		 val smas_and_se_smas = List.map (fn alloc => convert_alloc(alloc,env)) regions_for_resetting 
		 val (smas,se_smas,_) = unify_sma_se smas_and_se_smas SEMap.empty
	       in
		 (insert_ses(RESET_REGIONS{force=force,
					   regions_for_resetting=smas},se_smas),NONE_SE)
	       end
	   | MulExp.FRAME{declared_lvars, declared_excons} =>
	       let
		 val lvars = List.map #lvar declared_lvars
		 val lvars_and_labels' = 
		   List.map (fn lvar => 
			     (case CE.lookupVar env lvar of
				CE.FIX(lab,SOME(CE.LVAR lv_clos),i) => 
				  let 
				    val lab_sclos = fresh_lab(Lvars.pr_lvar lv_clos ^ "_lab")
				  in
				    (SOME{lvar=lv_clos,label=lab_sclos},{lvar=lvar,acc_type=CE.FIX(lab,SOME(CE.LABEL lab_sclos),i)})
				  end
			      | CE.FIX(lab,NONE,i) => (NONE,{lvar=lvar,acc_type=CE.FIX(lab,NONE,i)})
			      | CE.LVAR lv => 
				  let
				    val lab = fresh_lab(Lvars.pr_lvar lvar ^ "_lab")
				  in
				    (SOME{lvar=lvar,label=lab},{lvar=lvar,acc_type=CE.LABEL lab})
				  end
			      | _ => die "FRAME: lvar not bound to either LVAR or FIX.")) lvars
		 val (lv_and_lab,frame_env_lv) = ListPair.unzip lvars_and_labels'
		 val lvars_and_labels = List.foldr (fn (lv_lab,acc) => 
							  case lv_lab of
							    NONE => acc | SOME lv_lab => lv_lab::acc) [] lv_and_lab
		 val frame_env_lv = 
		   (ClosConvEnv.empty plus_decl_with CE.declareLvar)
		   (map (fn {lvar,acc_type} => (lvar,acc_type)) frame_env_lv)
		 val excons = List.map #1 declared_excons
		 val excons_and_labels = List.map (fn excon => {excon=excon,label=fresh_lab(Excon.pr_excon excon ^ "_lab")}) excons
		 val frame_env =
		   (frame_env_lv plus_decl_with CE.declareExcon)
		   (map (fn {excon,label} => (excon,CE.LABEL label)) excons_and_labels)
		 val _ = set_frame_env frame_env
	       in
		 (List.foldr (fn ({excon,label},acc) =>
			      let
				val (ce,se) = lookup_excon env excon
			      in
				LET{pat=[fresh_lvar("not_used")],bind=insert_se(STORE(ce,label),se),scope=acc}
			      end)
		  (List.foldr (fn ({lvar,label},acc) => LET{pat=[fresh_lvar("not_used")],bind=STORE(VAR lvar,label),scope=acc})
		   (FRAME{declared_lvars=lvars_and_labels,declared_excons=excons_and_labels}) lvars_and_labels)
		  excons_and_labels, NONE_SE)
	       end)
      in
	ccExp e
      end
  in 
    fun clos_conv(l2clos_exp_env,
		  prog as MulExp.PGM{expression = tr,
				     export_datbinds,
				     import_vars,
				     export_vars,
				     export_basis,
				     export_Psi}) = 
      let
	val _ = reset_lvars()
	val _ = reset_labs()
	val _ = reset_top_decls()
	val import_labs = 
	  find_globals_in_env (valOf(!import_vars)) l2clos_exp_env
	  handle _ => die "clos_conv: import_vars not specified."
	val env_datbind = add_datbinds_to_env export_datbinds CE.empty
	val global_env = CE.plus (l2clos_exp_env, env_datbind)
	val _ = set_global_env global_env
	val main_lab = fresh_lab "main"
	val clos_exp = insert_se(ccTrip tr global_env main_lab NONE)
	val _ = add_new_fn(main_lab,CallConv.mk_cc_fn([],NONE,[],[]),clos_exp)
	val export_env = CE.plus (env_datbind, (get_frame_env()))
	val export_labs = find_globals_in_env (export_vars) (get_frame_env()) 
      in
	{main_lab=main_lab,
	 code=get_top_decls(),
	 env=export_env,
	 imports=import_labs,
	 exports=export_labs}
      end
  end

  val empty = ClosConvEnv.empty
  val plus = ClosConvEnv.plus
  val init = ClosConvEnv.initialEnv
  val enrich = ClosConvEnv.enrich
  val match = ClosConvEnv.match
  val restrict = ClosConvEnv.restrict
  val layout_env = ClosConvEnv.layoutEnv

  (******************************)
  (* Perform Closure Conversion *)
  (******************************)
  fun cc(clos_env,
	 prog as MulExp.PGM{expression = tr,
			    export_datbinds,
			    import_vars,
			    export_vars,
			    export_basis,
			    export_Psi}) = 
    let
      val _ = chat "[Closure Conversion..."
      val n_prog = N prog
      val _ = 
	if Flags.is_on "print_normalized_program" then
	  display("\nReport: AFTER NORMALIZATION:", PhysSizeInf.layout_pgm n_prog)
	else
	  ()
      val Fenv = F n_prog
      val all = clos_conv (clos_env, n_prog)
      val _ = 
	if Flags.is_on "print_clos_conv_program" then
	  display("\nReport: AFTER CLOSURE CONVERSION:", layout_clos_prg (#code(all)))
	else
	  ()
      val _ = chat "]\n"
    in
      all
    end
end;



