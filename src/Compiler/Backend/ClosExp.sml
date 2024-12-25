functor ClosExp(structure CallConv: CALL_CONV where type lvar = Lvars.lvar
                structure ClosConvEnv: CLOS_CONV_ENV
                  where type con = Con.con
                  where type place = AtInf.place
                  where type excon = Excon.excon
                  where type lvar = Lvars.lvar
                  where type label = AddressLabels.label
                  where type phsize = PhysSizeInf.phsize
                  where type StringTree = PrettyPrint.StringTree
                structure BI : BACKEND_INFO) : CLOS_EXP =
struct
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  structure RE = MulExp.RegionExp
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

  val region_profiling : unit -> bool = Flags.is_on0 "region_profiling"

  val print_normalized_program_p = Flags.add_bool_entry
      {long="print_normalized_program", short=NONE,
       menu=["Printing of intermediate forms","print normalized expression"],
       item=ref false, neg=false, desc=
       "Print Region Expression after K-normalisation."}

  val print_clos_conv_program_p = Flags.add_bool_entry
      {long="print_clos_conv_program", short=SOME "Pccp",
       menu=["Printing of intermediate forms","print closure converted expression"],
       item=ref false, neg=false, desc=
       "Print Region Expression after closure conversion."}

  fun pp_lvars s lvs =
    let fun loop nil = ()
          | loop (lv::lvs) = (print (Lvars.pr_lvar lv); print ","; loop lvs)
    in print (s ^ " = ["); loop lvs; print "]\n"
    end

  (***********)
  (* ClosExp *)
  (***********)

  datatype foreign_type = CharArray | ForeignPtr | Bool | Int | Int32 | Int64 | Unit

  datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | UNBOXED_HIGH of int
    | BOXED of int

  type binder = place * phsize

  datatype ClosExp =
      VAR             of lvar
    | RVAR            of {rho:place}
    | DROPPED_RVAR    of {rho:place}
    | FETCH           of label
    | STORE           of ClosExp * label
    | INTEGER         of {value: IntInf.int, precision: int}
    | WORD            of {value: IntInf.int, precision: int}
    | STRING          of string
    | REAL            of string
    | F64             of string
    | PASS_PTR_TO_MEM of sma * int
    | PASS_PTR_TO_RHO of {sma:sma}
    | UB_RECORD       of ClosExp list
    | CLOS_RECORD     of {label: label, elems: ClosExp list * ClosExp list * ClosExp list, f64_vars: int, alloc: sma}
    | SCLOS_RECORD    of {elems: ClosExp list * ClosExp list * ClosExp list, f64_vars: int, alloc: sma}
    | RECORD          of {elems: ClosExp list, alloc: sma, tag: word, maybeuntag: bool}
    | BLOCKF64        of {elems: ClosExp list, alloc: sma, tag: word}
    | SCRATCHMEM      of {bytes:int, alloc: sma, tag: word}
    | SELECT          of int * ClosExp
    | FNJMP           of {opr: ClosExp, args: ClosExp list, clos: ClosExp option}
    | FNCALL          of {opr: ClosExp, args: ClosExp list, clos: ClosExp option}
    | JMP             of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list,
                          clos: ClosExp option}
    | FUNCALL         of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list,
                          clos: ClosExp option}
    | LETREGION       of {rhos: binder list, body: ClosExp}
    | LET             of {pat: lvar list, bind: ClosExp, scope: ClosExp}
    | RAISE           of {exp: ClosExp}
    | HANDLE          of ClosExp * ClosExp
    | SWITCH_I        of {switch: IntInf.int Switch, precision: int}
    | SWITCH_W        of {switch: IntInf.int Switch, precision: int}
    | SWITCH_S        of string Switch
    | SWITCH_C        of (con*con_kind) Switch
    | SWITCH_E        of excon Switch
    | CON0            of {con: con, con_kind: con_kind, aux_regions: sma list, alloc: sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: sma, arg: ClosExp}
    | DECON           of {con: con, con_kind: con_kind, con_exp: ClosExp}
    | DEREF           of {exp: ClosExp}
    | REF             of sma * ClosExp
    | ASSIGN          of sma * ClosExp * ClosExp
    | DROP            of {exp: ClosExp}
    | RESET_REGIONS   of {force: bool,
                          regions_for_resetting: sma list}
    | CCALL           of {name: string,
                          args: ClosExp list,
                          rhos_for_result : ClosExp list}
    | CCALL_AUTO      of {name: string,
                          args: (ClosExp * foreign_type) list,
                          res: foreign_type,
                          rhos_for_result : ClosExp list}   (* boxed res implies memory for the result *)
    | EXPORT          of {name: string,
                          clos_lab: label,
                          arg: ClosExp * foreign_type * foreign_type}
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

  fun pr_rho r = PP.flatten1(Effect.layout_effect r)

  local
    open PP

    fun pr_con_kind (ENUM i)    = "enum " ^ Int.toString i
      | pr_con_kind (UNBOXED i) = "unboxed " ^ Int.toString i
      | pr_con_kind (UNBOXED_HIGH i) = "unboxed-high " ^ Int.toString i
      | pr_con_kind (BOXED i)   = "boxed " ^ Int.toString i

    fun pr_phsize(PhysSizeInf.INF)     = "inf"
      | pr_phsize(PhysSizeInf.WORDS i) = Int.toString i

    fun pr_binder(place,phsize) =
      (flatten1(Effect.layout_effect place) ^ ":" ^ pr_phsize phsize)

    fun pr_pp pp = "pp" ^ Int.toString pp

    fun layout_f CharArray = LEAF "CharArray"
      | layout_f Int = LEAF "Int"
      | layout_f Int32 = LEAF "Int32"
      | layout_f Int64 = LEAF "Int64"
      | layout_f Bool = LEAF "Bool"
      | layout_f ForeignPtr = LEAF "ForeignPtr"
      | layout_f Unit = LEAF "Unit"

    fun layout_ce_f layout_ce (ce,f) = HNODE{start="",finish="",childsep=RIGHT":",
                                             children=[layout_ce ce, layout_f f]}

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
      | layout_ce(RVAR {rho})         = Effect.layout_effect rho
      | layout_ce(DROPPED_RVAR {rho}) = LEAF("D" ^ flatten1(Effect.layout_effect rho))
      | layout_ce(FETCH lab)          = LEAF("fetch(" ^ Labels.pr_label lab ^ ")")
      | layout_ce(STORE(ce,lab))      = LEAF("store(" ^ flatten1(layout_ce ce) ^ "," ^ Labels.pr_label lab ^ ")")
      | layout_ce(INTEGER {value,precision}) = LEAF(IntInf.toString value)
      | layout_ce(WORD {value,precision}) = LEAF("0x" ^ IntInf.fmt StringCvt.HEX value)
      | layout_ce(STRING s)           = LEAF("\"" ^ String.toString s ^ "\"")
      | layout_ce(REAL s)             = LEAF(s)
      | layout_ce(F64 s)              = LEAF(s ^ "f64")
      | layout_ce(PASS_PTR_TO_MEM(sma,i)) = LEAF("MEM(" ^ (flatten1(pr_sma sma)) ^ "," ^ Int.toString i ^ ")")
      | layout_ce(PASS_PTR_TO_RHO {sma}) = LEAF("PTR(" ^ (flatten1(pr_sma sma)) ^ ")")
      | layout_ce(UB_RECORD ces)      = HNODE{start="<",
                                              finish=">",
                                              childsep=RIGHT ",",
                                              children=map layout_ce ces}
      | layout_ce(CLOS_RECORD{label,elems=(lvs,excons,rhos),f64_vars,alloc}) =
        HNODE{start="[",
              finish="]clos(" ^ Int.toString f64_vars ^ ") " ^ (flatten1(pr_sma alloc)),
              childsep=RIGHT ",",
              children=LEAF(Labels.pr_label label)::
                       map layout_ce (rhos@excons@lvs)}
      | layout_ce(SCLOS_RECORD{elems=(lvs,excons,rhos),f64_vars,alloc}) =
        HNODE{start="[",
              finish="]sclos(" ^ Int.toString f64_vars ^ ") " ^ (flatten1(pr_sma alloc)),
              childsep=RIGHT ",",
              children= map layout_ce (rhos@excons@lvs)}
      | layout_ce(RECORD{elems,alloc,tag,maybeuntag}) = HNODE{start="(",
                                                              finish=") " ^ (flatten1(pr_sma alloc)),
                                                              childsep=RIGHT ",",
                                                              children= map layout_ce elems}
      | layout_ce(BLOCKF64{elems,alloc,tag}) = HNODE{start="{",
                                                     finish="} " ^ (flatten1(pr_sma alloc)),
                                                     childsep=RIGHT ",",
                                                     children= map layout_ce elems}
      | layout_ce(SCRATCHMEM{bytes,alloc,tag}) = LEAF("scratch(" ^ Int.toString bytes ^
                                                      flatten1(pr_sma alloc))
      | layout_ce(SELECT(i,ce)) = HNODE{start="#" ^ Int.toString i ^ "(",
                                        finish=")",
                                        childsep=NOSEP,
                                        children=[layout_ce ce]}
      | layout_ce(FNJMP{opr,args,clos}) =
          let
            val t1 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce args}
            val t2 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt clos]}
          in
            NODE{start=flatten1(layout_ce opr) ^ "_fnjmp ",
                 finish="", childsep=RIGHT " ",
                 indent=3,
                 children=[t1,t2]}
          end
      | layout_ce(FNCALL{opr,args,clos}) =
          let
            val t1 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce args}
            val t2 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt clos]}
          in
            NODE{start=flatten1(layout_ce opr) ^ "_fncall ",
                 finish="", childsep=RIGHT " ",
                 indent=3,
                 children=[t1,t2]}
          end
      | layout_ce(JMP{opr,args,reg_vec,reg_args,clos}) =
          let
            val t1 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce args}
            val t2 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt reg_vec]}
            val t3 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce reg_args}
            val t4 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt clos]}
          in
            NODE{start=Labels.pr_label opr ^ "_funjmp",
                 finish="", childsep=RIGHT " ",
                 indent=3,
                 children=[t1,t2,t3,t4]}
          end
      | layout_ce(FUNCALL{opr,args,reg_vec,reg_args,clos}) =
          let
            val t1 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce args}
            val t2 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt reg_vec]}
            val t3 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=map layout_ce reg_args}
            val t4 = NODE{start="<",finish=">",indent=3,childsep=RIGHT ",",children=[layout_ce_opt clos]}
          in
            NODE{start=Labels.pr_label opr ^ "_funcall",
                 finish="", childsep=RIGHT " ",
                 indent=3,
                 children=[t1,t2,t3,t4]}
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
      | layout_ce(RAISE {exp=ce}) = PP.LEAF("raise " ^ (flatten1(layout_ce ce)))
      | layout_ce(HANDLE(ce1,ce2)) = NODE{start="",finish="",childsep=RIGHT " handle ",indent=1,
                                          children=[layout_ce ce1,layout_ce ce2]}
      | layout_ce(SWITCH_I {switch,precision}) = layout_switch layout_ce (IntInf.toString) switch
      | layout_ce(SWITCH_W {switch,precision}) =
        layout_switch layout_ce (fn w => "0x" ^ IntInf.fmt StringCvt.HEX w) switch
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
      | layout_ce(DEREF {exp=ce}) = LEAF("!" ^ (flatten1(layout_ce ce)))
      | layout_ce(REF(sma,ce)) = LEAF("ref " ^ (flatten1(layout_ce ce)) ^ " " ^ (flatten1(pr_sma sma)))
      | layout_ce(ASSIGN(sma,ce1,ce2)) = HNODE{start="",
                                               finish="",
                                               childsep=RIGHT ":=",
                                               children=[layout_ce ce1,layout_ce ce2]}
      | layout_ce(DROP {exp=ce}) = layout_ce ce
(*
          PP.LEAF("drop " ^ (flatten1(layout_ce ce)))
*)
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
                children=(map layout_ce rhos_for_result) @ (map layout_ce args)}
      | layout_ce(CCALL_AUTO{name,args,res,rhos_for_result}) =
          HNODE{start="ccall_auto(\"" ^ name ^ "\", <",
                finish=">)",
                childsep=RIGHT ",",
                children=(map layout_ce rhos_for_result) @ (map (layout_ce_f layout_ce) args) @ [layout_f res]}
      | layout_ce(EXPORT{name,clos_lab,arg=(ce,ft1,ft2)}) =
          HNODE{start="_export(\"" ^ name ^ "\", <",
                finish=">)",
                childsep=RIGHT ",",
                children=[layout_ce ce, layout_f ft1, layout_f ft2]}
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

    fun layout_top_decl' f_string (lab,cc,ce) =
      NODE{start = f_string ^ Labels.pr_label lab ^ "{" ^ CallConv.pr_cc cc ^ "}=",
           finish = "",
           indent = 2,
           childsep = NOSEP,
           children = [layout_ce ce]}
  in
    fun layout_top_decl (FUN triple) = layout_top_decl' "FUN" triple
      | layout_top_decl (FN triple) = layout_top_decl' "FN" triple

    fun layout_clos_exp ce = layout_ce ce
    fun layout_clos_prg top_decls =
      let val _ = print "\nlayout begin..."
          val r = NODE{start="ClosExp program begin",
                       finish="ClosExp program end",
                       indent=2,
                       childsep=NOSEP,
                       children = map layout_top_decl top_decls}
          val _ = print "\nlayout end"
      in r
      end
    fun pp_sma sma = PP.flatten1(pr_sma sma)
  end

  local
    fun pr_seq [] pp = ""
      | pr_seq [e] pp = pp e
      | pr_seq (e::rest) pp = pp e ^ ", " ^ (pr_seq rest pp)
    fun pp_regvar rho =  PP.flatten1(Effect.layout_effect rho)
  in
    fun pr_rhos rhos = pr_seq rhos pp_regvar
    fun pr_lvars lvars = pr_seq lvars Lvars.pr_lvar
    fun pr_excons excons = pr_seq excons Excon.pr_excon
    fun pr_free (lvars, excons, rhos) =
      "(["^(pr_lvars lvars)^"],["^(pr_excons excons)^"],["^(pr_rhos rhos)^"])"
  end

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
            in fun lvar_as_term (x,mu) =
                   TR(VAR{lvar=x,il=il0,plain_arreffs=[],
                          fix_bound=false,rhos_actuals=ref [],other=dummy_'c},mu,[],ref Mul.empty_psi)
               fun mk_pat (lvar,mu) =
                   case mu of
                       RE.Mus[mu] =>
                       let val (ty,place) = RType.unbox mu
                       in [(lvar,ref([]:RType.il ref list),[],ref([]:effect list),ty,place,dummy_'c)]
                       end
                     | _ => die "mk_pat: metatype not (tau,rho)"
            end

            fun Nsw (tr, choices, opt) =
                MulExp.SWITCH(NTrip tr true,
                              map (fn(match,tr) => (match,NTrip tr true)) choices,
                              (case opt of
                                   SOME tr => SOME (NTrip tr true)
                                 | NONE => NONE))

            fun NExp e =
              (case e of
                 MulExp.VAR _ => e
               | MulExp.INTEGER(i,t,alloc) => e
               | MulExp.WORD(i,t,alloc) => e
               | MulExp.STRING(s,alloc) => e
               | MulExp.REAL(r,alloc) => e
               | MulExp.F64 r => e
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
               | MulExp.SWITCH_I {switch=MulExp.SWITCH(tr,choices,opt), precision} =>
                   MulExp.SWITCH_I {switch=Nsw(tr, choices, opt), precision=precision}
               | MulExp.SWITCH_W {switch=MulExp.SWITCH(tr,choices,opt), precision} =>
                   MulExp.SWITCH_W {switch=Nsw(tr, choices, opt), precision=precision}
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
               | MulExp.BLOCKF64(alloc, trs) => MulExp.BLOCKF64(alloc, map (fn tr => NTrip tr true) trs)
               | MulExp.SCRATCHMEM(n,alloc) => MulExp.SCRATCHMEM(n,alloc)
               | MulExp.SELECT(i,tr) => MulExp.SELECT(i,NTrip tr true)
               | MulExp.DEREF tr => MulExp.DEREF (NTrip tr true)
               | MulExp.REF(a,tr) => MulExp.REF(a,NTrip tr true)
               | MulExp.ASSIGN(tr1,tr2) => MulExp.ASSIGN(NTrip tr1 true,NTrip tr2 true)
               | MulExp.DROP tr => MulExp.DROP (NTrip tr true)
               | MulExp.EQUAL({mu_of_arg1,mu_of_arg2},tr1, tr2) =>
                   MulExp.EQUAL({mu_of_arg1=mu_of_arg1,mu_of_arg2=mu_of_arg2},
                                NTrip tr1 true,
                                NTrip tr2 true)
               | MulExp.CCALL({name, mu_result, rhos_for_result}, trs) =>
                   MulExp.CCALL({name=name,mu_result=mu_result,rhos_for_result=rhos_for_result},
                                map (fn tr => NTrip tr true) trs)
               | MulExp.EXPORT(i, tr) => MulExp.EXPORT(i, NTrip tr true)
               | MulExp.RESET_REGIONS({force, regions_for_resetting,liveset},tr) =>
                   MulExp.RESET_REGIONS({force=force,regions_for_resetting=regions_for_resetting,liveset=liveset},
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
    type args = lvar list

    datatype fenv =
      FN of args * free
    | FIX of args * free

    structure FuncEnv = Lvars.Map
(*
      OrderFinMap(struct
                      type T = lvar
                      fun lt(l1: T) l2 = Lvars.lt(l1,l2)
                  end)
*)
    local
      fun pp_dom lvar = PP.LEAF (Lvars.pr_lvar lvar)
      fun pp_ran (FN(args,free)) =
        PP.LEAF ("FN[Args:"^(pr_lvars args)^
                 ",Free:"^(pr_free free)^"]")
        | pp_ran (FIX(args,free)) =
        PP.LEAF ("FIX[Args:"^(pr_lvars args)^
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

    val empty_Fenv = FuncEnv.empty

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
        OrderSet(struct
                     type t = lvar
                     fun lt(l1: t, l2) = Lvars.lt(l1,l2)
                 end)

    structure EnvExCon =
        OrderSet(struct
                     type t = excon
                     fun lt(e1: t, e2) = Excon.< (e1,e2)
                 end)

    structure EnvRho =
        OrderSet(struct
                     type t = place
                     val lt = Effect.lt_eps_or_rho
                 end)

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
      FUNC of (args * free)
    | OTHER

  in
    fun F(prog as MulExp.PGM{expression = tr,
                             export_datbinds,
                             import_vars,
                             export_vars,
                             export_basis,
                             export_Psi}) =
      if false (*Flags.is_on "prune_closures"*) then
        let
          val export_vars_set = EnvLvar.fromList (#1 export_vars)
          val import_vars =
            case import_vars
              of ref (SOME vars) => vars
            | _ => die "ClosExp.F.no import vars info"

          fun FTrip ((MulExp.TR(e,metaType,ateffects,mulef))) Fenv Env =
            let
              fun FExp e Fenv (Env as (EnvLvar, EnvExCon, EnvRho)) =
                (case e of
                   MulExp.VAR{lvar,...} =>
                     if is_in_dom_Fenv Fenv lvar then
                       (rem_Fenv Fenv lvar, [OTHER])
                     else
                       (Fenv, [OTHER])
                 | MulExp.INTEGER(i,t,alloc) => (Fenv, [OTHER])
                 | MulExp.WORD(i,t,alloc) => (Fenv, [OTHER])
                 | MulExp.STRING(s,alloc) => (Fenv, [OTHER])
                 | MulExp.REAL(r,alloc) => (Fenv, [OTHER])
                 | MulExp.F64 r => (Fenv, [OTHER])
                 | MulExp.UB_RECORD trs =>
                       List.foldr (fn (tr,(Fenv',types')) =>
                                   (case FTrip tr Fenv' Env
                                      of (Fenv_t, [t]) => (Fenv_t,t::types')
                                    | _ => die "UB_RECORD")) (Fenv, []) trs
                 | MulExp.FN{pat,body,free,alloc} =>
                       let
                         val free_vars =
                           case (!free) of
                             SOME free => free
                           | NONE => ([], [], [])
                         val args = map #1 pat
                         val (Fenv',_) = FTrip body Fenv (add_Env (fresh_Env free_vars import_vars) (args,[],[]))
                       in
                         (Fenv', [FUNC(args,free_vars)])
                       end
                 | MulExp.LETREGION{B, rhos, body} =>
                       FTrip body Fenv (add_Env Env ([],[],List.map #1 (!rhos)))
                 | MulExp.LET{k_let,pat,bind,scope} =>
                       let
                         val lvars = List.map #1 pat
                         val (Fenv', types) = FTrip bind Fenv Env
                         val types' = zip (lvars,types)
                         val Fenv_scope = List.foldl (fn ((lvar',type'),base) =>
                                                      case type' of
                                                        FUNC(args,free) => add_Fenv base lvar' (FN(args, free))
                                                      | OTHER => base) Fenv' types'
                       in
                         FTrip scope Fenv_scope (add_Env Env (lvars,[],[]))
                       end
                 | MulExp.FIX{free,shared_clos,functions,scope} =>
                       let
                         (* funcs : (lvar, args, formals, free, body) list *)
                         fun f {lvar, occ, tyvars, rhos, epss, Type, rhos_formals: (place*phsize) list ref, other,
                                bound_but_never_written_into,
                                bind = MulExp.TR(MulExp.FN{pat,body,free,alloc},_,_,_)} =
                           (case (!free)
                              of NONE => (lvar, List.map #1 pat, List.map #1 (!rhos_formals), ([],[],[]), body)
                            | SOME free => (lvar, List.map #1 pat, List.map #1 (!rhos_formals), free, body))
                           | f _ = die "Functions not in expected shape."
                         val funcs = List.map f functions

                         val Fenv1 = List.foldl (fn ((lvar,args,_,free,_),base) =>
                                                 add_Fenv base lvar (FIX(args, free))) Fenv funcs

                         val FenvN = List.foldl (fn ((_,args,rhos_formals,free,body),base) =>
                                                 #1(FTrip body base
                                                    (add_Env (fresh_Env free import_vars)
                                                     (args,[],rhos_formals)))) Fenv1 funcs

                         val all_exists = List.foldl (fn ((lvar,_,_,_,_),base) =>
                                                      if is_in_dom_Fenv FenvN lvar andalso
                                                        not (EnvLvar.member lvar export_vars_set) andalso (* none of the letrec may be exported *)
                                                        base = true then true else false) true funcs

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
                          of MulExp.TR(MulExp.VAR{lvar,il, plain_arreffs,fix_bound=false,rhos_actuals,other},_,_,_) =>
                            (* Ordinary function call *)
                            let
                                   val Fenv' = (case lookup_Fenv Fenv lvar
                                                  of NONE => Fenv
                                                | SOME (FN(arg_fn,free_fn)) =>
                                                    if free_in_Env Env free_fn then
                                                      Fenv
                                                    else
                                                      rem_Fenv Fenv lvar
                                                | SOME (FIX(lvars,free)) => die "Function should be FN but is recorded as FIX")
                                   val (Fenv_res, _) = FTrip operand Fenv' Env
                            in
                              (Fenv_res, [OTHER])
                            end
                           | MulExp.TR(MulExp.VAR{lvar,il, plain_arreffs,fix_bound=true,rhos_actuals,other},_,_,_) =>
                               (* Region Polymorphic call *)
                               let
                                 val Fenv' = (case lookup_Fenv Fenv lvar
                                                of NONE => Fenv
                                              | SOME (FIX(args_fix,free_fix)) =>
                                                  if free_in_Env Env free_fix then
                                                    Fenv
                                                  else
                                                    rem_Fenv Fenv lvar
                                              | SOME (FN(lvars,free)) => die "Function should be a FIX but is recorded as FN")
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
                 | MulExp.SWITCH_I {switch=MulExp.SWITCH(tr,choices,opt), precision} =>
                          let
                            val (Fenv_tr,_) = FTrip tr Fenv Env
                            val Fenv_ch = List.foldl (fn ((_,tr),base) => #1(FTrip tr base Env)) Fenv_tr choices
                            val (Fenv_res) = (case opt of SOME tr => #1(FTrip tr Fenv_ch Env) | NONE => Fenv_ch)
                          in
                            (Fenv_res,[OTHER])
                          end
                 | MulExp.SWITCH_W {switch=MulExp.SWITCH(tr,choices,opt), precision} =>
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
                   let val Fenv_res = List.foldl (fn (tr,base) => #1(FTrip tr base Env)) Fenv trs
                   in (Fenv_res, [OTHER])
                   end
                 | MulExp.BLOCKF64(alloc, trs) =>
                   let val Fenv_res = List.foldl (fn (tr,base) => #1(FTrip tr base Env)) Fenv trs
                   in (Fenv_res, [OTHER])
                   end
                 | MulExp.SCRATCHMEM(n,alloc) => (Fenv, [OTHER])
                 | MulExp.SELECT(i,tr) => FTrip tr Fenv Env
                 | MulExp.DEREF tr => FTrip tr Fenv Env
                 | MulExp.REF(a,tr) => FTrip tr Fenv Env
                 | MulExp.ASSIGN(tr1,tr2) =>
                   let
                     val (Fenv1,_) = FTrip tr1 Fenv Env
                     val (Fenv2,_) = FTrip tr2 Fenv1 Env
                   in
                     (Fenv2, [OTHER])
                   end
                 | MulExp.DROP tr => FTrip tr Fenv Env
                 | MulExp.EQUAL({mu_of_arg1,mu_of_arg2},tr1, tr2) =>
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
                 | MulExp.EXPORT(i,tr) => FTrip tr Fenv Env
                 | MulExp.RESET_REGIONS({force, regions_for_resetting, ...},tr) => FTrip tr Fenv Env
                 | MulExp.FRAME{declared_lvars, declared_excons} =>
                   (List.foldl (fn ({lvar,...},base) => if is_in_dom_Fenv base lvar then
                                rem_Fenv base lvar
                                                        else
                                                          base) Fenv declared_lvars, [OTHER]))
            in
              FExp e Fenv Env
            end

          val (Fenv', _) = FTrip tr FuncEnv.empty (fresh_Env import_vars ([],[],[]))

          (* Remove all export_vars from Fenv'. Currently, they are closure implemented. *)
          val Fenv_res = List.foldl (fn (lvar,base) =>
                                     if is_in_dom_Fenv base lvar then
                                       rem_Fenv base lvar
                                     else
                                       base) Fenv' (#1(export_vars))

          (*val _ = pr_st (pp_Fenv Fenv_res) 29/08-2000, Niels *)
        in
          Fenv_res
        end
      else
        empty_Fenv
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

    fun get_pp(ATTOP_LI (ce,pp)) = pp
      | get_pp(ATTOP_LF (ce,pp)) = pp
      | get_pp(ATTOP_FI (ce,pp)) = pp
      | get_pp(ATTOP_FF (ce,pp)) = pp
      | get_pp(ATBOT_LI (ce,pp)) = pp
      | get_pp(ATBOT_LF (ce,pp)) = pp
      | get_pp(  SAT_FI (ce,pp)) = pp
      | get_pp(  SAT_FF (ce,pp)) = pp
      | get_pp(IGNORE) = die "get_pp: combination not recognized."

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

    fun pr_rho_kind (CE.FF) = "FF"
      | pr_rho_kind (CE.FI) = "FI"
      | pr_rho_kind (CE.LF) = "LF"
      | pr_rho_kind (CE.LI) = "LI"

    fun pr_at (AtInf.ATTOP _) = "Attop"
      | pr_at (AtInf.ATBOT _) = "Atbot"
      | pr_at (AtInf.SAT _) = "Sat"

    fun convert_sma (AtInf.ATTOP(rho,pp),CE.LI,ce) = ATTOP_LI (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.LF,ce) = ATTOP_LF (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.FI,ce) = ATTOP_FI (ce,pp)
      | convert_sma (AtInf.ATTOP(rho,pp),CE.FF,ce) = ATTOP_FF (ce,pp)
      | convert_sma (AtInf.ATBOT(rho,pp),CE.LI,ce) = ATBOT_LI (ce,pp)
      | convert_sma (AtInf.ATBOT(rho,pp),CE.LF,ce) = ATBOT_LF (ce,pp)
      | convert_sma (AtInf.SAT(rho,pp),CE.FI,ce) = SAT_FI (ce,pp)
      | convert_sma (AtInf.SAT(rho,pp),CE.FF,ce) = SAT_FF (ce,pp)
      | convert_sma (at,rk,ce) = die ("convert_sma: sma combination not recognized." ^
                                      "at=" ^ pr_at at ^ ", rk=" ^ pr_rho_kind rk ^ "\n")

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
        OrderFinMap(struct
                        type t = select_exp
                        val lt = lt_se
                    end)

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

    fun unify_smas_ces_and_ses_free (smas_and_ses,(lvs_ces_ses,excons_ces_ses,rhos_ces_ses)) =
      let
        val (smas,ses_smas,se_map) = unify_sma_se smas_and_ses SEMap.empty
        val (rhos_ces,rhos_ses,se_map) = unify_ce_se rhos_ces_ses se_map
        val (excons_ces,excons_ses,se_map) = unify_ce_se excons_ces_ses se_map
        val (lvs_ces,lvs_ses,_) = unify_ce_se lvs_ces_ses se_map
      in
        (smas,(lvs_ces,excons_ces,rhos_ces),ses_smas@rhos_ses@excons_ses@lvs_ses)
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

    fun lookup_fix_profiling env lv =
      case CE.lookupVarOpt env lv
        of SOME (CE.FIX(_,_,_,formals)) => formals
         | _ => die "lookup_fix_profiling"

    fun lookup_ve env lv =
      let
        fun resolve_se {f64} (CE.LVAR lv') =
            let val () = if f64 <> Lvars.get_ubf64 lv' then die "f64 mismatch" else ()
            in (VAR lv',NONE_SE)
            end
          | resolve_se {f64} (CE.SELECT(lv',i)) =
              let
                val lv'' = fresh_lvar ("lookup_ve")
                val () = if f64 then Lvars.set_ubf64 lv'' else ()
              in
                (VAR lv'',SELECT_SE(lv'',i,lv'))
              end
          | resolve_se {f64} (CE.LABEL lab) =
              let
                val lv' = fresh_lvar("lookup_ve")
                val () = if f64 then Lvars.set_ubf64 lv' else ()
              in
                (VAR lv',FETCH_SE (lv',lab))
              end
          | resolve_se _ _ = die "resolve_se: wrong FIX or RVAR binding in VE"
      in
        case CE.lookupVarOpt env lv of
            SOME (CE.FIX(_,SOME a,_,_)) => resolve_se {f64=false} a
          | SOME (CE.FIX(_,NONE,_,_)) => die "lookup_ve: this case should be caught in APP."
          | SOME a => resolve_se {f64=Lvars.get_ubf64 lv} a
          | NONE  => die ("lookup_ve: lvar(" ^ (Lvars.pr_lvar lv) ^ ") not bound in env.")
      end

    fun lookup_fun env lv =
        case CE.lookupVarOpt env lv of
            SOME(CE.FIX(lab,ce,size,_)) => (lab,size)
          | _ => die ("lookup_fun: function(" ^ Lvars.pr_lvar lv ^ ") does not exists")

    fun lookup_excon env excon =
        case CE.lookupExconOpt env excon of
            SOME(CE.LVAR lv') => (VAR lv',NONE_SE)
          | SOME(CE.SELECT(lv',i)) =>
            let val lv'' = fresh_lvar("lookup_excon")
            in (VAR lv'',SELECT_SE(lv'',i,lv'))
            end
          | SOME(CE.LABEL lab) =>
            let val lv' = fresh_lvar("lookup_excon")
            in (VAR lv',FETCH_SE (lv',lab))
            end
          | SOME _ => die "lookup_excon: excon bound to FIX or RVAR"
          | NONE  => die ("lookup_excon: excon(" ^ (Excon.pr_excon excon) ^ ") not bound")

    fun lookup_rho env place (f : unit -> string) =
        case CE.lookupRhoOpt env place of
            SOME(CE.LVAR lv') => (VAR lv',NONE_SE)
          | SOME(CE.RVAR place) => (RVAR {rho=place}, NONE_SE)
          | SOME(CE.DROPPED_RVAR place) => (DROPPED_RVAR {rho=place}, NONE_SE)
          | SOME(CE.SELECT(lv',i)) =>
            let val lv'' = fresh_lvar("lookup_rho")
            in (VAR lv'',SELECT_SE(lv'',i,lv'))
            end
          | SOME(CE.LABEL lab) =>
            let val lv' = fresh_lvar("lookup_rho")
            in (VAR lv',FETCH_SE (lv',lab))
            end
          | SOME _ => die ("lookup_rho: rho bound to FIX. " ^ f())
          | NONE  => die ("lookup_rho: rho(" ^ PP.flatten1(Effect.layout_effect place) ^ ") not bound. " ^ f())

    fun convert_alloc (alloc,env) =
        case alloc of
            AtInf.ATBOT(rho,pp) =>
            let val (ce,se) = lookup_rho env rho (fn () => "convert_alloc1")
            in (convert_sma(AtInf.ATBOT(rho,pp),CE.lookupRhoKind env rho,ce),se)
            end
          | AtInf.SAT(rho,pp) =>
            let val (ce,se) = lookup_rho env rho (fn () => "convert_alloc2")
            in (convert_sma(AtInf.SAT(rho,pp),CE.lookupRhoKind env rho,ce),se)
            end
          | AtInf.ATTOP(rho,pp) =>
            let val (ce,se) = lookup_rho env rho (fn () => "convert_alloc3")
            in (convert_sma(AtInf.ATTOP(rho,pp),CE.lookupRhoKind env rho,ce),se)
            end

    fun mult ("f",PhysSizeInf.INF) = CE.FI
      | mult ("f",PhysSizeInf.WORDS n) = CE.FF
      | mult ("l",PhysSizeInf.INF) = CE.LI
      | mult ("l",PhysSizeInf.WORDS n) = CE.LF
      | mult _ = die "mult: Wrong binding or phsize"

    fun lookup_con env con =
        case CE.lookupCon env con of
            CE.ENUM i    => ENUM i
          | CE.UB_NULLARY i => UNBOXED i
          | CE.UB_UNARY i => UNBOXED i
          | CE.UBH_NULLARY i => UNBOXED_HIGH i
          | CE.UBH_UNARY i => UNBOXED_HIGH i
          | CE.B_NULLARY i => BOXED i
          | CE.B_UNARY i => BOXED i

    (*------------------------------------------------------------------*)
    (* Analyse the datatype bindings and return an environment mapping  *)
    (* all constructors to representation information. Unary            *)
    (* constructors are enumerated from zero to the number of unary     *)
    (* constructors in the datatype. Similarly for nullary constructors.*)
    (* Note, that booleans and lists are allready defined,              *)
    (* see ClosConvEnv.sml, and lists are the only unboxed datatypes    *)
    (* that we have.                                                    *)
    (*------------------------------------------------------------------*)
    fun add_datbinds_to_env (RE.DATBINDS dbs) l2clos_exp_env : CE.env =
      let
        fun tags_split (N,U) (n0,n1) binds =
            case binds of
                nil => nil
              | ((c,RE.CONSTANT,_)::rest) =>
                (c,N n0) :: tags_split (N,U) (n0+1,n1) rest
              | ((c,RE.VALUE_CARRYING,_)::rest) =>
                (c,U n1) :: tags_split (N,U) (n0,n1+1) rest

        fun tags_same (N,U) n binds =
            case binds of
                nil => nil
              | ((c,RE.CONSTANT,_)::rest) =>
                (c,N n) :: tags_same (N,U) (n+1) rest
              | ((c,RE.VALUE_CARRYING,_)::rest) =>
                (c,U n) :: tags_same (N,U) (n+1) rest

        fun analyse_datbind (tn,binds: (con * RE.constructorKind * 'a) list) : (con * CE.con_kind) list =
            case TyName.boxity tn of
                TyName.ENUM => tags_same (CE.ENUM, fn _ => die "analyse_datbind.enum") 0 binds
              | TyName.UNB_LOW => tags_split (CE.UB_NULLARY, CE.UB_UNARY) (0,0) binds
              | TyName.UNB_ALL => tags_same (CE.UBH_NULLARY, CE.UBH_UNARY) 0 binds
              | TyName.BOXED => tags_split (CE.B_NULLARY, CE.B_UNARY) (0,0) binds
              | TyName.SINGLE _ => tags_same (fn _ => die "analyse_datbind.single", CE.UB_UNARY) 0 binds
    in
      List.foldl (fn (datbind,env) =>
                     (env plus_decl_with CE.declareCon) (analyse_datbind datbind))
                 l2clos_exp_env (concat_lists dbs)
    end

    local
      fun member tn nil = false
        | member tn (x::xs) = TyName.eq (tn,x) orelse member tn xs
    in
      fun tn_to_foreign_type (tn : TyName.TyName) : foreign_type =
          if TyName.eq(tn,TyName.tyName_BOOL) then Bool
          else if TyName.eq(tn,TyName.tyName_FOREIGNPTR) then ForeignPtr
          else if member tn [TyName.tyName_STRING, TyName.tyName_CHARARRAY] then CharArray
          else if member tn [TyName.tyName_IntDefault(), TyName.tyName_WordDefault(), TyName.tyName_CHAR, TyName.tyName_WORD8] then Int
          else if member tn [TyName.tyName_INT64, TyName.tyName_WORD64] then Int64
          else if member tn [TyName.tyName_INT32, TyName.tyName_WORD32] then Int32
          else die ("tn_to_foreign_type.Type name " ^ TyName.pr_TyName tn
                    ^ " not supported in auto conversion")
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

    (* ------------------------------------------------------ *)
    (*    Add EXPORT labels (for exporting ML functions to C) *)
    (* ------------------------------------------------------ *)
    local
      val exports : label list ref = ref []
    in
      fun reset_exports() = exports := []
      fun add_new_export(lab)  = exports := lab :: (!exports)
      fun get_exports() = !exports
    end

    fun precisionNumType t =
      case RType.unCONSTYPE t of
        SOME(tn,_,_,_) =>
          if TyName.eq(tn, TyName.tyName_INT31) then 31
          else if TyName.eq(tn, TyName.tyName_INT32) then 32
          else if TyName.eq(tn, TyName.tyName_INT63) then 63
          else if TyName.eq(tn, TyName.tyName_INT64) then 64
          else if TyName.eq(tn, TyName.tyName_WORD8) then 8
          else if TyName.eq(tn, TyName.tyName_WORD31) then 31
          else if TyName.eq(tn, TyName.tyName_WORD32) then 32
          else if TyName.eq(tn, TyName.tyName_WORD63) then 63
          else if TyName.eq(tn, TyName.tyName_WORD64) then 64
          else if TyName.eq(tn, TyName.tyName_CHAR) then 8
          else die "precisionNumType.wrong tyname"
      | NONE => die "precisionNumType.wrong type"

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
                  CE.FIX(lab,NONE,0,_) => remove lvs
                | CE.FIX(lab,NONE,i,_) => die "remove_zero_sized_region_closure_lvars: FIX messed up"
                | CE.FIX(lab,SOME _,0,_) => die "remove_zero_sized_region_closure_lvars: FIX messed up"
                | _ => lv :: remove lvs)
      in
        (remove lvs,rhos,excons)
      end

    (* Determine the free variables for an ordinary or shared closure.   *)
    (* org_env is the lexical environment in which this function is      *)
    (* declared --- used to look up the region sizes for the free        *)
    (* variables bound to letrec functions. new_env is used as base      *)
    (* when building the new environment.                                *)
    (* Region variables are FIRST in the closure, then comes unboxed     *)
    (* f64 lambda variables; necessary for tagging.                      *)

    (* Notice that lvars appears with the free lvars of type f64 first
     * in the list (file get_fvs() in PhysSizeInf) *)

    fun build_clos_env org_env new_env lv_clos base_offset (free_lv,free_excon,free_rho) =
      let
        (* When computing offsets we do not increase the offset counter when meeting *)
        (* a lambda variable bound to a zero sized shared region closure, since code *)
        (* is not constructed to put such region closures into the actual closure.   *)
        fun add_free_lv (lv,(env,i)) =
          (case CE.lookupVar org_env lv of
             CE.FIX(lab,NONE,0,formals)   => (CE.declareLvar(lv,CE.FIX(lab,NONE,0,formals),env),i)
           | CE.FIX(lab,NONE,s,formals)   => die "add_free_lv: CE.FIX messed up."
           | CE.FIX(lab,SOME _,0,formals) => die "add_free_lv: CE.FIX messed up."
           | CE.FIX(lab,SOME _,s,formals) => (CE.declareLvar(lv,CE.FIX(lab,SOME(CE.SELECT(lv_clos,i)),s,formals),env),i+1)
           | _ => (CE.declareLvar(lv,CE.SELECT(lv_clos,i),env),i+1))
        fun add_free_excon (excon,(env,i)) =
          (CE.declareExcon(excon,(CE.SELECT(lv_clos,i),
                                  CE.lookupExconArity org_env excon),env),i+1)
        fun add_free_rho (place,(env,i)) =
          (CE.declareRhoKind(place,CE.lookupRhoKind org_env place,
                             CE.declareRho(place,CE.SELECT(lv_clos,i),env)),i+1)
        val (env',_)  =
(*
          List.foldl add_free_lv
          (List.foldl add_free_excon
           (List.foldl add_free_rho (new_env, base_offset) free_rho) free_excon) free_lv
*)
          List.foldl add_free_excon
          (List.foldl add_free_lv
           (List.foldl add_free_rho (new_env, base_offset) free_rho) free_lv) free_excon
      in
        env'
      end

    (* Returns (ces,ses) corresponding to accessing the free variables *)
    fun gen_ces_and_ses_free env (free_lv,free_excon,free_rho) =
      let
        val lvs_and_ses = List.map (fn lv => lookup_ve env lv) free_lv
        val excons_and_ses = List.map (fn excon => lookup_excon env excon) free_excon
        val rhos_and_ses = List.map (fn place => lookup_rho env place (fn () => "gen_ces_and_ses_free")) free_rho
      in
        (lvs_and_ses,excons_and_ses,rhos_and_ses)
      end

    (* drop_rho rho: replace rho by a global region with the same runtime type; *)
    (* used when rho is letrec-bound, but never written into.                    *)
    fun drop_rho rho =
      (case Effect.get_place_ty rho of
         SOME Effect.STRING_RT => Effect.toplevel_region_withtype_string
       | SOME Effect.PAIR_RT   => Effect.toplevel_region_withtype_pair
       | SOME Effect.ARRAY_RT  => Effect.toplevel_region_withtype_array
       | SOME Effect.REF_RT    => Effect.toplevel_region_withtype_ref
       | SOME Effect.TRIPLE_RT => Effect.toplevel_region_withtype_triple
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

    local
      fun labs (fun_lab: label list, dat_lab: label list) (r:CE.access_type) : label list * label list =
        case r
          of CE.FIX(lab,SOME(CE.LABEL sclos_lab),_,_) => (lab::fun_lab,sclos_lab::dat_lab) (* lab is a function and sclos is a data object. *)
           | CE.FIX(lab,NONE,_,_) => (lab::fun_lab,dat_lab) (* lab is a function with empty shared closure. *)
           | CE.LABEL lab => (fun_lab,lab::dat_lab) (* Is a DatLab *)
           | CE.FIX(lab,SOME(CE.LVAR lvar),_,_) => die "find_globals_in_env: FIX with SCLOS bound to lvar."
           | CE.FIX(lab,SOME(CE.SELECT(lvar,i)),_,_)  => die "find_globals_in_env: FIX with SCLOS bound to SELECT."
           | CE.FIX(lab,_,_,_) => die "find_globals_in_env: global bound to wierd FIX."
           | CE.LVAR _ => die "find_globals_in_env: global bound to lvar."
           | CE.RVAR _ => die "find_globals_in_env: global bound to rvar."
           | CE.DROPPED_RVAR _ => die "find_globals_in_env: global bound to dropped rvar."
           | CE.SELECT _ => die "find_globals_in_env: global bound to select expression."
    in
      fun find_globals_in_env_all env = CE.labelsEnv labs env

      (* Result is a pair of label lists:              *)
      (*   -- labels to functions, (i.e., code labels) *)
      (*   -- labels to data, (i.e., data labels)      *)
      fun find_globals_in_env (lvars, excons, regvars) env =
        let
          fun lookup lv f_lookup (fun_lab,dat_lab) =
            case f_lookup env lv
              of SOME r => labs (fun_lab,dat_lab) r
               | NONE => die ("find_globals_in_env: lvar not bound in env.")
          val pair_labs = foldr (fn (lv,a) => lookup lv CE.lookupVarOpt a) ([],[]) lvars
          val pair_labs = foldr (fn (ex,a) => lookup ex CE.lookupExconOpt a) pair_labs excons
(*      val pair_labs = foldr (fn (rho,a) => lookup rho CE.lookupRhoOpt a) pair_labs regvars

 this one does not seem necessary because no new regions survive program units and because the
 the global regions 0-3 are allocated statically with address labels 0-3. ME 2000-10-31
*)
        in pair_labs
        end
    end

    fun gen_fresh_res_lvars (RE.Mus [mu]) =
        (case RType.unFUN (#1(RType.unbox mu)) of
             SOME(mus1,arroweffect,mus2) => List.map (fn _ => fresh_lvar("res")) mus2
           | NONE => die "gen_fresh_res: not a function type.")
      | gen_fresh_res_lvars (RE.Mus _) = die "gen_fresh_res: expecting singleton mu."
      | gen_fresh_res_lvars (RE.Frame _) = []
      | gen_fresh_res_lvars (RE.RaisedExnBind) = []

    (* Convert ~n to -n *)
    fun convert_real r =    (* Translate a real constant into C notation: *)
        let fun conv #"~" = #"-"
              | conv #"E" = #"e"
              | conv c = c
        in (implode o (map conv) o explode) r
        end

    fun pass_ptr_to_rho s pr sma =
        let
(*
            val ce = get_ce sma

            val () =
                case ce of
                    RVAR r => ()
                  | VAR _ => ()
                  | DROPPED_RVAR _ => die (s ^ "("^pr()^
                                           "): expecting only non-dropped rvars")
                  | _ => die (s ^ "("^pr()^
                              "): expecting only non-dropped rvars; sma=" ^ pp_sma sma)
*)
        in PASS_PTR_TO_RHO {sma=sma}
        end

    fun unTR (MulExp.TR a) = a

    (* ------------------------ *)
    (*    Closure Conversion    *)
    (* ------------------------ *)
    fun ccTrip (MulExp.TR(e,metaType,ateffects,mulef)) env lab cur_rv =
      let
        fun ccExp e =
          (case e of
             MulExp.VAR{lvar,...} => lookup_ve env lvar
           | MulExp.INTEGER(i,t,alloc) =>
             (INTEGER {value=i, precision=precisionNumType t}, NONE_SE)
           | MulExp.WORD(w,t,alloc) =>
             (WORD {value=w, precision=precisionNumType t}, NONE_SE)
           | MulExp.STRING(s,alloc) => (STRING s,NONE_SE)
           | MulExp.REAL(r,alloc) => (REAL (convert_real r),NONE_SE)
           | MulExp.F64 r => (F64 (convert_real r),NONE_SE)
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

                 val new_lab = Labels.renew lab "anon"
                 val lv_clos = fresh_lvar("clos")
                 val args = List.map #1 pat
                 val ress = gen_fresh_res_lvars metaType (* Result variables are not bound in env as they only exists in cc *)
                 val cc = CallConv.mk_cc_fn(args,SOME lv_clos,ress)

                 val env_body = build_clos_env env (get_global_env()) lv_clos BI.init_clos_offset free_vars_all
                 val env_with_args = (env_body plus_decl_with CE.declareLvar) (map (fn lv => (lv, CE.LVAR lv)) args)

                 val ces_and_ses = gen_ces_and_ses_free env free_vars
                   handle _ => die "FN"

                 val _ = add_new_fn(new_lab, cc, insert_se(ccTrip body env_with_args new_lab NONE))
                 val (sma,se_sma) = convert_alloc(alloc,env)
                 val (smas,ces,ses) = unify_smas_ces_and_ses_free([(sma,se_sma)],ces_and_ses)

                 val f64_vars = length (List.filter Lvars.get_ubf64 (#1 free_vars_all))
               in
                 (insert_ses(CLOS_RECORD{label=new_lab, elems=ces, f64_vars=f64_vars, alloc=one_in_list(smas)},ses),NONE_SE)
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
                   handle _ => die "FIX"
                 val lvars_labels_formals = map (fn {lvar, rhos_formals=ref formals, ...} =>
                                                 (lvar, fresh_lab(Lvars.pr_lvar lvar), formals)) functions
                 val lvars = map #lvar functions
                 val binds = map #bind functions
                 val formalss = map (! o #rhos_formals) functions (* place*phsize *)
                 val dropss = map (valOf o #bound_but_never_written_into) functions
                   handle Option => die "FIX.dropps: bound but never written was None"

                 val labels = map #2 lvars_labels_formals

                 val env_scope =
                   if shared_clos_size = 0 then
                     (env plus_decl_with CE.declareLvar)
                     (map (fn (lv,lab,formals) => (lv,CE.FIX(lab,NONE,0,formals))) lvars_labels_formals)
                   else
                     (env plus_decl_with CE.declareLvar)
                     (map (fn (lv,lab,formals) => (lv,CE.FIX(lab,SOME(CE.LVAR lv_sclos),shared_clos_size,formals))) lvars_labels_formals)

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
                         (map (fn (lv,lab,formals) => (lv,CE.FIX(lab,NONE,0,formals))) lvars_labels_formals)
                       else
                         (env_bodies plus_decl_with CE.declareLvar)
                         (map (fn (lv,lab,formals) => (lv,CE.FIX(lab,SOME(CE.LVAR lv_sclos_fn),shared_clos_size,formals))) lvars_labels_formals)
                     val lv_rv = fresh_lvar("rv")
                     val (reg_args, env_with_rv) =
                         List.foldr (fn ((place,_),(lvs,env)) =>
                                        let val lv = fresh_lvar "regarg"
                                        in (lv::lvs, CE.declareRho(place,CE.LVAR lv,env))
                                        end)
                                    (nil,env_with_funs) formals
                     val env_with_rho_kind =
                          (env_with_rv plus_decl_with CE.declareRhoKind)
                          (map (fn (place,phsize) => (place,mult("f",phsize))) formals)

                     val env_with_rho_drop =
                           (env_with_rho_kind plus_decl_with CE.declareRho)
                           (map (fn (place,_) => (place,CE.DROPPED_RVAR(drop_rho place))) drops)
                     val env_with_rho_drop_kind =
                           (env_with_rho_drop plus_decl_with CE.declareRhoKind)
                           (map (fn(place,phsize) => (place,mult("f",phsize))) drops)

                     val env_with_args =
                           (env_with_rho_drop_kind plus_decl_with CE.declareLvar)
                           (map (fn lv => (lv, CE.LVAR lv)) args)

(*                   val _ = print ("Closure size, " ^ (Lvars.pr_lvar lv_sclos_fn) ^ ": " ^ (Int.toString shared_clos_size) ^
                                    " " ^ (pr_free free_vars_in_shared_clos) ^ "\n") *)
                     val sclos = if shared_clos_size = 0 then NONE else SOME lv_sclos_fn (* 14/06-2000, Niels *)
                     val (fargs,args) = List.partition Lvars.get_ubf64 args
                     val cc = CallConv.mk_cc_fun(args,sclos,reg_args,fargs,ress)
                   in
                     add_new_fun(lab,cc,insert_se(ccTrip body env_with_args lab NONE))
                   end
                 val _ = List.app compile_fn (zip5 (lvars,binds,formalss,dropss,labels))
               in
                 if shared_clos_size = 0 then
                   (insert_se(ccTrip scope env_scope lab cur_rv),NONE_SE)
                 else
                   let
                     val (sma,se_a) = convert_alloc(alloc,env)
                     val (smas,ces,ses) = unify_smas_ces_and_ses_free([(sma,se_a)],ces_and_ses)
                     val f64_vars = length (List.filter Lvars.get_ubf64 (#1 free_vars_all))
                   in
                     (insert_ses(LET{pat=[lv_sclos],
                                     bind= SCLOS_RECORD{elems=ces,f64_vars=f64_vars,alloc=one_in_list(smas)},
                                     scope=insert_se(ccTrip scope env_scope lab cur_rv)},
                                 ses),NONE_SE)
                   end
               end
           | MulExp.FIX{free=_,shared_clos,functions,scope} => die "ccExp: No free variables in FIX"
           | MulExp.APP(SOME MulExp.JMP, _, tr1 as MulExp.TR(MulExp.VAR{lvar,fix_bound,rhos_actuals = ref rhos_actuals,...}, _, _, _), tr2) =>
               (* Poly tail call; this could be made more efficient if we distinguish between a tail call
                * and a jmp - that is, if we recognice that regions in registers and on the stack
                * can be reused. *)
               let
                 val _ =
                   if region_profiling() then
                     let val rhos_formals = lookup_fix_profiling env lvar
                     in RegionFlowGraphProfiling.add_edges((rhos_formals,Lvars.pr_lvar lvar),rhos_actuals)
                     end
                   else ()

                 val ces_and_ses = (* We remove the unboxed record. *)
                   case tr2 of
                     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
                   | _ => [ccTrip tr2 env lab cur_rv]

                 val (ce_clos,ces_arg,ses,lab_f) = compile_letrec_app env lvar ces_and_ses
               in
                   let val smas_regvec_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) rhos_actuals
                       val (smas,ses_sma,_) = unify_sma_se smas_regvec_and_ses SEMap.empty
                       val fresh_lvs = map (fn _ => fresh_lvar "sma") smas
                       fun maybe_insert_smas([],[],ce) = ce
                         | maybe_insert_smas(fresh_lvs,smas,ce) =
                           LET{pat=fresh_lvs,bind=UB_RECORD smas,scope=ce}
                   in
                     (insert_ses
                      (maybe_insert_smas
                       (fresh_lvs,map (pass_ptr_to_rho "JMP" (fn () => Lvars.pr_lvar lvar)) smas,
                        insert_ses
                        (JMP{opr=lab_f,args=ces_arg,reg_vec=NONE,
                             reg_args=map VAR fresh_lvs,clos=ce_clos},
                         ses)),
                        ses_sma),
                      NONE_SE)
                   end
               end
           | MulExp.APP(SOME MulExp.JMP, _, tr1 (*not lvar: error *), tr2) => die "JMP to other than lvar"
           | MulExp.APP(SOME MulExp.FUNCALL, _,
                        tr1 as MulExp.TR(MulExp.VAR{lvar,fix_bound=true, rhos_actuals=ref rhos_actuals,...},_,_,_),
                        tr2) =>
               let
                 (* Insert edges in the Region Flow Graph for Profiling. *)
                 val _ =
                   if region_profiling() then
                     let val rhos_formals = lookup_fix_profiling env lvar
                     in RegionFlowGraphProfiling.add_edges((rhos_formals,Lvars.pr_lvar lvar),rhos_actuals)
                     end
                   else ()
(*
                 fun check_rho s rho =
                     case lookup_rho env rho (fn () => "FUNCALL-check") of
                         (DROPPED_RVAR p,_) => die ("FUNCALL " ^ Lvars.pr_lvar lvar ^ ": check: " ^ s ^ " - rho: " ^ pr_rho rho ^ " - place: " ^ pr_rho p)
                       | _ => ()

                 val () = (* check that all rhos_actuals are NON-DROPPED *)
                     List.app (fn a => case a of
                                           AtInf.ATTOP(rho,pp) => check_rho "ATTOP" rho
                                         | AtInf.ATBOT(rho,pp) => check_rho "ATBOT" rho
                                         | AtInf.SAT(rho,pp) => check_rho "SAT" rho
                              ) rhos_actuals
*)
                 val ces_and_ses = (* We remove the unboxed record. *)
                   case tr2 of
                     MulExp.TR(MulExp.UB_RECORD trs,_,_,_) => List.map (fn tr => ccTrip tr env lab cur_rv) trs
                   | _ => [ccTrip tr2 env lab cur_rv]

                 val (ce_clos,ces_arg,ses,lab_f) = compile_letrec_app env lvar ces_and_ses
                 val (smas,ses_sma) =
                   let val smas_regvec_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) rhos_actuals
                       val (smas,ses_sma,_) = unify_sma_se smas_regvec_and_ses SEMap.empty
                   in (smas,ses_sma)
                   end

                 val fresh_lvs = map (fn _ => fresh_lvar "sma") smas
                 fun maybe_insert_smas([],[],ce) = ce
                   | maybe_insert_smas(fresh_lvs,smas,ce) =
                   LET{pat=fresh_lvs,bind=UB_RECORD smas,scope=ce}
               in
                 (insert_ses
                  (maybe_insert_smas
                   (fresh_lvs, map (pass_ptr_to_rho "FUNCALL" (fn () => Lvars.pr_lvar lvar)) smas,
                    insert_ses
                    (FUNCALL{opr=lab_f,args=ces_arg,reg_vec=NONE,
                             reg_args=map VAR fresh_lvs,clos=ce_clos},
                     ses)),
                    ses_sma),
                  NONE_SE)
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
                 (insert_ses(FNJMP{opr=ce_opr',args=ces',clos=SOME ce_opr'},
                             ses'),NONE_SE)
               end
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
                 (insert_ses(FNCALL{opr=ce_opr',args=ces',clos=SOME ce_opr'},
                             ses'),NONE_SE)
               end
           | MulExp.APP _ => die "application form not recognised"

           | MulExp.LETREGION{B,rhos=ref bound_regvars,body} =>
               let
                 (* Insert letregion nodes in the RegionFlowGraph. *)
                 val _ =
                   if region_profiling() then
                     RegionFlowGraphProfiling.add_nodes (bound_regvars,"LETREGION")
                   else ()

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
                 val env' = CE.declareExcon(excon,(CE.LVAR lv4,CE.NULLARY_EXCON),env)
                 val (sma,se_a) = convert_alloc(alloc,env)
               in
                 (LET{pat=[lv1],
                      bind=CCALL{name="__fresh_exname",
                                 args=[],
                                 rhos_for_result=[]},
                      scope=insert_se(LET{pat=[lv2],
                                          bind=STRING (Excon.pr_excon excon),
                                          scope=LET{pat=[lv3],
                                                    bind=RECORD{elems=[VAR lv1,VAR lv2],
                                                                alloc=sma,
                                                                tag=BI.tag_exname false,
                                                                maybeuntag=false},
                                                    scope=LET{pat=[lv4],
                                                              bind=RECORD{elems=[VAR lv3],
                                                                          alloc=sma,
                                                                          tag=BI.tag_excon0 false,
                                                                          maybeuntag=false},
                                                              scope=insert_se (ccTrip scope env' lab cur_rv)}}},
                                      se_a)},NONE_SE)
               end
           | MulExp.EXCEPTION(excon,false,typePlace,alloc,scope) => (* Unary exception constructor *)
               let
                 val lv1 = fresh_lvar "exn0-1"
                 val lv2 = fresh_lvar "exn0-2"
                 val lv3 = fresh_lvar "exn0-3"
                 val env' = CE.declareExcon(excon,(CE.LVAR lv3,CE.UNARY_EXCON),env)
                 val (sma,se_a) = convert_alloc(alloc,env)
               in
                 (LET{pat=[lv1],
                      bind=CCALL{name="__fresh_exname",
                                 args=[],
                                 rhos_for_result=[]},
                      scope=LET{pat=[lv2],
                                bind=STRING (Excon.pr_excon excon),
                                scope=insert_se(LET{pat=[lv3],
                                                    bind=RECORD{elems=[VAR lv1,VAR lv2],
                                                                alloc=sma,
                                                                tag=BI.tag_exname false,
                                                                maybeuntag=false},
                                                    scope=insert_se (ccTrip scope env' lab cur_rv)},se_a)}},NONE_SE)
               end
           | MulExp.RAISE tr =>
               let
                 val (ce,se) = ccTrip tr env lab cur_rv
               in
                 (insert_se(RAISE {exp=ce},se),NONE_SE)
               end
           | MulExp.HANDLE(tr1,tr2) => (HANDLE (insert_se(ccTrip tr1 env lab cur_rv),
                                                insert_se(ccTrip tr2 env lab cur_rv)),NONE_SE)
           | MulExp.SWITCH_I {switch=MulExp.SWITCH(tr,selections,opt), precision} =>
               let
                 val (selections,opt) =
                   compile_sels_and_default selections opt (fn i => i) (fn tr => ccTrip tr env lab cur_rv)
                 val (ce,se) = ccTrip tr env lab cur_rv
               in
                 (insert_se(SWITCH_I {switch=SWITCH(ce,selections,opt), precision=precision},se),NONE_SE)
               end
           | MulExp.SWITCH_W {switch=MulExp.SWITCH(tr,selections,opt), precision} =>
               let
                 val (selections,opt) =
                   compile_sels_and_default selections opt (fn i => i) (fn tr => ccTrip tr env lab cur_rv)
                 val (ce,se) = ccTrip tr env lab cur_rv
               in
                 (insert_se(SWITCH_W {switch=SWITCH(ce,selections,opt), precision=precision},se),NONE_SE)
               end
           | MulExp.SWITCH_S(MulExp.SWITCH(tr,selections,opt)) =>
               let
                 val (selections,opt) =
                   compile_sels_and_default selections opt (fn m=>m) (fn tr => ccTrip tr env lab cur_rv)
                 val (ce,se) = ccTrip tr env lab cur_rv

                 (* When tagging is enabled, integers in SWITCH_I are converted in
                  * CodeGenX86.sml - so in that case we must use an untagged representation
                  * of true, which is 1 (given that BI.ml_true is 3). *)
                 val True = IntInf.fromInt (if BI.ml_true = 3 then
                                              if BI.tag_values() then 1
                                              else BI.ml_true
                                            else die "True")
                 fun compile_seq_switch(ce,[],default) = default
                   | compile_seq_switch(ce,(s,ce')::rest,default) =
                   let
                     val lv_sw = fresh_lvar("sw")
                     val lv_s = fresh_lvar("str")
                   in
                     LET{pat=[lv_s],
                         bind=STRING s,
                         scope=LET{pat=[lv_sw],
                                   bind=CCALL{name="equalStringML",args=[ce,VAR lv_s],rhos_for_result=[]},
                                   scope=SWITCH_I{switch=SWITCH(VAR lv_sw,[(True,ce')],
                                                                compile_seq_switch(ce,rest,default)),
                                                  precision=BI.defaultIntPrecision()}}}
                   end
               in
                 (insert_se(compile_seq_switch(ce,selections,opt),se),NONE_SE)
               end

           | MulExp.SWITCH_C(MulExp.SWITCH(tr,selections,opt)) =>
               let
                 val () = if List.null selections
                          then die "ccTrip assumes empty switches have been eliminated"
                          else ()
                 val tn = case #2 (unTR tr) of
                              RE.Mus [mu] =>
                              (case RType.unCONSTYPE (#1 (RType.unbox mu)) of
                                   SOME (tn,_,_,_) => tn
                                 | NONE => die "SWITCH_C. expecting type name")
                            | _ => die "SWITCH_C. expecting single mu"
                 val () = case (selections, TyName.boxity tn) of
                              (nil,_) => ()
                            | (_, TyName.SINGLE _) =>
                              die ("expecting empty switches on single-constructor datatypes - "
                                   ^ (case opt of SOME _ => "SOME" | _ => "NONE"))
                            | _ => ()
                 fun tag con =
                   (case CE.lookupCon env con of
                      CE.ENUM i =>
                        if BI.tag_values() orelse (* hack to treat booleans tagged *)
                          Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then
                          (con,ENUM(2*i+1))
                        else
                          (con,ENUM i)
                    | CE.UB_NULLARY i => (con,UNBOXED(4*i+3))
                    | CE.UB_UNARY i => (con,UNBOXED i)
                    | CE.UBH_NULLARY i => (con,UNBOXED_HIGH i)
                    | CE.UBH_UNARY i => (con,UNBOXED_HIGH i)
                    | CE.B_NULLARY i => (con,BOXED(Word.toInt (BI.tag_con0(false,i))))
                    | CE.B_UNARY i => (con,BOXED(Word.toInt (BI.tag_con1(false,i)))))

                 val (selections,opt) =
                   compile_sels_and_default selections opt tag
                                            (fn tr => ccTrip tr env lab cur_rv)
                 val (ce,se) = ccTrip tr env lab cur_rv
               in
                 (insert_se(SWITCH_C(SWITCH(ce,selections,opt)),se),NONE_SE)
               end
           | MulExp.SWITCH_E(MulExp.SWITCH(tr,selections,opt)) =>
               let
                 val (selections,opt) =
                   compile_sels_and_default selections opt
                   (fn m=>(lookup_excon env m,CE.lookupExconArity env m))
                   (fn tr => ccTrip tr env lab cur_rv)
                 val (ce,se) = ccTrip tr env lab cur_rv
                 fun compile_seq_switch(ce,[],default) = default
                   | compile_seq_switch(ce,(((ce_e,se_e),arity),ce')::rest,default) =
                   let
                     val lv_sw = fresh_lvar("sw")
                     val lv_exn1 = fresh_lvar("exn1")
                     val lv_exn2 = fresh_lvar("exn2")
                   in
                     (case arity of
                        CE.NULLARY_EXCON =>
                          LET{pat=[lv_exn1],
                              bind=insert_se(SELECT(0,ce_e),se_e),
                              scope=LET{pat=[lv_exn2],
                                        bind=SELECT(0,VAR lv_exn1),
                                        scope=LET{pat=[lv_sw],
                                                  bind=CCALL{name="__equal_int32ub",
                                                             args=[ce,VAR lv_exn2],rhos_for_result=[]},
                                                  scope=SWITCH_I {switch=SWITCH(VAR lv_sw,[(IntInf.fromInt BI.ml_true,ce')],
                                                                                compile_seq_switch(ce,rest,default)),
                                                                  precision=BI.defaultIntPrecision()}}}}
                      | UNARY_EXCON =>
                          LET{pat=[lv_exn1],
                              bind=insert_se(SELECT(0,ce_e),se_e),
                              scope=LET{pat=[lv_sw],
                                        bind=CCALL{name="__equal_int32ub",
                                                   args=[ce,VAR lv_exn1],rhos_for_result=[]},
                                        scope=SWITCH_I {switch=SWITCH(VAR lv_sw,[(IntInf.fromInt BI.ml_true,ce')],
                                                                      compile_seq_switch(ce,rest,default)),
                                                        precision=BI.defaultIntPrecision()}}})
                   end
                 val lv_exn_arg1 = fresh_lvar("exn_arg1")
                 val lv_exn_arg2 = fresh_lvar("exn_arg2")
                 val ce_res =
                   insert_se(LET{pat=[lv_exn_arg1],
                                 bind=SELECT(0,ce),
                                 scope=LET{pat=[lv_exn_arg2],
                                           bind=SELECT(0,VAR lv_exn_arg1),
                                           scope=compile_seq_switch(VAR lv_exn_arg2,selections,opt)}},se)
               in
                 (ce_res,NONE_SE)
               end
           | MulExp.CON0{con,il,aux_regions,alloc=SOME alloc} =>
               let val (sma,se_a) = convert_alloc(alloc,env)
                   val smas_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) aux_regions
                   val (smas,ses',_) = unify_sma_se ((sma,se_a)::smas_and_ses) SEMap.empty
                   val (sma',smas') = split_in_hd_and_tl smas
               in
                 (insert_ses(CON0{con=con,
                                  con_kind=lookup_con env con,
                                  aux_regions=smas',
                                  alloc=sma'},ses'),NONE_SE)
               end
           | MulExp.CON0{con,il,aux_regions,alloc=NONE} =>
               let val smas_and_ses = List.map (fn alloc => convert_alloc(alloc,env)) aux_regions
                   val (smas,ses,_) = unify_sma_se smas_and_ses SEMap.empty
               in
                 (insert_ses(CON0{con=con,
                                  con_kind=lookup_con env con,
                                  aux_regions=smas,
                                  alloc=IGNORE},ses),NONE_SE)
               end
           | MulExp.CON1({con,il,alloc=SOME alloc},tr) =>
               let
                 val (sma,se_a) = convert_alloc(alloc,env)
                 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
                 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],[(ce_arg,se_arg)])
               in
                 (insert_ses(CON1{con=con,
                                  con_kind=lookup_con env con,
                                  alloc=one_in_list smas,
                                  arg=one_in_list ces},ses),NONE_SE)
               end
           | MulExp.CON1({con,il,alloc=NONE},tr) =>
               let
                 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
                 val (smas,ces,ses) = unify_smas_ces_and_ses([],[(ce_arg,se_arg)])
               in
                 (insert_ses(CON1{con=con,
                                  con_kind=lookup_con env con,
                                  alloc=IGNORE,
                                  arg=one_in_list ces},ses),NONE_SE)
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
                 val (smas,ces,ses) = unify_smas_ces_and_ses ([(sma,se_a)],[(ce_excon,se_excon),(ce_arg,se_arg)])
               in
                 (insert_ses(RECORD{elems=ces,
                                    alloc=one_in_list(smas),
                                    tag=BI.tag_excon1 false,
                                    maybeuntag=false},ses),NONE_SE)
               end
           | MulExp.DEEXCON(excon,tr) =>
               let
                 val (ce_arg,se_arg) = ccTrip tr env lab cur_rv
               in
                 (insert_se(SELECT(1,ce_arg),se_arg),NONE_SE)
               end
           | MulExp.RECORD(NONE, []) =>
             (insert_ses(RECORD{elems=[],
                                alloc=IGNORE,
                                tag=BI.tag_record(false,0),
                                maybeuntag=false},[]),NONE_SE)
           | MulExp.RECORD(NONE, _) => die "RECORD: expecting allocation place"
           | MulExp.RECORD(SOME alloc, trs) =>
               let
                 val ces_and_ses = List.foldr (fn (tr,b) => ccTrip tr env lab cur_rv::b) [] trs
                 val (sma,se_a) = convert_alloc(alloc,env)
                 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],ces_and_ses)
                 fun pair_or_triple_region rho =
                     case Effect.get_place_ty rho of
                         SOME Effect.PAIR_RT =>
                             if length trs = 2 then true
                             else die "RECORD.wrong number of elements in pair region"
                       | SOME Effect.TRIPLE_RT =>
                             if length trs = 3 then true
                             else die "RECORD.wrong number of elements in triple region"
                       | _ => false
                 val maybeuntag =
                     case alloc of
                         AtInf.ATTOP (rho,_) => pair_or_triple_region rho
                       | AtInf.ATBOT (rho,_) => pair_or_triple_region rho
                       | AtInf.SAT (rho,_) => pair_or_triple_region rho
               in
                 (insert_ses(RECORD{elems=ces,
                                    alloc=one_in_list smas,
                                    tag=BI.tag_record(false,length ces),
                                    maybeuntag=maybeuntag},ses),NONE_SE)
               end
           | MulExp.BLOCKF64(alloc, trs) =>
               let
                 val ces_and_ses = List.foldr (fn (tr,b) => ccTrip tr env lab cur_rv::b) [] trs
                 val (sma,se_a) = convert_alloc(alloc,env)
                 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],ces_and_ses)
               in
                 (insert_ses(BLOCKF64{elems=ces,
                                      alloc=one_in_list(smas),
                                      tag=BI.tag_blockf64(false,length ces)},ses),NONE_SE)
               end
           | MulExp.SCRATCHMEM(n,alloc) =>
               let
                 val (sma,se_a) = convert_alloc(alloc,env)
                 val (smas,ces,ses) = unify_smas_ces_and_ses([(sma,se_a)],[])
               in
                 (insert_ses(SCRATCHMEM{bytes=n,
                                        alloc=one_in_list(smas),
                                        tag=BI.tag_blockf64(false,(8+n-1) div 8)},ses),NONE_SE)
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
                 (insert_se(DEREF {exp=ce},se),NONE_SE)
               end
           | MulExp.ASSIGN(tr1,tr2) =>
               let
                 val (ce1,ce2,ses) =
                     case unify_smas_ces_and_ses([],[ccTrip tr1 env lab cur_rv,ccTrip tr2 env lab cur_rv]) of
                         ([],[ce1,ce2],ses) => (ce1,ce2,ses)
                   | _ => die "ASSIGN: error in unify."
               in
                 (insert_ses(ASSIGN(IGNORE,ce1,ce2),ses),NONE_SE)
               end
           | MulExp.DROP tr =>
               let
                 val (ce,se) = ccTrip tr env lab cur_rv
               in
                 (insert_se(DROP {exp=ce},se),NONE_SE)
               end
           | MulExp.EQUAL({mu_of_arg1,mu_of_arg2},tr1,tr2) =>
               let
                 val tau = case tr1 of
                               MulExp.TR(_,RE.Mus[mu],_,_) => #1(RType.unbox mu)
                             | _ => die "EQUAL.metaType not Mus."

                 val (ce1,ce2,ses) =
                     case unify_ce_se [ccTrip tr1 env lab cur_rv,
                                       ccTrip tr2 env lab cur_rv] SEMap.empty of
                         ([ce1,ce2],ses,_) => (ce1,ce2,ses)
                       | _ => die "EQUAL: error in unify."
                 fun eq_prim n = CCALL{name=n,args=[ce1,ce2],rhos_for_result=[]}
                 val ce =
                     case RType.unCONSTYPE tau of
                      SOME(tn,_,_,_) =>
                        if (TyName.eq(tn,TyName.tyName_BOOL)
                            orelse TyName.eq(tn,TyName.tyName_REF)
                            orelse TyName.eq(tn,TyName.tyName_ARRAY)
                            orelse TyName.eq(tn,TyName.tyName_CHARARRAY))
                          then
                            eq_prim "__equal_int32ub"
                        else if TyName.eq(tn,TyName.tyName_INT31) then
                          eq_prim "__equal_int31"
                        else if TyName.eq(tn,TyName.tyName_INT32) then
                          (if BI.tag_values() then eq_prim "__equal_int32b"
                           else eq_prim "__equal_int32ub")
                        else if TyName.eq(tn,TyName.tyName_INT63) then
                          eq_prim "__equal_int63"
                        else if TyName.eq(tn,TyName.tyName_INT64) then
                          (if BI.tag_values() then eq_prim "__equal_int64b"
                           else eq_prim "__equal_int64ub")
                        else if TyName.eq(tn,TyName.tyName_WORD31) then
                          eq_prim "__equal_word31"
                        else if TyName.eq(tn,TyName.tyName_WORD32) then
                          (if BI.tag_values() then eq_prim "__equal_word32b"
                           else eq_prim "__equal_word32ub")
                        else if TyName.eq(tn,TyName.tyName_WORD63) then
                          eq_prim "__equal_word63"
                        else if TyName.eq(tn,TyName.tyName_WORD64) then
                          (if BI.tag_values() then eq_prim "__equal_word64b"
                           else eq_prim "__equal_word64ub")
                        else if TyName.eq(tn,TyName.tyName_CHAR) then
                          eq_prim "__equal_char"
                        else if TyName.eq(tn,TyName.tyName_WORD8) then
                          eq_prim "__equal_word8"
                        else if TyName.eq(tn,TyName.tyName_STRING) then
                          eq_prim "equalStringML"
                        else if TyName.eq(tn,TyName.tyName_FOREIGNPTR) then
                          (if BI.tag_values() then eq_prim "__equal_word64b"
                           else eq_prim "__equal_word64ub")
                        else if TyName.eq(tn,TyName.tyName_VECTOR) then
                               die "`=' on vectors! EliminateEq should have dealt with this"
                        else eq_prim "equalPolyML"
                    | NONE =>
                      case RType.unRECORD tau of
                        SOME [] => eq_prim "__equal_int32ub" (* bool *)
                      | _ => eq_prim "equalPolyML"
               in
                 (insert_ses(ce,ses),NONE_SE)
               end
           | MulExp.CCALL({name = "id", mu_result, rhos_for_result}, trs) =>
               (case trs of
                  [tr] => (insert_se(ccTrip tr env lab cur_rv),NONE_SE)
                | _ => die "CCALL: ``id'' with more than one tr")
           | MulExp.CCALL({name = "pointer", mu_result, rhos_for_result}, trs) =>
             (* unsafe cast; pointer : 'a -> foreignptr, in particular with 'a
              * instantiated to a function *)
               (case trs of
                  [tr] => (insert_se(ccTrip tr env lab cur_rv),NONE_SE)
                | _ => die "CCALL: ``pointer'' with more than one tr")
           | MulExp.CCALL({name = "unsafe_cast", mu_result, rhos_for_result}, trs) =>
             (* unsafe cast; unsafe_cast : 'a -> 'b, in particular with 'a
              * instantiated to a function *)
               (case trs of
                  [tr] => (insert_se(ccTrip tr env lab cur_rv),NONE_SE)
                | _ => die "CCALL: ``pointer'' with more than one tr")
           | MulExp.CCALL({name = "ord", mu_result, rhos_for_result}, trs) =>
               (case trs of
                  [tr] => (insert_se(ccTrip tr env lab cur_rv),NONE_SE)
                | _ => die "CCALL: ``ord'' with more than one tr")
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
                 fun add_pp_for_profiling ([], args) = (name, args)
                   | add_pp_for_profiling ((sma,i_opt)::rest,args) =
                   if region_profiling() then
                       (case i_opt of
                          SOME 0 => die "get_pp_for_profiling (CCALL ...): argument region with size 0"
                        | SOME i => add_pp_for_profiling(rest,args)
                        | NONE   => (name ^ "Prof", args @ [INTEGER {value=IntInf.fromInt(get_pp sma),
                                                                     precision=BI.defaultIntPrecision()}]))
                                            (*get any arbitrary pp (they are the same):*)
                   else (name, args)

                 fun comp_region_args_sma [] = []
                   | comp_region_args_sma ((sma, i_opt)::rest) =
                     case i_opt of
                         SOME 0 => die "ccExp (CCALL ...): argument region with size 0"
                       | SOME i => PASS_PTR_TO_MEM(sma,i) :: comp_region_args_sma rest
                       | NONE   => pass_ptr_to_rho "CCALL" (fn () => name) sma :: comp_region_args_sma rest
                 val smas_and_ses = List.map (fn (alloc,_) => convert_alloc(alloc,env)) rhos_for_result
                 val i_opts = List.map #2 rhos_for_result

                 val ces_and_ses = List.map (fn tr => ccTrip tr env lab cur_rv) trs
                 val (smas',ces,ses) = unify_smas_ces_and_ses(smas_and_ses,ces_and_ses)
                 val rhos_for_result' = zip(smas',i_opts)
                 val smas = comp_region_args_sma rhos_for_result'
                 val maybe_return_unit =
                   if BI.tag_values() then
                   (case RType.unRECORD(#1 (RType.unbox (mu_result))) of
                      SOME [] => (fn ce => LET{pat=[fresh_lvar("ccall")],bind=ce,
                                               scope=RECORD{elems=[],
                                                            alloc=IGNORE,
                                                            tag=BI.tag_ignore,
                                                            maybeuntag=false}})
                    | _ => (fn ce => ce))
                   else (fn ce => ce)
                 val fresh_lvs = map (fn _ => fresh_lvar "sma") smas
                 fun maybe_insert_smas ([],[],ce) = ce
                   | maybe_insert_smas (fresh_lvs,smas,ce) =
                   LET{pat=fresh_lvs,bind=UB_RECORD smas,scope=ce}
               in
                 (case explode name of
                      #"@" :: rest =>    (* AUTO CONVERSION *)
                      let val name = implode rest
                          fun mu_trs tr =
                              case tr of
                                  MulExp.TR(_,RE.Mus[mu],_,_) => mu
                                | _ => die "CCALL_AUTO.ty"
                          fun fmu mu : foreign_type =
                              let val (ty,_) = RType.unbox mu
                              in case RType.unCONSTYPE ty of
                                     SOME(tn,_,_,_) => tn_to_foreign_type tn
                                   | NONE => case RType.unRECORD ty of
                                                 SOME [] => Unit
                                               | _ => die "CCALL_AUTO.fty"
                              end
                          val args = ListPair.zip(ces,map (fmu o mu_trs) trs)
                                     handle _ => die "CCALL_AUTO.zip"
                          val res = case fmu mu_result
                                      of CharArray => die "CCALL_AUTO.CharArray not supported in result"
                                       | t => t
                      in (insert_ses(maybe_insert_smas(fresh_lvs,smas,
                                                       CCALL_AUTO{name=name,
                                                                  args=args,
                                                                  res=res,
                                                                  rhos_for_result=map VAR fresh_lvs}),
                                     ses),
                          NONE_SE)
                      end
                    | _ =>

                      (* for overloaded primitives that may raise exceptions (e.g., div and mod),
                       * we add the evaluation context as the first parameter to the function; we
                       * do this here, instead of in the frontend, to avoid that other backends
                       * (e.g., the Javascript backend) are affected by the fact that the native
                       * backend requires that a context is made accessible during evaluation.
                       *)

                      let fun cons_ctx ces =
                              let val lv_ctx = fresh_lvar "ctx"
                              in ( fn e => LET{pat=[lv_ctx],bind=CCALL{name="__get_ctx",args=[],rhos_for_result=[]},
                                               scope=e}
                                 , VAR lv_ctx :: ces
                                 )
                              end
                          val (maybe_add_context, ces) =
                              case name of
                                  "__div_word31" => cons_ctx ces
                                | "__div_word32ub" => cons_ctx ces
                                | "__div_word32b" => cons_ctx ces
                                | "__div_word63" => cons_ctx ces
                                | "__div_word64ub" => cons_ctx ces
                                | "__div_word64b" => cons_ctx ces
                                | "__mod_word31" => cons_ctx ces
                                | "__mod_word32ub" => cons_ctx ces
                                | "__mod_word32b" => cons_ctx ces
                                | "__mod_word63" => cons_ctx ces
                                | "__mod_word64ub" => cons_ctx ces
                                | "__mod_word64b" => cons_ctx ces
                                | "__div_int31" => cons_ctx ces
                                | "__div_int32ub" => cons_ctx ces
                                | "__div_int32b" => cons_ctx ces
                                | "__div_int63" => cons_ctx ces
                                | "__div_int64ub" => cons_ctx ces
                                | "__div_int64b" => cons_ctx ces
                                | "__mod_int31" => cons_ctx ces
                                | "__mod_int32ub" => cons_ctx ces
                                | "__mod_int32b" => cons_ctx ces
                                | "__mod_int63" => cons_ctx ces
                                | "__mod_int64ub" => cons_ctx ces
                                | "__mod_int64b" => cons_ctx ces
                                | _ => (fn x => x, ces)
                          val (name, args) = add_pp_for_profiling(rhos_for_result',ces)
                      in (maybe_return_unit
                          (insert_ses(maybe_insert_smas(fresh_lvs,smas,
                                                        maybe_add_context
                                                            (CCALL{name=name,
                                                                   args=args,
                                                                   rhos_for_result=map VAR fresh_lvs})),
                                      ses)),
                          NONE_SE)
                      end)
               end
           | MulExp.EXPORT({name,mu_arg,mu_res},tr) =>
               let val (ce,se) = ccTrip tr env lab cur_rv
                   fun toForeignType mu : foreign_type =
                       let val ty = case RType.unBOX mu of
                                        SOME (ty,_) => ty
                                      | NONE => mu
                       in case RType.unCONSTYPE ty of
                              SOME(tn,_,_,_) => tn_to_foreign_type tn
                            | NONE => case RType.unRECORD ty of
                                          SOME [] => Unit
                                        | _ => die "EXPORT.toForeignType"
                       end
                   val maybe_return_unit =
                       if BI.tag_values() andalso toForeignType mu_res = Unit then
                           (fn ce => LET{pat=[fresh_lvar("export")],bind=ce,
                                         scope=RECORD{elems=[],
                                                      alloc=IGNORE,
                                                      tag=BI.tag_ignore,
                                                      maybeuntag=false}})
                       else (fn ce => ce)

                   val lab = Labels.new_named ("ExportClosLab_" ^ name)
                   val _ = add_new_export lab
               in
                   (maybe_return_unit
                    (insert_se (EXPORT{name=name,
                                       clos_lab=lab,
                                       arg=(ce,toForeignType mu_arg, toForeignType mu_res)},
                                se))
                    , NONE_SE)
               end
           | MulExp.RESET_REGIONS({force,regions_for_resetting,...},tr) =>
               let
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
                                CE.FIX(lab,SOME(CE.LVAR lv_clos),i,formals) =>
                                  let
                                    val lab_sclos = fresh_lab(Lvars.pr_lvar lv_clos ^ "_lab")
                                  in
                                    (SOME{lvar=lv_clos,label=lab_sclos},{lvar=lvar,acc_type=CE.FIX(lab,SOME(CE.LABEL lab_sclos),i,formals)})
                                  end
                              | CE.FIX(lab,NONE,i,formals) => (NONE,{lvar=lvar,acc_type=CE.FIX(lab,NONE,i,formals)})
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
                   (map (fn {excon,label} => (excon,(CE.LABEL label,
                                                     CE.lookupExconArity env excon))) excons_and_labels)
                 val _ = set_frame_env frame_env
               in
                 (List.foldr (fn ({excon,label},acc) =>
                              let
                                val (ce,se) = lookup_excon env excon
                              in
                                LET{pat=[(*fresh_lvar("not_used")*)],bind=insert_se(STORE(ce,label),se),scope=acc}
                              end)
                  (List.foldr (fn ({lvar,label},acc) => LET{pat=[(*fresh_lvar("not_used")*)],bind=STORE(VAR lvar,label),scope=acc})
                   (FRAME{declared_lvars=lvars_and_labels,declared_excons=excons_and_labels}) lvars_and_labels)
                  excons_and_labels, NONE_SE)
               end)
      in
        ccExp e
      end (* End ccTrip *)
  in
    fun clos_conv(l2clos_exp_env, Fenv,
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
        val _ = reset_exports()
        val import_labs =
          find_globals_in_env (valOf(!import_vars)) l2clos_exp_env
          handle _ => die "clos_conv: import_vars not specified."
        val env_datbind = add_datbinds_to_env export_datbinds CE.empty
        val global_env = CE.plus (l2clos_exp_env, env_datbind)
        val _ = set_global_env global_env
        val main_lab = fresh_lab "main"
        val clos_exp = insert_se(ccTrip tr global_env main_lab NONE)
        val _ = add_new_fn(main_lab,CallConv.mk_cc_fn([],NONE,[]),clos_exp)
        val export_env = CE.plus (env_datbind, (get_frame_env()))
        val export_labs = find_globals_in_env (export_vars) (get_frame_env())
        val export_labs = (#1 export_labs, #2 export_labs @ get_exports())
      (* val _ = display("\nReport: export_env:", CE.layoutEnv export_env)*)
      in
        {main_lab=main_lab,
         code=get_top_decls(),
         env=export_env,
         imports=import_labs,
         exports=export_labs}
      end (* End clos_conv *)

  end

  val empty = ClosConvEnv.empty
  val plus = ClosConvEnv.plus
  val init = ClosConvEnv.initialEnv
  val enrich = ClosConvEnv.enrich
  val match = ClosConvEnv.match
  val restrict = ClosConvEnv.restrict
  val restrict0 = ClosConvEnv.restrict0
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
        if print_normalized_program_p() then
          display("\nReport: AFTER NORMALIZATION:", PhysSizeInf.layout_pgm n_prog)
        else
          ()
      val Fenv = F n_prog
      val all = clos_conv (clos_env, Fenv, n_prog)
      val _ =
        if print_clos_conv_program_p() then
          display("\nReport: AFTER CLOSURE CONVERSION:", layout_clos_prg (#code(all)))
        else
          ()
      val _ = chat "]\n"
    in
      all
    end

  val pu = ClosConvEnv.pu

  fun retrieve_lvar (env:env) (lvar:lvar) : label option =
      case ClosConvEnv.lookupVarOpt env lvar of
          SOME (ClosConvEnv.LABEL l) => SOME l
        | _ => NONE

  local structure CCE = ClosConvEnv
  in
  fun conkind (env:env) (con:con) : con_kind =
      case CCE.lookupCon env con of
          CCE.ENUM i => ENUM i
        | CCE.UB_NULLARY i => UNBOXED i
        | CCE.UB_UNARY i => UNBOXED i
        | CCE.UBH_NULLARY i => UNBOXED_HIGH i
        | CCE.UBH_UNARY i => UNBOXED_HIGH i
        | CCE.B_NULLARY i => BOXED i
        | CCE.B_UNARY i => BOXED i
  end

end
