functor SubstAndSimplify(structure LineStmt: LINE_STMT
                           where type con = Con.con
                           where type excon = Excon.excon
                           where type lvar = Lvars.lvar
                           where type place = Effect.effect
                           where type label = AddressLabels.label
                           where type phsize = PhysSizeInf.phsize
                           where type StringTree = PrettyPrint.StringTree
                           where type cc = CallConv.cc
                         structure CalcOffset: CALC_OFFSET
                           where type lvar = Lvars.lvar
                           where type place = Effect.place
                           sharing type CalcOffset.Atom = LineStmt.Atom
                         structure RI : REGISTER_INFO
                           where type lvar = Lvars.lvar)
    : SUBST_AND_SIMPLIFY =
struct
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  structure RegvarFinMap = EffVarEnv
  val _ = Flags.add_bool_entry
    {long="print_simplified_program", short=NONE, item=ref false,
     menu=["Printing of intermediate forms", "print simplified program (LineStmt)"], neg=false,
     desc="Print simplified program after register\n\
      \allocation."}

  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  type phsize = PhysSizeInf.phsize
  type pp = PhysSizeInf.pp
  type cc = CallConv.cc
  type label = Labels.label
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
  type StoreTypeCO = CalcOffset.StoreType
  type AtomCO = CalcOffset.Atom
  type reg = RI.reg
  type offset = int

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("SubstAndSimplify." ^ s)
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

  (***************)
  (* Access Type *)
  (***************)
  datatype Aty =
      REG_I_ATY        of offset
    | REG_F_ATY        of offset
    | STACK_ATY        of offset
    | FLOW_VAR_ATY     of lvar * label * label
    | DROPPED_RVAR_ATY
    | PHREG_ATY        of reg
    | INTEGER_ATY      of {value: IntInf.int, precision: int}
    | WORD_ATY         of {value: IntInf.int, precision: int}
    | UNIT_ATY

  fun pr_offset offset = CalcOffset.pr_offset offset
  fun pr_phreg phreg = RI.pr_reg phreg

  fun pr_aty(REG_I_ATY offset) = "reg_i(" ^ Int.toString offset ^ ")"
    | pr_aty(REG_F_ATY offset) = "reg_f(" ^ Int.toString offset ^ ")"
    | pr_aty(STACK_ATY offset) = "stack(" ^ Int.toString offset ^ ")"
    | pr_aty(FLOW_VAR_ATY(lv,l1,l2)) = "FV(" ^ Lvars.pr_lvar lv ^ ")"
    | pr_aty(DROPPED_RVAR_ATY) = "DROPPED_RVAR"
    | pr_aty(PHREG_ATY phreg) = pr_phreg phreg
    | pr_aty(INTEGER_ATY {value, precision}) = IntInf.toString value
    | pr_aty(WORD_ATY {value, precision}) = "0w" ^ IntInf.toString value
    | pr_aty(UNIT_ATY) = "(.)"

  fun eq_aty(REG_I_ATY offset1,REG_I_ATY offset2) = offset1 = offset2
    | eq_aty(REG_F_ATY offset1,REG_F_ATY offset2) = offset1 = offset2
    | eq_aty(STACK_ATY offset1,STACK_ATY offset2) = offset1 = offset2
    | eq_aty(DROPPED_RVAR_ATY,DROPPED_RVAR_ATY) = true
    | eq_aty(PHREG_ATY phreg1,PHREG_ATY phreg2) = phreg1 = phreg2
    | eq_aty(INTEGER_ATY i1,INTEGER_ATY i2) = i1 = i2
    | eq_aty(WORD_ATY i1,WORD_ATY i2) = i1 = i2
    | eq_aty(UNIT_ATY,UNIT_ATY) = true
    | eq_aty _ = false


  (*****************************)
  (* Simplify And Substitution *)
  (*****************************)
  local
    structure LS = LineStmt
    structure CO = CalcOffset

    structure LvarFinMap = Lvars.Map
    structure RhoFinMap = RegvarFinMap

    fun add_sty_lv(CO.STACK_STY(lv,offset),ATYmap) = LvarFinMap.add(lv,STACK_ATY offset,ATYmap)
      | add_sty_lv(CO.PHREG_STY(lv,phreg),ATYmap) = LvarFinMap.add(lv,PHREG_ATY (RI.lv_to_reg phreg),ATYmap)
      | add_sty_lv(CO.FLUSHED_CALLEE_STY(phreg,offset),ATYmap) = ATYmap
      | add_sty_lv(CO.FLUSHED_CALLER_STY(lv,phreg,offset),ATYmap) = LvarFinMap.add(lv,PHREG_ATY (RI.lv_to_reg phreg),ATYmap)
      | add_sty_lv(CO.FV_STY lv,ATYmap) = ATYmap

    fun binder_to_aty((place,PhysSizeInf.INF),offset) = REG_I_ATY offset
      | binder_to_aty((place,PhysSizeInf.WORDS i),offset) = REG_F_ATY offset

    fun lookup_lv_aty(ATYmap,lv) =
(*    if Lvars.eq(Lvars.wild_card, lv) then UNIT_ATY
      else *)
      case LvarFinMap.lookup ATYmap lv
        of SOME r => r
         | NONE  => die ("lookup_lv_aty(" ^ (Lvars.pr_lvar lv) ^ ")")

    fun add_sty_lvs([],ATYmap) = ATYmap
      | add_sty_lvs(sty::rest,ATYmap) = add_sty_lvs(rest,add_sty_lv(sty,ATYmap))

    fun lookup_rho_aty(RHOmap,place) =
      case RhoFinMap.lookup RHOmap place of
        SOME r => r
      | NONE  => die ("lookup_binder_aty(" ^ (PP.flatten1(Effect.layout_effect place)) ^ ")")
    fun add_sty_binders([],RHOmap) = RHOmap
      | add_sty_binders(((place,phsize),offset)::rest,RHOmap) = add_sty_binders(rest,RhoFinMap.add(place,binder_to_aty((place,phsize),offset),RHOmap))

    fun atom_to_aty(LS.VAR lv,ATYmap,RHOmap) = lookup_lv_aty(ATYmap,lv)
      | atom_to_aty(LS.RVAR place,ATYmap,RHOmap) = lookup_rho_aty(RHOmap,place)
      | atom_to_aty(LS.DROPPED_RVAR place,ATYmap,RHOmap) = DROPPED_RVAR_ATY
      | atom_to_aty(LS.PHREG phreg,ATYmap,RHOmap) = PHREG_ATY (RI.lv_to_reg phreg)
      | atom_to_aty(LS.INTEGER i,ATYmap,RHOmap) = INTEGER_ATY i
      | atom_to_aty(LS.WORD i,ATYmap,RHOmap) = WORD_ATY i
      | atom_to_aty(LS.UNIT,ATYmap,RHOmap) = UNIT_ATY
      | atom_to_aty(LS.FLOW_VAR lv,ATYmap,RHOmap) = FLOW_VAR_ATY lv

    fun atom_to_aty_opt(NONE,ATYmap,RHOmap) = NONE
      | atom_to_aty_opt(SOME atom,ATYmap,RHOmap) = SOME(atom_to_aty(atom,ATYmap,RHOmap))

    fun sma_to_sma(LS.ATTOP_LI(atom,pp),ATYmap,RHOmap) = LS.ATTOP_LI(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.ATTOP_LF(atom,pp),ATYmap,RHOmap) = LS.ATTOP_LF(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.ATTOP_FI(atom,pp),ATYmap,RHOmap) = LS.ATTOP_FI(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.ATTOP_FF(atom,pp),ATYmap,RHOmap) = LS.ATTOP_FF(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.ATBOT_LI(atom,pp),ATYmap,RHOmap) = LS.ATBOT_LI(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.ATBOT_LF(atom,pp),ATYmap,RHOmap) = LS.ATBOT_LF(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.SAT_FI(atom,pp),ATYmap,RHOmap) = LS.SAT_FI(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.SAT_FF(atom,pp),ATYmap,RHOmap) = LS.SAT_FF(atom_to_aty(atom,ATYmap,RHOmap),pp)
      | sma_to_sma(LS.IGNORE,ATYmap,RHOmap) = LS.IGNORE

    fun aty_eq_se(aty1,LS.ATOM {aty=aty2}) = eq_aty(aty1,aty2)
      | aty_eq_se _ = false

    fun SS_sw(SS_lss,switch_con,LS.SWITCH(atom,sels,default),ATYmap,RHOmap) =
      let
        val sels' =
          foldr (fn ((sel,lss),sels_acum) => (sel,SS_lss(lss,ATYmap,RHOmap))::sels_acum) [] sels
        val default' = SS_lss(default,ATYmap,RHOmap)
      in
        switch_con(LS.SWITCH(atom_to_aty(atom,ATYmap,RHOmap),sels',default'))
      end

    fun SS_lss(lss,ATYmap,RHOmap) =
      let
        fun smas_to_smas smas = map (fn sma => sma_to_sma(sma,ATYmap,RHOmap)) smas
        fun atoms_to_atys atoms = map (fn atom => atom_to_aty(atom,ATYmap,RHOmap)) atoms

        fun sma_to_sma' sma = sma_to_sma(sma,ATYmap,RHOmap)
        fun atom_to_aty' atom = atom_to_aty(atom,ATYmap,RHOmap)

        fun do_fn_app{opr,args,clos,res,bv} =
          {opr=atom_to_aty' opr,
           args=atoms_to_atys args,
           clos=atom_to_aty_opt(clos,ATYmap,RHOmap),
           res=atoms_to_atys res,
           bv=bv}

        fun do_fun_app{opr,args,clos,res,reg_args,fargs,bv} =
          {opr=opr,
           args=atoms_to_atys args,
           clos=atom_to_aty_opt(clos,ATYmap,RHOmap),
           res=atoms_to_atys res,
           reg_args=atoms_to_atys reg_args,
           fargs=atoms_to_atys fargs,
           bv=bv}

        fun SS_se(LS.ATOM {aty=atom}) = LS.ATOM {aty=atom_to_aty' atom}
          | SS_se(LS.LOAD label) = LS.LOAD label
          | SS_se(LS.STORE(atom,label)) = LS.STORE(atom_to_aty' atom,label)
          | SS_se(LS.STRING str) = LS.STRING str
          | SS_se(LS.REAL str) = LS.REAL str
          | SS_se(LS.F64 str) = LS.F64 str
          | SS_se(LS.CLOS_RECORD{label,elems=(lvs,excons,rhos),f64_vars,alloc}) =
            LS.CLOS_RECORD{label=label,
                           elems=(atoms_to_atys lvs,atoms_to_atys excons,atoms_to_atys rhos),
                           f64_vars=f64_vars,
                           alloc= sma_to_sma' alloc}
          | SS_se(LS.SCLOS_RECORD{elems=(lvs,excons,rhos),f64_vars,alloc}) =
            LS.SCLOS_RECORD{elems=(atoms_to_atys lvs,atoms_to_atys excons,atoms_to_atys rhos),
                            f64_vars=f64_vars,
                            alloc = sma_to_sma' alloc}
          | SS_se(LS.RECORD{elems,alloc,tag,maybeuntag}) = LS.RECORD{elems=atoms_to_atys elems,alloc=sma_to_sma' alloc,tag=tag,
                                                                     maybeuntag=maybeuntag}
          | SS_se(LS.BLOCKF64{elems,alloc,tag}) = LS.BLOCKF64{elems=atoms_to_atys elems,alloc=sma_to_sma' alloc,tag=tag}
          | SS_se(LS.SCRATCHMEM{bytes,alloc,tag}) = LS.SCRATCHMEM{bytes=bytes,alloc=sma_to_sma' alloc,tag=tag}
          | SS_se(LS.SELECT(i,atom)) = LS.SELECT(i,atom_to_aty' atom)
          | SS_se(LS.CON0{con,con_kind,aux_regions,alloc}) = LS.CON0{con=con,con_kind=con_kind,aux_regions=smas_to_smas aux_regions,alloc=sma_to_sma' alloc}
          | SS_se(LS.CON1{con,con_kind,alloc,arg}) = LS.CON1{con=con,con_kind=con_kind,alloc=sma_to_sma' alloc,arg=atom_to_aty' arg}
          | SS_se(LS.DECON{con,con_kind,con_aty}) = LS.DECON{con=con,con_kind=con_kind,con_aty=atom_to_aty' con_aty}
          | SS_se(LS.DEREF {aty=atom}) = LS.DEREF {aty=atom_to_aty' atom}
          | SS_se(LS.REF(sma,atom)) = LS.REF(sma_to_sma' sma,atom_to_aty' atom)
          | SS_se(LS.ASSIGNREF(sma,atom1,atom2)) = LS.ASSIGNREF(sma_to_sma' sma,atom_to_aty' atom1,atom_to_aty' atom2)
          | SS_se(LS.PASS_PTR_TO_MEM(sma,i,b)) = LS.PASS_PTR_TO_MEM(sma_to_sma' sma,i,b)
          | SS_se(LS.PASS_PTR_TO_RHO {sma}) = LS.PASS_PTR_TO_RHO {sma=sma_to_sma' sma}

        fun SS_lss'([]) = []
          | SS_lss'(LS.ASSIGN{pat,bind}::lss) =
            let
              val pat' = atom_to_aty' pat
              val bind' = SS_se bind
            in
              if aty_eq_se(pat',bind') then
                SS_lss(lss,ATYmap,RHOmap)
              else
                LS.ASSIGN{pat=pat',bind=bind'} :: SS_lss(lss,ATYmap,RHOmap)
            end
          | SS_lss'(LS.FLUSH(atom,offset)::lss) = LS.FLUSH(atom_to_aty' atom,offset) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.FETCH(atom,offset)::lss) = LS.FETCH(atom_to_aty' atom,offset) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.FNJMP a::lss) = LS.FNJMP(do_fn_app a) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.FNCALL a::lss) = LS.FNCALL(do_fn_app a) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.JMP a::lss) = LS.JMP(do_fun_app a) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.FUNCALL a::lss) = LS.FUNCALL(do_fun_app a) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.LETREGION{rhos,body}::lss) = LS.LETREGION{rhos=rhos,body=SS_lss(body,ATYmap,add_sty_binders(rhos,RHOmap))} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.SCOPE{pat,scope}::lss) = LS.SCOPE{pat=pat,scope=SS_lss(scope,add_sty_lvs(pat,ATYmap),RHOmap)} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_lv,bv),offset}::lss) =
          (* MEGA HACK: *)
          (* The lvar handl_lv is the lvar that the handle closure is bound to in handl. We need *)
          (* handl_lv in CodeGen to make code that stores the handle closure in the              *)
          (* exception handler in the activation frame. handl_lv is bound to the outer most      *)
          (* scope declaration in handl and we match it out explicitly to get access to the sty  *)
          (* that is necessary to produce the aty for handl_lv.                                  *)
          (* Note, we do not have a problem with hand_return_lv that is the lvar containing the  *)
          (* result of evaluating either default or the handler, (i.e., hand_return_lv must be   *)
          (* bound outside default and therefore included in the incoming ATYmap).               *)
            let
              val handl_aty =
                  case handl of
                      LS.SCOPE{pat,scope}::[] => atom_to_aty(handl_lv,add_sty_lvs(pat,ATYmap),RHOmap)
                    | _ => die "SS_lss': SCOPE not first in handl"
            in
              LS.HANDLE{default=SS_lss(default,ATYmap,RHOmap),
                        handl=(SS_lss(handl,ATYmap,RHOmap),handl_aty),
                        handl_return=(SS_lss(handl_return,ATYmap,RHOmap),atom_to_aty' handl_return_lv,bv),
                        offset=offset} :: SS_lss(lss,ATYmap,RHOmap)
            end
          | SS_lss'(LS.RAISE{arg,defined_atys}::lss) = LS.RAISE{arg=atom_to_aty' arg,defined_atys=atoms_to_atys defined_atys} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.SWITCH_I {switch,precision}::lss) =
            (SS_sw(SS_lss, fn sw => LS.SWITCH_I {switch=sw,precision=precision},switch,ATYmap,RHOmap)
             :: SS_lss(lss,ATYmap,RHOmap))
          | SS_lss'(LS.SWITCH_W {switch,precision}::lss) =
            (SS_sw(SS_lss, fn sw => LS.SWITCH_W {switch=sw,precision=precision},switch,ATYmap,RHOmap)
             :: SS_lss(lss,ATYmap,RHOmap))
          | SS_lss'(LS.SWITCH_S sw::lss) = SS_sw(SS_lss,LS.SWITCH_S,sw,ATYmap,RHOmap) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.SWITCH_C sw::lss) = SS_sw(SS_lss,LS.SWITCH_C,sw,ATYmap,RHOmap) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.SWITCH_E sw::lss) = SS_sw(SS_lss,LS.SWITCH_E,sw,ATYmap,RHOmap) :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.RESET_REGIONS{force,regions_for_resetting}::lss) =
            LS.RESET_REGIONS{force=force,regions_for_resetting=smas_to_smas regions_for_resetting} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.PRIM{name,args,res}::lss) =
            LS.PRIM{name=name,args=atoms_to_atys args,res=atoms_to_atys res} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.CCALL{name,args,rhos_for_result,res}::lss) =
            LS.CCALL{name=name,args=atoms_to_atys args,rhos_for_result=atoms_to_atys rhos_for_result,res=atoms_to_atys res} :: SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.CCALL_AUTO{name,args,rhos_for_result,res}::lss) =
            LS.CCALL_AUTO{name=name,args=map (fn (a,ft) => (atom_to_aty' a,ft)) args,
                          rhos_for_result=atoms_to_atys rhos_for_result,
                          res=case res of (a,ft) => (atom_to_aty' a,ft)} ::
            SS_lss(lss,ATYmap,RHOmap)
          | SS_lss'(LS.EXPORT{name,clos_lab,arg=(a,ft1,ft2)}::lss) =
            LS.EXPORT{name=name,clos_lab=clos_lab,arg=(atom_to_aty' a,ft1,ft2)} ::
            SS_lss(lss,ATYmap,RHOmap)
      in
        SS_lss' lss
      end

    (********************************)
    (* SS on Top Level Declarations *)
    (********************************)
    fun SS_top_decl f =
      let
        fun process (lab,cc,lss) =
          let val lv_env = (foldl (fn ((lv,offset),e) => LvarFinMap.add(lv,STACK_ATY offset,e))
                            LvarFinMap.empty (CallConv.get_spilled_args_with_offsets cc))
              val lss_ss = SS_lss(lss,lv_env,RhoFinMap.empty)
          in (lab,cc,lss_ss)
          end
      in case f
           of LineStmt.FUN f => LineStmt.FUN (process f)
            | LineStmt.FN f => LineStmt.FN (process f)
      end
  in
    fun SS {main_lab:label,
            code=co_prg: (StoreTypeCO,offset,AtomCO) LinePrg,
            imports:label list * label list,
            exports:label list * label list} =
      let
        val _ = chat "[Substitution and Simplification..."
        val line_prg_ss = foldr (fn (func,acc) => SS_top_decl func :: acc) [] co_prg
        val _ =
          if Flags.is_on "print_simplified_program" then
            display("\nReport: AFTER SIMPLIFICATION:", LineStmt.layout_line_prg CalcOffset.pr_sty CalcOffset.pr_offset pr_aty true line_prg_ss)
          else
            ()
        val _ = chat "]\n"
      in
        {main_lab=main_lab,code=line_prg_ss: (StoreTypeCO,offset,Aty) LinePrg,imports=imports,exports=exports}
      end
    fun pr_sty sty = CalcOffset.pr_sty sty
    fun pr_offset offset = CalcOffset.pr_offset offset
  end

end
