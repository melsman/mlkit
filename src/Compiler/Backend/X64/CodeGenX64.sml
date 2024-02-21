(* Generate Target Code *)

functor CodeGenX64(structure BackendInfo : BACKEND_INFO
                     where type label = AddressLabels.label
                   structure JumpTables : JUMP_TABLES
                   structure CallConv: CALL_CONV
                     where type lvar = Lvars.lvar
                   structure LineStmt: LINE_STMT
                     where type con = Con.con
                     where type excon = Excon.excon
                     where type lvar = Lvars.lvar
                     where type label = AddressLabels.label
                     where type place = Effect.effect
                     where type StringTree = PrettyPrint.StringTree
                   sharing type CallConv.cc = LineStmt.cc
                   structure SubstAndSimplify: SUBST_AND_SIMPLIFY
                    where type ('a,'b,'c) LinePrg = ('a,'b,'c) LineStmt.LinePrg
                     where type lvar = Lvars.lvar
                     where type place = Effect.effect
                     where type reg = InstsX64.reg
                     where type label = AddressLabels.label)
    : CODE_GEN =
struct

  structure CodeGenUtil = CodeGenUtilX64(structure BackendInfo = BackendInfo
                                         structure JumpTables = JumpTables
                                         structure CallConv = CallConv
                                         structure LineStmt = LineStmt
                                         structure SubstAndSimplify = SubstAndSimplify)

  open CodeGenUtil

  fun die s  = Crash.impossible ("CodeGenX64." ^ s)

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val extra_gc_checks_p =
      Flags.add_bool_entry
          {long="extra_gc_checks", short=NONE, item=ref false,
           menu=["Compiler", "extra GC checks"], neg=false,
           desc="Insert check for GC even in functions that do not\n\
                \allocate."}

  val alloc_protect_always_p =
      Flags.add_bool_entry
          {long="alloc_protect_always", short=NONE, item=ref false,
           menu=["Compiler", "always protect allocation when parallelism is enabled"], neg=false,
           desc="Always protect allocation when parallelism is enabled. That\n\
                \is, disregard the result of protection inference."}

  val caller_save_regs_ccall = nil(*map RI.lv_to_reg RI.caller_save_ccall_phregs*)
  val callee_save_regs_ccall = map RI.lv_to_reg RI.callee_save_ccall_phregs
  val all_regs = map RI.lv_to_reg RI.all_regs

  val ctx_exnptr_offs = "8"  (* one word offset in Context struct *)

  fun inlineable C =
      case C of
          (i as I.jmp _) :: C => SOME ([i], i :: rem_dead_code C)
        | (i as I.ret) :: C => SOME ([i], i :: rem_dead_code C)
        | (i1 as I.leaq _) :: (i2 as I.ret) :: C => SOME ([i1,i2], i1 :: i2 :: rem_dead_code C)
        | _ => NONE

  local
     (*******************)
     (* Code Generation *)
     (*******************)

     fun CG_lss (lss,size_ff,size_ccf,C) =
       let
         fun pr_ls ls = LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls
         fun CG_ls (ls,C) =
           (case ls
              of LS.ASSIGN{pat=SS.FLOW_VAR_ATY(lv,lab_t,lab_f),
                           bind=LS.CON0{con,con_kind,aux_regions=[],alloc=LS.IGNORE}} =>
                if Con.eq(con,Con.con_TRUE) then I.jmp(L(LocalLab lab_t)) :: rem_dead_code C
                else
                  if Con.eq(con,Con.con_FALSE) then I.jmp(L(LocalLab lab_f)) :: rem_dead_code C
                  else die "CG_lss: unmatched assign on flow variable"
               | LS.ASSIGN{pat,bind} =>
                comment_fn (fn () => "ASSIGN: " ^ pr_ls ls,
                (case bind
                   of LS.ATOM {aty=src_aty} => move_aty_to_aty(src_aty,pat,size_ff,C)
                    | LS.LOAD label => load_from_label(DatLab label,pat,tmp_reg1,size_ff,C)
                    | LS.STORE(src_aty,label) =>
                     (gen_data_lab label;
                      store_in_label(src_aty,DatLab label,tmp_reg1,size_ff,C))
                    | LS.STRING str =>
                     let val string_lab = gen_string_lab str
                     in load_label_addr(string_lab,pat,tmp_reg1,size_ff,C)
                     end
                    | LS.REAL str =>
                     let val float_lab = new_float_lab()
                         val _ =
                           if BI.tag_values() then
                             add_static_data [I.dot_data,
                                              I.dot_align 8,
                                              I.lab float_lab,
                                              I.dot_quad(BI.pr_tag_w(BI.tag_real(true))),
                                              I.dot_double str]
                           else
                             add_static_data [I.dot_data,
                                              I.dot_align 8,
                                              I.lab float_lab,
                                              I.dot_double str]
                     in load_label_addr(float_lab,pat,tmp_reg1,size_ff,C)
                     end
                    | LS.F64 str =>
                      let val (d, C') = resolve_aty_def(pat, tmp_freg0, size_ff, C)
                      in case str of
                             "0.0" => I.xorps (R d, R d) :: C'
                           | _ => let val float_lab = new_float_lab()
                                      val _ = add_static_data [I.dot_data,
                                                               I.dot_align 8,
                                                               I.lab float_lab,
                                                               I.dot_double str]
                                  in I.movq(LA float_lab, R tmp_reg0) ::
                                     I.movsd(D("0", tmp_reg0),R d) :: C'
                                  end
                      end
                    | LS.CLOS_RECORD{label,elems=elems as (lvs,excons,rhos),f64_vars,alloc} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length (LS.smash_free elems)
                         val n_skip = length rhos + 1 (* We don't traverse region pointers
                                                       * or lvars of type f64, which appears first in lvs.
                                                       * i.e. we skip rhos + f64lvs + 1 fields *)
                                      + f64_vars
                     in
                       if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+2,size_ff,
                         store_immed(BI.tag_clos(false,num_elems+1,n_skip), reg_for_result, WORDS 0,

                         load_label_addr(MLFunLab label,SS.PHREG_ATY tmp_reg0,tmp_reg0,size_ff,
                         store_indexed(reg_for_result,WORDS 1,R tmp_reg0,

                         (*store_indexed(reg_for_result,WORDS 1, LA (MLFunLab label),*)
                         #2(foldr (fn (aty,(offset,C)) =>
                                   (offset-1,store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,
                                                                     WORDS offset,size_ff, C)))
                            (num_elems+1,C') (LS.smash_free elems))))))
                       else
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,

                         load_label_addr(MLFunLab label,SS.PHREG_ATY tmp_reg0,tmp_reg0,size_ff,
                         store_indexed(reg_for_result,WORDS 0,R tmp_reg0,

                         (*store_indexed(reg_for_result,WORDS 0, LA (MLFunLab label),*)
                         #2(foldr (fn (aty,(offset,C)) =>
                                   (offset-1,store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,
                                                                     WORDS offset,size_ff, C)))
                            (num_elems,C') (LS.smash_free elems)))))
                     end
                    | LS.REGVEC_RECORD{elems,alloc} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length elems
                     in
                       if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,
                         store_immed(BI.tag_regvec(false,num_elems), reg_for_result, WORDS 0,
                         #2(foldr (fn (sma,(offset,C)) =>
                                   (offset-1,store_sm_in_record(sma,tmp_reg0,reg_for_result,
                                                                WORDS offset,size_ff, C)))
                            (num_elems,C') elems)))
                       else
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems,size_ff,
                         #2(foldr (fn (sma,(offset,C)) =>
                                   (offset-1,store_sm_in_record(sma,tmp_reg0,reg_for_result,
                                                                WORDS offset,size_ff, C)))
                            (num_elems-1,C') elems))
                     end
                    | LS.SCLOS_RECORD{elems=elems as (lvs,excons,rhos),f64_vars,alloc} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length (LS.smash_free elems)
                         val n_skip =
                             length rhos (* We don't traverse region pointers or lvars of type f64,
                                            which appears first in lvs... *)
                             + f64_vars
                     in
                       if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,
                         store_immed(BI.tag_sclos(false,num_elems,n_skip), reg_for_result, WORDS 0,
                         #2(foldr (fn (aty,(offset,C)) =>
                                  (offset-1,store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,
                                                                    WORDS offset,size_ff, C)))
                            (num_elems,C') (LS.smash_free elems))))
                       else
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems,size_ff,
                         #2(foldr (fn (aty,(offset,C)) =>
                                   (offset-1,store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,
                                                                     WORDS offset,size_ff, C)))
                            (num_elems-1,C') (LS.smash_free elems)))
                     end
                    | LS.RECORD{elems=[],alloc,tag,maybeuntag} =>
                     move_aty_to_aty(SS.UNIT_ATY,pat,size_ff,C) (* Unit is unboxed *)
                    | LS.RECORD{elems,alloc,tag,maybeuntag} =>

                             (* Explanation of how we deal with untagged pairs and triples in the presence
                              * of garbage collection and tagging of values in general
                              * - mael 2002-10-14:
                              *
                              * Only pairs and triples that are stored in infinite regions are untagged
                              * - that is, pairs and triples stored in finite regions on the stack
                              * are tagged. Thus, we must be careful to deal
                              * correctly with regions passed to functions at runtime; if a
                              * formal region variable has 'finite' multiplicity, the region
                              * passed at runtime can either be finite or infinite, thus in
                              * this case, the exact layout of the pair is not determined
                              * until runtime.
                              *
                              * When finite regions of type pair is allocated on the stack, a
                              * pair-tag is installed in the stack-slot for the region. The
                              * function alloc_untagged_value_ap_kill_tmp01 returns a pointer to the
                              * object, or a pointer to the word before the object in case the
                              * object represents an untagged pair in an infinite region. *)
                     let
                         val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length elems
                         fun store_elems last_offset =
                             #2(foldr (fn (aty,(offset,C)) =>
                                       (offset-1,store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,
                                                                         WORDS offset,size_ff, C)))
                                (last_offset,C') elems)
                         val _ = if maybeuntag andalso num_elems <> 2 andalso num_elems <> 3 then
                                     die "cannot untag other tuples than pairs and triples"
                                 else ()
                     in
                       if BI.tag_values() andalso maybeuntag andalso not(tag_pairs_p()) then
                         alloc_untagged_value_ap_kill_tmp01 (alloc,reg_for_result,num_elems,size_ff,
                         store_elems num_elems)
                       else if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,
                         store_immed(tag, reg_for_result, WORDS 0,
                         store_elems num_elems))
                       else
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems,size_ff,
                         store_elems (num_elems-1))
                     end
                    | LS.BLOCKF64{elems=[],alloc,tag} =>
                     move_aty_to_aty(SS.UNIT_ATY,pat,size_ff,C) (* Unit is unboxed *)
                    | LS.BLOCKF64{elems,alloc,tag} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length elems
                         fun store_elems last_offset =
                             #2(foldr (fn (aty,(offset,C)) =>
                                       (offset-1,store_aty_in_reg_record(aty,tmp_freg0,reg_for_result,
                                                                         WORDS offset,size_ff, C)))
                                (last_offset,C') elems)
                     in
                       if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,
                         store_immed(tag, reg_for_result, WORDS 0,
                         store_elems num_elems))
                       else
                         (* don't bother to store the tag, but make room for it so that
                          * other operations work; currently, dynamically sized f64blocks are
                          * allocated with allocStringML and subscripting and updating values in such
                          * blocks are shared with subscripting and allocating in statically sized blocks.*)
                         alloc_ap_kill_tmp01(alloc,reg_for_result,num_elems+1,size_ff,
                         store_elems num_elems)
                     end
                    | LS.SCRATCHMEM{bytes=0,alloc,tag} =>
                     move_aty_to_aty(SS.UNIT_ATY,pat,size_ff,C) (* Unit is unboxed *)
                    | LS.SCRATCHMEM{bytes,alloc,tag} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val words = (8-1+bytes)div 8
                     in
                       if BI.tag_values() then
                         alloc_ap_kill_tmp01(alloc,reg_for_result,words+1,size_ff,
                         store_immed(tag, reg_for_result, WORDS 0,
                         C'))
                       else
                         (* don't bother to store the tag, but make room for it so that
                          * other operations work; currently, dynamically sized f64blocks are
                          * allocated with allocStringML and subscripting and updating values in such
                          * blocks are shared with subscripting and allocating in statically sized blocks.*)
                         alloc_ap_kill_tmp01(alloc,reg_for_result,words+1,size_ff,
                         C')
                     end
                    | LS.SELECT(i,aty) =>
                     if BI.tag_values() then
                       move_index_aty_to_aty(aty,pat,WORDS(i+1),tmp_reg1,size_ff,C)
                     else
                       move_index_aty_to_aty(aty,pat,WORDS i,tmp_reg1,size_ff,C)
                    | LS.CON0{con,con_kind,aux_regions,alloc} =>
                       (case con_kind of
                            LS.ENUM i =>
                            let val tag =
                                    if BI.tag_values() orelse (*hack to treat booleans tagged*)
                                       Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE)
                                    then 2*i+1
                                    else i
                                val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                            in move_immed(
                                IntInf.fromInt tag,
                                R reg_for_result,
                                C')
                            end
                          | LS.UNBOXED i =>
                            let val tag = 4*i+3
                                val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                                fun reset_regions C =
                                    foldr (fn (alloc,C) =>
                                              maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C))
                                          C aux_regions
                            in reset_regions(
                                move_immed(IntInf.fromInt tag,
                                           R reg_for_result,
                                           C'))
                            end
                          | LS.UNBOXED_HIGH i =>
                            let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                                fun reset_regions C =
                                    foldr (fn (alloc,C) =>
                                              maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C))
                                          C aux_regions
                            in reset_regions(
                                if i = 0 then
                                  move_immed(IntInf.fromInt(if BI.tag_values() then 1 else 0),
                                             R reg_for_result, C')
                                else
                                  move_immed(IntInf.fromInt i, R reg_for_result,
                                             I.shlq(I "48", R reg_for_result) ::
                                             (if BI.tag_values() then
                                                I.addq(I "1",R reg_for_result) :: C'
                                              else C'))
                              )
                            end
                          | LS.BOXED i =>
                            let val tag = i2s(Word32.toInt(BI.tag_con0(false,i)))
                                val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                                fun reset_regions C =
                                    List.foldr (fn (alloc,C) =>
                                                   maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C))
                                               C aux_regions
                            in reset_regions(
                                alloc_ap_kill_tmp01(alloc,reg_for_result,1,size_ff,
                                                    I.movq(I tag, D("0",reg_for_result)) :: C'))
                            end)
                    | LS.CON1{con,con_kind,alloc,arg} =>
                      (case con_kind of
                           LS.UNBOXED 0 => move_aty_to_aty(arg,pat,size_ff,C)
                         | LS.UNBOXED i =>
                               let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                               in case i
                                    of 1 => move_aty_into_reg(arg,reg_for_result,size_ff,
                                            I.orq(I "1", R reg_for_result) :: C')
                                     | 2 => move_aty_into_reg(arg,reg_for_result,size_ff,
                                            I.orq(I "2", R reg_for_result) :: C')
                                     | _ => die "CG_ls: UNBOXED CON1 with i > 2"
                               end
                         | LS.UNBOXED_HIGH 0 => move_aty_to_aty(arg,pat,size_ff,C)
                         | LS.UNBOXED_HIGH i =>
                               let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                               in move_aty_into_reg(arg,reg_for_result,size_ff,
                                                    I.movq(I (Int.toString i), R tmp_reg0) ::
                                                    I.shlq(I "48", R tmp_reg0) ::
                                                    I.orq(R tmp_reg0, R reg_for_result) :: C')
                               end
                         | LS.BOXED i =>
                               let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                                   val tag = i2s(Word32.toInt(BI.tag_con1(false,i)))
                               in
                                 if SS.eq_aty(pat,arg) then (* We must preserve arg. *)
                                   alloc_ap_kill_tmp01(alloc,tmp_reg1,2,size_ff,
                                   I.movq(I tag, D("0", tmp_reg1)) ::
                                   store_aty_in_reg_record(arg,tmp_reg0,tmp_reg1,WORDS 1,size_ff,
                                   copy(tmp_reg1,reg_for_result,C')))
                                 else
                                   alloc_ap_kill_tmp01(alloc,reg_for_result,2,size_ff,
                                   I.movq(I tag, D("0", reg_for_result)) ::
                                   store_aty_in_reg_record(arg,tmp_reg0,reg_for_result,WORDS 1,size_ff,C'))
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
                             I.movq(I "3", R tmp_reg0) ::
                             I.notq(R tmp_reg0) ::
                             I.andq(R tmp_reg0, R reg_for_result) :: C')
                           end
                         | LS.UNBOXED_HIGH 0 => move_aty_to_aty(con_aty,pat,size_ff,C)
                         | LS.UNBOXED_HIGH _ =>
                           let val (r, F) = resolve_arg_aty(con_aty,tmp_reg0,size_ff)
                               val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                           in
                             if r = reg_for_result then
                               move_aty_into_reg(con_aty,reg_for_result,size_ff,
                               I.movabsq(I "0xFFFFFFFFFFFF", R tmp_reg0) ::
                               I.andq(R tmp_reg0, R reg_for_result) :: C')
                             else
                               I.movabsq(I "0xFFFFFFFFFFFF", R reg_for_result) ::
                               F(I.andq(R r, R reg_for_result) :: C')
                           end
(*
                         | LS.UNBOXED_HIGH _ =>
                           let
                             val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                           in
                             move_aty_into_reg(con_aty,reg_for_result,size_ff,
                             I.movabsq(I "0xFFFFFFFFFFFF", R tmp_reg0) ::
                             I.andq(R tmp_reg0, R reg_for_result) :: C')
                           end
*)
                          | LS.BOXED _ => move_index_aty_to_aty(con_aty,pat,WORDS 1,tmp_reg1,size_ff,C)
                          | _ => die "CG_ls: DECON used with con_kind ENUM")
                    | LS.DEREF {aty} =>
                     let val offset = if BI.tag_values() then 1 else 0
                     in move_index_aty_to_aty(aty,pat,WORDS offset,tmp_reg1,size_ff,C)
                     end
                    | LS.REF(alloc,aty) =>
                     let val offset = if BI.tag_values() then 1 else 0
                         val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         fun maybe_tag_value C =
                           (* tag_pairs_p is false if pairs, tripples, tables and refs are untagged *)
                           if BI.tag_values() andalso tag_pairs_p() then
                             I.movq(I (i2s(Word32.toInt(BI.tag_ref(false)))),
                                    D("0", reg_for_result)) :: C
                           else C
                         fun allocate (reg_for_result,C) =
                           if BI.tag_values() andalso not (tag_pairs_p()) then
                             alloc_untagged_value_ap_kill_tmp01(alloc,reg_for_result,BI.size_of_ref()-1,size_ff,C)
                           else
                             alloc_ap_kill_tmp01(alloc,reg_for_result,BI.size_of_ref(),size_ff,C)
                     in
                       if SS.eq_aty(pat,aty) then (* We must preserve aty *)
                         (*alloc_ap_kill_tmp01(alloc,tmp_reg1,size_of_ref,size_ff, to be removed 2003-08-26, nh*)
                         allocate (tmp_reg1,
                                   store_aty_in_reg_record(aty,tmp_reg0,tmp_reg1,WORDS offset,size_ff,
                                                           copy(tmp_reg1,reg_for_result,maybe_tag_value C')))
                       else
                         (*alloc_ap_kill_tmp01(alloc,reg_for_result,size_of_ref,size_ff,to be removed 2003-08-26, nh*)
                         allocate (reg_for_result,
                                   store_aty_in_reg_record(aty,tmp_reg0,reg_for_result,WORDS offset,size_ff,
                                                           maybe_tag_value C'))
                     end
                    | LS.ASSIGNREF(alloc,aty1,aty2) =>
                     let
                       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                       val offset = if BI.tag_values() then 1 else 0
                     in
                       store_aty_in_aty_record(aty2,aty1,WORDS offset,tmp_reg1,tmp_reg0,size_ff,
                       if BI.tag_values() then
                         move_immed(IntInf.fromInt BI.ml_unit, R reg_for_result,C')
                       else C')
                     end
                    | LS.PASS_PTR_TO_MEM(alloc,i,untagged_value) =>
                     let
                       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                     in
                         (* HACK: When tagging is enabled, only pairs take up 3 words
                          * (of those type of objects that can be returned from a C function) *)
                         (* Hack eliminated: We now pass a boolean which is true for allocations
                          * of tag-free values. mael 2003-05-13 *)
                         if BI.tag_values() andalso not(tag_pairs_p()) andalso untagged_value then
                             alloc_untagged_value_ap_kill_tmp01 (alloc,reg_for_result,i-1,size_ff,C')
                         else
                             alloc_ap_kill_tmp01(alloc,reg_for_result,i,size_ff,C')
                     end
                    | LS.PASS_PTR_TO_RHO {sma=alloc} =>
                     let
                       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                     in
                       prefix_sm(alloc,reg_for_result,size_ff,C')
                     end)
                  ) (* END ASSIGN *)

               | LS.FLUSH(aty,offset) => comment_fn (fn () => "FLUSH: " ^ pr_ls ls,
                                         store_aty_in_reg_record(aty,tmp_reg1,rsp,WORDS(size_ff-offset-1),size_ff,C))
               | LS.FETCH(aty,offset) => comment_fn (fn () => "FETCH: " ^ pr_ls ls,
                                         load_aty_from_reg_record(aty,tmp_reg1,rsp,WORDS(size_ff-offset-1),size_ff,C))
               | LS.FNJMP(cc as {opr,args,clos,res,bv}) =>
                comment_fn (fn () => "FNJMP: " ^ pr_ls ls,
                let
                  val (spilled_args,_) =
                      CallConv.resolve_act_cc {arg_regs=RI.args_phreg, arg_fregs=RI.args_phfreg,
                                               res_regs=RI.res_phreg}
                                              {args=args, clos=clos, reg_args=[], fargs=[], res=res}
                  val offset_codeptr = if BI.tag_values() then "8" else "0"
                in
                  if List.length spilled_args > 0 then
                    CG_ls(LS.FNCALL cc,C)
                  else
                    case opr (* We fetch the addr from the closure and opr points at the closure *)
                      of SS.PHREG_ATY opr_reg =>
                        I.movq(D(offset_codeptr,opr_reg), R tmp_reg1) ::    (* Fetch code label from closure *)
                        base_plus_offset(rsp,WORDS(size_ff+size_ccf),rsp,   (* return label is now at top of stack *)
                        I.jmp(R tmp_reg1) :: rem_dead_code C)
                       | _ =>
                        move_aty_into_reg(opr,tmp_reg1,size_ff,
                        I.movq(D(offset_codeptr,tmp_reg1), R tmp_reg1) ::   (* Fetch code label from closure *)
                        base_plus_offset(rsp,WORDS(size_ff+size_ccf),rsp,   (* return label is now at top of stack *)
                        I.jmp(R tmp_reg1) :: rem_dead_code C))
                end)
               | LS.FNCALL{opr,args,clos,res,bv} =>
                  comment_fn (fn () => "FNCALL: " ^ pr_ls ls,
                  let
                    val offset_codeptr = if BI.tag_values() then "8" else "0"
                    val (spilled_args,spilled_res) =
                        CallConv.resolve_act_cc {arg_regs=RI.args_phreg, arg_fregs=RI.args_phfreg,
                                                 res_regs=RI.res_phreg}
                                                {args=args,clos=clos,reg_args=[],fargs=[],res=res}
                    val size_rcf = length spilled_res
                    val size_ccf = length spilled_args
                    val size_cc = size_rcf+size_ccf+1
(*val _ = if size_cc > 1 then die ("\nfncall: size_ccf: " ^ (Int.toString size_ccf) ^ " and size_rcf: " ^
                                 (Int.toString size_rcf) ^ ".") else () (* debug 2001-01-08, Niels *)*)

                    fun flush_args C =
                      foldr (fn ((aty,offset),C) => push_aty(aty,tmp_reg1,size_ff+offset,C)) C spilled_args
                    (* We pop in reverse order such that size_ff+offset works *)
                    fun fetch_res C =
                      foldr (fn ((aty,offset),C) =>
                             pop_aty(aty,tmp_reg1,size_ff+offset,C)) C (rev spilled_res)
                    fun jmp doit off C =
                      case opr (* We fetch the add from the closure and opr points at the closure *)
                        of SS.PHREG_ATY opr_reg =>
                          I.movq(D(offset_codeptr,opr_reg), R tmp_reg1) ::  (* Fetch code pointer *)
                          doit(R tmp_reg1) :: C
                         | _ =>
                          move_aty_into_reg(opr,tmp_reg1,size_ff+size_cc-off,   (* rsp is now pointing after the call *)
                          I.movq(D(offset_codeptr,tmp_reg1), R tmp_reg1) :: (* convention, i.e., size_ff+size_cc *)
                          doit(R tmp_reg1) :: C)
                    val C' = fetch_res C
                  in
                    base_plus_offset(rsp,WORDS(~size_rcf),rsp,                         (* Move rsp after rcf *)
                    if gc_p() orelse length spilled_args > 0
                    then let val return_lab = new_local_lab "ret_fncall"
                         in I.push(LA return_lab) ::                                       (* Push Return Label *)
                            flush_args(jmp I.jmp 0 (gen_bv(bv, I.lab return_lab :: C')))
                         end
                    else jmp I.call' 1 C')
                  end)
               | LS.JMP(cc as {opr,args,reg_args,clos,fargs,res,bv}) =>
                  comment_fn (fn () => "JMP: " ^ pr_ls ls,
                  let
                  (* The stack looks as follows - growing downwards to the right:
                   *
                   *   ... | ff | rcf | retlab | ccf | ff |
                   *                                     ^sp
                   * To perform a tail call, the arguments that need be passed on the stack
                   * should overwrite the ``| ccf | ff |'' part and the stack pointer
                   * should be adjusted accordingly. However, to compute the new arguments, some of
                   * the values in ``| ccf | ff |'' may be needed. On the other hand, some of the
                   * arguments may be positioned on the stack correctly already.
                   *)
                    val (spilled_args,   (* those arguments that need be passed on the stack *)
                         spilled_res) =  (* those return values that are returned on the stack *)
                        CallConv.resolve_act_cc {arg_regs=RI.args_phreg, arg_fregs=RI.args_phfreg,
                                                 res_regs=RI.res_phreg}
                                                {args=args,clos=clos,reg_args=reg_args,
                                                 fargs=fargs,res=res}

                    val size_rcf = length spilled_res
                    val size_ccf_new = length spilled_args
(*
                    val _ = if size_ccf_new > 0 then
                              print ("** JMP to " ^ Labels.pr_label opr ^ " with\n" ^
                                     "**    size_ccf_new = " ^ Int.toString size_ccf_new ^ "\n" ^
                                     "**    size_ccf = " ^ Int.toString size_ccf ^ "\n" ^
                                     "**    size_ff = " ^ Int.toString size_ff ^ "\n")
                            else ()
*)
                    fun flush_args C =
                      foldr (fn ((aty,offset),C) =>
                             push_aty(aty,tmp_reg1, size_ff + offset - 1 - size_rcf, C)) C spilled_args
                    (* We pop in reverse order such that size_ff+offset works, but we must adjust for the
                     * return label and the return convention frame that we didn't push onto the stack
                     * because we're dealing with a tail call. *)

                  (* After the arguments are pushed onto the stack, we copy them down to
                   * the current ``| ccf | ff |'', which is now dead. *)
                    fun copy_down 0 C = C
                      | copy_down n C = load_indexed(R tmp_reg1, rsp, WORDS (n-1),
                                         store_indexed(rsp, WORDS (size_ff+size_ccf+n-1), R tmp_reg1,
                                          copy_down (n-1) C))
                    fun jmp C = I.jmp(L(MLFunLab opr)) :: rem_dead_code C
                  in
                    flush_args
                    (copy_down size_ccf_new
                     (base_plus_offset(rsp,WORDS(size_ff+size_ccf),rsp,
                                       jmp C)))
                  end)
               | LS.FUNCALL{opr,args,reg_args,clos,fargs,res,bv} =>
                  comment_fn (fn () => "FUNCALL: " ^ pr_ls ls,
                  let
                    val (spilled_args,spilled_res) =
                        CallConv.resolve_act_cc {arg_regs=RI.args_phreg,arg_fregs=RI.args_phfreg,
                                                 res_regs=RI.res_phreg}
                                                {args=args, clos=clos, reg_args=reg_args,
                                                 fargs=fargs, res=res}
                    val size_rcf = List.length spilled_res
                    fun flush_args C =
                      foldr (fn ((aty,offset),C) => push_aty(aty,tmp_reg1,size_ff+offset,C)) C spilled_args
                    (* We pop in reverse order such that size_ff+offset works *)
                    fun fetch_res C =
                      foldr (fn ((aty,offset),C) => pop_aty(aty,tmp_reg1,size_ff+offset,C)) C (rev spilled_res)
                    fun jmp C = I.jmp(L(MLFunLab opr)) :: C
                    val C' = fetch_res C
                  in
                    base_plus_offset(rsp,WORDS(~size_rcf),rsp,                          (* Move rsp after rcf *)
                    if gc_p() orelse length spilled_args > 0
                    then
                      let val return_lab = new_local_lab "ret_funcall"
                      in I.push(LA return_lab) ::                                           (* Push Return Label *)
                         flush_args(jmp(gen_bv(bv, I.lab return_lab :: C')))
                      end
                    else I.call(MLFunLab opr) :: C')
                  end)
               | LS.LETREGION{rhos,body} =>
                  comment ("LETREGION",
                  let
                    fun key place = mkIntAty (Effect.key_of_eps_or_rho place)

                    fun maybe_store_tag (place,offset,C) =
                      if values_in_region_untagged place then
                           let val tag =
                               case Effect.get_place_ty place of
                                   SOME Effect.PAIR_RT => BI.tag_record (false,2)
                                 | SOME Effect.REF_RT => BI.tag_ref(false)
                                 | SOME Effect.TRIPLE_RT => BI.tag_record (false,3)
                                 | _ => die "maybe_store_tag"
                           in store_immed(tag, rsp, WORDS(size_ff-offset-1), C)
                           end
                      else C

                    fun alloc_region_prim (((place,phsize),offset),C) =
                      if region_profiling() then
                        case phsize
                          of LineStmt.WORDS 0 => C (* zero-sized finite region *)
                           | LineStmt.WORDS i =>   (* finite region *)
                            let (* The offset points at the object - not the region descriptor,
                                 * nor the object descriptor; allocRegionFiniteProfiling expects
                                 * a pointer to the region descriptor. See CalcOffset.sml for a
                                 * picture. The size i of the region does not include the sizes
                                 * of the object descriptor and the region descriptor. *)
                              val reg_offset = offset + BI.objectDescSizeP + BI.finiteRegionDescSizeP
                            in
                              base_plus_offset(rsp,WORDS(size_ff-reg_offset-1),tmp_reg1,
                               compile_c_call_prim("allocRegionFiniteProfilingMaybeUnTag",
                                                   [SS.PHREG_ATY tmp_reg1,
                                                    key place,
                                                    mkIntAty i], NONE,
                                                   size_ff,tmp_reg0(*not used*),
                                maybe_store_tag (place,offset,C)))
                            end
                           | LineStmt.INF =>
                            let val name =
                                if regions_holding_values_of_the_same_type_only place then
                                    case Effect.get_place_ty place of
                                        SOME Effect.PAIR_RT => "allocPairRegionInfiniteProfilingMaybeUnTag"
                                      | SOME Effect.REF_RT => "allocRefRegionInfiniteProfilingMaybeUnTag"
                                      | SOME Effect.TRIPLE_RT => "allocTripleRegionInfiniteProfilingMaybeUnTag"
                                      | SOME Effect.ARRAY_RT => "allocArrayRegionInfiniteProfilingMaybeUnTag"
                                      | _ => die "alloc_region_prim.name"
                                else "allocRegionInfiniteProfilingMaybeUnTag"
                            in
                            base_plus_offset(rsp,WORDS(size_ff-offset-1),tmp_reg1,
                              compile_c_call_prim(name,
                                                  [SS.PHREG_ATY I.r14,           (* evaluation context *)
                                                   SS.PHREG_ATY tmp_reg1,
                                                   key place], NONE,
                                                  size_ff,tmp_reg0(*not used*),C))
                            end
                      else
                        case phsize
                          of LineStmt.WORDS 0 => C
                           | LineStmt.WORDS i =>
                              maybe_store_tag (place,offset,C)  (* finite region; no code generated *)
                           | LineStmt.INF =>
                             let val name =
                                     if regions_holding_values_of_the_same_type_only place then
                                       case Effect.get_place_ty place of
                                           SOME Effect.PAIR_RT => "allocatePairRegion"
                                         | SOME Effect.REF_RT => "allocateRefRegion"
                                         | SOME Effect.TRIPLE_RT => "allocateTripleRegion"
                                         | SOME Effect.ARRAY_RT => "allocateArrayRegion"
                                         | _ => die "alloc_region_prim.name2"
                                     else "allocateRegion"
                                 val protect = if parallelism_p() then
                                                 [mkIntAty (if alloc_protect_always_p() then 1
                                                            else case Effect.get_protect place of
                                                                     SOME true => 1
                                                                   | _ => 0)]
                                               else []
                              in
                                  base_plus_offset(rsp,WORDS(size_ff-offset-1),tmp_reg1,
                                    compile_c_call_prim(name,[SS.PHREG_ATY I.r14, SS.PHREG_ATY tmp_reg1] @ protect,NONE,
                                                        size_ff,tmp_reg0(*not used*),C))
                              end
                    fun dealloc_region_prim (((place,phsize),offset),C) =
                      if region_profiling() then
                        case phsize
                          of LineStmt.WORDS 0 => C
                           | LineStmt.WORDS i =>
                            compile_c_call_prim("deallocRegionFiniteProfiling",[],NONE,
                                                size_ff,tmp_reg0(*not used*),C)
                           | LineStmt.INF =>
                            compile_c_call_prim("deallocateRegion",[SS.PHREG_ATY I.r14],NONE,size_ff,tmp_reg0(*not used*),C)
                      else
                        case phsize
                          of LineStmt.WORDS i => C
                           | LineStmt.INF =>
                            compile_c_call_prim("deallocateRegion",[SS.PHREG_ATY I.r14],NONE,size_ff,tmp_reg0(*not used*),C)
                  in
                    foldr alloc_region_prim
                    (CG_lss(body,size_ff,size_ccf,
                            foldl dealloc_region_prim C rhos)) rhos
                  end )
               | LS.SCOPE{pat,scope} => CG_lss(scope,size_ff,size_ccf,C)
               | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=(handl_return,handl_return_aty,bv),offset} =>
    (* An exception handler in an activation record starting at address offset contains the following fields: *)
    (* sp[offset] = label for handl_return code.                                                              *)
    (* sp[offset+1] = pointer to handle closure.                                                              *)
    (* sp[offset+2] = pointer to previous exception handler used when updating exnPtr.                        *)
    (* sp[offset+3] = address of the first cell after the activation record used when resetting sp.           *)
    (* Note that we call deallocate_regions_until to the address above the exception handler, (i.e., some of  *)
    (* the infinite regions inside the activation record are also deallocated)!                               *)
                 let
                    val handl_return_lab = new_local_lab "handl_return"
                    val handl_join_lab = new_local_lab "handl_join"
                    fun handl_code C = comment ("HANDL_CODE", CG_lss(handl,size_ff,size_ccf,C))
                    fun store_handl_lv C =
                      comment ("STORE HANDLE_LV: sp[offset+1] = handl_lv",
                      store_aty_in_reg_record(handl_lv,tmp_reg1,rsp,WORDS(size_ff-offset-1+1),size_ff,C))
                    fun store_handl_return_lab C =
                      comment ("STORE HANDL RETURN LAB: sp[offset] = handl_return_lab",
                      I.movq(LA handl_return_lab, R tmp_reg1) ::
                      store_indexed(rsp,WORDS(size_ff-offset-1), R tmp_reg1,C))
                    fun store_exn_ptr C =
                      comment ("STORE EXN PTR: sp[offset+2] = exnPtr",
                      I.movq(D(ctx_exnptr_offs,r14), R tmp_reg1) ::
                      store_indexed(rsp,WORDS(size_ff-offset-1+2), R tmp_reg1,
                      comment ("CALC NEW exnPtr: exnPtr = sp-size_ff+offset+size_of_handle",
                      base_plus_offset(rsp,WORDS(size_ff-offset-1(*-BI.size_of_handle()*)),tmp_reg1,        (*hmmm *)
                      I.movq(R tmp_reg1, D(ctx_exnptr_offs,r14)) ::
                      C))))
                    fun store_sp C =
                      comment ("STORE SP: sp[offset+3] = sp",
                      store_indexed(rsp,WORDS(size_ff-offset-1+3), R rsp,C))
                    fun default_code C = comment ("HANDLER DEFAULT CODE",
                      CG_lss(default,size_ff,size_ccf,C))
                    fun restore_exn_ptr C =
                      comment ("RESTORE EXN PTR: exnPtr = sp[offset+2]",
                      load_indexed(R tmp_reg1,rsp,WORDS(size_ff-offset-1+2),
                      I.movq(R tmp_reg1, D(ctx_exnptr_offs,r14)) ::
                      I.jmp(L handl_join_lab) ::C))
                    fun handl_return_code C =
                      let val res_reg = RI.lv_to_reg(CallConv.handl_return_phreg RI.res_phreg)
                      in comment ("HANDL RETURN CODE: handl_return_aty = res_phreg",
                         gen_bv(bv,
                         I.lab handl_return_lab ::
                         move_aty_to_aty(SS.PHREG_ATY res_reg,handl_return_aty,size_ff,
                         CG_lss(handl_return,size_ff,size_ccf,
                         I.lab handl_join_lab :: C))))
                      end
                  in
                    comment ("START OF EXCEPTION HANDLER",
                    handl_code(
                    store_handl_lv(
                    store_handl_return_lab(
                    store_exn_ptr(
                    store_sp(
                    default_code(
                    restore_exn_ptr(
                    handl_return_code(comment ("END OF EXCEPTION HANDLER", C))))))))))
                  end
               | LS.RAISE{arg=arg_aty,defined_atys} =>
                  move_aty_into_reg(arg_aty,rsi,size_ff,      (* arg1: context, arg2: exception value *)        (* function never returns *)
                  maybe_align 0 (fn C => I.movq(R r14, R rdi) :: I.call (NameLab "raise_exn") :: rem_dead_code C) C)
               | LS.SWITCH_I{switch=LS.SWITCH(SS.FLOW_VAR_ATY(lv,lab_t,lab_f),[(sel_val,lss)],default),
                             precision} =>
                  let
                    val (t_lab,f_lab) = if sel_val = IntInf.fromInt BI.ml_true then (lab_t,lab_f)
                                        else (lab_f,lab_t)
                    val lab_exit = new_local_lab "done"
                  in
                    I.lab(LocalLab t_lab) ::
                    CG_lss(lss,size_ff,size_ccf,
                    I.jmp(L lab_exit) ::
                    I.lab(LocalLab f_lab) ::
                    CG_lss(default,size_ff,size_ccf,
                    I.lab(lab_exit) :: C))
                  end
               | LS.SWITCH_I {switch=LS.SWITCH(opr_aty,sels,default), precision} =>
                  compileNumSwitch {size_ff=size_ff,
                                    size_ccf=size_ccf,
                                    CG_lss=CG_lss,
                                    toInt=fn i => maybeTagIntOrWord{value=i, precision=precision},
                                    opr_aty=opr_aty,
                                    oprBoxed=boxedNum precision,
                                    sels=sels,
                                    default=default,
                                    C=C}
               | LS.SWITCH_W {switch=LS.SWITCH(opr_aty,sels,default), precision} =>
                  compileNumSwitch {size_ff=size_ff,
                                    size_ccf=size_ccf,
                                    CG_lss=CG_lss,
                                    toInt=fn w => maybeTagIntOrWord{value=w, precision=precision},
                                    opr_aty=opr_aty,
                                    oprBoxed=boxedNum precision,
                                    sels=sels,
                                    default=default,
                                    C=C}
               | LS.SWITCH_S sw => die "SWITCH_S is unfolded in ClosExp"
               | LS.SWITCH_C(LS.SWITCH(SS.FLOW_VAR_ATY(lv,lab_t,lab_f),[((con,con_kind),lss)],default)) =>
                  let
                    val (t_lab,f_lab) = if Con.eq(con,Con.con_TRUE) then (lab_t,lab_f) else (lab_f,lab_t)
                  in
                    case inlineable C of
                        SOME (C,C') =>
                        I.lab(LocalLab t_lab) ::
                        CG_lss(lss,size_ff,size_ccf,
                               C @ I.lab(LocalLab f_lab) ::
                               CG_lss(default,size_ff,size_ccf, C'))
                      | NONE =>
                        let val lab_exit = new_local_lab "done"
                        in I.lab(LocalLab t_lab) ::
                           CG_lss(lss,size_ff,size_ccf,
                                  I.jmp(L lab_exit) ::
                                  I.lab(LocalLab f_lab) ::
                                  CG_lss(default,size_ff,size_ccf,
                                         I.lab lab_exit :: C))
                        end
                  end
               | LS.SWITCH_C(LS.SWITCH(opr_aty,[],default)) => CG_lss(default,size_ff,size_ccf,C)
               | LS.SWITCH_C(LS.SWITCH(opr_aty,sels,default)) =>
                  let (* NOTE: selectors in sels are tagged in ClosExp; values are
                       * tagged here in CodeGenX64! *)
                    (* if there are 1 or fewer nullary constructors, we can save some instructions *)
                    val (con_kind, lte1_nullary) =
                        case sels of [] => die ("CG_ls: SWITCH_C sels is empty: " ^ (pr_ls ls))
                                   | ((con,con_kind),_)::rest => (con_kind,
                                                                  Con.eq(Con.con_NIL,con) orelse
                                                                  Con.eq(Con.con_CONS,con))
                    val sels' = map (fn ((con,con_kind),sel_insts) =>
                                     case con_kind
                                       of LS.ENUM i => (IntInf.fromInt i,sel_insts)
                                        | LS.UNBOXED i => (IntInf.fromInt i,sel_insts)
                                        | LS.UNBOXED_HIGH i => (IntInf.fromInt i,sel_insts)
                                        | LS.BOXED i => (IntInf.fromInt i,sel_insts)) sels
                    fun UbTagCon (src_aty,C) =
                        if lte1_nullary then
                          (move_aty_into_reg(src_aty,tmp_reg1,size_ff,
                           I.andq(I "3", R tmp_reg1) ::
                           C))
                        else
                          (move_aty_into_reg(src_aty,tmp_reg0,size_ff,
                           copy(tmp_reg0, tmp_reg1, (* operand is in tmp_reg1, see SWITCH_I *)
                           I.andq(I "3", R tmp_reg1) ::
                           I.cmpq(I "3", R tmp_reg1) ::         (* do copy if tr = 3; in that case we      *)
                           I.cmoveq(R tmp_reg0,R tmp_reg1) ::   (* are dealing with a nullary constructor, *)
                           C)))                                 (* and all bits are used. *)

                    fun UbhTagCon (src_aty,C) =
                        move_aty_into_reg(src_aty,tmp_reg1,size_ff,
                        I.shrq(I "48", R tmp_reg1) ::
                        C)

                    val (F, opr_aty) =
                      case con_kind
                        of LS.ENUM _ => (fn C => C, opr_aty)
                         | LS.UNBOXED _ => (fn C => UbTagCon(opr_aty,C), SS.PHREG_ATY tmp_reg1)
                         | LS.UNBOXED_HIGH _ => (fn C => UbhTagCon(opr_aty,C), SS.PHREG_ATY tmp_reg1)
                         | LS.BOXED _ =>
                          (fn C => move_index_aty_to_aty(opr_aty,SS.PHREG_ATY tmp_reg1,
                                                         WORDS 0,tmp_reg1,size_ff,C),
                           SS.PHREG_ATY tmp_reg1)
                  in
                    F (compileNumSwitch {size_ff=size_ff,
                                         size_ccf=size_ccf,
                                         CG_lss=CG_lss,
                                         toInt=fn x => x,   (* tagging already done in ClosExp *)
                                         opr_aty=opr_aty,
                                         oprBoxed=false,
                                         sels=sels',
                                         default=default,
                                         C=C})
                  end
               | LS.SWITCH_E sw => die "SWITCH_E is unfolded in ClosExp"
               | LS.RESET_REGIONS{force=false,regions_for_resetting} =>
                  comment ("RESET_REGIONS(no force)",
                  foldr (fn (alloc,C) => maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C)) C regions_for_resetting)
               | LS.RESET_REGIONS{force=true,regions_for_resetting} =>
                  comment ("RESET_REGIONS(force)",
                  foldr (fn (alloc,C) => force_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C)) C regions_for_resetting)
               | LS.PRIM{name,args,res=[SS.FLOW_VAR_ATY(lv,lab_t,lab_f)]} =>
                  comment_fn (fn () => "PRIM FLOW: " ^ pr_ls ls,
                  let val (lab_t,lab_f) = (LocalLab lab_t,LocalLab lab_f)
                      val (x,y) = case args of
                                      [x,y] => (x,y)
                                    | _ => die "CG_ls: Expecting two arguments for flow primitive"
                      fun cmp i = cmpi_and_jmp_kill_tmp01 {quad=false} (i,x,y,lab_t,lab_f,size_ff,C)
                      fun cmp_boxed i = cmpbi_and_jmp_kill_tmp01 {quad=false} (i,x,y,lab_t,lab_f,size_ff,C)
                      fun cmp_quad i = cmpi_and_jmp_kill_tmp01 {quad=true} (i,x,y,lab_t,lab_f,size_ff,C)
                      fun cmp_boxed_quad i = cmpbi_and_jmp_kill_tmp01 {quad=true} (i,x,y,lab_t,lab_f,size_ff,C)
                      fun cmpf64 i = cmpf64_and_jmp(i,x,y,lab_t,lab_f,size_ff,C)
                      open PrimName
                  in case name of
                         Equal_int32ub =>  cmp       I.je
                       | Equal_int32b =>   cmp_boxed I.je
                       | Equal_int31 =>    cmp       I.je
                       | Equal_word31 =>   cmp       I.je
                       | Equal_word32ub => cmp       I.je
                       | Equal_word32b =>  cmp_boxed I.je

                       | Equal_int64ub =>  cmp_quad       I.je
                       | Equal_int64b =>   cmp_boxed_quad I.je
                       | Equal_int63 =>    cmp_quad       I.je
                       | Equal_word63 =>   cmp_quad       I.je
                       | Equal_word64ub => cmp_quad       I.je
                       | Equal_word64b =>  cmp_boxed_quad I.je
                       | Equal_ptr =>      cmp_quad       I.je

                       | Less_int32ub =>    cmp       I.jl
                       | Less_int32b =>     cmp_boxed I.jl
                       | Less_int31 =>      cmp       I.jl
                       | Less_word31 =>     cmp       I.jb
                       | Less_word32ub =>   cmp       I.jb
                       | Less_word32b =>    cmp_boxed I.jb

                       | Less_int64ub =>    cmp_quad       I.jl
                       | Less_int64b =>     cmp_boxed_quad I.jl
                       | Less_int63 =>      cmp_quad       I.jl
                       | Less_word63 =>     cmp_quad       I.jb
                       | Less_word64ub =>   cmp_quad       I.jb
                       | Less_word64b =>    cmp_boxed_quad I.jb

                       | Lesseq_int32ub =>  cmp       I.jle
                       | Lesseq_int32b =>   cmp_boxed I.jle
                       | Lesseq_int31 =>    cmp       I.jle
                       | Lesseq_word31 =>   cmp       I.jbe
                       | Lesseq_word32ub => cmp       I.jbe
                       | Lesseq_word32b =>  cmp_boxed I.jbe

                       | Lesseq_int64ub =>  cmp_quad       I.jle
                       | Lesseq_int64b =>   cmp_boxed_quad I.jle
                       | Lesseq_int63 =>    cmp_quad       I.jle
                       | Lesseq_word63 =>   cmp_quad       I.jbe
                       | Lesseq_word64ub => cmp_quad       I.jbe
                       | Lesseq_word64b =>  cmp_boxed_quad I.jbe

                       | Greater_int32ub =>  cmp       I.jg
                       | Greater_int32b =>   cmp_boxed I.jg
                       | Greater_int31 =>    cmp       I.jg
                       | Greater_word31 =>   cmp       I.ja
                       | Greater_word32ub => cmp       I.ja
                       | Greater_word32b =>  cmp_boxed I.ja

                       | Greater_int64ub =>  cmp_quad       I.jg
                       | Greater_int64b =>   cmp_boxed_quad I.jg
                       | Greater_int63 =>    cmp_quad       I.jg
                       | Greater_word63 =>   cmp_quad       I.ja
                       | Greater_word64ub => cmp_quad       I.ja
                       | Greater_word64b =>  cmp_boxed_quad I.ja

                       | Greatereq_int32ub =>  cmp       I.jge
                       | Greatereq_int32b =>   cmp_boxed I.jge
                       | Greatereq_int31 =>    cmp       I.jge
                       | Greatereq_word31 =>   cmp       I.jae
                       | Greatereq_word32ub => cmp       I.jae
                       | Greatereq_word32b =>  cmp_boxed I.jae

                       | Greatereq_int64ub =>  cmp_quad       I.jge
                       | Greatereq_int64b =>   cmp_boxed_quad I.jge
                       | Greatereq_int63 =>    cmp_quad       I.jge
                       | Greatereq_word63 =>   cmp_quad       I.jae
                       | Greatereq_word64ub => cmp_quad       I.jae
                       | Greatereq_word64b =>  cmp_boxed_quad I.jae

                       | Less_f64 => cmpf64 I.jb
                       | Lesseq_f64 => cmpf64 I.jbe
                       | Greater_f64 => cmpf64 I.ja
                       | Greatereq_f64 => cmpf64 I.jae
                       | _ => die "CG_ls: Unsupported PRIM used with Flow Variable"
                  end)
               | LS.PRIM{name,args,res} =>
                 let val d = case res of nil => SS.UNIT_ATY
                                       | [d] => d
                                       | _ => die "CG_ls: expecting at most a single return value for primitive"
                     open PrimName
                 in
                  comment_fn (fn () => "PRIM: " ^ pr_ls ls,
                  (* Note that the prim names are defined in BackendInfo! *)
                  (case args of
                       [] =>
                       (case name of
                            Fresh_exname =>  (* result in tmp_reg0 *)
                            I.movq(LA exn_counter_lab, R tmp_reg1) ::
                            I.movq(I "1", R tmp_reg0) ::
                            I.xaddq(R tmp_reg0, D("0",tmp_reg1)) ::
                            move_reg_into_aty(tmp_reg0,d,size_ff,C)
                         | Get_ctx =>
                           move_reg_into_aty(r14,d,size_ff,C)
                         | _ => die ("unsupported prim with 0 args: " ^ PrimName.pp_prim name))
                     | [x] =>
                       let val arg = (x,d,size_ff,C)
                       in case name of
                              Neg_int32ub => neg_int_kill_tmp0 {tag=false, quad=false} arg
                            | Neg_int31 =>   neg_int_kill_tmp0 {tag=true,  quad=false} arg
                            | Neg_int64ub => neg_int_kill_tmp0 {tag=false, quad=true}  arg
                            | Neg_int63 =>   neg_int_kill_tmp0 {tag=true,  quad=true}  arg

                            | Abs_int32ub => abs_int_kill_tmp0 {tag=false, quad=false} arg
                            | Abs_int31 =>   abs_int_kill_tmp0 {tag=true,  quad=false} arg
                            | Abs_int64ub => abs_int_kill_tmp0 {tag=false, quad=true}  arg
                            | Abs_int63 =>   abs_int_kill_tmp0 {tag=true,  quad=true}  arg

                            | Int31_to_int32ub     => num31_to_num32ub arg
                            | Int32b_to_int31      => int32_to_int31 {boxedarg=true} arg
                            | Int32ub_to_int31     => int32_to_int31 {boxedarg=false} arg
                            | Word31_to_word32ub   => num31_to_num32ub arg
                            | Word32b_to_word31    => word32_to_word31 {boxedarg=true} arg
                            | Word32ub_to_word31   => word32_to_word31 {boxedarg=false} arg
                            | Word31_to_word32ub_X => num31_to_num32ub arg
                            | Word32ub_to_int32ub  => word32ub_to_int32ub arg
                            | Word32b_to_int31     => word32_to_int31 {boxedarg=true,ovf=true} arg
                            | Int32b_to_word31     => word32_to_word31 {boxedarg=true} arg
                            | Word32b_to_int31_X   => word32_to_int31 {boxedarg=true,ovf=false} arg

                            | Word64ub_to_word32ub => word64ub_to_word32ub arg
                            | Word32ub_to_word64ub => word32ub_to_word64ub {signext=false} arg
                            | Word64ub_to_int32ub  => word64ub_to_int32ub arg
                            | Word64ub_to_int64ub  => num64ub_to_num64ub {ovf=true} arg
                            | Word64ub_to_int64ub_X => num64ub_to_num64ub {ovf=false} arg

                            | Word64b_to_int31     => word64_to_int31 {boxedarg=true,ovf=true} arg
                            | Word64b_to_int31_X   => word64_to_int31 {boxedarg=true,ovf=false} arg

                            | Int32ub_to_int64ub   => int32ub_to_int64ub arg
                            | Int64ub_to_word64ub  => num64ub_to_num64ub {ovf=false} arg

                            | Int64b_to_int31      => int64_to_int31 {boxedarg=true} arg

                            | Int64ub_to_int32ub   => int64ub_to_int32ub arg

                            | Int31_to_int64ub     => int31_to_int64ub arg

                            | Int64b_to_int63      => int64_to_int63 {boxedarg=true} arg
                            | Word64b_to_word63    => word64_to_word63 {boxedarg=true} arg
                            | Word64ub_to_word63   => word64_to_word63 {boxedarg=false} arg
                            | Word32b_to_word63    => word32_to_word63 {boxedarg=true,signext=false} arg
                            | Word63_to_word31     => word63_to_word31 arg
                            | Word31_to_word63_X   => num31_to_num63 {signext=true} arg
                            | Word31_to_word63     => num31_to_num63 {signext=false} arg

                            | Int32b_to_int63      => word32_to_word63 {boxedarg=true,signext=true} arg
                            | Int63_to_int31       => int63_to_int31 arg
                            | Int31_to_int63       => num31_to_num63 {signext=true} arg
                            | Word32b_to_int63     => word32_to_word63 {boxedarg=true,signext=false} arg
                            | Word32b_to_int63_X   => word32_to_word63 {boxedarg=true,signext=true} arg
                            | Word64b_to_word31    => word64_to_word31 {boxedarg=true} arg

                            | Word64b_to_int63     => word64_to_int63 {boxedarg=true,ovf=true} arg
                            | Word64b_to_int63_X   => word64_to_int63 {boxedarg=true,ovf=false} arg

                            | Int63_to_int64ub     => num63_to_num64ub {shr_inst=I.sarq} arg

                            | Int64ub_to_int63     => int64_to_int63 {boxedarg=false} arg

                            | Word63_to_word64ub   => num63_to_num64ub {shr_inst=I.shrq} arg
                            | Word63_to_word64ub_X => num63_to_num64ub {shr_inst=I.sarq} arg

                            | Word64ub_to_word31   => word64_to_word31 {boxedarg=false} arg
                            | Int64ub_to_int31     => int64_to_int31 {boxedarg=false} arg

                            | Word31_to_word64ub   => word31_to_word64ub {signext=false} arg
                            | Word31_to_word64ub_X => word31_to_word64ub {signext=true} arg

                            | Word32ub_to_int64ub  => word32ub_to_word64ub {signext=false} arg
                            | Word32ub_to_int64ub_X => word32ub_to_word64ub {signext=true} arg
                            | Word32ub_to_word64ub_X => word32ub_to_word64ub {signext=true} arg

                            | Bytetable_size => bytetable_size arg
                            | Table_size     => table_size arg

                            | Real_to_f64    => real_to_f64 arg
                            | Sqrt_f64       => sqrt_f64 arg
                            | Neg_f64        => neg_f64 arg
                            | Abs_f64        => abs_f64 arg
                            | Int_to_f64     => int_to_f64 arg
                            | Blockf64_size  => blockf64_size arg

                            | Is_null => cmpi_kill_tmp01_cmov {box=false,quad=false} I.cmoveq
                                                              (x, SS.INTEGER_ATY{value=IntInf.fromInt 0,
                                                                                 precision=32},d,size_ff,C)
                            | _ => die ("unsupported prim with 1 arg: " ^ PrimName.pp_prim name)
                       end
                     | [x,y] =>
                       let val arg = (x,y,d,size_ff,C)
                       in case name of
                              Equal_int32ub =>  cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmoveq arg
                            | Equal_int32b =>   cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmoveq arg
                            | Equal_int31 =>    cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmoveq arg
                            | Equal_word31 =>   cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmoveq arg
                            | Equal_word32ub => cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmoveq arg
                            | Equal_word32b =>  cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmoveq arg
                            | Equal_int64ub =>  cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmoveq arg
                            | Equal_int64b =>   cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmoveq arg
                            | Equal_int63 =>    cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmoveq arg
                            | Equal_word63 =>   cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmoveq arg
                            | Equal_word64ub => cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmoveq arg
                            | Equal_word64b =>  cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmoveq arg
                            | Equal_ptr =>      cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmoveq arg

                            | Plus_int32ub =>  add_num_kill_tmp01 {ovf=true,  tag=false, quad=false} arg
                            | Plus_int31 =>    add_num_kill_tmp01 {ovf=true,  tag=true,  quad=false} arg
                            | Plus_word31 =>   add_num_kill_tmp01 {ovf=false, tag=true,  quad=false} arg
                            | Plus_word32ub => add_num_kill_tmp01 {ovf=false, tag=false, quad=false} arg
                            | Plus_int64ub =>  add_num_kill_tmp01 {ovf=true,  tag=false, quad=true}  arg
                            | Plus_int63 =>    add_num_kill_tmp01 {ovf=true,  tag=true,  quad=true}  arg
                            | Plus_word63 =>   add_num_kill_tmp01 {ovf=false, tag=true,  quad=true}  arg
                            | Plus_word64ub => add_num_kill_tmp01 {ovf=false, tag=false, quad=true}  arg

                            | Minus_int32ub =>  sub_num_kill_tmp01 {ovf=true,  tag=false, quad=false} arg
                            | Minus_int31 =>    sub_num_kill_tmp01 {ovf=true,  tag=true,  quad=false} arg
                            | Minus_word31 =>   sub_num_kill_tmp01 {ovf=false, tag=true,  quad=false} arg
                            | Minus_word32ub => sub_num_kill_tmp01 {ovf=false, tag=false, quad=false} arg
                            | Minus_int64ub =>  sub_num_kill_tmp01 {ovf=true,  tag=false, quad=true}  arg
                            | Minus_int63 =>    sub_num_kill_tmp01 {ovf=true,  tag=true,  quad=true}  arg
                            | Minus_word63 =>   sub_num_kill_tmp01 {ovf=false, tag=true,  quad=true}  arg
                            | Minus_word64ub => sub_num_kill_tmp01 {ovf=false, tag=false, quad=true}  arg

                            | Mul_int32ub =>  mul_num_kill_tmp01 {ovf=true,  tag=false, quad=false} arg
                            | Mul_int31 =>    mul_num_kill_tmp01 {ovf=true,  tag=true,  quad=false} arg
                            | Mul_word31 =>   mul_num_kill_tmp01 {ovf=false, tag=true,  quad=false} arg
                            | Mul_word32ub => mul_num_kill_tmp01 {ovf=false, tag=false, quad=false} arg
                            | Mul_int64ub =>  mul_num_kill_tmp01 {ovf=true,  tag=false, quad=true}  arg
                            | Mul_int63 =>    mul_num_kill_tmp01 {ovf=true,  tag=true,  quad=true}  arg
                            | Mul_word63 =>   mul_num_kill_tmp01 {ovf=false, tag=true,  quad=true}  arg
                            | Mul_word64ub => mul_num_kill_tmp01 {ovf=false, tag=false, quad=true}  arg

                            | Neg_int32b => neg_int_boxed_kill_tmp0 {quad=false} arg
                            | Neg_int64b => neg_int_boxed_kill_tmp0 {quad=true} arg
                            | Neg_real =>   negf_kill_tmp01 arg
                            | Abs_int32b => abs_int_boxed_kill_tmp0 {quad=false} arg
                            | Abs_int64b => abs_int_boxed_kill_tmp0 {quad=true} arg
                            | Abs_real =>   absf_kill_tmp01 arg

                            | Less_int32ub =>  cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovlq arg
                            | Less_int32b =>   cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovlq arg
                            | Less_int31 =>    cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovlq arg
                            | Less_word31 =>   cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovbq arg
                            | Less_word32ub => cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovbq arg
                            | Less_word32b =>  cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovbq arg
                            | Less_int64ub =>  cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovlq arg
                            | Less_int64b =>   cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovlq arg
                            | Less_int63 =>    cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovlq arg
                            | Less_word63 =>   cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovbq arg
                            | Less_word64ub => cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovbq arg
                            | Less_word64b =>  cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovbq arg

                            | Less_real => cmpf_kill_tmp01_cmov I.cmovbq arg
                            | Less_f64 => cmpf64_kill_tmp01_cmov I.cmovbq arg

                            | Lesseq_int32ub =>  cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovleq arg
                            | Lesseq_int32b =>   cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovleq arg
                            | Lesseq_int31 =>    cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovleq arg
                            | Lesseq_word31 =>   cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovbeq arg
                            | Lesseq_word32ub => cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovbeq arg
                            | Lesseq_word32b =>  cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovbeq arg
                            | Lesseq_int64ub =>  cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovleq arg
                            | Lesseq_int64b =>   cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovleq arg
                            | Lesseq_int63 =>    cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovleq arg
                            | Lesseq_word63 =>   cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovbeq arg
                            | Lesseq_word64ub => cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovbeq arg
                            | Lesseq_word64b =>  cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovbeq arg

                            | Lesseq_real => cmpf_kill_tmp01_cmov I.cmovbeq arg
                            | Lesseq_f64 => cmpf64_kill_tmp01_cmov I.cmovbeq arg

                            | Greater_int32ub =>  cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovgq arg
                            | Greater_int32b =>   cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovgq arg
                            | Greater_int31 =>    cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovgq arg
                            | Greater_word31 =>   cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovaq arg
                            | Greater_word32ub => cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovaq arg
                            | Greater_word32b =>  cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovaq arg
                            | Greater_int64ub =>  cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovgq arg
                            | Greater_int64b =>   cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovgq arg
                            | Greater_int63 =>    cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovgq arg
                            | Greater_word63 =>   cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovaq arg
                            | Greater_word64ub => cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovaq arg
                            | Greater_word64b =>  cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovaq arg

                            | Greater_real => cmpf_kill_tmp01_cmov I.cmovaq arg
                            | Greater_f64 => cmpf64_kill_tmp01_cmov I.cmovaq arg

                            | Greatereq_int32ub =>  cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovgeq arg
                            | Greatereq_int32b =>   cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovgeq arg
                            | Greatereq_int31 =>    cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovgeq arg
                            | Greatereq_word31 =>   cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovaeq arg
                            | Greatereq_word32ub => cmpi_kill_tmp01_cmov {box=false, quad=false} I.cmovaeq arg
                            | Greatereq_word32b =>  cmpi_kill_tmp01_cmov {box=true,  quad=false} I.cmovaeq arg
                            | Greatereq_int64ub =>  cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovgeq arg
                            | Greatereq_int64b =>   cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovgeq arg
                            | Greatereq_int63 =>    cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovgeq arg
                            | Greatereq_word63 =>   cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovaeq arg
                            | Greatereq_word64ub => cmpi_kill_tmp01_cmov {box=false, quad=true}  I.cmovaeq arg
                            | Greatereq_word64b =>  cmpi_kill_tmp01_cmov {box=true,  quad=true}  I.cmovaeq arg

                            | Greatereq_real => cmpf_kill_tmp01_cmov I.cmovaeq arg
                            | Greatereq_f64 => cmpf64_kill_tmp01_cmov I.cmovaeq arg

                            | Andb_word31 =>   andb_word_kill_tmp01 {quad=false} arg
                            | Andb_word32ub => andb_word_kill_tmp01 {quad=false} arg
                            | Andb_word63 =>   andb_word_kill_tmp01 {quad=true}  arg
                            | Andb_word64ub => andb_word_kill_tmp01 {quad=true}  arg

                            | Orb_word31 =>   orb_word_kill_tmp01 {quad=false} arg
                            | Orb_word32ub => orb_word_kill_tmp01 {quad=false} arg
                            | Orb_word63 =>   orb_word_kill_tmp01 {quad=true}  arg
                            | Orb_word64ub => orb_word_kill_tmp01 {quad=true}  arg

                            | Xorb_word31 =>   xorb_word_kill_tmp01 {tag=true,  quad=false} arg
                            | Xorb_word32ub => xorb_word_kill_tmp01 {tag=false, quad=false} arg
                            | Xorb_word63 =>   xorb_word_kill_tmp01 {tag=true,  quad=true}  arg
                            | Xorb_word64ub => xorb_word_kill_tmp01 {tag=false, quad=true}  arg

                            | Shift_left_word31 =>   shift_left_word_kill_tmp01 {tag=true,  quad=false} arg
                            | Shift_left_word32ub => shift_left_word_kill_tmp01 {tag=false, quad=false} arg
                            | Shift_left_word63 =>   shift_left_word_kill_tmp01 {tag=true,  quad=true}  arg
                            | Shift_left_word64ub => shift_left_word_kill_tmp01 {tag=false, quad=true}  arg

                            | Shift_right_signed_word31 =>   shift_right_signed_word_kill_tmp01 {tag=true,  quad=false} arg
                            | Shift_right_signed_word32ub => shift_right_signed_word_kill_tmp01 {tag=false, quad=false} arg
                            | Shift_right_signed_word63 =>   shift_right_signed_word_kill_tmp01 {tag=true,  quad=true}  arg
                            | Shift_right_signed_word64ub => shift_right_signed_word_kill_tmp01 {tag=false, quad=true}  arg

                            | Shift_right_unsigned_word31 =>   shift_right_unsigned_word_kill_tmp01 {tag=true,  quad=false} arg
                            | Shift_right_unsigned_word32ub => shift_right_unsigned_word_kill_tmp01 {tag=false, quad=false} arg
                            | Shift_right_unsigned_word63 =>   shift_right_unsigned_word_kill_tmp01 {tag=true,  quad=true}  arg
                            | Shift_right_unsigned_word64ub => shift_right_unsigned_word_kill_tmp01 {tag=false, quad=true}  arg

                            | Int31_to_int32b => num31_to_num_boxed {quad=false} arg
                            | Word31_to_word32b => num31_to_num_boxed {quad=false} arg
                            | Word31_to_word32b_X => num31_to_num_boxed {quad=false} arg
                            | Word32b_to_int32b => num32b_to_num32b {ovf=true} arg
                            | Word32b_to_int32b_X => num32b_to_num32b {ovf=false} arg
                            | Int32b_to_word32b => num32b_to_num32b {ovf=false} arg

                            | Word64b_to_int64b_X => num64b_to_num64b {ovf=false} arg
                            | Word64b_to_int64b => num64b_to_num64b {ovf=true} arg
                            | Word32b_to_word64b => word32b_to_word64b {signext=false} arg
                            | Word32b_to_word64b_X => word32b_to_word64b {signext=true} arg
                            | Word64b_to_word32b => num64b_to_num32b arg
                            | Word31_to_word64b => word31_to_word64b {signext=false} arg
                            | Word31_to_word64b_X => word31_to_word64b {signext=true} arg

                            | Int32b_to_int64b => int32b_to_int64b arg
                            | Int64b_to_word64b => num64b_to_num64b {ovf=false} arg

                            | Int31_to_int64b => num31_to_num_boxed {quad=true} arg

                            | Int63_to_int64b => num63_to_num64b {shr_inst=I.sarq} arg
                            | Word63_to_word32b => word63_to_word32b arg
                            | Word63_to_word64b => num63_to_num64b {shr_inst=I.shrq} arg
                            | Word63_to_word64b_X => num63_to_num64b {shr_inst=I.sarq} arg

                            | Int63_to_int32b => int63_to_int32b arg

                            | Bytetable_sub => bytetable_sub arg
                            | Bytetable_sub_word16 => bytetable_sub_word16 arg
                            | Bytetable_sub_word31 => bytetable_sub_word31 arg
                            | Bytetable_sub_word32ub => bytetable_sub_word32ub arg
                            | Bytetable_sub_word63 => bytetable_sub_word63 arg
                            | Bytetable_sub_word64ub => bytetable_sub_word64ub arg

                            | Word_sub0 => word_sub0 arg
                            | Plus_f64 => plus_f64 arg
                            | Minus_f64 => minus_f64 arg
                            | Mul_f64 => mul_f64 arg
                            | Div_f64 => div_f64 arg
                            | Max_f64 => max_f64 arg
                            | Min_f64 => min_f64 arg
                            | F64_to_real => f64_to_real_kill_tmp01 arg
                            | Blockf64_alloc => blockf64_alloc arg
                            | Blockf64_sub_f64 => blockf64_sub_f64 arg
                            | _ => die ("unsupported prim with 2 args: " ^ PrimName.pp_prim name)
                       end
                     | [b,x,y] =>
                       (case name of
                            Plus_int32b => add_int32b (b,x,y,d,size_ff,C)
                          | Plus_int64b => add_int64b (b,x,y,d,size_ff,C)
                          | Plus_word32b => addw32boxed (b,x,y,d,size_ff,C)
                          | Plus_word64b => addw64boxed (b,x,y,d,size_ff,C)
                          | Plus_real => addf_kill_tmp01 (x,y,b,d,size_ff,C)
                          | Minus_int32b => sub_int32b (b,x,y,d,size_ff,C)
                          | Minus_int64b => sub_int64b (b,x,y,d,size_ff,C)
                          | Minus_word32b => subw32boxed (b,x,y,d,size_ff,C)
                          | Minus_word64b => subw64boxed (b,x,y,d,size_ff,C)
                          | Minus_real => subf_kill_tmp01 (x,y,b,d,size_ff,C)
                          | Mul_int32b => mul_int32b (b,x,y,d,size_ff,C)
                          | Mul_int64b => mul_int64b (b,x,y,d,size_ff,C)
                          | Mul_word32b => mulw32boxed (b,x,y,d,size_ff,C)
                          | Mul_word64b => mulw64boxed (b,x,y,d,size_ff,C)
                          | Mul_real => mulf_kill_tmp01 (x,y,b,d,size_ff,C)
                          | Div_real => divf_kill_tmp01 (x,y,b,d,size_ff,C)
                          | Andb_word32b => andw32boxed__ (b,x,y,d,size_ff,C)
                          | Andb_word64b => andw64boxed__ (b,x,y,d,size_ff,C)
                          | Orb_word32b => orw32boxed__ (b,x,y,d,size_ff,C)
                          | Orb_word64b => orw64boxed__ (b,x,y,d,size_ff,C)
                          | Xorb_word32b => xorw32boxed__ (b,x,y,d,size_ff,C)
                          | Xorb_word64b => xorw64boxed__ (b,x,y,d,size_ff,C)
                          | Shift_left_word32b => shift_leftw32boxed__ (b,x,y,d,size_ff,C)
                          | Shift_left_word64b => shift_leftw64boxed__ (b,x,y,d,size_ff,C)
                          | Shift_right_signed_word32b => shift_right_signedw32boxed__ (b,x,y,d,size_ff,C)
                          | Shift_right_signed_word64b => shift_right_signedw64boxed__ (b,x,y,d,size_ff,C)
                          | Shift_right_unsigned_word32b => shift_right_unsignedw32boxed__ (b,x,y,d,size_ff,C)
                          | Shift_right_unsigned_word64b => shift_right_unsignedw64boxed__ (b,x,y,d,size_ff,C)
                          | Bytetable_update => bytetable_update (b,x,y,d,size_ff,C)
                          | Bytetable_update_word16 => bytetable_update_word16 (b,x,y,d,size_ff,C)
                          | Bytetable_update_word31 => bytetable_update_word31 (b,x,y,d,size_ff,C)
                          | Bytetable_update_word32ub => bytetable_update_word32ub (b,x,y,d,size_ff,C)
                          | Bytetable_update_word32b => bytetable_update_word32b (b,x,y,d,size_ff,C)
                          | Bytetable_update_word63 => bytetable_update_word63 (b,x,y,d,size_ff,C)
                          | Bytetable_update_word64ub => bytetable_update_word64ub (b,x,y,d,size_ff,C)
                          | Bytetable_update_word64b => bytetable_update_word64b (b,x,y,d,size_ff,C)

                          | Bytetable_sub_word32b => bytetable_sub_word32b (b,x,y,d,size_ff,C)
                          | Bytetable_sub_word64b => bytetable_sub_word64b (b,x,y,d,size_ff,C)

                          | Word_update0 => word_update0 (b,x,y,d,size_ff,C)
                          | Blockf64_update_real => blockf64_update_real (b,x,y,d,size_ff,C)
                          | Blockf64_sub_real => blockf64_sub_real (b,x,y,d,size_ff,C)
                          | Blockf64_update_f64 => blockf64_update_f64 (b,x,y,d,size_ff,C)
                          | _ => die ("unsupported prim with 3 args: " ^ PrimName.pp_prim name))
                     | _ => die ("PRIM(" ^ PrimName.pp_prim name ^ ") not implemented")))
                 end
               | LS.CCALL{name="spawnone",args=[arg],rhos_for_result=nil,res=[res]} =>
                 let
                   (* The call_closure C function takes one argument (an ML closure). It
                    * extracts the closure pointer and the closure environment from the argument
                    * and makes an ML call to the function represented by the closure. *)
                   val name = "spawnone"
                   val offset_codeptr = if BI.tag_values() then "8" else "0"
                   val call_closure_lab = new_local_lab (name ^ "_call_closure")
                   val _ = add_static_data ([I.dot_text,
                                             I.dot_align 8,
                                             (*I.dot_globl call_closure_lab, (* The C function entry *) *)
                                             I.lab call_closure_lab]
                                            @ (map (fn r => I.push (R r)) callee_save_regs_ccall)
                                            @ [I.movq(R rdi,R tmp_reg0)]
                                            (* now initialize thread local data to point to the threadinfo struct *)
                                            @ compile_c_call_prim("thread_init", [SS.PHREG_ATY tmp_reg0], SOME (SS.PHREG_ATY tmp_reg0), size_ff (* not used *), tmp_reg1,
                                              [I.movq(R tmp_reg0, R rdi),            (* restore argument, which is passed through thread_init *)
                                               I.movq(D("0",rdi),R rax),             (* extract closure from threadinfo arg into closure register *)
                                               I.leaq(D("8",rdi),R r14),             (* extract ctx from threadinfo and store it in ctx-register r14 *)
                                               I.movq(D(offset_codeptr,rax), R r10), (* extract code pointer into %r10 from C arg *)
                                               I.call' (R r10),                      (* call ML function *)
                                               I.movq(R rdi, R tmp_reg0)
                                              ]
                                            @ compile_c_call_prim("thread_exit", [SS.PHREG_ATY tmp_reg0], NONE, size_ff (* not used *), tmp_reg1,
                                              [I.movq(I "0", R rax)]                 (* move result to %rax *)
                                            @ (map (fn r => I.pop (R r)) (List.rev callee_save_regs_ccall))
                                            @ [I.ret])))

                 in I.movq(LA call_closure_lab, R tmp_reg0) ::
                    move_aty_into_reg(arg, tmp_reg1, size_ff,
                    (* call thread_create function, which will create a thread from the
                     * argument function by applying it to the second argument *)
                    compile_c_call_prim("thread_create", [SS.PHREG_ATY tmp_reg0,SS.PHREG_ATY tmp_reg1], SOME res, size_ff, tmp_reg1, C))
                 end
               | LS.CCALL{name,args,rhos_for_result,res} =>
                  let
                    fun comp_c_call (all_args,res,C) =
                      compile_c_call_prim(name, all_args, res, size_ff, tmp_reg1, C)
                    val _ =
                        case (explode name, rhos_for_result) of
                            (_, nil) => ()
                          | (#"@" :: _, _) =>
                            die ("CCALL." ^ name ^ ": auto-convertion is supported only for\n" ^
                                 "functions returning integers and taking integers as arguments!\n" ^
                                 "The function " ^ name ^ " takes " ^ Int.toString (length rhos_for_result) ^
                                 "region arguments.")
                          | _ => ()
                  in

                    (* the first argument in a dynamic function call, is the name of the function, *)
                    (* that argument must be on the top of the stack, as it is poped just before   *)
                    (* function invocation.                                                        *)
                    (* It is used to bind an address the first time the function is called         *)

                    comment_fn (fn () => "CCALL: " ^ pr_ls ls,
                                let val all_args =
                                        case name of
                                            ":" => (case args of
                                                        a1::ar => a1 ::(rhos_for_result@ar)
                                                      | _ => die ("Dynamic liking requires a string as first argument."))
                                          | _ => (rhos_for_result@args)
                                in case res of
                                       [] => comp_c_call(all_args, NONE, C)
                                     | [res_aty] => comp_c_call(all_args, SOME res_aty, C)
                                     | _ => die "CCall with more than one result variable"
                                end handle X => ( print ("EXN: CCALL: " ^ pr_ls ls ^ "\n")
                                                ; raise X))
                  end
               | LS.CCALL_AUTO{name, args, rhos_for_result, res} =>

        (* With dynamicly linked functions the first argument must be the name of   *)
        (* the function. If we where to implement automatic conversion into regions *)
        (* this must be taken care of, like in the non-automatic case               *)

                    comment_fn (fn () => "CCALL_AUTO: " ^ pr_ls ls,
                                compile_c_call_auto(name,args,rhos_for_result,res,size_ff,tmp_reg1,C)
                                handle X => ( print ("EXN: CCALL_AUTO: " ^ pr_ls ls ^ "\n")
                                            ; raise X)
                               )

               | LS.EXPORT{name,
                           clos_lab,
                           arg=(aty,ft1,ft2)} =>
                  let val clos_lab = DatLab clos_lab
                      val return_lab = new_local_lab ("return_" ^ name)
                      val ctx_lab = NameLab ("ctx_" ^ name)
                      val offset_codeptr = if BI.tag_values() then "8" else "0"
                      val lab = NameLab name   (* lab is the C function to call after the hook has been setup *)
                      val stringlab = gen_string_lab name
                      val _ =
                          if ft1 <> LS.Int orelse ft2 <> LS.Int then
                              die "Export of ML function with type other than (int->int) not supported"
                          else ()

                      val callee_saves_ccall = callee_save_regs_ccall @ [r14, r14]  (* odd number *)
                      val _ = add_static_data
                          ([I.dot_data,
                            I.dot_align 8,
                            I.dot_globl (ctx_lab,I.OBJ),  (* Slot for storing ctx *)
                            I.lab ctx_lab,
                            I.dot_quad (i2s BI.ml_unit),
                            I.dot_globl (clos_lab,I.OBJ),
                            I.lab clos_lab,  (* Slot for storing a pointer to the ML closure; the
                                              * ML closure object may move due to GCs. *)
                            I.dot_quad (i2s BI.ml_unit),
                            I.dot_text,
                            I.dot_align 8,
                            I.dot_globl (lab,I.FUNC), (* The C function entry *)
                            I.lab lab]
                         @ (map (fn r => I.push (R r)) callee_saves_ccall) (* 5+2 regs *)
                         @ [I.movq (L ctx_lab, R r14),            (* load ctx into ctx register *)
                            I.movq (L clos_lab, R rax),           (* load closure into ML arg 1 *)
                            I.movq (R rdi, R rbx),                (* move C arg into ML arg 2 *)
                            I.movq(D(offset_codeptr,rax), R r10), (* extract code pointer into %r10 *)
                            I.push (LA return_lab),               (* push return address *)
                            I.jmp (R r10),                        (* call ML function *)
                            I.lab return_lab,
                            I.movq(R rdi, R rax)]                 (* move result to %rax *)
                         @ (map (fn r => I.pop (R r)) (List.rev callee_saves_ccall))
                         @ [I.ret])

                     val saveregs = rdi :: rsi :: rdx :: rcx :: r8 :: r9 :: rax ::
                                    caller_save_regs_ccall (* 0 regs *)
                     fun push_callersave_regs C =
                         foldl (fn (r, C) => I.push(R r) :: C) C saveregs
                     fun pop_callersave_regs C =
                         foldr (fn (r, C) => I.pop(R r) :: C) C saveregs

                  in comment_fn (fn () => "EXPORT: " ^ pr_ls ls,
                     store_in_label(aty,clos_lab,tmp_reg1,size_ff,
                     I.movq (R r14, L ctx_lab) ::
                     I.movq (LA lab, R tmp_reg0) ::
                     I.movq (LA stringlab, R tmp_reg1) ::
                     I.push (I "1") ::
                     push_callersave_regs
                     (compile_c_call_prim("sml_regCfuns",[SS.PHREG_ATY tmp_reg1,
                                                          SS.PHREG_ATY tmp_reg0],NONE,0, tmp_reg1,
                      pop_callersave_regs
                      (I.addq (I "8", R rsp) :: C)))))
                  end
                )
       in
         foldr (fn (ls,C) => CG_ls(ls,C)) C lss
       end

     fun do_simple_memprof C =
         if simple_memprof_p() andalso gc_p() then
             let val labCont = new_local_lab "cont"
             in  I.cmpq(R rsp, L stack_min) ::
                 I.jl labCont ::
                 I.movq(R rsp, L stack_min) ::
                 I.lab labCont ::
                 C
             end
         else C

     fun do_prof C =
       if region_profiling() then
         let val labStack = new_local_lab "profStack"
           val labCont = new_local_lab "profCont"
           val labCont1 = new_local_lab "profCont1-"
           val labCont2 = new_local_lab "profCont2-"
           val maxStackLab = NameLab "maxStack"
           val timeToProfLab = NameLab "timeToProfile"

           (* some intelligent operations *)
           fun bin (tmp:I.reg) (inst:ea*ea->I.inst) (ea1:ea,ea2:ea,C:I.inst list) : I.inst list =
               case ea1 of
                   L (NameLab n) => I.movq(LA (NameLab n), R tmp) :: inst(D("",tmp),ea2) :: C
                 | _ => case ea2 of
                            L (NameLab n) => I.movq(LA (NameLab n), R tmp) :: inst(ea1, D("",tmp)) :: C
                          | _ => inst(ea1,ea2) :: C
           fun imov a = bin tmp_reg1 I.movq a
           fun iadd a = bin tmp_reg1 I.addq a
           fun isub a = bin tmp_reg1 I.subq a
         in
           imov(L maxStackLab, R tmp_reg0,     (* The stack grows downwards!! *)
           I.cmpq(R rsp, R tmp_reg0) ::
           I.jl labCont ::                                                    (* if ( rsp < *maxStack ) {     *)
           imov(R rsp, L maxStackLab,                                         (*    *maxStack = rsp ;         *)
           imov(L (NameLab "regionDescUseProfInf"), R tmp_reg0,               (*    maxProfStack =            *)
           iadd(L (NameLab "regionDescUseProfFin"), R tmp_reg0,               (*       regionDescUseProfInf   *)
           iadd(L (NameLab "allocProfNowFin"), R tmp_reg0,                    (*     + regionDescUseProfFin   *)
           iadd(R tmp_reg0, L (NameLab "maxProfStack"),                       (*     + allocProfNowFin ;      *)
           I.lab labCont ::                                                   (* }                            *)
                                                                              (* reg0 = stackBot - rsp + 8*(allocNowInf-regionDescUseProfInf-regionDescUseProfFin-allocProfNowFin); *)
                                                                              (* if ( reg0 > maxMem ) maxMem = reg0; *)
           imov(L (NameLab "allocNowInf"), R tmp_reg0,
           isub(L (NameLab "regionDescUseProfInf"), R tmp_reg0,
           isub(L (NameLab "regionDescUseProfFin"), R tmp_reg0,
           isub(L (NameLab "allocProfNowFin"), R tmp_reg0,
           I.imulq(I "8", R tmp_reg0) ::
           iadd(L (NameLab "stackBot"), R tmp_reg0,
           I.subq(R rsp, R tmp_reg0) ::
           imov(L (NameLab "maxMem"), R tmp_reg1,                             (* we can store in tmp_reg1 even with imov *)
           I.cmpq(R tmp_reg1, R tmp_reg0) ::
           I.jl labCont1 ::
           imov(R tmp_reg0, L (NameLab "maxMem"),

           I.lab labCont1 ::
           imov(L timeToProfLab, R tmp_reg0,                                  (* if ( timeToProfile )         *)
           I.cmpq(I "0", R tmp_reg0) ::                                       (*    call __proftick(rsp);     *)
           I.je labCont2 ::
           I.movq (R rsp, R tmp_reg1) ::              (* proftick assumes argument in tmp_reg1 *)
           I.push (LA labCont2) ::                    (* push return address *)
           I.jmp (L(NameLab "__proftick")) ::

           I.lab labCont2 ::
           C))))))))))))))
         end
       else C

    fun CG_top_decl' gen_fn (lab,cc,lss) =
      let
        val w0 = Word32.fromInt 0
        fun pw w = print ("Word is " ^ (Word32.fmt StringCvt.BIN w) ^ "\n")
        fun pws ws = app pw ws
        fun set_bit (bit_no,w) = Word32.orb(w,Word32.<<(Word32.fromInt 1,Word.fromInt bit_no))

        val size_ff = CallConv.get_frame_size cc
        val size_ff = if not(gc_p()) andalso size_ff = 1 andalso basic_lss lss then 0
                      else size_ff
(*
        val () = print ("basic: " ^ Bool.toString (basic_lss lss) ^ "\n")
*)
        val size_ccf = CallConv.get_ccf_size cc
        val size_rcf = CallConv.get_rcf_size cc
(*val _ = if size_ccf + size_rcf > 0 then die ("\ndo_gc: size_ccf: " ^ (Int.toString size_ccf) ^ " and size_rcf: " ^
                                               (Int.toString size_rcf) ^ ".") else () (* 2001-01-08, Niels debug *)*)
        val size_spilled_region_and_float_args = List.length (CallConv.get_spilled_region_and_float_args cc)
        val reg_args = map lv_to_reg_no (CallConv.get_register_args_excluding_region_and_float_args cc)
        val reg_map = foldl (fn (reg_no,w) => set_bit(reg_no,w)) w0 reg_args
   (*
        val _ = app (fn reg_no => print ("reg_no " ^ Int.toString reg_no ^ " is an argument\n")) reg_args
        val _ = pw reg_map
   *)
        val (checkGC,GCsnippet) = do_gc(reg_map,size_ccf,size_rcf,size_spilled_region_and_float_args)

        val (checkGC,GCsnippet) =
            if extra_gc_checks_p() orelse LS.allocating lss
            then (checkGC, GCsnippet)
            else (fn C => C, nil)

        val () = reset_code_blocks()

        val return_code =
            base_plus_offset(rsp,WORDS(size_ff+size_ccf),rsp,
                             I.ret :: nil)
        val C =
            checkGC(
            base_plus_offset(rsp,WORDS(~size_ff),rsp,
            do_simple_memprof(
            do_prof(
            CG_lss(lss,size_ff,size_ccf, return_code
            )))))
      in
        gen_fn(lab, C @ GCsnippet @ get_code_blocks())
      end

    fun CG_top_decl(LS.FUN(lab,cc,lss)) = CG_top_decl' I.FUN (lab,cc,lss)
      | CG_top_decl(LS.FN(lab,cc,lss)) = CG_top_decl' I.FN (lab,cc,lss)

    local
        fun data_x_progunit_lab x l = NameLab(Labels.pr_label l ^ "_data_" ^ x)
        fun data_x_lab x (l:label, C) =
            if gc_p() then
                let val lab = data_x_progunit_lab x l
                in I.dot_globl (lab,I.OBJ) ::
                    I.lab lab :: C
                end
            else C
    in
        fun data_begin_progunit_lab (MLFunLab l) = data_x_progunit_lab "begin" l
          | data_begin_progunit_lab _ = die "data_begin_progunit_lab"
        fun data_begin_lab a = data_x_lab "begin" a
        fun data_end_progunit_lab (MLFunLab l) = data_x_progunit_lab "end" l
          | data_end_progunit_lab _ = die "data_end_progunit_lab"
        fun data_end_lab a = data_x_lab "end" a
    end

    (***************************************************)
    (* Init Code and Static Data for this program unit *)
    (***************************************************)
    fun static_data (l:label) =
      I.dot_data ::
      comment ("START OF STATIC DATA AREA",
      data_begin_lab (l,
      get_static_data (data_end_lab(l,
      comment ("END OF STATIC DATA AREA",nil)))))

    fun init_x64_code () = [I.dot_text]
  in
    fun CG {main_lab:label,
            code=ss_prg: (StoreTypeCO,offset,AtySS) LinePrg,
            imports:label list * label list,
            exports:label list * label list,
            safe:bool} =
      let
        val _ = chat "[X64 Code Generation..."
        val _ = reset_static_data()
        val _ = reset_label_counter()
        val _ = add_static_data (I.dot_data :: map (fn lab => I.dot_globl(MLFunLab lab,I.FUNC))
                                                   (main_lab::(#1 exports)))
        val _ = add_static_data (I.dot_data :: map (fn lab => I.dot_globl(DatLab lab,I.OBJ))
                                                   (#2 exports))
        val x64_prg = {top_decls = foldr (fn (func,acc) => CG_top_decl func :: acc) [] ss_prg,
                       init_code = init_x64_code(),
                       static_data = static_data main_lab}
        val x64_prg = I.optimise x64_prg
        val _ = chat "]\n"
      in
        x64_prg
      end

    (* ------------------------------------------------------------------------------ *)
    (*              Generate Link Code for Incremental Compilation                    *)
    (* ------------------------------------------------------------------------------ *)

    local

        fun slot_for_datlab ((_,l),C) =
            let fun maybe_dotsize C =
                    if I.sysname() = "Darwin" then C
                    else I.dot_size(DatLab l, 8) :: C
            in
                I.dot_globl (DatLab l,I.OBJ) ::
                I.dot_data ::
                I.dot_align 8 ::
                maybe_dotsize (I.lab (DatLab l) ::
                               I.dot_quad "1" :: C)
            end

        fun slots_for_datlabs (l,C) = foldr slot_for_datlab C l

        fun toplevel_handler C =
          let
            val (clos_lv,arg_lv) = CallConv.handl_arg_phreg RI.args_phreg
            val (clos_reg,arg_reg) = (RI.lv_to_reg clos_lv, RI.lv_to_reg arg_lv)
            val offset = if BI.tag_values() then 1 else 0
            val nlab = NameLab "TopLevelHandlerLab"
          in
              I.dot_globl (nlab,I.FUNC) ::
              I.lab nlab ::
              I.movq (R arg_reg, R tmp_reg0)::
              load_indexed(R arg_reg,arg_reg,WORDS offset,
              load_indexed(R tmp_reg1,arg_reg, WORDS offset,
              load_indexed(R arg_reg,arg_reg,WORDS (offset+1), (* Fetch pointer to exception string *)
              compile_c_call_prim("uncaught_exception",[SS.PHREG_ATY r14,   (* evaluation context *)
                                                        SS.PHREG_ATY arg_reg,SS.PHREG_ATY tmp_reg1,
                                                        SS.PHREG_ATY tmp_reg0],NONE,0,tmp_reg1,
              I.ret :: C))))
          end

        fun store_exported_data_for_gc (labs,C) =
            if gc_p() then
              let (* Make sure to leave stack 16-byte aligned if required by os *)
                  val F = if length(labs) mod 2 = 0 then
                            fn C => I.push (I "1") :: C (* align *)
                          else fn C => C
              in F(foldr (fn (l,acc) => I.push(LA l) :: acc)
                         (I.push (I (i2s (List.length labs))) ::
                          I.movq(R rsp, L data_lab_ptr_lab) :: C) labs)
              end
          else C

        (* allocinreg expects the following arguments:
         *   tmp_reg0: size in words
         *   tmp_reg1: region ptr
         *   result in tmp_reg1
         *)

        fun allocinreg C =
            if not(parallelism_p()) then C
            else
            let val allocinreg_protect = new_local_lab "alloc_protect"
                val mutex_offset_bytes = i2s (8*BackendInfo.region_mutex_offset_words())
                val l_expand = new_local_lab "expand"
                val l_expand_protect = new_local_lab "protect"
                fun mk_expand_protect C =
                    I.lab l_expand_protect ::                 (* expand_protect:                    *)
                    I.pop(R I.rax) ::                         (*   restore rax                      *)
                    I.pop(R tmp_reg1) ::
                    I.pop(R tmp_reg0) ::
                    I.jmp(L(NameLab "__allocate")) :: C       (*   jmp to __allocate with args in   *)
                                                              (*     tmp_reg1 and tmp_reg0; result  *)
                                                              (*     in tmp_reg1; return address is *)
                                                              (*     already on the stack...        *)
                fun mk_expand C =
                    I.lab l_expand ::                             (* expand:                            *)
                    I.pop(R tmp_reg1) ::                          (*   pop region ptr                   *)
                    I.pop(R tmp_reg0) ::
                    I.jmp(L(NameLab "__allocate_unprotected")) :: (*   jmp to __allocate with args in   *)
                    C                                             (*     tmp_reg1 and tmp_reg0; result  *)
                                                                  (*     in tmp_reg1; return address is *)
                                                                  (*     already on the stack...        *)

              (* remember to update alloc_period only when "alloc" or
               * "alloc_protected" are not called, as these runtime
               * routines themselves update alloc_period... *)
              fun maybe_update_alloc_period r C =
                  if gc_p() then
                    I.addq(R r,L(NameLab "alloc_period")) :: C
                  else C

            in I.dot_globl(NameLab "allocinreg",I.FUNC) ::
              I.lab (NameLab "allocinreg") ::
              I.andq(I (i2s (~4)), R tmp_reg1) ::                    (*   tmp_reg1 = clearBits(tmp_reg1)   *)
              I.cmpq(I "0",D(mutex_offset_bytes,tmp_reg1)) ::
              I.jne allocinreg_protect ::
              (* simple non-protected allocation *)
              I.push(R tmp_reg0) ::                                   (*   push tmp_reg0 (n)                *)
              I.push(R tmp_reg1) ::                                   (*   push tmp_reg1                    *)
              load_indexed(R tmp_reg1,tmp_reg1,WORDS 0,               (*   tmp_reg1 = tmp_reg1[0]           *)
              I.addq(I "-1", R tmp_reg1) ::                           (*   tmp_reg1 = tmp_reg1 - 1          *)
              copy(tmp_reg1, tmp_reg0,
              I.movq(D("8",rsp), R tmp_reg1) ::
              I.leaq(DD("0",tmp_reg0,tmp_reg1,"8"),R tmp_reg1) ::     (*   tmp_reg1 = tmp_reg0 + 8n         *)
              I.orq(I (allocBoundaryMask()), R tmp_reg0) ::
              I.cmpq(R tmp_reg0, R tmp_reg1) ::                       (*   jmp expand if (tmp_reg0 > tmp_reg1) *)
              I.jg l_expand ::                                        (*        (a-1+8n > boundary-1)       *)
              I.leaq(D("1",tmp_reg1),R tmp_reg0) ::                   (*   tmp_reg0 = tmp_reg1 + 1          *)
              I.pop (R tmp_reg1) ::
              store_indexed (tmp_reg1,WORDS 0,R tmp_reg0,             (*   tmp_reg1[0] = tmp_reg0           *)
              I.pop (R tmp_reg1) ::                                   (*   tmp_reg1 = n                     *)
              I.imulq(I"8",R tmp_reg1) ::                             (*   tmp_reg1 = 8n                   *)
              maybe_update_alloc_period tmp_reg1 (
              I.subq (R tmp_reg1,R tmp_reg0) ::                       (*   tmp_reg0 = tmp_reg0-8n          *)
              I.movq (R tmp_reg0,R tmp_reg1) ::
              I.ret ::

              (* complex atomic *)
              I.lab allocinreg_protect ::
              I.push(R tmp_reg0) ::                                  (*   push tmp_reg0 (n)                *)
              I.push(R tmp_reg1) ::                                  (*   push tmp_reg1                    *)
              load_indexed(R tmp_reg1,tmp_reg1,WORDS 0,              (*   tmp_reg1 = tmp_reg1[0]           *)
              I.push(R I.rax) ::                                     (*   save rax on the stack            *)
              I.movq(R tmp_reg1, R I.rax) ::                         (*   rax = tmp_reg1                   *)
              I.addq(I "-1", R tmp_reg1) ::                          (*   tmp_reg1 = tmp_reg1 - 1          *)
              copy(tmp_reg1, tmp_reg0,
              I.orq(I (allocBoundaryMask()), R tmp_reg0) ::
              I.movq(D("16",rsp), R tmp_reg1) ::                     (*   tmp_reg1 = n                     *)
              I.leaq(DD("-1",rax,tmp_reg1,"8"),R tmp_reg1) ::        (*   tmp_reg1 = a-1+8n                *)
              I.cmpq(R tmp_reg0, R tmp_reg1) ::                      (*   jmp expand if (tmp_reg0 > tmp_reg1) *)
              I.jg l_expand_protect ::                               (*        (a-1+8n > boundary-1)       *)
              I.leaq(D("1",tmp_reg1),R tmp_reg0) ::                  (*   tmp_reg0 = tmp_reg1 + 1          *)
              I.movq(D("8",rsp),R tmp_reg1) ::                       (*   restore tmp_reg1                 *)
              I.cmpxchgq(R tmp_reg0,D("0",tmp_reg1)) ::              (*   tmp_reg1[0] = tmp_reg0           *)
              I.jne l_expand_protect ::                              (*      (if rax = tmp_reg1[0])        *)
              I.pop (R rax) ::
              I.pop (R tmp_reg1) ::
              I.pop (R tmp_reg1) ::
              I.imulq(I"8",R tmp_reg1) ::                            (*   tmp_reg1 = 8n                   *)
              maybe_update_alloc_period tmp_reg1 (
              I.subq (R tmp_reg1,R tmp_reg0) ::                      (*   tmp_reg0 = tmp_reg0-8n          *)
              I.movq (R tmp_reg0,R tmp_reg1) ::
              I.ret ::

              mk_expand (
              mk_expand_protect C))))))))
            end

        fun raise_insts C = (* expects ctx in rdi and exception value in register rsi!! *)
          let
            val (clos_lv,arg_lv) = CallConv.handl_arg_phreg RI.args_phreg
            val (clos_reg,arg_reg) = (RI.lv_to_reg clos_lv, RI.lv_to_reg arg_lv)
            val offset_codeptr = if BI.tag_values() then "8" else "0"
          in
            I.dot_globl(NameLab "raise_exn",I.FUNC) ::
            I.lab (NameLab "raise_exn") ::
            I.subq (I "8", R rsp) :: (* adjust stack pointer for alignment *)
            I.movq (R rdi, R r14) :: (* reinstall context pointer *)
            I.movq (R rsi, R r15) :: (* move argument to callee-save register *)
            comment ("DEALLOCATE REGIONS UNTIL",
            I.movq(D(ctx_exnptr_offs,r14), R tmp_reg1) ::
            compile_c_call_prim("deallocateRegionsUntil",[SS.PHREG_ATY I.r14,SS.PHREG_ATY tmp_reg1],NONE,0,tmp_reg1,

            comment ("RESTORE EXN PTR",
            I.movq(D(ctx_exnptr_offs,r14), R tmp_reg1) ::
            I.movq(D("16",tmp_reg1), R tmp_reg0) ::   (* two word offset *)
            I.movq(R tmp_reg0, D(ctx_exnptr_offs,r14)) ::

            comment ("INSTALL HANDLER EXN-ARGUMENT",
            I.movq(R r15, R arg_reg) ::

            comment ("RESTORE RSP AND PUSH RETURN LAB",
            I.movq(D("24", tmp_reg1), R rsp) ::             (* Restore sp *)
            I.push(D("0", tmp_reg1)) ::                     (* Push Return Lab *)

            comment ("JUMP TO HANDLE FUNCTION",
            I.movq(D("8", tmp_reg1), R clos_reg) ::         (* Fetch Closure into Closure Argument Register *)
            I.movq(D(offset_codeptr,clos_reg), R tmp_reg0) ::
            I.jmp (R tmp_reg0) :: C))))))
          end

        (* primitive exceptions *)
        fun setup_primitive_exception ((n,exn_string,exn_lab,exn_flush_lab),C) =
          let
            val string_lab = gen_string_lab exn_string
            val _ =
              if BI.tag_values() then       (* Exception Name and Exception must be tagged. *)
                add_static_data [I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl (exn_lab,I.OBJ),
                                 I.lab exn_lab,
                                 I.dot_quad(BI.pr_tag_w(BI.tag_exname(true))),
                                 I.dot_quad "0", (*dummy for pointer to next word*)
                                 I.dot_quad(BI.pr_tag_w(BI.tag_excon0(true))),
                                 I.dot_quad(i2s n),
                                 I.dot_quad "0"  (*dummy for pointer to string*),
                                 I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl (exn_flush_lab,I.OBJ),
                                 I.lab exn_flush_lab, (* The Primitive Exception is Flushed at this Address *)
                                 I.dot_quad "0"]
              else
                add_static_data [I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl (exn_lab,I.OBJ),
                                 I.lab exn_lab,
                                 I.dot_quad "0", (*dummy for pointer to next word*)
                                 I.dot_quad(i2s n),
                                 I.dot_quad "0",  (*dummy for pointer to string*)
                                 I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl (exn_flush_lab,I.OBJ),
                                 I.lab exn_flush_lab, (* The Primitive Exception is Flushed at this Address *)
                                 I.dot_quad "0"]
          in
            if BI.tag_values() then
              comment ("SETUP PRIM EXN: " ^ exn_string,
              load_label_addr(exn_lab,SS.PHREG_ATY tmp_reg0,tmp_reg0,0,
              I.movq(R tmp_reg0, R tmp_reg1) ::
              I.addq(I "16", R tmp_reg1) ::
              I.movq(R tmp_reg1, D("8",tmp_reg0)) ::
              load_label_addr(string_lab,SS.PHREG_ATY tmp_reg1,tmp_reg1,0,
              I.movq(R tmp_reg1,D("32",tmp_reg0)) ::
              load_label_addr(exn_flush_lab,SS.PHREG_ATY tmp_reg1,tmp_reg1,0, (* Now flush the exception *)
              I.movq(R tmp_reg0, D("0",tmp_reg1)) :: C))))
            else
              comment ("SETUP PRIM EXN: " ^ exn_string,
              load_label_addr(exn_lab,SS.PHREG_ATY tmp_reg0,tmp_reg0,0,
              I.movq(R tmp_reg0, R tmp_reg1) ::
              I.addq(I "8", R tmp_reg1) ::
              I.movq(R tmp_reg1,D("0",tmp_reg0)) ::
              load_label_addr(string_lab,SS.PHREG_ATY tmp_reg1,tmp_reg1,0,
              I.movq(R tmp_reg1,D("16",tmp_reg0)) ::
              load_label_addr(exn_flush_lab,SS.PHREG_ATY tmp_reg1,tmp_reg1,0, (* Now flush the exception *)
              I.movq(R tmp_reg0,D("0",tmp_reg1)) :: C))))
          end

        fun push_reg (r,C) =
            if I.is_xmm r then
              I.subq(I "8", R rsp) :: I.movsd(R r, D("",rsp)) :: C
            else I.push (R r) :: C

        fun pop_reg (r,C) =
            if I.is_xmm r then
              I.movsd(D("",rsp), R r) :: I.addq(I "8", R rsp) :: C
            else I.pop (R r) :: C

        (* args can only be tmp_reg0 (r10) and tmp_reg1 (r11); no arguments
         * on the stack; only the return address! Destroys tmp_reg0! *)
        fun ccall_stub (stubname, cfunction, args, ret, C) =  (* result in tmp_reg1 if ret=true *)
          let
            val save_regs = rdi :: rsi :: rdx :: rcx :: r8 :: r9 :: rax :: map RI.lv_to_reg RI.f64_phregs
                        (*caller_save_regs_ccall*)  (* maybe also save the other
                                                     * ccall argument registers and the
                                                     * ccall result register rax *)

            (* Notice that caller_save_regs_ccall is defined as the empty list! *)

            (* The following registers must be preserved as register
               allocation may choose to map variables to these
               registers:

                 X = {rax,rbx,rdi,rdx,rsi} U {rbx,rbp,r12,r13,r14,r15} U {xmm0..xmm13}
                   = {rax,rbx,rdi,rdx,rsi,rbp,r12,r13,r14,r15} U {xmm0..xmm13}

               Here are the registers that are not saved:

                 X \ save_regs = [rbx,rbp,r12,r13,r14,r15]

               These should exactly be the callee-save registers!
            *)

            fun push_callersave_regs C = foldl push_reg C save_regs
            fun pop_callersave_regs C = foldr pop_reg C save_regs
            val size_ff = 0 (* dummy *)
            val stublab = NameLab stubname
            val res = if ret then SOME (SS.PHREG_ATY tmp_reg1) else NONE
          in
            I.dot_text ::
            I.dot_globl (stublab,I.FUNC) ::
            I.lab stublab ::
            push_callersave_regs
            (compile_c_call_prim(cfunction, map SS.PHREG_ATY args, res, size_ff, tmp_reg0,
              pop_callersave_regs
                  ( I.ret ::
                    (*
                    I.pop(R tmp_reg0) ::
                    I.jmp(R tmp_reg0) :: *)
                   C)))
          end

        fun allocate C = (* args in tmp_reg1 and tmp_reg0; result in tmp_reg1. *)
          ccall_stub("__allocate", "alloc", [tmp_reg1, tmp_reg0], true, C)

        fun allocate_unprotected C = (* args in tmp_reg1 and tmp_reg0; result in tmp_reg1. *)
            if parallelism_p() (*andalso par_alloc_unprotected_p()*) then
              ccall_stub("__allocate_unprotected", "alloc_unprotected", [tmp_reg1, tmp_reg0], true, C)
            else C

        fun resetregion C =
          ccall_stub("__reset_region", "resetRegion", [tmp_reg1], true, C)

        fun proftick C =
          if region_profiling() then
            ccall_stub("__proftick", "profileTick", [r14,tmp_reg1], false, C)   (* first argument is the evaluation context *)
          else C

        fun overflow_stub C =
          let val stublab = [(NameLab "__raise_overflow",BI.exn_OVERFLOW_lab),
                             (NameLab "__raise_div",BI.exn_DIV_lab),
                             (NameLab "__raise_match",BI.exn_MATCH_lab),
                             (NameLab "__raise_bind",BI.exn_BIND_lab),
                             (NameLab "__raise_interrupt", BI.exn_INTERRUPT_lab),
                             (NameLab "__raise_subscript", BI.exn_SUBSCRIPT_lab),
                             (NameLab "__raise_size", BI.exn_SIZE_lab)]
          in I.dot_text ::(List.foldr (fn ((nl,dl),C') =>
                                          I.dot_globl (nl,I.FUNC) ::
                                          I.lab nl::
                                          I.movq(R r14, R rdi) ::            (* arg1: context *)
                                          I.movq(L(DatLab dl),R rsi)::       (* arg2: exception value *)
                                          I.call(NameLab "raise_exn")::C') C stublab)
          end

        fun gc_stub C = (* tmp_reg1 must contain the register map and tmp_reg0 the return address. *)
          if gc_p() then
            let
              (* first push the fpr registers; the runtime systems knows
                 about the order the registers appear on the stack *)
              val regs_to_store = map RI.lv_to_reg RI.args_phfreg @ all_regs
              fun push_all_regs C = foldr push_reg C regs_to_store
              fun pop_all_regs C = foldl pop_reg C regs_to_store
              fun pop_size_ccf_rcf_reg_args C =
                  base_plus_offset(rsp,WORDS 3,rsp,C) (* they are pushed in do_gc *)
              val size_ff = 0 (*dummy*)
            in
              I.dot_text ::
              I.dot_globl (gc_stub_lab,I.FUNC) ::
              I.lab gc_stub_lab ::
              push_all_regs                             (* The return lab and rcx are also preserved (16 regs) *)
              (copy(rsp,tmp_reg0,
              (copy(rsp,r15,                            (* Save rsp in r15 (callee-save ccall register *)
              I.push(I "1") ::                          (* at this point we don't know whether the stack *)
              I.andq(I "0xFFFFFFFFFFFFFFF0", R rsp) ::  (* is aligned, so we force align it here... *)
              compile_c_call_prim("gc",[SS.PHREG_ATY r14,SS.PHREG_ATY tmp_reg0,SS.PHREG_ATY tmp_reg1],NONE,size_ff,rax,
              copy(r15,rsp,                             (* Reposition stack; r14 is the context (first arg to gc) *)
              pop_all_regs(                             (* The return lab and tmp_reg0 are also popped again *)
              pop_size_ccf_rcf_reg_args(
              (I.jmp(R tmp_reg0) :: C)))))))))
            end
          else C

        val data_begin_init_lab = NameLab "data_begin_init_lab"
        val data_end_init_lab = NameLab "data_end_init_lab"
        val data_begin_addr = NameLab "data_begin_addr"
        val data_end_addr = NameLab "data_end_addr"
        fun generate_data_begin_end (progunit_labs,C) =
            if gc_p() then
                let
                    fun comp (l,C) =
                        let val begin_punit_lab = data_begin_progunit_lab l
                            val end_punit_lab = data_end_progunit_lab l
                            val lbelow = new_local_lab "lbelow"
                            val labove = new_local_lab "labove"
                        in
                            I.cmpq(LA begin_punit_lab, R tmp_reg0) ::
                            I.jb lbelow ::
                            I.movq(LA begin_punit_lab, R tmp_reg0) ::
                            I.lab lbelow ::
                            I.cmpq(LA end_punit_lab, R tmp_reg1) ::
                            I.ja labove ::
                            I.movq(LA end_punit_lab, R tmp_reg1) ::
                            I.lab labove ::
                            C
                        end
                in
                    I.movq (LA data_begin_init_lab, R tmp_reg0) ::
                    I.movq (LA data_end_init_lab, R tmp_reg1) ::
                    foldl comp (I.movq (R tmp_reg0, L data_begin_addr) ::
                                I.movq (R tmp_reg1, L data_end_addr) :: C)
                    progunit_labs
                end
            else C

        fun generate_jump_code_progunits (progunit_labs,C) =
          foldr (fn (l,C) =>
                 let val next_lab = new_local_lab "next_progunit_lab"
                 in
                   comment ("PUSH NEXT LOCAL LABEL",
                   I.push(LA next_lab) ::
                   comment ("JUMP TO NEXT PROGRAM UNIT",
                   I.jmp(L l) ::
                   I.dot_quad "0xFFFFFFFFFFFFFFFF" :: (* Marks no more frames on stack. For calculating rootset. *)
                   I.dot_quad "0xFFFFFFFFFFFFFFFF" :: (* An arbitrary offsetToReturn *)
                   I.dot_quad "0xFFFFFFFFFFFFFFFF" :: (* An arbitrary function number. *)
                   I.lab next_lab ::
                   C))
                 end) C progunit_labs

        fun allocate_global_regions (region_labs,C) =
          let
            fun maybe_pass_region_id (rho,C) =
                if region_profiling()
                then let val region_id = Effect.key_of_eps_or_rho rho
                     in I.movq(I (i2s region_id), R rdx) :: C
                     end
                else if parallelism_p()
                then let val protect = "1"  (* use protection (mutexes) for global regions *)
                     in I.movq(I protect, R rdx) :: C
                     end
                else C
            (* Notice, that regionId is not tagged because compile_c_call is not used *)
            (* Therefore, we do not use the MaybeUnTag-version. 2001-05-11, Niels     *)
            fun c_name rho =
                if regions_holding_values_of_the_same_type_only rho then
                    case Effect.get_place_ty rho of
                        SOME Effect.PAIR_RT =>
                            if region_profiling() then "allocPairRegionInfiniteProfiling"
                            else "allocatePairRegion"
                      | SOME Effect.REF_RT =>
                            if region_profiling() then "allocRefRegionInfiniteProfiling"
                            else "allocateRefRegion"
                      | SOME Effect.TRIPLE_RT =>
                            if region_profiling() then "allocTripleRegionInfiniteProfiling"
                            else "allocateTripleRegion"
                      | SOME Effect.ARRAY_RT =>
                            if region_profiling() then "allocArrayRegionInfiniteProfiling"
                            else "allocateArrayRegion"
                      | _ => die "allocate_global_regions.c_name"
                else
                    if region_profiling() then "allocRegionInfiniteProfiling"
                    else "allocateRegion"
          in
            foldl (fn ((rho,lab),C) =>
                   let val name = c_name rho
                       val C = I.movq(R rax, L (DatLab lab)) :: C
                       val sz_regdesc = BI.size_of_reg_desc()
                       val sz_regdesc = if sz_regdesc mod 2 = 0 then sz_regdesc
                                        else sz_regdesc+1
                       val sz_regdesc_bytes = 8 * sz_regdesc
                       (* The stack is thus ensured to be 16-byte aligned after the
                        * return address is pushed on the stack by the call instruction.
                        *)
                   in
                       I.subq(I(i2s sz_regdesc_bytes), R rsp) ::
                       I.movq(R r14, R rdi) ::
                       I.movq(R rsp, R rsi) ::
                       maybe_pass_region_id (rho,
                                             I.call(NameLab name) ::
                                             C)
                   end) C region_labs
          end

        fun push_top_level_handler C =
          let
            fun gen_clos C =
              if BI.tag_values() then
                copy(rsp, tmp_reg1,
                I.addq(I "-8", R tmp_reg1) ::
                I.movq(R tmp_reg1, D("8", rsp)) :: C)
              else
                I.movq(R rsp, D("8", rsp)) :: C
          in
            comment ("PUSH TOP-LEVEL HANDLER ON STACK",
            I.subq(I "8", R rsp) ::                       (* anti-align *)
            I.subq(I "32", R rsp) ::
            I.movq(LA (NameLab "TopLevelHandlerLab"), R tmp_reg1) ::
            I.movq(R tmp_reg1, D("0", rsp)) ::
            gen_clos (
            I.movq(D(ctx_exnptr_offs,r14), R tmp_reg1) ::
            I.movq(R tmp_reg1, D("16", rsp)) ::
            I.movq(R rsp, D("24", rsp)) ::
            I.movq(R rsp, D(ctx_exnptr_offs,r14)) ::
            I.subq(I "8", R rsp) ::                       (* align *)
            C))
          end

(*

      rsp+40   &TopLevelHandlerLab   (closure, and alignment)
H[24] rsp+32   rsp+8
H[16] rsp+24   Prev exnPtr (NULL)
H[8]  rsp+16   rsp+40
H[0]  rsp+8    &TopExnContLab        <-- exnPtr
      rsp      1                     (alignment)

    An exception handler H on the stack consists of four words:
        H[24]: stack pointer for handler code
        H[16]: previous exn slot
        H[8] : closure pointer
        H[0] : return address,
*)

        fun push_top_level_handler_ret retlab C =
          let
            fun gen_clos C =               (* store closure under handler *)
              if BI.tag_values() then
                I.movq(LA (NameLab "TopLevelHandlerLab"), R tmp_reg1) ::
                I.movq(R tmp_reg1, D("32", rsp)) ::
                I.leaq(D("24", rsp), R tmp_reg1) ::
                I.movq(R tmp_reg1, D("8", rsp)) ::
                C
              else
                I.movq(LA (NameLab "TopLevelHandlerLab"), R tmp_reg1) ::
                I.movq(R tmp_reg1, D("32", rsp)) ::
                I.leaq(D("32", rsp), R tmp_reg1) ::   (* compute closure address *)
                I.movq(R tmp_reg1, D("8", rsp)) ::   (* store closure address in second word of exn handler *)
                C
          in
            comment ("PUSH CLOSURE AND TOP-LEVEL HANDLER ON STACK",
            I.subq(I "8", R rsp) ::                                   (* anti-align, closure *)
            I.subq(I "32", R rsp) ::                                  (* space for exception handler *)
            gen_clos (                                                (* create closure and store closure address in handler *)
            I.movq(D(ctx_exnptr_offs,r14), R tmp_reg1) ::
            I.movq(R tmp_reg1, D("16", rsp)) ::                       (* store previous exnPtr in handler *)
            I.movq(R rsp, D("24", rsp)) ::                            (* store current stack address in handler *)
            I.movq(LA (NameLab retlab), R tmp_reg1) ::
            I.movq(R tmp_reg1, D("0", rsp)) ::                        (* store retlab in handler *)
            I.movq(R rsp, D(ctx_exnptr_offs,r14)) ::                  (* store handler address in context's exnPtr *)
            I.push(I "1") ::                                          (* align *)
            C))
          end

        fun init_stack_bot_gc C =
          if gc_p() then  (* stack_bot_gc[0] = rsp *)
              let val C = if simple_memprof_p() then I.movq(R rsp, L stack_min) :: C
                          else C
              in
                  I.movq(R rsp, L stack_bot_gc_lab) :: C
              end
          else C

        fun init_prof C =
          if region_profiling() then  (* stack_bot_gc[0] = rsp *)
            I.movq(R rsp, L (NameLab "stackBot")) ::
            I.movq(R rsp, L (NameLab "maxStack")) ::
            I.movq(R rsp, L (NameLab "maxStackP")) ::
            C
          else C

        fun data_begin C =
            if gc_p() then
              (I.lab (data_begin_init_lab) :: C)
            else C

        fun data_end C =
            if gc_p() then
                (I.dot_align 8 ::
                 I.dot_globl (data_begin_addr,I.OBJ) ::
                 I.lab data_begin_addr ::
                 I.dot_quad "0" ::
                 I.dot_globl (data_end_addr,I.OBJ) ::
                 I.lab data_end_addr ::
                 I.dot_quad "0" ::
                 I.lab (data_end_init_lab) ::   C)
            else C

        fun declare_exceptions_and_regions C =
            let val primitive_exceptions =
                    [(0, "Match", NameLab "exn_MATCH", DatLab BI.exn_MATCH_lab),
                     (1, "Bind", NameLab "exn_BIND", DatLab BI.exn_BIND_lab),
                     (2, "Overflow", NameLab "exn_OVERFLOW", DatLab BI.exn_OVERFLOW_lab),
                     (3, "Interrupt", NameLab "exn_INTERRUPT", DatLab BI.exn_INTERRUPT_lab),
                     (4, "Div", NameLab "exn_DIV", DatLab BI.exn_DIV_lab),
                     (5, "Subscript", NameLab "exn_SUBSCRIPT", DatLab BI.exn_SUBSCRIPT_lab),
                     (6, "Size", NameLab "exn_SIZE", DatLab BI.exn_SIZE_lab)
                    ]
                val initial_exnname_counter =
                    List.length primitive_exceptions
                val static_data =
                    slots_for_datlabs(global_region_labs,
                                      I.dot_data ::
                                      I.dot_globl (exn_counter_lab,I.OBJ) ::
                                      I.lab exn_counter_lab :: (* The Global Exception Counter *)
                                      I.dot_quad (i2s initial_exnname_counter) ::
                                      nil)
                val _  = add_static_data static_data
            in foldl setup_primitive_exception C primitive_exceptions
            end
    in

    fun generate_link_code (linkinfos:label list, exports: label list * label list) : I.AsmPrg =
      let
        val _ = reset_static_data()
        val _ = reset_label_counter()

        val progunit_labs = map MLFunLab linkinfos
        val dat_labs = map DatLab (#2 exports) (* Also in the root set 2001-01-09, Niels *)
(*
val _ = print ("There are " ^ (Int.toString (List.length dat_labs)) ^ " data labels in the root set. ")
val _ = List.app (fn lab => print ("\n" ^ (I.pr_lab lab))) (List.rev dat_labs)
*)

        fun main_insts C =
           (I.dot_text ::
            I.dot_align 8 ::
            I.dot_globl (NameLab "code", I.FUNC) ::
            I.lab (NameLab "code") ::
            I.push(I "1") ::                           (* 16-align stack *)

            (* Install argument context in context register *)
            I.movq(R rdi, R r14) ::

            (* Compute range of data space *)
            generate_data_begin_end(progunit_labs,

            (* Initialize profiling *)
            init_prof(

            (* Initialize stack_bot_gc. *)
            init_stack_bot_gc(

            (* Put data labels on the stack; they are part of the root-set. *)
            store_exported_data_for_gc (dat_labs,

            (* Allocate global regions and push them on stack *)
            comment ("Allocate global regions and push them on the stack",
            allocate_global_regions(global_region_labs,

            (* Initialize primitive exceptions *)
            declare_exceptions_and_regions(

            (* Push top-level handler on stack *)
            push_top_level_handler(

            (* Code that jump to progunits. *)
            comment ("JUMP CODE TO PROGRAM UNITS",
            generate_jump_code_progunits(progunit_labs,

            (* Exit instructions *)
            (*I.push(I "1") ::*) (* ensure stack is 16-byte aligned after return address is pushed on the
                                  * stack by the I.call instruction. *)
            compile_c_call_prim("terminateML", [mkIntAty 0],
                                NONE,0,rax, (* instead of res we return the result from
                                             * the last function call *)
            I.ret :: C))))))))))))

        val init_link_code = (main_insts o raise_insts o
                              toplevel_handler o allocate o allocate_unprotected o resetregion o allocinreg o
                              overflow_stub o gc_stub o proftick) nil
      in
        {top_decls = [],
         init_code = init_link_code,
         static_data = (I.dot_data ::
                        comment ("START OF STATIC DATA AREA",
                        data_begin (
                        get_static_data (
                        data_end (
                        comment ("END OF STATIC DATA AREA",nil))))))}
      end

    fun generate_repl_init_code () : I.AsmPrg =
      let
        val _ = reset_static_data()
        val _ = reset_label_counter()

        fun main_insts C =
           (I.dot_text ::
            I.dot_align 8 ::
            I.dot_globl (NameLab "code",I.FUNC) ::
            I.lab (NameLab "code") ::
            I.push(I "1") ::                           (* 16-align stack *)

            (* Install argument context in context register *)
            I.movq(R rdi, R r14) ::

            (* Initialize profiling *)
            init_prof(

            (* Initialize stack_bot_gc. *)
            init_stack_bot_gc(

            (* Allocate global regions and push them on stack *)
            comment ("Allocate global regions and push them on the stack",
            allocate_global_regions(global_region_labs,

            (* Initialize primitive exceptions and global regions - datlabs *)
            declare_exceptions_and_regions(

            (* Call repl loop, which will terminate (exit) and never return *)
            I.movq(R r14, R rdi) ::
            I.call(NameLab "repl_interp") ::

            (* Exit instructions - never gets here... *)
            I.ret :: C))))))

        val init_code = (main_insts o raise_insts o
                         toplevel_handler o allocate o allocate_unprotected o resetregion o allocinreg o
                         overflow_stub o gc_stub o proftick) nil
      in
        {top_decls = [],
         init_code = init_code,
         static_data = (I.dot_data ::
                        comment ("START OF STATIC DATA AREA",
                        data_begin (
                        get_static_data (
                        data_end (
                        comment ("END OF STATIC DATA AREA",nil))))))}
      end

    fun generate_repl_link_code (lab:string, labs:label list) : I.AsmPrg =
      let
        val _ = reset_static_data()
        val _ = reset_label_counter()

        val progunit_labs = map MLFunLab labs
        val topretlab = "topretlab"

        local
          val callee_save_regs_ccall = r14::callee_save_regs_ccall
        in
          fun push_callee C =
              List.foldl (fn (r,C) => I.push (R r) :: C) C
                         callee_save_regs_ccall
          fun pop_callee C =
              List.foldr (fn (r,C) => I.pop (R r) :: C) C
                         callee_save_regs_ccall
        end

        fun main_insts C =
           (I.dot_text ::
            I.dot_align 8 ::
            I.dot_globl (NameLab lab,I.FUNC) ::
            I.lab (NameLab lab) ::
            I.push(I "1") ::     (* 16-align stack *)

            (* Push callee-save regs *)
            push_callee(

            (* Install top context in context register; setup in Runtime.c *)
            I.movq(L(NameLab "top_ctx"), R r14) ::

            (* Push top-level handler on stack *)
            push_top_level_handler_ret topretlab (

            (* Code that jump to progunits. *)
            comment ("JUMP CODE TO PROGRAM UNITS",
            generate_jump_code_progunits(progunit_labs,

            I.addq(I "8", R rsp) ::    (* align *)

            I.lab(NameLab topretlab) ::
            I.addq(I "40", R rsp) ::   (* 40: top-level handler *)

            pop_callee(
            I.addq(I "8", R rsp) ::    (* align *)
            I.ret :: C))))))

      in
        {top_decls = [],
         init_code = main_insts nil,
         static_data = (I.dot_data ::
                        comment ("START OF STATIC DATA AREA",
                        data_begin (
                        get_static_data (
                        data_end (
                        comment ("END OF STATIC DATA AREA",nil))))))}
      end


    end
  end

  local
    val messages_p = Flags.is_on0 "messages"
  in
    fun message f = if messages_p() then print (f())
                    else ()
  end

  (* ------------------------------------------------------------ *)
  (*  Emitting Target Code                                        *)
  (* ------------------------------------------------------------ *)
  fun emit (prg: AsmPrg,filename: string) : unit =
    (I.emit(prg,filename);
     message (fn () => "[wrote X64 code file:\t" ^ filename ^ "]\n"))
    handle IO.Io {name,...} => Crash.impossible ("CodeGenX64.emit:\nI cannot open \""
                                                 ^ filename ^ "\":\n" ^ name)

end
