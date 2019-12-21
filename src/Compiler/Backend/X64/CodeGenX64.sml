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
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  structure I = InstsX64
  structure RI = I.RI (* RegisterInfo *)
  structure BI = BackendInfo
  structure SS = SubstAndSimplify
  structure LS = LineStmt

  val region_profiling : unit -> bool = Flags.is_on0 "region_profiling"

  type label = Labels.label
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
  type StoreTypeCO = SubstAndSimplify.StoreTypeCO
  type AtySS = SubstAndSimplify.Aty
  datatype reg = datatype I.reg
  datatype ea = datatype I.ea
  datatype lab = datatype I.lab
  type offset = int
  type AsmPrg = I.AsmPrg

  val tmp_reg0 = I.tmp_reg0
  val tmp_reg1 = I.tmp_reg1
  val caller_save_regs_ccall = map RI.lv_to_reg RI.caller_save_ccall_phregs
  val callee_save_regs_ccall = map RI.lv_to_reg RI.callee_save_ccall_phregs
  val all_regs = map RI.lv_to_reg RI.all_regs

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat(s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CodeGenX64." ^ s)
  fun not_impl n = die ("prim(" ^ n ^ ") not implemented")
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
  val _ = Flags.add_bool_entry {long="comments_in_x64_asmcode", short=NONE, item=ref false,
                                menu=["Debug", "comments in x64 assembler code"], neg=false,
                                desc="Insert comments in x64 assembler code."}

  val jump_tables = true
  val comments_in_asmcode = Flags.lookup_flag_entry "comments_in_x64_asmcode"
  val gc_p = Flags.is_on0 "garbage_collection"
  val tag_pairs_p = Flags.is_on0 "tag_pairs"

  (* Simple memory profiling - remember to enable the flag
   * SIMPLE_MEMPROF in Runtime/Flags.h when you change this flag. *)
  fun simple_memprof_p () = false
  val stack_min = NameLab "stack_min"

  (**********************************
   * Some code generation utilities *
   **********************************)

  fun comment(str,C) = if !comments_in_asmcode then I.comment str :: C
                       else C
  fun comment_fn(f, C) = if !comments_in_asmcode then I.comment (f()) :: C
                         else C

  fun rem_dead_code nil = nil
    | rem_dead_code (C as i :: C') =
    case i
      of I.lab _ => C
       | I.dot_long _ => C
       | I.dot_quad _ => C
       | I.dot_byte _ => C
       | I.dot_align _ => C
       | I.dot_globl _ => C
       | I.dot_text => C
       | I.dot_data => C
       | I.comment s => i :: rem_dead_code C'
       | _ => rem_dead_code C'

  (********************************)
  (* CG on Top Level Declarations *)
  (********************************)

  local
  (******************************)
  (* Dynamicly linked functions *)
  (******************************)

  local val dynamic = ref (Binarymap.mkDict String.compare)
  in fun add_dynamic (name,l1,l2) = dynamic := Binarymap.insert(!dynamic, name, (l1,l2))
     val get_dynamic = fn x=> Binarymap.peek (!dynamic, x)
  end


    (* Global Labels *)
    val exn_ptr_lab = NameLab "exn_ptr"
    val exn_counter_lab = NameLab "exnameCounter"
    val time_to_gc_lab = NameLab "time_to_gc"     (* Declared in GC.c *)
    val data_lab_ptr_lab = NameLab "data_lab_ptr" (* Declared in GC.c *)
    val stack_bot_gc_lab = NameLab "stack_bot_gc" (* Declared in GC.c *)
    val gc_stub_lab = NameLab "__gc_stub"
    val global_region_labs =
      [(Effect.toplevel_region_withtype_top, BI.toplevel_region_withtype_top_lab),
       (Effect.toplevel_region_withtype_string, BI.toplevel_region_withtype_string_lab),
       (Effect.toplevel_region_withtype_pair, BI.toplevel_region_withtype_pair_lab),
       (Effect.toplevel_region_withtype_array, BI.toplevel_region_withtype_array_lab),
       (Effect.toplevel_region_withtype_ref, BI.toplevel_region_withtype_ref_lab),
       (Effect.toplevel_region_withtype_triple, BI.toplevel_region_withtype_triple_lab)]

    (* Labels Local To This Compilation Unit *)
    fun new_local_lab name = LocalLab (Labels.new_named name)
    local
      val counter = ref 0
      fun incr() = (counter := !counter + 1; !counter)
    in
      fun new_dynamicFn_lab() : lab = DatLab(Labels.new_named ("DynLab" ^ Int.toString(incr())))
      fun new_string_lab() : lab = DatLab(Labels.new_named ("StringLab" ^ Int.toString(incr())))
      fun new_float_lab() : lab = DatLab(Labels.new_named ("FloatLab" ^ Int.toString(incr())))
      fun new_num_lab() : lab = DatLab(Labels.new_named ("BoxedNumLab" ^ Int.toString(incr())))
      fun reset_label_counter() = counter := 0
    end

    (* Static Data inserted at the beginning of the code. *)
    local
      val static_data : I.inst list ref = ref []
    in
      fun add_static_data (insts) = (static_data := insts @ !static_data)
      fun reset_static_data () = static_data := []
      fun get_static_data C = !static_data @ C
    end

    (* giving numbers to registers---for garbage collection *)
    fun lv_to_reg_no lv =
        case RI.lv_to_reg lv of
            rax => 0 | rbx => 1 | rcx => 2 | rdx => 3
          | rsi => 4 | rdi => 5 | rbp => 6 | rsp => 7
          | r8 => 8 | r9 => 9 | r10 => 10 | r11 => 11
          | r12 => 12 | r13 => 13 | r14 => 14 | r15 => 15
          | r => die ("lv_to_reg.no: " ^ I.pr_reg r)

    (* Convert ~n to -n; works for all int32 values including Int32.minInt *)
    fun intToStr (i : Int32.int) : string =
      let fun tr s = case explode s
                       of #"~"::rest => implode (#"-"::rest)
                        | _ => s
      in tr (Int32.toString i)
      end

    fun wordToStr (w : Word32.word) : string =
      "0x" ^ Word32.toString w

    (* Convert ~n to -n *)
    fun i2s i = if i >= 0 then Int.toString i
                else "-" ^ Int.toString (~i)

    (* We make the offset base explicit in the following functions *)
    datatype Offset =
        WORDS of int
      | BYTES of int

    fun isZeroOffset (WORDS 0) = true
      | isZeroOffset (BYTES 0) = true
      | isZeroOffset _ = false

    fun offset_bytes (WORDS w) = i2s (8*w)  (* a WORD can contain a ptr or an unboxed integer or word value *)
      | offset_bytes (BYTES b) = i2s b

    fun copy(r1, r2, C) = if r1 = r2 then C
                          else I.movq(R r1, R r2) :: C

    (* Can be used to load from the stack or from a record *)
    (* d = b[n]                                            *)
    fun load_indexed(d:ea,b:reg,n:Offset,C) =
        I.movq(D(offset_bytes n,b), d) :: C

    (* Can be used to update the stack or store in a record *)
    (* b[n] = s                                             *)
    fun store_indexed(b:reg,n:Offset,s:ea,C) =
        I.movq(s,D(offset_bytes n,b)) :: C

    (* Calculate an address given a base and an offset *)
    (* dst = base + x                                  *)
    fun base_plus_offset(b:reg,n:Offset,d:reg,C) =
        if d = b andalso isZeroOffset n then C
        else I.leaq(D(offset_bytes n, b), R d) :: C

    fun mkIntAty i = SS.INTEGER_ATY {value=Int32.fromInt i,
                                     precision=if BI.tag_values() then 31 else 32}

    fun maybeTagInt {value: Int32.int, precision:int} : Int32.int =
      case precision
        of 31 => ((2 * value + 1)         (* use tagged-unboxed representation *)
                  handle Overflow => die "maybeTagInt.Overflow")
         | 32 => value                    (* use untagged representation - maybe boxed *)
         | _ => die "maybeTagInt"

    fun maybeTagWord {value: Word32.word, precision:int} : Word32.word =
      case precision
        of 31 =>                            (* use tagged representation *)
          let val w = 0w2 * value + 0w1
          in if w < value then die "maybeTagWord.Overflow"
             else w
          end
         | 32 => value                      (* use untagged representation - maybe boxed *)
         | _ => die "maybeTagWord"

    (* formatting of immediate integer and word values *)
    fun fmtInt a : string = intToStr(maybeTagInt a)
    fun fmtWord a : string = wordToStr(maybeTagWord a)

    (* Store a constant *)
    fun store_immed(w:Word32.word,r:reg,offset:Offset,C) =
      I.movq(I (wordToStr w), D(offset_bytes offset,r)) :: C

    fun move_immed(0,R d,C) = I.xorq(R d, R d) :: C
      | move_immed(x,d:ea,C) = I.movq(I (intToStr x), d) :: C

    fun move_num(x,ea:ea,C) =
      if (x = "0" orelse x = "0x0") andalso (case ea of R _ => true | _ => false)
          then I.xorq(ea, ea) :: C
      else I.movq(I x, ea) :: C

    fun move_num_boxed(x,ea:ea,C) =
      if not(BI.tag_values()) then die "move_num_boxed.boxed integers/words necessary only when tagging is enabled"
      else
        let val num_lab = new_num_lab()
          val _ = add_static_data [I.dot_data,
                                   I.dot_align 8,
                                   I.lab num_lab,
                                   I.dot_quad(BI.pr_tag_w(BI.tag_word_boxed(true))),
                                   I.dot_quad x]
        in I.movq(LA num_lab, ea) :: C
        end

    (* returns true if boxed representation is used for
     * integers of the given precision *)
    fun boxedNum (precision:int) : bool =
      precision > 31 andalso BI.tag_values()


    (* Find a register for aty and generate code to store into the aty *)
    fun resolve_aty_def(SS.STACK_ATY offset,t:reg,size_ff,C) =
         (t,store_indexed(rsp,WORDS(size_ff-offset-1),R t,C))       (*was ~size_ff+offset*)
      | resolve_aty_def(SS.PHREG_ATY phreg,t:reg,size_ff,C)  = (phreg,C)
      | resolve_aty_def(SS.UNIT_ATY,t:reg,size_ff,C)  = (t,C)
      | resolve_aty_def _ = die "resolve_aty_def: ATY cannot be defined"

    fun move_num_generic (precision, num, ea, C) =
        if boxedNum precision then move_num_boxed(num, ea, C)
        else move_num(num, ea, C)

    fun move_unit(ea,C) =
        if BI.tag_values() then
            move_immed(Int32.fromInt BI.ml_unit,ea,C) (* gc needs value! *)
        else C

    (* Make sure that the aty ends up in register dst_reg *)
    fun move_aty_into_reg(aty,dst_reg,size_ff,C) =
      case aty
        of SS.REG_I_ATY offset =>
          base_plus_offset(rsp,BYTES(size_ff*8-offset*8-8+BI.inf_bit),dst_reg,C)
         | SS.REG_F_ATY offset =>
          base_plus_offset(rsp,WORDS(size_ff-offset-1),dst_reg,C)
         | SS.STACK_ATY offset =>
          load_indexed(R dst_reg,rsp,WORDS(size_ff-offset-1),C)
         | SS.DROPPED_RVAR_ATY => C
         | SS.PHREG_ATY phreg => copy(phreg,dst_reg,C)
         | SS.INTEGER_ATY i => move_num_generic (#precision i, fmtInt i, R dst_reg, C)
         | SS.WORD_ATY w => move_num_generic (#precision w, fmtWord w, R dst_reg, C)
         | SS.UNIT_ATY => move_unit (R dst_reg, C)
         | SS.FLOW_VAR_ATY _ => die "move_aty_into_reg: FLOW_VAR_ATY cannot be moved"

    (* dst_aty = src_reg *)
    fun move_reg_into_aty(src_reg:reg,dst_aty,size_ff,C) =
      case dst_aty
        of SS.PHREG_ATY dst_reg => copy(src_reg,dst_reg,C)
         | SS.STACK_ATY offset => store_indexed(rsp,WORDS(size_ff-offset-1),R src_reg,C)    (*was ~size_ff+offset*)
         | SS.UNIT_ATY => C (* wild card definition - do nothing *)
         | _ => die "move_reg_into_aty: ATY not recognized"

    (* dst_aty = src_aty *)
    fun move_aty_to_aty(SS.PHREG_ATY src_reg,dst_aty,size_ff,C) = move_reg_into_aty(src_reg,dst_aty,size_ff,C)
      | move_aty_to_aty(src_aty,SS.PHREG_ATY dst_reg,size_ff,C) = move_aty_into_reg(src_aty,dst_reg,size_ff,C)
      | move_aty_to_aty(src_aty,SS.UNIT_ATY,size_ff,C) = C
      | move_aty_to_aty(src_aty,dst_aty,size_ff,C) =
      let val (reg_for_result,C') = resolve_aty_def(dst_aty,tmp_reg1,size_ff,C)
      in move_aty_into_reg(src_aty,reg_for_result,size_ff,C')
      end

    (* dst_aty = src_aty[offset] *)
    fun move_index_aty_to_aty(SS.PHREG_ATY src_reg,SS.PHREG_ATY dst_reg,offset:Offset,t:reg,size_ff,C) =
          load_indexed(R dst_reg,src_reg,offset,C)
      | move_index_aty_to_aty(SS.PHREG_ATY src_reg,dst_aty,offset:Offset,t:reg,size_ff,C) =
          load_indexed(R t,src_reg,offset,
          move_reg_into_aty(t,dst_aty,size_ff,C))
      | move_index_aty_to_aty(src_aty,dst_aty,offset,t:reg,size_ff,C) = (* can be optimised!! *)
          move_aty_into_reg(src_aty,t,size_ff,
          load_indexed(R t,t,offset,
          move_reg_into_aty(t,dst_aty,size_ff,C)))

    (* dst_aty = &lab *)
    fun load_label_addr(lab,dst_aty,t:reg,size_ff,C) =
        case dst_aty of
            SS.PHREG_ATY d => I.movq(LA lab, R d) :: C
          | SS.STACK_ATY offset =>
            I.movq(LA lab, R t) :: store_indexed(rsp, WORDS(size_ff-offset-1), R t, C)
            (*store_indexed(rsp, WORDS(size_ff-offset-1), LA lab, C)*)
          | _ => die "load_label_addr.wrong ATY"

    (* dst_aty = lab[0] *)
    fun load_from_label(lab,dst_aty,t:reg,size_ff,C) =
        case dst_aty of
            SS.PHREG_ATY d =>
            I.movq(LA lab, R d) ::
            I.movq(D("0",d), R d) :: C
          | SS.STACK_ATY offset =>
            I.movq(LA lab, R t) ::
            I.movq(D("0",t), R t) ::
            store_indexed(rsp, WORDS(size_ff-offset-1), R t, C)
          | SS.UNIT_ATY => C
          | _ => die "load_from_label.wrong ATY"

    (* lab[0] = src_aty *)
    fun store_in_label(src_aty,lab,tmp1:reg,size_ff,C) =
        case src_aty of
            SS.PHREG_ATY s =>
            I.movq(LA lab,R tmp1) :: I.movq(R s, D("0",tmp1)) :: C
          | SS.INTEGER_ATY i =>
            I.movq(LA lab,R tmp1) :: move_num_generic (#precision i, fmtInt i, D("0",tmp1), C)
          | SS.WORD_ATY w =>
            I.movq(LA lab,R tmp1) :: move_num_generic (#precision w, fmtWord w, D("0",tmp1), C)
          | SS.UNIT_ATY =>
            I.movq(LA lab,R tmp1) :: move_unit(D("0",tmp1), C)
(*        | SS.STACK_ATY offset => load_indexed(L lab, rsp, WORDS(size_ff-offset-1), C) *)
          | _ => move_aty_into_reg(src_aty,tmp1,size_ff,
                 I.movq(R tmp1, L lab) :: C)

    (* Generate a string label *)
    fun gen_string_lab str =
      let val string_lab = new_string_lab()

          (* generate a .byte pseudo instuction for each character in
           * the string and generate a .byte 0 instruction at the end. *)
          val bytes =
            foldr(fn (ch, acc) => I.dot_byte (Int.toString(ord ch)) :: acc)
            [I.dot_byte "0"] (explode str)

          val _ = add_static_data (I.dot_data ::
                                   I.dot_align 8 ::
                                   I.lab string_lab ::
                                   I.dot_quad(BI.pr_tag_w(BI.tag_string(true,size(str)))) ::
                                   bytes)
      in string_lab
      end

    (* Generate a Data label *)
    fun gen_data_lab lab = add_static_data [I.dot_data,
                                            I.dot_align 8,
                                            I.lab (DatLab lab),
                                            I.dot_quad (i2s BI.ml_unit)]  (* was "0" but use ml_unit instead for GC 2001-01-09, Niels *)

    fun store_aty_indexed(b:reg,n:Offset,aty,t:reg,size_ff,C) =
        let fun ea() = D(offset_bytes n,b)
            fun default() =
                move_aty_into_reg(aty,t,size_ff,
                 store_indexed(b,n,R t,C))
            fun direct_word (w:{value: Word32.word, precision:int}) : bool =
                not(boxedNum(#precision w)) andalso
                case #precision w of
                    32 => #value w <= 0wxFFFF
                  | 31 => #value w <= 0wx7FFF
                  | _ => die "store_aty_indexed.direct_word - weird precision"
            fun direct_int (i:{value: Int32.int, precision:int}) =
                not(boxedNum(#precision i)) andalso
                case #precision i of
                    32 => #value i <= 0x7FFF andalso #value i > ~0x8000
                  | 31 => #value i <= 0x3FFF andalso #value i > ~0x4000
                  | _ => die "store_aty_indexed.direct_int - weird precision"
        in
            case aty of
                SS.PHREG_ATY s => I.movq(R s,ea()) :: C
              | SS.INTEGER_ATY i => if direct_int i then
                                      move_num_generic (#precision i, fmtInt i, ea(), C)
                                    else default()
              | SS.WORD_ATY w => if direct_word w then move_num_generic (#precision w, fmtWord w, ea(), C)
                                 else default()
              | SS.UNIT_ATY => move_unit(ea(),C)
              | _ => default()
        end


    (* Can be used to update the stack or a record when the argument is an ATY *)
    (* base_reg[offset] = src_aty *)
    fun store_aty_in_reg_record(aty,t:reg,b,n:Offset,size_ff,C) =
        store_aty_indexed(b:reg,n:Offset,aty,t:reg,size_ff,C)

    (* Can be used to load from the stack or a record when destination is an ATY *)
    (* dst_aty = base_reg[offset] *)
    fun load_aty_from_reg_record(SS.PHREG_ATY dst_reg,t:reg,base_reg,offset:Offset,size_ff,C) =
          load_indexed(R dst_reg,base_reg,offset,C)
      | load_aty_from_reg_record(dst_aty,t:reg,base_reg,offset:Offset,size_ff,C) =
          load_indexed(R t,base_reg,offset,
          move_reg_into_aty(t,dst_aty,size_ff,C))

    (* base_aty[offset] = src_aty *)
    fun store_aty_in_aty_record(src_aty,base_aty,offset:Offset,t1:reg,t2:reg,size_ff,C) =
      case (src_aty,base_aty)
        of (SS.PHREG_ATY src_reg,SS.PHREG_ATY base_reg) => store_indexed(base_reg,offset,R src_reg,C)
         | (SS.PHREG_ATY src_reg,base_aty) => move_aty_into_reg(base_aty,t2,size_ff,  (* can be optimised *)
                                              store_indexed(t2,offset,R src_reg,C))
         | (src_aty,SS.PHREG_ATY base_reg) => move_aty_into_reg(src_aty,t1,size_ff,
                                              store_indexed(base_reg,offset,R t1,C))
         | (src_aty,base_aty) => move_aty_into_reg(src_aty,t1,size_ff, (* can be optimised *)
                                 move_aty_into_reg(base_aty,t2,size_ff,
                                 store_indexed(t2,offset,R t1,C)))

    (* push(aty), i.e., rsp-=8; rsp[0] = aty (different than on hp) *)
    (* size_ff is for rsp before rsp is moved. *)
    fun push_aty(aty,t:reg,size_ff,C) =
      let
        fun default() = move_aty_into_reg(aty,t,size_ff,
                         I.push(R t) :: C)
      in case aty
           of SS.PHREG_ATY aty_reg => I.push(R aty_reg) :: C
            | SS.INTEGER_ATY i =>
             if boxedNum (#precision i) then default()
             else I.push(I (fmtInt i)) :: C
            | SS.WORD_ATY w =>
               if boxedNum (#precision w) then default()
               else I.push(I (fmtWord w)) :: C
            | _ => default()
      end

    (* pop(aty), i.e., aty=rsp[0]; rsp+=8 *)
    (* size_ff is for sp after pop *)
    fun pop_aty(SS.PHREG_ATY aty_reg,t:reg,size_ff,C) = I.pop(R aty_reg) :: C
      | pop_aty(aty,t:reg,size_ff,C) = (I.pop(R t) ::
                                        move_reg_into_aty(t,aty,size_ff,C))

    (* Returns a register with arg and a continuation function. *)
    fun resolve_arg_aty(arg:SS.Aty,t:reg,size_ff:int) : reg * (I.inst list -> I.inst list) =
      case arg
        of SS.PHREG_ATY r => (r, fn C => C)
         | _ => (t, fn C => move_aty_into_reg(arg,t,size_ff,C))

    fun add_aty_to_reg(arg:SS.Aty,tmp:reg,t:reg,size_ff:int,C:I.inst list) : I.inst list =
      case arg
        of SS.PHREG_ATY r => I.addq(R r, R t) :: C
         | _ => move_aty_into_reg(arg,tmp,size_ff, I.addq(R tmp, R t) :: C)

    (* Push float on float stack *)
    fun load_float_aty(float_aty, t, size_ff, xmm_reg) =
      let val disp = if BI.tag_values() then "8"
                     else "0"
      in fn C => case float_aty
                   of SS.PHREG_ATY x => I.movsd(D(disp, x),R xmm_reg) :: C
                    | _ => move_aty_into_reg(float_aty,t,size_ff,
                           I.movsd(D(disp, t),R xmm_reg) :: C)
      end

    (* Pop float from float stack *)
    fun store_float_reg(base_reg,t:reg,xmm_reg,C) =
      if BI.tag_values() then
        store_immed(BI.tag_real false, base_reg, WORDS 0,
        I.movsd (R xmm_reg,D("8",base_reg)) :: C)   (* mael 2003-05-08 *)
      else
        I.movsd (R xmm_reg,D("0",base_reg)) :: C


    (* When tag free collection of pairs is enabled, a bit is stored
       in the region descriptor if the region is an infinite region
       holding pairs, refs, triples and arrays. Here we arrange that
       special C functions for allocating regions are called for
       regions containing pairs, refs, triples and arrays; these C
       functions then take care of setting the appropriate bit.

       Notice the difference between the function
       values_in_region_untagged being regions containing untagged
       values and the function
       regions_holding_values_of_the_same_type_only being regions
       holding values of the same type and this type is set in the
       region descriptor.*)

    fun values_in_region_untagged (place:Effect.place) : bool =
        BI.tag_values() andalso not(tag_pairs_p())
        andalso (case Effect.get_place_ty place of
                     SOME Effect.PAIR_RT => true
                   | SOME Effect.REF_RT => true
                   | SOME Effect.TRIPLE_RT => true
                   | _ => false)

    fun regions_holding_values_of_the_same_type_only (place:Effect.place) : bool =
        BI.tag_values() andalso not(tag_pairs_p())
        andalso (case Effect.get_place_ty place of
                     SOME Effect.PAIR_RT => true
                   | SOME Effect.REF_RT => true
                   | SOME Effect.TRIPLE_RT => true
                   | SOME Effect.ARRAY_RT => true
                   | _ => false)


    (***********************)
    (* Calling C Functions *)
    (***********************)

    local

      fun callc_static_or_dynamic (name : string, nargs, fnlab, C) =
          case name of
              ":" =>
              let
                  val () = die ("callc_static_or_dynamic.dynamic call not yet ported: '" ^ name ^ "'")
                  val () =
                      if nargs < 1 then
                          die "callc_static_or_dynamic: Dynamic linking requires a string as first argument."
                      else ()
                  val fp = new_dynamicFn_lab()
                  val fcall = new_dynamicFn_lab()
                  val nfcall = new_dynamicFn_lab()
                  val finish = new_dynamicFn_lab()
              in
                  I.movq (L fp, R rax) ::
                  I.cmpq (I "0",R rax) ::
                  I.je nfcall ::
                I.lab fcall ::
                  I.addq (I "8",R rsp) ::
                  I.call' (R rax) ::
                  I.jmp (L finish) ::
                I.lab nfcall ::
                  I.subq (I "8", R rsp) ::
                  I.movq (LA fp, R rdx) ::
                  I.movq (R rdx, D("0",rsp)) ::
                  I.call fnlab ::
                  I.addq (I "8", R rsp) ::
                  I.movq (L fp, R rax) ::
                  I.cmpq (I "0", R rax) ::
                  I.jne fcall::
                  I.addq (I "8", R rsp)::
                  I.call (NameLab "__raise_match")::
                  I.jmp (L finish)::
                  I.dot_data::
                  I.dot_align 8::
                  I.dot_size (fp, 8)::
                I.lab fp ::
                  I.dot_quad "0" ::
                  I.dot_text ::
                I.lab finish :: C
              end
       | _ => I.call(NameLab name) :: C
    in

    (* better alignment technique that allows for arguments on the stack *)
    fun push_args push_arg size_ff args C =
        let fun loop ([], _) = C
              | loop (arg :: rest, size_ff) = (push_arg(arg,size_ff,
                                                        loop (rest, size_ff + 1)))
        in loop(rev args, size_ff)
        end

    fun pop_args name nargs C =
        case nargs
         of 0 => C
          | n => I.addq(I (i2s (8* (case name of ":" => n-1 | _ => n))), R rsp) :: C

    local
      fun iterl f a n =
	  if n <= 0 then a
	  else iterl f (f(n,a)) (n-1)
(*
    fun iterr f a n =
	if n <= 0 then a
	else f(n, iterr f a (n-1))
*)

      (* for alignment of the stack, both tmp_reg0 and tmp_reg1 can be used *)
      fun align nargs C =
	  let val tmp = tmp_reg0
	      val tmp1 = tmp_reg1
	  in
	    I.leaq(D(i2s(8*nargs), rsp), R tmp) ::      (*  tmp = rsp + 8n; memoize esp as it should be *)
                                                        (*                  restored after call *)
	    I.subq(I(i2s(8*(nargs+5))), R rsp) ::       (*  rsp = rsp - 32 - 8 - 8n ; alignment *)
	    I.andq(I "0xFFFFFFFFFFFFFFF0", R rsp) ::    (*  rsp = rsp & 0xFFFFFFFFFFFFFFF0; alignment *)
	    I.addq(I(i2s(8*(nargs+1))), R rsp) ::       (* Make room for args to be pushed, so that once *)
	    I.push(R tmp) ::                            (*  the args are pushed, the stack is aligned *)
	    iterl (fn (i,C) =>
		     I.movq(D(i2s(~8*i), tmp), R tmp1) ::  (* notice: for x64, rsp points to the last slot used *)
		     I.push(R tmp1) :: C
		  )
		  C nargs
	  end

      fun restore_stack_alignment nargs C =
	  let val tmp = tmp_reg0
	  in I.movq(D(i2s(8*nargs), rsp), R tmp) ::  (* notice: for x64, rsp points to the last slot used *)
	     I.movq(R tmp, R rsp) ::
	     C
	  end
    in
      fun needs_align () = true
	  (* I.sysname() = "Darwin" *)

      fun maybe_align nargs F C =
          if needs_align() then
            align nargs (F (restore_stack_alignment nargs C))
          else F C
    end
(*
    fun maybe_align {even:bool} F C =                (* ME: maybe there is a better way *)
        let val tmp = I.rbx                          (* callee save scratch register *)
            fun align C =
                I.comment "ALIGN USING rbx" ::
                I.push (R rbx) ::
                I.movq(R rsp, R tmp) ::      (* tmp = rsp; memoize rsp as it should be restored after call *)
                I.subq(I "16", R rsp) ::     (* rsp = rsp - 16; alignment *)
                I.andq(I "0xFFFFFFFFFFFFFFF0", R rsp) :: (* rsp = rsp & 0xFFFFFFFFFFFFFFF0; alignment *)
                if even then C else I.subq (I "8", R rsp) :: C
            fun restore_align C =  (* restore previous stack pointer *)
                I.movq(R tmp, R rsp) ::
                I.pop (R rbx) ::
                C
            fun needs_align () = I.sysname() = "Darwin"
        in if needs_align() then
             align(F(restore_align C))
           else F C
        end
*)
    fun regs_atys nil acc = nil
      | regs_atys (SS.PHREG_ATY r::atys) acc = regs_atys atys (r::acc)
      | regs_atys (_ ::atys) acc = regs_atys atys acc

    fun member r nil = false
      | member r (x::xs) = r = x orelse member r xs
(*
    fun subst_ea s t ea =
        case ea of
            R r => if r=s then R t else ea
          | L _ => ea
          | LA _ => ea
          | I _ => ea
          | D(str,r) => if r=s then D(str,t) else ea
          | DD(str1,r1,r2,str2) =>
            if r1=s orelse r2=s then
              let val r1'=if r1=s then t else r1
                  val r2'=if r2=s then t else r2
              in DD(str1,r1',r2',str2)
              end
            else ea
*)
    (* move the first six arguments into the appropriate registers *)
    fun shuffle_args (size_ff:int)
                     (mv_aty_to_reg: SS.Aty * 'a * reg * int * I.inst list -> I.inst list)
                     (args:(SS.Aty * 'a * reg)list)
                     (C:I.inst list) : I.inst list =
        let (*val args = List.filter (fn (aty,_,r) => not(SS.eq_aty (aty,SS.PHREG_ATY r))) args*)
            val regs = regs_atys (List.map #1 args) nil
            fun loop nil acc = acc
              | loop ((aty,info,r)::args) (C,rem)=
                if not (member r regs) then
                  let val (C,rem) = loop args (C,rem)
                  in (mv_aty_to_reg (aty:SS.Aty,info:'a,r:reg,size_ff,C),rem)
                  end
                else loop args (C,(aty,info,r)::rem)
            val (C,args) = loop args (C,nil)
        in case args of
               nil => C
             | (_,_,r)::_ => die "shuffle_args: not quite done"
        end

    fun warn s = print ("** WARNING: " ^ s ^ "\n")

    (* 1. push stack arguments
       2. shuffle register arguments (adjust size_ff)
       3. align rsp (and modify location of stack arguments)
       4. make the call
       5. on return, reestablish (esp)
     *)

    fun compile_c_call_prim (name:string, args:SS.Aty list, opt_ret:SS.Aty option, size_ff:int, tmp:reg, C) =
        let fun drop n nil = nil
              | drop 0 xs = xs
              | drop n (x::xs) = drop (n-1) xs
            fun push_arg(aty,size_ff,C) = push_aty(aty,tmp,size_ff,C)
            val nargs = List.length args
(*
            val () = if nargs > List.length RI.args_reg_ccall then
                       warn ("compile_c_call_prim: at most " ^
                             Int.toString (List.length RI.args_reg_ccall) ^
                             " arguments are passed in registers - " ^ name ^ " takes " ^
                             Int.toString nargs ^ " arguments")
                     else ()
*)
            val args_stack = drop (List.length RI.args_reg_ccall) args
            val nargs_stack = List.length args_stack
            val args = ListPair.zip (args, RI.args_reg_ccall)
            val args = map (fn (x,y) => (x,(),y)) args
            fun store_ret(SOME d,C) = move_reg_into_aty(rax,d,size_ff,C)
              | store_ret(NONE,C) = C
            (* val _ = print ("CodeGen: Compiling C Call - " ^ name ^ "\n") *)
            (* With dynamic linking there must be at least one argument (the name to be bound). *)
            val dynlinklab = "localResolveLibFnManual"
            fun mv (aty,_,r,sz_ff,C) = move_aty_into_reg(aty,r,sz_ff,C)
        in shuffle_args size_ff mv args
            (push_args push_arg size_ff args_stack
              (maybe_align nargs_stack
                (fn C => callc_static_or_dynamic (name, nargs, NameLab dynlinklab, C))
                  (store_ret(opt_ret,C))))
        end

    (* Compile a C call with auto-conversion: convert ML arguments to C arguments and
     * convert the C result to an ML result. Currently supports at most 6 arguments. *)
    fun compile_c_call_auto (name,args,opt_res,size_ff,tmp,C) =
        let val args = if List.length args > List.length RI.args_reg_ccall then
                         die ("compile_c_call_auto: at most " ^
                              Int.toString (List.length RI.args_reg_ccall) ^
                              " arguments are supported")
                       else ListPair.zip (args, RI.args_reg_ccall)
            val args = List.map (fn ((x:SS.Aty,y:LS.foreign_type),z:reg) => (x,y,z)) args
            fun mov_bool ((aty,r),size_ff,C) =
                move_aty_into_reg(aty,r,size_ff,
                                  I.shrq(I "1", R r) :: C)

            fun mov_int ((aty,r),size_ff,C) =
                if BI.tag_values() then
                  move_aty_into_reg(aty,r,size_ff,
                                    I.shrq(I "1", R r) :: C)
                else
                  move_aty_into_reg(aty,r,size_ff,C)

            fun mov_foreignptr ((aty,r),size_ff,C) =
                if BI.tag_values() then
                  case aty of
                      SS.PHREG_ATY r => I.leaq(D("-1", r), R r) :: C
                    | _ => move_aty_into_reg(aty,r,size_ff,
                                             I.leaq(D("-1", r), R r) :: C)
                else move_aty_into_reg(aty,r,size_ff,C)

            fun mov_chararray ((aty,r),size_ff,C) =
                case aty of
                    SS.PHREG_ATY r' => I.leaq(D("8", r'), R r) :: C
                  | _ => move_aty_into_reg(aty,r,size_ff,
                                           I.leaq(D("8", r), R r) :: C)

            fun mov_arg (aty,ft:LS.foreign_type,r,size_ff,C) =
                let val mov_fun = case ft
                                   of LS.Bool => mov_bool
                                    | LS.Int => mov_int
                                    | LS.ForeignPtr => mov_foreignptr
                                    | LS.CharArray => mov_chararray
                                    | LS.Unit => die "CCALL_AUTO.Unit type in argument not supported"
                in mov_fun((aty,r),size_ff,C)
                end

            fun tag_bool_result (r,C) = I.leaq(DD("1", r, r, ""), R r) :: C

            fun maybe_tag_int_result (r,C) =
                if BI.tag_values() then I.leaq(DD("1", r, r, ""), R r) :: C
                else C

            fun maybe_tag_foreignptr_result (r,C) =
                if BI.tag_values() then I.leaq(D("1", r), R r) :: C
                else C

            fun convert_result ft =
                case ft of
                    LS.Bool => tag_bool_result
                  | LS.Int => maybe_tag_int_result
                  | LS.ForeignPtr => maybe_tag_foreignptr_result
                  | LS.Unit => die "convert_result.Unit already dealt with"
                  | LS.CharArray => die "convert_result.CharArray foreign type not supported in auto-conversion result"

            fun store_result ((aty,ft:LS.foreign_type), C) =
                case ft of
                    LS.Unit => C
                  | _ => convert_result ft (rax, move_reg_into_aty(rax,aty,size_ff,C))

            val dynlinklab = "localResolveLibFnAuto"
            val nargs = List.length args (* not used for static calls *)
        in shuffle_args size_ff mov_arg args
             (maybe_align 0 (fn C => callc_static_or_dynamic (name, nargs, NameLab dynlinklab,C))
               (store_result(opt_res,C)))
        end
    end

    (**********************)
    (* Garbage Collection *)
    (**********************)

    (* Put a bitvector into the code. *)
    fun gen_bv (ws,C) =
      let fun gen_bv'([],C) = C
            | gen_bv'(w::ws,C) = gen_bv'(ws,I.dot_quad ("0x"^Word32.fmt StringCvt.HEX w)::C)
      in if gc_p() then gen_bv'(ws,C)
         else C
      end

    (* reg_map is a register map describing live registers at entry to the function       *)
    (* The stub requires reg_map to reside in tmp_reg1 and the return address in tmp_reg0 *)
    fun do_gc(reg_map: Word32.word,size_ccf,size_rcf,size_spilled_region_args,C) =
      if gc_p() then
        let
          val l = new_local_lab "return_from_gc_stub"
          val reg_map_immed = "0x" ^ Word32.fmt StringCvt.HEX reg_map
          val size_ff = 0 (*dummy*)
        in
(*
          load_label_addr(time_to_gc_lab,SS.PHREG_ATY tmp_reg1,tmp_reg1,size_ff, (* tmp_reg1 = &gc_flag *)
          I.movq(D("0",tmp_reg1),R tmp_reg1) ::                       (* tmp_reg1 = gc_flag  *)
*)
          I.cmpq(I "1", L time_to_gc_lab) ::
(*          I.jmp (L l) ::  (* for disabling gc *) *)
          I.jne l ::
          I.movq(I reg_map_immed, R tmp_reg1) ::                    (* tmp_reg1 = reg_map  *)
          load_label_addr(l,SS.PHREG_ATY tmp_reg0,tmp_reg0,size_ff, (* tmp_reg0 = return address *)
          I.push(I (i2s size_ccf)) ::
          I.push(I (i2s size_rcf)) ::
          I.push(I (i2s size_spilled_region_args)) ::
          I.jmp(L gc_stub_lab) ::
          I.lab l :: C)
        end
      else C

    (*********************)
    (* Allocation Points *)
    (*********************)

    (* Status Bits Are Not Cleared! We preserve the value in register t,
     * t may be used in a call to alloc. *)

    fun reset_region(t:reg,tmp:reg,size_ff,C) =
      let val l = new_local_lab "return_from_alloc"
      in copy(t,tmp_reg1,
         I.push(LA l) ::
         I.jmp(L(NameLab "__reset_region")) ::
         I.lab l ::
         copy(tmp_reg1, t, C))
      end

    fun alloc_kill_tmp01(t:reg,n0:int,size_ff,pp:LS.pp,C) =
      let val n = if region_profiling() then n0 + BI.objectDescSizeP
                  else n0
          val l = new_local_lab "return_from_alloc"
          fun post_prof C =
            if region_profiling() then   (* tmp_reg1 now points at the object descriptor; initialize it *)
              I.movq(I (i2s pp), D("0",tmp_reg1)) ::               (* first word is pp *)
              I.movq(I (i2s n0), D("8",tmp_reg1)) ::               (* second word is object size *)
              I.leaq(D (i2s (8*BI.objectDescSizeP), tmp_reg1), R tmp_reg1) :: C  (* make tmp_reg1 point at object *)
            else C
      in
        copy(t,tmp_reg1,
        I.push(LA l) ::
        move_immed(Int32.fromInt n, R tmp_reg0,
        I.jmp(L(NameLab "__allocate")) :: (* assumes args in tmp_reg1 and tmp_reg0; result in tmp_reg1 *)
        I.lab l ::
        post_prof
        (copy(tmp_reg1,t,C))))
      end

    (* When tagging is enabled (for gc) and tag-free pairs (and triples) are enabled
     * then the following function is used for allocating pairs in
     * infinite regions. *)

    fun alloc_untagged_value_kill_tmp01(t:reg,size_alloc,size_ff,pp:LS.pp,C) =
      let val n0 = size_alloc (* size of untagged pair, e.g. *)
          val n = if region_profiling() then n0 + BI.objectDescSizeP
                  else n0
          val l = new_local_lab "return_from_alloc"
          fun post (t, C) =
            if region_profiling() then   (* tmp_reg1 now points at the object descriptor; initialize it *)
              I.movq(I (i2s pp), D("0",tmp_reg1)) ::               (* first word is pp *)
              I.movq(I (i2s n0), D("8",tmp_reg1)) ::               (* second word is object size *)
              I.leaq(D (i2s (8*(BI.objectDescSizeP-1)), tmp_reg1), R t) :: C  (* make tmp_reg1 point at
                                                                                         * word before object *)
            else
              I.leaq(D("-8",tmp_reg1), R t) :: C  (* make tmp_reg1 point at
                                                   * word before object *)
      in
        copy(t,tmp_reg1,
        I.push(LA l) ::
        move_immed(Int32.fromInt n, R tmp_reg0,
        I.jmp(L(NameLab "__allocate")) :: (* assumes args in tmp_reg1 and tmp_reg0; result in tmp_reg1 *)
        I.lab l ::
        post (t,C)))
      end

    fun set_atbot_bit(dst_reg:reg,C) =
      I.orq(I "2", R dst_reg) :: C

    fun clear_atbot_bit(dst_reg:reg,C) =
      I.btrq (I "1", R dst_reg) :: C

    fun set_inf_bit(dst_reg:reg,C) =
      I.orq(I "1", R dst_reg) :: C

    fun set_inf_bit_and_atbot_bit(dst_reg:reg,C) =
      I.orq(I "3", R dst_reg) :: C

    (* move_aty_into_reg_ap differs from move_aty_into_reg in the case where aty is a phreg! *)
    (* We must always make a copy of phreg because we may overwrite status bits in phreg.    *)
    fun move_aty_into_reg_ap(aty,dst_reg,size_ff,C) =
      case aty
        of SS.REG_I_ATY offset => base_plus_offset(rsp,BYTES(size_ff*8-offset*8-8(*+BI.inf_bit*)),dst_reg,
                                                   set_inf_bit(dst_reg,C))
         | SS.REG_F_ATY offset => base_plus_offset(rsp,WORDS(size_ff-offset-1),dst_reg,C)
         | SS.STACK_ATY offset => load_indexed(R dst_reg,rsp,WORDS(size_ff-offset-1),C)
         | SS.PHREG_ATY phreg  => copy(phreg,dst_reg, C)
         | _ => die "move_aty_into_reg_ap: ATY cannot be used to allocate memory"

    fun store_pp_prof (obj_ptr:reg, pp:LS.pp, C) =
      if region_profiling() then
        if pp < 2 then die ("store_pp_prof.pp (" ^ Int.toString pp ^ ") is less than two.")
        else I.movq(I(i2s pp), D("-16", obj_ptr)) :: C  (* two words offset *)
      else C

    fun alloc_ap_kill_tmp01(sma, dst_reg:reg, n, size_ff, C) =
      case sma
        of LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp) => C
         | LS.IGNORE => C
         | LS.ATTOP_LI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   alloc_kill_tmp01(dst_reg,n,size_ff,pp,C))
         | LS.ATTOP_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   store_pp_prof(dst_reg,pp,C))
         | LS.ATBOT_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,    (* atbot bit not set; its a finite region *)
                                   store_pp_prof(dst_reg,pp,C))
         | LS.ATTOP_FI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   alloc_kill_tmp01(dst_reg,n,size_ff,pp,C))
         | LS.ATTOP_FF(aty,pp) =>
          let val cont_lab = new_local_lab "no_alloc"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq(I "0", R dst_reg) :: (* inf bit set? *)
             I.jnc cont_lab ::
             alloc_kill_tmp01(dst_reg,n,size_ff,pp,
             I.lab cont_lab :: C))
          end
         | LS.ATBOT_LI(aty,pp) =>
          move_aty_into_reg_ap(aty,dst_reg,size_ff,
          reset_region(dst_reg,tmp_reg0,size_ff,     (* dst_reg is preserved for alloc *)
          alloc_kill_tmp01(dst_reg,n,size_ff,pp,C)))
         | LS.SAT_FI(aty,pp) =>
          let val default_lab = new_local_lab "no_reset"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq(I "1", R dst_reg) ::     (* atbot bit set? *)
             I.jnc default_lab ::
             reset_region(dst_reg,tmp_reg0,size_ff,
             I.lab default_lab ::         (* dst_reg is preverved over the call *)
             alloc_kill_tmp01(dst_reg,n,size_ff,pp,C)))
          end
         | LS.SAT_FF(aty,pp) =>
          let val finite_lab = new_local_lab "no_alloc"
              val attop_lab = new_local_lab "no_reset"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq (I "0", R dst_reg) ::  (* inf bit set? *)
             I.jnc finite_lab ::
             I.btq (I "1", R dst_reg) ::  (* atbot bit set? *)
             I.jnc attop_lab ::
             reset_region(dst_reg,tmp_reg0,size_ff,  (* dst_reg is preserved over the call *)
             I.lab attop_lab ::
             alloc_kill_tmp01(dst_reg,n,size_ff,pp,
             I.lab finite_lab :: C)))
          end

    fun alloc_untagged_value_ap_kill_tmp01 (sma, dst_reg:reg, size_alloc, size_ff, C) =
      case sma
        of LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.1"
         | LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.2"
         | LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.3"
         | LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.4"
         | LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.5"
         | LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.6"
         | LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.7"
         | LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp) => die "alloc_untagged_value_ap_kill_tmp01.8"
         | LS.IGNORE => die "alloc_untagged_value_ap_kill_tmp01.9"
         | LS.ATTOP_LI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,C))
         | LS.ATTOP_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   store_pp_prof(dst_reg,pp, C))
         | LS.ATBOT_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,    (* atbot bit not set; its a finite region *)
                                   store_pp_prof(dst_reg,pp, C))
         | LS.ATTOP_FI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,
                                   alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,C))
         | LS.ATTOP_FF(aty,pp) =>
          let val cont_lab = new_local_lab "cont"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq(I "0", R dst_reg) :: (* inf bit set? *)
             I.jnc cont_lab ::
             alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,
             I.lab cont_lab :: C))
          end
         | LS.ATBOT_LI(aty,pp) =>
          move_aty_into_reg_ap(aty,dst_reg,size_ff,
          reset_region(dst_reg,tmp_reg0,size_ff,     (* dst_reg is preserved for alloc *)
          alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,C)))
         | LS.SAT_FI(aty,pp) =>
          let val default_lab = new_local_lab "no_reset"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq(I "1", R dst_reg) ::     (* atbot bit set? *)
             I.jnc default_lab ::
             reset_region(dst_reg,tmp_reg0,size_ff,
             I.lab default_lab ::         (* dst_reg is preverved over the call *)
             alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,C)))
          end
         | LS.SAT_FF(aty,pp) =>
          let val finite_lab = new_local_lab "no_alloc"
              val attop_lab = new_local_lab "no_reset"
              val cont_lab = new_local_lab "cont"
          in move_aty_into_reg_ap(aty,dst_reg,size_ff,
             I.btq (I "0", R dst_reg) ::  (* inf bit set? *)
             I.jnc cont_lab ::
             I.btq (I "1", R dst_reg) ::  (* atbot bit set? *)
             I.jnc attop_lab ::
             reset_region(dst_reg,tmp_reg0,size_ff,  (* dst_reg is preserved over the call *)
             I.lab attop_lab ::
             alloc_untagged_value_kill_tmp01(dst_reg,size_alloc,size_ff,pp,
             I.lab cont_lab :: C)))
          end

    (* Set Atbot bits on region variables *)
    fun prefix_sm(sma,dst_reg:reg,size_ff,C) =
      case sma
        of LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp) => die "prefix_sm: DROPPED_RVAR_ATY not implemented."
         | LS.IGNORE => die "prefix_sm: IGNORE not implemented."
         | LS.ATTOP_LI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
         | LS.ATTOP_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
         | LS.ATTOP_FI(aty,pp) =>
          move_aty_into_reg_ap(aty,dst_reg,size_ff,
          clear_atbot_bit(dst_reg,C))
         | LS.ATTOP_FF(aty,pp) =>
          move_aty_into_reg_ap(aty,dst_reg,size_ff, (* It is necessary to clear atbot bit *)
          clear_atbot_bit(dst_reg,C))               (* because the region may be infinite *)
         | LS.ATBOT_LI(SS.REG_I_ATY offset_reg_i,pp) =>
          base_plus_offset(rsp,BYTES(size_ff*8-offset_reg_i*8-8(*+BI.inf_bit+BI.atbot_bit*)),dst_reg,
          set_inf_bit_and_atbot_bit(dst_reg, C))
         | LS.ATBOT_LI(aty,pp) =>
          move_aty_into_reg_ap(aty,dst_reg,size_ff,
          set_atbot_bit(dst_reg,C))
         | LS.ATBOT_LF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
         | LS.SAT_FI(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,C)
         | LS.SAT_FF(aty,pp) => move_aty_into_reg_ap(aty,dst_reg,size_ff,C)

    (* Used to build a region vector *)
    fun store_sm_in_record(sma,tmp:reg,base_reg,offset,size_ff,C) =
      case sma
        of LS.ATTOP_LI(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_LF(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_FI(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.ATTOP_FF(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.ATBOT_LI(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.ATBOT_LF(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.SAT_FI(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.SAT_FF(SS.DROPPED_RVAR_ATY,pp) => die "store_sm_in_record: DROPPED_RVAR_ATY not implemented."
         | LS.IGNORE => die "store_sm_in_record: IGNORE not implemented."
         | LS.ATTOP_LI(SS.PHREG_ATY phreg,pp) => store_indexed(base_reg,offset,R phreg,C)
         | LS.ATTOP_LI(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                  store_indexed(base_reg,offset,R tmp,C))
         | LS.ATTOP_LF(SS.PHREG_ATY phreg,pp) => store_indexed(base_reg,offset,R phreg,C)
         | LS.ATTOP_LF(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                  store_indexed(base_reg,offset,R tmp,C))
         | LS.ATTOP_FI(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                  clear_atbot_bit(tmp,
                                  store_indexed(base_reg,offset,R tmp,C)))
         | LS.ATTOP_FF(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                  clear_atbot_bit(tmp,                   (* The region may be infinite *)
                                  store_indexed(base_reg,offset,R tmp,C))) (* so we clear the atbot bit *)
         | LS.ATBOT_LI(SS.REG_I_ATY offset_reg_i,pp) =>
          base_plus_offset(rsp,BYTES(size_ff*8-offset_reg_i*8-8(*+BI.inf_bit+BI.atbot_bit*)),tmp,
          set_inf_bit_and_atbot_bit(tmp,
          store_indexed(base_reg,offset,R tmp,C)))
         | LS.ATBOT_LI(aty,pp) =>
          move_aty_into_reg_ap(aty,tmp,size_ff,
          set_atbot_bit(tmp,
          store_indexed(base_reg,offset,R tmp,C)))
         | LS.ATBOT_LF(SS.PHREG_ATY phreg,pp) =>
          store_indexed(base_reg,offset,R phreg,C) (* The region is finite so no atbot bit is necessary *)
         | LS.ATBOT_LF(aty,pp) =>
          move_aty_into_reg_ap(aty,tmp,size_ff,
          store_indexed(base_reg,offset,R tmp,C))
         | LS.SAT_FI(SS.PHREG_ATY phreg,pp) =>
          store_indexed(base_reg,offset,R phreg,C) (* The storage bit is already recorded in phreg *)
         | LS.SAT_FI(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                store_indexed(base_reg,offset,R tmp,C))
         | LS.SAT_FF(SS.PHREG_ATY phreg,pp) =>
          store_indexed(base_reg,offset,R phreg,C) (* The storage bit is already recorded in phreg *)
         | LS.SAT_FF(aty,pp) => move_aty_into_reg_ap(aty,tmp,size_ff,
                                store_indexed(base_reg,offset,R tmp,C))

    fun force_reset_aux_region_kill_tmp0(sma,t:reg,size_ff,C) =
      let fun do_reset(aty,pp) = move_aty_into_reg_ap(aty,t,size_ff,
                                  reset_region(t,tmp_reg0,size_ff,C))
          fun maybe_reset(aty,pp) =
            let val default_lab = new_local_lab "no_reset"
            in move_aty_into_reg_ap(aty,t,size_ff, (* We check the inf bit but not the storage mode *)
               I.btq(I "0", R t) ::                (* Is region infinite? kill tmp_reg0. *)
               I.jnc default_lab ::
               reset_region(t,tmp_reg0,size_ff,
               I.lab default_lab :: C))
            end
      in case sma
           of LS.ATTOP_LI(aty,pp) => do_reset(aty,pp)
            | LS.ATTOP_LF _ => C
            | LS.ATTOP_FI(aty,pp) => do_reset(aty,pp)
            | LS.ATTOP_FF(aty,pp) => maybe_reset(aty,pp)
            | LS.ATBOT_LI(aty,pp) => do_reset(aty,pp)
            | LS.ATBOT_LF _ => C
            | LS.SAT_FI(aty,pp) => do_reset(aty,pp)     (* We do not check the storage mode *)
            | LS.SAT_FF(aty,pp) => maybe_reset(aty,pp)
            | LS.IGNORE => C
      end

      fun maybe_reset_aux_region_kill_tmp0(sma,t:reg,size_ff,C) =
        case sma
          of LS.ATBOT_LI(aty,pp) => move_aty_into_reg_ap(aty,t,size_ff,
                                    reset_region(t,tmp_reg0,size_ff,C))
           | LS.SAT_FI(aty,pp) =>
            let val default_lab = new_local_lab "no_reset"
            in move_aty_into_reg_ap(aty,t,size_ff,
               I.btq(I "1", R t) :: (* Is storage mode atbot? kill tmp_reg0. *)
               I.jnc default_lab ::
               reset_region(t,tmp_reg0,size_ff,
               I.lab default_lab :: C))
            end
           | LS.SAT_FF(aty,pp) =>
            let val default_lab = new_local_lab "no_reset"
            in move_aty_into_reg_ap(aty,t,size_ff,
               I.btq (I "0", R t) ::  (* Is region infinite? *)
               I.jnc default_lab ::
               I.btq (I "1", R t) ::  (* Is atbot bit set? *)
               I.jnc default_lab ::
               reset_region(t,tmp_reg0,size_ff,
               I.lab default_lab :: C))
            end
           | _ => C

      (* Compile Switch Statements *)
      local
        fun new_label str = new_local_lab str
        fun label(lab,C) = I.lab lab :: C
        fun jmp(lab,C) = I.jmp(L lab) :: rem_dead_code C
        fun inline_cont C =
          case C
            of (i as I.jmp _) :: _ => SOME (fn C => i :: rem_dead_code C)
             | _ => NONE
      in
        fun binary_search(sels,
                          default,
                          opr: I.ea,
                          compile_insts,
                          toInt : 'a -> Int32.int,
                          C) =
          let
            val sels = map (fn (i,e) => (toInt i, e)) sels
            fun if_not_equal_go_lab (lab,i,C) = I.cmpq(I (intToStr i),opr) :: I.jne lab :: C
            fun if_less_than_go_lab (lab,i,C) = I.cmpq(I (intToStr i),opr) :: I.jl lab :: C
            fun if_greater_than_go_lab (lab,i,C) = I.cmpq(I (intToStr i),opr) :: I.jg lab :: C
          in
            if jump_tables then
              JumpTables.binary_search_new
              (sels,
               default,
               comment,
               new_label,
               if_not_equal_go_lab,
               if_less_than_go_lab,
               if_greater_than_go_lab,
               compile_insts,
               label,
               jmp,
               fn (sel1,sel2) => Int32.abs(sel1-sel2), (* sel_dist *)
               fn (lab,sel,_,C) => (I.movq(opr, R tmp_reg0) ::
                                    I.salq(I "3", R tmp_reg0) ::
                                    I.push(R tmp_reg1) ::
                                    I.movq(LA lab,R tmp_reg1) ::
                                    I.addq(R tmp_reg1, R tmp_reg0) ::
                                    I.pop(R tmp_reg1) ::
                                    I.jmp(D(intToStr(~8*sel), tmp_reg0)) ::
                                    rem_dead_code C),
               fn (lab,C) => I.dot_quad (I.pr_lab lab) :: C, (*add_label_to_jump_tab*)
               I.eq_lab,
               inline_cont,
               C)
            else
              JumpTables.linear_search_new(sels,
                                           default,
                                           comment,
                                           new_label,
                                           if_not_equal_go_lab,
                                           compile_insts,
                                           label,
                                           jmp,
                                           inline_cont,
                                           C)
          end
      end

      (* Compile switches on constructors, integers, and words *)
      fun compileNumSwitch {size_ff,size_ccf,CG_lss,toInt,opr_aty,oprBoxed,sels,default,C} =
        let
          val (opr_reg, F) =
            case opr_aty
              of SS.PHREG_ATY r => (r, fn C => C)
               | _ => (tmp_reg1, fn C => move_aty_into_reg(opr_aty,tmp_reg1,size_ff, C))
          val opr = if oprBoxed then D("8", opr_reg)   (* boxed representation of nums *)
                    else R opr_reg                     (* unboxed representation of nums *)
        in
          F (binary_search(sels,
                           default,
                           opr,
                           fn (lss,C) => CG_lss(lss,size_ff,size_ccf,C), (* compile_insts *)
                           toInt,
                           C))
        end


      fun cmpi_kill_tmp01 {box} (jump,x,y,d,size_ff,C) =
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            val true_lab = new_local_lab "true"
            val cont_lab = new_local_lab "cont"
            fun compare C =
              if box then
                I.movq(D("8",y_reg), R tmp_reg1) ::
                I.movq(D("8",x_reg), R tmp_reg0) ::
                I.cmpl(R (I.doubleOfQuadReg tmp_reg1),
                       R (I.doubleOfQuadReg tmp_reg0)) :: C
              else I.cmpl(R (I.doubleOfQuadReg y_reg),
                          R (I.doubleOfQuadReg x_reg)) :: C
        in
           x_C(
           y_C(
           compare (
           jump true_lab ::
           I.movq(I (i2s BI.ml_false), R d_reg) ::
           I.jmp(L cont_lab) ::
           I.lab true_lab ::
           I.movq(I (i2s BI.ml_true), R d_reg) ::
           I.lab cont_lab :: C')))
        end

      fun cmpi_and_jmp_kill_tmp01(jump,x,y,lab_t,lab_f,size_ff,C) =
        let
          val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
          val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
        in
          x_C(y_C(
          I.cmpl(R (I.doubleOfQuadReg y_reg), R (I.doubleOfQuadReg x_reg)) ::
          jump lab_t ::
          I.jmp (L lab_f) :: rem_dead_code C))
        end

      (* version with boxed arguments; assume tagging is enabled *)
      fun cmpbi_and_jmp_kill_tmp01(jump,x,y,lab_t,lab_f,size_ff,C) =
        if BI.tag_values() then
          let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
              val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
          in
            x_C(y_C(
            I.movq(D("8", y_reg), R tmp_reg1) ::
            I.movq(D("8", x_reg), R tmp_reg0) ::
            I.cmpl(R (I.doubleOfQuadReg tmp_reg1),
                   R (I.doubleOfQuadReg tmp_reg0)) ::
            jump lab_t ::
            I.jmp (L lab_f) :: rem_dead_code C))
          end
        else die "cmpbi_and_jmp_kill_tmp01: tagging disabled!"

      fun jump_overflow C = I.jo (NameLab "__raise_overflow") :: C

      fun sub_num_kill_tmp01 {ovf : bool, tag: bool} (x,y,d,size_ff,C) =
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            fun check_ovf C = if ovf then jump_overflow C else C
            fun do_tag C = if tag then I.addq(I "1",R d_reg) :: check_ovf C   (* check twice *)
                           else C
        in
          x_C(y_C(
          copy(y_reg, tmp_reg1,
          copy(x_reg, d_reg,
          I.subl(R (I.doubleOfQuadReg tmp_reg1),
                 R (I.doubleOfQuadReg d_reg)) ::
          check_ovf (do_tag C')))))
        end

      fun add_num_kill_tmp01 {ovf,tag} (x,y,d,size_ff,C) =  (* Be careful - when tag and ovf, add may
                                                             * raise overflow when it is not supposed
                                                             * to, if one is not careful! sub_num above
                                                             * is ok, I think! mael 2001-05-19 *)
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            fun check_ovf C = if ovf then jump_overflow C else C
            fun do_tag C = if tag then I.addl(I "-1", R (I.doubleOfQuadReg d_reg)) :: check_ovf C
                           else C
        in if tag andalso ovf then
             (x_C(y_C(
              copy(y_reg, tmp_reg1, I.sarl(I "1", R (I.doubleOfQuadReg tmp_reg1)) ::    (* t1 = untag y *)
              copy(x_reg, tmp_reg0, I.sarl(I "1", R (I.doubleOfQuadReg tmp_reg0)) ::    (* t0 = untag x *)
              I.addl(R (I.doubleOfQuadReg tmp_reg0),
                     R (I.doubleOfQuadReg tmp_reg1)) ::             (* t1 = t1 + t0 *)
              copy(tmp_reg1, d_reg,
              I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::         (* d = tag d *)
              I.sarl(I "1", R (I.doubleOfQuadReg d_reg)) ::         (* d = untag d *)
              I.cmpl(R (I.doubleOfQuadReg d_reg),
                     R (I.doubleOfQuadReg tmp_reg1)) ::
              I.jne (NameLab "__raise_overflow") ::
              I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::         (* d = tag d *)
              C'))))))
           else
             (x_C(y_C(
              copy(y_reg, tmp_reg1,
              copy(x_reg, d_reg,
              I.addl(R (I.doubleOfQuadReg tmp_reg1),
                     R (I.doubleOfQuadReg d_reg)) ::
              check_ovf (do_tag C'))))))
        end

      fun mul_num_kill_tmp01 {ovf,tag} (x,y,d,size_ff,C) = (* does (1 * valOf Int31.minInt) raise Overflow ? *)
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            fun check_ovf C = if ovf then jump_overflow C else C
        in x_C(y_C(
           copy(y_reg, tmp_reg1,
           copy(x_reg, d_reg,
           if tag then (* A[i*j] = 1 + (A[i] >> 1) * (A[j]-1) *)
                I.sarl(I "1", R (I.doubleOfQuadReg d_reg)) ::
                I.subl(I "1", R (I.doubleOfQuadReg tmp_reg1)) ::
                I.imull(R (I.doubleOfQuadReg tmp_reg1),
                        R (I.doubleOfQuadReg d_reg)) ::
                check_ovf (
                I.addl(I "1", R (I.doubleOfQuadReg d_reg)) ::
                check_ovf C')
           else
             I.imull(R (I.doubleOfQuadReg tmp_reg1),
                     R (I.doubleOfQuadReg d_reg)) ::
             check_ovf C'))))
        end

      fun neg_int_kill_tmp0 {tag} (x,d,size_ff,C) =
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            fun do_tag C = if tag then I.addl(I "2", R (I.doubleOfQuadReg d_reg)) ::
                                       jump_overflow C
                           else C
        in x_C(copy(x_reg, d_reg,
           I.negl (R (I.doubleOfQuadReg d_reg)) ::
           jump_overflow (
           do_tag C')))
        end

      fun neg_int32b_kill_tmp0 (b,x,d,size_ff,C) =
        if not(BI.tag_values()) then die "neg_int32b_kill_tmp0.tagging required"
        else
          let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
              val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
          in x_C(
             load_indexed(R tmp_reg0,x_reg,WORDS 1,
             I.negl(R (I.doubleOfQuadReg tmp_reg0)) ::
             jump_overflow (
             move_aty_into_reg(b,d_reg,size_ff,
             store_indexed(d_reg,WORDS 1, R tmp_reg0,                        (* store negated value *)
             store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))   (* store tag *)
          end

     fun abs_int_kill_tmp0 {tag} (x,d,size_ff,C) =
       let val cont_lab = new_local_lab "cont"
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun do_tag C = if tag then I.addl(I "2", R (I.doubleOfQuadReg d_reg)) ::
                                      jump_overflow C
                          else C
       in
         x_C(copy(x_reg,d_reg,
         I.cmpl(I "0", R (I.doubleOfQuadReg d_reg)) ::
         I.jge cont_lab ::
         I.negl (R (I.doubleOfQuadReg d_reg)) ::
         jump_overflow (
         do_tag (
         I.lab cont_lab :: C'))))
       end


     fun abs_int32b_kill_tmp0 (b,x,d,size_ff,C) =
       let val cont_lab = new_local_lab "cont"
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
       in
         x_C(
         load_indexed(R tmp_reg0,x_reg,WORDS 1,
         I.cmpl(I "0", R (I.doubleOfQuadReg tmp_reg0)) ::
         I.jge cont_lab ::
         I.negl (R (I.doubleOfQuadReg tmp_reg0)) ::
         jump_overflow (
         I.lab cont_lab ::
         move_aty_into_reg(b,d_reg,size_ff,
         store_indexed(d_reg, WORDS 1, R tmp_reg0,                       (* store negated value *)
         store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))   (* store tag *)
       end

     fun word32ub_to_int32ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(copy(x_reg, d_reg,
                   I.btq(I "31", R d_reg) ::     (* sign bit set? *)
                   I.jc (NameLab "__raise_overflow") :: C'))
       end

     fun num31_to_num32ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(copy(x_reg, d_reg, I.sarl (I "1", R (I.doubleOfQuadReg d_reg)) :: C'))
       end

     fun int32_to_int31 {boxedarg} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
       in x_C(
          maybe_unbox(
          I.imull(I "2", R (I.doubleOfQuadReg d_reg)) ::
          jump_overflow (
          I.addq(I "1", R d_reg) :: C')))   (* No need to check for overflow after adding 1; the
                                             * intermediate result is even (after multiplying
                                             * with 2) so adding one cannot give Overflow because the
                                             * largest integer is odd! mael 2001-04-29 *)
       end

     fun word32_to_int31 {boxedarg,ovf} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
           fun check_ovf C =
             if ovf then
               I.btq(I "30", R d_reg) ::
               I.jc (NameLab "__raise_overflow") ::
               C
             else C
       in x_C(
          maybe_unbox(
          check_ovf(
          I.imull(I "2", R (I.doubleOfQuadReg d_reg)) ::
          jump_overflow (
          I.addq(I "1", R d_reg) :: C'))))   (* No need to check for overflow after adding 1; the
                                              * intermediate result is even (after multiplying
                                              * with 2) so adding one cannot give Overflow because the
                                              * largest integer is odd! mael 2001-04-29 *)
       end

     fun word32_to_word31 {boxedarg} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
       in x_C(
          maybe_unbox(
(*
          I.salq(I "1", R d_reg) ::
          I.addq(I "1", R d_reg) ::
*)        I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
          C'))
       end

     fun bin_float_op_kill_tmp01 finst (x,y,b,d,size_ff,C) =
       let val x_C = load_float_aty(x, tmp_reg0, size_ff, xmm1)
           val y_C = load_float_aty(y, tmp_reg0, size_ff, xmm0)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         y_C(x_C(finst(R xmm0,R xmm1) ::
         b_C(store_float_reg(b_reg,tmp_reg1,xmm1,
         copy(b_reg,d_reg, C')))))
       end

     fun addf_kill_tmp01 a = bin_float_op_kill_tmp01 I.addsd a
     fun subf_kill_tmp01 a = bin_float_op_kill_tmp01 I.subsd a
     fun mulf_kill_tmp01 a = bin_float_op_kill_tmp01 I.mulsd a
     fun divf_kill_tmp01 a = bin_float_op_kill_tmp01 I.divsd a

     fun negf_kill_tmp01 (b,x,d,size_ff,C) =
       let val x_C = load_float_aty(x, tmp_reg0, size_ff,xmm1)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         x_C(I.xorps (R xmm0,R xmm0) :: I.subsd (R xmm1,R xmm0) ::
         b_C(store_float_reg(b_reg,tmp_reg1,xmm0,
         copy(b_reg,d_reg, C'))))
       end

     fun absf_kill_tmp01 (b,x,d,size_ff,C) =
       let val x_C = load_float_aty(x, tmp_reg0, size_ff,xmm1)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         x_C(I.xorps (R xmm0,R xmm0) :: I.subsd (R xmm1,R xmm0) :: I.maxsd (R xmm1,R xmm0) ::
         b_C(store_float_reg(b_reg,tmp_reg1,xmm0,
         copy(b_reg,d_reg, C'))))
       end

     datatype cond = LESSTHAN | LESSEQUAL | GREATERTHAN | GREATEREQUAL

     fun cmpf_kill_tmp01 (cond,x,y,d,size_ff,C) = (* ME MEMO *)
       let val x_C = load_float_aty(x, tmp_reg0, size_ff, xmm0)
           val y_C = load_float_aty(y, tmp_reg0, size_ff, xmm1)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
           val true_lab = new_local_lab "true"
           val cont_lab = new_local_lab "cont"
           val jump = (* from gcc experiments *)
               case cond of
                   LESSTHAN => I.jb      (*below*)
                 | LESSEQUAL => I.jbe    (*below or equal*)
                 | GREATERTHAN => I.ja   (*above*)
                 | GREATEREQUAL => I.jae (*above or equal*)
           val load_args = x_C o y_C
       in
         load_args(I.ucomisd (R xmm1, R xmm0) ::
         jump true_lab ::
         I.movq(I (i2s BI.ml_false), R d_reg) ::
         I.jmp(L cont_lab) ::
         I.lab true_lab ::
         I.movq(I (i2s BI.ml_true), R d_reg) ::
         I.lab cont_lab ::
         C')
       end

     fun bin_op_kill_tmp01 inst (x,y,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
       in
         x_C(y_C(
         copy(y_reg, tmp_reg1,
         copy(x_reg, d_reg,
         inst(R (I.doubleOfQuadReg tmp_reg1),
              R (I.doubleOfQuadReg d_reg)) :: C'))))
       end

     (* andb and orb are the same for 31 bit (tagged) and
      * 32 bit (untagged) representations *)
     fun andb_word_kill_tmp01 a = bin_op_kill_tmp01 I.andl a   (* A[x&y] = A[x] & A[y]  tagging *)
     fun orb_word_kill_tmp01 a = bin_op_kill_tmp01 I.orl a     (* A[x|y] = A[x] | A[y]  tagging *)

     (* xorb needs to set the lowest bit for the 31 bit (tagged) version *)
     fun xorb_word_kill_tmp01 {tag} (x,y,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
           fun do_tag C = if tag then I.orq(I "1", R d_reg) :: C else C
       in
         x_C(y_C(
         copy(y_reg, tmp_reg1,
         copy(x_reg, d_reg,
         I.xorl(R (I.doubleOfQuadReg tmp_reg1),
                R (I.doubleOfQuadReg d_reg)) ::
         do_tag C'))))
       end

     fun bin_op_w32boxed__ {ovf} inst (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       if not(BI.tag_values()) then die "bin_op_w32boxed__.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
             fun check_ovf C = if ovf then jump_overflow C else C
         in
           x_C(
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           y_C(
           load_indexed(R tmp_reg1,y_reg,WORDS 1,
           inst(R (I.doubleOfQuadReg tmp_reg0),
                R (I.doubleOfQuadReg tmp_reg1)) ::
           check_ovf (
           move_aty_into_reg(r,d_reg,size_ff,
           store_indexed(d_reg,WORDS 1,R tmp_reg1,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))))  (* store tag *)
         end

     fun addw32boxed(r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.addl (r,x,y,d,size_ff,C)

     fun subw32boxed(r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.subl (r,y,x,d,size_ff,C) (* x and y swapped, see spec for subq *)

     fun mulw32boxed(r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.imull (r,x,y,d,size_ff,C)

     fun orw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.orl (r,x,y,d,size_ff,C)

     fun andw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.andl (r,x,y,d,size_ff,C)

     fun xorw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       bin_op_w32boxed__ {ovf=false} I.xorl (r,x,y,d,size_ff,C)

     fun mul_int32b (b,x,y,d,size_ff,C) =
       bin_op_w32boxed__ {ovf=true} I.imull (b,x,y,d,size_ff,C)

     fun sub_int32b (b,x,y,d,size_ff,C) =
       bin_op_w32boxed__ {ovf=true} I.subl (b,y,x,d,size_ff,C)

     fun add_int32b (b,x,y,d,size_ff,C) =
       bin_op_w32boxed__ {ovf=true} I.addl (b,x,y,d,size_ff,C)

     fun num31_to_num32b(b,x,d,size_ff,C) =   (* a boxed word is tagged as a scalar record *)
       if BI.tag_values() then
         let val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
         in
           move_aty_into_reg(x,tmp_reg0,size_ff,
           I.sarl(I "1", R (I.doubleOfQuadReg tmp_reg0)) ::
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg,WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))   (* store tag *)
         end
       else die "num31_to_num32b.tagging_disabled"

     fun num32b_to_num32b {ovf:bool} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "num32b_to_num32b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
             fun check_ovf C =
               if ovf then
                 I.btq(I "31", R tmp_reg0) ::     (* sign bit set? *)
                 I.jc (NameLab "__raise_overflow") :: C
               else C
         in
           x_C (
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           check_ovf (
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg, WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))  (* store tag *)
         end

     fun shift_w32boxed__ inst (r,x,y,d,size_ff,C) =
       if not(BI.tag_values()) then die "shift_w32boxed__.tagging is not enabled as required"
       else
       (* y is unboxed and tagged *)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
       in
         x_C(
         load_indexed(R tmp_reg1,x_reg,WORDS 1,
         copy(rcx, tmp_reg0,                         (* save rcx *)
         y_C(
         copy(y_reg,rcx,                             (* tmp_reg0 = %r10, see InstsX64.sml *)
         I.sarq (I "1", R rcx) ::                    (* untag y: y >> 1 *)
         inst(R cl, R (I.doubleOfQuadReg tmp_reg1)) ::
         copy(tmp_reg0, rcx,                         (* restore rcx *)
         move_aty_into_reg(r,d_reg,size_ff,
         store_indexed(d_reg,WORDS 1, R tmp_reg1,
         store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))))))   (* store tag *)
       end

     fun shift_leftw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_w32boxed__ I.sall (r,x,y,d,size_ff,C)

     fun shift_right_signedw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_w32boxed__ I.sarl (r,x,y,d,size_ff,C)

     fun shift_right_unsignedw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_w32boxed__ I.shrl (r,x,y,d,size_ff,C)

     fun shift_left_word_kill_tmp01 {tag} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10*)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
       in
         if tag then                     (* 1 + ((x - 1) << (y >> 1)) *)
           x_C(
           I.movq(R rcx, R tmp_reg0) ::                       (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.decq (R tmp_reg1) ::                             (* x - 1  *)
           untag_y (                                          (* y >> 1 *)
           I.sall (R cl, R (I.doubleOfQuadReg tmp_reg1)) ::   (*   <<   *)
           I.movq (R tmp_reg0, R rcx) ::                      (* restore rcx *)
           I.incq (R tmp_reg1) ::           (* 1 +    *)
           copy(tmp_reg1, d_reg, C'))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.sall(R cl, R (I.doubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun shift_right_signed_word_kill_tmp01 {tag} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10*)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
       in
         if tag then                         (* 1 | ((x) >> (y >> 1)) *)
           x_C(
           I.movq(R rcx, R tmp_reg0) ::                        (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.decq (R tmp_reg1) ::                              (* x - 1  *)
           untag_y (                                           (* y >> 1 *)
           I.sarl (R cl, R (I.doubleOfQuadReg tmp_reg1)) ::    (* x >>   *)
           copy(tmp_reg0, rcx,                                 (* restore rcx *)
           I.orq (I "1", R tmp_reg1) ::                        (* 1 |    *)
           copy(tmp_reg1, d_reg, C')))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.sarl(R cl, R (I.doubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                 (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun shift_right_unsigned_word_kill_tmp01 {tag} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10 *)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
       in
         if tag then                                           (* 1 | ((unsigned long)(x) >> (y >> 1)) *)
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           untag_y (                                           (* y >> 1                *)
           I.shrl (R cl, R (I.doubleOfQuadReg tmp_reg1)) ::    (* (unsigned long)x >>   *)
           I.orq (I "1", R tmp_reg1) ::                        (* 1 |                   *)
           copy(tmp_reg0, rcx,
           copy(tmp_reg1, d_reg, C'))))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.shrl(R cl, R (I.doubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                 (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun bytetable_sub(t,i,d,size_ff,C) =
       let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg1,size_ff)
           val (i_reg,i_C) = resolve_arg_aty(i,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* i is represented tagged only when BI.tag_values() is true *)
       in if BI.tag_values() then
            t_C(i_C(
            copy(i_reg, rcx,                 (* tmp_reg0 = %rcx *)
            I.sarq (I "1", R rcx) ::         (* i >> 1 *)
            I.movzbq(DD("8",t_reg,rcx,"1"), R d_reg) ::
            I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
            C')))
          else
            t_C(i_C(
            I.movzbq(DD("8",t_reg,i_reg,"1"), R d_reg) ::
            C'))
       end

     fun resolve_args(atys,ts,size_ff) =
       case atys
         of nil => SOME (nil, fn C => C)
          | SS.PHREG_ATY r :: atys =>
           (case resolve_args(atys,ts,size_ff)
              of SOME (rs,F) => SOME (r::rs,F)
               | NONE => NONE)
          | aty :: atys =>
              (case ts
                 of nil => NONE
                  | t::ts =>
                   (case resolve_args(atys,ts,size_ff)
                      of SOME (rs,F) => SOME (t::rs, fn C => F(move_aty_into_reg(aty,t,size_ff,C)))
                       | NONE => NONE))

     fun bytetable_update(t,i,x,d,size_ff,C) =
       if BI.tag_values() then
         let
           (* i, x are represented tagged only when BI.tag_values() is true *)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
         in
            move_aty_into_reg(t,tmp_reg0,size_ff,               (* tmp_reg0 = t *)
            move_aty_into_reg(i,tmp_reg1,size_ff,               (* tmp_reg1 = i *)
            I.sarq (I "1", R tmp_reg1) ::                       (* untag i: tmp_reg1 >> 1 *)
            I.addq(R tmp_reg0, R tmp_reg1) ::                   (* tmp_reg1 += tmp_reg0 *)
            move_aty_into_reg(x,tmp_reg0,size_ff,               (* tmp_reg0 (%r10) = x *)
            I.sarq (I "1", R tmp_reg0) ::                       (* untag x: tmp_reg0 >> 1 *)
            I.movb(R r10b, D("8", tmp_reg1)) ::                 (* *(tmp_reg1+8) = %r10b *)
            move_immed(Int32.fromInt BI.ml_unit, R d_reg,       (* d = () *)
            C'))))
         end
       else
         (case resolve_args([t,i],[tmp_reg1],size_ff)
            of SOME ([t_reg,i_reg],F) =>
              F(
              move_aty_into_reg(x,tmp_reg0,size_ff,
              I.movb(R r10b, DD("8", t_reg, i_reg, "1")) ::    (*tmp_reg0==%r10*)
              C))
             | SOME _ => die "bytetable_update"
             | NONE =>
              move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
              move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
              I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
              move_aty_into_reg(x,tmp_reg0,size_ff,            (* tmp_reg0 (%rcx) = x *)
              I.movb(R r10b, D("8", tmp_reg1)) ::              (* *(tmp_reg1+8) = %r10b *)
              C))))

     fun bytetable_size(t,d,size_ff,C) =
       let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
       in if BI.tag_values() then
            t_C(
            I.movq(D("0",t_reg), R d_reg) ::
            I.sarq (I "6", R d_reg) ::         (* d >> 6: remove tag (Tagging.h) *)
(*
            I.salq(I "1", R d_reg) ::          (* d = tag d *)
            I.addq(I "1", R d_reg) ::
*)          I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
            C')
          else
            t_C(
            I.movq(D("0",t_reg), R d_reg) ::
            I.sarq (I "6", R d_reg) ::         (* d >> 6: remove tag (Tagging.h) *)
            C')
       end

     fun word_sub0(t,i,d,size_ff,C) =
       let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg1,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* i is represented tagged only when BI.tag_values() is true *)
       in if BI.tag_values() then
            t_C(
            move_aty_into_reg(i,tmp_reg0,size_ff,
            I.sarq (I "1", R tmp_reg0) ::         (* i >> 1 *)
            I.movq(DD("8",t_reg,tmp_reg0,"8"), R d_reg) ::
            C'))
          else
            let val (i_reg,i_C) = resolve_arg_aty(i,tmp_reg0,size_ff)
            in
              t_C(i_C(
              I.movq(DD("8",t_reg,i_reg,"8"), R d_reg) ::
              C'))
            end
       end

     fun word_update0(t,i,x,d,size_ff,C) =
       if BI.tag_values() then
         let
           (* i, x are represented tagged only when BI.tag_values() is true *)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
         in
            case resolve_args([t,x],[tmp_reg1], size_ff)
              of SOME ([t_reg,x_reg], F) =>
                F(move_aty_into_reg(i,tmp_reg0,size_ff,
                  I.sarq (I "1", R tmp_reg0) ::
                  I.movq(R x_reg, DD("8", t_reg, tmp_reg0, "8")) ::
                  move_immed(Int32.fromInt BI.ml_unit, R d_reg,
                  C')))
               | SOME _ => die "word_update0_1"
               | NONE =>
                (move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
                 I.sarq(I "1", R tmp_reg1) ::                     (* untag i: tmp_reg1 >> 1 *)
                 I.salq(I "2", R tmp_reg1) ::                     (* i << 2 *)
                 move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
                 I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
                 move_aty_into_reg(x,tmp_reg0,size_ff,            (* tmp_reg0 = x *)
                 I.movq(R tmp_reg0, D("8", tmp_reg1)) ::          (* *(tmp_reg1+8) = tmp_reg0 *)
                 move_immed(Int32.fromInt BI.ml_unit, R d_reg,    (* d = () *)
                 C')))))
         end
       else
         (case resolve_args([t,i,x],[tmp_reg0,tmp_reg1], size_ff)
            of SOME ([t_reg,i_reg,x_reg], F) =>
              F(I.movq(R x_reg, DD("8", t_reg, i_reg, "8")) :: C)
             | SOME _ => die "word_update0_2"
             | NONE =>
              move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
              I.imulq(I "4", R tmp_reg1) ::                    (* i << 2 *)
              move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
              I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
              move_aty_into_reg(x,tmp_reg0,size_ff,            (* tmp_reg0 = x *)
              I.movq(R tmp_reg0, D("8", tmp_reg1)) ::          (* *(tmp_reg1+8) = tmp_reg0 *)
              C))))

     fun table_size a = bytetable_size a

     (*******************)
     (* Code Generation *)
     (*******************)

     (* printing an assignment *)
     fun debug_assign(str,C) = C
(*      if Flags.is_on "debug_codeGen" then
      let
        val string_lab = gen_string_lab (str ^ "\n")
      in
        COMMENT "Start of Debug Assignment" ::
        load_label_addr_kill_gen1(string_lab,SS.PHREG_ATY arg0,0,
                        compile_c_call_prim("printString",[SS.PHREG_ATY arg0],NONE,0,tmp_reg0 (*not used*),
                                            COMMENT "End of Debug Assignment" :: C))
      end
      else C*)

     fun CG_lss(lss,size_ff,size_ccf,C) =
       let
         fun pr_ls ls = LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls
         fun CG_ls(ls,C) =
           (case ls
              of LS.ASSIGN{pat=SS.FLOW_VAR_ATY(lv,lab_t,lab_f),
                           bind=LS.CON0{con,con_kind,aux_regions=[],alloc=LS.IGNORE}} =>
                if Con.eq(con,Con.con_TRUE) then I.jmp(L(LocalLab lab_t)) :: rem_dead_code C
                else
                  if Con.eq(con,Con.con_FALSE) then I.jmp(L(LocalLab lab_f)) :: rem_dead_code C
                  else die "CG_lss: unmatched assign on flow variable"
               | LS.ASSIGN{pat,bind} =>
                debug_assign(""(*pr_ls ls*),
                comment_fn (fn () => "ASSIGN: " ^ pr_ls ls,
                (case bind
                   of LS.ATOM src_aty => move_aty_to_aty(src_aty,pat,size_ff,C)
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
                                              I.lab float_lab,
                                              I.dot_quad(BI.pr_tag_w(BI.tag_real(true))),
                                              I.dot_double str]
                           else
                             add_static_data [I.dot_data,
                                              I.lab float_lab,
                                              I.dot_double str]
                     in load_label_addr(float_lab,pat,tmp_reg1,size_ff,C)
                     end
                    | LS.CLOS_RECORD{label,elems=elems as (lvs,excons,rhos),alloc} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length (LS.smash_free elems)
                         val n_skip = length rhos + 1 (* We don't traverse region pointers,
                                                       * i.e. we skip rhos+1 fields *)
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
                    | LS.SCLOS_RECORD{elems=elems as (lvs,excons,rhos),alloc} =>
                     let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                         val num_elems = List.length (LS.smash_free elems)
                         val n_skip = length rhos (* We don't traverse region pointers *)
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
                    | LS.SELECT(i,aty) =>
                     if BI.tag_values() then
                       move_index_aty_to_aty(aty,pat,WORDS(i+1),tmp_reg1,size_ff,C)
                     else
                       move_index_aty_to_aty(aty,pat,WORDS i,tmp_reg1,size_ff,C)
                    | LS.CON0{con,con_kind,aux_regions,alloc} =>
                       (case con_kind of
                          LS.ENUM i =>
                            let
                              val tag =
                                if BI.tag_values() orelse (*hack to treat booleans tagged*)
                                  Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then
                                  2*i+1
                                else i
                              val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                            in
                              move_immed(Int32.fromInt tag, R reg_for_result,C')
                            end
                        | LS.UNBOXED i =>
                            let
                              val tag = 4*i+3
                              val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                              fun reset_regions C =
                                foldr (fn (alloc,C) =>
                                       maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C))
                                C aux_regions
                            in
                              reset_regions(move_immed(Int32.fromInt tag, R reg_for_result,C'))
                            end
                        | LS.BOXED i =>
                            let
                              val tag = i2s(Word32.toInt(BI.tag_con0(false,i)))
                              val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                              fun reset_regions C =
                                List.foldr (fn (alloc,C) =>
                                            maybe_reset_aux_region_kill_tmp0(alloc,tmp_reg1,size_ff,C))
                                C aux_regions
                            in
                              reset_regions(
                              alloc_ap_kill_tmp01(alloc,reg_for_result,1,size_ff,
                              I.movq(I tag, D("0",reg_for_result)) :: C'))
                            end)
                    | LS.CON1{con,con_kind,alloc,arg} =>
                          (case con_kind
                             of LS.UNBOXED 0 => move_aty_to_aty(arg,pat,size_ff,C)
                              | LS.UNBOXED i =>
                               let val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                               in case i
                                    of 1 => move_aty_into_reg(arg,reg_for_result,size_ff,
                                            I.orq(I "1", R reg_for_result) :: C')
                                     | 2 => move_aty_into_reg(arg,reg_for_result,size_ff,
                                            I.orq(I "2", R reg_for_result) :: C')
                                     | _ => die "CG_ls: UNBOXED CON1 with i > 2"
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
                      (case con_kind
                         of LS.UNBOXED 0 => move_aty_to_aty(con_aty,pat,size_ff,C)
                          | LS.UNBOXED _ =>
                           let
                             val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                           in
                             move_aty_into_reg(con_aty,reg_for_result,size_ff,
                             I.movq(I "3", R tmp_reg0) ::
                             I.notq(R tmp_reg0) ::
                             I.andq(R tmp_reg0, R reg_for_result) :: C')
                           end
                          | LS.BOXED _ => move_index_aty_to_aty(con_aty,pat,WORDS 1,tmp_reg1,size_ff,C)
                          | _ => die "CG_ls: DECON used with con_kind ENUM")
                    | LS.DEREF aty =>
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
(*                       val size_of_ref = to be removed 2003-08-26, nh
                           if BI.tag_values() andalso not (tag_pairs_p()) then
                             BI.size_of_ref() - 1
                           else
                             BI.size_of_ref()*)
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
                         move_immed(Int32.fromInt BI.ml_unit, R reg_for_result,C')
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
                    | LS.PASS_PTR_TO_RHO(alloc) =>
                     let
                       val (reg_for_result,C') = resolve_aty_def(pat,tmp_reg1,size_ff,C)
                     in
                       prefix_sm(alloc,reg_for_result,size_ff,C')
                     end))
                  ) (* END ASSIGN *)

               | LS.FLUSH(aty,offset) => comment_fn (fn () => "FLUSH: " ^ pr_ls ls,
                                         store_aty_in_reg_record(aty,tmp_reg1,rsp,WORDS(size_ff-offset-1),size_ff,C))
               | LS.FETCH(aty,offset) => comment_fn (fn () => "FETCH: " ^ pr_ls ls,
                                         load_aty_from_reg_record(aty,tmp_reg1,rsp,WORDS(size_ff-offset-1),size_ff,C))
               | LS.FNJMP(cc as {opr,args,clos,res,bv}) =>
                comment_fn (fn () => "FNJMP: " ^ pr_ls ls,
                let
                  val (spilled_args,_,_) = CallConv.resolve_act_cc RI.args_phreg RI.res_phreg {args=args,clos=clos,
                                                                    reg_args=[],reg_vec=NONE,res=res}
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
                    val (spilled_args,spilled_res,_) =
                      CallConv.resolve_act_cc RI.args_phreg RI.res_phreg {args=args,clos=clos,reg_args=[],reg_vec=NONE,res=res}
                    val size_rcf = length spilled_res
                    val size_ccf = length spilled_args
                    val size_cc = size_rcf+size_ccf+1
(*val _ = if size_cc > 1 then die ("\nfncall: size_ccf: " ^ (Int.toString size_ccf) ^ " and size_rcf: " ^
                                 (Int.toString size_rcf) ^ ".") else () (* debug 2001-01-08, Niels *)*)

                    val return_lab = new_local_lab "return_from_app"
                    fun flush_args C =
                      foldr (fn ((aty,offset),C) => push_aty(aty,tmp_reg1,size_ff+offset,C)) C spilled_args
                    (* We pop in reverse order such that size_ff+offset works *)
                    fun fetch_res C =
                      foldr (fn ((aty,offset),C) =>
                             pop_aty(aty,tmp_reg1,size_ff+offset,C)) C (rev spilled_res)
                    fun jmp C =
                      case opr (* We fetch the add from the closure and opr points at the closure *)
                        of SS.PHREG_ATY opr_reg =>
                          I.movq(D(offset_codeptr,opr_reg), R tmp_reg1) ::  (* Fetch code pointer *)
                          I.jmp(R tmp_reg1) :: C
                         | _ =>
                          move_aty_into_reg(opr,tmp_reg1,size_ff+size_cc,   (* rsp is now pointing after the call *)
                          I.movq(D(offset_codeptr,tmp_reg1), R tmp_reg1) :: (* convention, i.e., size_ff+size_cc *)
                          I.jmp(R tmp_reg1) :: C)
                  in
                    base_plus_offset(rsp,WORDS(~size_rcf),rsp,                         (* Move rsp after rcf *)
                    I.push(LA return_lab) ::                                          (* Push Return Label *)
                    flush_args(jmp(gen_bv(bv, I.lab return_lab :: fetch_res C))))
                  end)
               | LS.JMP(cc as {opr,args,reg_vec,reg_args,clos,res,bv}) =>
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
                    val (spilled_args, (* those arguments that need be passed on the stack *)
                         spilled_res,  (* those return values that are returned on the stack *)
                         _) = CallConv.resolve_act_cc RI.args_phreg RI.res_phreg
                              {args=args,clos=clos,reg_args=reg_args,reg_vec=reg_vec,res=res}

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
               | LS.FUNCALL{opr,args,reg_vec,reg_args,clos,res,bv} =>
                  comment_fn (fn () => "FUNCALL: " ^ pr_ls ls,
                  let
                    val (spilled_args,spilled_res,_) =
                        CallConv.resolve_act_cc RI.args_phreg RI.res_phreg {args=args,clos=clos,reg_args=reg_args,
                                                                            reg_vec=reg_vec,res=res}
                    val size_rcf = List.length spilled_res
                    val return_lab = new_local_lab "return_from_app"
                    fun flush_args C =
                      foldr (fn ((aty,offset),C) => push_aty(aty,tmp_reg1,size_ff+offset,C)) C (spilled_args)
                    (* We pop in reverse order such that size_ff+offset works *)
                    fun fetch_res C =
                      foldr (fn ((aty,offset),C) => pop_aty(aty,tmp_reg1,size_ff+offset,C)) C (rev spilled_res)
                    fun jmp C = I.jmp(L(MLFunLab opr)) :: C
                  in
                    base_plus_offset(rsp,WORDS(~size_rcf),rsp,                          (* Move rsp after rcf *)
                    I.push(LA return_lab) ::                                           (* Push Return Label *)
                    flush_args(jmp(gen_bv(bv, I.lab return_lab :: fetch_res C))))
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

                    fun alloc_region_prim(((place,phsize),offset),C) =
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
                                                  [SS.PHREG_ATY tmp_reg1,
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
                              in
                                  base_plus_offset(rsp,WORDS(size_ff-offset-1),tmp_reg1,
                                    compile_c_call_prim(name,[SS.PHREG_ATY tmp_reg1],NONE,
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
                            compile_c_call_prim("deallocateRegion",[],NONE,size_ff,tmp_reg0(*not used*),C)
                      else
                        case phsize
                          of LineStmt.WORDS i => C
                           | LineStmt.INF =>
                            compile_c_call_prim("deallocateRegion",[],NONE,size_ff,tmp_reg0(*not used*),C)
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
                      I.movq(L exn_ptr_lab, R tmp_reg1) ::
                      store_indexed(rsp,WORDS(size_ff-offset-1+2), R tmp_reg1,
                      comment ("CALC NEW exnPtr: exnPtr = sp-size_ff+offset+size_of_handle",
                      base_plus_offset(rsp,WORDS(size_ff-offset-1(*-BI.size_of_handle()*)),tmp_reg1,        (*hmmm *)
                      I.movq(R tmp_reg1, L exn_ptr_lab) :: C))))
                    fun store_sp C =
                      comment ("STORE SP: sp[offset+3] = sp",
                      store_indexed(rsp,WORDS(size_ff-offset-1+3), R rsp,C))
                    fun default_code C = comment ("HANDLER DEFAULT CODE",
                      CG_lss(default,size_ff,size_ccf,C))
                    fun restore_exp_ptr C =
                      comment ("RESTORE EXN PTR: exnPtr = sp[offset+2]",
                      load_indexed(R tmp_reg1,rsp,WORDS(size_ff-offset-1+2),
                      I.movq(R tmp_reg1, L exn_ptr_lab) ::
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
                    restore_exp_ptr(
                    handl_return_code(comment ("END OF EXCEPTION HANDLER", C))))))))))
                  end
               | LS.RAISE{arg=arg_aty,defined_atys} =>
                  move_aty_into_reg(arg_aty,rdi,size_ff,              (* function never returns *)
                  maybe_align 0 (fn C => I.call (NameLab "raise_exn") :: rem_dead_code C) C)
               | LS.SWITCH_I{switch=LS.SWITCH(SS.FLOW_VAR_ATY(lv,lab_t,lab_f),[(sel_val,lss)],default),
                             precision} =>
                  let
                    val (t_lab,f_lab) = if sel_val = Int32.fromInt BI.ml_true then (lab_t,lab_f) else (lab_f,lab_t)
                    val lab_exit = new_local_lab "lab_exit"
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
                                    toInt=fn i => maybeTagInt{value=i, precision=precision},
                                    opr_aty=opr_aty,
                                    oprBoxed=boxedNum precision,
                                    sels=sels,
                                    default=default,
                                    C=C}
               | LS.SWITCH_W {switch=LS.SWITCH(opr_aty,sels,default), precision} =>
                  compileNumSwitch {size_ff=size_ff,
                                    size_ccf=size_ccf,
                                    CG_lss=CG_lss,
                                    toInt=fn w => Int32.fromLarge(Word32.toLargeIntX (maybeTagWord{value=w, precision=precision})),
                                    opr_aty=opr_aty,
                                    oprBoxed=boxedNum precision,
                                    sels=sels,
                                    default=default,
                                    C=C}
               | LS.SWITCH_S sw => die "SWITCH_S is unfolded in ClosExp"
               | LS.SWITCH_C(LS.SWITCH(SS.FLOW_VAR_ATY(lv,lab_t,lab_f),[((con,con_kind),lss)],default)) =>
                  let
                    val (t_lab,f_lab) = if Con.eq(con,Con.con_TRUE) then (lab_t,lab_f) else (lab_f,lab_t)
                    val lab_exit = new_local_lab "lab_exit"
                  in
                    I.lab(LocalLab t_lab) ::
                    CG_lss(lss,size_ff,size_ccf,
                    I.jmp(L lab_exit) ::
                    I.lab(LocalLab f_lab) ::
                    CG_lss(default,size_ff,size_ccf,
                    I.lab lab_exit :: C))
                  end
               | LS.SWITCH_C(LS.SWITCH(opr_aty,[],default)) => CG_lss(default,size_ff,size_ccf,C)
               | LS.SWITCH_C(LS.SWITCH(opr_aty,sels,default)) =>
                  let (* NOTE: selectors in sels are tagged in ClosExp; values are
                       * tagged here in CodeGenX64! *)
                    val con_kind = case sels
                                     of [] => die ("CG_ls: SWITCH_C sels is empty: " ^ (pr_ls ls))
                                      | ((con,con_kind),_)::rest => con_kind
                    val sels' = map (fn ((con,con_kind),sel_insts) =>
                                     case con_kind
                                       of LS.ENUM i => (Int32.fromInt i,sel_insts)
                                        | LS.UNBOXED i => (Int32.fromInt i,sel_insts)
                                        | LS.BOXED i => (Int32.fromInt i,sel_insts)) sels
                    fun UbTagCon(src_aty,C) =
                      let val cont_lab = new_local_lab "cont"
                      in move_aty_into_reg(src_aty,tmp_reg0,size_ff,
                         copy(tmp_reg0, tmp_reg1, (* operand is in tmp_reg1, see SWITCH_I *)
                         I.andq(I "3", R tmp_reg1) ::
                         I.cmpq(I "3", R tmp_reg1) ::   (* do copy if tr = 3; in that case we      *)
                         I.jne cont_lab ::              (* are dealing with a nullary constructor, *)
                         copy(tmp_reg0, tmp_reg1,       (* and all bits are used. *)
                         I.lab cont_lab :: C)))
                      end
                    val (F, opr_aty) =
                      case con_kind
                        of LS.ENUM _ => (fn C => C, opr_aty)
                         | LS.UNBOXED _ => (fn C => UbTagCon(opr_aty,C), SS.PHREG_ATY tmp_reg1)
                         | LS.BOXED _ =>
                          (fn C => move_index_aty_to_aty(opr_aty,SS.PHREG_ATY tmp_reg1,
                                                         WORDS 0,tmp_reg1,size_ff,C),
                           SS.PHREG_ATY tmp_reg1)
                  in
                    F (compileNumSwitch {size_ff=size_ff,
                                         size_ccf=size_ccf,
                                         CG_lss=CG_lss,
                                         toInt=fn i => i,   (* tagging already done in ClosExp *)
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
                      fun cmp(i,x,y) = cmpi_and_jmp_kill_tmp01(i,x,y,lab_t,lab_f,size_ff,C)
                      fun cmp_boxed(i,x,y) = cmpbi_and_jmp_kill_tmp01(i,x,y,lab_t,lab_f,size_ff,C)
                  in case (name,args)
                       of ("__equal_int32ub",[x,y]) => cmp(I.je,x,y)
                        | ("__equal_int32b",[x,y]) => cmp_boxed(I.je,x,y)
                        | ("__equal_int31",[x,y]) => cmp(I.je,x,y)
                        | ("__equal_word31",[x,y]) => cmp(I.je,x,y)
                        | ("__equal_word32ub",[x,y]) => cmp(I.je,x,y)
                        | ("__equal_word32b",[x,y]) => cmp_boxed(I.je,x,y)
                        | ("__less_int32ub",[x,y]) => cmp(I.jl,x,y)
                        | ("__less_int32b",[x,y]) => cmp_boxed(I.jl,x,y)
                        | ("__less_int31",[x,y]) => cmp(I.jl,x,y)
                        | ("__less_word31",[x,y]) => cmp(I.jb,x,y)
                        | ("__less_word32ub",[x,y]) => cmp(I.jb,x,y)
                        | ("__less_word32b",[x,y]) => cmp_boxed(I.jb,x,y)
                        | ("__lesseq_int32ub",[x,y]) => cmp(I.jle,x,y)
                        | ("__lesseq_int32b",[x,y]) => cmp_boxed(I.jle,x,y)
                        | ("__lesseq_int31",[x,y]) => cmp(I.jle,x,y)
                        | ("__lesseq_word31",[x,y]) => cmp(I.jbe,x,y)
                        | ("__lesseq_word32ub",[x,y]) => cmp(I.jbe,x,y)
                        | ("__lesseq_word32b",[x,y]) => cmp_boxed(I.jbe,x,y)
                        | ("__greater_int32ub",[x,y]) => cmp(I.jg,x,y)
                        | ("__greater_int32b",[x,y]) => cmp_boxed(I.jg,x,y)
                        | ("__greater_int31",[x,y]) => cmp(I.jg,x,y)
                        | ("__greater_word31",[x,y]) => cmp(I.ja,x,y)
                        | ("__greater_word32ub",[x,y]) => cmp(I.ja,x,y)
                        | ("__greater_word32b",[x,y]) => cmp_boxed(I.ja,x,y)
                        | ("__greatereq_int32ub",[x,y]) => cmp(I.jge,x,y)
                        | ("__greatereq_int32b",[x,y]) => cmp_boxed(I.jge,x,y)
                        | ("__greatereq_int31",[x,y]) => cmp(I.jge,x,y)
                        | ("__greatereq_word31",[x,y]) => cmp(I.jae,x,y)
                        | ("__greatereq_word32ub",[x,y]) => cmp(I.jae,x,y)
                        | ("__greatereq_word32b",[x,y]) => cmp_boxed(I.jae,x,y)
                        | _ => die "CG_ls: Unknown PRIM used on Flow Variable"
                  end)
               | LS.PRIM{name,args,res} =>
                  let
                  in
                  comment_fn (fn () => "PRIM: " ^ pr_ls ls,
                  (* Note that the prim names are defined in BackendInfo! *)
                  (case (name,args,case res of nil => [SS.UNIT_ATY] | _ => res)
                     of ("__equal_int32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.je,x,y,d,size_ff,C)
                      | ("__equal_int32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.je,x,y,d,size_ff,C)
                      | ("__equal_int31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.je,x,y,d,size_ff,C)
                      | ("__equal_word31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.je,x,y,d,size_ff,C)
                      | ("__equal_word32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.je,x,y,d,size_ff,C)
                      | ("__equal_word32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.je,x,y,d,size_ff,C)

                      | ("__plus_int32ub",[x,y],[d]) => add_num_kill_tmp01 {ovf=true,tag=false} (x,y,d,size_ff,C)
                      | ("__plus_int32b",[b,x,y],[d]) => add_int32b (b,x,y,d,size_ff,C)
                      | ("__plus_int31",[x,y],[d]) => add_num_kill_tmp01 {ovf=true,tag=true} (x,y,d,size_ff,C)
                      | ("__plus_word31",[x,y],[d]) => add_num_kill_tmp01 {ovf=false,tag=true} (x,y,d,size_ff,C)
                      | ("__plus_word32ub",[x,y],[d]) => add_num_kill_tmp01 {ovf=false,tag=false} (x,y,d,size_ff,C)
                      | ("__plus_word32b",[b,x,y],[d]) => addw32boxed(b,x,y,d,size_ff,C)
                      | ("__plus_real",[b,x,y],[d]) => addf_kill_tmp01(x,y,b,d,size_ff,C)

                      | ("__minus_int32ub",[x,y],[d]) => sub_num_kill_tmp01 {ovf=true,tag=false} (x,y,d,size_ff,C)
                      | ("__minus_int32b",[b,x,y],[d]) => sub_int32b (b,x,y,d,size_ff,C)
                      | ("__minus_int31",[x,y],[d]) => sub_num_kill_tmp01 {ovf=true,tag=true} (x,y,d,size_ff,C)
                      | ("__minus_word31",[x,y],[d]) => sub_num_kill_tmp01 {ovf=false,tag=true} (x,y,d,size_ff,C)
                      | ("__minus_word32ub",[x,y],[d]) => sub_num_kill_tmp01 {ovf=false,tag=false} (x,y,d,size_ff,C)
                      | ("__minus_word32b",[b,x,y],[d]) => subw32boxed(b,x,y,d,size_ff,C)
                      | ("__minus_real",[b,x,y],[d]) => subf_kill_tmp01(x,y,b,d,size_ff,C)

                      | ("__mul_int32ub", [x,y], [d]) => mul_num_kill_tmp01 {ovf=true,tag=false} (x,y,d,size_ff,C)
                      | ("__mul_int32b", [b,x,y], [d]) => mul_int32b (b,x,y,d,size_ff,C)
                      | ("__mul_int31", [x,y], [d]) => mul_num_kill_tmp01 {ovf=true,tag=true} (x,y,d,size_ff,C)
                      | ("__mul_word31", [x,y], [d]) => mul_num_kill_tmp01 {ovf=false,tag=true} (x,y,d,size_ff,C)
                      | ("__mul_word32ub", [x,y], [d]) => mul_num_kill_tmp01 {ovf=false,tag=false} (x,y,d,size_ff,C)
                      | ("__mul_word32b", [b,x,y], [d]) => mulw32boxed(b,x,y,d,size_ff,C)
                      | ("__mul_real",[b,x,y],[d]) => mulf_kill_tmp01(x,y,b,d,size_ff,C)

                      | ("__div_real", [b,x,y],[d]) => divf_kill_tmp01(x,y,b,d,size_ff,C)

                      | ("__neg_int32ub",[x],[d]) => neg_int_kill_tmp0 {tag=false} (x,d,size_ff,C)
                      | ("__neg_int32b",[b,x],[d]) => neg_int32b_kill_tmp0 (b,x,d,size_ff,C)
                      | ("__neg_int31",[x],[d]) => neg_int_kill_tmp0 {tag=true} (x,d,size_ff,C)
                      | ("__neg_real",[b,x],[d]) => negf_kill_tmp01(b,x,d,size_ff,C)

                      | ("__abs_int32ub",[x],[d]) => abs_int_kill_tmp0 {tag=false} (x,d,size_ff,C)
                      | ("__abs_int32b",[b,x],[d]) => abs_int32b_kill_tmp0 (b,x,d,size_ff,C)
                      | ("__abs_int31",[x],[d]) => abs_int_kill_tmp0 {tag=true} (x,d,size_ff,C)
                      | ("__abs_real",[b,x],[d]) => absf_kill_tmp01(b,x,d,size_ff,C)

                      | ("__less_int32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jl,x,y,d,size_ff,C)
                      | ("__less_int32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jl,x,y,d,size_ff,C)
                      | ("__less_int31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jl,x,y,d,size_ff,C)
                      | ("__less_word31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jb,x,y,d,size_ff,C)
                      | ("__less_word32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jb,x,y,d,size_ff,C)
                      | ("__less_word32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jb,x,y,d,size_ff,C)
                      | ("__less_real",[x,y],[d]) => cmpf_kill_tmp01(LESSTHAN,x,y,d,size_ff,C)

                      | ("__lesseq_int32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jle,x,y,d,size_ff,C)
                      | ("__lesseq_int32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jle,x,y,d,size_ff,C)
                      | ("__lesseq_int31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jle,x,y,d,size_ff,C)
                      | ("__lesseq_word31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jbe,x,y,d,size_ff,C)
                      | ("__lesseq_word32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jbe,x,y,d,size_ff,C)
                      | ("__lesseq_word32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jbe,x,y,d,size_ff,C)
                      | ("__lesseq_real",[x,y],[d]) => cmpf_kill_tmp01(LESSEQUAL,x,y,d,size_ff,C)

                      | ("__greater_int32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jg,x,y,d,size_ff,C)
                      | ("__greater_int32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jg,x,y,d,size_ff,C)
                      | ("__greater_int31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jg,x,y,d,size_ff,C)
                      | ("__greater_word31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.ja,x,y,d,size_ff,C)
                      | ("__greater_word32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.ja,x,y,d,size_ff,C)
                      | ("__greater_word32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.ja,x,y,d,size_ff,C)
                      | ("__greater_real",[x,y],[d]) => cmpf_kill_tmp01(GREATERTHAN,x,y,d,size_ff,C)

                      | ("__greatereq_int32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jge,x,y,d,size_ff,C)
                      | ("__greatereq_int32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jge,x,y,d,size_ff,C)
                      | ("__greatereq_int31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jge,x,y,d,size_ff,C)
                      | ("__greatereq_word31",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jae,x,y,d,size_ff,C)
                      | ("__greatereq_word32ub",[x,y],[d]) => cmpi_kill_tmp01 {box=false} (I.jae,x,y,d,size_ff,C)
                      | ("__greatereq_word32b",[x,y],[d]) => cmpi_kill_tmp01 {box=true} (I.jae,x,y,d,size_ff,C)
                      | ("__greatereq_real",[x,y],[d]) => cmpf_kill_tmp01(GREATEREQUAL,x,y,d,size_ff,C)

                      | ("__andb_word31",[x,y],[d]) => andb_word_kill_tmp01(x,y,d,size_ff,C)
                      | ("__andb_word32ub",[x,y],[d]) => andb_word_kill_tmp01(x,y,d,size_ff,C)
                      | ("__andb_word32b",[b,x,y],[d]) => andw32boxed__(b,x,y,d,size_ff,C)

                      | ("__orb_word31",[x,y],[d]) => orb_word_kill_tmp01(x,y,d,size_ff,C)
                      | ("__orb_word32ub",[x,y],[d]) => orb_word_kill_tmp01(x,y,d,size_ff,C)
                      | ("__orb_word32b",[b,x,y],[d]) => orw32boxed__(b,x,y,d,size_ff,C)

                      | ("__xorb_word31",[x,y],[d]) => xorb_word_kill_tmp01 {tag=true} (x,y,d,size_ff,C)
                      | ("__xorb_word32ub",[x,y],[d]) => xorb_word_kill_tmp01 {tag=false} (x,y,d,size_ff,C)
                      | ("__xorb_word32b",[b,x,y],[d]) => xorw32boxed__(b,x,y,d,size_ff,C)

                      | ("__shift_left_word31",[x,y],[d]) => shift_left_word_kill_tmp01 {tag=true} (x,y,d,size_ff,C)
                      | ("__shift_left_word32ub",[x,y],[d]) => shift_left_word_kill_tmp01 {tag=false} (x,y,d,size_ff,C)
                      | ("__shift_left_word32b",[b,x,y],[d]) => shift_leftw32boxed__(b,x,y,d,size_ff,C)

                      | ("__shift_right_signed_word31",[x,y],[d]) =>
                       shift_right_signed_word_kill_tmp01 {tag=true} (x,y,d,size_ff,C)
                      | ("__shift_right_signed_word32ub",[x,y],[d]) =>
                       shift_right_signed_word_kill_tmp01 {tag=false} (x,y,d,size_ff,C)
                      | ("__shift_right_signed_word32b",[b,x,y],[d]) =>
                       shift_right_signedw32boxed__(b,x,y,d,size_ff,C)

                      | ("__shift_right_unsigned_word31",[x,y],[d]) =>
                       shift_right_unsigned_word_kill_tmp01 {tag=true} (x,y,d,size_ff,C)
                      | ("__shift_right_unsigned_word32ub",[x,y],[d]) =>
                       shift_right_unsigned_word_kill_tmp01 {tag=false} (x,y,d,size_ff,C)
                      | ("__shift_right_unsigned_word32b",[b,x,y],[d]) =>
                       shift_right_unsignedw32boxed__(b,x,y,d,size_ff,C)

                      | ("__int31_to_int32b",[b,x],[d]) => num31_to_num32b(b,x,d,size_ff,C)
                      | ("__int31_to_int32ub",[x],[d]) => num31_to_num32ub(x,d,size_ff,C)
                      | ("__int32b_to_int31",[x],[d]) => int32_to_int31 {boxedarg=true} (x,d,size_ff,C)
                      | ("__int32ub_to_int31",[x],[d]) => int32_to_int31 {boxedarg=false} (x,d,size_ff,C)

                      | ("__word31_to_word32b",[b,x],[d]) => num31_to_num32b(b,x,d,size_ff,C)
                      | ("__word31_to_word32ub",[x],[d]) => num31_to_num32ub(x,d,size_ff,C)
                      | ("__word32b_to_word31",[x],[d]) => word32_to_word31 {boxedarg=true} (x,d,size_ff,C)
                      | ("__word32ub_to_word31",[x],[d]) => word32_to_word31 {boxedarg=false} (x,d,size_ff,C)

                      | ("__word31_to_word32ub_X",[x],[d]) => num31_to_num32ub(x,d,size_ff,C)
                      | ("__word31_to_word32b_X",[b,x],[d]) => num31_to_num32b(b,x,d,size_ff,C)

                      | ("__word32b_to_int32b",[b,x],[d]) => num32b_to_num32b {ovf=true} (b,x,d,size_ff,C)
                      | ("__word32b_to_int32b_X",[b,x],[d]) => num32b_to_num32b {ovf=false} (b,x,d,size_ff,C)
                      | ("__int32b_to_word32b",[b,x],[d]) => num32b_to_num32b {ovf=false} (b,x,d,size_ff,C)
                      | ("__word32ub_to_int32ub",[x],[d]) => word32ub_to_int32ub(x,d,size_ff,C)
                      | ("__word32b_to_int31",[x],[d]) => word32_to_int31 {boxedarg=true,ovf=true} (x,d,size_ff,C)
                      | ("__int32b_to_word31",[x],[d]) => word32_to_word31 {boxedarg=true} (x,d,size_ff,C)
                      | ("__word32b_to_int31_X", [x],[d]) => word32_to_int31 {boxedarg=true,ovf=false} (x,d,size_ff,C)

                      | ("__fresh_exname",[],[aty]) =>
                       I.movq(L exn_counter_lab, R tmp_reg0) ::
                       move_reg_into_aty(tmp_reg0,aty,size_ff,
                       I.addq(I "1", R tmp_reg0) ::
                       I.movq(R tmp_reg0, L exn_counter_lab) :: C)

                      | ("__bytetable_sub", [t,i], [d]) => bytetable_sub(t,i,d,size_ff,C)
                      | ("__bytetable_size", [t], [d]) => bytetable_size(t,d,size_ff,C)
                      | ("__bytetable_update", [t,i,x], [d]) => bytetable_update(t,i,x,d,size_ff,C)

                      | ("word_sub0", [t,i], [d]) => word_sub0(t,i,d,size_ff,C)
                      | ("table_size", [t], [d]) => table_size(t,d,size_ff,C)
                      | ("word_update0", [t,i,x], [d]) => word_update0(t,i,x,d,size_ff,C)

                      | ("__is_null", [t], [d]) =>
                       cmpi_kill_tmp01 {box=false} (I.je,t, SS.INTEGER_ATY{value=Int32.fromInt 0,
                                                                           precision=32},d,size_ff,C)
                      | _ => die ("PRIM(" ^ name ^ ") not implemented")))
                  end
               | LS.CCALL{name="spawnone",args=[arg],rhos_for_result=nil,res=[res]} =>
                 let
                   (* The call_closure C function takes one argument (an ML closure). It
                    * extracts the closure pointer and the closure environment from the argument
                    * and makes an ML call to the function represented by the closure. *)
                   val name = "spawnone"
                   val offset_codeptr = if BI.tag_values() then "8" else "0"
                   val call_closure_lab = new_local_lab (name ^ "_call_closure")
                   val return_lab = new_local_lab ("return_" ^ name)
                   val _ = add_static_data ([I.dot_text,
                                             I.dot_align 8,
                                             (*I.dot_globl call_closure_lab, (* The C function entry *) *)
                                             I.lab call_closure_lab]
                                            @ (map (fn r => I.push (R r)) callee_save_regs_ccall)
                                            @ [I.movq(R rdi,R tmp_reg0)]
                                            (* now initialize thread local data to point to the threadinfor struct *)
                                            @ compile_c_call_prim("thread_init", [SS.PHREG_ATY tmp_reg0], SOME (SS.PHREG_ATY tmp_reg0), size_ff (* not used *), tmp_reg1,
                                              [I.movq(R tmp_reg0, R rdi),            (* restore argument, which is passed through thread_init *)
                                               I.movq(D("0",rdi),R rdi),             (* extract closure from threadinfo arg *)
                                               I.movq(R rdi,R rax),                  (* move closure into closure register *)
                                               I.movq(D(offset_codeptr,rdi), R r10), (* extract code pointer into %r10 from C arg *)
                                               I.push (LA return_lab),               (* push return address *)
                                               I.jmp (R r10),                        (* call ML function *)
                                               I.lab return_lab,                     (* ML result is now in rdi *)
                                               I.movq(R rdi, R tmp_reg0)]
                                            @ compile_c_call_prim("pthread_exit", [SS.PHREG_ATY tmp_reg0], NONE, size_ff (* not used *), tmp_reg1,
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
                    fun comp_c_call(all_args,res,C) =
                      compile_c_call_prim(name, all_args, res, size_ff, tmp_reg1, C)
                    val _ =
                      if BI.is_prim name then
                        die ("CCALL." ^ name ^ " is meant to be a primitive inlined by the compiler " ^
                             "- but it is not dealt with!")
                      else ()
                    val _ =
                      case (explode name, rhos_for_result)
                        of (_, nil) => ()
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
                   (case (case name of ":" => (let val (a1,ar) = valOf (List.getItem args)
                                  in a1 ::(rhos_for_result@ar)
                                  end
                                  handle Option.Option =>
                                         die ("Dynamic liking requires a string as first argument."))
                     | _ => (rhos_for_result@args), res)
                        of (all_args,[]) => comp_c_call(all_args, NONE, C)
                         | (all_args, [res_aty]) => comp_c_call(all_args, SOME res_aty, C)
                         | _ => die "CCall with more than one result variable"))
                  end
               | LS.CCALL_AUTO{name, args, res} =>
                 let
                    (*val () = print ("CCALL_AUTO: " ^ name ^ "\n")*)
                    val _ =
                      if BI.is_prim name then
                        die ("CCALL_AUTO." ^ name ^ " is meant to be a primitive inlined by the compiler " ^
                             "- but it is not dealt with!")
                      else ()
                  in

        (* With dynamicly linked functions the first argument must be the name of   *)
        (* the function. If we where to implement automatic conversion into regions *)
        (* this must be taken care of, like in the non-automatic case               *)

                    comment_fn (fn () => "CCALL_AUTO: " ^ pr_ls ls,
                                compile_c_call_auto(name,args,res,size_ff,tmp_reg1,C))
                  end
               | LS.EXPORT{name,
                           clos_lab,
                           arg=(aty,ft1,ft2)} =>
                  let val clos_lab = DatLab clos_lab
                      (*val clos_lab = NameLab (name ^ "_clos")*)
                      val return_lab = new_local_lab ("return_" ^ name)
                      val offset_codeptr = if BI.tag_values() then "8" else "0"
                      val lab = NameLab name   (* lab is the C function to call after the hook has been setup *)
                      val stringlab = gen_string_lab name
                      val _ =
                          if ft1 <> LS.Int orelse ft2 <> LS.Int then
                              die "Export of ML function with type other than (int->int) not supported"
                          else ()

                      val _ = add_static_data
                          ([I.dot_data,
                            I.dot_align 8,
                            I.dot_globl clos_lab,
                            I.lab clos_lab,  (* Slot for storing a pointer to the ML closure; the
                                              * ML closure object may move due to GCs. *)
                            I.dot_quad (i2s BI.ml_unit),
                            I.dot_text,
                            I.dot_globl lab, (* The C function entry *)
                            I.lab lab]
                         @ (map (fn r => I.push (R r)) callee_save_regs_ccall)
                         @ [I.movq (L clos_lab, R rax),           (* load closure into ML arg 1 *)
                            I.movq (R rdi, R rbx),                (* move C arg into ML arg 2 *)
                            I.movq(D(offset_codeptr,rax), R r10), (* extract code pointer into %r10 *)
                            I.push (I "1"),                       (* push dummy *)
                            I.push (LA return_lab),               (* push return address *)
                            I.jmp (R r10),                        (* call ML function *)
                            I.lab return_lab,
                            I.movq(R rdi, R rax),                 (* move result to %rax *)
                            I.addq(I "8", R rsp)]                 (* pop dummy *)
                         @ (map (fn r => I.pop (R r)) (List.rev callee_save_regs_ccall))
                         @ [I.ret])

                     val saveregs = rdi :: rsi :: rdx :: rcx :: r8 :: r9 :: rax ::
                                    caller_save_regs_ccall
                     fun push_callersave_regs C =
                         foldl (fn (r, C) => I.push(R r) :: C) C saveregs
                     fun pop_callersave_regs C =
                         foldr (fn (r, C) => I.pop(R r) :: C) C saveregs

                  in comment_fn (fn () => "EXPORT: " ^ pr_ls ls,
                                 store_in_label(aty,clos_lab,tmp_reg1,size_ff,
                                                I.movq (LA lab, R tmp_reg0) ::
                                                I.movq (LA stringlab, R tmp_reg1) ::
                                                push_callersave_regs
                                                (compile_c_call_prim("sml_regCfuns",[SS.PHREG_ATY tmp_reg1,
                                                                                     SS.PHREG_ATY tmp_reg0],NONE,0, tmp_reg1,
                                                pop_callersave_regs C))))
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
           val labCont2 = new_local_lab "profCont2-"
           val maxStackLab = NameLab "maxStack"
           val timeToProfLab = NameLab "timeToProfile"
         in I.movq(L maxStackLab, R tmp_reg0) ::     (* The stack grows downwards!! *)
           I.cmpq(R rsp, R tmp_reg0) ::
           I.jl labCont ::                                                    (* if ( rsp < *maxStack ) {     *)
           I.movq(R rsp, L maxStackLab) ::                                    (*    *maxStack = rsp ;         *)
           I.movq(L (NameLab "regionDescUseProfInf"), R tmp_reg0) ::          (*    maxProfStack =            *)
           I.addq(L (NameLab "regionDescUseProfFin"), R tmp_reg0) ::          (*       regionDescUseProfInf   *)
           I.addq(L (NameLab "allocProfNowFin"), R tmp_reg0) ::               (*     + regionDescUseProfFin   *)
           I.movq(R tmp_reg0, L (NameLab "maxProfStack")) ::                  (*     + allocProfNowFin ;      *)
           I.lab labCont ::                                                   (* }                            *)
           I.movq(L timeToProfLab, R tmp_reg0) ::                             (* if ( timeToProfile )         *)
           I.cmpq(I "0", R tmp_reg0) ::                                       (*    call __proftick(rsp);     *)
           I.je labCont2 ::
           I.movq (R rsp, R tmp_reg1) ::              (* proftick assumes argument in tmp_reg1 *)
           I.push (LA labCont2) ::                    (* push return address *)
           I.jmp (L(NameLab "__proftick")) ::
           I.lab labCont2 ::
           C
         end
       else C

    fun CG_top_decl' gen_fn (lab,cc,lss) =
      let
        val w0 = Word32.fromInt 0
        fun pw w = print ("Word is " ^ (Word32.fmt StringCvt.BIN w) ^ "\n")
        fun pws ws = app pw ws
        fun set_bit(bit_no,w) = Word32.orb(w,Word32.<<(Word32.fromInt 1,Word.fromInt bit_no))

        val size_ff = CallConv.get_frame_size cc
        val size_ccf = CallConv.get_ccf_size cc
        val size_rcf = CallConv.get_rcf_size cc
(*val _ = if size_ccf + size_rcf > 0 then die ("\ndo_gc: size_ccf: " ^ (Int.toString size_ccf) ^ " and size_rcf: " ^
                                               (Int.toString size_rcf) ^ ".") else () (* 2001-01-08, Niels debug *)*)
        val C = base_plus_offset(rsp,WORDS(size_ff+size_ccf),rsp,
                                 I.pop (R tmp_reg1) ::
                                 I.jmp (R tmp_reg1) :: [])
        val size_spilled_region_args = List.length (CallConv.get_spilled_region_args cc)
        val reg_args = map lv_to_reg_no (CallConv.get_register_args_excluding_region_args cc)
        val reg_map = foldl (fn (reg_no,w) => set_bit(reg_no,w)) w0 reg_args
   (*
        val _ = app (fn reg_no => print ("reg_no " ^ Int.toString reg_no ^ " is an argument\n")) reg_args
        val _ = pw reg_map
   *)
      in
        gen_fn(lab,
               do_gc(reg_map,size_ccf,size_rcf,size_spilled_region_args,
                base_plus_offset(rsp,WORDS(~size_ff),rsp,
                 do_simple_memprof(
                 do_prof(
                  CG_lss(lss,size_ff,size_ccf,C))))))
      end

    fun CG_top_decl(LS.FUN(lab,cc,lss)) = CG_top_decl' I.FUN (lab,cc,lss)
      | CG_top_decl(LS.FN(lab,cc,lss)) = CG_top_decl' I.FN (lab,cc,lss)

    local
        fun data_x_progunit_lab x l = NameLab(Labels.pr_label l ^ "_data_" ^ x)
        fun data_x_lab x (l:label, C) =
            if gc_p() then
                let val lab = data_x_progunit_lab x l
                in I.dot_globl lab ::
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
    fun static_data(l:label) =
      I.dot_data ::
      comment ("START OF STATIC DATA AREA",
      data_begin_lab (l,
      get_static_data (data_end_lab(l,
      comment ("END OF STATIC DATA AREA",nil)))))

    fun init_x64_code() = [I.dot_text]
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
        val _ = add_static_data (I.dot_data :: map (fn lab => I.dot_globl(MLFunLab lab)) (main_lab::(#1 exports)))
        val _ = add_static_data (I.dot_data :: map (fn lab => I.dot_globl(DatLab lab)) (#2 exports))
        val x64_prg = {top_decls = foldr (fn (func,acc) => CG_top_decl func :: acc) [] ss_prg,
                       init_code = init_x64_code(),
                       static_data = static_data main_lab}
        val _ = chat "]\n"
      in
        x64_prg
      end

    (* ------------------------------------------------------------------------------ *)
    (*              Generate Link Code for Incremental Compilation                    *)
    (* ------------------------------------------------------------------------------ *)
    fun generate_link_code (linkinfos:label list, exports: label list * label list) : I.AsmPrg =
      let
        val _ = reset_static_data()
        val _ = reset_label_counter()

        val lab_exit = NameLab "__lab_exit"
        val next_prog_unit = Labels.new_named "next_prog_unit"
        val progunit_labs = map MLFunLab linkinfos
        val dat_labs = map DatLab (#2 exports) (* Also in the root set 2001-01-09, Niels *)
(*
val _ = print ("There are " ^ (Int.toString (List.length dat_labs)) ^ " data labels in the root set. ")
val _ = List.app (fn lab => print ("\n" ^ (I.pr_lab lab))) (List.rev dat_labs)
*)

        fun slot_for_datlab((_,l),C) =
            let fun maybe_dotsize C =
                    if I.sysname() = "Darwin" then C
                    else I.dot_size(DatLab l, 8) :: C
            in
                I.dot_globl (DatLab l) ::
                I.dot_data ::
                I.dot_align 8 ::
                maybe_dotsize (I.lab (DatLab l) ::
                               I.dot_quad "1" :: C)
            end

        fun slots_for_datlabs(l,C) = foldr slot_for_datlab C l

        fun toplevel_handler C =
          let
            val (clos_lv,arg_lv) = CallConv.handl_arg_phreg RI.args_phreg
            val (clos_reg,arg_reg) = (RI.lv_to_reg clos_lv, RI.lv_to_reg arg_lv)
            val offset = if BI.tag_values() then 1 else 0
          in
              I.lab (NameLab "TopLevelHandlerLab") ::
              I.movq (R arg_reg, R tmp_reg0)::
              load_indexed(R arg_reg,arg_reg,WORDS offset,
              load_indexed(R tmp_reg1,arg_reg, WORDS offset,
              load_indexed(R arg_reg,arg_reg,WORDS (offset+1), (* Fetch pointer to exception string *)
              compile_c_call_prim("uncaught_exception",[SS.PHREG_ATY arg_reg,SS.PHREG_ATY tmp_reg1,
                                                        SS.PHREG_ATY tmp_reg0],NONE,0,tmp_reg1,C))))
          end

        fun store_exported_data_for_gc (labs,C) =
            if gc_p() then
              let (* Make sure to leave stack 16-byte aligned if required by os *)
                  val F = if needs_align () andalso length(labs) mod 2 = 0 then
                            fn C => I.push (I "1") :: C (* align *)
                          else fn C => C
              in F(foldr (fn (l,acc) => I.push(LA l) :: acc)
                         (I.push (I (i2s (List.length labs))) ::
                          I.movq(R rsp, L data_lab_ptr_lab) :: C) labs)
              end
          else C


        fun raise_insts C = (* expects exception value in register rdi!! *)
          let
            val (clos_lv,arg_lv) = CallConv.handl_arg_phreg RI.args_phreg
            val (clos_reg,arg_reg) = (RI.lv_to_reg clos_lv, RI.lv_to_reg arg_lv)
            val offset_codeptr = if BI.tag_values() then "8" else "0"
          in
            I.dot_globl(NameLab "raise_exn") ::
            I.lab (NameLab "raise_exn") ::
            I.movq (R rdi, R r15) :: (* move argument to callee-save register *)
            comment ("DEALLOCATE REGIONS UNTIL",
            I.movq(L exn_ptr_lab, R tmp_reg1) ::
            compile_c_call_prim("deallocateRegionsUntil_X64",[SS.PHREG_ATY tmp_reg1],NONE,0,tmp_reg1,

            comment ("RESTORE EXN PTR",
            I.movq(L exn_ptr_lab, R tmp_reg1) ::
            I.movq(D("16",tmp_reg1), R tmp_reg0) ::   (* was:8 *)
            I.movq(R tmp_reg0, L exn_ptr_lab) ::

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
        fun setup_primitive_exception((n,exn_string,exn_lab,exn_flush_lab),C) =
          let
            val string_lab = gen_string_lab exn_string
            val _ =
              if BI.tag_values() then       (* Exception Name and Exception must be tagged. *)
                add_static_data [I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl exn_lab,
                                 I.lab exn_lab,
                                 I.dot_quad(BI.pr_tag_w(BI.tag_exname(true))),
                                 I.dot_quad "0", (*dummy for pointer to next word*)
                                 I.dot_quad(BI.pr_tag_w(BI.tag_excon0(true))),
                                 I.dot_quad(i2s n),
                                 I.dot_quad "0"  (*dummy for pointer to string*),
                                 I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl exn_flush_lab,
                                 I.lab exn_flush_lab, (* The Primitive Exception is Flushed at this Address *)
                                 I.dot_quad "0"]
              else
                add_static_data [I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl exn_lab,
                                 I.lab exn_lab,
                                 I.dot_quad "0", (*dummy for pointer to next word*)
                                 I.dot_quad(i2s n),
                                 I.dot_quad "0",  (*dummy for pointer to string*)
                                 I.dot_data,
                                 I.dot_align 8,
                                 I.dot_globl exn_flush_lab,
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

        val primitive_exceptions = [(0, "Match", NameLab "exn_MATCH", DatLab BI.exn_MATCH_lab),
                                    (1, "Bind", NameLab "exn_BIND", DatLab BI.exn_BIND_lab),
                                    (2, "Overflow", NameLab "exn_OVERFLOW", DatLab BI.exn_OVERFLOW_lab),
                                    (3, "Interrupt", NameLab "exn_INTERRUPT", DatLab BI.exn_INTERRUPT_lab),
                                    (4, "Div", NameLab "exn_DIV", DatLab BI.exn_DIV_lab)]
        val initial_exnname_counter = 5

        fun init_primitive_exception_constructors_code C =
          foldl (fn (t,C) => setup_primitive_exception(t,C)) C primitive_exceptions

        val static_data =
          slots_for_datlabs(global_region_labs,
                            I.dot_data ::
                            I.dot_globl exn_counter_lab ::
                            I.lab exn_counter_lab :: (* The Global Exception Counter *)
                            I.dot_quad (i2s initial_exnname_counter) ::

                            I.dot_globl exn_ptr_lab ::
                            I.lab exn_ptr_lab :: (* The Global Exception Pointer *)
                            I.dot_quad "0" :: nil)
        val _  = add_static_data static_data

        (* args can only be tmp_reg0 and tmp_reg1; no arguments
         * on the stack; only the return address! Destroys tmp_reg0! *)
        fun ccall_stub(stubname, cfunction, args, ret, C) =  (* result in tmp_reg1 if ret=true *)
          let
            val save_regs = rdi :: rsi :: rdx :: rcx :: r8 :: r9 :: rax ::
                            caller_save_regs_ccall  (* maybe also save the other
                                                     * ccall argument registers and the
                                                     * ccall result register rax *)

            (* Notice that caller_save_regs_ccall is defined as the empty list! *)

            (* The following registers must be preserved as register
               allocation may choose to map variables to these
               registers:

                 X = [rax,rbx,rdi,rdx,rsi] u [rbx,rbp,r12,r13,r14,r15]
                   = [rax,rbx,rdi,rdx,rsi,rbp,r12,r13,r14,r15]

               Here are the registers that are not saved:

                 X \ save_regs = [rbx,rbp,r12,r13,r14,r15]

               These should exactly be the callee-save registers!
            *)

            fun push_callersave_regs C =
              foldl (fn (r, C) => I.push(R r) :: C) C save_regs
            fun pop_callersave_regs C =
              foldr (fn (r, C) => I.pop(R r) :: C) C save_regs
            val size_ff = 0 (* dummy *)
            val stublab = NameLab stubname
            val res = if ret then SOME (SS.PHREG_ATY tmp_reg1) else NONE
          in
            I.dot_text ::
            I.dot_globl stublab ::
            I.lab stublab ::
            push_callersave_regs
            (compile_c_call_prim(cfunction, map SS.PHREG_ATY args, res, size_ff, tmp_reg0,
              pop_callersave_regs
                  (I.pop(R tmp_reg0) ::
                   I.jmp(R tmp_reg0) :: C)))
          end

        fun allocate C = (* args in tmp_reg1 and tmp_reg0; result in tmp_reg1. *)
          ccall_stub("__allocate", "alloc", [tmp_reg1, tmp_reg0], true, C)

        fun resetregion C =
          ccall_stub("__reset_region", "resetRegion", [tmp_reg1], true, C)

        fun proftick C =
          if region_profiling() then
            ccall_stub("__proftick", "profileTick", [tmp_reg1], false, C)
          else C

        fun overflow_stub C =
          let val stublab = [(NameLab "__raise_overflow",BI.exn_OVERFLOW_lab),
                             (NameLab "__raise_div",BI.exn_DIV_lab),
                             (NameLab "__raise_match",BI.exn_MATCH_lab),
                             (NameLab "__raise_bind",BI.exn_BIND_lab),
                             (NameLab "__raise_interrupt", BI.exn_INTERRUPT_lab)]
          in I.dot_text ::(List.foldr (fn ((nl,dl),C') =>
                                          I.dot_globl nl ::
                                          I.lab nl::
                                          I.movq(L(DatLab dl),R rdi)::
                                          I.call(NameLab "raise_exn")::C') C stublab)
          end

        fun gc_stub C = (* tmp_reg1 must contain the register map and tmp_reg0 the return address. *)
          if gc_p() then
            let
              fun push_all_regs C =
                foldr (fn (r, C) => I.push(R r) :: C) C all_regs
              fun pop_all_regs C =
                foldl (fn (r, C) => I.pop(R r) :: C) C all_regs
              fun pop_size_ccf_rcf_reg_args C = base_plus_offset(rsp,WORDS 3,rsp,C) (* they are pushed in do_gc *)
              val size_ff = 0 (*dummy*)
            in
              I.dot_text ::
              I.dot_globl gc_stub_lab ::
              I.lab gc_stub_lab ::
              push_all_regs (* The return lab and rcx are also preserved *)
              (copy(rsp,tmp_reg0,
                    compile_c_call_prim("gc",[SS.PHREG_ATY tmp_reg0,SS.PHREG_ATY tmp_reg1],NONE,size_ff,rax,
                                        pop_all_regs( (* The return lab and tmp_reg0 are also popped again *)
                                        pop_size_ccf_rcf_reg_args(
                                        (I.jmp(R tmp_reg0) :: C))))))
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

        fun generate_jump_code_progunits(progunit_labs,C) =
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
                   I.lab next_lab :: C))
                 end) C progunit_labs

        fun allocate_global_regions(region_labs,C) =
          let
            fun maybe_pass_region_id (region_id,C) =
              if region_profiling() then I.movq(I (i2s region_id), R rsi) :: C
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
            fun maybe_align16 i =
                if i mod 16 = 0 then i + 8
                else i + (16 - i mod 16) + 8
          in
            foldl (fn ((rho,lab),C) =>
                   let val region_id = Effect.key_of_eps_or_rho rho
                       val name = c_name rho
                       val C = I.movq(R rax, L (DatLab lab)) :: C
                       val sz_regdesc_bytes = 8*BI.size_of_reg_desc()
                       val sz_regdesc_bytes = maybe_align16 sz_regdesc_bytes
                   in
                       I.subq(I(i2s sz_regdesc_bytes), R rsp) ::  (* MAEL: maybe align *)
                       I.movq(R rsp, R rdi) ::
                       maybe_pass_region_id (region_id,
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
            I.subq(I "32", R rsp) ::
            I.movq(LA (NameLab "TopLevelHandlerLab"), R tmp_reg1) ::
            I.movq(R tmp_reg1, D("0", rsp)) ::
            gen_clos (
            I.movq(L exn_ptr_lab, R tmp_reg1) ::
            I.movq(R tmp_reg1, D("16", rsp)) ::
            I.movq(R rsp, D("24", rsp)) ::
            I.movq(R rsp, L exn_ptr_lab) :: C))
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

        fun main_insts C =
           (I.dot_text ::
            I.dot_align 8 ::
            I.dot_globl (NameLab "code") ::
            I.lab (NameLab "code") ::

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
            init_primitive_exception_constructors_code(

            (* Push top-level handler on stack *)
            push_top_level_handler(

            (* Code that jump to progunits. *)
            comment ("JUMP CODE TO PROGRAM UNITS",
            generate_jump_code_progunits(progunit_labs,

            (* Exit instructions *)
            compile_c_call_prim("terminateML", [mkIntAty 0],
                                NONE,0,rax, (* instead of res we return the result from
                                             * the last function call *)
            I.ret :: C))))))))))))

        val init_link_code = (main_insts o raise_insts o
                              toplevel_handler o allocate o resetregion o
                              overflow_stub o gc_stub o proftick) nil
        fun data_begin C =
            if gc_p() then
                (I.lab (data_begin_init_lab) :: C)
            else C
        fun data_end C =
            if gc_p() then
                (I.dot_align 8 ::
                 I.dot_globl data_begin_addr ::
                 I.lab data_begin_addr ::
                 I.dot_quad "0" ::
                 I.dot_globl data_end_addr ::
                 I.lab data_end_addr ::
                 I.dot_quad "0" ::
                 I.lab (data_end_init_lab) ::   C)
            else C
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
  end


  (* ------------------------------------------------------------ *)
  (*  Emitting Target Code                                        *)
  (* ------------------------------------------------------------ *)
  fun emit(prg: AsmPrg,filename: string) : unit =
    (I.emit(prg,filename);
     print ("[wrote X64 code file:\t" ^ filename ^ "]\n"))
    handle IO.Io {name,...} => Crash.impossible ("CodeGenX64.emit:\nI cannot open \""
                                                 ^ filename ^ "\":\n" ^ name)

end
