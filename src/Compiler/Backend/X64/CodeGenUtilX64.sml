functor CodeGenUtilX64(structure BackendInfo : BACKEND_INFO
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
                         where type label = AddressLabels.label) =
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
  val tmp_freg0 = I.tmp_freg0
  val tmp_freg1 = I.tmp_freg1

  val caller_save_regs_ccall = map RI.lv_to_reg RI.caller_save_ccall_phregs
  val callee_save_regs_ccall = map RI.lv_to_reg RI.callee_save_ccall_phregs
  val all_regs = map RI.lv_to_reg RI.all_regs

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat (s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CodeGenUtilX64." ^ s)
  fun fast_pr stringtree =
    (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
     TextIO.output(!Flags.log, "\n"))

  fun display (title, tree) =
    fast_pr(PP.NODE{start=title ^ ": ",
                    finish="",
                    indent=3,
                    children=[tree],
                    childsep=PP.NOSEP
                    })

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)
  val _ = Flags.add_bool_entry {long="comments_in_asmcode", short=NONE, item=ref false,
                                menu=["Debug", "comments in assembler code"], neg=false,
                                desc="Insert comments in assembler code."}

  val jump_tables = true
  val comments_in_asmcode = Flags.lookup_flag_entry "comments_in_asmcode"
  val gc_p = Flags.is_on0 "garbage_collection"
  val tag_pairs_p = Flags.is_on0 "tag_pairs"

  (* Simple memory profiling - remember to enable the flag
   * SIMPLE_MEMPROF in Runtime/Flags.h when you change this flag. *)
  fun simple_memprof_p () = false
  val stack_min = NameLab "stack_min"

  (**********************************
   * Some code generation utilities *
   **********************************)

  fun comment (str,C) = if !comments_in_asmcode then I.comment str :: C
                        else C
  fun comment_fn (f, C) = if !comments_in_asmcode then I.comment (f()) :: C
                          else C

  val rem_dead_code = I.rem_dead_code

  (********************************)
  (* CG on Top Level Declarations *)
  (********************************)

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
      fun incr () = (counter := !counter + 1; !counter)
    in
      fun new_dynamicFn_lab () : lab = DatLab(Labels.new_named ("DynLab" ^ Int.toString(incr())))
      fun new_string_lab () : lab = DatLab(Labels.new_named ("StringLab" ^ Int.toString(incr())))
      fun new_float_lab () : lab = DatLab(Labels.new_named ("FloatLab" ^ Int.toString(incr())))
      fun new_num_lab () : lab = DatLab(Labels.new_named ("BoxedNumLab" ^ Int.toString(incr())))
      fun reset_label_counter () = counter := 0
    end

    (* Static Data inserted at the beginning of the code. *)
    local
      val static_data : I.inst list ref = ref []
    in
      fun add_static_data insts = (static_data := insts @ !static_data)
      fun reset_static_data () = static_data := []
      fun get_static_data C = !static_data @ C
    end

    (* Additional code blocks added at the end of functions (for avoiding jumping over blocks) *)
    local
      val code_blocks : I.inst list ref = ref []
    in
      fun add_code_block insts = (code_blocks := insts @ !code_blocks)
      fun reset_code_blocks () = code_blocks := []
      fun get_code_blocks () = !code_blocks
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
    fun intToStr (i : IntInf.int) : string =
      let fun tr s = case explode s
                       of #"~"::rest => implode (#"-"::rest)
                        | _ => s
      in tr (IntInf.toString i)
      end

    fun wordToStr (w : IntInf.int) : string =
      "0x" ^ IntInf.fmt StringCvt.HEX w

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

    fun copy (r1, r2, C) = if r1 = r2 then C
                           else case (I.is_xmm r1, I.is_xmm r2) of
                                    (true, true) => I.movsd (R r1, R r2) :: C
                                  | (false, false) => I.movq(R r1, R r2) :: C
                                  | _ => die "copy: incompatible registers"

    fun is_xmm_ea (R r) = I.is_xmm r
      | is_xmm_ea _ = false

    (* Can be used to load from the stack or from a record *)
    (* d = b[n]                                            *)
    fun load_indexed (d:ea,b:reg,n:Offset,C) =
        if I.is_xmm b then die "load_indexed: wrong kind of register"
        else if is_xmm_ea d then I.movsd(D(offset_bytes n,b), d) :: C
        else I.movq(D(offset_bytes n,b), d) :: C

    (* Can be used to update the stack or store in a record *)
    (* b[n] = s                                             *)
    fun store_indexed (b:reg,n:Offset,s:ea,C) =
        if is_xmm_ea s then I.movsd(s,D(offset_bytes n,b)) :: C
        else I.movq(s,D(offset_bytes n,b)) :: C

    (* Calculate an address given a base and an offset *)
    (* dst = base + x                                  *)
    fun base_plus_offset (b:reg,n:Offset,d:reg,C) =
        if d = b andalso isZeroOffset n then C
        else I.leaq(D(offset_bytes n, b), R d) :: C

    fun mkIntAty i = SS.INTEGER_ATY {value=IntInf.fromInt i,
                                     precision=if BI.tag_values() then 63 else 64}

    fun maybeTagIntOrWord {value: IntInf.int, precision:int} : IntInf.int =
        if precision = 31 orelse precision = 63
        then 2 * value + 1                (* use tagged-unboxed representation *)
        else if precision = 32 orelse precision = 64
        then value                        (* use untagged representation - maybe boxed *)
        else die "maybeTagIntOrWord"

    (* formatting of immediate integer and word values *)
    fun fmtInt a : string = intToStr(maybeTagIntOrWord a)
    fun fmtWord a : string = wordToStr(maybeTagIntOrWord a)

    (* Store a constant *)
    fun store_immed (w:Word32.word,r:reg,offset:Offset,C) =
      I.movq(I (wordToStr (Word32.toLargeInt w)), D(offset_bytes offset,r)) :: C

    fun move_immed (0,R d,C) = I.xorq(R d, R d) :: C
      | move_immed (x,d:ea,C) = I.movq(I (intToStr x), d) :: C

    fun move_num (x,ea:ea,C) =
      if (x = "0" orelse x = "0x0") andalso (case ea of R _ => true | _ => false)
          then I.xorq(ea, ea) :: C
      else I.movq(I x, ea) :: C

    fun move_num_boxed (x,ea:ea,C) =
        if not(BI.tag_values()) then
          die "move_num_boxed.boxed integers/words necessary only when tagging is enabled"
        else
          let val num_lab = new_num_lab()
              val () = add_static_data [I.dot_data,
                                        I.dot_align 8,
                                        I.lab num_lab,
                                        I.dot_quad(BI.pr_tag_w(BI.tag_word_boxed(true))),
                                        I.dot_quad x]
          in I.movq(LA num_lab, ea) :: C
          end

    (* returns true if boxed representation is used for
     * integers of the given precision *)
    fun boxedNum (precision:int) : bool =
        (precision = 32 orelse precision = 64) andalso BI.tag_values()

    (* Find a register for aty and generate code to store into the aty *)
    fun resolve_aty_def (SS.STACK_ATY offset,t:reg,size_ff,C) =
        (t,store_indexed(rsp,WORDS(size_ff-offset-1),R t,C))
      | resolve_aty_def (SS.PHREG_ATY phreg,t:reg,size_ff,C) = (phreg,C)
      | resolve_aty_def (SS.UNIT_ATY,t:reg,size_ff,C)  = (t,C)
      | resolve_aty_def _ = die "resolve_aty_def: ATY cannot be defined"

    fun move_num_generic (precision, num, ea, C) =
        if boxedNum precision then move_num_boxed(num, ea, C)
        else move_num(num, ea, C)

    fun move_unit (ea,C) =
        if BI.tag_values() then
            move_immed(IntInf.fromInt BI.ml_unit,ea,C) (* gc needs value! *)
        else C

    (* Make sure that the aty ends up in register dst_reg *)
    fun move_aty_into_reg (aty,dst_reg,size_ff,C) =
        case aty of
            SS.REG_I_ATY offset =>
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
    fun move_reg_into_aty (src_reg:reg,dst_aty,size_ff,C) =
        case dst_aty of
            SS.PHREG_ATY dst_reg => copy(src_reg,dst_reg,C)
          | SS.STACK_ATY offset => store_indexed(rsp,WORDS(size_ff-offset-1),R src_reg,C)
          | SS.UNIT_ATY => C (* wild card definition - do nothing *)
          | _ => die "move_reg_into_aty: ATY not recognized"

    (* dst_aty = src_aty; may kill tmp_reg1 *)
    fun move_aty_to_aty (SS.PHREG_ATY src_reg,dst_aty,size_ff,C) = move_reg_into_aty(src_reg,dst_aty,size_ff,C)
      | move_aty_to_aty (src_aty,SS.PHREG_ATY dst_reg,size_ff,C) = move_aty_into_reg(src_aty,dst_reg,size_ff,C)
      | move_aty_to_aty (src_aty,SS.UNIT_ATY,size_ff,C) = C
      | move_aty_to_aty (src_aty,dst_aty,size_ff,C) =
      let val (reg_for_result,C') = resolve_aty_def(dst_aty,tmp_reg1,size_ff,C)
      in move_aty_into_reg(src_aty,reg_for_result,size_ff,C')
      end

    (* dst_aty = src_aty[offset] *)
    fun move_index_aty_to_aty (SS.PHREG_ATY src_reg,SS.PHREG_ATY dst_reg,offset:Offset,t:reg,size_ff,C) =
        load_indexed(R dst_reg,src_reg,offset,C)
      | move_index_aty_to_aty (SS.PHREG_ATY src_reg,dst_aty,offset:Offset,t:reg,size_ff,C) =
        load_indexed(R t,src_reg,offset,
         move_reg_into_aty(t,dst_aty,size_ff,C))
      | move_index_aty_to_aty (src_aty,SS.PHREG_ATY dst_reg,offset,_,size_ff,C) =
        move_aty_into_reg(src_aty,dst_reg,size_ff,
         load_indexed(R dst_reg,dst_reg,offset,C))
      | move_index_aty_to_aty (src_aty,dst_aty,offset,t:reg,size_ff,C) = (* can be optimised!! *)
        move_aty_into_reg(src_aty,t,size_ff,
         load_indexed(R t,t,offset,
          move_reg_into_aty(t,dst_aty,size_ff,C)))

    (* dst_aty = &lab *)
    fun load_label_addr (lab,dst_aty,t:reg,size_ff,C) =
        case dst_aty of
            SS.PHREG_ATY d => I.movq(LA lab, R d) :: C
          | SS.STACK_ATY offset =>
            I.movq(LA lab, R t) :: store_indexed(rsp, WORDS(size_ff-offset-1), R t, C)
          | _ => die "load_label_addr.wrong ATY"

    (* dst_aty = lab[0] *)
    fun load_from_label (lab,dst_aty,t:reg,size_ff,C) =
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
    fun store_in_label (src_aty,lab,tmp1:reg,size_ff,C) =
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

          val () = add_static_data (I.dot_data ::
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
                                            I.dot_quad (i2s BI.ml_unit)]  (* was "0" but use ml_unit instead
                                                                           * for GC *)

    fun store_aty_indexed (b:reg,n:Offset,aty,t:reg,size_ff,C) =
        let fun ea () = D(offset_bytes n,b)
            fun default () =
                move_aty_into_reg(aty,t,size_ff,
                 store_indexed(b,n,R t,C))
            fun direct_word (w:{value: IntInf.int, precision:int}) : bool =
                not(boxedNum(#precision w)) andalso
                case #precision w of
                    31 => #value w <= 0x7FFF
                  | 32 => #value w <= 0xFFFF
                  | 63 => #value w <= 0x7FFF
                  | 64 => #value w <= 0xFFFF
                  | _ => die "store_aty_indexed.direct_word - weird precision"
            fun direct_int (i:{value: IntInf.int, precision:int}) =
                not(boxedNum(#precision i)) andalso
                case #precision i of
                    31 => #value i <= 0x3FFF andalso #value i > ~0x4000
                  | 32 => #value i <= 0x7FFF andalso #value i > ~0x8000
                  | 63 => #value i <= 0x3FFF andalso #value i > ~0x4000
                  | 64 => #value i <= 0x7FFF andalso #value i > ~0x8000
                  | _ => die "store_aty_indexed.direct_int - weird precision"
        in
            case aty of
                SS.PHREG_ATY s =>
                if I.is_xmm s then I.movsd(R s,ea()) :: C
                else I.movq(R s,ea()) :: C
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
    fun store_aty_in_reg_record (aty,t:reg,b,n:Offset,size_ff,C) =
        store_aty_indexed(b:reg,n:Offset,aty,t:reg,size_ff,C)

    (* Can be used to load from the stack or a record when destination is an ATY *)
    (* dst_aty = base_reg[offset] *)
    fun load_aty_from_reg_record (SS.PHREG_ATY dst_reg,t:reg,base_reg,offset:Offset,size_ff,C) =
        load_indexed(R dst_reg,base_reg,offset,C)
      | load_aty_from_reg_record (dst_aty,t:reg,base_reg,offset:Offset,size_ff,C) =
        load_indexed(R t,base_reg,offset,
         move_reg_into_aty(t,dst_aty,size_ff,C))

    (* base_aty[offset] = src_aty *)
    fun store_aty_in_aty_record (src_aty,base_aty,offset:Offset,t1:reg,t2:reg,size_ff,C) =
        case (src_aty,base_aty) of
            (SS.PHREG_ATY src_reg,SS.PHREG_ATY base_reg) => store_indexed(base_reg,offset,R src_reg,C)
          | (SS.PHREG_ATY src_reg,base_aty) =>
            move_aty_into_reg(base_aty,t2,size_ff,  (* can be optimised *)
             store_indexed(t2,offset,R src_reg,C))
          | (src_aty,SS.PHREG_ATY base_reg) =>
            move_aty_into_reg(src_aty,t1,size_ff,
             store_indexed(base_reg,offset,R t1,C))
          | (src_aty,base_aty) =>
            move_aty_into_reg(src_aty,t1,size_ff, (* can be optimised *)
             move_aty_into_reg(base_aty,t2,size_ff,
              store_indexed(t2,offset,R t1,C)))

    (* push(aty), i.e., rsp-=8; rsp[0] = aty (different than on hp) *)
    (* size_ff is for rsp before rsp is moved. *)
    fun push_aty (aty,t:reg,size_ff,C) =
      let
        fun default () = move_aty_into_reg(aty,t,size_ff,
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
    fun pop_aty (SS.PHREG_ATY aty_reg,t:reg,size_ff,C) = I.pop(R aty_reg) :: C
      | pop_aty (aty,t:reg,size_ff,C) = (I.pop(R t) ::
                                         move_reg_into_aty(t,aty,size_ff,C))

    (* Returns a register with arg and a continuation function. *)
    fun resolve_arg_aty (arg:SS.Aty,t:reg,size_ff:int) : reg * (I.inst list -> I.inst list) =
      case arg
        of SS.PHREG_ATY r => (r, fn C => C)
         | _ => (t, fn C => move_aty_into_reg(arg,t,size_ff,C))

    fun add_aty_to_reg (arg:SS.Aty,tmp:reg,t:reg,size_ff:int,C:I.inst list) : I.inst list =
      case arg
        of SS.PHREG_ATY r => I.addq(R r, R t) :: C
         | _ => move_aty_into_reg(arg,tmp,size_ff, I.addq(R tmp, R t) :: C)

    fun Id x = x
    fun rep8bit (i: IntInf.int) = ~0x80 <= i andalso i <= 0x7F
    fun rep16bit (i: IntInf.int) = ~0x8000 <= i andalso i <= 0x7FFF

    fun protect_arg_aty (arg:SS.Aty,t:reg,size_ff:int,{avoid:SS.Aty}) : ea * (I.inst list -> I.inst list) =
        let fun default () =
                (R t, fn C => move_aty_into_reg(arg,t,size_ff,C))
        in case arg of
               SS.PHREG_ATY r =>
               (case avoid of
                    SS.PHREG_ATY avoid =>
                    if r = avoid then default()
                    else (R r, Id)
                  | _ => (R r, Id))
             | SS.INTEGER_ATY i =>
               if boxedNum (#precision i) then die "protect_arg_aty.boxed int"
               else if rep16bit (#value i) then (I (fmtInt i), Id)
               else default()
             | SS.WORD_ATY w =>
               if boxedNum (#precision w) then die "protect_arg_aty.boxed int"
               else if rep16bit (#value w) then (I (fmtWord w), Id)
               else default()
             | _ => default()
        end

    fun resolve_arg_aty_ea (arg:SS.Aty,t:reg,size_ff:int) : ea * (I.inst list -> I.inst list) =
        let fun default () = (R t, fn C => move_aty_into_reg(arg,t,size_ff,C))
        in case arg of
               SS.PHREG_ATY r => (R r, Id)
             | SS.INTEGER_ATY i =>
               if boxedNum (#precision i) then die "resolve_arg_aty_ea.boxed int"
               else if rep16bit (#value i) then (I (fmtInt i), Id)
               else default()
             | SS.WORD_ATY w =>
               if boxedNum (#precision w) then die "resolve_arg_aty_ea.boxed int"
               else if rep16bit (#value w) then (I (fmtWord w), Id)
               else default()
             | _ => default()
        end

    (* load real into xmm register (freg) *)
    fun load_real (float_aty, t, size_ff, freg) =
      let val disp = if BI.tag_values() then "8"
                     else "0"
      in fn C => case float_aty
                   of SS.PHREG_ATY x => I.movsd(D(disp, x),R freg) :: C
                    | _ => move_aty_into_reg(float_aty,t,size_ff,
                           I.movsd(D(disp, t),R freg) :: C)
      end

    (* store xmm register value (freg) in real value (maybe incl. tag) *)
    fun store_real (base_reg,t:reg,freg,C) =
      if BI.tag_values() then
        store_immed(BI.tag_real false, base_reg, WORDS 0,
        I.movsd (R freg,D("8",base_reg)) :: C)
      else
        I.movsd (R freg,D("0",base_reg)) :: C

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

    fun maybe_align nargs F C =
        if nargs = 0 then F C
        else F (I.addq(I(i2s(8*nargs)),R rsp):: C)

    fun regs_atys nil acc = nil
      | regs_atys (SS.PHREG_ATY r::atys) acc = regs_atys atys (r::acc)
      | regs_atys (_ ::atys) acc = regs_atys atys acc

    fun member r nil = false
      | member r (x::xs) = r = x orelse member r xs

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

            fun mov_int ((aty,r),size_ff,C) =
                if BI.tag_values() then
                  move_aty_into_reg(aty,r,size_ff,
                                    I.shrq(I "1", R r) :: C)
                else
                  move_aty_into_reg(aty,r,size_ff,C)


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
      let fun gen ([],C) = C
            | gen (w::ws,C) = gen(ws,I.dot_quad ("0x"^Word32.fmt StringCvt.HEX w)::C)
      in if gc_p() then gen(ws,C)
         else C
      end

    (* Stub code that makes an immediate forward jump if gc is triggered; otherwise
     * it falls through.
     * reg_map is a register map describing live registers at entry to the function
     * The stub requires reg_map to reside in tmp_reg1 and the return address in tmp_reg0
     *)
    fun do_gc (reg_map:Word32.word,size_ccf,size_rcf,
               size_spilled_region_args) =
      if gc_p() then
        let
          val l_gc_done = new_local_lab "gc_done"
          val l_gc_do = new_local_lab "gc_do"
          val reg_map_immed = "0x" ^ Word32.fmt StringCvt.HEX reg_map
          val size_ff = 0 (*dummy*)
        in
          (fn C => I.cmpq(I "1", L time_to_gc_lab) ::
                   I.je l_gc_do ::
                   I.lab l_gc_done :: C,
           I.lab l_gc_do ::
           I.movq(I reg_map_immed, R tmp_reg1) ::                              (* tmp_reg1 = reg_map  *)
           load_label_addr(l_gc_done,SS.PHREG_ATY tmp_reg0,tmp_reg0,size_ff,   (* tmp_reg0 = return address *)
           I.push(I (i2s size_ccf)) ::
           I.push(I (i2s size_rcf)) ::
           I.push(I (i2s size_spilled_region_args)) ::
           I.jmp(L gc_stub_lab) :: nil))
        end
      else (fn C => C, nil)


    (*********************)
    (* Allocation Points *)
    (*********************)

    (* Status Bits Are Not Cleared! We preserve the value in register t,
     * t may be used in a call to alloc. *)

    fun reset_region (t:reg,tmp:reg,size_ff,C) =
      let val l = new_local_lab "return_from_alloc"
      in copy(t,tmp_reg1,
         I.push(LA l) ::
         I.jmp(L(NameLab "__reset_region")) ::
         I.lab l ::
         copy(tmp_reg1, t, C))
      end

    fun alloc_kill_tmp01 (t:reg,n0:int,size_ff,pp:LS.pp,C) =
        if region_profiling() then
          let val n = n0 + BI.objectDescSizeP
              val l = new_local_lab "return_from_alloc"
              fun post_prof C =
                  (* tmp_reg1 now points at the object descriptor; initialize it *)
                  I.movq(I (i2s pp), D("0",tmp_reg1)) ::               (* first word is pp *)
                  I.movq(I (i2s n0), D("8",tmp_reg1)) ::               (* second word is object size *)
                  I.leaq(D (i2s (8*BI.objectDescSizeP), tmp_reg1), R tmp_reg1) ::
                  C                                                    (* make tmp_reg1 point at object *)
          in copy(t,tmp_reg1,
             I.push(LA l) ::
             move_immed(IntInf.fromInt n, R tmp_reg0,
             I.jmp(L(NameLab "__allocate")) :: (* assumes args in tmp_reg1 and tmp_reg0; result in tmp_reg1 *)
             I.lab l ::
             post_prof
             (copy(tmp_reg1,t,C))))
          end
        else
          let val n = n0
              val l = new_local_lab "return_from_alloc"
              val l_expand = new_local_lab "alloc_expand"
              fun maybe_update_alloc_period C =
                  if gc_p() then
                    I.movq(L(NameLab "alloc_period"), R tmp_reg0) ::
                    I.addq(I (i2s (8*n)), R tmp_reg0) ::
                    I.movq(R tmp_reg0, L(NameLab "alloc_period")) ::
                    C
                  else C
              val () =
                  add_code_block
                      (I.lab l_expand ::                            (* expand:                            *)
                       I.push(LA l) ::                              (*   push continuation label          *)
                       move_immed(IntInf.fromInt n, R tmp_reg0,     (*   tmp_reg0 = n                     *)
                       I.jmp(L(NameLab "__allocate")) :: nil))      (*   jmp to __allocate with args in   *)
          in
            copy(t,tmp_reg1,                                        (*   tmp_reg1 = t                     *)
            I.andq(I (i2s (~4)), R tmp_reg1) ::                     (*   tmp_reg1 = clearBits(tmp_reg1)   *)
            load_indexed(R tmp_reg0,tmp_reg1,WORDS 0,               (*   tmp_reg0 = tmp_reg1[0]           *)
            I.addq(I (i2s (8*n)), R tmp_reg0) ::                    (*   tmp_reg0 = tmp_reg0 + 8n         *)
            I.cmpq(D("8",tmp_reg1), R tmp_reg0) ::                  (*   jmp expand if (tmp_reg0 > boundary) *)
            I.jg l_expand ::                                        (*     (boundary offset 1Word in rd   *)
            store_indexed (tmp_reg1,WORDS 0,R tmp_reg0,             (*   tmp_reg1[0] = tmp_reg0           *)
            I.leaq(D(i2s(~8*n),tmp_reg0),R tmp_reg1) ::             (*   tmp_reg1 = tmp_reg0 - 8n         *)
            maybe_update_alloc_period(
            I.lab l ::                                              (*     tmp_reg1 and tmp_reg0; result  *)
            (copy(tmp_reg1,t,C))))))                                (*     in tmp_reg1.                   *)
          end

    (* When tagging is enabled (for gc) and tag-free pairs (and triples) are enabled
     * then the following function is used for allocating pairs in
     * infinite regions. *)

    fun alloc_untagged_value_kill_tmp01 (t:reg,n,size_ff,pp:LS.pp,C) =  (* n: size of untagged pair, e.g. *)
        alloc_kill_tmp01 (t,n,size_ff,pp,
        I.leaq(D("-8",t), R t) :: C)

    fun set_atbot_bit (dst_reg:reg,C) =
      I.orq(I "2", R dst_reg) :: C

    fun clear_atbot_bit (dst_reg:reg,C) =
      I.btrq(I "1", R dst_reg) :: C

    fun set_inf_bit (dst_reg:reg,C) =
      I.orq(I "1", R dst_reg) :: C

    fun set_inf_bit_and_atbot_bit (dst_reg:reg,C) =
      I.orq(I "3", R dst_reg) :: C

    (* move_aty_into_reg_ap differs from move_aty_into_reg in the case where aty is a phreg! *)
    (* We must always make a copy of phreg because we may overwrite status bits in phreg.    *)
    fun move_aty_into_reg_ap (aty,dst_reg,size_ff,C) =
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

    fun alloc_ap_kill_tmp01 (sma, dst_reg:reg, n, size_ff, C) =
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
    fun prefix_sm (sma,dst_reg:reg,size_ff,C) =
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
    fun store_sm_in_record (sma,tmp:reg,base_reg,offset,size_ff,C) =
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

    fun force_reset_aux_region_kill_tmp0 (sma,t:reg,size_ff,C) =
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

      fun maybe_reset_aux_region_kill_tmp0 (sma,t:reg,size_ff,C) =
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
        fun binary_search (sels,
                           default,
                           opr: I.ea,
                           compile_insts,
                           toInt : 'a -> IntInf.int,
                           C) =
          let
            val sels = map (fn (i,e) => (toInt i, e)) sels
            fun cmp (i,opr,C) =
                if rep16bit i then I.cmpq(I (intToStr i),opr) :: C
                else I.movq(I (intToStr i), R tmp_reg0) :: I.cmpq(R tmp_reg0,opr) :: C
            fun if_not_equal_go_lab (lab,i,C) = cmp(i, opr, I.jne lab :: C)
            fun if_less_than_go_lab (lab,i,C) = cmp(i, opr, I.jl lab :: C)
            fun if_greater_than_go_lab (lab,i,C) = cmp(i, opr, I.jg lab :: C)
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
               fn (sel1,sel2) => IntInf.abs(sel1-sel2), (* sel_dist *)
               fn (lab,sel,_,C) => (I.movq(opr, R tmp_reg0) ::
                                    I.salq(I "3", R tmp_reg0) ::
                                    I.push(R tmp_reg1) ::
                                    I.movq(LA lab,R tmp_reg1) ::
                                    I.addq(R tmp_reg1, R tmp_reg0) ::
                                    I.pop(R tmp_reg1) ::
                                    I.jmp(D(intToStr(~8*sel), tmp_reg0)) ::
                                    rem_dead_code C),
               fn (lab,C) => I.dot_quad' lab :: C, (*add_label_to_jump_tab*)
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

      fun cmpi_kill_tmp01 {box,quad} jump (x,y,d,size_ff,C) =
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            val true_lab = new_local_lab "true"
            val cont_lab = new_local_lab "cont"
            val (inst_cmp, maybeDoubleOfQuadReg) =
                if quad
                then (I.cmpq, fn r => r)
                else (I.cmpl, I.doubleOfQuadReg)
            fun compare C =
              if box then
                I.movq(D("8",y_reg), R tmp_reg1) ::
                I.movq(D("8",x_reg), R tmp_reg0) ::
                inst_cmp(R (maybeDoubleOfQuadReg tmp_reg1),
                         R (maybeDoubleOfQuadReg tmp_reg0)) :: C
              else inst_cmp(R (maybeDoubleOfQuadReg y_reg),
                            R (maybeDoubleOfQuadReg x_reg)) :: C
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

      fun doubleOfQuadEa ea =
          case ea of
              R r => R (I.doubleOfQuadReg r)
            | D _ => die "maybeDoubleOfQuadEa.D"
            | DD _ => die "maybeDoubleOfQuadEa.DD"
            | _ => ea

      fun cmpi_and_jmp_kill_tmp01 {quad} (jump,x,y,lab_t,lab_f,size_ff,C) =
        let
          val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
          val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
          val (inst_cmp, maybeDoubleOfQuadReg) =
              if quad
              then (I.cmpq, fn r => r)
              else (I.cmpl, I.doubleOfQuadReg)
        in
          x_C(y_C(
          inst_cmp(R (maybeDoubleOfQuadReg y_reg),
                   R (maybeDoubleOfQuadReg x_reg)) ::
          jump lab_t ::
          I.jmp (L lab_f) :: rem_dead_code C))
        end

      (* version with boxed arguments; assume tagging is enabled *)
      fun cmpbi_and_jmp_kill_tmp01 {quad} (jump,x,y,lab_t,lab_f,size_ff,C) =
        if BI.tag_values() then
          let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
              val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
              val (inst_cmp, maybeDoubleOfQuadReg) =
                  if quad
                  then (I.cmpq, fn r => r)
                  else (I.cmpl, I.doubleOfQuadReg)
          in
            x_C(y_C(
            I.movq(D("8", y_reg), R tmp_reg1) ::
            I.movq(D("8", x_reg), R tmp_reg0) ::
            inst_cmp(R (maybeDoubleOfQuadReg tmp_reg1),
                     R (maybeDoubleOfQuadReg tmp_reg0)) ::
            jump lab_t ::
            I.jmp (L lab_f) :: rem_dead_code C))
          end
        else die "cmpbi_and_jmp_kill_tmp01: tagging disabled!"

(*
      (* MEMO: we need to support spilled f64 values *)
     fun resolve_f64_aty f aty : reg =
         case aty of
             SS.PHREG_ATY x => if I.is_xmm x then x
                               else die ("resolve_f64_aty: expecting xmm register. " ^ f() ^ " - got " ^ I.pr_reg x)
           | _ => die ("resolve_f64_aty: expecting physical register - " ^ f())
  *)

      fun cmpf64_and_jmp (jump,x,y,lab_t,lab_f,size_ff,C) =
          let val (x,x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
              val (y,y_C) = resolve_arg_aty(y,tmp_freg1,size_ff)
          in x_C(y_C(I.ucomisd (R y, R x) ::
                     jump lab_t ::
                     I.jmp (L lab_f) ::
                     rem_dead_code C))
          end

      fun jump_overflow C = I.jo (NameLab "__raise_overflow") :: C

      fun sub_num_kill_tmp01 {ovf,tag,quad} (x,y,d,size_ff,C) =
          let val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
              val (x_reg,x_C) = resolve_arg_aty(x,d_reg,size_ff)
              val (y_ea,y_C) = protect_arg_aty(y,tmp_reg1,size_ff,{avoid=d})

              fun check_ovf C = if ovf then jump_overflow C else C
              fun do_tag C = if tag then I.addq(I "1",R d_reg) :: check_ovf C   (* check twice *)
                             else C
              val (inst_sub, maybeDoubleOfQuadEa) =
                  if quad
                  then (I.subq, fn ea => ea)
                  else (I.subl, doubleOfQuadEa)
          in
            y_C(x_C(
            copy(x_reg, d_reg,
            inst_sub(maybeDoubleOfQuadEa y_ea,
                     maybeDoubleOfQuadEa (R d_reg)) ::
            check_ovf (do_tag C'))))
          end

      fun add_num_kill_tmp01 {ovf,tag,quad} (x,y,d,size_ff,C) =  (* Be careful - when tag and ovf, add may
                                                                  * raise overflow when it is not supposed
                                                                  * to, if one is not careful! sub_num above
                                                                  * is ok, I think! mael 2001-05-19 *)
          let val (inst_add, inst_sar, inst_cmp, maybeDoubleOfQuadReg) =
                  if quad
                  then (I.addq, I.sarq, I.cmpq, fn r => r)
                  else (I.addl, I.sarl, I.cmpl, I.doubleOfQuadReg)
              fun default () =
                 let val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
                     fun check_ovf C = if ovf then jump_overflow C else C
                     fun do_tag C = if tag then inst_add(I "-1", R (maybeDoubleOfQuadReg d_reg)) :: check_ovf C
                                    else C
                 in if tag andalso ovf then
                      let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
                          val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
                      in
                      (x_C(y_C(
                       copy(y_reg, tmp_reg1, inst_sar(I "1", R (maybeDoubleOfQuadReg tmp_reg1)) ::    (* t1 = untag y *)
                       copy(x_reg, tmp_reg0, inst_sar(I "1", R (maybeDoubleOfQuadReg tmp_reg0)) ::    (* t0 = untag x *)
                       inst_add(R (maybeDoubleOfQuadReg tmp_reg0),
                                R (maybeDoubleOfQuadReg tmp_reg1)) ::             (* t1 = t1 + t0 *)
                       copy(tmp_reg1, d_reg,
                       I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::              (* d = tag d *)
                       inst_sar(I "1", R (maybeDoubleOfQuadReg d_reg)) ::         (* d = untag d *)
                       inst_cmp(R (maybeDoubleOfQuadReg d_reg),
                                R (maybeDoubleOfQuadReg tmp_reg1)) ::
                       I.jne (NameLab "__raise_overflow") ::
                       I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::              (* d = tag d *)
                       C'))))))
                      end
                    else
                      let (* rearrange x and y if y=d *)
                          val (x,y) = if SS.eq_aty(y,d) then (y,x) else (x,y)
                          val (x_reg,x_C) = resolve_arg_aty(x,d_reg,size_ff)
                          val (y_ea,y_C) = protect_arg_aty(y,tmp_reg1,size_ff,{avoid=d})
                          val maybeDoubleOfQuadEa = if quad then Id else doubleOfQuadEa
                      in
                      (y_C(x_C(
                       copy(x_reg, d_reg,
                       inst_add(maybeDoubleOfQuadEa y_ea,
                                maybeDoubleOfQuadEa (R d_reg)) ::
                       check_ovf (do_tag C')))))
                      end
                 end
          in case y of
                 SS.INTEGER_ATY {value,...} =>
                 if tag andalso ovf andalso rep16bit value then
                   let val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
                       val (x_reg,x_C) = resolve_arg_aty(x,d_reg,size_ff)
                   in x_C(
                       copy(x_reg,d_reg,
                       inst_add(I (intToStr (2*value)), R (maybeDoubleOfQuadReg d_reg)) ::
                       jump_overflow C'))
                   end
                 else default()
               | _ => default()
          end


      fun mul_num_kill_tmp01 {ovf,tag,quad} (x,y,d,size_ff,C) = (* does (1 * valOf Int31.minInt) raise Overflow ? *)
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            fun check_ovf C = if ovf then jump_overflow C else C
            val (inst_imul, inst_add, inst_sub, inst_sar, maybeDoubleOfQuadReg) =
                if quad
                then (I.imulq, I.addq, I.subq, I.sarq, fn r => r)
                else (I.imull, I.addl, I.subl, I.sarl, I.doubleOfQuadReg)

        in x_C(y_C(
           copy(y_reg, tmp_reg1,
           copy(x_reg, d_reg,
           if tag then (* A[i*j] = 1 + (A[i] >> 1) * (A[j]-1) *)
                inst_sar(I "1", R (maybeDoubleOfQuadReg d_reg)) ::
                inst_sub(I "1", R (maybeDoubleOfQuadReg tmp_reg1)) ::
                inst_imul(R (maybeDoubleOfQuadReg tmp_reg1),
                          R (maybeDoubleOfQuadReg d_reg)) ::
                check_ovf (
                inst_add(I "1", R (maybeDoubleOfQuadReg d_reg)) ::
                check_ovf C')
           else
             inst_imul(R (maybeDoubleOfQuadReg tmp_reg1),
                       R (maybeDoubleOfQuadReg d_reg)) ::
             check_ovf C'))))
        end

      fun neg_int_kill_tmp0 {tag,quad} (x,d,size_ff,C) =
        let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
            val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
            val (inst_add, inst_neg, maybeDoubleOfQuadReg) =
                if quad
                then (I.addq, I.negq, fn r => r)
                else (I.addl, I.negl, I.doubleOfQuadReg)
            fun do_tag C = if tag then inst_add (I "2", R (maybeDoubleOfQuadReg d_reg)) ::
                                       jump_overflow C
                           else C
        in x_C(copy(x_reg, d_reg,
           inst_neg (R (maybeDoubleOfQuadReg d_reg)) ::
           jump_overflow (
           do_tag C')))
        end

      fun neg_int_boxed_kill_tmp0 {quad:bool} (b,x,d,size_ff,C) =
        if not(BI.tag_values()) then die "neg_int_boxed_kill_tmp0.tagging required"
        else
          let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
              val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
              val (inst_neg, maybeDoubleOfQuadReg) =
                  if quad
                  then (I.negq, fn r => r)
                  else (I.negl, I.doubleOfQuadReg)
          in x_C(
             load_indexed(R tmp_reg0,x_reg,WORDS 1,
             inst_neg(R (maybeDoubleOfQuadReg tmp_reg0)) ::
             jump_overflow (
             move_aty_into_reg(b,d_reg,size_ff,
             store_indexed(d_reg,WORDS 1, R tmp_reg0,                        (* store negated value *)
             store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))   (* store tag *)
          end

     fun abs_int_kill_tmp0 {tag,quad} (x,d,size_ff,C) =
       let val cont_lab = new_local_lab "cont"
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           val (inst_add, inst_cmp, inst_neg, maybeDoubleOfQuadReg) =
               if quad
               then (I.addq, I.cmpq, I.negq, fn r => r)
               else (I.addl, I.cmpl, I.negl, I.doubleOfQuadReg)
           fun do_tag C = if tag then inst_add(I "2", R (maybeDoubleOfQuadReg d_reg)) ::
                                      jump_overflow C
                          else C
       in
         x_C(copy(x_reg,d_reg,
         inst_cmp (I "0", R (maybeDoubleOfQuadReg d_reg)) ::
         I.jge cont_lab ::
         inst_neg (R (maybeDoubleOfQuadReg d_reg)) ::
         jump_overflow (
         do_tag (
         I.lab cont_lab :: C'))))
       end


     fun abs_int_boxed_kill_tmp0 {quad} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "abs_int_boxed_kill_tmp0.tagging required"
       else
       let val cont_lab = new_local_lab "cont"
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
           val (inst_cmp, inst_neg, maybeDoubleOfQuadReg) =
               if quad
               then (I.cmpq, I.negq, fn r => r)
               else (I.cmpl, I.negl, I.doubleOfQuadReg)
       in
         x_C(
         load_indexed(R tmp_reg0,x_reg,WORDS 1,
         inst_cmp (I "0", R (maybeDoubleOfQuadReg tmp_reg0)) ::
         I.jge cont_lab ::
         inst_neg (R (maybeDoubleOfQuadReg tmp_reg0)) ::
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

     fun int31_to_int64ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(copy(x_reg, d_reg, I.sarl (I "1", R (I.doubleOfQuadReg d_reg)) ::
                                 I.movslq (R (I.doubleOfQuadReg d_reg),
                                           R d_reg) :: C'))
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

     fun int64_to_int31 {boxedarg} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
       in x_C(
          maybe_unbox(   (* MEMO: we should raise Overflow more often *)
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

     fun word64_to_int31 {boxedarg,ovf} (x,d,size_ff,C) =
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
          I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
          C'))
       end

     (* Conversions involving 64bit ints and words *)

     fun word64ub_to_word32ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(copy(x_reg, d_reg, C')) (* just do a copy *)
       end

     fun word64ub_to_int32ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
           (* to test whether the argument can be represented in 32 bits, we copy with
            * zero-extension and compare *)
       in x_C(copy(x_reg,tmp_reg0,
              I.mov(R (I.doubleOfQuadReg tmp_reg0), R (I.doubleOfQuadReg d_reg)) ::
              I.btq(I "31", R d_reg) ::        (* sign bit set? *)
              I.jc (NameLab "__raise_overflow") ::
              I.cmpq(R tmp_reg0, R d_reg) ::
              I.jne (NameLab "__raise_overflow") :: C'))
       end

     fun int64ub_to_int32ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           (* to test whether the argument can be represented in 32 bits, we copy with
            * sign-extension and compare *)
       in x_C(copy(x_reg,tmp_reg0,
              I.movslq(R (I.doubleOfQuadReg tmp_reg0), R d_reg) ::
              I.cmpq(R tmp_reg0, R d_reg) ::
              I.jne (NameLab "__raise_overflow") :: C'))
       end

     fun int32ub_to_int64ub (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(I.movslq(R (I.doubleOfQuadReg x_reg),
                       R d_reg) :: C')  (* sign-extend *)
       end

     fun num64ub_to_num64ub {ovf} (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           val C'' = if ovf then
                       I.btq(I "63", R d_reg) ::     (* sign bit set? *)
                       I.jc (NameLab "__raise_overflow") :: C'
                     else C'
       in x_C(copy(x_reg, d_reg, C''))
       end

     fun int64_to_int63 {boxedarg} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
       in x_C(
          maybe_unbox(
          I.imulq(I "2", R d_reg) ::
          jump_overflow (
          I.addq(I "1", R d_reg) :: C')))   (* No need to check for overflow after adding 1; the
                                             * intermediate result is even (after multiplying
                                             * with 2) so adding one cannot give Overflow because the
                                             * largest integer is odd! mael 2001-04-29 *)
       end

     fun word64_to_num63 {boxedarg,ovf} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
           fun check_ovf C =
             if ovf then
               I.btq(I "62", R d_reg) ::
               I.jc (NameLab "__raise_overflow") ::
               C
             else C
       in x_C(
          maybe_unbox(
          check_ovf(
          I.imulq(I "2", R d_reg) ::
          jump_overflow (                    (* memo: why is this needed? *)
          I.addq(I "1", R d_reg) :: C'))))   (* No need to check for overflow after adding 1; the
                                              * intermediate result is even (after multiplying
                                              * with 2) so adding one cannot give Overflow because the
                                              * largest integer is odd! mael 2001-04-29 *)
       end

     fun word32_to_word63 {boxedarg,signext} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
           fun maybe_signext C =
               if signext then
                 I.movslq(R (I.doubleOfQuadReg d_reg), R d_reg) :: C
               else C
       in x_C(
          maybe_unbox(
          maybe_signext(
          I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
          C')))
       end

     fun word63_to_word31 (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(
          I.mov(R (I.doubleOfQuadReg x_reg), R (I.doubleOfQuadReg d_reg)) ::
          C')
       end

     fun num31_to_num63 {signext} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
       in x_C(if signext then
                I.movslq(R (I.doubleOfQuadReg x_reg), R d_reg) :: C'
              else
                I.mov(R (I.doubleOfQuadReg x_reg), R (I.doubleOfQuadReg d_reg)) ::
                C'
             )
       end

     fun int63_to_int31 (x,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
           (* to test whether the argument can be represented in 32 (31) bits, we copy with
            * sign-extension and compare *)
       in x_C(copy(x_reg,tmp_reg0,
              I.movslq(R (I.doubleOfQuadReg tmp_reg0), R d_reg) ::
              I.cmpq(R tmp_reg0, R d_reg) ::
              I.jne (NameLab "__raise_overflow") :: C'))
       end

     fun word64_to_word31 {boxedarg} (x,d,size_ff,C) =
       let
           val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff, C)
           fun maybe_unbox C = if boxedarg then load_indexed(R d_reg,x_reg,WORDS 1,C)
                               else copy(x_reg,d_reg,C)
       in x_C(
          maybe_unbox(
          I.mov(R (I.doubleOfQuadReg d_reg), R (I.doubleOfQuadReg d_reg)) ::
          I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
          C'))
       end

     fun num63_to_num64ub {shr_inst} (x,d,size_ff,C) =
       if BI.tag_values() then die "num63_to_num64ub.tagging_enabled"
       else
         (* shr_inst is either I.sarq (sign extend) or I.shrq *)
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (copy(x_reg, d_reg,
                shr_inst (I "1", R d_reg) ::
                C'))
         end

     fun word31_to_word64ub {signext} (x,d,size_ff,C) =
       if BI.tag_values() then die "word31_to_word64ub.tagging_enabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
             fun maybe_signext C =
                 if signext then
                   I.sarl (I "1", R (I.doubleOfQuadReg d_reg)) ::
                   I.movslq(R (I.doubleOfQuadReg d_reg), R d_reg) :: C
                 else I.shrq (I "1", R d_reg) :: C
         in
           x_C (
           copy(x_reg, d_reg,
           maybe_signext C'))
         end

     fun word32ub_to_word64ub {signext} (x,d,size_ff,C) =
       if BI.tag_values() then die "word32ub_to_word64ub.tagging_enabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (if signext then
                  I.movslq(R (I.doubleOfQuadReg x_reg),
                           R d_reg) :: C'
                else (* zero-extend the rest of d_reg *)
                  I.mov(R (I.doubleOfQuadReg x_reg),
                        R (I.doubleOfQuadReg d_reg)) :: C'
               )
         end

     (* unboxed f64 operations *)

     fun copy_f64 (x,y,C) =
         if x = y then C
         else if I.is_xmm x andalso I.is_xmm y then I.movsd(R x,R y)::C
         else die "copy_f64: expecting xmm registers"

     fun bin_f64_op s finst (x,y,d,size_ff:int,C) =
         let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
             val (y, y_C) = resolve_arg_aty(y,tmp_freg1,size_ff)
             val (d, C') = resolve_aty_def(d,tmp_freg0,size_ff, C)
         in x_C(y_C(
            if y = d then
              if x = d then
                finst(R d, R d) :: C'
              else
                copy_f64(y, tmp_freg1,
                copy_f64(x, d,
                finst(R tmp_freg1, R d) ::
                C'))
            else
              copy_f64(x, d,
              finst(R y, R d) ::
              C') ))
         end

     fun uno_f64_op s finst (x,d,size_ff:int,C) =
         let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
             val (d, C') = resolve_aty_def(d,tmp_freg0,size_ff,C)
         in x_C(finst(R x, R d) :: C')
         end

     val plus_f64 = bin_f64_op "addsd" I.addsd
     val minus_f64 = bin_f64_op "subsd" I.subsd
     val mul_f64 = bin_f64_op "mulsd" I.mulsd
     val div_f64 = bin_f64_op "divsd" I.divsd
     val max_f64 = bin_f64_op "maxsd" I.maxsd
     val min_f64 = bin_f64_op "minsd" I.minsd
     val sqrt_f64 = uno_f64_op "sqrtsd" I.sqrtsd

     fun neg_f64 (x,d,size_ff:int,C) =
       let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
           val (d, C') = resolve_aty_def(d,tmp_freg1,size_ff, C)
       in x_C(copy_f64 (x, tmp_freg0,
              I.xorps (R d, R d) ::
              I.subsd (R tmp_freg0, R d) :: C'))
       end

     fun abs_f64 (x,d,size_ff,C) =
       let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
           val (d, C') = resolve_aty_def(d,tmp_freg1,size_ff, C)
       in x_C(I.movsd (R x, R tmp_freg0) ::
              I.xorps (R d, R d) ::
              I.subsd (R tmp_freg0, R d) ::
              I.maxsd (R tmp_freg0, R d) :: C')
       end

     datatype cond = LESSTHAN | LESSEQUAL | GREATERTHAN | GREATEREQUAL
     fun pp_cond LESSTHAN = "LESSTHAN"
       | pp_cond LESSEQUAL = "LESSEQUAL"
       | pp_cond GREATERTHAN = "GREATERTHAN"
       | pp_cond GREATEREQUAL = "GREATEREQUAL"
     fun cmpf64_kill_tmp0 cond (x,y,d,size_ff,C) = (* ME MEMO *)
         let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
             val (y, y_C) = resolve_arg_aty(y,tmp_freg1,size_ff)
             val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
             val true_lab = new_local_lab "true"
             val cont_lab = new_local_lab "cont"
             val jump = (* from gcc experiments *)
                 case cond of
                     LESSTHAN => I.jb      (*below*)
                   | LESSEQUAL => I.jbe    (*below or equal*)
                   | GREATERTHAN => I.ja   (*above*)
                   | GREATEREQUAL => I.jae (*above or equal*)
         in x_C(y_C(I.ucomisd (R y, R x) ::
                    jump true_lab ::
                    I.movq(I (i2s BI.ml_false), R d_reg) ::
                    I.jmp(L cont_lab) ::
                    I.lab true_lab ::
                    I.movq(I (i2s BI.ml_true), R d_reg) ::
                    I.lab cont_lab ::
                    C'))
         end

     local
       fun mov_int ((aty,r),size_ff,C) =
           if BI.tag_values() then
             move_aty_into_reg(aty,r,size_ff,
                               I.sarq(I "1", R r) :: C)
           else
             move_aty_into_reg(aty,r,size_ff,C)
     in
       fun int_to_f64 (x,d,size_ff,C) =
           let val (d, C') = resolve_aty_def(d,tmp_freg0,size_ff,C)
           in mov_int ((x,tmp_reg0),size_ff,
              I.cvtsi2sdq(R tmp_reg0, R d) :: C')
           end
     end

     fun real_to_f64 (x,d,size_ff,C) =
         let val (d, C') = resolve_aty_def(d,tmp_freg0,size_ff,C)
         in load_real (x, tmp_reg0, size_ff, d) C'
         end

     fun f64_to_real_kill_tmp01 (b,x,d,size_ff,C) =
         let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
             val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
             val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
         in x_C(b_C(store_real(b_reg,tmp_reg1,x,
            copy(b_reg,d_reg, C'))))
         end


     (* boxed operations on reals (floats) *)

     fun bin_float_op_kill_tmp01 finst (x,y,b,d,size_ff,C) =
       let val x_C = load_real(x, tmp_reg0, size_ff, tmp_freg1)
           val y_C = load_real(y, tmp_reg0, size_ff, tmp_freg0)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         y_C(x_C(finst(R tmp_freg0,R tmp_freg1) ::
         b_C(store_real(b_reg,tmp_reg1,tmp_freg1,
         copy(b_reg,d_reg, C')))))
       end

     fun addf_kill_tmp01 a = bin_float_op_kill_tmp01 I.addsd a
     fun subf_kill_tmp01 a = bin_float_op_kill_tmp01 I.subsd a
     fun mulf_kill_tmp01 a = bin_float_op_kill_tmp01 I.mulsd a
     fun divf_kill_tmp01 a = bin_float_op_kill_tmp01 I.divsd a

     fun negf_kill_tmp01 (b,x,d,size_ff,C) =
       let val x_C = load_real(x, tmp_reg0, size_ff,tmp_freg1)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         x_C(I.xorps (R tmp_freg0,R tmp_freg0) :: I.subsd (R tmp_freg1,R tmp_freg0) ::
         b_C(store_real(b_reg,tmp_reg1,tmp_freg0,
         copy(b_reg,d_reg, C'))))
       end

     fun absf_kill_tmp01 (b,x,d,size_ff,C) =
       let val x_C = load_real(x, tmp_reg0, size_ff,tmp_freg1)
           val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
           val (d_reg, C') = resolve_aty_def(d, tmp_reg0, size_ff, C)
       in
         x_C(I.xorps (R tmp_freg0,R tmp_freg0) :: I.subsd (R tmp_freg1,R tmp_freg0) :: I.maxsd (R tmp_freg1,R tmp_freg0) ::
         b_C(store_real(b_reg,tmp_reg1,tmp_freg0,
         copy(b_reg,d_reg, C'))))
       end

     fun cmpf_kill_tmp01 cond (x,y,d,size_ff,C) = (* ME MEMO *)
       let val x_C = load_real(x, tmp_reg0, size_ff, tmp_freg0)
           val y_C = load_real(y, tmp_reg0, size_ff, tmp_freg1)
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
         load_args(I.ucomisd (R tmp_freg1, R tmp_freg0) ::
         jump true_lab ::
         I.movq(I (i2s BI.ml_false), R d_reg) ::
         I.jmp(L cont_lab) ::
         I.lab true_lab ::
         I.movq(I (i2s BI.ml_true), R d_reg) ::
         I.lab cont_lab ::
         C')
       end

     fun bin_op_kill_tmp01 {quad} inst (x,y,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
           val maybeDoubleOfQuadReg = if quad then fn r => r
                                      else I.doubleOfQuadReg
       in
         x_C(y_C(
         copy(y_reg, tmp_reg1,
         copy(x_reg, d_reg,
         inst(R (maybeDoubleOfQuadReg tmp_reg1),
              R (maybeDoubleOfQuadReg d_reg)) :: C'))))
       end

     (* andb and orb are the same for 31 bit (tagged) and
      * 32 bit (untagged) representations; same for 63/64 bits *)
     fun andb_word_kill_tmp01 {quad:bool} a =
         let val inst = if quad then I.andq else I.andl
         in bin_op_kill_tmp01 {quad=quad} inst a     (* A[x&y] = A[x] & A[y]  tagging *)
         end

     fun orb_word_kill_tmp01 {quad:bool} a =
         let val inst = if quad then I.orq else I.orl
         in bin_op_kill_tmp01 {quad=quad} inst a     (* A[x|y] = A[x] | A[y]  tagging *)
         end

     (* xorb needs to set the lowest bit for the 31 bit (tagged) version and for the 63 bit (tagged) version *)
     fun xorb_word_kill_tmp01 {tag,quad} (x,y,d,size_ff,C) =
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
           fun do_tag C = if tag then I.orq(I "1", R d_reg) :: C else C
           val (inst_xor, maybeDoubleOfQuadReg) =
               if quad
               then (I.xorq, fn r => r)
               else (I.xorl, I.doubleOfQuadReg)
       in
         x_C(y_C(
         copy(y_reg, tmp_reg1,
         copy(x_reg, d_reg,
         inst_xor(R (maybeDoubleOfQuadReg tmp_reg1),
                  R (maybeDoubleOfQuadReg d_reg)) ::
         do_tag C'))))
       end

     fun binop_word_boxed__ {ovf,quad} inst (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       if not(BI.tag_values()) then die "binop_word_boxed__.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (y_reg,y_C) = resolve_arg_aty(y,tmp_reg1,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
             fun check_ovf C = if ovf then jump_overflow C else C
             val maybeDoubleOfQuadReg = if quad then fn r => r
                                        else I.doubleOfQuadReg
         in
           x_C(
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           y_C(
           load_indexed(R tmp_reg1,y_reg,WORDS 1,
           inst(R (maybeDoubleOfQuadReg tmp_reg0),
                R (maybeDoubleOfQuadReg tmp_reg1)) ::
           check_ovf (
           move_aty_into_reg(r,d_reg,size_ff,
           store_indexed(d_reg,WORDS 1,R tmp_reg1,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))))  (* store tag *)
         end

     fun addw32boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.addl (r,x,y,d,size_ff,C)

     fun addw64boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.addq (r,x,y,d,size_ff,C)

     fun subw32boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.subl (r,y,x,d,size_ff,C) (* x and y swapped, see spec for subq *)

     fun subw64boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.subq (r,y,x,d,size_ff,C) (* x and y swapped, see spec for subq *)

     fun mulw32boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.imull (r,x,y,d,size_ff,C)

     fun mulw64boxed (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.imulq (r,x,y,d,size_ff,C)

     fun orw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.orl (r,x,y,d,size_ff,C)

     fun orw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.orq (r,x,y,d,size_ff,C)

     fun andw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.andl (r,x,y,d,size_ff,C)

     fun andw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.andq (r,x,y,d,size_ff,C)

     fun xorw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
         binop_word_boxed__ {ovf=false,quad=false} I.xorl (r,x,y,d,size_ff,C)

     fun xorw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
         binop_word_boxed__ {ovf=false,quad=true} I.xorq (r,x,y,d,size_ff,C)

     fun mul_int32b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=false} I.imull (b,x,y,d,size_ff,C)

     fun mul_int64b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=true} I.imulq (b,x,y,d,size_ff,C)

     fun sub_int32b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=false} I.subl (b,y,x,d,size_ff,C)

     fun sub_int64b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=true} I.subq (b,y,x,d,size_ff,C)

     fun add_int32b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=false} I.addl (b,x,y,d,size_ff,C)

     fun add_int64b (b,x,y,d,size_ff,C) =
         binop_word_boxed__ {ovf=true,quad=true} I.addq (b,x,y,d,size_ff,C)

     fun num31_to_num_boxed {quad} (b,x,d,size_ff,C) =   (* a boxed word is tagged as a scalar record *)
       if BI.tag_values() then
         let val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
             val (inst_sar, maybeDoubleOfQuadReg) =
                 if quad
                 then (I.sarq, fn r => r)
                 else (I.sarl, I.doubleOfQuadReg)
         in
           move_aty_into_reg(x,tmp_reg0,size_ff,
           inst_sar (I "1", R (maybeDoubleOfQuadReg tmp_reg0)) ::
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg,WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))   (* store tag *)
         end
       else die "num31_to_num_boxed.tagging_disabled"

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

     fun num64b_to_num64b {ovf:bool} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "num64b_to_num64b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
             fun check_ovf C =
               if ovf then
                 I.btq(I "63", R tmp_reg0) ::     (* sign bit set? *)
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

     fun word32b_to_word64b {signext} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "num32b_to_num64b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
             fun maybe_signext C =
                 if signext then
                   I.movslq(R (I.doubleOfQuadReg tmp_reg0), R tmp_reg0) :: C
                 else C
         in
           x_C (
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           maybe_signext(
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg, WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))  (* store tag *)
         end

     fun int32b_to_int64b (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "int32b_to_int64b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           I.movslq(R (I.doubleOfQuadReg tmp_reg0), R tmp_reg0) ::       (* sign-extend *)
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg, WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))  (* store tag *)
         end

     fun num64b_to_num32b (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "num64b_to_num32b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (
           load_indexed(R tmp_reg0,x_reg,WORDS 1,
           I.mov(R (I.doubleOfQuadReg tmp_reg0),R (I.doubleOfQuadReg tmp_reg0)) ::  (* clears the upper bits *)
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg, WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))  (* store tag *)
         end

     fun num63_to_num64b {shr_inst} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "int63_to_int64b.tagging_disabled"
       else
         (* shr_inst is either I.sarq (sign extend) or I.shrq *)
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (copy(x_reg, tmp_reg0,
                shr_inst (I "1", R tmp_reg0) ::
                move_aty_into_reg(b,d_reg,size_ff,
                store_indexed(d_reg, WORDS 1, R tmp_reg0,
                store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))  (* store tag *)
         end

     fun word63_to_word32b (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "word63_to_word32b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
         in
           x_C (copy(x_reg, tmp_reg0,
                I.shrq (I "1", R tmp_reg0) ::
                I.mov(R (I.doubleOfQuadReg tmp_reg0), R (I.doubleOfQuadReg tmp_reg0)) ::
                move_aty_into_reg(b,d_reg,size_ff,
                store_indexed(d_reg, WORDS 1, R tmp_reg0,
                store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))  (* store tag *)
         end

     fun int63_to_int32b (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "int63_to_int32b.tagging_disabled"
       else
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
           (* to test whether the argument can be represented in 32 (31) bits, we copy with
            * sign-extension and compare *)
       in x_C(copy(x_reg, tmp_reg0,
              I.sarq (I "1", R tmp_reg0) ::
              I.movslq(R (I.doubleOfQuadReg tmp_reg0), R tmp_reg1) ::
              I.cmpq(R tmp_reg0, R tmp_reg1) ::
              I.jne (NameLab "__raise_overflow") ::
              copy(tmp_reg1, tmp_reg0,
              move_aty_into_reg(b,d_reg,size_ff,
              store_indexed(d_reg, WORDS 1, R tmp_reg0,
              store_immed(BI.tag_word_boxed false, d_reg, WORDS 0,
              C'))))))
       end

     fun word31_to_word64b {signext} (b,x,d,size_ff,C) =
       if not(BI.tag_values()) then die "word31_to_word64b.tagging_disabled"
       else
         let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff, C)
             fun maybe_signext C =
                 if signext then
                   I.sarl (I "1", R (I.doubleOfQuadReg tmp_reg0)) ::
                   I.movslq(R (I.doubleOfQuadReg tmp_reg0), R tmp_reg0) :: C
                 else I.shrq (I "1", R tmp_reg0) :: C
         in
           x_C (
           copy(x_reg, tmp_reg0,
           maybe_signext(
           move_aty_into_reg(b,d_reg,size_ff,
           store_indexed(d_reg, WORDS 1, R tmp_reg0,
           store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C'))))))  (* store tag *)
         end

     fun shift_word_boxed__ {quad} inst (r,x,y,d,size_ff,C) =
       if not(BI.tag_values()) then die "shift_word_boxed__.tagging is not enabled as required"
       else
       (* y is unboxed and tagged *)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg0,size_ff,C)
           val maybeDoubleOfQuadReg = if quad then fn r => r
                                      else I.doubleOfQuadReg
       in
         x_C(
         load_indexed(R tmp_reg1,x_reg,WORDS 1,
         copy(rcx, tmp_reg0,                         (* save rcx *)
         y_C(
         copy(y_reg,rcx,                             (* tmp_reg0 = %r10, see InstsX64.sml *)
         I.sarq (I "1", R rcx) ::                    (* untag y: y >> 1 *)
         inst(R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::
         copy(tmp_reg0, rcx,                         (* restore rcx *)
         move_aty_into_reg(r,d_reg,size_ff,
         store_indexed(d_reg,WORDS 1, R tmp_reg1,
         store_immed(BI.tag_word_boxed false, d_reg, WORDS 0, C')))))))))   (* store tag *)
       end

     fun shift_leftw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_word_boxed__ {quad=false} I.sall (r,x,y,d,size_ff,C)

     fun shift_leftw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
       shift_word_boxed__ {quad=true} I.salq (r,x,y,d,size_ff,C)

     fun shift_right_signedw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_word_boxed__ {quad=false} I.sarl (r,x,y,d,size_ff,C)

     fun shift_right_signedw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
       shift_word_boxed__ {quad=true} I.sarq (r,x,y,d,size_ff,C)

     fun shift_right_unsignedw32boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word32.sml *)
       shift_word_boxed__ {quad=false} I.shrl (r,x,y,d,size_ff,C)

     fun shift_right_unsignedw64boxed__ (r,x,y,d,size_ff,C) = (* Only used when tagging is enabled; Word64.sml *)
       shift_word_boxed__ {quad=true} I.shrq (r,x,y,d,size_ff,C)

     fun shift_left_word_kill_tmp01 {tag,quad} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10*)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
           val (inst_sal, maybeDoubleOfQuadReg) =
               if quad
               then (I.salq, fn r => r)
               else (I.sall, I.doubleOfQuadReg)
       in
         if tag then                     (* 1 + ((x - 1) << (y >> 1)) *)
           x_C(
           I.movq(R rcx, R tmp_reg0) ::                       (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.decq (R tmp_reg1) ::                                  (* x - 1  *)
           untag_y (                                               (* y >> 1 *)
           inst_sal (R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::   (*   <<   *)
           I.movq (R tmp_reg0, R rcx) ::                           (* restore rcx *)
           I.incq (R tmp_reg1) ::           (* 1 +    *)
           copy(tmp_reg1, d_reg, C'))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           inst_sal(R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun shift_right_signed_word_kill_tmp01 {tag,quad} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10*)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
           val (inst_sar, maybeDoubleOfQuadReg) =
               if quad
               then (I.sarq, fn r => r)
               else (I.sarl, I.doubleOfQuadReg)
       in
         if tag then                         (* 1 | ((x) >> (y >> 1)) *)
           x_C(
           I.movq(R rcx, R tmp_reg0) ::                        (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           I.decq (R tmp_reg1) ::                                 (* x - 1  *)
           untag_y (                                              (* y >> 1 *)
           inst_sar (R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::  (* x >>   *)
           copy(tmp_reg0, rcx,                                    (* restore rcx *)
           I.orq (I "1", R tmp_reg1) ::                           (* 1 |    *)
           copy(tmp_reg1, d_reg, C')))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           inst_sar (R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                 (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun shift_right_unsigned_word_kill_tmp01 {tag,quad} (x,y,d,size_ff,C) =  (*tmp_reg0 = %r10 *)
       let val (x_reg,x_C) = resolve_arg_aty(x,tmp_reg1,size_ff)
           val (y_reg,y_C) = resolve_arg_aty(y,rcx,size_ff)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
           (* y is represented tagged only when BI.tag_values() is true *)
           fun untag_y C = if BI.tag_values() then I.sarq (I "1", R rcx) :: C     (* y >> 1 *)
                           else C
           val (inst_shr, maybeDoubleOfQuadReg) =
               if quad
               then (I.shrq, fn r => r)
               else (I.shrl, I.doubleOfQuadReg)
       in
         if tag then                                           (* 1 | ((unsigned long)(x) >> (y >> 1)) *)
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           untag_y (                                                (* y >> 1                *)
           inst_shr (R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::    (* (unsigned long)x >>   *)
           I.orq (I "1", R tmp_reg1) ::                             (* 1 |                   *)
           copy(tmp_reg0, rcx,
           copy(tmp_reg1, d_reg, C'))))))))
         else
           x_C(
           copy(rcx, tmp_reg0,                                 (* save rcx *)
           copy(x_reg, tmp_reg1,
           y_C(
           copy(y_reg, rcx,
           inst_shr (R cl, R (maybeDoubleOfQuadReg tmp_reg1)) ::
           copy(tmp_reg0, rcx,                                 (* restore rcx *)
           copy(tmp_reg1, d_reg, C')))))))
       end

     fun bytetable_sub (t,i,d,size_ff,C) =
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

     fun resolve_args (atys,ts,size_ff) =
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

     fun bytetable_update (t,i,x,d,size_ff,C) =
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
            move_immed(IntInf.fromInt BI.ml_unit, R d_reg,      (* d = () *)
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

     fun bytetable_size (t,d,size_ff,C) =
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

     fun word_sub0 (t,i,d,size_ff,C) =
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

     fun word_update0 (t,i,x,d,size_ff,C) =
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
                  move_immed(IntInf.fromInt BI.ml_unit, R d_reg,
                  C')))
               | SOME _ => die "word_update0_1"
               | NONE =>
                (move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
                 I.sarq(I "1", R tmp_reg1) ::                     (* untag i: tmp_reg1 >> 1 *)
                 I.salq(I "3", R tmp_reg1) ::                     (* i << 3 *)
                 move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
                 I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
                 move_aty_into_reg(x,tmp_reg0,size_ff,            (* tmp_reg0 = x *)
                 I.movq(R tmp_reg0, D("8", tmp_reg1)) ::          (* *(tmp_reg1+8) = tmp_reg0 *)
                 move_immed(IntInf.fromInt BI.ml_unit, R d_reg,   (* d = () *)
                 C')))))
         end
       else
         (case resolve_args([t,i,x],[tmp_reg0,tmp_reg1], size_ff)
            of SOME ([t_reg,i_reg,x_reg], F) =>
              F(I.movq(R x_reg, DD("8", t_reg, i_reg, "8")) :: C)
             | SOME _ => die "word_update0_2"
             | NONE =>
              move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
              I.salq(I "3", R tmp_reg1) ::                     (* i << 3 *)
              move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
              I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
              move_aty_into_reg(x,tmp_reg0,size_ff,            (* tmp_reg0 = x *)
              I.movq(R tmp_reg0, D("8", tmp_reg1)) ::          (* *(tmp_reg1+8) = tmp_reg0 *)
              C))))

     fun table_size a = bytetable_size a

     (* operations on blockf64 values *)
     fun blockf64_size (t,d,size_ff,C) =
         let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg0,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
         in if BI.tag_values() then
              t_C(
              I.movq(D("0",t_reg), R d_reg) ::
              I.sarq (I "9", R d_reg) ::         (* d >> 6: remove tag (Tagging.h) and divide by 8 *)
              I.leaq(DD("1", d_reg, d_reg, ""), R d_reg) ::
              C')
            else
              t_C(
              I.movq(D("0",t_reg), R d_reg) ::
              I.sarq (I "9", R d_reg) ::         (* d >> 6: remove tag (Tagging.h) and divide by 8 *)
              C')
         end

     fun blockf64_alloc (x,y,d,size_ff,C) = die "blockf64_alloc: not implemented"

     fun blockf64_update_real (t,i,x,d,size_ff,C) =
       if BI.tag_values() then
         let
           (* i, x are represented tagged only when BI.tag_values() is true *)
           val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
         in
            case resolve_args([t,x],[tmp_reg1], size_ff)
              of SOME ([t_reg,x_reg], F) =>
                F(move_aty_into_reg(i,tmp_reg0,size_ff,
                  I.sarq (I "1", R tmp_reg0) ::
                  I.movsd(D("8", x_reg), R tmp_freg0) ::                    (* x points to a real *)
                  I.movsd(R tmp_freg0, DD("8", t_reg, tmp_reg0, "8")) ::
                  move_immed(IntInf.fromInt BI.ml_unit, R d_reg,
                  C')))
               | SOME _ => die "blockf64_update_real_1"
               | NONE =>
                (move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
                 I.sarq(I "1", R tmp_reg1) ::                     (* untag i: tmp_reg1 >> 1 *)
                 I.salq(I "3", R tmp_reg1) ::                     (* i << 3 *)
                 move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
                 I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
                 load_real(x,tmp_reg0,size_ff,tmp_freg0)          (* tmp_freg0 = !x *)
                 (I.movsd(R tmp_freg0, D("8", tmp_reg1)) ::       (* *(tmp_reg1+8) = tmp_freg0 *)
                  move_immed(IntInf.fromInt BI.ml_unit, R d_reg,  (* d = () *)
                  C')))))
         end
       else
         (case resolve_args([t,i,x],[tmp_reg0,tmp_reg1], size_ff)
            of SOME ([t_reg,i_reg,x_reg], F) =>
              F(I.movsd(D("0", x_reg), R tmp_freg0) ::                    (* x points to a real *)
                I.movsd(R tmp_freg0, DD("8", t_reg, i_reg, "8")) :: C)
             | SOME _ => die "blockf64_update_real_2"
             | NONE =>
              move_aty_into_reg(i,tmp_reg1,size_ff,            (* tmp_reg1 = i *)
              I.salq(I "3", R tmp_reg1) ::                     (* i << 3 *)
              move_aty_into_reg(t,tmp_reg0,size_ff,            (* tmp_reg0 = t *)
              I.addq(R tmp_reg0, R tmp_reg1) ::                (* tmp_reg1 += tmp_reg0 *)
              load_real(x,tmp_reg0,size_ff,tmp_freg0)          (* tmp_freg0 = !x *)
              (I.movsd(R tmp_freg0, D("8", tmp_reg1)) ::       (* *(tmp_reg1+8) = tmp_freg0 *)
               C))))

     fun blockf64_sub_real (b,t,i,d,size_ff,C) =
         let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg1,size_ff)
             val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
             val (b_reg, b_C) = resolve_arg_aty(b, tmp_reg0, size_ff)
         in if BI.tag_values() then                          (* i is represented tagged only *)
              t_C(                                           (* when BI.tag_values() is true *)
              move_aty_into_reg(i,tmp_reg0,size_ff,
              I.sarq (I "1", R tmp_reg0) ::         (* i >> 1 *)
              I.movsd(DD("8",t_reg,tmp_reg0,"8"), R tmp_freg0) ::
              b_C(store_real(b_reg,tmp_reg1,tmp_freg0,
              copy(b_reg,d_reg, C')))))
            else
              let val (i_reg,i_C) = resolve_arg_aty(i,tmp_reg0,size_ff)
              in t_C(i_C(
                 I.movsd(DD("8",t_reg,i_reg,"8"), R tmp_freg0) ::
                 b_C(store_real(b_reg,tmp_reg1,tmp_freg0,
                 copy(b_reg,d_reg, C')))))
              end
         end

     fun blockf64_update_f64 (t,i,x,d,size_ff,C) =
         let val (x, x_C) = resolve_arg_aty(x,tmp_freg0,size_ff)
         in x_C(if BI.tag_values() then
                  let (* i is represented tagged only when BI.tag_values() is true *)
                    val (d_reg,C') = resolve_aty_def(d,tmp_reg1,size_ff,C)
                  in move_aty_into_reg(i,tmp_reg1,size_ff,              (* tmp_reg1 = i *)
                     I.sarq(I "1", R tmp_reg1) ::                       (* untag i: tmp_reg1 >> 1 *)
                     move_aty_into_reg(t,tmp_reg0,size_ff,              (* tmp_reg0 = t *)
                     (I.movsd(R x, DD("8",tmp_reg0,tmp_reg1,"8")) ::    (* *(8+tmp_reg1+8*tmp_reg1) = freg *)
                      move_immed(IntInf.fromInt BI.ml_unit, R d_reg,    (* d = () *)
                      C'))))
                  end
                else
                  move_aty_into_reg(i,tmp_reg1,size_ff,                 (* tmp_reg1 = i *)
                  move_aty_into_reg(t,tmp_reg0,size_ff,                 (* tmp_reg0 = t *)
                  (I.movsd(R x, DD("8",tmp_reg0,tmp_reg1,"8")) ::       (* *(8+tmp_reg1+8*tmp_reg1) = freg *)
                   C))))
         end

     fun blockf64_sub_f64 (t,i,d,size_ff,C) =
         let val (t_reg,t_C) = resolve_arg_aty(t,tmp_reg1,size_ff)
             val (d,C') = resolve_aty_def(d,tmp_freg0,size_ff,C)
         in if BI.tag_values() then                          (* i is represented tagged only *)
              t_C(                                           (* when BI.tag_values() is true *)
              move_aty_into_reg(i,tmp_reg0,size_ff,
              I.sarq (I "1", R tmp_reg0) ::                  (* i >> 1 *)
              I.movsd(DD("8",t_reg,tmp_reg0,"8"), R d) ::
              C'))
            else
              let val (i_reg,i_C) = resolve_arg_aty(i,tmp_reg0,size_ff)
              in t_C(i_C(
                 I.movsd(DD("8",t_reg,i_reg,"8"), R d) ::
                 C'))
              end
         end


end
