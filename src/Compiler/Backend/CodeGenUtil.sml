
signature INSTS_COMMON = sig

  include INSTS_BASE

  type lvar = Lvars.lvar
  eqtype reg
  val tmp_reg0         : reg
  val tmp_reg1         : reg
  val tmp_freg0        : reg
  val tmp_freg1        : reg
  val sp_reg           : reg

  datatype ea = R of reg                          (* register *)
              | L of lab                          (* label *)
              | LA of lab                         (* label address *)
              | I of string                       (* immediate *)
              | D of string * reg                 (* displaced *)
              | DD of string * reg * reg * string (* double displaced *)
  val pr_ea : ea -> string
  val eq_ea : ea * ea -> bool

  type inst
  type code = inst list
  datatype top_decl = FUN of label * code
                    | FN of label * code

  type AsmPrg = {top_decls: top_decl list,
                 init_code: code,
                 static_data: code}

  structure RI : REGISTER_INFO
      where type reg = reg
      where type lvar = lvar

  val copy             : reg * reg * code -> code
  val load_indexed     : ea * reg * Offset * code -> code
  val store_indexed    : reg * Offset * ea * code -> code
  val base_plus_offset : reg * Offset * reg * code -> code
  val store_immed      : word * reg * Offset * code -> code
  val move_immed       : IntInf.int * ea * code -> code
  val move_num         : string * ea * code -> code
  val move_num_boxed   : (unit -> lab) -> (code -> unit) -> (unit -> string) -> string * ea * code -> code
  val move_ea_to_reg   : ea * reg * code -> code
  val move_reg_to_ea   : reg * ea * code -> code
  val comment_str      : string * code -> code

  val rem_dead_code    : code -> code
end

functor CodeGenUtil(structure Insts : INSTS_COMMON
                    structure BackendInfo : BACKEND_INFO
                        where type label = AddressLabels.label
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
                        where type reg = Insts.reg
                        where type label = AddressLabels.label
                   ) =
struct

  structure I = Insts
  structure PP = PrettyPrint
  structure Labels = AddressLabels
  structure RI = I.RI
  structure BI = BackendInfo
  structure SS = SubstAndSimplify
  structure LS = LineStmt
  type label = Labels.label
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
  type StoreTypeCO = SubstAndSimplify.StoreTypeCO
  type AtySS = SubstAndSimplify.Aty
  type AsmPrg = I.AsmPrg

  type reg = I.reg
  datatype ea = datatype I.ea
  datatype lab = datatype I.lab
  datatype Offset = datatype I.Offset
  type offset = int

  (* ------------------------
   * Some utilities
   * ------------------------ *)

  fun member r nil = false
    | member r (x::xs) = r = x orelse member r xs

  (* --------------------
   * Logging
   * -------------------- *)

  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun msg s = TextIO.output(TextIO.stdOut, s)
  fun chat (s: string) = if !Flags.chat then msg (s) else ()
  fun die s  = Crash.impossible ("CodeGenUtil." ^ s)
  fun warn s = print ("** WARNING: " ^ s ^ "\n")
  fun fast_pr st =
      (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , st, !Flags.colwidth);
       TextIO.output(!Flags.log, "\n"))
  fun display (title, tree) =
      fast_pr(PP.NODE{start=title ^ ": ", finish="", indent=3,
                      children=[tree], childsep=PP.NOSEP})

  (* -----------------------
   * Dynamic Flags
   * ----------------------- *)

  val region_profiling : unit -> bool = Flags.is_on0 "region_profiling"
  fun parallelism_p () : bool = Flags.is_on "parallelism"
  fun par_alloc_unprotected_p () : bool = Flags.is_on "parallelism_alloc_unprotected"

  val _ = Flags.add_bool_entry {long="comments_in_asmcode", short=NONE, item=ref false,
                                menu=["Debug", "comments in assembler code"], neg=false,
                                desc="Insert comments in assembler code."}
  val comments_in_asmcode = Flags.lookup_flag_entry "comments_in_asmcode"

  val jump_tables = true
  val gc_p = Flags.is_on0 "garbage_collection"
  val tag_pairs_p = Flags.is_on0 "tag_pairs"

  (* Simple memory profiling - remember to enable the flag
   * SIMPLE_MEMPROF in Runtime/Flags.h when you change this flag. *)
  fun simple_memprof_p () = false
  val stack_min = NameLab "stack_min"

  (* ----------------------------------
   * Some code generation utilities
   * ---------------------------------- *)

  fun comment (str,C) = if !comments_in_asmcode then I.comment_str (str, C)
                        else C
  fun comment_fn (f, C) = if !comments_in_asmcode then I.comment_str (f(), C)
                          else C

  (* ----------------------------------
   * CG on Top Level Declarations
   * ---------------------------------- *)

  (* Global Labels *)
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

  (* Labels local to a compilation unit *)
  fun new_local_lab name = LocalLab (Labels.new_named name)
  local val counter = ref 0
        fun incr () = (counter := !counter + 1; !counter)
  in fun new_dynamicFn_lab () : lab = DatLab(Labels.new_named ("DynLab" ^ Int.toString(incr())))
     fun new_string_lab () : lab = DatLab(Labels.new_named ("StringLab" ^ Int.toString(incr())))
     fun new_float_lab () : lab = DatLab(Labels.new_named ("FloatLab" ^ Int.toString(incr())))
     fun new_num_lab () : lab = DatLab(Labels.new_named ("BoxedNumLab" ^ Int.toString(incr())))
     fun reset_label_counter () = counter := 0
  end

  (* Static Data inserted at the beginning of the code. *)
  local val static_data : I.inst list ref = ref []
  in fun add_static_data insts = (static_data := insts @ !static_data)
     fun reset_static_data () = static_data := []
     fun get_static_data C = !static_data @ C
  end

  (* Additional code blocks added at the end of functions (for avoiding jumping over blocks) *)
  local val code_blocks : I.inst list ref = ref []
  in fun add_code_block insts = (code_blocks := insts @ !code_blocks)
     fun reset_code_blocks () = code_blocks := []
     fun get_code_blocks () = !code_blocks
  end

  val tmp_reg0 = I.tmp_reg0
  val tmp_reg1 = I.tmp_reg1
  val tmp_freg0 = I.tmp_freg0
  val tmp_freg1 = I.tmp_freg1

  val copy = I.copy
  val store_indexed = I.store_indexed
  val load_indexed = I.load_indexed
  val base_plus_offset = I.base_plus_offset
  val store_immed = I.store_immed
  val move_immed = I.move_immed
  val move_num = I.move_num
  val move_ea_to_reg = I.move_ea_to_reg
  val move_reg_to_ea = I.move_reg_to_ea

  fun mkIntAty i = SS.INTEGER_ATY {value=IntInf.fromInt i,
                                   precision=if BI.tag_values() then 63 else 64}

  fun maybeTagIntOrWord {value: IntInf.int, precision:int} : IntInf.int =
      if precision = 31 orelse precision = 63 orelse (precision = 8 andalso BI.tag_values())
      then 2 * value + 1                (* use tagged-unboxed representation *)
      else if precision = 32 orelse precision = 64 orelse (precision = 8 andalso not(BI.tag_values()))
      then value                        (* use untagged representation - maybe boxed *)
      else die "maybeTagIntOrWord"

  (* formatting of immediate integer and word values *)
  fun fmtInt a : string = I.intToStr(maybeTagIntOrWord a)
  fun fmtWord a : string = I.wordToStr(maybeTagIntOrWord a)

  (* returns true if boxed representation is used for
   * integers of the given precision *)
  fun boxedNum (precision:int) : bool =
      (precision = 32 orelse precision = 64) andalso BI.tag_values()

  (* Find a register for aty and generate code to store into the aty *)
  fun resolve_aty_def (SS.STACK_ATY offset,t:reg,size_ff,C) =
      (t,store_indexed(I.sp_reg,WORDS(size_ff-offset-1),R t,C))
    | resolve_aty_def (SS.PHREG_ATY phreg,t:reg,size_ff,C) = (phreg,C)
    | resolve_aty_def (SS.UNIT_ATY,t:reg,size_ff,C)  = (t,C)
    | resolve_aty_def _ = die "resolve_aty_def: ATY cannot be defined"

  fun move_unit (ea,C) =
      if BI.tag_values() then
        move_immed(IntInf.fromInt BI.ml_unit,ea,C) (* gc needs value! *)
      else C

  fun move_num_boxed (x,ea,C) =
      if not(BI.tag_values()) then
        die "move_num_boxed.boxed integers/words necessary only when tagging is enabled"
      else
        I.move_num_boxed new_num_lab add_static_data
                         (fn () => BI.pr_tag_w(BI.tag_word_boxed true))
                         (x,ea,C)

  fun move_num_generic (precision, num, ea, C) =
      if boxedNum precision then move_num_boxed(num, ea, C)
      else move_num(num, ea, C)

  (* Make sure that the aty ends up in register dst_reg *)
  fun move_aty_into_reg (aty,dst_reg,size_ff,C) =
      case aty of
          SS.REG_I_ATY offset =>
          base_plus_offset(I.sp_reg,BYTES(size_ff*8-offset*8-8+BI.inf_bit),dst_reg,C)
        | SS.REG_F_ATY offset =>
          base_plus_offset(I.sp_reg,WORDS(size_ff-offset-1),dst_reg,C)
        | SS.STACK_ATY offset =>
          load_indexed(R dst_reg,I.sp_reg,WORDS(size_ff-offset-1),C)
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
        | SS.STACK_ATY offset => store_indexed(I.sp_reg,WORDS(size_ff-offset-1),R src_reg,C)
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
      load_indexed (R dst_reg,src_reg,offset,C)
    | move_index_aty_to_aty (SS.PHREG_ATY src_reg,dst_aty,offset:Offset,t:reg,size_ff,C) =
      load_indexed (R t,src_reg,offset,
       move_reg_into_aty(t,dst_aty,size_ff,C))
    | move_index_aty_to_aty (src_aty,SS.PHREG_ATY dst_reg,offset,t:reg,size_ff,C) =
      move_aty_into_reg(src_aty,t(*dst_reg*),size_ff,
       load_indexed (R dst_reg,t(*dst_reg*),offset,C))
    | move_index_aty_to_aty (src_aty,dst_aty,offset,t:reg,size_ff,C) = (* can be optimised!! *)
      move_aty_into_reg(src_aty,t,size_ff,
       load_indexed (R t,t,offset,
        move_reg_into_aty(t,dst_aty,size_ff,C)))

  (* dst_aty = &lab *)
  fun load_label_addr (lab,dst_aty,t:reg,size_ff,C) =
      case dst_aty of
          SS.PHREG_ATY d => I.move_ea_to_reg(LA lab, d, C)
        | SS.STACK_ATY offset =>
          I.move_ea_to_reg(LA lab, t,
           store_indexed(I.sp_reg, WORDS(size_ff-offset-1), R t, C))
        | _ => die "load_label_addr.wrong ATY"

  (* dst_aty = lab[0] *)
  fun load_from_label (lab,dst_aty,t:reg,size_ff,C) =
      case dst_aty of
          SS.PHREG_ATY d => I.move_ea_to_reg(L lab, d, C)
        | SS.STACK_ATY offset =>
          I.move_ea_to_reg(L lab, t,
           store_indexed(I.sp_reg, WORDS(size_ff-offset-1), R t, C))
        | SS.UNIT_ATY => C
        | _ => die "load_from_label.wrong ATY"

  (* lab[0] = src_aty *)
  fun store_in_label (src_aty,lab,tmp1:reg,size_ff,C) =
      case src_aty of
          SS.PHREG_ATY s =>
          move_ea_to_reg (LA lab, tmp1,
           move_reg_to_ea (s, D("0",tmp1), C))
        | SS.INTEGER_ATY i =>
          move_ea_to_reg(LA lab, tmp1,
           move_num_generic (#precision i, fmtInt i, D("0",tmp1), C))
        | SS.WORD_ATY w =>
          move_ea_to_reg(LA lab,tmp1,
           move_num_generic (#precision w, fmtWord w, D("0",tmp1), C))
        | SS.UNIT_ATY =>
          move_ea_to_reg(LA lab,tmp1,
           move_unit(D("0",tmp1), C))
        | _ => move_aty_into_reg(src_aty,tmp1,size_ff,
                move_reg_to_ea(tmp1, L lab,C))

  fun store_aty_indexed (b:reg,n:Offset,aty,t:reg,size_ff,C) =
      let fun ea () = D(I.offset_bytes n,b)
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
                | 8 => true
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
            SS.PHREG_ATY s => move_reg_to_ea(s,ea(),C)
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
      load_indexed (R dst_reg,base_reg,offset,C)
    | load_aty_from_reg_record (dst_aty,t:reg,base_reg,offset:Offset,size_ff,C) =
      load_indexed (R t,base_reg,offset,
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

  (* Returns a register with arg and a continuation function. *)
  fun resolve_arg_aty (arg:SS.Aty,t:reg,size_ff:int) : reg * (I.inst list -> I.inst list) =
      case arg of
          SS.PHREG_ATY r => (r, fn C => C)
        | _ => (t, fn C => move_aty_into_reg(arg,t,size_ff,C))

  fun Id x = x
  fun rep8bit (i: IntInf.int) = ~0x80 <= i andalso i <= 0x7F
  fun rep16bit (i: IntInf.int) = ~0x8000 <= i andalso i <= 0x7FFF

  fun protect_arg_aty (arg:SS.Aty,t:reg,size_ff:int,{avoid:SS.Aty}) : ea * (I.inst list -> I.inst list) =
      let fun default () = (R t, fn C => move_aty_into_reg(arg,t,size_ff,C))
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
      in fn C => case float_aty of
                     SS.PHREG_ATY x => move_ea_to_reg(D(disp, x),freg,C)
                   | _ => move_aty_into_reg(float_aty,t,size_ff,
                           move_ea_to_reg(D(disp, t),freg,C))
      end

  (* store xmm register value (freg) in real value (maybe incl. tag) *)
  fun store_real (base_reg,t:reg,freg,C) =
      if BI.tag_values() then
        store_immed(BI.tag_real false, base_reg, WORDS 0,
         move_reg_to_ea (freg,D("8",base_reg),C))
      else move_reg_to_ea (freg,D("0",base_reg),C)

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
     region descriptor.
   *)

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

  (* Utilities for calling C functions *)
  fun regs_atys nil acc = nil
    | regs_atys (SS.PHREG_ATY r::atys) acc = regs_atys atys (r::acc)
    | regs_atys (_ ::atys) acc = regs_atys atys acc

  fun push_args push_arg size_ff args C =
      let fun loop ([], _) = C
            | loop (arg :: rest, size_ff) = (push_arg(arg,size_ff,
                                                      loop (rest, size_ff + 1)))
      in loop(rev args, size_ff)
      end

  (* move a number of arguments into the appropriate registers *)
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

  local

    fun basic_sw basic_lss (LS.SWITCH(_,xlsss,lss)) =
        basic_lss lss andalso
        List.all (fn (_,lss) => basic_lss lss) xlsss

    fun basic_regions nil = true
      | basic_regions _ = false

    fun nonbasic_prim p : bool =
        let open PrimName
        in case p of
               Plus_int31 => true | Plus_int32ub => true | Plus_int32b => true
             | Plus_word31 => false | Plus_word32ub => false | Plus_word32b => false
             | Plus_int63 => true| Plus_int64ub => true | Plus_int64b => true
             | Plus_word63 => false | Plus_word64ub => false| Plus_word64b => false
             | Plus_real => false | Plus_f64 => false
             | Minus_int31 => true | Minus_int32ub => true | Minus_int32b => true
             | Minus_word31 => false | Minus_word32ub => false | Minus_word32b => false
             | Minus_int63 => true| Minus_int64ub => true | Minus_int64b => true
             | Minus_word63 => false | Minus_word64ub => false | Minus_word64b => false
             | Minus_real => false | Minus_f64 => false
             | Mul_int31 => true | Mul_int32ub => true | Mul_int32b => true
             | Mul_word31 => false | Mul_word32ub => false | Mul_word32b => false
             | Mul_int63  => true| Mul_int64ub => true | Mul_int64b => true
             | Mul_word63 => false | Mul_word64ub => false | Mul_word64b => false
             | Mul_real => false | Mul_f64 => false
             | Div_real => false | Div_f64 => false
             | Neg_int31 => true | Neg_int32ub => true | Neg_int32b => true
             | Neg_int63 => true | Neg_int64ub => true | Neg_int64b => true
             | Neg_real => false | Neg_f64 => false
             | Abs_int31 => true | Abs_int32ub => true | Abs_int32b => true
             | Abs_int63 => true | Abs_int64ub => true | Abs_int64b => true
             | Abs_real => false | Abs_f64 => false
             | Andb_word31 => false | Andb_word32ub => false | Andb_word32b => false
             | Andb_word63 => false | Andb_word64ub => false | Andb_word64b => false
             | Orb_word31 => false | Orb_word32ub => false | Orb_word32b => false
             | Orb_word63 => false | Orb_word64ub => false | Orb_word64b => false
             | Xorb_word31 => false | Xorb_word32ub => false | Xorb_word32b => false
             | Xorb_word63 => false | Xorb_word64ub => false | Xorb_word64b => false
             | Shift_left_word31 => false | Shift_left_word32ub => false | Shift_left_word32b => false
             | Shift_left_word63 => false | Shift_left_word64ub => false | Shift_left_word64b => false
             | Shift_right_signed_word31 => false | Shift_right_signed_word32ub => false
             | Shift_right_signed_word32b => false
             | Shift_right_signed_word63 => false | Shift_right_signed_word64ub => false
             | Shift_right_signed_word64b => false
             | Shift_right_unsigned_word31 => false | Shift_right_unsigned_word32ub => false
             | Shift_right_unsigned_word32b => false
             | Shift_right_unsigned_word63 => false | Shift_right_unsigned_word64ub => false
             | Shift_right_unsigned_word64b => false
             | Int31_to_int32b => false | Int31_to_int32ub => false | Int32b_to_int31 => true
             | Int32b_to_word32b => false | Int32ub_to_int31 => true
             | Int31_to_int64b => false | Int31_to_int64ub => false | Int64b_to_int31 => true
             | Word31_to_word32b => false | Word31_to_word32ub => false | Word32b_to_word31 => false
             | Word32ub_to_word31 => false
             | Word31_to_word32ub_X => false | Word31_to_word32b_X => false
             | Word32b_to_int32b => true | Word32b_to_int32b_X => false | Word32ub_to_int32ub => true
             | Word31_to_int31 => true
             | Word32b_to_int31 => true | Int32b_to_word31 => true | Word32b_to_int31_X => true
             | Word64ub_to_int32ub => true
             | Word32ub_to_word64ub => false | Word64ub_to_word32ub => false | Word64ub_to_int64ub => true
             | Word64ub_to_int64ub_X => false
             | Word31_to_word64b => false | Word31_to_word64b_X => false | Word64b_to_int31 => true
             | Word64b_to_int64b_X => true | Word64b_to_int64b => true | Word32b_to_word64b => false
             | Word32b_to_word64b_X => false | Word64b_to_word32b => false | Word64b_to_int31_X => true
             | Int32b_to_int64b => false | Int32ub_to_int64ub => false | Int64b_to_word64b => false
             | Int64ub_to_word64ub => false | Int64ub_to_int32ub => true
             | Int63_to_int64b => false | Int64b_to_int63 => true | Word32b_to_word63 => false
             | Word63_to_word32b => false
             | Word63_to_word31 => false | Word31_to_word63 => false | Word31_to_word63_X => false
             | Word63_to_word64b => false
             | Word63_to_word64b_X => false | Word64b_to_word63 => false
             | Int31_to_int63 => false | Int63_to_int31 => true | Int32b_to_int63 => false
             | Int63_to_int32b => true
             | Word32b_to_int63 => false | Word32b_to_int63_X => false | Word64b_to_word31 => false
             | Word64b_to_int63 => true | Word64b_to_int63_X => true | Int63_to_int64ub => false
             | Int64ub_to_int63 => true | Word63_to_word64ub => false | Word63_to_word64ub_X => false
             | Word64ub_to_word31 => false | Int64ub_to_int31 => true | Word31_to_word64ub => false
             | Word31_to_word64ub_X => false | Word32ub_to_int64ub => false
             | Word32ub_to_int64ub_X => false | Word32ub_to_word64ub_X => false
             | Exn_ptr => false | Fresh_exname => false | Get_ctx => false
             | Bytetable_sub => false | Bytetable_size => false | Bytetable_update => false
             | Bytetable_sub_word16 => false | Bytetable_update_word16 => false
             | Bytetable_sub_word31 => false | Bytetable_update_word31 => false
             | Bytetable_sub_word32ub => false | Bytetable_update_word32ub => false
             | Bytetable_sub_word32b => false | Bytetable_update_word32b => false
             | Bytetable_sub_word63 => false | Bytetable_update_word63 => false
             | Bytetable_sub_word64ub => false | Bytetable_update_word64ub => false
             | Bytetable_sub_word64b => false | Bytetable_update_word64b => false
             | Word_sub0 => false | Word_update0 => false | Table_size => false | Is_null => false
             | ServerGetCtx => true
             | Max_f64 => false | Min_f64 => false | Real_to_f64 => false | F64_to_real => false
             | Sqrt_f64 => false
             | Int_to_f64 => false
             | Blockf64_update_real => false | Blockf64_sub_real => false | Blockf64_size => false
             | Blockf64_alloc => true
             | Blockf64_update_f64 => false | Blockf64_sub_f64 => false
             | _ => true
        end

    fun basic_prim p : bool =
        PrimName.is_flow_prim p orelse
        not(nonbasic_prim p)

    fun basic_assign {bind,pat} : bool =
        case bind of
            LS.ATOM _ => true
          | LS.LOAD _ => true
          | LS.STORE _ => true
          | LS.STRING _ => true
          | LS.REAL _ => true
          | LS.F64 _ => true
          | LS.CLOS_RECORD _ => false
          | LS.SCLOS_RECORD _ => false
          | LS.RECORD {elems=[],...} => true
          | LS.RECORD _ => false
          | LS.BLOCKF64 {elems=[],...} => true
          | LS.BLOCKF64 _ => false
          | LS.SCRATCHMEM{bytes=0,alloc,tag} => true
          | LS.SCRATCHMEM _ => false
          | LS.SELECT _ => true
          | LS.CON0 {con_kind=LS.ENUM _,...} => true
          | LS.CON0 _ => false
          | LS.CON1 {con_kind=LS.UNBOXED _,...} => true
          | LS.CON1 _ => false
          | LS.DECON _ => true
          | LS.REF _ => false
          | LS.DEREF _ => true
          | LS.ASSIGNREF _ => true
          | LS.PASS_PTR_TO_MEM _ => false
          | LS.PASS_PTR_TO_RHO _ => true

    fun basic_lss lss : bool =
        case lss of
            nil => true
          | ls::lss => basic_ls ls andalso basic_lss lss

    and basic_ls ls : bool =
        case ls of
            LS.ASSIGN a => basic_assign a
          | LS.FLUSH(atom,_) => false
          | LS.FETCH(atom,_) => false
          | LS.FNJMP a => false
          | LS.FNCALL a => false
          | LS.JMP a => true
          | LS.FUNCALL a => false
          | LS.LETREGION{rhos,body} => basic_regions rhos andalso basic_lss body
          | LS.SCOPE{pat,scope} => basic_lss scope
          | LS.HANDLE _ => false
          | LS.RAISE a => false
          | LS.SWITCH_I {switch,precision} => basic_sw basic_lss switch
          | LS.SWITCH_W {switch,precision} => basic_sw basic_lss switch
          | LS.SWITCH_S sw => basic_sw basic_lss sw
          | LS.SWITCH_C sw => basic_sw basic_lss sw
          | LS.SWITCH_E sw => basic_sw basic_lss sw
          | LS.RESET_REGIONS a => false
          | LS.PRIM a => basic_prim (#name a)
          | LS.CCALL a => false
          | LS.CCALL_AUTO a => false
          | LS.EXPORT a => false
  in
    val basic_lss = basic_lss
  end


end
