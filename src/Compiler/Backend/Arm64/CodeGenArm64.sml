(* Generate Target Code *)

functor CodeGenArm64(structure LineStmt: LINE_STMT
                     where type con = Con.con
                     where type excon = Excon.excon
                     where type lvar = Lvars.lvar
                     where type label = AddressLabels.label
                     where type place = Effect.effect
                     where type StringTree = PrettyPrint.StringTree
                     where type cc = CallConv.cc
                   structure SubstAndSimplify: SUBST_AND_SIMPLIFY
                    where type ('a,'b,'c) LinePrg = ('a,'b,'c) LineStmt.LinePrg
                     where type lvar = Lvars.lvar
                     where type place = Effect.effect
                     where type reg = InstsArm64.reg
                     where type label = AddressLabels.label)
    : CODE_GEN =
struct
  structure LS = LineStmt
  structure SS = SubstAndSimplify
  structure A = InstsArm64
  open CodeGenUtilArm64
  type label = AddressLabels.label
  type ('s,'o,'a) LinePrg = ('s,'o,'a) LS.LinePrg
  type offset = int
  type StoreTypeCO = SS.StoreTypeCO
  type AtySS = SS.Aty
  type AsmPrg = A.AsmPrg
  val emit = A.emit
  fun message f = print(f())
  val extra_gc_checks = Flags.add_bool_entry
    {long="extra_gc_checks",short=NONE,item=ref false,neg=false,
     menu=["Compiler","extra GC checks"],desc="Collect at every ARM64 function entry."}
  val alloc_protect_always = Flags.add_bool_entry
    {long="alloc_protect_always",short=NONE,item=ref false,neg=false,
     menu=["Compiler","always protect allocation"],desc="Always protect parallel allocation."}
  fun parallel () = Flags.is_on "parallelism"
  fun unprotected () = Flags.is_on "parallelism_alloc_unprotected"
  val gc = Flags.is_on0 "garbage_collection"
  val tagged = BackendInfo.tag_values
  val profiling = Flags.is_on0 "region_profiling"
  val tagPairs = Flags.is_on0 "tag_pairs"
  fun payload () = if tagged() then 8 else 0
  fun slot fsz off = 8*(fsz-off-1)
  val staticData : A.inst list ref = ref []
  fun number {value,precision} dst =
    if tagged() andalso (precision=32 orelse precision=64) then
      let val l=DatLab(AddressLabels.new_named "arm64_integer")
          val ()=staticData := !staticData @ [Directive ".data",Directive ".p2align 3",Label l,
            Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_word_boxed true)),
            Directive(".quad 0x" ^ IntInf.fmt StringCvt.HEX (IntInf.mod(value,18446744073709551616)))]
      in address(l,dst) end
    else constant(if precision=63 orelse precision=31 orelse (precision=8 andalso tagged()) then 2*value+1 else value,dst)
  fun read fsz aty dst =
    case aty of
      SS.PHREG_ATY src => move(src,dst)
    | SS.STACK_ATY off => load(SP,slot fsz off,dst)
    | SS.INTEGER_ATY n => number n dst
    | SS.WORD_ATY n => number n dst
    | SS.REG_I_ATY off => addOffset(SP,slot fsz off,dst) @ [ins "orr" [r dst,r dst,"#1"]]
    | SS.REG_F_ATY off => addOffset(SP,slot fsz off,dst)
    | SS.DROPPED_RVAR_ATY => constant(0,dst)
    | SS.UNIT_ATY => constant(1,dst)
    | _ => unsupported ("operand " ^ SS.pr_aty aty)
  fun write fsz dst src =
    case dst of SS.PHREG_ATY d => move(src,d)
              | SS.STACK_ATY off => store(src,SP,slot fsz off)
              | SS.UNIT_ATY => []
              | _ => unsupported ("result " ^ SS.pr_aty dst)
  fun localFresh () = A.LocalLab(AddressLabels.new_named "arm64")
  (* Stage all arguments before loading their target registers. This handles
   * cycles without destroying input registers and keeps SP aligned. *)
  fun arguments fsz args =
    let val n = length args
        val bytes = 16*((n+1) div 2)
        val () = if n <= 8 then () else unsupported "stack-passed call arguments"
        val save = List.concat(mapi (fn (i,a) =>
          read (fsz+bytes div 8) a (X 16) @ store(X 16,SP,8*i)) args)
        val restore = List.concat(List.tabulate(n,fn i=>load(SP,8*i,X i)))
    in stack(true,bytes) @ save @ restore @ stack(false,bytes) end
  fun even n = n + n mod 2
  fun first n xs = List.take(xs,Int.min(n,length xs))
  fun rest n xs = List.drop(xs,Int.min(n,length xs))
  val currentArgs = ref 0
  val currentResults = ref 0
  (* The callee releases its locals, argument area and header. The caller
   * receives a separately aligned result area and then releases that area. *)
  fun results fsz res =
    let val spilled = Int.max(0,length res-3)
        val rw = even spilled
        val temp = even(length res)
        val saves = List.concat(mapi (fn (i,_) =>
          if i<3 then store(X i,SP,8*i)
          else load(SP,8*(temp+spilled-1-(i-3)),X 16) @ store(X 16,SP,8*i)) res)
        val writes = List.concat(mapi (fn (i,a)=>load(SP,8*i,X 16) @ write (fsz+rw+temp) a (X 16)) res)
    in stack(true,8*temp) @ saves @ writes @ stack(false,8*(temp+rw)) end
  val frameIndex : (A.lab*A.lab) list ref = ref []
  fun continuation bv =
    let val pc=localFresh() val anchor=DatLab(AddressLabels.new_named "arm64_frame")
        val ()=if gc() then
          (staticData := !staticData @ [Directive ".data",Directive ".p2align 3"] @
            map(fn w=>Directive(".quad 0x" ^ Word32.fmt StringCvt.HEX w)) (rev bv) @ [Label anchor];
           frameIndex := (pc,anchor):: !frameIndex) else ()
    in pc end
  fun unitSymbol lab suffix = NameLab(AddressLabels.pr_label lab ^ "_arm64_" ^ suffix)
  datatype target = Direct of label | Indirect of SS.Aty
  fun mlcall tail fsz target {args,reg_args,fargs,clos,res,bv} =
    let val returnLabel=if tail then localFresh() else continuation bv
        val gp = (case clos of NONE=>[] | SOME a=>[a]) @ args @ reg_args
        val fp = fargs
        val sa = rest 8 gp @ rest 8 fp
        val ac = length sa
        val aw = even ac
        val rc = Int.max(0,length res-3)
        val rw = even rc
        val staged = gp @ fp @ (case target of Direct _=>[] | Indirect a=>[a])
        val sw = even(length staged)
        val newCall = aw+2+rw
        val oldArg = even(!currentArgs)+2
        val workspace = if tail then sw+aw+2 else sw+newCall
        val dest = if tail then workspace+fsz+oldArg-(aw+2) else sw
        val () = if tail andalso rc <> !currentResults then
                   unsupported "tail call with incompatible result area" else ()
        fun save (i,a) = read (fsz+workspace) a (X 16) @ store(X 16,SP,8*i)
        val stackArgs = mapi (fn (i,_) => i+8) (rest 8 gp) @
                        mapi (fn (i,_) => length gp+8+i) (rest 8 fp)
        val spill = List.concat(mapi(fn (i,source)=>
          load(SP,8*source,X 16) @ store(X 16,SP,8*(dest+ac-1-i))) stackArgs)
        val registers = List.concat(mapi(fn (i,_)=>load(SP,8*i,X i)) (first 8 gp)) @
                        List.concat(mapi(fn (i,_)=>load(SP,8*(length gp+i),D i)) (first 8 fp))
        val destination = case target of Direct l => []
          | Indirect _ => load(SP,8*(length staged-1),X 17) @ load(X 17,payload(),X 17)
        val restore = if tail then
            load(SP,8*(workspace+fsz+even(!currentArgs)),X 29) @
            load(SP,8*(workspace+fsz+even(!currentArgs)+1),X 30) else []
        val transfer = case target of Direct l => [ins (if tail then "b" else "bl") [pr_lab(MLFunLab l)]]
                         | Indirect _ => [ins (if tail then "br" else "blr") ["x17"]]
    in stack(true,8*workspace) @ List.concat(mapi save staged) @ spill @ restore @ registers @ destination @
       stack(false,8*(if tail then dest else sw)) @ transfer @
       (if tail then [] else [Label returnLabel] @ results fsz res)
    end
  (* Precision zero is private to raw runtime-helper arguments. *)
  fun integer n = SS.INTEGER_ATY{value=IntInf.fromInt n,precision=0}
  (* Allocation helpers are invisible to register allocation. Preserve every
   * allocatable register, including the full 64 bits of floating registers. *)
  val savedRegs = List.tabulate(16,X) @ List.tabulate(8,fn i=>X(i+19)) @ List.tabulate(30,D)
  fun internalCall fsz name args =
    let val words = length savedRegs
    in stack(true,8*words) @
       List.concat(mapi(fn(i,a)=>store(a,SP,8*i)) savedRegs) @
       arguments (fsz+words) args @ [ins "bl" [pr_lab(NameLab name)]] @ move(X 0,X 16) @
       List.concat(mapi(fn(i,a)=>load(SP,8*i,a)) savedRegs) @ stack(false,8*words)
    end
  (* Foreign calls may re-enter ML through an exported hook. The existing IR
   * carries no root map at a C call, so defer collection across that dynamic
   * extent, including callbacks. Pending collection is retained on return. *)
  fun foreignCall fsz name args convert =
    if name=":" then unsupported "dynamic foreign symbol resolution" else
      scalarCall{name=name,fixed=map(fn _=>AbiArm64.I64) args,variadic=[],protectGC=gc(),
        loadArgument=fn(i,extra)=>read (fsz+extra) (List.nth(args,i)) (X 16) @ convert(i,X 16)}
  fun autoCall fsz {name,args:(SS.Aty*LS.foreign_type) list,rhos_for_result,res=(dst,ft)} =
    let fun convert (i,r) = case #2(List.nth(args,i)) of
          LS.Bool=>[ins "lsr" [pr_reg r,pr_reg r,"#1"]]
        | LS.Int=>if tagged() then [ins "asr" [pr_reg r,pr_reg r,"#1"]] else []
        | LS.Int32=>if tagged() then load(r,8,r) else []
        | LS.Int64=>if tagged() then load(r,8,r) else []
        | LS.ForeignPtr=>if tagged() then [ins "sub" [pr_reg r,pr_reg r,"#1"]] else []
        | LS.CharArray=>[ins "add" [pr_reg r,pr_reg r,"#8"]]
        | LS.Unit=>unsupported "unit foreign argument"
        val boxed=tagged() andalso (ft=LS.Int32 orelse ft=LS.Int64)
        val save=if boxed then case rhos_for_result of [a]=>
          stack(true,16) @ read (fsz+2) a (X 16) @ store(X 16,SP,0)
          | _=>unsupported "boxed foreign result without storage" else []
        fun tag () = [ins "lsl" ["x0","x0","#1"],ins "add" ["x0","x0","#1"]]
        val result=case ft of LS.Unit=>constant(1,X 0)
          | LS.Bool=>[ins "cmp" ["x0","#0"],ins "cset" ["x0","ne"]] @ tag()
          | LS.Int=>if tagged() then tag() else []
          | LS.ForeignPtr=>if tagged() then [ins "add" ["x0","x0","#1"]] else []
          | LS.CharArray=>unsupported "foreign char-array result"
          | _=>if boxed then load(SP,0,X 16) @ store(X 0,X 16,8) @
              constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_word_boxed false)),X 17) @
              store(X 17,X 16,0) @ move(X 16,X 0) @ stack(false,16) else []
    in save @ foreignCall (fsz+(if boxed then 2 else 0)) name (map #1 args) convert @ result @ write fsz dst (X 0) end
  val cSaved = List.tabulate(12,fn i=>X(i+19)) @ List.tabulate(8,fn i=>D(i+8))
  fun saveC () = stack(true,160) @ List.concat(mapi(fn(i,r)=>store(r,SP,8*i)) cSaved) @ addOffset(SP,80,X 29)
  fun restoreC () = List.concat(mapi(fn(i,r)=>load(SP,8*i,r)) cSaved) @ stack(false,160)
  fun deferGC () = if not(gc()) then [] else stack(true,16) @
    address(NameLab "disable_gc",X 16) @ load(X 16,0,X 17) @ store(X 17,SP,0) @
    constant(1,X 17) @ store(X 17,X 16,0)
  fun resumeGC () = if not(gc()) then [] else load(SP,0,X 17) @
    address(NameLab "disable_gc",X 16) @ store(X 17,X 16,0) @ stack(false,16)
  fun registerUnit l = address(unitSymbol l "frames",X 0) @ load(X 0,0,X 1) @ addOffset(X 0,8,X 0) @
    address(unitSymbol l "begin",X 2) @ address(unitSymbol l "end",X 3) @
    address(unitSymbol l "roots",X 4) @ load(X 4,0,X 5) @ addOffset(X 4,8,X 4) @
    [ins "bl" ["_mlkit_arm64_register_image"]]
  fun static words =
    let val l = DatLab(AddressLabels.new_named "arm64_data")
    in staticData := !staticData @ [Directive ".data",Directive ".p2align 3",Label l] @ words; l end
  fun realData value = static ((if tagged() then [Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_real true))] else []) @ [Directive(".double " ^ String.translate(fn #"~"=>"-" | c=>String.str c) value)])
  fun stringData str = static
    [Directive(".quad " ^ ("0x" ^ Word.toString(BackendInfo.tag_string(true,size str)))),
     Directive(".byte " ^ String.concatWith "," (map (Int.toString o Char.ord) (String.explode str) @ ["0"]))]
  (* Mode 0 allocates at top; 1 honors the dynamic at-bottom bit; 2 resets.
   * The low infinite-region bit distinguishes descriptors from finite storage. *)
  fun regionArg sma =
    case sma of LS.ATTOP_LI(a,_) => (a,0) | LS.ATTOP_LF(a,_) => (a,0)
      | LS.ATTOP_FI(a,_) => (a,0) | LS.ATTOP_FF(a,_) => (a,0)
      | LS.ATBOT_LI(a,_) => (a,2) | LS.ATBOT_LF(a,_) => (a,0)
      | LS.SAT_FI(a,_) => (a,1) | LS.SAT_FF(a,_) => (a,1)
      | LS.IGNORE => unsupported "ignored allocation"
  fun programPoint sma = case sma of
      LS.ATTOP_LI(_,p)=>p | LS.ATTOP_LF(_,p)=>p | LS.ATTOP_FI(_,p)=>p | LS.ATTOP_FF(_,p)=>p
    | LS.ATBOT_LI(_,p)=>p | LS.ATBOT_LF(_,p)=>p | LS.SAT_FI(_,p)=>p | LS.SAT_FF(_,p)=>p
    | LS.IGNORE=>0
  fun allocate fsz sma words =
    let val (a,mode)=regionArg sma
    in internalCall fsz "mlkit_arm64_alloc" [a,integer words,integer mode,integer 0,integer(programPoint sma)] end
  (* Keep the destination on the stack while filling it: source operands may
   * use either scratch register during address materialization. *)
  fun header tag = if tagged() then [constant(IntInf.fromInt(Word.toInt tag),X 16)] else []
  fun recordWithUntag untag fsz pat alloc prefix elems =
    let val (region,mode)=regionArg alloc
        val untag=untag andalso tagged() andalso not(tagPairs())
        val allocCode=if untag then internalCall fsz "mlkit_arm64_alloc"
          [region,integer(length elems),integer mode,integer 1,integer(programPoint alloc)]
          else allocate fsz alloc (length prefix+length elems)
        val skip=localFresh()
        val prefixGuard=if untag then read (fsz+2) region (X 17) @ [ins "tbnz" ["x17","#0",pr_lab skip]] else []
    in allocCode @ stack(true,16) @ store(X 16,SP,0) @
    prefixGuard @ List.concat(mapi(fn(i,code)=>code @ load(SP,0,X 17) @ store(X 16,X 17,8*i)) prefix) @ [Label skip] @
    List.concat(mapi(fn(i,a)=>read (fsz+2) a (X 16) @ load(SP,0,X 17) @
      store(X 16,X 17,8*(i+length prefix))) elems) @
    load(SP,0,X 16) @ stack(false,16) @ write fsz pat (X 16) end
  val record = recordWithUntag false
  fun regionAllocator place =
    let val kind=if gc() andalso not(tagPairs()) then case Effect.get_place_ty place of
      SOME Effect.PAIR_RT=>"Pair" | SOME Effect.REF_RT=>"Ref"
    | SOME Effect.TRIPLE_RT=>"Triple" | SOME Effect.ARRAY_RT=>"Array"
    | _=>"" else ""
    in if profiling() then "alloc" ^ kind ^ "RegionInfiniteProfiling"
       else "allocate" ^ kind ^ "Region" end
  fun regionPolicy global place =
    if profiling() then Effect.key_of_eps_or_rho place
    else if parallel() andalso (global orelse alloc_protect_always() orelse Effect.get_protect place = SOME true) then 1
    else 0
  fun primitive fsz {name,args,res} =
    let open PrimName
        fun binary opn = case (args,res) of
          ([a,b],[d]) => read fsz a (X 16) @ read fsz b (X 17) @
                        [ins opn ["x16","x16","x17"]] @ write fsz d (X 16)
        | _ => unsupported "primitive arity"
        fun compare cc = case (args,res) of
          ([a,b],[SS.FLOW_VAR_ATY(_,t,f)]) =>
            read fsz a (X 16) @ read fsz b (X 17) @
            [ins "cmp" ["x16","x17"],ins ("b." ^ cc) [pr_lab(LocalLab t)],
             ins "b" [pr_lab(LocalLab f)]]
        | ([a,b],[d]) => read fsz a (X 16) @ read fsz b (X 17) @
            [ins "cmp" ["x16","x17"],ins "cset" ["x16",cc],
             ins "lsl" ["x16","x16","#1"],ins "add" ["x16","x16","#1"]] @ write fsz d (X 16)
        | _ => unsupported "comparison arity"
        fun fpBinary opn = case (args,res) of
          ([a,b],[d]) => read fsz a (D 30) @ read fsz b (D 31) @
            [ins opn ["d30","d30","d31"]] @ write fsz d (D 30)
        | _ => unsupported "floating binary arity"
        fun fpUnary opn = case (args,res) of
          ([a],[d]) => read fsz a (D 30) @ [ins opn ["d30","d30"]] @ write fsz d (D 30)
        | _ => unsupported "floating unary arity"
        (* MI/LS/GT/GE all reject unordered FP comparisons. Signed integer
         * LT/LE would incorrectly treat NaN as less than another value. *)
        fun fpCompare cc = case (args,res) of
          ([a,b],[SS.FLOW_VAR_ATY(_,t,f)]) => read fsz a (D 30) @ read fsz b (D 31) @
            [ins "fcmp" ["d30","d31"],ins ("b." ^ cc) [pr_lab(LocalLab t)],ins "b" [pr_lab(LocalLab f)]]
        | ([a,b],[d]) => read fsz a (D 30) @ read fsz b (D 31) @
            [ins "fcmp" ["d30","d31"],ins "cset" ["x16",cc],
             ins "lsl" ["x16","x16","#1"],ins "add" ["x16","x16","#1"]] @ write fsz d (X 16)
        | _ => unsupported "floating comparison arity"
        fun overflow () = address(NameLab "exn_OVERFLOW",X 1) @ move(X 28,X 0) @ [ins "b" ["_raise_exn"]]
        fun checked opn = case (args,res) of
          ([a,b],[d]) => let val ok=localFresh()
            in read fsz a (X 16) @ read fsz b (X 17) @ [ins opn ["x16","x16","x17"],
              ins "b.vc" [pr_lab ok]] @ overflow() @ [Label ok] @ write fsz d (X 16) end
        | _=>unsupported "checked integer arity"
        fun taggedBinary opn adjustment = case (args,res) of
          ([a,b],[d])=>read fsz a (X 16) @ read fsz b (X 17) @
            [ins "sub" ["x17","x17","#1"],ins opn ["x16","x16","x17"]] @
            (if adjustment then let val ok=localFresh() in [ins "b.vc" [pr_lab ok]] @ overflow() @ [Label ok] end else []) @ write fsz d (X 16)
        | _=>unsupported "tagged arithmetic arity"
        fun boxed opn = case (args,res) of
          ([buffer,a,b],[d])=>read fsz a (X 16) @ load(X 16,payload(),D 30) @
            read fsz b (X 16) @ load(X 16,payload(),D 31) @ [ins opn ["d30","d30","d31"]] @
            read fsz buffer (X 16) @ (if tagged() then constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) @ store(X 17,X 16,0) else []) @ store(D 30,X 16,payload()) @ write fsz d (X 16)
        | _=>unsupported "boxed floating arity"
    in case name of
      Plus_int63 => taggedBinary "adds" true | Minus_int63 => taggedBinary "subs" true
    | Plus_word63 => taggedBinary "add" false | Minus_word63 => taggedBinary "sub" false
    | Plus_int64ub => checked "adds" | Minus_int64ub => checked "subs"
    | Plus_real => boxed "fadd" | Minus_real => boxed "fsub" | Mul_real => boxed "fmul" | Div_real => boxed "fdiv"
    | Plus_f64 => fpBinary "fadd" | Minus_f64 => fpBinary "fsub"
    | Mul_f64 => fpBinary "fmul" | Div_f64 => fpBinary "fdiv"
    | Neg_f64 => fpUnary "fneg" | Abs_f64 => fpUnary "fabs" | Sqrt_f64 => fpUnary "fsqrt"
    | Less_f64 => fpCompare "mi" | Lesseq_f64 => fpCompare "ls"
    | Greater_f64 => fpCompare "gt" | Greatereq_f64 => fpCompare "ge"
    | Int_to_f64 => (case (args,res) of ([a],[d])=>read fsz a (X 16) @
        (if tagged() then [ins "asr" ["x16","x16","#1"]] else []) @ [ins "scvtf" ["d30","x16"]] @ write fsz d (D 30) | _=>unsupported "int to float arity")
    | Real_to_f64 => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ load(X 16,payload(),D 30) @
        write fsz d (D 30) | _=>unsupported "real unboxing arity")
    | F64_to_real => (case (args,res) of ([a,b],[d])=>
        read fsz a (X 16) @ read fsz b (D 30) @ (if tagged() then constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) @ store(X 17,X 16,0) else []) @ store(D 30,X 16,payload()) @ write fsz d (X 16) | _=>unsupported "real boxing arity")
    | Get_ctx => (case res of [d]=>write fsz d (X 28) | _=>unsupported "context arity")
    | Exn_ptr => (case res of [d]=>load(X 28,8,X 16) @ write fsz d (X 16) | _=>unsupported "exception pointer arity")
    | Fresh_exname =>
        address(NameLab "exnameCounter",X 17) @
        (if parallel() then let val retry=localFresh()
          (* Incoming LR is already saved in the ML frame; w30 is scratch. *)
          in [Label retry,ins "ldaxr" ["x16","[x17]"],ins "add" ["x16","x16","#1"],
              ins "stlxr" ["w30","x16","[x17]"],ins "cbnz" ["w30",pr_lab retry]] end
         else load(X 17,0,X 16) @ [ins "add" ["x16","x16","#1"]] @ store(X 16,X 17,0)) @
        (case res of [d]=>write fsz d (X 16) | _=>unsupported "exception name arity")
    | Equal_ptr => compare "eq"
    | Plus_word64ub => binary "add"
    | Minus_word64ub => binary "sub"
    | Mul_word64ub => binary "mul"
    | Andb_word64ub => binary "and"
    | Orb_word64ub => binary "orr"
    | Xorb_word64ub => binary "eor"
    | Equal_word64ub => compare "eq"
    | Less_word64ub => compare "lo"
    | Lesseq_word64ub => compare "ls"
    | Greater_word64ub => compare "hi"
    | Greatereq_word64ub => compare "hs"
    | Equal_int32ub => compare "eq"
    | Equal_int63 => compare "eq" | Equal_word63 => compare "eq"
    | Less_int63 => compare "lt" | Lesseq_int63 => compare "le"
    | Greater_int63 => compare "gt" | Greatereq_int63 => compare "ge"
    | Less_word63 => compare "lo" | Lesseq_word63 => compare "ls"
    | Greater_word63 => compare "hi" | Greatereq_word63 => compare "hs"
    | Equal_int64ub => compare "eq"
    | Less_int64ub => compare "lt"
    | Lesseq_int64ub => compare "le"
    | Greater_int64ub => compare "gt"
    | Greatereq_int64ub => compare "ge"
    | Word64ub_to_int64ub => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ write fsz d (X 16)
                             | _ => unsupported "conversion arity")
    | Int64ub_to_word64ub => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ write fsz d (X 16)
                             | _ => unsupported "conversion arity")
    | _ => unsupported ("primitive " ^ PrimName.pp_prim name)
    end
  val dataLabels : label list ref = ref []
  fun dataLabel l = if List.exists (fn x=>AddressLabels.eq(x,l)) (!dataLabels) then ()
                    else dataLabels := l :: !dataLabels
  fun epilogue fsz =
    load(SP,8*(fsz+even(!currentArgs)),X 29) @
    load(SP,8*(fsz+even(!currentArgs)+1),X 30) @
    stack(false,8*(fsz+even(!currentArgs)+2)) @ [ins "ret" []]
  fun stmts fsz statements = List.concat(map (stmt fsz) statements)
  and stmt fsz ls =
    case ls of
      LS.SCOPE {scope,...} => stmts fsz scope
    | LS.LETREGION {rhos,body} =>
        List.concat(map(fn ((place,sz),off)=>case sz of LS.INF=>
          internalCall fsz (regionAllocator place) [SS.PHREG_ATY(X 28),SS.REG_F_ATY off,integer(regionPolicy false place)]
          | LS.WORDS n=>if n=0 orelse not(profiling()) then [] else
            internalCall fsz "allocRegionFiniteProfiling"
              [SS.REG_F_ATY(off+BackendInfo.objectDescSizeP+BackendInfo.finiteRegionDescSizeP),
               integer(Effect.key_of_eps_or_rho place),integer n]) rhos) @ stmts fsz body @
        List.concat(map(fn ((_,sz),_)=>case sz of LS.INF=>
          internalCall fsz "deallocateRegion" [SS.PHREG_ATY(X 28)]
          | LS.WORDS n=>if n=0 orelse not(profiling()) then [] else
            internalCall fsz "deallocRegionFiniteProfiling" []) (rev rhos))
    | LS.ASSIGN {pat=SS.FLOW_VAR_ATY(_,t,f),bind=LS.CON0{con,...}} =>
        [ins "b" [pr_lab(LocalLab(if Con.eq(con,Con.con_TRUE) then t else f))]]
    | LS.ASSIGN {pat,bind=LS.ATOM{aty}} => read fsz aty (X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.LOAD l} => address(DatLab l,X 16) @ load(X 16,0,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.STORE(aty,l)} =>
        (dataLabel l; read fsz aty (X 16) @ address(DatLab l,X 17) @ store(X 16,X 17,0) @
         constant(1,X 16) @ write fsz pat (X 16))
    | LS.ASSIGN {pat,bind=LS.REAL value} => address(realData value,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.F64 value} => address(static [Directive(".double " ^ String.translate(fn #"~"=>"-" | c=>String.str c) value)],X 16) @ load(X 16,0,D 30) @ write fsz pat (D 30)
    | LS.ASSIGN {pat,bind=LS.STRING value} => address(stringData value,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.RECORD{elems=[],...}} => constant(1,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.RECORD{elems,alloc,tag,maybeuntag}} => recordWithUntag maybeuntag fsz pat alloc (header tag) elems
    | LS.ASSIGN {pat,bind=LS.CLOS_RECORD{label,elems=elems as (_,_,rhos),alloc,f64_vars}} =>
        record fsz pat alloc (header(BackendInfo.tag_clos(false,1+length(LS.smash_free elems),1+length rhos+f64_vars)) @ [address(MLFunLab label,X 16)]) (LS.smash_free elems)
    | LS.ASSIGN {pat,bind=LS.SCLOS_RECORD{elems=elems as (_,_,rhos),alloc,f64_vars}} => record fsz pat alloc (header(BackendInfo.tag_sclos(false,length(LS.smash_free elems),length rhos+f64_vars))) (LS.smash_free elems)
    | LS.ASSIGN {pat,bind=LS.SELECT(i,a)} => read fsz a (X 16) @ load(X 16,8*i+payload(),X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.DEREF{aty}} => read fsz aty (X 16) @ load(X 16,payload(),X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.REF(alloc,a)} => recordWithUntag true fsz pat alloc (header(BackendInfo.tag_ref false)) [a]
    | LS.ASSIGN {pat,bind=LS.ASSIGNREF(_,a,b)} =>
        stack(true,16) @ read (fsz+2) a (X 16) @ store(X 16,SP,0) @
        read (fsz+2) b (X 16) @ load(SP,0,X 17) @ store(X 16,X 17,payload()) @ stack(false,16) @
        constant(1,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_MEM(alloc,n,untag)} =>
        (if untag andalso tagged() andalso not(tagPairs()) then let val(a,mode)=regionArg alloc
         in internalCall fsz "mlkit_arm64_alloc" [a,integer(n-1),integer mode,integer 1,integer(programPoint alloc)] end
         else allocate fsz alloc n) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_RHO{sma}} =>
        let val (a,mode)=regionArg sma
        in read fsz a (X 16) @ (case mode of 0=>[ins "and" ["x16","x16","#-3"]]
            | 2=>[ins "orr" ["x16","x16","#2"]] | _=>[]) @ write fsz pat (X 16) end
    | LS.ASSIGN {pat,bind=LS.CON0{con,con_kind,aux_regions,alloc}} =>
        let val reset=stmts fsz [LS.RESET_REGIONS{force=false,regions_for_resetting=aux_regions}]
            fun value n = reset @ constant(n,X 16) @ write fsz pat (X 16)
        in case con_kind of
          LS.ENUM i=>value(IntInf.fromInt(if tagged() orelse Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 2*i+1 else i))
        | LS.UNBOXED i=>value(IntInf.fromInt(4*i+3))
        | LS.UNBOXED_HIGH i=>value(IntInf.fromInt i * 281474976710656)
        | LS.BOXED i=>reset @ record fsz pat alloc [constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_con0(false,i))),X 16)] [] end
    | LS.ASSIGN {pat,bind=LS.CON1{con_kind,alloc,arg,...}} =>
        (case con_kind of LS.BOXED i=>record fsz pat alloc
           [constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_con1(false,i))),X 16)] [arg]
         | LS.UNBOXED i=>read fsz arg (X 16) @ constant(IntInf.fromInt i,X 17) @
           [ins "orr" ["x16","x16","x17"]] @ write fsz pat (X 16)
         | LS.UNBOXED_HIGH i=>read fsz arg (X 16) @ constant(IntInf.fromInt i*281474976710656,X 17) @
           [ins "orr" ["x16","x16","x17"]] @ write fsz pat (X 16)
         | _=>unsupported "unary enumeration")
    | LS.ASSIGN {pat,bind=LS.DECON{con_kind,con_aty,...}} =>
        read fsz con_aty (X 16) @ (case con_kind of LS.BOXED _=>load(X 16,8,X 16)
          | LS.UNBOXED 0=>[]
          | LS.UNBOXED _=>[ins "and" ["x16","x16","#-4"]]
          | LS.UNBOXED_HIGH _=>[ins "and" ["x16","x16","#0xffffffffffff"]]
          | _=>unsupported "enumeration deconstruction") @ write fsz pat (X 16)
    | LS.HANDLE {default,handl=(handl,closure),handl_return=(returned,result,bv),offset} =>
        let val ret=continuation bv val join=localFresh() val off=slot fsz offset
        in stmts fsz handl @ address(ret,X 16) @ store(X 16,SP,off) @
           read fsz closure (X 16) @ store(X 16,SP,off+8) @
           load(X 28,8,X 16) @ store(X 16,SP,off+16) @
           move(SP,X 16) @ store(X 16,SP,off+24) @ store(X 29,SP,off+32) @
           load(X 28,0,X 16) @ store(X 16,SP,off+40) @
           addOffset(SP,off,X 16) @ store(X 16,X 28,8) @ stmts fsz default @
           load(SP,off+16,X 16) @ store(X 16,X 28,8) @ [ins "b" [pr_lab join],Label ret] @
           write fsz result (X 0) @ stmts fsz returned @ [Label join] end
    | LS.RAISE {arg,...} => arguments fsz [SS.PHREG_ATY(X 28),arg] @ [ins "b" ["_raise_exn"]]
    | LS.FLUSH (aty,off) => read fsz aty (X 16) @ store(X 16,SP,slot fsz off)
    | LS.FETCH (aty,off) => load(SP,slot fsz off,X 16) @ write fsz aty (X 16)
    | LS.PRIM p => primitive fsz p
    | LS.CCALL {name="spawnone",args=[arg],rhos_for_result=[],res=[res]} =>
        let val ()=if parallel() then () else unsupported "spawnone without -par"
            val entry=localFresh()
            (* thread_init returns ThreadInfo*. Its leading fields are the
             * closure and context, checked by Runtime/Layout.c. *)
            val ()=staticData := !staticData @ [Directive ".text",Directive ".p2align 2",Label entry] @
              saveC() @ [ins "bl" ["_thread_init"]] @ addOffset(X 0,8,X 28) @
              load(X 0,0,X 0) @ load(X 0,0,X 17) @ constant(1,X 1) @
              stack(true,16) @ [ins "blr" ["x17"],ins "bl" ["_thread_exit"],ins "brk" ["#0"]]
        in stack(true,16) @ read (fsz+2) arg (X 16) @ store(X 16,SP,0) @
           address(entry,X 0) @ load(SP,0,X 1) @ [ins "bl" ["_thread_create"]] @
           stack(false,16) @ write fsz res (X 0) end
    | LS.CCALL {name,args,rhos_for_result,res} =>
        if length res > 1 then unsupported "multiple C results"
        else foreignCall fsz name (rhos_for_result@args) (fn _=>[]) @ results fsz res
    | LS.CCALL_AUTO c => autoCall fsz c
    | LS.EXPORT{name,clos_lab,arg=(aty,ft1,ft2)} =>
        let val ()=if ft1=LS.Int andalso ft2=LS.Int then () else unsupported "export other than int -> int"
            val ctx=DatLab(AddressLabels.new_named "arm64_export_ctx")
            val str=stringData name
            val ()=dataLabel clos_lab
            val ()=staticData := !staticData @ [Directive ".data",Directive ".p2align 3",Label ctx,Directive ".quad 0"] @
              function(NameLab name) @ saveC() @ deferGC() @
              (if parallel() then
                 (* The callback runs in the caller's runtime thread, which
                  * need not be the thread that registered the closure. *)
                 move(X 0,X 19) @ [ins "bl" ["_thread_info"]] @
                 addOffset(X 0,8,X 28) @ move(X 19,X 1)
               else move(X 0,X 1) @ address(ctx,X 16) @ load(X 16,0,X 28)) @
              address(DatLab clos_lab,X 0) @ load(X 0,0,X 0) @ load(X 0,payload(),X 17) @
              stack(true,16) @ [ins "blr" ["x17"]] @ resumeGC() @ restoreC() @ [ins "ret" []]
        in read fsz aty (X 16) @ address(DatLab clos_lab,X 17) @ store(X 16,X 17,0) @
           address(ctx,X 17) @ store(X 28,X 17,0) @
           address(str,X 16) @ address(NameLab name,X 17) @
           internalCall fsz "sml_regCfuns" [SS.PHREG_ATY(X 16),SS.PHREG_ATY(X 17)] end
    | LS.FUNCALL {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcall false fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res,bv=bv}
    | LS.JMP {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcall true fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res,bv=bv}
    | LS.FNCALL {opr,args,clos,res,bv} =>
        mlcall false fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res,bv=bv}
    | LS.FNJMP {opr,args,clos,res,bv} =>
        mlcall true fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res,bv=bv}
    | LS.SWITCH_I {switch=LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[(v,yes)],no),...} =>
        if v=IntInf.fromInt BackendInfo.ml_true then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_C (LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[((c,_),yes)],no)) =>
        if Con.eq(c,Con.con_TRUE) then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_C (LS.SWITCH(a,[],default)) => stmts fsz default
    | LS.SWITCH_C (LS.SWITCH(a,cases as ((_,kind),_)::_,default)) =>
        (* Constructor selectors are already encoded by closure conversion. *)
        let fun tag k=IntInf.fromInt(case k of LS.ENUM i=>i | LS.UNBOXED i=>i | LS.UNBOXED_HIGH i=>i | LS.BOXED i=>i)
            val prepare=case kind of LS.ENUM _=>[] | LS.BOXED _=>load(X 16,0,X 16)
              | LS.UNBOXED_HIGH _=>[ins "lsr" ["x16","x16","#48"]]
              | LS.UNBOXED _=>[ins "and" ["x17","x16","#3"],ins "cmp" ["x17","#3"],ins "csel" ["x16","x16","x17","eq"]]
        in read fsz a (X 16) @ prepare @ switchCode fsz
          (LS.SWITCH(SS.PHREG_ATY(X 16),map(fn((_,k),body)=>(tag k,body)) cases,default)) end
    | LS.SWITCH_W {switch=LS.SWITCH(a,cases,default),precision=63} => switchCode fsz (LS.SWITCH(a,map(fn(n,b)=>(2*n+1,b)) cases,default))
    | LS.SWITCH_I {switch=LS.SWITCH(a,cases,default),precision=63} => switchCode fsz (LS.SWITCH(a,map(fn(n,b)=>(2*n+1,b)) cases,default))
    | LS.SWITCH_W {switch,precision=64} => switchCode fsz switch
    | LS.SWITCH_I {switch,precision=64} => switchCode fsz switch
    | LS.RESET_REGIONS {regions_for_resetting,force} =>
        List.concat(map(fn LS.IGNORE=>[] | sma=>let val(a,mode)=regionArg sma
          in if mode=0 andalso not force then [] else internalCall fsz "mlkit_arm64_reset"
            [a,integer(if force orelse mode=2 then 2 else 1)] end) regions_for_resetting)
    | _ => unsupported (LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls)
  and flow fsz (t,f,yes,no) =
    let val done = localFresh()
    in [Label(LocalLab t)] @ stmts fsz yes @ [ins "b" [pr_lab done],Label(LocalLab f)] @
       stmts fsz no @ [Label done] end
  and switchCode fsz (LS.SWITCH(a,cases,default)) =
    let val done = localFresh()
        val branches = map (fn (v,body)=>(v,localFresh(),body)) cases
    in read fsz a (X 16) @
       List.concat(map (fn (v,l,_) => constant(v,X 17) @
         [ins "cmp" ["x16","x17"],ins "b.eq" [pr_lab l]]) branches) @
       stmts fsz default @ [ins "b" [pr_lab done]] @
       List.concat(map (fn (_,l,body)=>[Label l] @ stmts fsz body @ [ins "b" [pr_lab done]]) branches) @
       [Label done]
    end
  fun entryGC cc =
    if not(gc()) then [] else
    let val done=localFresh()
        val ac=CallConv.get_ccf_size cc val rc=CallConv.get_rcf_size cc
        val skip=length(CallConv.get_spilled_region_and_float_args cc)
        val mask=foldl(fn(lv,w)=>case A.RI.lv_to_reg lv of X n=>
          Word32.orb(w,Word32.<<(0w1,Word.fromInt n)) | _=>w) 0w0
          (CallConv.get_register_args_excluding_region_and_float_args cc)
        val registers=List.filter(fn n=>n<>18) (List.tabulate(31,fn i=>i))
        val save=List.concat(map(fn n=>store(X n,SP,8*(31-n))) registers)
        val restore=List.concat(map(fn n=>load(SP,8*(31-n),X n)) registers)
    in address(NameLab "disable_gc",X 16) @ load(X 16,0,X 16) @ [ins "cbnz" ["x16",pr_lab done]] @
       address(NameLab "time_to_gc",X 16) @ load(X 16,0,X 16) @
       (if extra_gc_checks() then [] else [ins "cbz" ["x16",pr_lab done]]) @
       stack(true,352) @ save @ addOffset(SP,352,X 16) @ store(X 16,SP,0) @ store(X 16,SP,344) @
       constant(0,X 16) @ store(X 16,SP,104) @
       List.concat(List.tabulate(8,fn i=>store(D i,SP,8*(39-i)))) @
       List.concat(mapi(fn(i,n)=>constant(IntInf.fromInt n,X 16) @ store(X 16,SP,320+8*i)) [skip,rc,ac]) @
       move(X 28,X 0) @ move(SP,X 1) @ constant(Word32.toLargeInt mask,X 2) @ [ins "bl" ["_gc"]] @
       List.concat(List.tabulate(8,fn i=>load(SP,8*(39-i),D i))) @ restore @ stack(false,352) @ [Label done]
    end
  fun top (l,cc,body) =
    let val ac = CallConv.get_ccf_size cc
        val () = currentArgs := ac
        val () = currentResults := CallConv.get_rcf_size cc
        val fsz = CallConv.get_frame_size cc
    in function (MLFunLab l) @ store(X 29,SP,8*even ac) @ store(X 30,SP,8*(even ac+1)) @
       addOffset(SP,8*even ac,X 29) @ entryGC cc @ stack(true,8*fsz) @
       (if profiling() then internalCall fsz "mlkit_arm64_profile_entry" [SS.PHREG_ATY(X 28),SS.REG_F_ATY(fsz-1)] else []) @ stmts fsz body @ epilogue fsz
    end
  fun CG {main_lab,code,imports,exports,safe} =
    let val () = dataLabels := []
        val () = staticData := []
        val () = frameIndex := []
        val text = List.concat(map (fn LS.FUN x=>top x | LS.FN x=>top x) code)
        fun data l = [Directive ".data",Directive ".p2align 3",
                      Directive(".globl " ^ pr_lab(DatLab l)),Label(DatLab l),Directive ".quad 1"]
            fun marker suffix = let val l=unitSymbol main_lab suffix
          in [Directive ".data",Directive ".p2align 3",Directive(".globl " ^ pr_lab l),Label l] end
        val metadata=marker "frames" @ [Directive(".quad " ^ Int.toString(length(!frameIndex)))] @
          List.concat(map(fn(pc,fd)=>[Directive(".quad " ^ pr_lab pc),Directive(".quad " ^ pr_lab fd)]) (!frameIndex)) @
          marker "roots" @ [Directive(".quad " ^ Int.toString(length(!dataLabels)))] @
          map(fn l=>Directive(".quad " ^ pr_lab(DatLab l))) (!dataLabels)
    in text @ marker "begin" @ List.concat(map data (!dataLabels)) @ !staticData @ marker "end" @
       (if gc() then metadata else []) end
  (* Runtime main enters code with the context in x0. This entry terminates
   * the process; returning foreign callbacks need a separate preserving bridge. *)
  fun linkCode repl (labs,_) =
    let
      val globals = [(Effect.toplevel_region_withtype_top,BackendInfo.toplevel_region_withtype_top_lab),
        (Effect.toplevel_region_withtype_string,BackendInfo.toplevel_region_withtype_string_lab),
        (Effect.toplevel_region_withtype_pair,BackendInfo.toplevel_region_withtype_pair_lab),
        (Effect.toplevel_region_withtype_array,BackendInfo.toplevel_region_withtype_array_lab),
        (Effect.toplevel_region_withtype_ref,BackendInfo.toplevel_region_withtype_ref_lab),
        (Effect.toplevel_region_withtype_triple,BackendInfo.toplevel_region_withtype_triple_lab)]
      val () = staticData := []
      fun datum l words = [Directive ".data",Directive ".p2align 3",
        Directive(".globl " ^ pr_lab l),Label l] @ map(fn s=>Directive(".quad " ^ s)) words
      fun init (place,l) = stack(true,8*even(BackendInfo.size_of_reg_desc())) @ move(X 28,X 0) @ move(SP,X 1) @ constant(IntInf.fromInt(regionPolicy true place),X 2) @
        [ins "bl" [pr_lab(NameLab(regionAllocator place))],ins "orr" ["x0","x0","#1"]] @ address(DatLab l,X 17) @ store(X 0,X 17,0)
      val exceptions = [("MATCH","Match",BackendInfo.exn_MATCH_lab),
        ("BIND","Bind",BackendInfo.exn_BIND_lab),("OVERFLOW","Overflow",BackendInfo.exn_OVERFLOW_lab),
        ("INTERRUPT","Interrupt",BackendInfo.exn_INTERRUPT_lab),("DIV","Div",BackendInfo.exn_DIV_lab),
        ("SUBSCRIPT","Subscript",BackendInfo.exn_SUBSCRIPT_lab),("SIZE","Size",BackendInfo.exn_SIZE_lab)]
      fun exnData(i,(name,display,lab)) =
        let val l=NameLab("exn_" ^ name) val str=stringData display
        in datum l (if tagged() then ["0x" ^ Word.toString(BackendInfo.tag_exname true),pr_lab l ^ "+16",
          "0x" ^ Word.toString(BackendInfo.tag_excon0 true),Int.toString i,pr_lab str]
          else [pr_lab l ^ "+8",Int.toString i,pr_lab str]) @ datum (DatLab lab) [pr_lab l] end
      val data = List.concat(map(fn(_,l)=>datum (DatLab l) ["0"]) globals) @
        datum (NameLab "exnameCounter") ["7"] @ List.concat(mapi exnData exceptions)
      val alloc = NameLab "mlkit_arm64_alloc"
      val finite=localFresh() val noreset=localFresh()
      val reset=NameLab "mlkit_arm64_reset" val resetDone=localFresh()
      val raising=NameLab "raise_exn" val unwind=localFresh() val unwound=localFresh() val uncaught=localFresh()
      val linkBegin=NameLab "arm64_link_begin" val linkEnd=NameLab "arm64_link_end"
      val sentinel=NameLab "arm64_sentinel"
      val linkFrames=NameLab "arm64_link_frames"
      val returnLabels=map(fn _=>localFresh()) labs
      val gcInit=if not(gc()) then [] else List.concat(map registerUnit labs) @
        address(linkFrames,X 0) @ constant(IntInf.fromInt(length labs),X 1) @
        address(linkBegin,X 2) @ address(linkEnd,X 3) @ constant(0,X 4) @ constant(0,X 5) @
        [ins "bl" ["_mlkit_arm64_register_image"]] @
        address(NameLab "stack_bot_gc",X 16) @ move(SP,X 17) @ store(X 17,X 16,0)
      val gcData=if not(gc()) then [] else datum (NameLab "data_begin_addr") [pr_lab linkBegin] @
        datum (NameLab "data_end_addr") [pr_lab linkEnd] @
        [Directive ".quad -1",Directive ".quad 0",Directive ".quad 0",Label sentinel] @
        datum linkFrames (List.concat(map(fn pc=>[pr_lab pc,pr_lab sentinel]) returnLabels))
    in function(NameLab "code") @ move(X 0,X 28) @
       (if profiling() then List.concat(map(fn name=>address(NameLab name,X 16) @ move(SP,X 17) @ store(X 17,X 16,0)) ["stackBot","maxStack","maxStackP"]) else []) @
       List.concat(map init globals) @ gcInit @
       (if repl then move(X 28,X 0) @ [ins "bl" ["_repl_interp"]]
        else List.concat(ListPair.map(fn(l,pc)=>stack(true,16) @ [ins "bl" [pr_lab(MLFunLab l)],Label pc]) (labs,returnLabels))) @
       List.concat(map(fn _=>move(X 28,X 0) @ [ins "bl" ["_deallocateRegion"]]) globals) @
       constant(0,X 0) @ [ins "b" ["_terminateML"]] @
       function alloc @ [ins "tbz" ["x0","#0",pr_lab finite]] @
       stack(true,48) @ store(X 19,SP,0) @ store(X 20,SP,8) @ store(X 30,SP,16) @ store(X 21,SP,24) @ store(X 22,SP,32) @
       move(X 4,X 22) @ move(X 3,X 21) @ move(X 0,X 19) @ move(X 1,X 20) @
       [ins "cmp" ["x2","#2"],ins "b.eq" [pr_lab resetDone],
        ins "cbz" ["x2",pr_lab noreset],ins "tbz" ["x0","#1",pr_lab noreset],Label resetDone,
        ins "bl" ["_resetRegion"],Label noreset] @
       move(X 19,X 0) @ move(X 20,X 1) @ move(X 22,X 2) @ [ins "bl" [if profiling() then "_allocProfiling" else if parallel() andalso unprotected() then "_alloc_unprotected" else "_alloc"],ins "sub" ["x0","x0","x21, lsl #3"]] @
       load(SP,32,X 22) @ load(SP,24,X 21) @ load(SP,0,X 19) @ load(SP,8,X 20) @ load(SP,16,X 30) @ stack(false,48) @ [ins "ret" [],Label finite,
        ins "and" ["x0","x0","#-4"]] @ (if profiling() then store(X 4,X 0,~16) else []) @ [ins "ret" []] @
       function reset @ [ins "tbz" ["x0","#0","1f"],ins "cmp" ["x1","#2"],
        ins "b.eq" ["2f"],ins "tbz" ["x0","#1","1f"],Directive "2:",ins "b" ["_resetRegion"],Directive "1:",ins "ret" []] @
       function raising @ move(X 0,X 28) @ move(X 1,X 27) @ load(X 28,8,X 19) @
       [ins "cbz" ["x19",pr_lab uncaught]] @
       (if profiling() then move(X 28,X 0) @ move(X 19,X 1) @ [ins "bl" ["_deallocateRegionsUntil"]]
        else [Label unwind] @ load(X 28,0,X 16) @ load(X 19,40,X 17) @
        [ins "cmp" ["x16","x17"],ins "b.eq" [pr_lab unwound]] @ move(X 28,X 0) @
        [ins "bl" ["_deallocateRegion"],ins "b" [pr_lab unwind],Label unwound]) @
       load(X 19,16,X 16) @ store(X 16,X 28,8) @ load(X 19,24,X 16) @ move(X 16,SP) @
       load(X 19,32,X 29) @ load(X 19,0,X 30) @ load(X 19,8,X 0) @ move(X 27,X 1) @
       load(X 0,payload(),X 17) @ stack(true,16) @ [ins "br" ["x17"],Label uncaught] @
       move(X 28,X 0) @ load(X 27,payload(),X 16) @ load(X 16,8+payload(),X 1) @ load(X 16,payload(),X 2) @ move(X 27,X 3) @
       [ins "b" ["_uncaught_exception"]] @ [Directive ".data",Directive ".p2align 3",Label linkBegin] @ data @ !staticData @ [Label linkEnd] @ gcData
    end
  fun generate_link_code args = linkCode false args
  fun generate_repl_init_code () = linkCode true ([],([],[]))
  fun generate_repl_link_code (name,labs) =
    let val join=localFresh() val handler=localFresh() val closure=localFresh()
        val beginData=localFresh() val endData=localFresh() val frames=localFresh() val sentinel=localFresh()
        val pcs=map(fn _=>localFresh()) labs
        val metadata=if not(gc()) then [] else
          List.concat(map registerUnit labs) @ address(frames,X 0) @ constant(IntInf.fromInt(length pcs),X 1) @
          address(beginData,X 2) @ address(endData,X 3) @ constant(0,X 4) @ constant(0,X 5) @
          [ins "bl" ["_mlkit_arm64_register_image"]]
    in function(NameLab name) @ saveC() @ address(NameLab "top_ctx",X 16) @ load(X 16,0,X 28) @ metadata @
       stack(true,48) @ address(join,X 16) @ store(X 16,SP,0) @
       address(closure,X 16) @ store(X 16,SP,8) @ load(X 28,8,X 16) @ store(X 16,SP,16) @
       move(SP,X 16) @ store(X 16,SP,24) @ store(X 29,SP,32) @ load(X 28,0,X 16) @ store(X 16,SP,40) @
       move(SP,X 16) @ store(X 16,X 28,8) @
       List.concat(ListPair.map(fn(l,pc)=>stack(true,16) @ [ins "bl" [pr_lab(MLFunLab l)],Label pc]) (labs,pcs)) @
       [Label join] @ load(SP,16,X 16) @ store(X 16,X 28,8) @ stack(false,48) @ restoreC() @ [ins "ret" [],Label handler] @
       store(X 30,SP,8) @ move(X 1,X 3) @ load(X 1,payload(),X 16) @ load(X 16,8+payload(),X 1) @
       load(X 16,payload(),X 2) @ move(X 28,X 0) @ [ins "bl" ["_uncaught_exception"]] @
       load(SP,8,X 30) @ stack(false,16) @ [ins "ret" [],Directive ".data",Directive ".p2align 3",Label beginData,Label closure] @
       (if tagged() then [Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_clos(true,1,1)))] else []) @
       [Directive(".quad " ^ pr_lab handler),Label endData] @
       (if not(gc()) then [] else [Directive ".quad -1",Directive ".quad 0",Directive ".quad 0",Label sentinel,Label frames] @
         List.concat(map(fn pc=>[Directive(".quad " ^ pr_lab pc),Directive(".quad " ^ pr_lab sentinel)]) pcs))
    end
end
