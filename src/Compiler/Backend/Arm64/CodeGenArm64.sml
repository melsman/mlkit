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
                    where type ('a,'rhs,'c) LinePrg = ('a,'rhs,'c) LineStmt.LinePrg
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
  (* Builders compose in instruction order and apply to the suffix right to left. *)
  infixr 5 ++
  type builder = A.inst list -> A.inst list
  fun ((f : builder) ++ (g : builder)) x = f(g x)
  fun one (i : A.inst) code = i :: code
  fun instruction make args code = make args :: code
  type label = AddressLabels.label
  type ('s,'o,'a) LinePrg = ('s,'o,'a) LS.LinePrg
  type offset = int
  type StoreTypeCO = SS.StoreTypeCO
  type AtySS = SS.Aty
  type AsmPrg = A.AsmPrg
  fun emit (code,file) = A.emit(A.optimise code,file)
  val messages_p = Flags.is_on0 "messages"
  fun message f = if messages_p() then print(f()) else ()
  val extra_gc_checks = Flags.add_bool_entry
    {long = "extra_gc_checks",short = NONE,item = ref false,neg = false,
     menu = ["Compiler","extra GC checks"],desc = "Collect at every ARM64 function entry."}
  val alloc_protect_always = Flags.add_bool_entry
    {long = "alloc_protect_always",short = NONE,item = ref false,neg = false,
     menu = ["Compiler","always protect allocation"],desc = "Always protect parallel allocation."}
  val tail_wrappers = Flags.add_bool_entry
    {long = "arm64_tail_wrappers",short = NONE,item = ref true,neg = true,
     menu = ["Compiler","ARM64 tail wrappers"],desc = "Omit frames in identity tail wrappers."}
  val self_loops = Flags.add_bool_entry
    {long = "arm64_self_loops",short = NONE,item = ref true,neg = true,
     menu = ["Compiler","ARM64 self loops"],desc = "Reuse frames in simple non-allocating self-tail recursion."}
  fun parallel () = Flags.is_on "parallelism"
  fun unprotected () = Flags.is_on "parallelism_alloc_unprotected"
  val gc = Flags.is_on0 "garbage_collection"
  val gengc = Flags.is_on0 "generational_garbage_collection"
  val tagged = BackendInfo.tag_values
  val profiling = Flags.is_on0 "region_profiling"
  val tagPairs = Flags.is_on0 "tag_pairs"
  fun payload () = if tagged() then 8 else 0
  fun slot fsz off = 8*(fsz-off-1)
  (* Keep fragments in reverse insertion order; never copy accumulated data. *)
  val staticChunks : A.inst list list ref = ref []
  fun addStatic code = staticChunks := code :: !staticChunks
  fun staticDataInto code =
    foldl (fn (chunk,code) => foldr (op ::) code chunk) code (!staticChunks)
  fun numberInto {value,precision} dst code =
    if tagged() andalso (precision = 32 orelse precision = 64) then
      let
        val l = DatLab(AddressLabels.new_named "arm64_integer")
        val () = addStatic
          [Directive(Data),Directive(Align 3),Label l,
           Directive(Quad ["0x" ^ Word.toString(BackendInfo.tag_word_boxed true)]),
           Directive(Quad ["0x" ^ IntInf.fmt StringCvt.HEX
             (IntInf.mod(value,18446744073709551616))])]
      in
        addressInto (l,dst) code
      end
    else constantInto
      (if precision = 63 orelse precision = 31 orelse (precision = 8 andalso tagged())
       then 2*value+1 else value,dst) code
  fun readInto fsz aty dst code =
    case aty of
      SS.PHREG_ATY src => moveInto (src,dst) code
    | SS.STACK_ATY off => loadInto (SP,slot fsz off,dst) code
    | SS.INTEGER_ATY n => numberInto n dst code
    | SS.WORD_ATY n => numberInto n dst code
    | SS.REG_I_ATY off =>
        (addOffsetInto (SP,slot fsz off,dst)
           ++ instruction A.orr (R(dst),R(dst),I(1))) code
    | SS.REG_F_ATY off => addOffsetInto (SP,slot fsz off,dst) code
    | SS.DROPPED_RVAR_ATY => constantInto (0,dst) code
    | SS.UNIT_ATY => constantInto (1,dst) code
    | _ => unsupported ("operand " ^ SS.pr_aty aty)
  fun writeInto fsz dst src code =
    case dst of
      SS.PHREG_ATY d => moveInto (src,d) code
    | SS.STACK_ATY off => storeInto (src,SP,slot fsz off) code
    | SS.UNIT_ATY => code
    | _ => unsupported ("result " ^ SS.pr_aty dst)
  (* Resolve allocated operands without routing them through scratch registers.
   * Materialisation still owns x16/x17; keep those out of the direct GPR path. *)
  fun operandInto fsz aty tmp =
    case (aty,tmp) of
      (SS.PHREG_ATY (src as X n),X _) =>
        if n <> 16 andalso n <> 17 then (src,fn code => code)
        else (tmp,readInto fsz aty tmp)
    | (SS.PHREG_ATY (src as D _),D _) => (src,fn code => code)
    | _ => (tmp,readInto fsz aty tmp)
  fun destinationInto fsz aty tmp =
    case (aty,tmp) of
      (SS.PHREG_ATY (dst as X _),X _) => (dst,fn code => code)
    | (SS.PHREG_ATY (dst as D _),D _) => (dst,fn code => code)
    | _ => (tmp,writeInto fsz aty tmp)
  (* Immediates must denote machine values, not boxed constants. *)
  fun machineConstant aty =
    let
      fun number {value,precision} =
        if tagged() andalso (precision = 32 orelse precision = 64) then NONE
        else SOME(if precision = 63 orelse precision = 31 orelse
                     (precision = 8 andalso tagged()) then 2*value+1 else value)
    in
      case aty of
        SS.INTEGER_ATY n => number n
      | SS.WORD_ATY n => number n
      | _ => NONE
    end
  fun smallImmediate (n:IntInf.int) = n >= 0 andalso n <= 4095
  fun immediate n = I n
  fun compareConstantInto src value code =
    if smallImmediate value then instruction A.cmp (R(src),immediate value) code
    else (constantInto(value,X 17)
       ++ instruction A.cmp (R(src),R(X 17))) code
  fun selectInto fsz aty offset pat code =
    let val (src,load) = operandInto fsz aty (X 16)
        val (dst,store) = destinationInto fsz pat (X 16)
    in (load ++ loadInto(src,offset,dst) ++ store) code
    end
  fun assignInto fsz src dst code =
    case (src,dst) of
      (SS.PHREG_ATY a,_) => writeInto fsz dst a code
    | (SS.STACK_ATY _,SS.PHREG_ATY rhs) => readInto fsz src rhs code
    | (_,SS.PHREG_ATY rhs) =>
        (case rhs of
           X _ => readInto fsz src rhs code
         | _ => (readInto fsz src (X 16) ++ moveInto (X 16,rhs)) code)
    | _ => (readInto fsz src (X 16) ++ writeInto fsz dst (X 16)) code
  fun localFresh () = A.LocalLab(AddressLabels.new_named "arm64")
  (* Stage all arguments before loading their target registers. This handles
   * cycles without destroying input registers and keeps SP aligned. *)
  fun argumentsInto fsz args code =
    let
      val n = length args
      val bytes = 16*((n+1) div 2)
      val () = if n <= 8 then () else unsupported "stack-passed call arguments"
      val code = stackInto (false,bytes) code
      val code = foldr (fn (i,code) => loadInto (SP,8*i,X i) code)
                       code (List.tabulate(n,fn i => i))
      val code = foldri (fn (i,a,code) =>
        (readInto (fsz+bytes div 8) a (X 16)
           ++ storeInto (X 16,SP,8*i)) code)
        code args
    in
      stackInto (true,bytes) code
    end
  fun even n = n + n mod 2
  fun first n xs = List.take(xs,Int.min(n,length xs))
  fun rest n xs = List.drop(xs,Int.min(n,length xs))
  val currentArgs = ref 0
  val currentResults = ref 0
  val currentLoop : (label * A.lab) option ref = ref NONE
  (* The callee releases its locals, argument area and header. The caller
   * receives a separately aligned result area and then releases that area. *)
  fun resultsInto fsz res code =
    let
      val spilled = Int.max(0,length res-3)
      val rw = even spilled
      val temp = even(length res)
      val code = stackInto (false,8*(temp+rw)) code
      val code = foldri (fn (i,a,code) =>
        (loadInto (SP,8*i,X 16)
           ++ writeInto (fsz+rw+temp) a (X 16)) code) code res
      val code = foldri (fn (i,_,code) =>
        if i<3 then storeInto (X i,SP,8*i) code
        else (loadInto (SP,8*(temp+spilled-1-(i-3)),X 16)
           ++ storeInto (X 16,SP,8*i)) code) code res
    in
      stackInto (true,8*temp) code
    end
  fun registersPlaced regs args =
    List.all (fn (i,a) => case a of SS.PHREG_ATY r => r = List.nth(regs,i)
                                | _ => false)
      (mapi (fn x => x) (first (length regs) args))
  fun mlResultsInto fsz res code =
    if not(registersPlaced [X 0,X 1,X 2] res) then resultsInto fsz res code
    else
      let
        val spilled = rest 3 res
        val n = length spilled
        val rw = even n
        (* Only stack results need copying. Stage them before writing caller
         * slots so overlapping destinations remain safe. *)
        val code = stackInto (false,16*rw) code
        val code = foldri (fn (i,a,code) =>
          (loadInto (SP,8*i,X 16)
           ++ writeInto (fsz+2*rw) a (X 16)) code) code spilled
        val code = foldri (fn (i,_,code) =>
          (loadInto (SP,8*(rw+n-1-i),X 16)
           ++ storeInto (X 16,SP,8*i)) code) code spilled
      in stackInto (true,8*rw) code
      end
  (* Return PCs anchor inline descriptors, just as on X64. The branch to
   * the callee skips the data; returning through x30 reaches executable code. *)
  fun continuationInto pc bv code =
    if gc() then
      Directive(Align 3) ::
      foldl (fn (w,code) => Directive(Quad ["0x" ^ Word32.fmt StringCvt.HEX w]) :: code)
        (Label pc :: code) bv
    else Label pc :: code
  datatype target = Direct of label | Indirect of SS.Aty
  fun callInto target pc code =
    if gc() then
      (addressInto (pc,X 30)
         ++ instruction (case target of Direct _ => A.b | Indirect _ => A.br) (case target of Direct l => L(MLFunLab l) | Indirect _ => R(X 17))) code
    else
      (instruction (case target of Direct _ => A.bl | Indirect _ => A.blr) (case target of Direct l => L(MLFunLab l) | Indirect _ => R(X 17))) code
  fun unitSymbol lab suffix = NameLab(AddressLabels.pr_label lab ^ "_arm64_" ^ suffix)
  fun mlcallInto tail fsz target {args,reg_args,fargs,clos,res,bv} code =
    let
      val suffix = code
      val returnLabel = localFresh()
      val gp = (case clos of NONE => [] | SOME a => [a]) @ args @ reg_args
      val fp = fargs
      val sa = rest 8 gp @ rest 8 fp
      val ac = length sa
      val aw = even ac
      val rc = Int.max(0,length res-3)
      val rw = even rc
      (* RegAlloc has already placed ML register arguments. The fallback also
       * supports direct users of the code generator with unresolved operands. *)
      val placed = registersPlaced (map X AbiArm64.mlArgumentGPRs) gp andalso
                   registersPlaced (map D AbiArm64.mlArgumentFPRs) fp
      val staged = (if placed then sa else gp @ fp) @
                   (case target of Direct _ => [] | Indirect a => [a])
      val sw = even(length staged)
      val newCall = aw+2+rw
      val oldArg = even(!currentArgs)+2
      val workspace = if tail then sw+aw+2 else sw+newCall
      val dest = if tail then workspace+fsz+oldArg-(aw+2) else sw
      val () = if tail andalso rc <> !currentResults then
                 unsupported "tail call with incompatible result area" else ()
      val stackArgs = if placed then List.tabulate(ac,fn i => i)
                      else mapi (fn (i,_) => i+8) (rest 8 gp) @
                      mapi (fn (i,_) => length gp+8+i) (rest 8 fp)
      val code = if tail then
          (case target of
             Direct l => A.b (L(MLFunLab l))
           | Indirect _ => A.br (R(X 17))) :: code
        else (callInto target returnLabel
           ++ continuationInto returnLabel bv
           ++ mlResultsInto fsz res) code
      val code = stackInto (false,8*(if tail then dest else sw)) code
      val code = case target of
                   Direct _ => code
                 | Indirect _ => (loadInto (SP,8*(length staged-1),X 17)
                    ++ loadInto (X 17,payload(),X 17)) code
      val code = if placed then code else foldri (fn (i,_,code) => loadInto (SP,8*(length gp+i),D i) code)
                        code (first 8 fp)
      val code = if placed then code else
        foldri (fn (i,_,code) => loadInto (SP,8*i,X i) code) code (first 8 gp)
      val code = if tail then
                   (loadInto (SP,8*(workspace+fsz+even(!currentArgs)),X 29)
                      ++ loadInto (SP,8*(workspace+fsz+even(!currentArgs)+1),X 30)) code
                 else code
      val code = foldri (fn (i,source,code) =>
        (loadInto (SP,8*source,X 16)
           ++ storeInto (X 16,SP,8*(dest+ac-1-i))) code)
        code stackArgs
      val code = foldri (fn (i,a,code) =>
        (readInto (fsz+workspace) a (X 16)
           ++ storeInto (X 16,SP,8*i)) code) code staged
    in
      case (tail,placed,ac,target,!currentLoop) of
        (true,true,0,Direct target,SOME (self,loop)) =>
          if AddressLabels.eq(target,self) then instruction A.b (L(loop)) suffix
          else stackInto (true,8*workspace) code
      | _ => stackInto (true,8*workspace) code
    end
  (* Precision zero is private to raw runtime-helper arguments. *)
  fun integer n = SS.INTEGER_ATY{value = IntInf.fromInt n,precision = 0}
  (* Late allocation calls preserve C-clobbered ML registers, including the
   * FP spill temporaries. C callees already preserve x19-x28 and d8-d15.
   * Actual collection uses the separate, collector-visible entryGCInto image. *)
  val savedRegs = List.tabulate(16,X) @ List.tabulate(8,D) @
                  List.tabulate(14,fn i => D(i+16))
  fun internalCallLiveInto live fsz name args code =
    let
      val regs = List.filter (fn r => List.exists (fn s => r = s) live) savedRegs
      val words = even(length regs)
      val code = stackInto (false,8*words) code
      val code = foldri (fn (i,a,code) => loadInto (SP,8*i,a) code) code regs
      val code = (argumentsInto (fsz+words) args
         ++ instruction A.bl (L(NameLab name))
         ++ moveInto (X 0,X 16)) code
      val code = foldri (fn (i,a,code) => storeInto (a,SP,8*i) code) code regs
    in
      stackInto (true,8*words) code
    end
  fun internalCallInto fsz name args = internalCallLiveInto savedRegs fsz name args
  (* Foreign calls may re-enter ML through an exported hook. The existing IR
   * carries no root map at a C call, so defer collection across that dynamic
   * extent, including callbacks. Pending collection is retained on return. *)
  fun foreignCallInto fsz name args convert code =
    if name = ":" then unsupported "dynamic foreign symbol resolution"
    else scalarCallInto
      {name = name,fixed = map (fn _ => AbiArm64.I64) args,variadic = [],protectGC = gc(),
       loadArgument = fn (i,extra) =>
         readInto (fsz+extra) (List.nth(args,i)) (X 16) ++ convert(i,X 16)} code
  fun autoCallInto fsz {name,args:(SS.Aty*LS.foreign_type) list,rhos_for_result,res = (dst,ft)} code =
    let
      fun convert (i,r) code =
        case #2(List.nth(args,i)) of
          LS.Bool => A.lsr (R(r),R(r),I(1)) :: code
        | LS.Int => if tagged() then A.asr (R(r),R(r),I(1)) :: code else code
        | LS.Int32 => if tagged() then loadInto (r,8,r) code else code
        | LS.Int64 => if tagged() then loadInto (r,8,r) code else code
        | LS.ForeignPtr => if tagged() then A.sub (R(r),R(r),I(1)) :: code else code
        | LS.CharArray => A.add (R(r),R(r),I(8)) :: code
        | LS.Unit => unsupported "unit foreign argument"
      val boxed = tagged() andalso (ft = LS.Int32 orelse ft = LS.Int64)
      fun tag code = (instruction A.lsl (R(X 0),R(X 0),I(1))
         ++ instruction A.add (R(X 0),R(X 0),I(1))) code
      val code = writeInto fsz dst (X 0) code
      val code = case ft of
          LS.Unit => constantInto (1,X 0) code
        | LS.Bool => (instruction A.cmp (R(X 0),I(0))
           ++ instruction A.cset (R(X 0),C NE)
           ++ tag) code
        | LS.Int => if tagged() then tag code else code
        | LS.ForeignPtr => if tagged() then A.add (R(X 0),R(X 0),I(1)) :: code else code
        | LS.CharArray => unsupported "foreign char-array result"
        | _ => if boxed then (loadInto (SP,0,X 16)
           ++ storeInto (X 0,X 16,8)
           ++ constantInto (IntInf.fromInt(Word.toInt(BackendInfo.tag_word_boxed false)),X 17)
           ++ storeInto (X 17,X 16,0)
           ++ moveInto (X 16,X 0)
           ++ stackInto (false,16)) code
          else code
      val code = foreignCallInto (fsz+(if boxed then 2 else 0)) name (map #1 args) convert code
    in
      if boxed then
        case rhos_for_result of
          [a] => (stackInto (true,16)
             ++ readInto (fsz+2) a (X 16)
             ++ storeInto (X 16,SP,0)) code
        | _ => unsupported "boxed foreign result without storage"
      else code
    end
  val cSaved = List.tabulate(12,fn i => X(i+19)) @ List.tabulate(8,fn i => D(i+8))
  fun saveRegisters code =
    foldri (fn (i,r,code) => storeInto (r,SP,8*i) code) code cSaved
  fun restoreRegisters code =
    foldri (fn (i,r,code) => loadInto (SP,8*i,r) code) code cSaved
  fun saveCInto () code =
    (stackInto(true,160)
      ++ saveRegisters
      ++ addOffsetInto(SP,80,X 29)) code
  fun restoreCInto () code =
    (restoreRegisters
      ++ stackInto(false,160)) code
  fun deferGCInto () code =
    if not(gc()) then
      code
    else
      (stackInto(true,16)
        ++ addressInto(NameLab "disable_gc",X 16)
        ++ loadInto(X 16,0,X 17)
        ++ storeInto(X 17,SP,0)
        ++ constantInto(1,X 17)
        ++ storeInto(X 17,X 16,0)) code
  fun resumeGCInto () code =
    if not(gc()) then
      code
    else
      (loadInto(SP,0,X 17)
        ++ addressInto(NameLab "disable_gc",X 16)
        ++ storeInto(X 17,X 16,0)
        ++ stackInto(false,16)) code
  fun registerUnitInto l code =
    (addressInto(unitSymbol l "roots",X 0)
      ++ addressInto(unitSymbol l "begin",X 1)
      ++ addressInto(unitSymbol l "end",X 2)
      ++ addOffsetInto(X 0,8,X 3)
      ++ loadInto(X 0,0,X 4)
      ++ instruction A.bl (L(NameLab "mlkit_arm64_register_static_image"))) code
  fun static words =
    let
      val l = DatLab(AddressLabels.new_named "arm64_data")
    in
      addStatic (Directive(Data) :: Directive(Align 3) :: Label l :: words); l
    end
  fun realData value =
    let
      val code = [Directive(Double (String.translate(fn #"~" => "-" | c => String.str c) value))]
      val code = if tagged() then
                   Directive(Quad ["0x" ^ Word.toString(BackendInfo.tag_real true)]) :: code
                 else code
    in
      static code
    end
  fun stringData textValue = static
    [Directive(Quad [("0x" ^ Word.toString(BackendInfo.tag_string(true,size textValue)))]),
     Directive(Bytes (map (Int.toString o Char.ord) (String.explode textValue) @ ["0"]))]
  (* Mode 0 allocates at top; 1 honors the dynamic at-bottom bit; 2 resets.
   * The low infinite-region bit distinguishes descriptors from finite storage. *)
  fun regionArg sma =
    case sma of LS.ATTOP_LI(a,_) => (a,0) | LS.ATTOP_LF(a,_) => (a,0)
      | LS.ATTOP_FI(a,_) => (a,0) | LS.ATTOP_FF(a,_) => (a,0)
      | LS.ATBOT_LI(a,_) => (a,2) | LS.ATBOT_LF(a,_) => (a,0)
      | LS.SAT_FI(a,_) => (a,1) | LS.SAT_FF(a,_) => (a,1)
      | LS.IGNORE => unsupported "ignored allocation"
  fun programPoint sma = case sma of
      LS.ATTOP_LI(_,p) => p | LS.ATTOP_LF(_,p) => p | LS.ATTOP_FI(_,p) => p | LS.ATTOP_FF(_,p) => p
    | LS.ATBOT_LI(_,p) => p | LS.ATBOT_LF(_,p) => p | LS.SAT_FI(_,p) => p | LS.SAT_FF(_,p) => p
    | LS.IGNORE => 0
  (* 0: known finite; 1: known infinite; 2: inspect the runtime status bit. *)
  fun regionKind sma =
    case sma of
      LS.ATTOP_LF _ => 0 | LS.ATBOT_LF _ => 0
    | LS.ATTOP_FF _ => 2 | LS.SAT_FF _ => 2
    | _ => 1
  fun countInto reg n code =
    if n >= 0 andalso n < 65536 then A.mov (R(reg),imm n) :: code
    else constantInto (IntInf.fromInt n,reg) code
  fun adjustInto opn reg bytes code =
    if bytes = 0 then code
    else
      let
        val n = Int.min(bytes,4095)
      in
        (instruction opn (R(reg),R(reg),imm n)
           ++ adjustInto opn reg (bytes-n)) code
      end
  val allocStub = NameLab "mlkit_arm64_allocate_preserving"
  val untaggedAllocStub = NameLab "mlkit_arm64_allocate_untagged_preserving"
  val resetStub = NameLab "mlkit_arm64_reset_preserving"
  (* Private helper ABI: x16 is the region/result, x17 the word count.
   * x16/x17/x30 are scratch; all allocatable ML registers survive a slow call.
   * Profiling passes its program point in an aligned caller stack slot. *)
  fun allocSlowInto words untag pp code =
    let
      val target = if untag then untaggedAllocStub else allocStub
      val code = if profiling() then stackInto (false,16) code else code
      val code = (countInto (X 17) words
         ++ instruction A.bl (L(target))) code
    in
      if profiling() then (stackInto (true,16)
         ++ countInto (X 17) pp
         ++ storeInto (X 17,SP,0)) code
      else code
    end
  fun resetLoadedInto mode code =
    if mode = 0 then code
    else
      let
        val done = localFresh()
        val slow = localFresh()
        val generations = if gengc() then [0,16] else [0]
        val header = if gengc() then 24 else 16
        val lobjs = if gengc() then 40 else 24
        fun firstPage off code =
          (instruction A.and_ (R(X 17),R(X 16),I(~4))
             ++ loadInto (X 17,off+8,X 30)
             ++ instruction A.and_ (R(X 30),R(X 30),I(~32))) code
        fun check (off,code) = (firstPage off
           ++ loadInto (X 30,0,X 17)
           ++ instruction A.cbnz (R(X 17),L(slow))) code
        fun reset (off,code) =
          (firstPage off
           ++ instruction A.add (R(X 30),R(X 30),imm header)
           ++ storeInto (X 30,X 17,off)
           ++ (if gengc() then storeInto (X 30,X 30,~8) else fn code => code)) code
        val code = Label done :: code
        val code = if profiling() orelse parallel() then A.bl (L(resetStub)) :: code
          else
            let
              val () = addStatic
                [Directive(Text),Directive(Align 2),Label slow,
                 A.bl (L(resetStub)),A.b (L(done))]
              val code = foldr reset code generations
              val code = foldr check code generations
            in
              (instruction A.and_ (R(X 17),R(X 16),I(~4))
                 ++ loadInto (X 17,lobjs,X 30)
                 ++ instruction A.cbnz (R(X 30),L(slow))) code
            end
      in
        if mode = 1 then A.tbz (R(X 16),I(1),L(done)) :: code else code
      end
  fun resetRegionInto fsz force sma code =
    let
      val (a,mode) = regionArg sma
      val mode = if force then 2 else mode
      val kind = regionKind sma
    in
      if kind = 0 orelse mode = 0 then code
      else
        let
          val done = localFresh()
          val code = (resetLoadedInto mode
             ++ one (Label done)) code
          val code = if kind = 2 then A.tbz (R(X 16),I(0),L(done)) :: code else code
        in
          readInto fsz a (X 16) code
        end
    end
  fun allocateInRegionInto fsz sma words untag code =
    let
      val (a,mode) = regionArg sma
      val kind = regionKind sma
      val pp = programPoint sma
      val done = localFresh()
      val finite = localFresh()
      fun finiteCode code =
        (instruction A.and_ (R(X 16),R(X 16),I(~4))
         ++ (if profiling() then countInto (X 17) pp ++ storeInto (X 17,X 16,~16)
             else fn code => code)) code
      fun infiniteCode code =
        if profiling() orelse (parallel() andalso not(unprotected())) orelse
           words > BackendInfo.size_region_page() div 8 - (if gengc() then 3 else 2) then
          allocSlowInto words untag pp code
        else
          let
            val slow = localFresh()
            val joined = localFresh()
            val bytes = 8*words
            val () = addStatic
              (Directive(Text) :: Directive(Align 2) :: Label slow ::
               A.orr (R(X 16),R(X 16),I(1)) ::
               allocSlowInto words untag pp [A.b (L(joined))])
            val code = Label joined :: code
            val code = if gc() then (addressInto (NameLab "alloc_period",X 17)
               ++ loadInto (X 17,0,X 30)
               ++ adjustInto A.add (X 30) bytes
               ++ storeInto (X 30,X 17,0)) code else code
          in
            (instruction A.and_ (R(X 16),R(X 16),I(~4))
             ++ loadInto (X 16,0,X 17)
             ++ instruction A.sub (R(X 17),R(X 17),I(1))
             ++ instruction A.orr (R(X 30),R(X 17),imm(BackendInfo.size_region_page()-1))
             ++ adjustInto A.add (X 17) bytes
             ++ instruction A.cmp (R(X 17),R(X 30))
             ++ instruction A.b_hi (L(slow))
             ++ instruction A.add (R(X 17),R(X 17),I(1))
             ++ storeInto (X 17,X 16,0)
             ++ moveInto (X 17,X 16)
             ++ adjustInto A.sub (X 16) (bytes+(if untag then 8 else 0))) code
          end
      val code = if kind = 0 then finiteCode code
        else if kind = 1 then (resetLoadedInto mode
           ++ infiniteCode) code
        else (instruction A.tbz (R(X 16),I(0),L(finite))
           ++ resetLoadedInto mode
           ++ infiniteCode
           ++ instruction A.b (L(done))
           ++ one (Label finite)
           ++ finiteCode
           ++ one (Label done)) code
    in
      readInto fsz a (X 16) code
    end
  fun allocateInto fsz sma words code = allocateInRegionInto fsz sma words false code
  (* A non-conflicting allocated destination survives field materialisation.
   * Keep the stack fallback for spilled destinations and source aliases. *)
  datatype record_field = Constant of IntInf.int | Address of A.lab
  fun header tag fields =
    if tagged() then Constant(IntInf.fromInt(Word.toInt tag)) :: fields else fields
  fun fieldInto (Constant n) code = constantInto (n,X 16) code
    | fieldInto (Address l) code = addressInto (l,X 16) code
  fun recordWithUntagInto untag fsz pat alloc prefix elems code =
    let
      val (region,mode) = regionArg alloc
      val untag = untag andalso tagged() andalso not(tagPairs())
      val skip = localFresh()
      fun mentions dst (SS.PHREG_ATY src) = src = dst
        | mentions _ _ = false
      val resident = case pat of
          SS.PHREG_ATY (dst as X n) =>
            if List.exists (fn r => r = n) AbiArm64.allocatableGPRs andalso
               not(List.exists (mentions dst) elems) andalso
               not(untag andalso mentions dst region) then SOME dst else NONE
        | _ => NONE
      val words = if untag then length elems else length prefix+length elems
    in
      case resident of
        SOME dst =>
          let
            val code = foldri (fn (i,a,code) =>
              case a of
                SS.PHREG_ATY src => storeInto (src,dst,8*(i+length prefix)) code
              | _ => (readInto fsz a (X 16)
                   ++ storeInto (X 16,dst,8*(i+length prefix))) code) code elems
            val code = foldri (fn (i,fragment,code) =>
              (fieldInto fragment ++ storeInto (X 16,dst,8*i)) code)
              (Label skip :: code) prefix
            val code = if untag then (readInto fsz region (X 17)
               ++ instruction A.tbnz (R(X 17),I(0),L(skip))) code else code
          in
            (allocateInRegionInto fsz alloc words untag
             ++ moveInto (X 16,dst)) code
          end
      | NONE =>
          let
            val code = (loadInto (SP,0,X 16)
               ++ stackInto (false,16)
               ++ writeInto fsz pat (X 16)) code
            val code = foldri (fn (i,a,code) =>
              (readInto (fsz+2) a (X 16)
                 ++ loadInto (SP,0,X 17)
                 ++ storeInto (X 16,X 17,8*(i+length prefix))) code) code elems
            val code = foldri (fn (i,fragment,code) =>
              (fieldInto fragment
                 ++ loadInto (SP,0,X 17)
                 ++ storeInto (X 16,X 17,8*i)) code)
              (Label skip :: code) prefix
            val code = if untag then (readInto (fsz+2) region (X 17)
               ++ instruction A.tbnz (R(X 17),I(0),L(skip))) code else code
          in
            (allocateInRegionInto fsz alloc words untag
             ++ stackInto (true,16)
             ++ storeInto (X 16,SP,0)) code
          end
    end
  val recordInto = recordWithUntagInto false
  fun regionAllocator place =
    let
      val kind =
        if gc() andalso not(tagPairs()) then
          case Effect.get_place_ty place of
            SOME Effect.PAIR_RT => "Pair" | SOME Effect.REF_RT => "Ref"
          | SOME Effect.TRIPLE_RT => "Triple" | SOME Effect.ARRAY_RT => "Array"
          | _ => ""
        else ""
    in
      if profiling() then "alloc" ^ kind ^ "RegionInfiniteProfiling"
       else "allocate" ^ kind ^ "Region"
    end
  fun regionPolicy global place =
    if profiling() then Effect.key_of_eps_or_rho place
    else if parallel() andalso (global orelse alloc_protect_always() orelse Effect.get_protect place = SOME true) then 1
    else 0
  datatype numeric_operation = NumAbs | NumAndb | NumEqual | NumGreater | NumGreatereq | NumLess | NumLesseq | NumMinus | NumMul | NumNeg | NumOrb | NumPlus | NumXorb
  fun primitiveInto fsz {name,args,res} code =
    let
      open PrimName
      fun operation immediateOK opn tmp1 tmp2 code =
        (case (args,res) of
          ([a,rhs],[d]) =>
            let val constant = machineConstant rhs
                val (a,loadA) = operandInto fsz a tmp1
                val (rhs,loadB) = operandInto fsz rhs tmp2
                val (d,storeD) = destinationInto fsz d tmp1
                val operation = case constant of
                    SOME n =>
                      if immediateOK andalso smallImmediate n then
                        instruction opn (R(d),R(a),immediate n)
                      else loadB ++ instruction opn (R(d),R(a),R(rhs))
                  | NONE => loadB ++ instruction opn (R(d),R(a),R(rhs))
            in (loadA ++ operation ++ storeD) code
            end
          | _ => unsupported "binary primitive arity")
      fun binary immediateOK opn = operation immediateOK opn (X 16) (X 17)
      fun fpBinary opn = operation false opn (D 30) (D 31)
      fun comparison immediateOK opn tmp1 tmp2 cc code =
        (case (args,res) of
          ([a,rhs],[d]) =>
            let val constant = machineConstant rhs
                val (a,loadA) = operandInto fsz a tmp1
                val (rhs,loadB) = operandInto fsz rhs tmp2
                val finish = case d of
                    SS.FLOW_VAR_ATY(_,t,f) =>
                      instruction (branch cc) (L(LocalLab t)) ++
                      instruction A.b (L(LocalLab f))
                  | _ =>
                      let val (d,storeD) = destinationInto fsz d (X 16)
                      in instruction A.cset (R(d),C(cc)) ++
                         instruction A.lsl (R(d),R(d),I(1)) ++
                         instruction A.add (R(d),R(d),I(1)) ++ storeD
                      end
                val compare =
                  case constant of
                    SOME n =>
                      if immediateOK andalso smallImmediate n then instruction A.cmp (R(a),immediate n)
                      else loadB ++ instruction opn (R(a),R(rhs))
                  | _ => loadB ++ instruction opn (R(a),R(rhs))
            in (loadA ++ compare ++ finish) code
            end
          | _ => unsupported "comparison arity")
      fun compare cc = comparison true A.cmp (X 16) (X 17) cc
      (* MI/LS/GT/GE all reject unordered FP comparisons. *)
      fun fpCompare cc = comparison false A.fcmp (D 30) (D 31) cc
      fun fpUnary opn code =
        (case (args,res) of
          ([a],[d]) =>
            let val (a,loadA) = operandInto fsz a (D 30)
                val (d,storeD) = destinationInto fsz d (D 30)
            in (loadA ++ instruction opn (R(d),R(a)) ++ storeD) code
            end
          | _ => unsupported "floating unary arity")
      fun overflow () code =
        (addressInto(NameLab "exn_OVERFLOW",X 1)
          ++ moveInto(X 28,X 0)
          ++ instruction A.b (L(NameLab "raise_exn"))) code
      fun checked opn code =
        (case (args,res) of
          ([a,rhs],[d]) =>
            let val constant = machineConstant rhs
                val ok = localFresh()
                val (a,loadA) = operandInto fsz a (X 16)
                val (rhs,loadB) = operandInto fsz rhs (X 17)
                val (d,storeD) = destinationInto fsz d (X 16)
                val arithmetic = case constant of
                    SOME n =>
                      if smallImmediate n then
                        instruction opn (R(d),R(a),immediate n)
                      else loadB ++ instruction opn (R(d),R(a),R(rhs))
                  | NONE => loadB ++ instruction opn (R(d),R(a),R(rhs))
            in
              (loadA ++ arithmetic
               ++ instruction A.b_vc (L(ok))
               ++ overflow()
               ++ one (Label ok)
               ++ storeD) code
            end
          | _ => unsupported "checked integer arity")
      fun taggedBinary opn adjustment code =
        (case (args,res) of
          ([a,rhs],[d]) =>
            let val constant = machineConstant rhs
                val (a,loadA) = operandInto fsz a (X 16)
                val (rhs,loadB) = operandInto fsz rhs (X 17)
                val (d,storeD) = destinationInto fsz d (X 16)
                val code = storeD code
                val code = if adjustment then
                    let val ok = localFresh()
                    in (instruction A.b_vc (L(ok)) ++ overflow()
                        ++ one (Label ok)) code
                    end
                  else code
                val arithmetic =
                  case constant of
                    SOME n =>
                      if smallImmediate(n-1) then
                        instruction opn (R(d),R(a),immediate(n-1))
                      else loadB ++ instruction A.sub (R(X 17),R(rhs),I(1)) ++
                           instruction opn (R(d),R(a),R(X 17))
                  | NONE => loadB ++ instruction A.sub (R(X 17),R(rhs),I(1)) ++
                            instruction opn (R(d),R(a),R(X 17))
            in (loadA ++ arithmetic) code
            end
          | _ => unsupported "tagged arithmetic arity")
      fun boxed opn code =
        (case (args,res) of
          ([buffer,a,rhs],[d]) =>
            let
              val code =
                (storeInto(D 30,X 16,payload())
                 ++ writeInto fsz d (X 16)) code
              val code = if tagged() then
                  (constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17)
                    ++ storeInto(X 17,X 16,0)) code
                else
                  code
            in
              (readInto fsz a (X 16)
               ++ loadInto(X 16,payload(),D 30)
               ++ readInto fsz rhs (X 16)
               ++ loadInto(X 16,payload(),D 31)
               ++ instruction opn (R(D 30),R(D 30),R(D 31))
               ++ readInto fsz buffer (X 16)) code
            end
          | _ => unsupported "boxed floating arity")
      fun tagResult () code =
        if tagged() then
          (instruction A.lsl (R(X 16),R(X 16),I(1))
             ++ instruction A.add (R(X 16),R(X 16),I(1))) code
        else
          code
      fun size shift code =
        (case (args,res) of
          ([a],[d]) =>
            (readInto fsz a (X 16)
              ++ loadInto(X 16,0,X 16)
              ++ instruction A.lsr (R(X 16),R(X 16),imm (shift))
              ++ tagResult()
              ++ writeInto fsz d (X 16)) code
          | _ => unsupported "table size arity")
      fun index t i scale code =
        let
          val code = A.add (R(X 17),R(X 16),Shifted(X 17,LSL,scale)) :: code
          val code = if tagged() then
              A.asr (R(X 17),R(X 17),I(1)) :: code
            else
              code
        in
          (readInto fsz t (X 16)
           ++ readInto fsz i (X 17)) code
        end
      fun subscript scale opn scalar code =
        (case (args,res) of
          ([t,i],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = if scalar then
                  tagResult() code
                else
                  code
            in
              (index t i scale
               ++ instruction opn (if scale < 2 then R(W 16) else R(X 16),M(X 17,8))) code
            end
          | _ => unsupported "table subscript arity")
      fun update scale opn scalar code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            let
              val code =
                (instruction opn (if scale < 2 then R(W 16) else R(X 16),M(X 17,8))
                 ++ constantInto(1,X 16)
                 ++ writeInto fsz d (X 16)) code
              val code = if scalar andalso tagged() then
                  A.lsr (R(X 16),R(X 16),I(1)) :: code
                else
                  code
            in
              (index t i scale
               ++ stackInto(true,16)
               ++ storeInto(X 17,SP,0)
               ++ readInto (fsz+2) v (X 16)
               ++ loadInto(SP,0,X 17)
               ++ stackInto(false,16)) code
            end
          | _ => unsupported "table update arity")
      (* Numerical representation tuples are (value bits, signed, boxed, tagged).
       * Decode numerical representations before operating, then normalize
       * and encode the result. LR is available as scratch after the prologue. *)
      fun normalize (bits,sgn,_,_) reg code =
        if bits = 64 then
          code
        else
          (if sgn then A.sbfx else A.ubfx) (R(reg),R(reg),I(0),imm (bits)) :: code
      fun getnumAt level (rep as (bits,sgn,box,tag)) a reg code =
        let
          val code = normalize rep reg code
          val code = if tag then
              A.lsr (R(reg),R(reg),I(1)) :: code
            else
              code
          val code = if box then
              loadInto(reg,8,reg) code
            else
              code
        in
          readInto level a reg code
        end
      fun getnum rep a reg code =
        getnumAt fsz rep a reg code
      fun failUnless cc code =
        let
          val ok = localFresh()
        in
          (instruction (branch cc) (L(ok))
           ++ overflow()
           ++ one (Label ok)) code
        end
      fun range (bits,sgn,_,_) code =
        if not sgn then
          code
        else
          if bits = 64 then
            code
          else
            (instruction A.sbfx (R(X 30),R(X 16),I(0),imm (bits))
              ++ instruction A.cmp (R(X 30),R(X 16))
              ++ failUnless EQ) code
      fun putnum (rep as (_,_,box,tag)) buffer d code =
        let
          val code = if box then
              (case buffer of
                SOME rhs =>
                  (stackInto(true,16)
                    ++ storeInto(X 16,SP,0)
                    ++ readInto (fsz+2) rhs (X 17)
                    ++ loadInto(SP,0,X 16)
                    ++ stackInto(false,16)
                    ++ storeInto(X 16,X 17,8)
                    ++ constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_word_boxed false)),X 16)
                    ++ storeInto(X 16,X 17,0)
                    ++ writeInto fsz d (X 17)) code
                | NONE => unsupported "boxed result without buffer")
            else
              writeInto fsz d (X 16) code
          val code = if tag then
              (instruction A.lsl (R(X 16),R(X 16),I(1))
                 ++ instruction A.add (R(X 16),R(X 16),I(1))) code
            else
              code
        in
          normalize rep (X 16) code
        end
      fun numeric opn (rep as (bits,sgn,box,tag)) code =
        let
          val comparison = List.exists(fn x => x = opn) [NumEqual,NumLess,NumLesseq,NumGreater,NumGreatereq]
          val (buffer,operands) = if box andalso not comparison then
                            case args of rhs::xs => (SOME rhs,xs) | _ => unsupported "numeric buffer"
                          else (NONE,args)
          val d = case res of [d] => d | _ => unsupported "numeric result"
          fun cc () = case opn of NumEqual => EQ | NumLess => if sgn then LT else LO
                          | NumLesseq => if sgn then LE else LS | NumGreater => if sgn then GT else HI
                          | _ => if sgn then GE else HS
          fun compareResult () code =
            (case d of
              SS.FLOW_VAR_ATY(_,t,f) =>
                (instruction (branch(cc())) (L(LocalLab t))
                   ++ instruction A.b (L(LocalLab f))) code
              | _ =>
                (instruction A.cset (R(X 16),C(cc()))
                  ++ instruction A.lsl (R(X 16),R(X 16),I(1))
                  ++ instruction A.add (R(X 16),R(X 16),I(1))
                  ++ writeInto fsz d (X 16)) code)
          fun arithmetic inst code =
            let
              val code = if sgn then
                  (failUnless VC
                    ++ range rep) code
                else
                  code
            in
              inst (R(X 16),R(X 16),R(X 17)) :: code
            end
        in
          (case operands of
            [a] =>
              let
                val code =
                  let
                    val code = (range rep
                       ++ putnum rep buffer d) code
                  in
                    (case opn of
                      NumNeg =>
                        (instruction A.negs (R(X 16),R(X 16))
                          ++ failUnless VC) code
                      | NumAbs =>
                        let
                          val done = localFresh()
                        in
                          (instruction A.cmp (R(X 16),I(0))
                           ++ instruction A.b_ge (L(done))
                           ++ instruction A.negs (R(X 16),R(X 16))
                           ++ failUnless VC
                           ++ one (Label done)) code
                        end
                      | _ => unsupported "numeric unary operation")
                  end
              in
                getnum rep a (X 16) code
              end
            | [a,rhs] =>
              let
                val code = if comparison then
                    let
                      val code = compareResult() code
                    in
                      A.cmp (R(X 16),R(X 17)) :: code
                    end
                  else
                    (case opn of
                      NumPlus =>
                        arithmetic(if sgn then A.adds else A.add)
                          (putnum rep buffer d code)
                      | NumMinus =>
                        arithmetic(if sgn then A.subs else A.sub)
                          (putnum rep buffer d code)
                      | NumMul =>
                        let
                          val code = if sgn then
                              (instruction A.cmp (R(X 30),Shifted(X 16,ASR,63))
                                ++ failUnless EQ
                                ++ range rep
                                ++ putnum rep buffer d) code
                            else
                              putnum rep buffer d code
                          val code = A.mul (R(X 16),R(X 16),R(X 17)) :: code
                        in
                          if sgn then
                            A.smulh (R(X 30),R(X 16),R(X 17)) :: code
                          else
                            code
                        end
                      | NumAndb =>
                        (instruction A.and_ (R(X 16),R(X 16),R(X 17))
                           ++ putnum rep buffer d) code
                      | NumOrb =>
                        (instruction A.orr (R(X 16),R(X 16),R(X 17))
                           ++ putnum rep buffer d) code
                      | NumXorb =>
                        (instruction A.eor (R(X 16),R(X 16),R(X 17))
                           ++ putnum rep buffer d) code
                      | _ => unsupported "numeric binary operation")
              in
                (getnum rep a (X 16)
                 ++ stackInto(true,16)
                 ++ storeInto(X 16,SP,0)
                 ++ getnumAt (fsz+2) rep rhs (X 17)
                 ++ loadInto(SP,0,X 16)
                 ++ stackInto(false,16)) code
              end
            | _ => unsupported "numeric operands")
        end
      fun shift arithmeticShift opn (rep as (bits,_,box,_)) code =
        let
          val (buffer,a,rhs,d) = case (box,args,res) of
                          (false,[a,rhs],[d]) => (NONE,a,rhs,d)
                        | (true,[buf,a,rhs],[d]) => (SOME buf,a,rhs,d)
                        | _ => unsupported "shift arity"
          val wide = localFresh()
          val done = localFresh()
          val code =
            (one (Label done)
             ++ putnum rep buffer d) code
          val code = if arithmeticShift then
              A.asr (R(X 16),R(X 16),I(63)) :: code
            else
              constantInto(0,X 16) code
          val code = (instruction A.cmp (R(X 17),imm (bits))
             ++ instruction A.b_hs (L(wide))
             ++ instruction opn (R(X 16),R(X 16),R(X 17))
             ++ instruction A.b (L(done))
             ++ one (Label wide)) code
          val code = if arithmeticShift then
              normalize (bits,true,false,false) (X 16) code
            else
              code
        in
          (getnum rep a (X 16)
           ++ stackInto(true,16)
           ++ storeInto(X 16,SP,0)
           ++ getnumAt (fsz+2) (if tagged() then (63,false,false,true) else (64,false,false,false)) rhs (X 17)
           ++ loadInto(SP,0,X 16)
           ++ stackInto(false,16)) code
        end
      fun convert (src as (bits,sgn,_,_)) (dst as (_,signed,box,_)) extend code =
        let
          val (buffer,a,d) = case (box,args,res) of
                          (false,[a],[d]) => (NONE,a,d)
                        | (true,[buf,a],[d]) => (SOME buf,a,d)
                        | _ => unsupported "conversion arity"
          val code = putnum dst buffer d code
          val code = if signed then
              let
                val code = range dst code
              in
                if not sgn andalso not extend then
                  (instruction A.cmp (R(X 16),I(0))
                    ++ failUnless GE) code
                else
                  code
              end
            else
              code
          val code = if extend then
              normalize (bits,true,false,false) (X 16) code
            else
              code
        in
          getnum src a (X 16) code
        end
      fun realUnary opn code =
        (case (args,res) of
          ([buffer,a],[d]) =>
            let
              val code =
                (storeInto(D 30,X 16,payload())
                 ++ writeInto fsz d (X 16)) code
              val code = if tagged() then
                  (constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17)
                    ++ storeInto(X 17,X 16,0)) code
                else
                  code
            in
              (readInto fsz a (X 16)
               ++ loadInto(X 16,payload(),D 30)
               ++ instruction opn (R(D 30),R(D 30))
               ++ readInto fsz buffer (X 16)) code
            end
          | _ => unsupported "real unary arity")
      fun realCompare cc code =
        (case args of
          [a,rhs] =>
            (readInto fsz a (X 16)
              ++ loadInto(X 16,payload(),D 30)
              ++ readInto fsz rhs (X 16)
              ++ loadInto(X 16,payload(),D 31)
              ++ primitiveInto fsz {name = (case cc of 0 => Less_f64 | 1 => Lesseq_f64 | 2 => Greater_f64 | _ => Greatereq_f64),
                                         args = [SS.PHREG_ATY(D 30),SS.PHREG_ATY(D 31)],res = res}) code
          | _ => unsupported "real comparison arity")
      fun toInt isReal code =
        (case (args,res) of
          ([a],[d]) =>
            let
              val code =
                (instruction A.fcvtzs (R(X 16),R(D 30))
                 ++ tagResult()
                 ++ writeInto fsz d (X 16)) code
            in
              if isReal then
                (readInto fsz a (X 16)
                  ++ loadInto(X 16,payload(),D 30)) code
              else
                readInto fsz a (D 30) code
            end
          | _ => unsupported "float conversion arity")
      fun tableSub scale (rep as (_,_,box,_)) code =
        let
          val (buffer,t,i,d) = case (box,args,res) of
                            (false,[t,i],[d]) => (NONE,t,i,d)
                          | (true,[rhs,t,i],[d]) => (SOME rhs,t,i,d)
                          | _ => unsupported "wide table subscript"
        in
          (index t i scale
           ++ instruction A.ldr (if scale = 2 then R(W 16) else R(X 16),M(X 17,8))
           ++ putnum rep buffer d) code
        end
      fun tableUpdate scale rep code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            (index t i scale
              ++ stackInto(true,16)
              ++ storeInto(X 17,SP,0)
              ++ getnumAt (fsz+2) rep v (X 16)
              ++ loadInto(SP,0,X 17)
              ++ stackInto(false,16)
              ++ instruction A.str (if scale = 2 then R(W 16) else R(X 16),M(X 17,8))
              ++ constantInto(1,X 16)
              ++ writeInto fsz d (X 16)) code
          | _ => unsupported "wide table update")
      fun blockSub boxed code =
        (case (boxed,args,res) of
          (false,[t,i],[d]) =>
            (index t i 3
              ++ loadInto(X 17,8,D 30)
              ++ writeInto fsz d (D 30)) code
          | (true,[rhs,t,i],[d]) =>
            let
              val code =
                (storeInto(D 30,X 16,payload())
                 ++ writeInto fsz d (X 16)) code
              val code = if tagged() then
                  (constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17)
                    ++ storeInto(X 17,X 16,0)) code
                else
                  code
            in
              (index t i 3
               ++ loadInto(X 17,8,D 30)
               ++ readInto fsz rhs (X 16)) code
            end
          | _ => unsupported "float block subscript")
      fun blockUpdate boxed code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            let
              val code =
                (loadInto(SP,0,X 17)
                 ++ stackInto(false,16)
                 ++ storeInto(D 30,X 17,8)
                 ++ constantInto(1,X 16)
                 ++ writeInto fsz d (X 16)) code
              val code = if boxed then
                  (readInto (fsz+2) v (X 16)
                    ++ loadInto(X 16,payload(),D 30)) code
                else
                  readInto (fsz+2) v (D 30) code
            in
              (index t i 3
               ++ stackInto(true,16)
               ++ storeInto(X 17,SP,0)) code
            end
          | _ => unsupported "float block update")
    in
      (case name of
        Plus_int63 => taggedBinary A.adds true code
        | Minus_int63 => taggedBinary A.subs true code
        | Plus_word63 => taggedBinary A.add false code
        | Minus_word63 => taggedBinary A.sub false code
        | Plus_int64ub => checked A.adds code
        | Minus_int64ub => checked A.subs code
        | Plus_real => boxed A.fadd code
        | Minus_real => boxed A.fsub code
        | Mul_real => boxed A.fmul code
        | Div_real => boxed A.fdiv code
        | Plus_f64 => fpBinary A.fadd code
        | Minus_f64 => fpBinary A.fsub code
        | Mul_f64 => fpBinary A.fmul code
        | Div_f64 => fpBinary A.fdiv code
        | Neg_f64 => fpUnary A.fneg code
        | Abs_f64 => fpUnary A.fabs code
        | Sqrt_f64 => fpUnary A.fsqrt code
        | Less_f64 => fpCompare MI code
        | Lesseq_f64 => fpCompare LS code
        | Greater_f64 => fpCompare GT code
        | Greatereq_f64 => fpCompare GE code
        | Int_to_f64 =>
          (case (args,res) of
            ([a],[d]) =>
              let
                val code =
                  (instruction A.scvtf (R(D 30),R(X 16))
                   ++ writeInto fsz d (D 30)) code
                val code = if tagged() then
                    A.asr (R(X 16),R(X 16),I(1)) :: code
                  else
                    code
              in
                readInto fsz a (X 16) code
              end
            | _ => unsupported "int to float arity")
        | Real_to_f64 =>
          (case (args,res) of
            ([a],[d]) =>
              (readInto fsz a (X 16)
                ++ loadInto(X 16,payload(),D 30)
                ++ writeInto fsz d (D 30)) code
            | _ => unsupported "real unboxing arity")
        | F64_to_real =>
          (case (args,res) of
            ([a,rhs],[d]) =>
              let
                val code =
                  (storeInto(D 30,X 16,payload())
                   ++ writeInto fsz d (X 16)) code
                val code = if tagged() then
                    (constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17)
                      ++ storeInto(X 17,X 16,0)) code
                  else
                    code
              in
                (readInto fsz a (X 16)
                 ++ readInto fsz rhs (D 30)) code
              end
            | _ => unsupported "real boxing arity")
        | Get_ctx =>
          (case res of
            [d] =>
              writeInto fsz d (X 28) code
            | _ => unsupported "context arity")
        | Exn_ptr =>
          (case res of
            [d] =>
              (loadInto(X 28,8,X 16)
                ++ writeInto fsz d (X 16)) code
            | _ => unsupported "exception pointer arity")
        | Fresh_exname =>
          let
            val code = (case res of
                [d] =>
                  writeInto fsz d (X 16) code
                | _ => unsupported "exception name arity")
            val code = if parallel() then
                let
                  val retry = localFresh()
                            (* Incoming LR is already saved in the ML frame; w30 is scratch. *)
                in
                  (one (Label retry)
                     ++ instruction A.ldaxr (R(X 16),M(X 17,0))
                     ++ instruction A.add (R(X 16),R(X 16),I(1))
                     ++ instruction A.stlxr (R(W 30),R(X 16),M(X 17,0))
                     ++ instruction A.cbnz (R(W 30),L(retry))) code
                end
              else
                (loadInto(X 17,0,X 16)
                  ++ instruction A.add (R(X 16),R(X 16),I(1))
                  ++ storeInto(X 16,X 17,0)) code
          in
            addressInto(NameLab "exnameCounter",X 17) code
          end
        | Equal_ptr => compare EQ code
        | Plus_word64ub => binary true A.add code
        | Minus_word64ub => binary true A.sub code
        | Mul_word64ub => binary false A.mul code
        | Andb_word64ub => binary false A.and_ code
        | Orb_word64ub => binary false A.orr code
        | Xorb_word64ub => binary false A.eor code
        | Equal_word64ub => compare EQ code
        | Less_word64ub => compare LO code
        | Lesseq_word64ub => compare LS code
        | Greater_word64ub => compare HI code
        | Greatereq_word64ub => compare HS code
        | Equal_int32ub => numeric NumEqual (32,true,false,false) code
        | Equal_int63 => compare EQ code
        | Equal_word63 => compare EQ code
        | Less_int63 => compare LT code
        | Lesseq_int63 => compare LE code
        | Greater_int63 => compare GT code
        | Greatereq_int63 => compare GE code
        | Less_word63 => compare LO code
        | Lesseq_word63 => compare LS code
        | Greater_word63 => compare HI code
        | Greatereq_word63 => compare HS code
        | Equal_int64ub => compare EQ code
        | Less_int64ub => compare LT code
        | Lesseq_int64ub => compare LE code
        | Greater_int64ub => compare GT code
        | Greatereq_int64ub => compare GE code
        | Bytetable_size => size 6 code
        | Table_size => size 6 code
        | Blockf64_size => size 9 code
        | Bytetable_sub => subscript 0 A.ldrb true code
        | Bytetable_sub_word16 => subscript 1 A.ldrh true code
        | Word_sub0 => subscript 3 A.ldr false code
        | Bytetable_update => update 0 A.strb true code
        | Bytetable_update_word16 => update 1 A.strh true code
        | Word_update0 => update 3 A.str false code
        | Equal_int31 => numeric NumEqual (31,true,false,true) code
        | Equal_int32b => numeric NumEqual (32,true,true,false) code
        | Equal_char => numeric NumEqual (8,false,false,tagged()) code
        | Equal_word8 => numeric NumEqual (8,false,false,tagged()) code
        | Equal_word31 => numeric NumEqual (31,false,false,true) code
        | Equal_word32ub => numeric NumEqual (32,false,false,false) code
        | Equal_word32b => numeric NumEqual (32,false,true,false) code
        | Equal_int64b => numeric NumEqual (64,true,true,false) code
        | Equal_word64b => numeric NumEqual (64,false,true,false) code
        | Less_int31 => numeric NumLess (31,true,false,true) code
        | Less_int32ub => numeric NumLess (32,true,false,false) code
        | Less_int32b => numeric NumLess (32,true,true,false) code
        | Less_char => numeric NumLess (8,false,false,tagged()) code
        | Less_word8 => numeric NumLess (8,false,false,tagged()) code
        | Less_word31 => numeric NumLess (31,false,false,true) code
        | Less_word32ub => numeric NumLess (32,false,false,false) code
        | Less_word32b => numeric NumLess (32,false,true,false) code
        | Less_int64b => numeric NumLess (64,true,true,false) code
        | Less_word64b => numeric NumLess (64,false,true,false) code
        | Lesseq_int31 => numeric NumLesseq (31,true,false,true) code
        | Lesseq_int32ub => numeric NumLesseq (32,true,false,false) code
        | Lesseq_int32b => numeric NumLesseq (32,true,true,false) code
        | Lesseq_char => numeric NumLesseq (8,false,false,tagged()) code
        | Lesseq_word8 => numeric NumLesseq (8,false,false,tagged()) code
        | Lesseq_word31 => numeric NumLesseq (31,false,false,true) code
        | Lesseq_word32ub => numeric NumLesseq (32,false,false,false) code
        | Lesseq_word32b => numeric NumLesseq (32,false,true,false) code
        | Lesseq_int64b => numeric NumLesseq (64,true,true,false) code
        | Lesseq_word64b => numeric NumLesseq (64,false,true,false) code
        | Greater_int31 => numeric NumGreater (31,true,false,true) code
        | Greater_int32ub => numeric NumGreater (32,true,false,false) code
        | Greater_int32b => numeric NumGreater (32,true,true,false) code
        | Greater_char => numeric NumGreater (8,false,false,tagged()) code
        | Greater_word8 => numeric NumGreater (8,false,false,tagged()) code
        | Greater_word31 => numeric NumGreater (31,false,false,true) code
        | Greater_word32ub => numeric NumGreater (32,false,false,false) code
        | Greater_word32b => numeric NumGreater (32,false,true,false) code
        | Greater_int64b => numeric NumGreater (64,true,true,false) code
        | Greater_word64b => numeric NumGreater (64,false,true,false) code
        | Greatereq_int31 => numeric NumGreatereq (31,true,false,true) code
        | Greatereq_int32ub => numeric NumGreatereq (32,true,false,false) code
        | Greatereq_int32b => numeric NumGreatereq (32,true,true,false) code
        | Greatereq_char => numeric NumGreatereq (8,false,false,tagged()) code
        | Greatereq_word8 => numeric NumGreatereq (8,false,false,tagged()) code
        | Greatereq_word31 => numeric NumGreatereq (31,false,false,true) code
        | Greatereq_word32ub => numeric NumGreatereq (32,false,false,false) code
        | Greatereq_word32b => numeric NumGreatereq (32,false,true,false) code
        | Greatereq_int64b => numeric NumGreatereq (64,true,true,false) code
        | Greatereq_word64b => numeric NumGreatereq (64,false,true,false) code
        | Plus_int31 => numeric NumPlus (31,true,false,true) code
        | Plus_int32ub => numeric NumPlus (32,true,false,false) code
        | Plus_int32b => numeric NumPlus (32,true,true,false) code
        | Plus_word31 => numeric NumPlus (31,false,false,true) code
        | Plus_word32ub => numeric NumPlus (32,false,false,false) code
        | Plus_word32b => numeric NumPlus (32,false,true,false) code
        | Plus_int64b => numeric NumPlus (64,true,true,false) code
        | Plus_word64b => numeric NumPlus (64,false,true,false) code
        | Minus_int31 => numeric NumMinus (31,true,false,true) code
        | Minus_int32ub => numeric NumMinus (32,true,false,false) code
        | Minus_int32b => numeric NumMinus (32,true,true,false) code
        | Minus_word31 => numeric NumMinus (31,false,false,true) code
        | Minus_word32ub => numeric NumMinus (32,false,false,false) code
        | Minus_word32b => numeric NumMinus (32,false,true,false) code
        | Minus_int64b => numeric NumMinus (64,true,true,false) code
        | Minus_word64b => numeric NumMinus (64,false,true,false) code
        | Mul_int31 => numeric NumMul (31,true,false,true) code
        | Mul_int32ub => numeric NumMul (32,true,false,false) code
        | Mul_int32b => numeric NumMul (32,true,true,false) code
        | Mul_word31 => numeric NumMul (31,false,false,true) code
        | Mul_word32ub => numeric NumMul (32,false,false,false) code
        | Mul_word32b => numeric NumMul (32,false,true,false) code
        | Mul_int63 => numeric NumMul (63,true,false,true) code
        | Mul_int64ub => numeric NumMul (64,true,false,false) code
        | Mul_int64b => numeric NumMul (64,true,true,false) code
        | Mul_word63 => numeric NumMul (63,false,false,true) code
        | Mul_word64b => numeric NumMul (64,false,true,false) code
        | Neg_int31 => numeric NumNeg (31,true,false,true) code
        | Neg_int32ub => numeric NumNeg (32,true,false,false) code
        | Neg_int32b => numeric NumNeg (32,true,true,false) code
        | Neg_int63 => numeric NumNeg (63,true,false,true) code
        | Neg_int64ub => numeric NumNeg (64,true,false,false) code
        | Neg_int64b => numeric NumNeg (64,true,true,false) code
        | Abs_int31 => numeric NumAbs (31,true,false,true) code
        | Abs_int32ub => numeric NumAbs (32,true,false,false) code
        | Abs_int32b => numeric NumAbs (32,true,true,false) code
        | Abs_int63 => numeric NumAbs (63,true,false,true) code
        | Abs_int64ub => numeric NumAbs (64,true,false,false) code
        | Abs_int64b => numeric NumAbs (64,true,true,false) code
        | Andb_word31 => numeric NumAndb (31,false,false,true) code
        | Andb_word32ub => numeric NumAndb (32,false,false,false) code
        | Andb_word32b => numeric NumAndb (32,false,true,false) code
        | Andb_word63 => numeric NumAndb (63,false,false,true) code
        | Andb_word64b => numeric NumAndb (64,false,true,false) code
        | Orb_word31 => numeric NumOrb (31,false,false,true) code
        | Orb_word32ub => numeric NumOrb (32,false,false,false) code
        | Orb_word32b => numeric NumOrb (32,false,true,false) code
        | Orb_word63 => numeric NumOrb (63,false,false,true) code
        | Orb_word64b => numeric NumOrb (64,false,true,false) code
        | Xorb_word31 => numeric NumXorb (31,false,false,true) code
        | Xorb_word32ub => numeric NumXorb (32,false,false,false) code
        | Xorb_word32b => numeric NumXorb (32,false,true,false) code
        | Xorb_word63 => numeric NumXorb (63,false,false,true) code
        | Xorb_word64b => numeric NumXorb (64,false,true,false) code
        | Neg_real => realUnary A.fneg code
        | Abs_real => realUnary A.fabs code
        | Less_real => realCompare 0 code
        | Lesseq_real => realCompare 1 code
        | Greater_real => realCompare 2 code
        | Greatereq_real => realCompare 3 code
        | Max_f64 => fpBinary A.fmax code
        | Min_f64 => fpBinary A.fmin code
        | F64_to_int => toInt false code
        | Real_to_int => toInt true code
        | Is_null =>
          (case args of
            [a] =>
              primitiveInto fsz {name = Equal_ptr,args = [a,integer 0],res = res} code
            | _ => unsupported "null arity")
        | Shift_left_word31 => shift false A.lsl (31,false,false,true) code
        | Shift_left_word32ub => shift false A.lsl (32,false,false,false) code
        | Shift_left_word32b => shift false A.lsl (32,false,true,false) code
        | Shift_left_word63 => shift false A.lsl (63,false,false,true) code
        | Shift_left_word64ub => shift false A.lsl (64,false,false,false) code
        | Shift_left_word64b => shift false A.lsl (64,false,true,false) code
        | Shift_right_signed_word31 => shift true A.asr (31,false,false,true) code
        | Shift_right_signed_word32ub => shift true A.asr (32,false,false,false) code
        | Shift_right_signed_word32b => shift true A.asr (32,false,true,false) code
        | Shift_right_signed_word63 => shift true A.asr (63,false,false,true) code
        | Shift_right_signed_word64ub => shift true A.asr (64,false,false,false) code
        | Shift_right_signed_word64b => shift true A.asr (64,false,true,false) code
        | Shift_right_unsigned_word31 => shift false A.lsr (31,false,false,true) code
        | Shift_right_unsigned_word32ub => shift false A.lsr (32,false,false,false) code
        | Shift_right_unsigned_word32b => shift false A.lsr (32,false,true,false) code
        | Shift_right_unsigned_word63 => shift false A.lsr (63,false,false,true) code
        | Shift_right_unsigned_word64ub => shift false A.lsr (64,false,false,false) code
        | Shift_right_unsigned_word64b => shift false A.lsr (64,false,true,false) code
        | Int31_to_int32b => convert (31,true,false,true) (32,true,true,false) false code
        | Int31_to_int32ub => convert (31,true,false,true) (32,true,false,false) false code
        | Int32b_to_int31 => convert (32,true,true,false) (31,true,false,true) false code
        | Int32b_to_word32b => convert (32,true,true,false) (32,false,true,false) false code
        | Int32ub_to_int31 => convert (32,true,false,false) (31,true,false,true) false code
        | Int31_to_int64b => convert (31,true,false,true) (64,true,true,false) false code
        | Int31_to_int64ub => convert (31,true,false,true) (64,true,false,false) false code
        | Int64b_to_int31 => convert (64,true,true,false) (31,true,false,true) false code
        | Word31_to_word32b => convert (31,false,false,true) (32,false,true,false) false code
        | Word31_to_word32ub => convert (31,false,false,true) (32,false,false,false) false code
        | Word32b_to_word31 => convert (32,false,true,false) (31,false,false,true) false code
        | Word32ub_to_word31 => convert (32,false,false,false) (31,false,false,true) false code
        | Word31_to_word32ub_X => convert (31,false,false,true) (32,false,false,false) true code
        | Word31_to_word32b_X => convert (31,false,false,true) (32,false,true,false) true code
        | Word32b_to_int32b => convert (32,false,true,false) (32,true,true,false) false code
        | Word32b_to_int32b_X => convert (32,false,true,false) (32,true,true,false) true code
        | Word32ub_to_int32ub => convert (32,false,false,false) (32,true,false,false) false code
        | Word31_to_int31 => convert (31,false,false,true) (31,true,false,true) false code
        | Word32b_to_int31 => convert (32,false,true,false) (31,true,false,true) false code
        | Int32b_to_word31 => convert (32,true,true,false) (31,false,false,true) false code
        | Word32b_to_int31_X => convert (32,false,true,false) (31,true,false,true) true code
        | Word64ub_to_int32ub => convert (64,false,false,false) (32,true,false,false) false code
        | Word32ub_to_word64ub => convert (32,false,false,false) (64,false,false,false) false code
        | Word64ub_to_word32ub => convert (64,false,false,false) (32,false,false,false) false code
        | Word64ub_to_int64ub => convert (64,false,false,false) (64,true,false,false) false code
        | Word64ub_to_int64ub_X => convert (64,false,false,false) (64,true,false,false) true code
        | Word31_to_word64b => convert (31,false,false,true) (64,false,true,false) false code
        | Word31_to_word64b_X => convert (31,false,false,true) (64,false,true,false) true code
        | Word64b_to_int31 => convert (64,false,true,false) (31,true,false,true) false code
        | Word64b_to_int64b_X => convert (64,false,true,false) (64,true,true,false) true code
        | Word64b_to_int64b => convert (64,false,true,false) (64,true,true,false) false code
        | Word32b_to_word64b => convert (32,false,true,false) (64,false,true,false) false code
        | Word32b_to_word64b_X => convert (32,false,true,false) (64,false,true,false) true code
        | Word64b_to_word32b => convert (64,false,true,false) (32,false,true,false) false code
        | Word64b_to_int31_X => convert (64,false,true,false) (31,true,false,true) true code
        | Int32b_to_int64b => convert (32,true,true,false) (64,true,true,false) false code
        | Int32ub_to_int64ub => convert (32,true,false,false) (64,true,false,false) false code
        | Int64b_to_word64b => convert (64,true,true,false) (64,false,true,false) false code
        | Int64ub_to_word64ub => convert (64,true,false,false) (64,false,false,false) false code
        | Int64ub_to_int32ub => convert (64,true,false,false) (32,true,false,false) false code
        | Int63_to_int64b => convert (63,true,false,true) (64,true,true,false) false code
        | Int64b_to_int63 => convert (64,true,true,false) (63,true,false,true) false code
        | Word32b_to_word63 => convert (32,false,true,false) (63,false,false,true) false code
        | Word63_to_word32b => convert (63,false,false,true) (32,false,true,false) false code
        | Word63_to_word31 => convert (63,false,false,true) (31,false,false,true) false code
        | Word31_to_word63 => convert (31,false,false,true) (63,false,false,true) false code
        | Word31_to_word63_X => convert (31,false,false,true) (63,false,false,true) true code
        | Word63_to_word64b => convert (63,false,false,true) (64,false,true,false) false code
        | Word63_to_word64b_X => convert (63,false,false,true) (64,false,true,false) true code
        | Word64b_to_word63 => convert (64,false,true,false) (63,false,false,true) false code
        | Word64ub_to_word63 => convert (64,false,false,false) (63,false,false,true) false code
        | Int31_to_int63 => convert (31,true,false,true) (63,true,false,true) false code
        | Int63_to_int31 => convert (63,true,false,true) (31,true,false,true) false code
        | Int32b_to_int63 => convert (32,true,true,false) (63,true,false,true) false code
        | Int63_to_int32b => convert (63,true,false,true) (32,true,true,false) false code
        | Word32b_to_int63 => convert (32,false,true,false) (63,true,false,true) false code
        | Word32b_to_int63_X => convert (32,false,true,false) (63,true,false,true) true code
        | Word64b_to_word31 => convert (64,false,true,false) (31,false,false,true) false code
        | Word64b_to_int63 => convert (64,false,true,false) (63,true,false,true) false code
        | Word64b_to_int63_X => convert (64,false,true,false) (63,true,false,true) true code
        | Int63_to_int64ub => convert (63,true,false,true) (64,true,false,false) false code
        | Int64ub_to_int63 => convert (64,true,false,false) (63,true,false,true) false code
        | Word63_to_word64ub => convert (63,false,false,true) (64,false,false,false) false code
        | Word63_to_word64ub_X => convert (63,false,false,true) (64,false,false,false) true code
        | Word64ub_to_word31 => convert (64,false,false,false) (31,false,false,true) false code
        | Int64ub_to_int31 => convert (64,true,false,false) (31,true,false,true) false code
        | Word31_to_word64ub => convert (31,false,false,true) (64,false,false,false) false code
        | Word31_to_word64ub_X => convert (31,false,false,true) (64,false,false,false) true code
        | Word32ub_to_int64ub => convert (32,false,false,false) (64,true,false,false) false code
        | Word32ub_to_int64ub_X => convert (32,false,false,false) (64,true,false,false) true code
        | Word32ub_to_word64ub_X => convert (32,false,false,false) (64,false,false,false) true code
        | Blockf64_sub_real => blockSub true code
        | Blockf64_sub_f64 => blockSub false code
        | Blockf64_update_real => blockUpdate true code
        | Blockf64_update_f64 => blockUpdate false code
        | Bytetable_sub_word31 => tableSub 2 (if tagged() then 31 else 32,false,false,tagged()) code
        | Bytetable_update_word31 =>
          tableUpdate 2 (if tagged() then 31 else 32,false,false,tagged()) code
        | Bytetable_sub_word32ub => tableSub 2 (32,false,false,false) code
        | Bytetable_update_word32ub => tableUpdate 2 (32,false,false,false) code
        | Bytetable_sub_word32b => tableSub 2 (32,false,true,false) code
        | Bytetable_update_word32b => tableUpdate 2 (32,false,true,false) code
        | Bytetable_sub_word63 => tableSub 3 (if tagged() then 63 else 64,false,false,tagged()) code
        | Bytetable_update_word63 =>
          tableUpdate 3 (if tagged() then 63 else 64,false,false,tagged()) code
        | Bytetable_sub_word64ub => tableSub 3 (64,false,false,false) code
        | Bytetable_update_word64ub => tableUpdate 3 (64,false,false,false) code
        | Bytetable_sub_word64b => tableSub 3 (64,false,true,false) code
        | Bytetable_update_word64b => tableUpdate 3 (64,false,true,false) code
        | _ => unsupported ("primitive " ^ PrimName.pp_prim name))
    end
  val dataLabels : label list ref = ref []
  fun dataLabel l = if List.exists (fn x => AddressLabels.eq(x,l)) (!dataLabels) then ()
                    else dataLabels := l :: !dataLabels
  fun epilogueInto fsz code =
    (loadInto (SP,8*(fsz+even(!currentArgs)),X 29)
       ++ loadInto (SP,8*(fsz+even(!currentArgs)+1),X 30)
       ++ stackInto (false,8*(fsz+even(!currentArgs)+2))
       (* GC calls materialise x30 and branch without a hardware call.
        * Pair them with an ordinary indirect branch, not a return-stack pop. *)
       ++ one (if gc() then A.br (R(X 30)) else A.ret)) code
  (* Physical-register liveness is separate from GC root liveness: raw words,
   * region pointers and unboxed doubles all need preservation here. Only
   * C-clobbered registers can require a save at a region helper call. *)
  fun memberReg r rs = List.exists (fn s => r = s) rs
  fun unionRegs (rs,ss) = foldl (fn (r,ss) => if memberReg r ss then ss else r::ss) ss rs
  fun atyRegs (SS.PHREG_ATY r) = if memberReg r savedRegs then [r] else []
    | atyRegs _ = []
  val functionRegs : A.reg list ref = ref []
  val conservativeRegionCalls = ref false
  fun regionCallInto live =
    internalCallLiveInto (if !conservativeRegionCalls then savedRegs else live)
  val registerLvars = map (fn lv => (A.RI.lv_to_reg lv,lv)) (A.RI.all_regs @ A.RI.f64_phregs)
  fun registerLvar r = #2(valOf(List.find (fn (s,_) => r = s) registerLvars))
  fun switchParts (LS.SWITCH(a,cases,default)) = (a,map #2 cases,default)
  fun liveStmts statements live = foldr (fn (ls,live) => liveStmt ls live) live statements
  and liveSwitch (a,cases,default) live =
    unionRegs(atyRegs a,foldl (fn (body,rs) => unionRegs(liveStmts body live,rs))
      (liveStmts default live) cases)
  and liveStmt ls live =
    case ls of
      LS.SCOPE {scope,...} => liveStmts scope live
    | LS.LETREGION {body,...} => liveStmts body live
    (* Exception transfers are not ordinary fall-through edges. Keep the
     * blanket set before and throughout handlers until those edges are modelled. *)
    | LS.HANDLE _ => savedRegs
    | LS.SWITCH_I {switch,...} => liveSwitch (switchParts switch) live
    | LS.SWITCH_W {switch,...} => liveSwitch (switchParts switch) live
    | LS.SWITCH_C switch => liveSwitch (switchParts switch) live
    | LS.SWITCH_S switch => liveSwitch (switchParts switch) live
    | LS.SWITCH_E switch => liveSwitch (switchParts switch) live
    | _ =>
        let
          val flow = ref false
          fun atom a =
            case a of
              SS.PHREG_ATY r => if memberReg r savedRegs then LS.PHREG(registerLvar r) else LS.UNIT
            | SS.FLOW_VAR_ATY _ => (flow := true; LS.UNIT)
            | _ => LS.UNIT
          val mapped = hd(LS.map_lss atom (fn x => x) (fn x => x) [ls])
          val (defs,uses) = LS.def_use_var_ls mapped
          val defs = map A.RI.lv_to_reg defs
          val uses = map A.RI.lv_to_reg uses
          val incoming = unionRegs(uses,List.filter (fn r => not(memberReg r defs)) live)
        in
          (* Flow-producing operations jump directly to a later switch arm.
           * Include every register used in this function on that edge, so a
           * skipped definition cannot incorrectly kill a branch's input. *)
          if !flow then unionRegs(!functionRegs,incoming) else incoming
        end
  (* Build statements right-to-left onto an explicit code suffix. Function
   * context is set by topInto and remains fixed throughout this traversal.
   * Fresh labels and metadata may be registered in a different order, but
   * each label is bound before emission and its frame association stays paired. *)
  fun stmtsInto fsz live statements code = #1(stmtsLiveInto fsz live statements code)
  and stmtsLiveInto fsz live statements code =
    foldr (fn (ls,(code,live)) => stmtLiveInto fsz live ls code) (code,live) statements
  and stmtLiveInto fsz live ls code =
    case ls of
      LS.SCOPE {scope,...} =>
        stmtsLiveInto fsz live scope code
    | LS.LETREGION {rhos,body} =>
        let
          fun release (((_,sz),_),code) =
            case sz of
              LS.INF => regionCallInto live fsz "deallocateRegion" [SS.PHREG_ATY(X 28)] code
            | LS.WORDS n =>
                if n = 0 orelse not(profiling()) then code
                else regionCallInto live fsz "deallocRegionFiniteProfiling" [] code
          val code = foldr release code (rev rhos)
          val (code,entryLive) = stmtsLiveInto fsz live body code
          fun enter (((place,sz),off),code) =
            case sz of
              LS.INF => regionCallInto entryLive fsz (regionAllocator place)
                [SS.PHREG_ATY(X 28),SS.REG_F_ATY off,integer(regionPolicy false place)] code
            | LS.WORDS n =>
                if n = 0 orelse not(profiling()) then code
                else regionCallInto entryLive fsz "allocRegionFiniteProfiling"
                  [SS.REG_F_ATY(off+BackendInfo.objectDescSizeP+BackendInfo.finiteRegionDescSizeP),
                   integer(Effect.key_of_eps_or_rho place),integer n] code
        in
          (foldr enter code rhos,entryLive)
        end
    | _ => (stmtInto fsz live ls code,liveStmt ls live)
  and stmtInto fsz live ls code =
    case ls of
      LS.ASSIGN {pat = SS.FLOW_VAR_ATY(_,t,f),bind = LS.CON0{con,...}} =>
        A.b (L(LocalLab(if Con.eq(con,Con.con_TRUE) then t else f))) :: code
    | LS.ASSIGN {pat,bind = LS.ATOM{aty}} => assignInto fsz aty pat code
    | LS.ASSIGN {pat,bind = LS.LOAD l} =>
        (addressInto(DatLab l,X 16)
           ++ loadInto(X 16,0,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.STORE(aty,l)} =>
        (dataLabel l;
          (readInto fsz aty (X 16)
            ++ addressInto(DatLab l,X 17)
            ++ storeInto(X 16,X 17,0)
            ++ constantInto(1,X 16)
            ++ writeInto fsz pat (X 16)) code)
    | LS.ASSIGN {pat,bind = LS.REAL value} =>
        (addressInto(realData value,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.F64 value} =>
        (addressInto(static [Directive(Double (String.translate(fn #"~" => "-" | c => String.str c) value))],X 16)
           ++ loadInto(X 16,0,D 30)
           ++ writeInto fsz pat (D 30)) code
    | LS.ASSIGN {pat,bind = LS.STRING value} =>
        (addressInto(stringData value,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.RECORD{elems = [],...}} =>
        (constantInto(1,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.BLOCKF64{elems = [],...}} =>
        (constantInto(1,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.BLOCKF64{elems,alloc,tag}} =>
        recordInto fsz pat alloc [Constant(IntInf.fromInt(Word.toInt tag))] elems code
    | LS.ASSIGN {pat,bind = LS.SCRATCHMEM{bytes,alloc,tag}} =>
        if bytes = 0 then
          (constantInto(1,X 16)
            ++ writeInto fsz pat (X 16)) code
        else
          (allocateInto fsz alloc (1+(bytes+7) div 8)
            ++ constantInto(IntInf.fromInt(Word.toInt tag),X 17)
            ++ storeInto(X 17,X 16,0)
            ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.RECORD{elems,alloc,tag,maybeuntag}} =>
        recordWithUntagInto maybeuntag fsz pat alloc (header tag []) elems code
    | LS.ASSIGN {pat,bind = LS.CLOS_RECORD{label,elems = elems as (_,_,rhos),alloc,f64_vars}} =>
        recordInto fsz pat alloc (header(BackendInfo.tag_clos(false,1+length(LS.smash_free elems),1+length rhos+f64_vars)) [Address(MLFunLab label)]) (LS.smash_free elems) code
    | LS.ASSIGN {pat,bind = LS.SCLOS_RECORD{elems = elems as (_,_,rhos),alloc,f64_vars}} =>
        recordInto fsz pat alloc (header(BackendInfo.tag_sclos(false,length(LS.smash_free elems),length rhos+f64_vars)) []) (LS.smash_free elems) code
    | LS.ASSIGN {pat,bind = LS.SELECT(i,a)} =>
        selectInto fsz a (8*i+payload()) pat code
    | LS.ASSIGN {pat,bind = LS.DEREF{aty}} =>
        selectInto fsz aty (payload()) pat code
    | LS.ASSIGN {pat,bind = LS.REF(alloc,a)} =>
        recordWithUntagInto true fsz pat alloc (header(BackendInfo.tag_ref false) []) [a] code
    | LS.ASSIGN {pat,bind = LS.ASSIGNREF(_,a,rhs)} =>
        (stackInto(true,16)
           ++ readInto (fsz+2) a (X 16)
           ++ storeInto(X 16,SP,0)
           ++ readInto (fsz+2) rhs (X 16)
           ++ loadInto(SP,0,X 17)
           ++ storeInto(X 16,X 17,payload())
           ++ stackInto(false,16)
           ++ constantInto(1,X 16)
           ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.PASS_PTR_TO_MEM(alloc,n,untag)} =>
        (allocateInRegionInto fsz alloc
            (if untag andalso tagged() andalso not(tagPairs()) then n-1 else n)
            (untag andalso tagged() andalso not(tagPairs()))
          ++ writeInto fsz pat (X 16)) code
    | LS.ASSIGN {pat,bind = LS.PASS_PTR_TO_RHO{sma}} =>
        let
          val (a,mode) = regionArg sma
          val code = writeInto fsz pat (X 16) code
          val code = (case mode of
              0 =>
                A.and_ (R(X 16),R(X 16),I(~3)) :: code
              | 2 =>
                A.orr (R(X 16),R(X 16),I(2)) :: code
              | _ => code)
        in
          readInto fsz a (X 16) code
        end
    | LS.ASSIGN {pat,bind = LS.CON0{con,con_kind,aux_regions,alloc}} =>
        let
          fun reset () code =
            stmtsInto fsz live [LS.RESET_REGIONS{force = false,regions_for_resetting = aux_regions}] code
          fun value n code =
            let
              val code =
                (constantInto(n,X 16)
                 ++ writeInto fsz pat (X 16)) code
            in
              reset() code
            end
        in
          (case con_kind of
            LS.ENUM i =>
              value(IntInf.fromInt(if tagged() orelse Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 2*i+1 else i)) code
            | LS.UNBOXED i =>
              value(IntInf.fromInt(4*i+3)) code
            | LS.UNBOXED_HIGH i =>
              value(IntInf.fromInt i * 281474976710656 + (if tagged() then 1 else 0)) code
            | LS.BOXED i =>
              let
                val code =
                  recordInto fsz pat alloc [Constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_con0(false,i))))] [] code
              in
                reset() code
              end)
        end
    | LS.ASSIGN {pat,bind = LS.CON1{con_kind,alloc,arg,...}} =>
        (case con_kind of
          LS.BOXED i =>
            recordInto fsz pat alloc
                       [Constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_con1(false,i))))] [arg] code
          | LS.UNBOXED i =>
            (readInto fsz arg (X 16)
              ++ constantInto(IntInf.fromInt i,X 17)
              ++ instruction A.orr (R(X 16),R(X 16),R(X 17))
              ++ writeInto fsz pat (X 16)) code
          | LS.UNBOXED_HIGH i =>
            (readInto fsz arg (X 16)
              ++ constantInto(IntInf.fromInt i*281474976710656,X 17)
              ++ instruction A.orr (R(X 16),R(X 16),R(X 17))
              ++ writeInto fsz pat (X 16)) code
          | _ => unsupported "unary enumeration")
    | LS.ASSIGN {pat,bind = LS.DECON{con_kind,con_aty,...}} =>
        (case con_kind of
           LS.BOXED _ => selectInto fsz con_aty 8 pat code
         | LS.UNBOXED 0 => assignInto fsz con_aty pat code
         | _ =>
             (readInto fsz con_aty (X 16)
              ++ (case con_kind of
                    LS.UNBOXED _ => instruction A.and_ (R(X 16),R(X 16),I(~4))
                  | LS.UNBOXED_HIGH _ => instruction A.and_ (R(X 16),R(X 16),I(0xffffffffffff))
                  | _ => unsupported "enumeration deconstruction")
              ++ writeInto fsz pat (X 16)) code)
    | LS.HANDLE {default,handl = (handl,closure),handl_return = (returned,result,bv),offset} =>
        let
          val returnLab = localFresh()
          val join = localFresh()
          val off = slot fsz offset
          val previous = !conservativeRegionCalls
          val () = conservativeRegionCalls := true
          val code = (stmtsInto fsz live handl
           ++ addressInto (returnLab,X 16)
           ++ storeInto (X 16,SP,off)
           ++ readInto fsz closure (X 16)
           ++ storeInto (X 16,SP,off+8)
           ++ loadInto (X 28,8,X 16)
           ++ storeInto (X 16,SP,off+16)
           ++ moveInto (SP,X 16)
           ++ storeInto (X 16,SP,off+24)
           ++ storeInto (X 29,SP,off+32)
           ++ loadInto (X 28,0,X 16)
           ++ storeInto (X 16,SP,off+40)
           ++ addOffsetInto (SP,off,X 16)
           ++ storeInto (X 16,X 28,8)
           ++ stmtsInto fsz live default
           ++ loadInto (SP,off+16,X 16)
           ++ storeInto (X 16,X 28,8)
           ++ instruction A.b (L(join))
           ++ continuationInto returnLab bv
           ++ writeInto fsz result (X 0)
           ++ stmtsInto fsz live returned
           ++ one (Label join)) code
          val () = conservativeRegionCalls := previous
        in code
        end
    | LS.RAISE {arg,...} =>
        (argumentsInto fsz [SS.PHREG_ATY(X 28),arg]
           ++ instruction A.b (L(NameLab "raise_exn"))) code
    | LS.FLUSH (aty,off) =>
        (readInto fsz aty (X 16)
           ++ storeInto(X 16,SP,slot fsz off)) code
    | LS.FETCH (aty,off) =>
        (loadInto(SP,slot fsz off,X 16)
           ++ writeInto fsz aty (X 16)) code
    | LS.PRIM p =>
        primitiveInto fsz p code
    | LS.CCALL {name = "spawnone",args = [arg],rhos_for_result = [],res = [res]} =>
        let
          val () = if parallel() then () else unsupported "spawnone without -par"
          val entry = localFresh()
                      (* thread_init returns ThreadInfo*. Its leading fields are the
                       * closure and context, checked by Runtime/Layout.c. *)
          val () = addStatic
            (
              let
                val code = (instruction A.blr (R(X 17))
                   ++ instruction A.bl (L(NameLab "thread_exit"))
                   ++ instruction A.brk (I(0))) []
              in
                (one (Directive(Text))
                 ++ one (Directive(Align 2))
                 ++ one (Label entry)
                 ++ saveCInto()
                 ++ instruction A.bl (L(NameLab "thread_init"))
                 ++ addOffsetInto(X 0,8,X 28)
                 ++ loadInto(X 0,0,X 0)
                 ++ loadInto(X 0,0,X 17)
                 ++ constantInto(1,X 1)
                 ++ stackInto(true,16)) code
              end)
        in
          (stackInto(true,16)
           ++ readInto (fsz+2) arg (X 16)
           ++ storeInto(X 16,SP,0)
           ++ addressInto(entry,X 0)
           ++ loadInto(SP,0,X 1)
           ++ instruction A.bl (L(NameLab "thread_create"))
           ++ stackInto(false,16)
           ++ writeInto fsz res (X 0)) code
        end
    | LS.CCALL {name,args,rhos_for_result,res} =>
        if length res > 1 then unsupported "multiple C results"
        else (foreignCallInto fsz name (rhos_for_result @ args) (fn _ => fn code => code)
           ++ resultsInto fsz res) code
    | LS.CCALL_AUTO c =>
        autoCallInto fsz c code
    | LS.EXPORT{name,clos_lab,arg = (aty,ft1,ft2)} =>
        let
          val () = if ft1 = LS.Int andalso ft2 = LS.Int then () else unsupported "export other than int -> int"
          val returnLab = localFresh()
          val ctx = DatLab(AddressLabels.new_named "arm64_export_ctx")
          val textValue = stringData name
          val () = dataLabel clos_lab
          val () = addStatic
            (
              let
                val code = A.ret ::
                  []
                val code =
                  (addressInto(DatLab clos_lab,X 0)
                   ++ loadInto(X 0,0,X 0)
                   ++ loadInto(X 0,payload(),X 17)
                   ++ stackInto(true,16)
                   (* Collection is deferred across this bridge, so no GC
                    * descriptor is needed. Match the callee's ML return kind
                    * without disturbing the enclosing native call/return. *)
                   ++ callInto (Indirect (SS.PHREG_ATY(X 17))) returnLab
                   ++ one (Label returnLab)
                   ++ resumeGCInto()
                   ++ restoreCInto()) code
                val code = if parallel() then
                    (moveInto(X 0,X 19)
                      ++ instruction A.bl (L(NameLab "thread_info"))
                      ++ addOffsetInto(X 0,8,X 28)
                      ++ moveInto(X 19,X 1)) code
                  else
                    (moveInto(X 0,X 1)
                      ++ addressInto(ctx,X 16)
                      ++ loadInto(X 16,0,X 28)) code
              in
                (one (Directive(Data))
                 ++ one (Directive(Align 3))
                 ++ one (Label ctx)
                 ++ one (Directive(Quad ["0"]))
                 ++ functionInto(NameLab name)
                 ++ saveCInto()
                 ++ deferGCInto()) code
              end)
        in
          (readInto fsz aty (X 16)
           ++ addressInto(DatLab clos_lab,X 17)
           ++ storeInto(X 16,X 17,0)
           ++ addressInto(ctx,X 17)
           ++ storeInto(X 28,X 17,0)
           ++ addressInto(textValue,X 16)
           ++ addressInto(NameLab name,X 17)
           ++ internalCallInto fsz "sml_regCfuns" [SS.PHREG_ATY(X 16),SS.PHREG_ATY(X 17)]) code
        end
    | LS.FUNCALL {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcallInto false fsz (Direct opr) {args = args,reg_args = reg_args,fargs = fargs,clos = clos,res = res,bv = bv} code
    | LS.JMP {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcallInto true fsz (Direct opr) {args = args,reg_args = reg_args,fargs = fargs,clos = clos,res = res,bv = bv} code
    | LS.FNCALL {opr,args,clos,res,bv} =>
        mlcallInto false fsz (Indirect opr) {args = args,reg_args = [],fargs = [],clos = clos,res = res,bv = bv} code
    | LS.FNJMP {opr,args,clos,res,bv} =>
        mlcallInto true fsz (Indirect opr) {args = args,reg_args = [],fargs = [],clos = clos,res = res,bv = bv} code
    | LS.SWITCH_I {switch = LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[(v,yes)],no),...} =>
        if v = IntInf.fromInt BackendInfo.ml_true then flowInto fsz live (t,f,yes,no) code
        else flowInto fsz live (f,t,yes,no) code
    | LS.SWITCH_C (LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[((c,_),yes)],no)) =>
        if Con.eq(c,Con.con_TRUE) then flowInto fsz live (t,f,yes,no) code
        else flowInto fsz live (f,t,yes,no) code
    | LS.SWITCH_C (LS.SWITCH(a,[],default)) =>
        stmtsInto fsz live default code
    | LS.SWITCH_C (sw as LS.SWITCH(a,[((con,LS.UNBOXED _),yes)],no)) =>
        (* Valid list values have low bits 00 (CONS) or 11 (NIL).
         * The constructor identity, not just its numeric tag, proves this. *)
        if Con.eq(con,Con.con_NIL) orelse Con.eq(con,Con.con_CONS) then
          let val (src,load) = operandInto fsz a (X 16)
              val otherwise = localFresh()
              val done = localFresh()
              val branch = if Con.eq(con,Con.con_CONS) then A.tbnz else A.tbz
          in
            (load ++ instruction branch (R(src),I(0),L(otherwise))
             ++ stmtsInto fsz live yes ++ instruction A.b (L(done))
             ++ one(Label otherwise) ++ stmtsInto fsz live no
             ++ one(Label done)) code
          end
        else constructorSwitchInto fsz live sw code
    | LS.SWITCH_C sw => constructorSwitchInto fsz live sw code
    | LS.SWITCH_W {switch = LS.SWITCH(a,cases,default),precision = 63} =>
        switchCodeInto fsz live (LS.SWITCH(a,map(fn(n,rhs) => (2*n+1,rhs)) cases,default)) code
    | LS.SWITCH_I {switch = LS.SWITCH(a,cases,default),precision = 63} =>
        switchCodeInto fsz live (LS.SWITCH(a,map(fn(n,rhs) => (2*n+1,rhs)) cases,default)) code
    | LS.SWITCH_W {switch,precision} =>
        numericSwitchInto fsz live false precision switch code
    | LS.SWITCH_I {switch,precision} =>
        numericSwitchInto fsz live true precision switch code
    | LS.RESET_REGIONS {regions_for_resetting,force} =>
        foldr (fn (LS.IGNORE,code) => code
                | (sma,code) => resetRegionInto fsz force sma code) code regions_for_resetting
    | _ => unsupported (LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls)
  and constructorSwitchInto fsz live
        (LS.SWITCH(a,cases as ((con,kind),_)::_,default)) code =
        let
          (* Constructor selectors are already encoded by closure conversion. *)
          fun tag k = IntInf.fromInt
            (case k of LS.ENUM i => i | LS.UNBOXED i => i
                     | LS.UNBOXED_HIGH i => i | LS.BOXED i => i)
          val (src,load) = operandInto fsz a (X 16)
          val selector = case kind of LS.ENUM _ => src | _ => X 16
          val code = switchCodeInto fsz live
            (LS.SWITCH(SS.PHREG_ATY selector,map (fn ((_,k),body) => (tag k,body)) cases,default)) code
          val code = case kind of
              LS.ENUM _ => code
            | LS.BOXED _ => loadInto (src,0,X 16) code
            | LS.UNBOXED_HIGH _ => A.lsr (R(X 16),R(src),I(48)) :: code
            | LS.UNBOXED _ =>
                if Con.eq(con,Con.con_NIL) orelse Con.eq(con,Con.con_CONS) then
                  instruction A.and_ (R(X 16),R(src),I(3)) code
                else (instruction A.and_ (R(X 17),R(src),I(3))
               ++ instruction A.cmp (R(X 17),I(3))
               ++ instruction A.csel (R(X 16),R(src),R(X 17),C EQ)) code
        in
          load code
        end
    | constructorSwitchInto _ _ _ _ = unsupported "empty constructor switch"
  and numericSwitchInto fsz live signed precision (LS.SWITCH(a,cases,default)) code =
    let
      val tag = precision = 31 orelse precision = 63 orelse (precision = 8 andalso tagged())
      val box = tagged() andalso (precision = 32 orelse precision = 64)
      fun value n = if tag then 2*n+1 else n
      val (src,load) = operandInto fsz a (X 16)
      val narrow = precision = 31 orelse precision = 32
      val selector = if box orelse narrow then X 16 else src
      val code = switchCodeInto fsz live
        (LS.SWITCH(SS.PHREG_ATY selector,map (fn (n,rhs) => (value n,rhs)) cases,default)) code
      (* Int31 values from packed tables have only their encoded low 32 bits.
       * Normalise into scratch storage without modifying an allocated source. *)
      val code = if narrow then
          let val input = if box then X 16 else src
              val low = case input of X n => W n | _ => unsupported "numeric switch register"
          in (if signed then A.sxtw else A.uxtw) (R(X 16),R low) :: code
          end
        else code
      val code = if box then loadInto (src,8,X 16) code else code
    in
      load code
    end
  and flowInto fsz live (t,f,yes,no) code =
    let
      val done = localFresh()
    in
      (one (Label(LocalLab t))
       ++ stmtsInto fsz live yes
       ++ instruction A.b (L(done))
       ++ one (Label(LocalLab f))
       ++ stmtsInto fsz live no
       ++ one (Label done)) code
    end
  and switchCodeInto fsz live (LS.SWITCH(a,cases,default)) code =
    let
      (* Order every selector by its signed 64-bit machine representation.
       * This also handles Word64 and tagged Word63 cases across the sign bit. *)
      val modulus = IntInf.pow(2,64)
      val sign = IntInf.pow(2,63)
      fun machineValue n =
        let val n = IntInf.mod(n,modulus)
        in if n >= sign then n-modulus else n
        end
      val cases = map (fn (v,body) => (machineValue v,body)) cases
      val (src,load) = operandInto fsz a (X 16)
      fun compare branch (lab,value,code) =
        (compareConstantInto src value
           ++ instruction branch (L(lab))) code
      fun label (lab,code) = Label lab :: code
      fun jump (lab,code) = instruction A.b (L(lab)) code
      fun compile (body,code) = stmtsInto fsz live body code
      fun header (lab,start,_,code) =
        (* Bounds have already been checked by JumpTables. Entries are signed
         * offsets from the table, so linked and REPL code need no data fixups. *)
        (constantInto (start,X 17)
           ++ instruction A.sub (R(X 16),R(src),R(X 17))
           ++ addressInto (lab,X 17)
           ++ instruction A.ldr (R(X 16),Indexed(X 17,X 16,LSL,3))
           ++ instruction A.add (R(X 16),R(X 17),R(X 16))
           ++ instruction A.br (R(X 16))
           ++ one (Directive(Align 3))) code
      fun entry (lab,table,code) =
        Directive(Quad [pr_lab lab ^ " - " ^ pr_lab table]) :: code
      val code = JumpTables.binary_search_new
        (cases,default,fn (_,code) => code,fn _ => localFresh(),
         compare A.b_ne,compare A.b_lt,compare A.b_gt,compile,label,jump,
         fn (a,rhs) => IntInf.abs(a-rhs),header,entry,
         fn (a,rhs) => eq_lab(a,rhs),fn _ => NONE,code)
    in
      load code
    end

  val unitGCStub : A.lab option ref = ref NONE
  (* x16 points at [mask,skipped,results,args] metadata; BL supplies the
   * stub continuation in x30. The incoming ML return PC is already in the
   * function header and the root walker continues to read it there. *)
  fun gcStubInto lab code =
    let
      val registers = List.filter (fn n => n<>18) (List.tabulate(31,fn i => i))
      val floats = List.tabulate(8,fn i => i)
      val code = (stackInto (false,352) ++ (one A.ret)) code
      val code = foldr (fn (n,code) => loadInto (SP,8*(31-n),X n) code) code registers
      val code = foldr (fn (i,code) => loadInto (SP,8*(39-i),D i) code) code floats
      val code = (loadInto (X 16,0,X 2)
         ++ moveInto (X 28,X 0)
         ++ moveInto (SP,X 1)
         ++ instruction A.bl (L(NameLab "gc"))) code
      val code = foldr (fn (i,code) =>
        (loadInto (X 16,8*(i+1),X 17)
         ++ storeInto (X 17,SP,320+8*i)) code) code [0,1,2]
      val code = (addOffsetInto (SP,352,X 17)
         ++ storeInto (X 17,SP,0)
         ++ storeInto (X 17,SP,344)
         ++ constantInto (0,X 17)
         ++ storeInto (X 17,SP,104)) code
      val code = foldr (fn (i,code) => storeInto (D i,SP,8*(39-i)) code) code floats
      val code = foldr (fn (n,code) => storeInto (X n,SP,8*(31-n)) code) code registers
    in
      (one (Directive(Text)) ++ one (Directive(Align 2))
       ++ one (Label lab) ++ stackInto (true,352)) code
    end
  fun entryGCInto cc code =
    if not(gc()) then code
    else
      let
        val done = localFresh()
        val stub = case !unitGCStub of
            SOME l => l
          | NONE =>
              let val l = localFresh()
                  val () = unitGCStub := SOME l
                  val () = addStatic (gcStubInto l [])
              in l
              end
        val ac = CallConv.get_ccf_size cc
        val rc = CallConv.get_rcf_size cc
        val skip = length(CallConv.get_spilled_region_and_float_args cc)
        val mask = foldl (fn (lv,w) =>
          case A.RI.lv_to_reg lv of
            X n => Word32.orb(w,Word32.<<(0w1,Word.fromInt n))
          | _ => w) 0w0 (CallConv.get_register_args_excluding_region_and_float_args cc)
        val metadata = localFresh()
        val () = addStatic
          [Directive(Data),Directive(Align 3),Label metadata,
           Directive(Quad ["0x" ^ Word32.fmt StringCvt.HEX mask]),
           Directive(Quad [Int.toString skip]),
           Directive(Quad [Int.toString rc]),
           Directive(Quad [Int.toString ac])]
        val code = (addressInto (NameLab "disable_gc",X 16)
           ++ loadInto (X 16,0,X 16)
           ++ instruction A.cbnz (R(X 16),L(done))
           ++ addressInto (metadata,X 16)
           ++ instruction A.bl (L(stub))
           ++ one (Label done)) code
      in
        if extra_gc_checks() then code
        else (addressInto (NameLab "time_to_gc",X 16)
           ++ loadInto (X 16,0,X 16)
           ++ instruction A.cbz (R(X 16),L(done))) code
      end
  (* Match the IR, not an assembly pattern: every operand must already be
   * in its incoming argument register, with identical argument/result shapes.
   * The unused local frame can then be omitted along with its header saves. *)
  fun identityTail cc statements =
    let
      val incoming = CallConv.decompose_cc cc
      fun sameLength (xs,ys) = length xs = length ys
      fun find [LS.SCOPE {scope,...}] = find scope
        | find [LS.LETREGION {rhos,body}] =
            if List.all (fn ((_,LS.WORDS _),_) => true | _ => false) rhos
            then find body else NONE
        | find [LS.JMP {opr,args,reg_args,fargs,clos,res,...}] =
            let val gp = (case clos of NONE => [] | SOME a => [a]) @ args @ reg_args
            in
              if CallConv.get_ccf_size cc = 0 andalso length gp <= 8 andalso length fargs <= 8
                 andalso sameLength(args,#args incoming)
                 andalso sameLength(reg_args,#reg_args incoming)
                 andalso sameLength(fargs,#fargs incoming)
                 andalso Option.isSome clos = Option.isSome(#clos incoming)
                 andalso sameLength(res,#res incoming) andalso length res <= 3
                 andalso registersPlaced (map X AbiArm64.mlArgumentGPRs) gp
                 andalso registersPlaced (map D AbiArm64.mlArgumentFPRs) fargs
                 andalso registersPlaced (map X AbiArm64.mlResultGPRs) res
              then SOME opr else NONE
            end
        | find _ = NONE
    in find statements
    end
  (* Keep the first loop implementation deliberately small. These primitives
   * emit inline arithmetic/comparisons (possibly an exiting raise), never calls.
   * Frames with locals, incoming stack arguments, regions or handlers stay on
   * the normal tail-call path. Forced polling and profiling do too. *)
  fun loopPrimitive name =
    let open PrimName
    in case name of
         Plus_int63 => true | Minus_int63 => true
       | Plus_word63 => true | Minus_word63 => true
       | Plus_int64ub => true | Minus_int64ub => true
       | Equal_int63 => true | Equal_word63 => true | Equal_ptr => true
       | Less_int63 => true | Lesseq_int63 => true
       | Greater_int63 => true | Greatereq_int63 => true
       | Less_word63 => true | Lesseq_word63 => true
       | Greater_word63 => true | Greatereq_word63 => true
       | Plus_f64 => true | Minus_f64 => true | Mul_f64 => true | Div_f64 => true
       | _ => false
    end
  fun loopStatements self statements = List.all (loopStatement self) statements
  and loopStatement (self as (label,recursive)) ls =
    case ls of
      LS.SCOPE {scope,...} => loopStatements self scope
    | LS.LETREGION {rhos = [],body} => loopStatements self body
    | LS.SWITCH_I {switch,...} => loopSwitch self (switchParts switch)
    | LS.SWITCH_W {switch,...} => loopSwitch self (switchParts switch)
    | LS.SWITCH_C switch => loopSwitch self (switchParts switch)
    | LS.ASSIGN {bind,...} =>
        (case bind of
           LS.ATOM _ => true | LS.LOAD _ => true | LS.STORE _ => true
         | LS.SELECT _ => true | LS.DECON _ => true | LS.DEREF _ => true
         | LS.CON0 {con_kind = LS.ENUM _,aux_regions = [],alloc = LS.IGNORE,...} => true
         | _ => false)
    | LS.PRIM {name,...} => loopPrimitive name
    | LS.JMP {opr,...} => (if AddressLabels.eq(opr,label) then recursive := true else (); true)
    | LS.RAISE _ => true
    | _ => false
  and loopSwitch self (_,cases,default) =
    loopStatements self default andalso List.all (loopStatements self) cases
  fun topInto (l,cc,body) code =
    let
      val suffix = code
      val ac = CallConv.get_ccf_size cc
      val () = currentArgs := ac
      val () = currentResults := CallConv.get_rcf_size cc
      val fsz = CallConv.get_frame_size cc
      (* Return values remain live through a final region release. Stack
       * results are already stored; the three register result slots suffice. *)
      val results = first (length(CallConv.get_res_lvars cc)) [X 0,X 1,X 2]
      val () = functionRegs := results
      val frameOperand = ref false
      fun remember a =
        (functionRegs := unionRegs(atyRegs a,!functionRegs);
         (case a of SS.STACK_ATY _ => frameOperand := true
                  | SS.REG_I_ATY _ => frameOperand := true
                  | SS.REG_F_ATY _ => frameOperand := true | _ => ()); a)
      val _ = LS.map_lss remember (fn x => x) (fn x => x) body
      val () = conservativeRegionCalls := false
      val optimiseFrame = not(profiling()) andalso not(extra_gc_checks())
      val wrapper = if optimiseFrame andalso tail_wrappers() then identityTail cc body else NONE
      val recursive = ref false
      val loop = if optimiseFrame andalso self_loops() andalso fsz = 0 andalso ac = 0
                    andalso length(CallConv.get_res_lvars cc) <= 3 andalso not(!frameOperand)
                    andalso not(LS.allocating body) andalso loopStatements (l,recursive) body andalso !recursive
                 then SOME (LocalLab(AddressLabels.new_named "arm64_loop")) else NONE
      val () = currentLoop := Option.map (fn loop => (l,loop)) loop
      val code = (stmtsInto fsz results body
         ++ epilogueInto fsz) code
      val code = if profiling() then internalCallInto fsz "mlkit_arm64_profile_entry"
                   [SS.PHREG_ATY(X 28),SS.REG_F_ATY(fsz-1)] code
                 else code
    in
      case wrapper of
        SOME target => (functionInto (MLFunLab l)
          ++ instruction A.b (L(MLFunLab target))) suffix
      | NONE => (functionInto (MLFunLab l)
       ++ storeInto (X 29,SP,8*even ac)
       ++ storeInto (X 30,SP,8*(even ac+1))
       ++ addOffsetInto (SP,8*even ac,X 29)
       ++ (if extra_gc_checks() orelse LS.allocating body then entryGCInto cc
           else fn code => code)
       ++ stackInto (true,8*fsz)
       ++ (case loop of SOME lab => one (Label lab) | NONE => fn code => code)) code
    end
  fun CG {main_lab,code,imports,exports,safe} =
    let
      val () = staticChunks := []
      val () = unitGCStub := NONE
      val () = dataLabels := []
      val text = foldr (fn (LS.FUN x,code) => topInto x code
                        | (LS.FN x,code) => topInto x code) [] code
      fun data (l,code) =
        (one (Directive(Data))
           ++ one (Directive(Align 3))
           ++ one (Directive(Global (DatLab l)))
           ++ one (Label(DatLab l))
           ++ one (Directive(Quad ["1"]))) code
      fun marker suffix code =
        let
          val l = unitSymbol main_lab suffix
        in
          (one (Directive(Data))
             ++ one (Directive(Align 3))
             ++ one (Directive(Global (l)))
             ++ one (Label l)) code
        end
      (* Metadata is discovered while lowering the functions. Put data before
       * text so the completed instruction stream need not be copied. *)
      val code = marker "begin" (foldr data (staticDataInto (marker "end" text)) (!dataLabels))
    in
      if not(gc()) then code
      else marker "roots" (Directive(Quad [Int.toString(length(!dataLabels))]) ::
        foldr (fn (l,code) => Directive(Quad [pr_lab(DatLab l)]) :: code) code (!dataLabels))
    end
  (* Runtime main enters code with the context in x0. This entry terminates
   * the process; returning foreign callbacks need a separate preserving bridge. *)
  fun registerUnitsInto labs code =
    foldr (fn (l,code) => registerUnitInto l code) code labs
  fun callUnitsInto (labs,pcs) code =
    ListPair.foldr (fn (l,pc,code) =>
      (stackInto (true,16)
         ++ callInto (Direct l) pc
         ++ (if gc() then
               one (Directive(Align 3))
               ++ one (Directive(Quad ["-1"]))
               ++ one (Directive(Quad ["0"]))
               ++ one (Directive(Quad ["0"]))
             else fn code => code)
         ++ one (Label pc)) code) code (labs,pcs)
  fun preservingStubInto lab setup target code =
    let
      val bytes = 16*((length savedRegs+2) div 2)
      val code =
        (loadInto (SP,8*length savedRegs,X 30)
         ++ stackInto (false,bytes)
         ++ (one A.ret)) code
      val code = foldri (fn (i,reg,code) => loadInto (SP,8*i,reg) code) code savedRegs
      val code = (instruction A.bl (L(target))
         ++ moveInto (X 0,X 16)) code
      val code = setup bytes code
      val code = storeInto (X 30,SP,8*length savedRegs) code
      val code = foldri (fn (i,reg,code) => storeInto (reg,SP,8*i) code) code savedRegs
    in
      (functionInto lab
         ++ stackInto (true,bytes)) code
    end
  fun allocationStubsInto code =
    let
      fun setup untag bytes code =
        (moveInto (X 16,X 0)
         ++ moveInto (X 17,X 1)
         ++ countInto (X 2) 0
         ++ countInto (X 3) untag
         ++ (if profiling() then loadInto (SP,bytes,X 4) else countInto (X 4) 0)) code
    in
      (preservingStubInto allocStub (setup 0) (NameLab "mlkit_arm64_alloc")
       ++ preservingStubInto untaggedAllocStub (setup 1) (NameLab "mlkit_arm64_alloc")
       ++ preservingStubInto resetStub
        (fn _ => fn code => moveInto (X 16,X 0) code) (NameLab "resetRegion")) code
    end
  fun linkCode repl (labs,_) =
    let
      val globals = [(Effect.toplevel_region_withtype_top,BackendInfo.toplevel_region_withtype_top_lab),
        (Effect.toplevel_region_withtype_string,BackendInfo.toplevel_region_withtype_string_lab),
        (Effect.toplevel_region_withtype_pair,BackendInfo.toplevel_region_withtype_pair_lab),
        (Effect.toplevel_region_withtype_array,BackendInfo.toplevel_region_withtype_array_lab),
        (Effect.toplevel_region_withtype_ref,BackendInfo.toplevel_region_withtype_ref_lab),
        (Effect.toplevel_region_withtype_triple,BackendInfo.toplevel_region_withtype_triple_lab)]
      val () = staticChunks := []
      fun datum l words code =
        Directive(Data) :: Directive(Align 3) ::
        Directive(Global (l)) :: Label l ::
        foldr (fn (s,code) => Directive(Quad [s]) :: code) code words
      fun init (place,l) code =
        (stackInto(true,8*even(BackendInfo.size_of_reg_desc()))
          ++ moveInto(X 28,X 0)
          ++ moveInto(SP,X 1)
          ++ constantInto(IntInf.fromInt(regionPolicy true place),X 2)
          ++ instruction A.bl (L(NameLab(regionAllocator place)))
          ++ instruction A.orr (R(X 0),R(X 0),I(1))
          ++ addressInto(DatLab l,X 17)
          ++ storeInto(X 0,X 17,0)) code
      val exceptions = [("MATCH","Match",BackendInfo.exn_MATCH_lab),
        ("BIND","Bind",BackendInfo.exn_BIND_lab),("OVERFLOW","Overflow",BackendInfo.exn_OVERFLOW_lab),
        ("INTERRUPT","Interrupt",BackendInfo.exn_INTERRUPT_lab),("DIV","Div",BackendInfo.exn_DIV_lab),
        ("SUBSCRIPT","Subscript",BackendInfo.exn_SUBSCRIPT_lab),("SIZE","Size",BackendInfo.exn_SIZE_lab)]
      val exceptionData = mapi (fn (i,(name,display,lab)) =>
        let
          val l = NameLab("exn_" ^ name)
          val textValue = stringData display
          val words = if tagged() then
              ["0x" ^ Word.toString(BackendInfo.tag_exname true),pr_lab l ^ "+16",
               "0x" ^ Word.toString(BackendInfo.tag_excon0 true),Int.toString i,pr_lab textValue]
            else [pr_lab l ^ "+8",Int.toString i,pr_lab textValue]
        in
          (l,words,lab)
        end) exceptions
      fun data code =
        foldr (fn ((_,l),code) => datum (DatLab l) ["0"] code)
          (datum (NameLab "exnameCounter") ["7"]
            (foldr (fn ((l,words,lab),code) =>
              (datum l words
                 ++ datum (DatLab lab) [pr_lab l]) code) code exceptionData)) globals
      val alloc = NameLab "mlkit_arm64_alloc"
      val finite = localFresh()
      val noreset = localFresh()
      val reset = NameLab "mlkit_arm64_reset"
      val resetDone = localFresh()
      val raising = NameLab "raise_exn"
      val unwind = localFresh()
      val unwound = localFresh()
      val uncaught = localFresh()
      val linkBegin = NameLab "arm64_link_begin"
      val linkEnd = NameLab "arm64_link_end"
      val returnLabels = map(fn _ => localFresh()) labs
      fun gcInit code =
        if not(gc()) then
          code
        else
          (registerUnitsInto labs
            ++ addressInto(linkBegin,X 0)
            ++ moveInto(X 0,X 1)
            ++ addressInto(linkEnd,X 2)
            ++ constantInto(0,X 3)
            ++ constantInto(0,X 4)
            ++ instruction A.bl (L(NameLab "mlkit_arm64_register_static_image"))
            ++ instruction A.bl (L(NameLab "mlkit_arm64_seal_main_image"))
            ++ addressInto(NameLab "stack_bot_gc",X 16)
            ++ moveInto(SP,X 17)
            ++ storeInto(X 17,X 16,0)) code
      fun gcData code =
        if not(gc()) then
          code
        else
          (datum (NameLab "data_begin_addr") [pr_lab linkBegin]
            ++ datum (NameLab "data_end_addr") [pr_lab linkEnd]) code

      fun profileStack () code =
        foldr (fn (name,code) => (addressInto (NameLab name,X 16)
           ++ moveInto (SP,X 17)
           ++ storeInto (X 17,X 16,0)) code)
          code ["stackBot","maxStack","maxStackP"]
      fun initGlobals () code = foldr (fn (g,code) => init g code) code globals
      val code = gcData []
      val code =
        (loadInto(X 19,16,X 16)
         ++ storeInto(X 16,X 28,8)
         ++ loadInto(X 19,24,X 16)
         ++ moveInto(X 16,SP)
         ++ loadInto(X 19,32,X 29)
         ++ loadInto(X 19,0,X 30)
         ++ loadInto(X 19,8,X 0)
         ++ moveInto(X 27,X 1)
         ++ loadInto(X 0,payload(),X 17)
         ++ stackInto(true,16)
         ++ instruction A.br (R(X 17))
         ++ one (Label uncaught)
         ++ moveInto(X 28,X 0)
         ++ loadInto(X 27,payload(),X 16)
         ++ loadInto(X 16,8+payload(),X 1)
         ++ loadInto(X 16,payload(),X 2)
         ++ moveInto(X 27,X 3)
         ++ instruction A.b (L(NameLab "uncaught_exception"))
         ++ one (Directive(Data))
         ++ one (Directive(Align 3))
         ++ one (Label linkBegin)
         ++ data
         ++ staticDataInto
         ++ one (Label linkEnd)) code
      val code = if profiling() then
          (moveInto(X 28,X 0)
            ++ moveInto(X 19,X 1)
            ++ instruction A.bl (L(NameLab "deallocateRegionsUntil"))) code
        else
          (one (Label unwind)
            ++ loadInto(X 28,0,X 16)
            ++ loadInto(X 19,40,X 17)
            ++ instruction A.cmp (R(X 16),R(X 17))
            ++ instruction A.b_eq (L(unwound))
            ++ moveInto(X 28,X 0)
            ++ instruction A.bl (L(NameLab "deallocateRegion"))
            ++ instruction A.b (L(unwind))
            ++ one (Label unwound)) code
      val code =
        ((one A.ret)
         ++ functionInto reset
         ++ instruction A.tbz (R(X 0),I(0),Forward 1)
         ++ instruction A.cmp (R(X 1),I(2))
         ++ instruction A.b_eq (Forward 2)
         ++ instruction A.tbz (R(X 0),I(1),Forward 1)
         ++ one (Directive(NumericLabel 2))
         ++ instruction A.b (L(NameLab "resetRegion"))
         ++ one (Directive(NumericLabel 1))
         ++ (one A.ret)
         ++ functionInto raising
         ++ moveInto(X 0,X 28)
         ++ moveInto(X 1,X 27)
         ++ loadInto(X 28,8,X 19)
         ++ instruction A.cbz (R(X 19),L(uncaught))) code
      val code = if profiling() then
          storeInto(X 4,X 0,~16) code
        else
          code
      val code =
        (constantInto(0,X 0)
         ++ instruction A.b (L(NameLab "terminateML"))
         ++ allocationStubsInto
         ++ functionInto alloc
         ++ instruction A.tbz (R(X 0),I(0),L(finite))
         ++ stackInto(true,48)
         ++ storeInto(X 19,SP,0)
         ++ storeInto(X 20,SP,8)
         ++ storeInto(X 30,SP,16)
         ++ storeInto(X 21,SP,24)
         ++ storeInto(X 22,SP,32)
         ++ moveInto(X 4,X 22)
         ++ moveInto(X 3,X 21)
         ++ moveInto(X 0,X 19)
         ++ moveInto(X 1,X 20)
         ++ instruction A.cmp (R(X 2),I(2))
         ++ instruction A.b_eq (L(resetDone))
         ++ instruction A.cbz (R(X 2),L(noreset))
         ++ instruction A.tbz (R(X 0),I(1),L(noreset))
         ++ one (Label resetDone)
         ++ instruction A.bl (L(NameLab "resetRegion"))
         ++ one (Label noreset)
         ++ moveInto(X 19,X 0)
         ++ moveInto(X 20,X 1)
         ++ moveInto(X 22,X 2)
         ++ instruction A.bl (if profiling() then L(NameLab "allocProfiling") else if parallel() andalso unprotected() then L(NameLab "alloc_unprotected") else L(NameLab "alloc"))
         ++ instruction A.sub (R(X 0),R(X 0),Shifted(X 21,LSL,3))
         ++ loadInto(SP,32,X 22)
         ++ loadInto(SP,24,X 21)
         ++ loadInto(SP,0,X 19)
         ++ loadInto(SP,8,X 20)
         ++ loadInto(SP,16,X 30)
         ++ stackInto(false,48)
         ++ (one A.ret)
         ++ one (Label finite)
         ++ instruction A.and_ (R(X 0),R(X 0),I(~4))) code
      val code = if repl then
          (moveInto(X 28,X 0)
            ++ instruction A.bl (L(NameLab "repl_interp"))) code
        else
          callUnitsInto (labs,returnLabels) code
      val code =
        (initGlobals ()
         ++ gcInit) code
      val code = if profiling() then
          profileStack () code
        else
          code
    in
      (functionInto(NameLab "code")
       ++ moveInto(X 0,X 28)) code
    end
  fun generate_link_code args = linkCode false args
  fun generate_repl_init_code () = linkCode true ([],([],[]))
  fun generate_repl_link_code (name,labs) =
    let
      val join = localFresh()
      val handler = localFresh()
      val closure = localFresh()
      val beginData = localFresh()
      val endData = localFresh()
      val pcs = map(fn _ => localFresh()) labs
      fun metadata code =
        if not(gc()) then
          code
        else
          (registerUnitsInto labs
            ++ addressInto(beginData,X 0)
            ++ moveInto(X 0,X 1)
            ++ addressInto(endData,X 2)
            ++ constantInto(0,X 3)
            ++ constantInto(0,X 4)
            ++ instruction A.bl (L(NameLab "mlkit_arm64_register_static_image"))) code
      val code = []
      val code = (one (Directive(Quad [pr_lab handler]))
         ++ one (Label endData)) code
      val code = if tagged() then
          Directive(Quad ["0x" ^ Word.toString(BackendInfo.tag_clos(true,1,1))]) :: code
        else
          code
    in
      (functionInto(NameLab name)
       ++ saveCInto()
       ++ addressInto(NameLab "top_ctx",X 16)
       ++ loadInto(X 16,0,X 28)
       ++ metadata
       ++ stackInto(true,48)
       ++ addressInto(join,X 16)
       ++ storeInto(X 16,SP,0)
       ++ addressInto(closure,X 16)
       ++ storeInto(X 16,SP,8)
       ++ loadInto(X 28,8,X 16)
       ++ storeInto(X 16,SP,16)
       ++ moveInto(SP,X 16)
       ++ storeInto(X 16,SP,24)
       ++ storeInto(X 29,SP,32)
       ++ loadInto(X 28,0,X 16)
       ++ storeInto(X 16,SP,40)
       ++ moveInto(SP,X 16)
       ++ storeInto(X 16,X 28,8)
       ++ callUnitsInto (labs,pcs)
       ++ one (Label join)
       ++ loadInto(SP,16,X 16)
       ++ storeInto(X 16,X 28,8)
       ++ stackInto(false,48)
       ++ restoreCInto()
       ++ (one A.ret)
       ++ one (Label handler)
       ++ storeInto(X 30,SP,8)
       ++ moveInto(X 1,X 3)
       ++ loadInto(X 1,payload(),X 16)
       ++ loadInto(X 16,8+payload(),X 1)
       ++ loadInto(X 16,payload(),X 2)
       ++ moveInto(X 28,X 0)
       ++ instruction A.bl (L(NameLab "uncaught_exception"))
       ++ loadInto(SP,8,X 30)
       ++ stackInto(false,16)
       ++ (one A.ret)
       ++ one (Directive(Data))
       ++ one (Directive(Align 3))
       ++ one (Label beginData)
       ++ one (Label closure)) code
    end

end
