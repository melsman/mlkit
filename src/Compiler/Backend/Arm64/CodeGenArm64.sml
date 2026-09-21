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
  val messages_p = Flags.is_on0 "messages"
  fun message f = if messages_p() then print(f()) else ()
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
  (* Keep fragments in reverse insertion order; never copy accumulated data. *)
  val staticChunks : A.inst list list ref = ref []
  fun addStatic code = staticChunks := code :: !staticChunks
  fun staticDataInto code =
    foldl (fn (chunk,code) => foldr (op ::) code chunk) code (!staticChunks)
  fun numberInto {value,precision} dst code =
    if tagged() andalso (precision=32 orelse precision=64) then
      let
        val l = DatLab(AddressLabels.new_named "arm64_integer")
        val () = addStatic
          [Directive ".data",Directive ".p2align 3",Label l,
           Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_word_boxed true)),
           Directive(".quad 0x" ^ IntInf.fmt StringCvt.HEX
             (IntInf.mod(value,18446744073709551616)))]
      in
        addressInto (l,dst) code
      end
    else constantInto
      (if precision=63 orelse precision=31 orelse (precision=8 andalso tagged())
       then 2*value+1 else value,dst) code
  fun readInto fsz aty dst code =
    case aty of
      SS.PHREG_ATY src => moveInto (src,dst) code
    | SS.STACK_ATY off => loadInto (SP,slot fsz off,dst) code
    | SS.INTEGER_ATY n => numberInto n dst code
    | SS.WORD_ATY n => numberInto n dst code
    | SS.REG_I_ATY off =>
        addOffsetInto (SP,slot fsz off,dst) (ins "orr" [r dst,r dst,"#1"] :: code)
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
        readInto (fsz+bytes div 8) a (X 16) (storeInto (X 16,SP,8*i) code))
        code args
    in
      stackInto (true,bytes) code
    end
  fun even n = n + n mod 2
  fun first n xs = List.take(xs,Int.min(n,length xs))
  fun rest n xs = List.drop(xs,Int.min(n,length xs))
  val currentArgs = ref 0
  val currentResults = ref 0
  (* The callee releases its locals, argument area and header. The caller
   * receives a separately aligned result area and then releases that area. *)
  fun resultsInto fsz res code =
    let
      val spilled = Int.max(0,length res-3)
      val rw = even spilled
      val temp = even(length res)
      val code = stackInto (false,8*(temp+rw)) code
      val code = foldri (fn (i,a,code) =>
        loadInto (SP,8*i,X 16) (writeInto (fsz+rw+temp) a (X 16) code)) code res
      val code = foldri (fn (i,_,code) =>
        if i<3 then storeInto (X i,SP,8*i) code
        else loadInto (SP,8*(temp+spilled-1-(i-3)),X 16)
                      (storeInto (X 16,SP,8*i) code)) code res
    in
      stackInto (true,8*temp) code
    end
  val frameIndex : (A.lab*A.lab) list ref = ref []
  fun continuation bv =
    let
      val pc=localFresh()
      val anchor=DatLab(AddressLabels.new_named "arm64_frame")
      val ()=if gc() then
          (addStatic (Directive ".data" :: Directive ".p2align 3" ::
            foldl (fn (w,code) => Directive(".quad 0x" ^ Word32.fmt StringCvt.HEX w) :: code)
              [Label anchor] bv);
           frameIndex := (pc,anchor):: !frameIndex) else ()
    in
      pc
    end
  fun unitSymbol lab suffix = NameLab(AddressLabels.pr_label lab ^ "_arm64_" ^ suffix)
  datatype target = Direct of label | Indirect of SS.Aty
  fun mlcallInto tail fsz target {args,reg_args,fargs,clos,res,bv} code =
    let
      val returnLabel = if tail then localFresh() else continuation bv
      val gp = (case clos of NONE => [] | SOME a => [a]) @ args @ reg_args
      val fp = fargs
      val sa = rest 8 gp @ rest 8 fp
      val ac = length sa
      val aw = even ac
      val rc = Int.max(0,length res-3)
      val rw = even rc
      val staged = gp @ fp @ (case target of Direct _ => [] | Indirect a => [a])
      val sw = even(length staged)
      val newCall = aw+2+rw
      val oldArg = even(!currentArgs)+2
      val workspace = if tail then sw+aw+2 else sw+newCall
      val dest = if tail then workspace+fsz+oldArg-(aw+2) else sw
      val () = if tail andalso rc <> !currentResults then
                 unsupported "tail call with incompatible result area" else ()
      val stackArgs = mapi (fn (i,_) => i+8) (rest 8 gp) @
                      mapi (fn (i,_) => length gp+8+i) (rest 8 fp)
      val code = if tail then code else Label returnLabel :: resultsInto fsz res code
      val code = (case target of
                    Direct l => ins (if tail then "b" else "bl") [pr_lab(MLFunLab l)]
                  | Indirect _ => ins (if tail then "br" else "blr") ["x17"]) :: code
      val code = stackInto (false,8*(if tail then dest else sw)) code
      val code = case target of
                   Direct _ => code
                 | Indirect _ => loadInto (SP,8*(length staged-1),X 17)
                                          (loadInto (X 17,payload(),X 17) code)
      val code = foldri (fn (i,_,code) => loadInto (SP,8*(length gp+i),D i) code)
                        code (first 8 fp)
      val code = foldri (fn (i,_,code) => loadInto (SP,8*i,X i) code) code (first 8 gp)
      val code = if tail then
                   loadInto (SP,8*(workspace+fsz+even(!currentArgs)),X 29)
                     (loadInto (SP,8*(workspace+fsz+even(!currentArgs)+1),X 30) code)
                 else code
      val code = foldri (fn (i,source,code) =>
        loadInto (SP,8*source,X 16) (storeInto (X 16,SP,8*(dest+ac-1-i)) code))
        code stackArgs
      val code = foldri (fn (i,a,code) =>
        readInto (fsz+workspace) a (X 16) (storeInto (X 16,SP,8*i) code)) code staged
    in
      stackInto (true,8*workspace) code
    end
  (* Precision zero is private to raw runtime-helper arguments. *)
  fun integer n = SS.INTEGER_ATY{value=IntInf.fromInt n,precision=0}
  (* Allocation helpers are invisible to register allocation. Preserve every
   * allocatable register, including the full 64 bits of floating registers. *)
  val savedRegs = List.tabulate(16,X) @ List.tabulate(8,fn i => X(i+19)) @ List.tabulate(30,D)
  fun internalCallInto fsz name args code =
    let
      val words = length savedRegs
      val code = stackInto (false,8*words) code
      val code = foldri (fn (i,a,code) => loadInto (SP,8*i,a) code) code savedRegs
      val code = argumentsInto (fsz+words) args
        (ins "bl" [pr_lab(NameLab name)] :: moveInto (X 0,X 16) code)
      val code = foldri (fn (i,a,code) => storeInto (a,SP,8*i) code) code savedRegs
    in
      stackInto (true,8*words) code
    end
  (* Foreign calls may re-enter ML through an exported hook. The existing IR
   * carries no root map at a C call, so defer collection across that dynamic
   * extent, including callbacks. Pending collection is retained on return. *)
  fun foreignCallInto fsz name args convert code =
    if name=":" then unsupported "dynamic foreign symbol resolution"
    else scalarCallInto
      {name=name,fixed=map (fn _ => AbiArm64.I64) args,variadic=[],protectGC=gc(),
       loadArgument=fn (i,extra) => fn code => readInto (fsz+extra) (List.nth(args,i)) (X 16)
                                    (convert(i,X 16) code)} code
  fun autoCallInto fsz {name,args:(SS.Aty*LS.foreign_type) list,rhos_for_result,res=(dst,ft)} code =
    let
      fun convert (i,r) code =
        case #2(List.nth(args,i)) of
          LS.Bool => ins "lsr" [pr_reg r,pr_reg r,"#1"] :: code
        | LS.Int => if tagged() then ins "asr" [pr_reg r,pr_reg r,"#1"] :: code else code
        | LS.Int32 => if tagged() then loadInto (r,8,r) code else code
        | LS.Int64 => if tagged() then loadInto (r,8,r) code else code
        | LS.ForeignPtr => if tagged() then ins "sub" [pr_reg r,pr_reg r,"#1"] :: code else code
        | LS.CharArray => ins "add" [pr_reg r,pr_reg r,"#8"] :: code
        | LS.Unit => unsupported "unit foreign argument"
      val boxed = tagged() andalso (ft=LS.Int32 orelse ft=LS.Int64)
      fun tag code = ins "lsl" ["x0","x0","#1"] :: ins "add" ["x0","x0","#1"] :: code
      val code = writeInto fsz dst (X 0) code
      val code = case ft of
          LS.Unit => constantInto (1,X 0) code
        | LS.Bool => ins "cmp" ["x0","#0"] :: ins "cset" ["x0","ne"] :: tag code
        | LS.Int => if tagged() then tag code else code
        | LS.ForeignPtr => if tagged() then ins "add" ["x0","x0","#1"] :: code else code
        | LS.CharArray => unsupported "foreign char-array result"
        | _ => if boxed then loadInto (SP,0,X 16)
            (storeInto (X 0,X 16,8)
            (constantInto (IntInf.fromInt(Word.toInt(BackendInfo.tag_word_boxed false)),X 17)
            (storeInto (X 17,X 16,0) (moveInto (X 16,X 0) (stackInto (false,16) code)))))
          else code
      val code = foreignCallInto (fsz+(if boxed then 2 else 0)) name (map #1 args) convert code
    in
      if boxed then
        case rhos_for_result of
          [a] => stackInto (true,16) (readInto (fsz+2) a (X 16) (storeInto (X 16,SP,0) code))
        | _ => unsupported "boxed foreign result without storage"
      else code
    end
  val cSaved = List.tabulate(12,fn i => X(i+19)) @ List.tabulate(8,fn i => D(i+8))
  fun saveRegisters code =
    foldri (fn (i,r,code) => storeInto (r,SP,8*i) code) code cSaved
  fun restoreRegisters code =
    foldri (fn (i,r,code) => loadInto (SP,8*i,r) code) code cSaved
  fun saveCInto () code =
    let
      val code = addOffsetInto(SP,80,X 29) code
      val code = saveRegisters code
    in
      stackInto(true,160) code
    end
  fun restoreCInto () code =
    let
      val code = stackInto(false,160) code
    in
      restoreRegisters code
    end
  fun deferGCInto () code =
    if not(gc()) then
      code
    else
      let
        val code = storeInto(X 17,X 16,0) code
        val code = constantInto(1,X 17) code
        val code = storeInto(X 17,SP,0) code
        val code = loadInto(X 16,0,X 17) code
        val code = addressInto(NameLab "disable_gc",X 16) code
      in
        stackInto(true,16) code
      end
  fun resumeGCInto () code =
    if not(gc()) then
      code
    else
      let
        val code = stackInto(false,16) code
        val code = storeInto(X 17,X 16,0) code
        val code = addressInto(NameLab "disable_gc",X 16) code
      in
        loadInto(SP,0,X 17) code
      end
  fun registerUnitInto l code =
    let
      val code = ins "bl" ["_mlkit_arm64_register_image"] :: code
      val code = addOffsetInto(X 4,8,X 4) code
      val code = loadInto(X 4,0,X 5) code
      val code = addressInto(unitSymbol l "roots",X 4) code
      val code = addressInto(unitSymbol l "end",X 3) code
      val code = addressInto(unitSymbol l "begin",X 2) code
      val code = addOffsetInto(X 0,8,X 0) code
      val code = loadInto(X 0,0,X 1) code
    in
      addressInto(unitSymbol l "frames",X 0) code
    end
  fun static words =
    let
      val l = DatLab(AddressLabels.new_named "arm64_data")
    in
      addStatic (Directive ".data" :: Directive ".p2align 3" :: Label l :: words); l
    end
  fun realData value =
    let
      val code = [Directive(".double " ^ String.translate(fn #"~" => "-" | c => String.str c) value)]
      val code = if tagged() then
                   Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_real true)) :: code
                 else code
    in
      static code
    end
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
      LS.ATTOP_LI(_,p) => p | LS.ATTOP_LF(_,p) => p | LS.ATTOP_FI(_,p) => p | LS.ATTOP_FF(_,p) => p
    | LS.ATBOT_LI(_,p) => p | LS.ATBOT_LF(_,p) => p | LS.SAT_FI(_,p) => p | LS.SAT_FF(_,p) => p
    | LS.IGNORE => 0
  fun allocateInto fsz sma words code =
    let
      val (a,mode)=regionArg sma
    in
      internalCallInto fsz "mlkit_arm64_alloc" [a,integer words,integer mode,integer 0,integer(programPoint sma)] code
    end
  (* Keep the destination on the stack while filling it: source operands may
   * use either scratch register during address materialization. *)
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
      val code = loadInto (SP,0,X 16)
        (stackInto (false,16) (writeInto fsz pat (X 16) code))
      val code = foldri (fn (i,a,code) =>
        readInto (fsz+2) a (X 16)
          (loadInto (SP,0,X 17) (storeInto (X 16,X 17,8*(i+length prefix)) code)))
        code elems
      val code = foldri (fn (i,fragment,code) =>
        fieldInto fragment (loadInto (SP,0,X 17) (storeInto (X 16,X 17,8*i) code)))
        (Label skip :: code) prefix
      val code = if untag then readInto (fsz+2) region (X 17)
                   (ins "tbnz" ["x17","#0",pr_lab skip] :: code)
                 else code
      val code = stackInto (true,16) (storeInto (X 16,SP,0) code)
    in
      if untag then internalCallInto fsz "mlkit_arm64_alloc"
        [region,integer(length elems),integer mode,integer 1,integer(programPoint alloc)] code
      else internalCallInto fsz "mlkit_arm64_alloc"
        [region,integer(length prefix+length elems),integer mode,integer 0,
         integer(programPoint alloc)] code
    end
  val recordInto = recordWithUntagInto false
  fun regionAllocator place =
    let
        val kind=if gc() andalso not(tagPairs()) then case Effect.get_place_ty place of
        SOME Effect.PAIR_RT => "Pair" | SOME Effect.REF_RT => "Ref"
      | SOME Effect.TRIPLE_RT => "Triple" | SOME Effect.ARRAY_RT => "Array"
      | _ => "" else ""
    in
      if profiling() then "alloc" ^ kind ^ "RegionInfiniteProfiling"
       else "allocate" ^ kind ^ "Region"
    end
  fun regionPolicy global place =
    if profiling() then Effect.key_of_eps_or_rho place
    else if parallel() andalso (global orelse alloc_protect_always() orelse Effect.get_protect place = SOME true) then 1
    else 0
  fun primitiveInto fsz {name,args,res} code =
    let
      open PrimName
      fun binary opn code =
        (case (args,res) of
          ([a,b],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = ins opn ["x16","x16","x17"] :: code
              val code = readInto fsz b (X 17) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "primitive arity")
      fun compare cc code =
        (case (args,res) of
          ([a,b],[SS.FLOW_VAR_ATY(_,t,f)]) =>
            let
              val code = ins "cmp" ["x16","x17"] ::
                ins ("b." ^ cc) [pr_lab(LocalLab t)] ::
                ins "b" [pr_lab(LocalLab f)] :: code
              val code = readInto fsz b (X 17) code
            in
              readInto fsz a (X 16) code
            end
          | ([a,b],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = ins "cmp" ["x16","x17"] ::
                ins "cset" ["x16",cc] ::
                ins "lsl" ["x16","x16","#1"] ::
                ins "add" ["x16","x16","#1"] :: code
              val code = readInto fsz b (X 17) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "comparison arity")
      fun fpBinary opn code =
        (case (args,res) of
          ([a,b],[d]) =>
            let
              val code = writeInto fsz d (D 30) code
              val code = ins opn ["d30","d30","d31"] :: code
              val code = readInto fsz b (D 31) code
            in
              readInto fsz a (D 30) code
            end
          | _ => unsupported "floating binary arity")
      fun fpUnary opn code =
        (case (args,res) of
          ([a],[d]) =>
            let
              val code = writeInto fsz d (D 30) code
              val code = ins opn ["d30","d30"] :: code
            in
              readInto fsz a (D 30) code
            end
          | _ => unsupported "floating unary arity")
      (* MI/LS/GT/GE all reject unordered FP comparisons. Signed integer
       * LT/LE would incorrectly treat NaN as less than another value. *)
      fun fpCompare cc code =
        (case (args,res) of
          ([a,b],[SS.FLOW_VAR_ATY(_,t,f)]) =>
            let
              val code = ins "fcmp" ["d30","d31"] ::
                ins ("b." ^ cc) [pr_lab(LocalLab t)] ::
                ins "b" [pr_lab(LocalLab f)] :: code
              val code = readInto fsz b (D 31) code
            in
              readInto fsz a (D 30) code
            end
          | ([a,b],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = ins "fcmp" ["d30","d31"] ::
                ins "cset" ["x16",cc] ::
                ins "lsl" ["x16","x16","#1"] ::
                ins "add" ["x16","x16","#1"] :: code
              val code = readInto fsz b (D 31) code
            in
              readInto fsz a (D 30) code
            end
          | _ => unsupported "floating comparison arity")
      fun overflow () code =
        let
          val code = ins "b" ["_raise_exn"] :: code
          val code = moveInto(X 28,X 0) code
        in
          addressInto(NameLab "exn_OVERFLOW",X 1) code
        end
      fun checked opn code =
        (case (args,res) of
          ([a,b],[d]) =>
            let
              val ok=localFresh()
              val code = writeInto fsz d (X 16) code
              val code = Label ok :: code
              val code = overflow() code
              val code = ins opn ["x16","x16","x17"] ::
                ins "b.vc" [pr_lab ok] :: code
              val code = readInto fsz b (X 17) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "checked integer arity")
      fun taggedBinary opn adjustment code =
        (case (args,res) of
          ([a,b],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = if adjustment then
                  let
                    val ok=localFresh()
                    val code = Label ok :: code
                    val code = overflow() code
                  in
                    ins "b.vc" [pr_lab ok] :: code
                  end
                else
                  code
              val code = ins "sub" ["x17","x17","#1"] ::
                ins opn ["x16","x16","x17"] :: code
              val code = readInto fsz b (X 17) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "tagged arithmetic arity")
      fun boxed opn code =
        (case (args,res) of
          ([buffer,a,b],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = storeInto(D 30,X 16,payload()) code
              val code = if tagged() then
                  let
                    val code = storeInto(X 17,X 16,0) code
                  in
                    constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) code
                  end
                else
                  code
              val code = readInto fsz buffer (X 16) code
              val code = ins opn ["d30","d30","d31"] :: code
              val code = loadInto(X 16,payload(),D 31) code
              val code = readInto fsz b (X 16) code
              val code = loadInto(X 16,payload(),D 30) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "boxed floating arity")
      fun tagResult () code =
        if tagged() then
          ins "lsl" ["x16","x16","#1"] ::
          ins "add" ["x16","x16","#1"] :: code
        else
          code
      fun size shift code =
        (case (args,res) of
          ([a],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = tagResult() code
              val code = ins "lsr" ["x16","x16","#" ^ Int.toString shift] :: code
              val code = loadInto(X 16,0,X 16) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "table size arity")
      fun index t i scale code =
        let
          val code = ins "add" ["x17","x16","x17, lsl #" ^ Int.toString scale] :: code
          val code = if tagged() then
              ins "asr" ["x17","x17","#1"] :: code
            else
              code
          val code = readInto fsz i (X 17) code
        in
          readInto fsz t (X 16) code
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
              val code = ins opn [if opn="ldrb" orelse opn="ldrh" then "w16" else "x16","[x17, #8]"] :: code
            in
              index t i scale code
            end
          | _ => unsupported "table subscript arity")
      fun update scale opn scalar code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = constantInto(1,X 16) code
              val code = ins opn [if opn="strb" orelse opn="strh" then "w16" else "x16","[x17, #8]"] :: code
              val code = if scalar andalso tagged() then
                  ins "lsr" ["x16","x16","#1"] :: code
                else
                  code
              val code = stackInto(false,16) code
              val code = loadInto(SP,0,X 17) code
              val code = readInto (fsz+2) v (X 16) code
              val code = storeInto(X 17,SP,0) code
              val code = stackInto(true,16) code
            in
              index t i scale code
            end
          | _ => unsupported "table update arity")
      (* Numerical representation tuples are (value bits, signed, boxed, tagged).
       * Decode numerical representations before operating, then normalize
       * and encode the result. LR is available as scratch after the prologue. *)
      fun normalize (bits,sgn,_,_) reg code =
        if bits=64 then
          code
        else
          ins (if sgn then "sbfx" else "ubfx") [r reg,r reg,"#0","#" ^ Int.toString bits] :: code
      fun getnumAt level (rep as (bits,sgn,box,tag)) a reg code =
        let
          val code = normalize rep reg code
          val code = if tag then
              ins "lsr" [r reg,r reg,"#1"] :: code
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
          val ok=localFresh()
          val code = Label ok :: code
          val code = overflow() code
        in
          ins ("b."^cc) [pr_lab ok] :: code
        end
      fun range (bits,sgn,_,_) code =
        if not sgn then
          code
        else
          if bits=64 then
            code
          else
            let
              val code = failUnless "eq" code
            in
              ins "sbfx" ["x30","x16","#0","#" ^ Int.toString bits] ::
              ins "cmp" ["x30","x16"] :: code
            end
      fun putnum (rep as (_,_,box,tag)) buffer d code =
        let
          val code = if box then
              (case buffer of
                SOME b =>
                  let
                    val code = writeInto fsz d (X 17) code
                    val code = storeInto(X 16,X 17,0) code
                    val code =
                      constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_word_boxed false)),X 16) code
                    val code = storeInto(X 16,X 17,8) code
                    val code = stackInto(false,16) code
                    val code = loadInto(SP,0,X 16) code
                    val code = readInto (fsz+2) b (X 17) code
                    val code = storeInto(X 16,SP,0) code
                  in
                    stackInto(true,16) code
                  end
                | NONE => unsupported "boxed result without buffer")
            else
              writeInto fsz d (X 16) code
          val code = if tag then
              ins "lsl" ["x16","x16","#1"] ::
              ins "add" ["x16","x16","#1"] :: code
            else
              code
        in
          normalize rep (X 16) code
        end
      fun numeric opn (rep as (bits,sgn,box,tag)) code =
        let
          val comparison = List.exists(fn x => x=opn) ["Equal","Less","Lesseq","Greater","Greatereq"]
          val (buffer,operands) = if box andalso not comparison then
                            case args of b::xs => (SOME b,xs) | _ => unsupported "numeric buffer"
                          else (NONE,args)
          val d=case res of [d] => d | _ => unsupported "numeric result"
          fun cc () = case opn of "Equal" => "eq" | "Less" => if sgn then "lt" else "lo"
                          | "Lesseq" => if sgn then "le" else "ls" | "Greater" => if sgn then "gt" else "hi"
                          | _ => if sgn then "ge" else "hs"
          fun compareResult () code =
            (case d of
              SS.FLOW_VAR_ATY(_,t,f) =>
                ins ("b."^cc()) [pr_lab(LocalLab t)] ::
                ins "b" [pr_lab(LocalLab f)] :: code
              | _ =>
                let
                  val code = writeInto fsz d (X 16) code
                in
                  ins "cset" ["x16",cc()] ::
                  ins "lsl" ["x16","x16","#1"] ::
                  ins "add" ["x16","x16","#1"] :: code
                end)
          fun arithmetic inst code =
            let
              val code = if sgn then
                  let
                    val code = range rep code
                  in
                    failUnless "vc" code
                  end
                else
                  code
            in
              ins inst ["x16","x16","x17"] :: code
            end
        in
          (case operands of
            [a] =>
              let
                val code =
                  let
                    val code = range rep
                        (putnum rep buffer d code)
                  in
                    (case opn of
                      "Neg" =>
                        let
                          val code = failUnless "vc" code
                        in
                          ins "negs" ["x16","x16"] :: code
                        end
                      | "Abs" =>
                        let
                          val done=localFresh()
                          val code = Label done :: code
                          val code = failUnless "vc" code
                        in
                          ins "cmp" ["x16","#0"] ::
                          ins "b.ge" [pr_lab done] ::
                          ins "negs" ["x16","x16"] :: code
                        end
                      | _ => unsupported("numeric unary "^opn))
                  end
              in
                getnum rep a (X 16) code
              end
            | [a,b] =>
              let
                val code = if comparison then
                    let
                      val code = compareResult() code
                    in
                      ins "cmp" ["x16","x17"] :: code
                    end
                  else
                    (case opn of
                      "Plus" =>
                        arithmetic(if sgn then "adds" else "add")
                          (putnum rep buffer d code)
                      | "Minus" =>
                        arithmetic(if sgn then "subs" else "sub")
                          (putnum rep buffer d code)
                      | "Mul" =>
                        let
                          val code = if sgn then
                              let
                                val code = range rep
                                    (putnum rep buffer d code)
                                val code = failUnless "eq" code
                              in
                                ins "cmp" ["x30","x16, asr #63"] :: code
                              end
                            else
                              putnum rep buffer d code
                          val code = ins "mul" ["x16","x16","x17"] :: code
                        in
                          if sgn then
                            ins "smulh" ["x30","x16","x17"] :: code
                          else
                            code
                        end
                      | "Andb" =>
                        ins "and" ["x16","x16","x17"] ::
                        putnum rep buffer d code
                      | "Orb" =>
                        ins "orr" ["x16","x16","x17"] ::
                        putnum rep buffer d code
                      | "Xorb" =>
                        ins "eor" ["x16","x16","x17"] ::
                        putnum rep buffer d code
                      | _ => unsupported("numeric binary "^opn))
                val code = stackInto(false,16) code
                val code = loadInto(SP,0,X 16) code
                val code = getnumAt (fsz+2) rep b (X 17) code
                val code = storeInto(X 16,SP,0) code
                val code = stackInto(true,16) code
              in
                getnum rep a (X 16) code
              end
            | _ => unsupported "numeric operands")
        end
      fun shift opn (rep as (bits,_,box,_)) code =
        let
          val (buffer,a,b,d) = case (box,args,res) of
                          (false,[a,b],[d]) => (NONE,a,b,d)
                        | (true,[buf,a,b],[d]) => (SOME buf,a,b,d)
                        | _ => unsupported "shift arity"
          val wide=localFresh()
          val done=localFresh()
          val code = putnum rep buffer d code
          val code = Label done :: code
          val code = if opn="asr" then
              ins "asr" ["x16","x16","#63"] :: code
            else
              constantInto(0,X 16) code
          val code = ins "cmp" ["x17","#"^Int.toString bits] ::
            ins "b.hs" [pr_lab wide] ::
            ins opn ["x16","x16","x17"] ::
            ins "b" [pr_lab done] ::
            Label wide :: code
          val code = if opn="asr" then
              normalize (bits,true,false,false) (X 16) code
            else
              code
          val code = stackInto(false,16) code
          val code = loadInto(SP,0,X 16) code
          val code =
            getnumAt (fsz+2) (if tagged() then (63,false,false,true) else (64,false,false,false)) b (X 17) code
          val code = storeInto(X 16,SP,0) code
          val code = stackInto(true,16) code
        in
          getnum rep a (X 16) code
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
                  let
                    val code = failUnless "ge" code
                  in
                    ins "cmp" ["x16","#0"] :: code
                  end
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
              val code = writeInto fsz d (X 16) code
              val code = storeInto(D 30,X 16,payload()) code
              val code = if tagged() then
                  let
                    val code = storeInto(X 17,X 16,0) code
                  in
                    constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) code
                  end
                else
                  code
              val code = readInto fsz buffer (X 16) code
              val code = ins opn ["d30","d30"] :: code
              val code = loadInto(X 16,payload(),D 30) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "real unary arity")
      fun realCompare cc code =
        (case args of
          [a,b] =>
            let
              val code =
                primitiveInto fsz {name=(case cc of 0 => Less_f64 | 1 => Lesseq_f64 | 2 => Greater_f64 | _ => Greatereq_f64),
                                         args=[SS.PHREG_ATY(D 30),SS.PHREG_ATY(D 31)],res=res} code
              val code = loadInto(X 16,payload(),D 31) code
              val code = readInto fsz b (X 16) code
              val code = loadInto(X 16,payload(),D 30) code
            in
              readInto fsz a (X 16) code
            end
          | _ => unsupported "real comparison arity")
      fun toInt isReal code =
        (case (args,res) of
          ([a],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = tagResult() code
              val code = ins "fcvtzs" ["x16","d30"] :: code
            in
              if isReal then
                let
                  val code = loadInto(X 16,payload(),D 30) code
                in
                  readInto fsz a (X 16) code
                end
              else
                readInto fsz a (D 30) code
            end
          | _ => unsupported "float conversion arity")
      fun tableSub scale (rep as (_,_,box,_)) code =
        let
          val (buffer,t,i,d)=case (box,args,res) of
                            (false,[t,i],[d]) => (NONE,t,i,d)
                          | (true,[b,t,i],[d]) => (SOME b,t,i,d)
                          | _ => unsupported "wide table subscript"
          val code = putnum rep buffer d code
          val code = ins "ldr" [if scale=2 then "w16" else "x16","[x17, #8]"] :: code
        in
          index t i scale code
        end
      fun tableUpdate scale rep code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = constantInto(1,X 16) code
              val code = ins "str" [if scale=2 then "w16" else "x16","[x17, #8]"] :: code
              val code = stackInto(false,16) code
              val code = loadInto(SP,0,X 17) code
              val code = getnumAt (fsz+2) rep v (X 16) code
              val code = storeInto(X 17,SP,0) code
              val code = stackInto(true,16) code
            in
              index t i scale code
            end
          | _ => unsupported "wide table update")
      fun blockSub boxed code =
        (case (boxed,args,res) of
          (false,[t,i],[d]) =>
            let
              val code = writeInto fsz d (D 30) code
              val code = loadInto(X 17,8,D 30) code
            in
              index t i 3 code
            end
          | (true,[b,t,i],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = storeInto(D 30,X 16,payload()) code
              val code = if tagged() then
                  let
                    val code = storeInto(X 17,X 16,0) code
                  in
                    constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) code
                  end
                else
                  code
              val code = readInto fsz b (X 16) code
              val code = loadInto(X 17,8,D 30) code
            in
              index t i 3 code
            end
          | _ => unsupported "float block subscript")
      fun blockUpdate boxed code =
        (case (args,res) of
          ([t,i,v],[d]) =>
            let
              val code = writeInto fsz d (X 16) code
              val code = constantInto(1,X 16) code
              val code = storeInto(D 30,X 17,8) code
              val code = stackInto(false,16) code
              val code = loadInto(SP,0,X 17) code
              val code = if boxed then
                  let
                    val code = loadInto(X 16,payload(),D 30) code
                  in
                    readInto (fsz+2) v (X 16) code
                  end
                else
                  readInto (fsz+2) v (D 30) code
              val code = storeInto(X 17,SP,0) code
              val code = stackInto(true,16) code
            in
              index t i 3 code
            end
          | _ => unsupported "float block update")
    in
      (case name of
        Plus_int63 => taggedBinary "adds" true code
        | Minus_int63 => taggedBinary "subs" true code
        | Plus_word63 => taggedBinary "add" false code
        | Minus_word63 => taggedBinary "sub" false code
        | Plus_int64ub => checked "adds" code
        | Minus_int64ub => checked "subs" code
        | Plus_real => boxed "fadd" code
        | Minus_real => boxed "fsub" code
        | Mul_real => boxed "fmul" code
        | Div_real => boxed "fdiv" code
        | Plus_f64 => fpBinary "fadd" code
        | Minus_f64 => fpBinary "fsub" code
        | Mul_f64 => fpBinary "fmul" code
        | Div_f64 => fpBinary "fdiv" code
        | Neg_f64 => fpUnary "fneg" code
        | Abs_f64 => fpUnary "fabs" code
        | Sqrt_f64 => fpUnary "fsqrt" code
        | Less_f64 => fpCompare "mi" code
        | Lesseq_f64 => fpCompare "ls" code
        | Greater_f64 => fpCompare "gt" code
        | Greatereq_f64 => fpCompare "ge" code
        | Int_to_f64 =>
          (case (args,res) of
            ([a],[d]) =>
              let
                val code = writeInto fsz d (D 30) code
                val code = ins "scvtf" ["d30","x16"] :: code
                val code = if tagged() then
                    ins "asr" ["x16","x16","#1"] :: code
                  else
                    code
              in
                readInto fsz a (X 16) code
              end
            | _ => unsupported "int to float arity")
        | Real_to_f64 =>
          (case (args,res) of
            ([a],[d]) =>
              let
                val code = writeInto fsz d (D 30) code
                val code = loadInto(X 16,payload(),D 30) code
              in
                readInto fsz a (X 16) code
              end
            | _ => unsupported "real unboxing arity")
        | F64_to_real =>
          (case (args,res) of
            ([a,b],[d]) =>
              let
                val code = writeInto fsz d (X 16) code
                val code = storeInto(D 30,X 16,payload()) code
                val code = if tagged() then
                    let
                      val code = storeInto(X 17,X 16,0) code
                    in
                      constantInto(IntInf.fromInt(Word.toInt(BackendInfo.tag_real false)),X 17) code
                    end
                  else
                    code
                val code = readInto fsz b (D 30) code
              in
                readInto fsz a (X 16) code
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
              let
                val code = writeInto fsz d (X 16) code
              in
                loadInto(X 28,8,X 16) code
              end
            | _ => unsupported "exception pointer arity")
        | Fresh_exname =>
          let
            val code = (case res of
                [d] =>
                  writeInto fsz d (X 16) code
                | _ => unsupported "exception name arity")
            val code = if parallel() then
                let
                  val retry=localFresh()
                            (* Incoming LR is already saved in the ML frame; w30 is scratch. *)
                in
                  Label retry ::
                  ins "ldaxr" ["x16","[x17]"] ::
                  ins "add" ["x16","x16","#1"] ::
                  ins "stlxr" ["w30","x16","[x17]"] ::
                  ins "cbnz" ["w30",pr_lab retry] :: code
                end
              else
                let
                  val code = storeInto(X 16,X 17,0) code
                  val code = ins "add" ["x16","x16","#1"] :: code
                in
                  loadInto(X 17,0,X 16) code
                end
          in
            addressInto(NameLab "exnameCounter",X 17) code
          end
        | Equal_ptr => compare "eq" code
        | Plus_word64ub => binary "add" code
        | Minus_word64ub => binary "sub" code
        | Mul_word64ub => binary "mul" code
        | Andb_word64ub => binary "and" code
        | Orb_word64ub => binary "orr" code
        | Xorb_word64ub => binary "eor" code
        | Equal_word64ub => compare "eq" code
        | Less_word64ub => compare "lo" code
        | Lesseq_word64ub => compare "ls" code
        | Greater_word64ub => compare "hi" code
        | Greatereq_word64ub => compare "hs" code
        | Equal_int32ub => numeric "Equal" (32,true,false,false) code
        | Equal_int63 => compare "eq" code
        | Equal_word63 => compare "eq" code
        | Less_int63 => compare "lt" code
        | Lesseq_int63 => compare "le" code
        | Greater_int63 => compare "gt" code
        | Greatereq_int63 => compare "ge" code
        | Less_word63 => compare "lo" code
        | Lesseq_word63 => compare "ls" code
        | Greater_word63 => compare "hi" code
        | Greatereq_word63 => compare "hs" code
        | Equal_int64ub => compare "eq" code
        | Less_int64ub => compare "lt" code
        | Lesseq_int64ub => compare "le" code
        | Greater_int64ub => compare "gt" code
        | Greatereq_int64ub => compare "ge" code
        | Bytetable_size => size 6 code
        | Table_size => size 6 code
        | Blockf64_size => size 9 code
        | Bytetable_sub => subscript 0 "ldrb" true code
        | Bytetable_sub_word16 => subscript 1 "ldrh" true code
        | Word_sub0 => subscript 3 "ldr" false code
        | Bytetable_update => update 0 "strb" true code
        | Bytetable_update_word16 => update 1 "strh" true code
        | Word_update0 => update 3 "str" false code
        | Equal_int31 => numeric "Equal" (31,true,false,true) code
        | Equal_int32b => numeric "Equal" (32,true,true,false) code
        | Equal_char => numeric "Equal" (8,false,false,tagged()) code
        | Equal_word8 => numeric "Equal" (8,false,false,tagged()) code
        | Equal_word31 => numeric "Equal" (31,false,false,true) code
        | Equal_word32ub => numeric "Equal" (32,false,false,false) code
        | Equal_word32b => numeric "Equal" (32,false,true,false) code
        | Equal_int64b => numeric "Equal" (64,true,true,false) code
        | Equal_word64b => numeric "Equal" (64,false,true,false) code
        | Less_int31 => numeric "Less" (31,true,false,true) code
        | Less_int32ub => numeric "Less" (32,true,false,false) code
        | Less_int32b => numeric "Less" (32,true,true,false) code
        | Less_char => numeric "Less" (8,false,false,tagged()) code
        | Less_word8 => numeric "Less" (8,false,false,tagged()) code
        | Less_word31 => numeric "Less" (31,false,false,true) code
        | Less_word32ub => numeric "Less" (32,false,false,false) code
        | Less_word32b => numeric "Less" (32,false,true,false) code
        | Less_int64b => numeric "Less" (64,true,true,false) code
        | Less_word64b => numeric "Less" (64,false,true,false) code
        | Lesseq_int31 => numeric "Lesseq" (31,true,false,true) code
        | Lesseq_int32ub => numeric "Lesseq" (32,true,false,false) code
        | Lesseq_int32b => numeric "Lesseq" (32,true,true,false) code
        | Lesseq_char => numeric "Lesseq" (8,false,false,tagged()) code
        | Lesseq_word8 => numeric "Lesseq" (8,false,false,tagged()) code
        | Lesseq_word31 => numeric "Lesseq" (31,false,false,true) code
        | Lesseq_word32ub => numeric "Lesseq" (32,false,false,false) code
        | Lesseq_word32b => numeric "Lesseq" (32,false,true,false) code
        | Lesseq_int64b => numeric "Lesseq" (64,true,true,false) code
        | Lesseq_word64b => numeric "Lesseq" (64,false,true,false) code
        | Greater_int31 => numeric "Greater" (31,true,false,true) code
        | Greater_int32ub => numeric "Greater" (32,true,false,false) code
        | Greater_int32b => numeric "Greater" (32,true,true,false) code
        | Greater_char => numeric "Greater" (8,false,false,tagged()) code
        | Greater_word8 => numeric "Greater" (8,false,false,tagged()) code
        | Greater_word31 => numeric "Greater" (31,false,false,true) code
        | Greater_word32ub => numeric "Greater" (32,false,false,false) code
        | Greater_word32b => numeric "Greater" (32,false,true,false) code
        | Greater_int64b => numeric "Greater" (64,true,true,false) code
        | Greater_word64b => numeric "Greater" (64,false,true,false) code
        | Greatereq_int31 => numeric "Greatereq" (31,true,false,true) code
        | Greatereq_int32ub => numeric "Greatereq" (32,true,false,false) code
        | Greatereq_int32b => numeric "Greatereq" (32,true,true,false) code
        | Greatereq_char => numeric "Greatereq" (8,false,false,tagged()) code
        | Greatereq_word8 => numeric "Greatereq" (8,false,false,tagged()) code
        | Greatereq_word31 => numeric "Greatereq" (31,false,false,true) code
        | Greatereq_word32ub => numeric "Greatereq" (32,false,false,false) code
        | Greatereq_word32b => numeric "Greatereq" (32,false,true,false) code
        | Greatereq_int64b => numeric "Greatereq" (64,true,true,false) code
        | Greatereq_word64b => numeric "Greatereq" (64,false,true,false) code
        | Plus_int31 => numeric "Plus" (31,true,false,true) code
        | Plus_int32ub => numeric "Plus" (32,true,false,false) code
        | Plus_int32b => numeric "Plus" (32,true,true,false) code
        | Plus_word31 => numeric "Plus" (31,false,false,true) code
        | Plus_word32ub => numeric "Plus" (32,false,false,false) code
        | Plus_word32b => numeric "Plus" (32,false,true,false) code
        | Plus_int64b => numeric "Plus" (64,true,true,false) code
        | Plus_word64b => numeric "Plus" (64,false,true,false) code
        | Minus_int31 => numeric "Minus" (31,true,false,true) code
        | Minus_int32ub => numeric "Minus" (32,true,false,false) code
        | Minus_int32b => numeric "Minus" (32,true,true,false) code
        | Minus_word31 => numeric "Minus" (31,false,false,true) code
        | Minus_word32ub => numeric "Minus" (32,false,false,false) code
        | Minus_word32b => numeric "Minus" (32,false,true,false) code
        | Minus_int64b => numeric "Minus" (64,true,true,false) code
        | Minus_word64b => numeric "Minus" (64,false,true,false) code
        | Mul_int31 => numeric "Mul" (31,true,false,true) code
        | Mul_int32ub => numeric "Mul" (32,true,false,false) code
        | Mul_int32b => numeric "Mul" (32,true,true,false) code
        | Mul_word31 => numeric "Mul" (31,false,false,true) code
        | Mul_word32ub => numeric "Mul" (32,false,false,false) code
        | Mul_word32b => numeric "Mul" (32,false,true,false) code
        | Mul_int63 => numeric "Mul" (63,true,false,true) code
        | Mul_int64ub => numeric "Mul" (64,true,false,false) code
        | Mul_int64b => numeric "Mul" (64,true,true,false) code
        | Mul_word63 => numeric "Mul" (63,false,false,true) code
        | Mul_word64b => numeric "Mul" (64,false,true,false) code
        | Neg_int31 => numeric "Neg" (31,true,false,true) code
        | Neg_int32ub => numeric "Neg" (32,true,false,false) code
        | Neg_int32b => numeric "Neg" (32,true,true,false) code
        | Neg_int63 => numeric "Neg" (63,true,false,true) code
        | Neg_int64ub => numeric "Neg" (64,true,false,false) code
        | Neg_int64b => numeric "Neg" (64,true,true,false) code
        | Abs_int31 => numeric "Abs" (31,true,false,true) code
        | Abs_int32ub => numeric "Abs" (32,true,false,false) code
        | Abs_int32b => numeric "Abs" (32,true,true,false) code
        | Abs_int63 => numeric "Abs" (63,true,false,true) code
        | Abs_int64ub => numeric "Abs" (64,true,false,false) code
        | Abs_int64b => numeric "Abs" (64,true,true,false) code
        | Andb_word31 => numeric "Andb" (31,false,false,true) code
        | Andb_word32ub => numeric "Andb" (32,false,false,false) code
        | Andb_word32b => numeric "Andb" (32,false,true,false) code
        | Andb_word63 => numeric "Andb" (63,false,false,true) code
        | Andb_word64b => numeric "Andb" (64,false,true,false) code
        | Orb_word31 => numeric "Orb" (31,false,false,true) code
        | Orb_word32ub => numeric "Orb" (32,false,false,false) code
        | Orb_word32b => numeric "Orb" (32,false,true,false) code
        | Orb_word63 => numeric "Orb" (63,false,false,true) code
        | Orb_word64b => numeric "Orb" (64,false,true,false) code
        | Xorb_word31 => numeric "Xorb" (31,false,false,true) code
        | Xorb_word32ub => numeric "Xorb" (32,false,false,false) code
        | Xorb_word32b => numeric "Xorb" (32,false,true,false) code
        | Xorb_word63 => numeric "Xorb" (63,false,false,true) code
        | Xorb_word64b => numeric "Xorb" (64,false,true,false) code
        | Neg_real => realUnary "fneg" code
        | Abs_real => realUnary "fabs" code
        | Less_real => realCompare 0 code
        | Lesseq_real => realCompare 1 code
        | Greater_real => realCompare 2 code
        | Greatereq_real => realCompare 3 code
        | Max_f64 => fpBinary "fmax" code
        | Min_f64 => fpBinary "fmin" code
        | F64_to_int => toInt false code
        | Real_to_int => toInt true code
        | Is_null =>
          (case args of
            [a] =>
              primitiveInto fsz {name=Equal_ptr,args=[a,integer 0],res=res} code
            | _ => unsupported "null arity")
        | Shift_left_word31 => shift "lsl" (31,false,false,true) code
        | Shift_left_word32ub => shift "lsl" (32,false,false,false) code
        | Shift_left_word32b => shift "lsl" (32,false,true,false) code
        | Shift_left_word63 => shift "lsl" (63,false,false,true) code
        | Shift_left_word64ub => shift "lsl" (64,false,false,false) code
        | Shift_left_word64b => shift "lsl" (64,false,true,false) code
        | Shift_right_signed_word31 => shift "asr" (31,false,false,true) code
        | Shift_right_signed_word32ub => shift "asr" (32,false,false,false) code
        | Shift_right_signed_word32b => shift "asr" (32,false,true,false) code
        | Shift_right_signed_word63 => shift "asr" (63,false,false,true) code
        | Shift_right_signed_word64ub => shift "asr" (64,false,false,false) code
        | Shift_right_signed_word64b => shift "asr" (64,false,true,false) code
        | Shift_right_unsigned_word31 => shift "lsr" (31,false,false,true) code
        | Shift_right_unsigned_word32ub => shift "lsr" (32,false,false,false) code
        | Shift_right_unsigned_word32b => shift "lsr" (32,false,true,false) code
        | Shift_right_unsigned_word63 => shift "lsr" (63,false,false,true) code
        | Shift_right_unsigned_word64ub => shift "lsr" (64,false,false,false) code
        | Shift_right_unsigned_word64b => shift "lsr" (64,false,true,false) code
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
    loadInto (SP,8*(fsz+even(!currentArgs)),X 29)
      (loadInto (SP,8*(fsz+even(!currentArgs)+1),X 30)
        (stackInto (false,8*(fsz+even(!currentArgs)+2)) (ins "ret" [] :: code)))
  (* Build statements right-to-left onto an explicit code suffix. Function
   * context is set by topInto and remains fixed throughout this traversal.
   * Fresh labels and metadata may be registered in a different order, but
   * each label is bound before emission and its frame association stays paired. *)
  fun stmtsInto fsz statements code =
    foldr (fn (ls,code) => stmtInto fsz ls code) code statements
  and stmtInto fsz ls code =
    case ls of
      LS.SCOPE {scope,...} =>
        stmtsInto fsz scope code
    | LS.LETREGION {rhos,body} =>
        let
          fun release (((_,sz),_),code) =
            case sz of
              LS.INF => internalCallInto fsz "deallocateRegion" [SS.PHREG_ATY(X 28)] code
            | LS.WORDS n =>
                if n=0 orelse not(profiling()) then code
                else internalCallInto fsz "deallocRegionFiniteProfiling" [] code
          fun enter (((place,sz),off),code) =
            case sz of
              LS.INF => internalCallInto fsz (regionAllocator place)
                [SS.PHREG_ATY(X 28),SS.REG_F_ATY off,integer(regionPolicy false place)] code
            | LS.WORDS n =>
                if n=0 orelse not(profiling()) then code
                else internalCallInto fsz "allocRegionFiniteProfiling"
                  [SS.REG_F_ATY(off+BackendInfo.objectDescSizeP+BackendInfo.finiteRegionDescSizeP),
                   integer(Effect.key_of_eps_or_rho place),integer n] code
          val code = foldr release code (rev rhos)
          val code = stmtsInto fsz body code
        in
          foldr enter code rhos
        end
    | LS.ASSIGN {pat=SS.FLOW_VAR_ATY(_,t,f),bind=LS.CON0{con,...}} =>
        ins "b" [pr_lab(LocalLab(if Con.eq(con,Con.con_TRUE) then t else f))] :: code
    | LS.ASSIGN {pat,bind=LS.ATOM{aty}} =>
        readInto fsz aty (X 16) (writeInto fsz pat (X 16) code)
    | LS.ASSIGN {pat,bind=LS.LOAD l} =>
        addressInto(DatLab l,X 16) (loadInto(X 16,0,X 16) (writeInto fsz pat (X 16) code))
    | LS.ASSIGN {pat,bind=LS.STORE(aty,l)} =>
        (dataLabel l;
          let
            val code = writeInto fsz pat (X 16) code
            val code = constantInto(1,X 16) code
            val code = storeInto(X 16,X 17,0) code
            val code = addressInto(DatLab l,X 17) code
          in
            readInto fsz aty (X 16) code
          end)
    | LS.ASSIGN {pat,bind=LS.REAL value} =>
        addressInto(realData value,X 16) (writeInto fsz pat (X 16) code)
    | LS.ASSIGN {pat,bind=LS.F64 value} =>
        addressInto(static [Directive(".double " ^ String.translate(fn #"~" => "-" | c => String.str c) value)],X 16)
          (loadInto(X 16,0,D 30)
          (writeInto fsz pat (D 30) code))
    | LS.ASSIGN {pat,bind=LS.STRING value} =>
        addressInto(stringData value,X 16) (writeInto fsz pat (X 16) code)
    | LS.ASSIGN {pat,bind=LS.RECORD{elems=[],...}} =>
        constantInto(1,X 16) (writeInto fsz pat (X 16) code)
    | LS.ASSIGN {pat,bind=LS.BLOCKF64{elems=[],...}} =>
        constantInto(1,X 16) (writeInto fsz pat (X 16) code)
    | LS.ASSIGN {pat,bind=LS.BLOCKF64{elems,alloc,tag}} =>
        recordInto fsz pat alloc [Constant(IntInf.fromInt(Word.toInt tag))] elems code
    | LS.ASSIGN {pat,bind=LS.SCRATCHMEM{bytes,alloc,tag}} =>
        if bytes=0 then
          let
            val code = writeInto fsz pat (X 16) code
          in
            constantInto(1,X 16) code
          end
        else
          let
            val code = writeInto fsz pat (X 16) code
            val code = storeInto(X 17,X 16,0) code
            val code = constantInto(IntInf.fromInt(Word.toInt tag),X 17) code
          in
            allocateInto fsz alloc (1+(bytes+7) div 8) code
          end
    | LS.ASSIGN {pat,bind=LS.RECORD{elems,alloc,tag,maybeuntag}} =>
        recordWithUntagInto maybeuntag fsz pat alloc (header tag []) elems code
    | LS.ASSIGN {pat,bind=LS.CLOS_RECORD{label,elems=elems as (_,_,rhos),alloc,f64_vars}} =>
        recordInto fsz pat alloc (header(BackendInfo.tag_clos(false,1+length(LS.smash_free elems),1+length rhos+f64_vars)) [Address(MLFunLab label)]) (LS.smash_free elems) code
    | LS.ASSIGN {pat,bind=LS.SCLOS_RECORD{elems=elems as (_,_,rhos),alloc,f64_vars}} =>
        recordInto fsz pat alloc (header(BackendInfo.tag_sclos(false,length(LS.smash_free elems),length rhos+f64_vars)) []) (LS.smash_free elems) code
    | LS.ASSIGN {pat,bind=LS.SELECT(i,a)} =>
        readInto fsz a (X 16) (loadInto(X 16,8*i+payload(),X 16) (writeInto fsz pat (X 16) code))
    | LS.ASSIGN {pat,bind=LS.DEREF{aty}} =>
        readInto fsz aty (X 16) (loadInto(X 16,payload(),X 16) (writeInto fsz pat (X 16) code))
    | LS.ASSIGN {pat,bind=LS.REF(alloc,a)} =>
        recordWithUntagInto true fsz pat alloc (header(BackendInfo.tag_ref false) []) [a] code
    | LS.ASSIGN {pat,bind=LS.ASSIGNREF(_,a,b)} =>
        stackInto(true,16)
          (readInto (fsz+2) a (X 16)
          (storeInto(X 16,SP,0)
          (readInto (fsz+2) b (X 16)
          (loadInto(SP,0,X 17)
          (storeInto(X 16,X 17,payload())
          (stackInto(false,16)
          (constantInto(1,X 16)
          (writeInto fsz pat (X 16) code))))))))
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_MEM(alloc,n,untag)} =>
        let
          val code = writeInto fsz pat (X 16) code
        in
          if untag andalso tagged() andalso not(tagPairs()) then
            let
              val(a,mode)=regionArg alloc
            in
              internalCallInto fsz "mlkit_arm64_alloc" [a,integer(n-1),integer mode,integer 1,integer(programPoint alloc)] code
            end
          else
            allocateInto fsz alloc n code
        end
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_RHO{sma}} =>
        let
          val (a,mode)=regionArg sma
          val code = writeInto fsz pat (X 16) code
          val code = (case mode of
              0 =>
                ins "and" ["x16","x16","#-3"] :: code
              | 2 =>
                ins "orr" ["x16","x16","#2"] :: code
              | _ => code)
        in
          readInto fsz a (X 16) code
        end
    | LS.ASSIGN {pat,bind=LS.CON0{con,con_kind,aux_regions,alloc}} =>
        let
          fun reset () code =
            stmtsInto fsz [LS.RESET_REGIONS{force=false,regions_for_resetting=aux_regions}] code
          fun value n code =
            let
              val code = writeInto fsz pat (X 16) code
              val code = constantInto(n,X 16) code
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
    | LS.ASSIGN {pat,bind=LS.CON1{con_kind,alloc,arg,...}} =>
        (case con_kind of
          LS.BOXED i =>
            recordInto fsz pat alloc
                       [Constant(IntInf.fromInt(Word.toInt(BackendInfo.tag_con1(false,i))))] [arg] code
          | LS.UNBOXED i =>
            let
              val code = writeInto fsz pat (X 16) code
              val code = ins "orr" ["x16","x16","x17"] :: code
              val code = constantInto(IntInf.fromInt i,X 17) code
            in
              readInto fsz arg (X 16) code
            end
          | LS.UNBOXED_HIGH i =>
            let
              val code = writeInto fsz pat (X 16) code
              val code = ins "orr" ["x16","x16","x17"] :: code
              val code = constantInto(IntInf.fromInt i*281474976710656,X 17) code
            in
              readInto fsz arg (X 16) code
            end
          | _ => unsupported "unary enumeration")
    | LS.ASSIGN {pat,bind=LS.DECON{con_kind,con_aty,...}} =>
        readInto fsz con_aty (X 16)
          ((case con_kind of
            LS.BOXED _ =>
              loadInto(X 16,8,X 16)
                (writeInto fsz pat (X 16) code)
            | LS.UNBOXED 0 =>
              writeInto fsz pat (X 16) code
            | LS.UNBOXED _ =>
              ins "and" ["x16","x16","#-4"] ::
              writeInto fsz pat (X 16) code
            | LS.UNBOXED_HIGH _ =>
              ins "and" ["x16","x16","#0xffffffffffff"] ::
              writeInto fsz pat (X 16) code
            | _ => unsupported "enumeration deconstruction"))
    | LS.HANDLE {default,handl=(handl,closure),handl_return=(returned,result,bv),offset} =>
        let
          val ret = continuation bv
          val join = localFresh()
          val off = slot fsz offset
          val code = stmtsInto fsz returned (Label join :: code)
          val code = loadInto (SP,off+16,X 16)
            (storeInto (X 16,X 28,8)
              (ins "b" [pr_lab join] :: Label ret :: writeInto fsz result (X 0) code))
          val code = stmtsInto fsz default code
          val code = addressInto (ret,X 16) (storeInto (X 16,SP,off)
            (readInto fsz closure (X 16) (storeInto (X 16,SP,off+8)
            (loadInto (X 28,8,X 16) (storeInto (X 16,SP,off+16)
            (moveInto (SP,X 16) (storeInto (X 16,SP,off+24)
            (storeInto (X 29,SP,off+32) (loadInto (X 28,0,X 16)
            (storeInto (X 16,SP,off+40) (addOffsetInto (SP,off,X 16)
            (storeInto (X 16,X 28,8) code))))))))))))
        in
          stmtsInto fsz handl code
        end
    | LS.RAISE {arg,...} =>
        argumentsInto fsz [SS.PHREG_ATY(X 28),arg] (ins "b" ["_raise_exn"] :: code)
    | LS.FLUSH (aty,off) =>
        readInto fsz aty (X 16) (storeInto(X 16,SP,slot fsz off) code)
    | LS.FETCH (aty,off) =>
        loadInto(SP,slot fsz off,X 16) (writeInto fsz aty (X 16) code)
    | LS.PRIM p =>
        primitiveInto fsz p code
    | LS.CCALL {name="spawnone",args=[arg],rhos_for_result=[],res=[res]} =>
        let
          val ()=if parallel() then () else unsupported "spawnone without -par"
          val entry=localFresh()
                      (* thread_init returns ThreadInfo*. Its leading fields are the
                       * closure and context, checked by Runtime/Layout.c. *)
          val ()=addStatic
            (
              let
                val code = ins "blr" ["x17"] ::
                  ins "bl" ["_thread_exit"] ::
                  ins "brk" ["#0"] ::
                  []
                val code = stackInto(true,16) code
                val code = constantInto(1,X 1) code
                val code = loadInto(X 0,0,X 17) code
                val code = loadInto(X 0,0,X 0) code
                val code = addOffsetInto(X 0,8,X 28) code
                val code = ins "bl" ["_thread_init"] :: code
                val code = saveCInto() code
              in
                Directive ".text" ::
                Directive ".p2align 2" ::
                Label entry :: code
              end)
          val code = writeInto fsz res (X 0) code
          val code = stackInto(false,16) code
          val code = ins "bl" ["_thread_create"] :: code
          val code = loadInto(SP,0,X 1) code
          val code = addressInto(entry,X 0) code
          val code = storeInto(X 16,SP,0) code
          val code = readInto (fsz+2) arg (X 16) code
        in
          stackInto(true,16) code
        end
    | LS.CCALL {name,args,rhos_for_result,res} =>
        if length res > 1 then unsupported "multiple C results"
        else foreignCallInto fsz name (rhos_for_result @ args) (fn _ => fn code => code)
               (resultsInto fsz res code)
    | LS.CCALL_AUTO c =>
        autoCallInto fsz c code
    | LS.EXPORT{name,clos_lab,arg=(aty,ft1,ft2)} =>
        let
          val ()=if ft1=LS.Int andalso ft2=LS.Int then () else unsupported "export other than int -> int"
          val ctx=DatLab(AddressLabels.new_named "arm64_export_ctx")
          val str=stringData name
          val ()=dataLabel clos_lab
          val ()=addStatic
            (
              let
                val code = ins "ret" [] ::
                  []
                val code = restoreCInto() code
                val code = resumeGCInto() code
                val code = ins "blr" ["x17"] :: code
                val code = stackInto(true,16) code
                val code = loadInto(X 0,payload(),X 17) code
                val code = loadInto(X 0,0,X 0) code
                val code = addressInto(DatLab clos_lab,X 0) code
                val code = if parallel() then
                    let
                      val code = moveInto(X 19,X 1) code
                      val code = addOffsetInto(X 0,8,X 28) code
                      val code = ins "bl" ["_thread_info"] :: code
                    in
                      moveInto(X 0,X 19) code
                    end
                  else
                    let
                      val code = loadInto(X 16,0,X 28) code
                      val code = addressInto(ctx,X 16) code
                    in
                      moveInto(X 0,X 1) code
                    end
                val code = deferGCInto() code
                val code = saveCInto() code
                val code = functionInto(NameLab name) code
              in
                Directive ".data" ::
                Directive ".p2align 3" ::
                Label ctx ::
                Directive ".quad 0" :: code
              end)
          val code = internalCallInto fsz "sml_regCfuns" [SS.PHREG_ATY(X 16),SS.PHREG_ATY(X 17)] code
          val code = addressInto(NameLab name,X 17) code
          val code = addressInto(str,X 16) code
          val code = storeInto(X 28,X 17,0) code
          val code = addressInto(ctx,X 17) code
          val code = storeInto(X 16,X 17,0) code
          val code = addressInto(DatLab clos_lab,X 17) code
        in
          readInto fsz aty (X 16) code
        end
    | LS.FUNCALL {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcallInto false fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res,bv=bv} code
    | LS.JMP {opr,args,reg_args,fargs,clos,res,bv} =>
        mlcallInto true fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res,bv=bv} code
    | LS.FNCALL {opr,args,clos,res,bv} =>
        mlcallInto false fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res,bv=bv} code
    | LS.FNJMP {opr,args,clos,res,bv} =>
        mlcallInto true fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res,bv=bv} code
    | LS.SWITCH_I {switch=LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[(v,yes)],no),...} =>
        if v=IntInf.fromInt BackendInfo.ml_true then flowInto fsz (t,f,yes,no) code
        else flowInto fsz (f,t,yes,no) code
    | LS.SWITCH_C (LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[((c,_),yes)],no)) =>
        if Con.eq(c,Con.con_TRUE) then flowInto fsz (t,f,yes,no) code
        else flowInto fsz (f,t,yes,no) code
    | LS.SWITCH_C (LS.SWITCH(a,[],default)) =>
        stmtsInto fsz default code
    | LS.SWITCH_C (LS.SWITCH(a,cases as ((_,kind),_)::_,default)) =>
        let
          (* Constructor selectors are already encoded by closure conversion. *)
          fun tag k = IntInf.fromInt
            (case k of LS.ENUM i => i | LS.UNBOXED i => i
                     | LS.UNBOXED_HIGH i => i | LS.BOXED i => i)
          val code = switchCodeInto fsz
            (LS.SWITCH(SS.PHREG_ATY(X 16),map (fn ((_,k),body) => (tag k,body)) cases,default)) code
          val code = case kind of
              LS.ENUM _ => code
            | LS.BOXED _ => loadInto (X 16,0,X 16) code
            | LS.UNBOXED_HIGH _ => ins "lsr" ["x16","x16","#48"] :: code
            | LS.UNBOXED _ => ins "and" ["x17","x16","#3"] ::
                ins "cmp" ["x17","#3"] :: ins "csel" ["x16","x16","x17","eq"] :: code
        in
          readInto fsz a (X 16) code
        end
    | LS.SWITCH_W {switch=LS.SWITCH(a,cases,default),precision=63} =>
        switchCodeInto fsz (LS.SWITCH(a,map(fn(n,b) => (2*n+1,b)) cases,default)) code
    | LS.SWITCH_I {switch=LS.SWITCH(a,cases,default),precision=63} =>
        switchCodeInto fsz (LS.SWITCH(a,map(fn(n,b) => (2*n+1,b)) cases,default)) code
    | LS.SWITCH_W {switch,precision} =>
        numericSwitchInto fsz false precision switch code
    | LS.SWITCH_I {switch,precision} =>
        numericSwitchInto fsz true precision switch code
    | LS.RESET_REGIONS {regions_for_resetting,force} =>
        foldr (fn (LS.IGNORE,code) => code
                | (sma,code) =>
                    let
                      val (a,mode) = regionArg sma
                    in
                      if mode=0 andalso not force then code
                      else internalCallInto fsz "mlkit_arm64_reset"
                        [a,integer(if force orelse mode=2 then 2 else 1)] code
                    end) code regions_for_resetting
    | _ => unsupported (LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls)
  and numericSwitchInto fsz signed precision (LS.SWITCH(a,cases,default)) code =
    let
      val tag = precision=31 orelse precision=63 orelse (precision=8 andalso tagged())
      val box = tagged() andalso (precision=32 orelse precision=64)
      fun value n = if tag then 2*n+1 else n
      val code = switchCodeInto fsz
        (LS.SWITCH(SS.PHREG_ATY(X 16),map (fn (n,b) => (value n,b)) cases,default)) code
      (* Int31 values from packed tables have only their encoded low 32 bits. *)
      val code = if precision=31 orelse precision=32 then
                   ins (if signed then "sxtw" else "uxtw") ["x16","w16"] :: code
                 else code
      val code = if box then loadInto (X 16,8,X 16) code else code
    in
      readInto fsz a (X 16) code
    end
  and flowInto fsz (t,f,yes,no) code =
    let
      val done = localFresh()
      val code = stmtsInto fsz no (Label done :: code)
      val code = stmtsInto fsz yes
        (ins "b" [pr_lab done] :: Label(LocalLab f) :: code)
    in
      Label(LocalLab t) :: code
    end
  and switchCodeInto fsz (LS.SWITCH(a,cases,default)) code =
    let
      val done = localFresh()
      val branches = map (fn (v,body) => (v,localFresh(),body)) cases
      val code = foldr (fn ((_,l,body),code) =>
        Label l :: stmtsInto fsz body (ins "b" [pr_lab done] :: code))
        (Label done :: code) branches
      val code = stmtsInto fsz default (ins "b" [pr_lab done] :: code)
      val code = foldr (fn ((v,l,_),code) => constantInto (v,X 17)
        (ins "cmp" ["x16","x17"] :: ins "b.eq" [pr_lab l] :: code)) code branches
    in
      readInto fsz a (X 16) code
    end
  fun entryGCInto cc code =
    if not(gc()) then code
    else
      let
        val done = localFresh()
        val ac = CallConv.get_ccf_size cc
        val rc = CallConv.get_rcf_size cc
        val skip = length(CallConv.get_spilled_region_and_float_args cc)
        val mask = foldl (fn (lv,w) =>
          case A.RI.lv_to_reg lv of
            X n => Word32.orb(w,Word32.<<(0w1,Word.fromInt n))
          | _ => w) 0w0 (CallConv.get_register_args_excluding_region_and_float_args cc)
        val registers = List.filter (fn n => n<>18) (List.tabulate(31,fn i => i))
        val floats = List.tabulate(8,fn i => i)
        val code = stackInto (false,352) (Label done :: code)
        val code = foldr (fn (n,code) => loadInto (SP,8*(31-n),X n) code) code registers
        val code = foldr (fn (i,code) => loadInto (SP,8*(39-i),D i) code) code floats
        val code = moveInto (X 28,X 0) (moveInto (SP,X 1)
          (constantInto (Word32.toLargeInt mask,X 2) (ins "bl" ["_gc"] :: code)))
        val code = foldri (fn (i,n,code) => constantInto (IntInf.fromInt n,X 16)
          (storeInto (X 16,SP,320+8*i) code)) code [skip,rc,ac]
        val code = foldr (fn (i,code) => storeInto (D i,SP,8*(39-i)) code) code floats
        val code = addOffsetInto (SP,352,X 16) (storeInto (X 16,SP,0)
          (storeInto (X 16,SP,344) (constantInto (0,X 16) (storeInto (X 16,SP,104) code))))
        val code = foldr (fn (n,code) => storeInto (X n,SP,8*(31-n)) code) code registers
        val code = stackInto (true,352) code
        val code = if extra_gc_checks() then code else ins "cbz" ["x16",pr_lab done] :: code
      in
        addressInto (NameLab "disable_gc",X 16) (loadInto (X 16,0,X 16)
          (ins "cbnz" ["x16",pr_lab done] :: addressInto (NameLab "time_to_gc",X 16)
            (loadInto (X 16,0,X 16) code)))
      end
  fun topInto (l,cc,body) code =
    let
      val ac = CallConv.get_ccf_size cc
      val () = currentArgs := ac
      val () = currentResults := CallConv.get_rcf_size cc
      val fsz = CallConv.get_frame_size cc
      val code = stmtsInto fsz body (epilogueInto fsz code)
      val code = if profiling() then internalCallInto fsz "mlkit_arm64_profile_entry"
                   [SS.PHREG_ATY(X 28),SS.REG_F_ATY(fsz-1)] code
                 else code
    in
      functionInto (MLFunLab l) (storeInto (X 29,SP,8*even ac)
        (storeInto (X 30,SP,8*(even ac+1))
          (addOffsetInto (SP,8*even ac,X 29) (entryGCInto cc (stackInto (true,8*fsz) code)))))
    end
  fun CG {main_lab,code,imports,exports,safe} =
    let
      val () = staticChunks := []
      val () = dataLabels := []
      val () = frameIndex := []
      val text = foldr (fn (LS.FUN x,code) => topInto x code
                        | (LS.FN x,code) => topInto x code) [] code
      fun data (l,code) =
        Directive ".data" :: Directive ".p2align 3" ::
        Directive(".globl " ^ pr_lab(DatLab l)) :: Label(DatLab l) :: Directive ".quad 1" :: code
      fun marker suffix code =
        let
          val l = unitSymbol main_lab suffix
        in
          Directive ".data" :: Directive ".p2align 3" ::
          Directive(".globl " ^ pr_lab l) :: Label l :: code
        end
      (* Metadata is discovered while lowering the functions. Put data before
       * text so the completed instruction stream need not be copied. *)
      val code = marker "begin" (foldr data (staticDataInto (marker "end" text)) (!dataLabels))
    in
      if not(gc()) then code
      else marker "frames"
        (Directive(".quad " ^ Int.toString(length(!frameIndex))) ::
         foldr (fn ((pc,fd),code) =>
           Directive(".quad " ^ pr_lab pc) :: Directive(".quad " ^ pr_lab fd) :: code)
           (marker "roots" (Directive(".quad " ^ Int.toString(length(!dataLabels))) ::
             foldr (fn (l,code) => Directive(".quad " ^ pr_lab(DatLab l)) :: code) code (!dataLabels)))
           (!frameIndex))
    end
  (* Runtime main enters code with the context in x0. This entry terminates
   * the process; returning foreign callbacks need a separate preserving bridge. *)
  fun registerUnitsInto labs code =
    foldr (fn (l,code) => registerUnitInto l code) code labs
  fun callUnitsInto (labs,pcs) code =
    ListPair.foldr (fn (l,pc,code) =>
      stackInto (true,16) (ins "bl" [pr_lab(MLFunLab l)] :: Label pc :: code)) code (labs,pcs)
  fun frameWordsInto pcs sentinel code =
    foldr (fn (pc,code) =>
      Directive(".quad " ^ pr_lab pc) :: Directive(".quad " ^ pr_lab sentinel) :: code) code pcs
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
        Directive ".data" :: Directive ".p2align 3" ::
        Directive(".globl " ^ pr_lab l) :: Label l ::
        foldr (fn (s,code) => Directive(".quad " ^ s) :: code) code words
      fun init (place,l) code =
        let
          val code = storeInto(X 0,X 17,0) code
          val code = addressInto(DatLab l,X 17) code
          val code = ins "bl" [pr_lab(NameLab(regionAllocator place))] ::
            ins "orr" ["x0","x0","#1"] :: code
          val code = constantInto(IntInf.fromInt(regionPolicy true place),X 2) code
          val code = moveInto(SP,X 1) code
          val code = moveInto(X 28,X 0) code
        in
          stackInto(true,8*even(BackendInfo.size_of_reg_desc())) code
        end
      val exceptions = [("MATCH","Match",BackendInfo.exn_MATCH_lab),
        ("BIND","Bind",BackendInfo.exn_BIND_lab),("OVERFLOW","Overflow",BackendInfo.exn_OVERFLOW_lab),
        ("INTERRUPT","Interrupt",BackendInfo.exn_INTERRUPT_lab),("DIV","Div",BackendInfo.exn_DIV_lab),
        ("SUBSCRIPT","Subscript",BackendInfo.exn_SUBSCRIPT_lab),("SIZE","Size",BackendInfo.exn_SIZE_lab)]
      val exceptionData = mapi (fn (i,(name,display,lab)) =>
        let
          val l = NameLab("exn_" ^ name)
          val str = stringData display
          val words = if tagged() then
              ["0x" ^ Word.toString(BackendInfo.tag_exname true),pr_lab l ^ "+16",
               "0x" ^ Word.toString(BackendInfo.tag_excon0 true),Int.toString i,pr_lab str]
            else [pr_lab l ^ "+8",Int.toString i,pr_lab str]
        in
          (l,words,lab)
        end) exceptions
      fun data code =
        foldr (fn ((_,l),code) => datum (DatLab l) ["0"] code)
          (datum (NameLab "exnameCounter") ["7"]
            (foldr (fn ((l,words,lab),code) =>
              datum l words (datum (DatLab lab) [pr_lab l] code)) code exceptionData)) globals
      val alloc = NameLab "mlkit_arm64_alloc"
      val finite=localFresh()
      val noreset=localFresh()
      val reset=NameLab "mlkit_arm64_reset"
      val resetDone=localFresh()
      val raising=NameLab "raise_exn"
      val unwind=localFresh()
      val unwound=localFresh()
      val uncaught=localFresh()
      val linkBegin=NameLab "arm64_link_begin"
      val linkEnd=NameLab "arm64_link_end"
      val sentinel=NameLab "arm64_sentinel"
      val linkFrames=NameLab "arm64_link_frames"
      val returnLabels=map(fn _ => localFresh()) labs
      fun gcInit code =
        if not(gc()) then
          code
        else
          let
            val code = storeInto(X 17,X 16,0) code
            val code = moveInto(SP,X 17) code
            val code = addressInto(NameLab "stack_bot_gc",X 16) code
            val code = ins "bl" ["_mlkit_arm64_register_image"] :: code
            val code = constantInto(0,X 5) code
            val code = constantInto(0,X 4) code
            val code = addressInto(linkEnd,X 3) code
            val code = addressInto(linkBegin,X 2) code
            val code = constantInto(IntInf.fromInt(length labs),X 1) code
            val code = addressInto(linkFrames,X 0) code
          in
            registerUnitsInto labs code
          end
      fun gcData code =
        if not(gc()) then
          code
        else
          let
            val code =
              datum linkFrames [] (frameWordsInto returnLabels sentinel code)
            val code = Directive ".quad -1" ::
              Directive ".quad 0" ::
              Directive ".quad 0" ::
              Label sentinel :: code
            val code = datum (NameLab "data_end_addr") [pr_lab linkEnd] code
          in
            datum (NameLab "data_begin_addr") [pr_lab linkBegin] code
          end

      fun profileStack () code =
        foldr (fn (name,code) => addressInto (NameLab name,X 16)
          (moveInto (SP,X 17) (storeInto (X 17,X 16,0) code)))
          code ["stackBot","maxStack","maxStackP"]
      fun initGlobals () code = foldr (fn (g,code) => init g code) code globals
      val code = gcData []
      val code = Label linkEnd :: code
      val code = staticDataInto code
      val code = data code
      val code = Directive ".data" ::
        Directive ".p2align 3" ::
        Label linkBegin :: code
      val code = ins "b" ["_uncaught_exception"] :: code
      val code = moveInto(X 27,X 3) code
      val code = loadInto(X 16,payload(),X 2) code
      val code = loadInto(X 16,8+payload(),X 1) code
      val code = loadInto(X 27,payload(),X 16) code
      val code = moveInto(X 28,X 0) code
      val code = ins "br" ["x17"] ::
        Label uncaught :: code
      val code = stackInto(true,16) code
      val code = loadInto(X 0,payload(),X 17) code
      val code = moveInto(X 27,X 1) code
      val code = loadInto(X 19,8,X 0) code
      val code = loadInto(X 19,0,X 30) code
      val code = loadInto(X 19,32,X 29) code
      val code = moveInto(X 16,SP) code
      val code = loadInto(X 19,24,X 16) code
      val code = storeInto(X 16,X 28,8) code
      val code = loadInto(X 19,16,X 16) code
      val code = if profiling() then
          let
            val code = ins "bl" ["_deallocateRegionsUntil"] :: code
            val code = moveInto(X 19,X 1) code
          in
            moveInto(X 28,X 0) code
          end
        else
          let
            val code = ins "bl" ["_deallocateRegion"] ::
              ins "b" [pr_lab unwind] ::
              Label unwound :: code
            val code = moveInto(X 28,X 0) code
            val code = ins "cmp" ["x16","x17"] ::
              ins "b.eq" [pr_lab unwound] :: code
            val code = loadInto(X 19,40,X 17) code
            val code = loadInto(X 28,0,X 16) code
          in
            Label unwind :: code
          end
      val code = ins "cbz" ["x19",pr_lab uncaught] :: code
      val code = loadInto(X 28,8,X 19) code
      val code = moveInto(X 1,X 27) code
      val code = moveInto(X 0,X 28) code
      val code = functionInto raising code
      val code = ins "tbz" ["x0","#0","1f"] ::
        ins "cmp" ["x1","#2"] ::
        ins "b.eq" ["2f"] ::
        ins "tbz" ["x0","#1","1f"] ::
        Directive "2:" ::
        ins "b" ["_resetRegion"] ::
        Directive "1:" ::
        ins "ret" [] :: code
      val code = functionInto reset code
      val code = ins "ret" [] :: code
      val code = if profiling() then
          storeInto(X 4,X 0,~16) code
        else
          code
      val code = ins "ret" [] ::
        Label finite ::
        ins "and" ["x0","x0","#-4"] :: code
      val code = stackInto(false,48) code
      val code = loadInto(SP,16,X 30) code
      val code = loadInto(SP,8,X 20) code
      val code = loadInto(SP,0,X 19) code
      val code = loadInto(SP,24,X 21) code
      val code = loadInto(SP,32,X 22) code
      val code =
        ins "bl" [if profiling() then "_allocProfiling" else if parallel() andalso unprotected() then "_alloc_unprotected" else "_alloc"] ::
        ins "sub" ["x0","x0","x21, lsl #3"] :: code
      val code = moveInto(X 22,X 2) code
      val code = moveInto(X 20,X 1) code
      val code = moveInto(X 19,X 0) code
      val code = ins "cmp" ["x2","#2"] ::
        ins "b.eq" [pr_lab resetDone] ::
        ins "cbz" ["x2",pr_lab noreset] ::
        ins "tbz" ["x0","#1",pr_lab noreset] ::
        Label resetDone ::
        ins "bl" ["_resetRegion"] ::
        Label noreset :: code
      val code = moveInto(X 1,X 20) code
      val code = moveInto(X 0,X 19) code
      val code = moveInto(X 3,X 21) code
      val code = moveInto(X 4,X 22) code
      val code = storeInto(X 22,SP,32) code
      val code = storeInto(X 21,SP,24) code
      val code = storeInto(X 30,SP,16) code
      val code = storeInto(X 20,SP,8) code
      val code = storeInto(X 19,SP,0) code
      val code = stackInto(true,48) code
      val code = ins "tbz" ["x0","#0",pr_lab finite] :: code
      val code = functionInto alloc code
      val code = ins "b" ["_terminateML"] :: code
      val code = constantInto(0,X 0) code
      val code = if repl then
          let
            val code = ins "bl" ["_repl_interp"] :: code
          in
            moveInto(X 28,X 0) code
          end
        else
          callUnitsInto (labs,returnLabels) code
      val code = gcInit code
      val code = initGlobals () code
      val code = if profiling() then
          profileStack () code
        else
          code
      val code = moveInto(X 0,X 28) code
    in
      functionInto(NameLab "code") code
    end
  fun generate_link_code args = linkCode false args
  fun generate_repl_init_code () = linkCode true ([],([],[]))
  fun generate_repl_link_code (name,labs) =
    let
      val join=localFresh()
      val handler=localFresh()
      val closure=localFresh()
      val beginData=localFresh()
      val endData=localFresh()
      val frames=localFresh()
      val sentinel=localFresh()
      val pcs=map(fn _ => localFresh()) labs
      fun metadata code =
        if not(gc()) then
          code
        else
          let
            val code = ins "bl" ["_mlkit_arm64_register_image"] :: code
            val code = constantInto(0,X 5) code
            val code = constantInto(0,X 4) code
            val code = addressInto(endData,X 3) code
            val code = addressInto(beginData,X 2) code
            val code = constantInto(IntInf.fromInt(length pcs),X 1) code
            val code = addressInto(frames,X 0) code
          in
            registerUnitsInto labs code
          end
      val code = if not(gc()) then
          []
        else
          let
            val code = frameWordsInto pcs sentinel []
          in
            Directive ".quad -1" ::
            Directive ".quad 0" ::
            Directive ".quad 0" ::
            Label sentinel ::
            Label frames :: code
          end
      val code = Directive(".quad " ^ pr_lab handler) ::
        Label endData :: code
      val code = if tagged() then
          Directive(".quad 0x" ^ Word.toString(BackendInfo.tag_clos(true,1,1))) :: code
        else
          code
      val code = ins "ret" [] ::
        Directive ".data" ::
        Directive ".p2align 3" ::
        Label beginData ::
        Label closure :: code
      val code = stackInto(false,16) code
      val code = loadInto(SP,8,X 30) code
      val code = ins "bl" ["_uncaught_exception"] :: code
      val code = moveInto(X 28,X 0) code
      val code = loadInto(X 16,payload(),X 2) code
      val code = loadInto(X 16,8+payload(),X 1) code
      val code = loadInto(X 1,payload(),X 16) code
      val code = moveInto(X 1,X 3) code
      val code = storeInto(X 30,SP,8) code
      val code = ins "ret" [] ::
        Label handler :: code
      val code = restoreCInto() code
      val code = stackInto(false,48) code
      val code = storeInto(X 16,X 28,8) code
      val code = loadInto(SP,16,X 16) code
      val code = Label join :: code
      val code = callUnitsInto (labs,pcs) code
      val code = storeInto(X 16,X 28,8) code
      val code = moveInto(SP,X 16) code
      val code = storeInto(X 16,SP,40) code
      val code = loadInto(X 28,0,X 16) code
      val code = storeInto(X 29,SP,32) code
      val code = storeInto(X 16,SP,24) code
      val code = moveInto(SP,X 16) code
      val code = storeInto(X 16,SP,16) code
      val code = loadInto(X 28,8,X 16) code
      val code = storeInto(X 16,SP,8) code
      val code = addressInto(closure,X 16) code
      val code = storeInto(X 16,SP,0) code
      val code = addressInto(join,X 16) code
      val code = stackInto(true,48) code
      val code = metadata code
      val code = loadInto(X 16,0,X 28) code
      val code = addressInto(NameLab "top_ctx",X 16) code
      val code = saveCInto() code
    in
      functionInto(NameLab name) code
    end

end
