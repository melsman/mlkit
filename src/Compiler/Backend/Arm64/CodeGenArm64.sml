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
     menu=["Compiler","extra GC checks"],desc="Insert extra GC checks (not yet supported on ARM64)."}
  val alloc_protect_always = Flags.add_bool_entry
    {long="alloc_protect_always",short=NONE,item=ref false,neg=false,
     menu=["Compiler","always protect allocation"],desc="Always protect parallel allocation."}
  fun slot fsz off = 8*(fsz-off-1)
  fun read fsz aty dst =
    case aty of
      SS.PHREG_ATY src => move(src,dst)
    | SS.STACK_ATY off => load(SP,slot fsz off,dst)
    | SS.INTEGER_ATY {value,precision=_} => constant(value,dst)
    | SS.WORD_ATY {value,precision=_} => constant(value,dst)
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
  datatype target = Direct of label | Indirect of SS.Aty
  fun mlcall tail fsz target {args,reg_args,fargs,clos,res} =
    let val gp = (case clos of NONE=>[] | SOME a=>[a]) @ args @ reg_args
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
          | Indirect _ => load(SP,8*(length staged-1),X 17) @ load(X 17,0,X 17)
        val restore = if tail then
            load(SP,8*(workspace+fsz+even(!currentArgs)),X 29) @
            load(SP,8*(workspace+fsz+even(!currentArgs)+1),X 30) else []
        val transfer = case target of Direct l => [ins (if tail then "b" else "bl") [pr_lab(MLFunLab l)]]
                         | Indirect _ => [ins (if tail then "br" else "blr") ["x17"]]
    in stack(true,8*workspace) @ List.concat(mapi save staged) @ spill @ restore @ registers @ destination @
       stack(false,8*(if tail then dest else sw)) @ transfer @
       (if tail then [] else results fsz res)
    end
  fun integer n = SS.INTEGER_ATY{value=IntInf.fromInt n,precision=64}
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
  val staticData : A.inst list ref = ref []
  fun static words =
    let val l = DatLab(AddressLabels.new_named "arm64_data")
    in staticData := !staticData @ [Directive ".data",Directive ".p2align 3",Label l] @ words; l end
  fun realData value = static [Directive(".double " ^ String.translate(fn #"~"=>"-" | c=>String.str c) value)]
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
  fun allocate fsz sma words =
    let val (a,mode)=regionArg sma
    in internalCall fsz "mlkit_arm64_alloc" [a,integer words,integer mode] end
  (* Keep the destination on the stack while filling it: source operands may
   * use either scratch register during address materialization. *)
  fun record fsz pat alloc prefix elems =
    allocate fsz alloc (length prefix+length elems) @ stack(true,16) @ store(X 16,SP,0) @
    List.concat(mapi(fn(i,code)=>code @ load(SP,0,X 17) @ store(X 16,X 17,8*i)) prefix) @
    List.concat(mapi(fn(i,a)=>read (fsz+2) a (X 16) @ load(SP,0,X 17) @
      store(X 16,X 17,8*(i+length prefix))) elems) @
    load(SP,0,X 16) @ stack(false,16) @ write fsz pat (X 16)
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
        fun boxed opn = case (args,res) of
          ([buffer,a,b],[d])=>read fsz a (X 16) @ load(X 16,0,D 30) @
            read fsz b (X 16) @ load(X 16,0,D 31) @ [ins opn ["d30","d30","d31"]] @
            read fsz buffer (X 16) @ store(D 30,X 16,0) @ write fsz d (X 16)
        | _=>unsupported "boxed floating arity"
    in case name of
      Plus_int64ub => checked "adds" | Minus_int64ub => checked "subs"
    | Plus_real => boxed "fadd" | Minus_real => boxed "fsub" | Mul_real => boxed "fmul" | Div_real => boxed "fdiv"
    | Plus_f64 => fpBinary "fadd" | Minus_f64 => fpBinary "fsub"
    | Mul_f64 => fpBinary "fmul" | Div_f64 => fpBinary "fdiv"
    | Neg_f64 => fpUnary "fneg" | Abs_f64 => fpUnary "fabs" | Sqrt_f64 => fpUnary "fsqrt"
    | Less_f64 => fpCompare "mi" | Lesseq_f64 => fpCompare "ls"
    | Greater_f64 => fpCompare "gt" | Greatereq_f64 => fpCompare "ge"
    | Int_to_f64 => (case (args,res) of ([a],[d])=>read fsz a (X 16) @
        [ins "scvtf" ["d30","x16"]] @ write fsz d (D 30) | _=>unsupported "int to float arity")
    | Real_to_f64 => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ load(X 16,0,D 30) @
        write fsz d (D 30) | _=>unsupported "real unboxing arity")
    | F64_to_real => (case (args,res) of ([a,b],[d])=>
        read fsz a (X 16) @ read fsz b (D 30) @ store(D 30,X 16,0) @ write fsz d (X 16) | _=>unsupported "real boxing arity")
    | Get_ctx => (case res of [d]=>write fsz d (X 28) | _=>unsupported "context arity")
    | Exn_ptr => (case res of [d]=>load(X 28,8,X 16) @ write fsz d (X 16) | _=>unsupported "exception pointer arity")
    | Fresh_exname => address(NameLab "exnameCounter",X 17) @ load(X 17,0,X 16) @
        [ins "add" ["x16","x16","#1"]] @ store(X 16,X 17,0) @
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
        List.concat(map(fn ((_,sz),off)=>case sz of LS.INF=>
          internalCall fsz "allocateRegion" [SS.PHREG_ATY(X 28),SS.REG_F_ATY off,integer 0]
          | _=>[]) rhos) @ stmts fsz body @
        List.concat(map(fn ((_,sz),_)=>case sz of LS.INF=>
          internalCall fsz "deallocateRegion" [SS.PHREG_ATY(X 28)] | _=>[]) (rev rhos))
    | LS.ASSIGN {pat,bind=LS.ATOM{aty}} => read fsz aty (X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.LOAD l} => address(DatLab l,X 16) @ load(X 16,0,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.STORE(aty,l)} =>
        (dataLabel l; read fsz aty (X 16) @ address(DatLab l,X 17) @ store(X 16,X 17,0) @
         constant(1,X 16) @ write fsz pat (X 16))
    | LS.ASSIGN {pat,bind=LS.REAL value} => address(realData value,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.F64 value} => address(realData value,X 16) @ load(X 16,0,D 30) @ write fsz pat (D 30)
    | LS.ASSIGN {pat,bind=LS.STRING value} => address(stringData value,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.RECORD{elems=[],...}} => constant(1,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.RECORD{elems,alloc,...}} => record fsz pat alloc [] elems
    | LS.ASSIGN {pat,bind=LS.CLOS_RECORD{label,elems,alloc,...}} =>
        record fsz pat alloc [address(MLFunLab label,X 16)] (LS.smash_free elems)
    | LS.ASSIGN {pat,bind=LS.SCLOS_RECORD{elems,alloc,...}} => record fsz pat alloc [] (LS.smash_free elems)
    | LS.ASSIGN {pat,bind=LS.SELECT(i,a)} => read fsz a (X 16) @ load(X 16,8*i,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.DEREF{aty}} => read fsz aty (X 16) @ load(X 16,0,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.REF(alloc,a)} => record fsz pat alloc [] [a]
    | LS.ASSIGN {pat,bind=LS.ASSIGNREF(_,a,b)} =>
        stack(true,16) @ read (fsz+2) a (X 16) @ store(X 16,SP,0) @
        read (fsz+2) b (X 16) @ load(SP,0,X 17) @ store(X 16,X 17,0) @ stack(false,16) @
        constant(1,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_MEM(alloc,n,_)} => allocate fsz alloc n @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.PASS_PTR_TO_RHO{sma}} =>
        let val (a,mode)=regionArg sma
        in read fsz a (X 16) @ (case mode of 0=>[ins "and" ["x16","x16","#-3"]]
            | 2=>[ins "orr" ["x16","x16","#2"]] | _=>[]) @ write fsz pat (X 16) end
    | LS.ASSIGN {pat,bind=LS.CON0{con,con_kind,aux_regions,alloc}} =>
        let val reset=stmts fsz [LS.RESET_REGIONS{force=false,regions_for_resetting=aux_regions}]
            fun value n = reset @ constant(n,X 16) @ write fsz pat (X 16)
        in case con_kind of
          LS.ENUM i=>value(IntInf.fromInt(if Con.eq(con,Con.con_TRUE) orelse Con.eq(con,Con.con_FALSE) then 2*i+1 else i))
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
    | LS.HANDLE {default,handl=(handl,closure),handl_return=(returned,result,_),offset} =>
        let val ret=localFresh() val join=localFresh() val off=slot fsz offset
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
    | LS.CCALL {name,args,rhos_for_result,res} =>
        if length res > 1 then unsupported "multiple C results"
        else arguments fsz (rhos_for_result@args) @ [ins "bl" [pr_lab(NameLab name)]] @ results fsz res
    | LS.FUNCALL {opr,args,reg_args,fargs,clos,res,...} =>
        mlcall false fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res}
    | LS.JMP {opr,args,reg_args,fargs,clos,res,...} =>
        mlcall true fsz (Direct opr) {args=args,reg_args=reg_args,fargs=fargs,clos=clos,res=res}
    | LS.FNCALL {opr,args,clos,res,...} =>
        mlcall false fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res}
    | LS.FNJMP {opr,args,clos,res,...} =>
        mlcall true fsz (Indirect opr) {args=args,reg_args=[],fargs=[],clos=clos,res=res}
    | LS.SWITCH_I {switch=LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[(v,yes)],no),...} =>
        if v=IntInf.fromInt BackendInfo.ml_true then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_C (LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[((c,_),yes)],no)) =>
        if Con.eq(c,Con.con_TRUE) then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_C (LS.SWITCH(a,[],default)) => stmts fsz default
    | LS.SWITCH_C (LS.SWITCH(a,cases as ((_,kind),_)::_,default)) =>
        let fun tag k=IntInf.fromInt(case k of LS.ENUM i=>i | LS.UNBOXED i=>i | LS.UNBOXED_HIGH i=>i | LS.BOXED i=>i)
            val prepare=case kind of LS.ENUM _=>[] | LS.BOXED _=>load(X 16,0,X 16)
              | LS.UNBOXED_HIGH _=>[ins "lsr" ["x16","x16","#48"]]
              | LS.UNBOXED _=>[ins "and" ["x17","x16","#3"],ins "cmp" ["x17","#3"],ins "csel" ["x16","x16","x17","eq"]]
        in read fsz a (X 16) @ prepare @ switchCode fsz
          (LS.SWITCH(SS.PHREG_ATY(X 16),map(fn((_,k),body)=>(tag k,body)) cases,default)) end
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
  fun top (l,cc,body) =
    let val ac = CallConv.get_ccf_size cc
        val () = currentArgs := ac
        val () = currentResults := CallConv.get_rcf_size cc
        val fsz = CallConv.get_frame_size cc
    in function (MLFunLab l) @ store(X 29,SP,8*even ac) @ store(X 30,SP,8*(even ac+1)) @
       addOffset(SP,8*even ac,X 29) @ stack(true,8*fsz) @ stmts fsz body @ epilogue fsz
    end
  fun CG {main_lab,code,imports,exports,safe} =
    let val () = dataLabels := []
        val () = staticData := []
        val text = List.concat(map (fn LS.FUN x=>top x | LS.FN x=>top x) code)
        fun data l = [Directive ".data",Directive ".p2align 3",
                      Directive(".globl " ^ pr_lab(DatLab l)),Label(DatLab l),Directive ".quad 0"]
    in text @ List.concat(map data (!dataLabels)) @ !staticData end
  (* Runtime main enters code with the context in x0. This entry terminates
   * the process; returning foreign callbacks need a separate preserving bridge. *)
  fun generate_link_code (labs,_) =
    let
      val globals = map DatLab [BackendInfo.toplevel_region_withtype_top_lab,
        BackendInfo.toplevel_region_withtype_string_lab,BackendInfo.toplevel_region_withtype_pair_lab,
        BackendInfo.toplevel_region_withtype_array_lab,BackendInfo.toplevel_region_withtype_ref_lab,
        BackendInfo.toplevel_region_withtype_triple_lab]
      val () = staticData := []
      fun datum l words = [Directive ".data",Directive ".p2align 3",
        Directive(".globl " ^ pr_lab l),Label l] @ map(fn s=>Directive(".quad " ^ s)) words
      fun init l = stack(true,32) @ move(X 28,X 0) @ move(SP,X 1) @ constant(0,X 2) @
        [ins "bl" ["_allocateRegion"],ins "orr" ["x0","x0","#1"]] @ address(l,X 17) @ store(X 0,X 17,0)
      val exceptions = [("MATCH","Match",BackendInfo.exn_MATCH_lab),
        ("BIND","Bind",BackendInfo.exn_BIND_lab),("OVERFLOW","Overflow",BackendInfo.exn_OVERFLOW_lab),
        ("INTERRUPT","Interrupt",BackendInfo.exn_INTERRUPT_lab),("DIV","Div",BackendInfo.exn_DIV_lab),
        ("SUBSCRIPT","Subscript",BackendInfo.exn_SUBSCRIPT_lab),("SIZE","Size",BackendInfo.exn_SIZE_lab)]
      fun exnData(i,(name,display,lab)) =
        let val l=NameLab("exn_" ^ name) val str=stringData display
        in datum l [pr_lab l ^ "+8",Int.toString i,pr_lab str] @ datum (DatLab lab) [pr_lab l] end
      val data = List.concat(map(fn l=>datum l ["0"]) globals) @
        datum (NameLab "exnameCounter") ["7"] @ List.concat(mapi exnData exceptions)
      val alloc = NameLab "mlkit_arm64_alloc"
      val finite=localFresh() val noreset=localFresh()
      val reset=NameLab "mlkit_arm64_reset" val resetDone=localFresh()
      val raising=NameLab "raise_exn" val unwind=localFresh() val unwound=localFresh() val uncaught=localFresh()
    in function(NameLab "code") @ move(X 0,X 28) @
       List.concat(map init globals) @
       List.concat(map(fn l=>stack(true,16) @ [ins "bl" [pr_lab(MLFunLab l)]]) labs) @
       List.concat(map(fn _=>move(X 28,X 0) @ [ins "bl" ["_deallocateRegion"]]) globals) @
       constant(0,X 0) @ [ins "b" ["_terminateML"]] @
       function alloc @ [ins "tbz" ["x0","#0",pr_lab finite]] @
       stack(true,32) @ store(X 19,SP,0) @ store(X 20,SP,8) @ store(X 30,SP,16) @
       move(X 0,X 19) @ move(X 1,X 20) @
       [ins "cmp" ["x2","#2"],ins "b.eq" [pr_lab resetDone],
        ins "cbz" ["x2",pr_lab noreset],ins "tbz" ["x0","#1",pr_lab noreset],Label resetDone,
        ins "bl" ["_resetRegion"],Label noreset] @
       move(X 19,X 0) @ move(X 20,X 1) @ [ins "bl" ["_alloc"]] @
       load(SP,0,X 19) @ load(SP,8,X 20) @ load(SP,16,X 30) @ stack(false,32) @ [ins "ret" [],Label finite,
        ins "and" ["x0","x0","#-4"],ins "ret" []] @
       function reset @ [ins "tbz" ["x0","#0","1f"],ins "cmp" ["x1","#2"],
        ins "b.eq" ["2f"],ins "tbz" ["x0","#1","1f"],Directive "2:",ins "b" ["_resetRegion"],Directive "1:",ins "ret" []] @
       function raising @ move(X 0,X 28) @ move(X 1,X 27) @ load(X 28,8,X 19) @
       [ins "cbz" ["x19",pr_lab uncaught],Label unwind] @ load(X 28,0,X 16) @ load(X 19,40,X 17) @
       [ins "cmp" ["x16","x17"],ins "b.eq" [pr_lab unwound]] @ move(X 28,X 0) @
       [ins "bl" ["_deallocateRegion"],ins "b" [pr_lab unwind],Label unwound] @
       load(X 19,16,X 16) @ store(X 16,X 28,8) @ load(X 19,24,X 16) @ move(X 16,SP) @
       load(X 19,32,X 29) @ load(X 19,0,X 30) @ load(X 19,8,X 0) @ move(X 27,X 1) @
       load(X 0,0,X 17) @ stack(true,16) @ [ins "br" ["x17"],Label uncaught] @
       move(X 28,X 0) @ load(X 27,0,X 16) @ load(X 16,8,X 1) @ load(X 16,0,X 2) @ move(X 27,X 3) @
       [ins "b" ["_uncaught_exception"]] @ data @ !staticData
    end
  fun generate_repl_init_code () = unsupported "REPL initialization"
  fun generate_repl_link_code _ = unsupported "REPL linking"
end
