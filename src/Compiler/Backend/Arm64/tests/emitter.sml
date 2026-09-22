(* Exercise stack results directly: source-level tuple returns may remain
 * boxed, so they cannot establish coverage of the internal multi-result ABI. *)
structure N = BackendArm64.NativeCompile
structure G = BackendArm64.CodeGen
structure L = N.LineStmt
structure S = N.SubstAndSimplify
structure I = InstsArm64
(* Check both useful rewrites and boundaries where a similar rewrite would
 * change register width, memory semantics, metadata, or pair encodability. *)
local
  open I
  fun key (Op(n,args)) = n ^ " " ^ String.concatWith "," args
    | key (Label l) = pr_lab l ^ ":"
    | key (Directive s) = s
  fun check (name,input,expected) =
    if map key (optimise input) = map key expected then ()
    else raise Fail("ARM64 peephole: " ^ name)
  fun unchanged (name,code) = check(name,code,code)
  val l = LocalLab(AddressLabels.new_named "peep_target")
  val other = LocalLab(AddressLabels.new_named "peep_other")
  val branch = Op("b",[pr_lab other])
  val store = Op("str",["x0","[sp, #0]"])
  val storeD = Op("str",["d0","[sp, #0]"])
in
  val () = check("self move",[Op("mov",["x0","x0"])],[])
  val () = unchanged("32-bit self move clears upper bits",[Op("mov",["w0","w0"])])
  val () = unchanged("FP self move may clear upper bits",[Op("fmov",["d0","d0"])])
  val () = check("branch to next",[Op("b",[pr_lab l]),Label l],[Label l])
  val () = check("conditional to next",[Op("cbz",["x0",pr_lab l]),Label l],[Label l])
  val () = check("invert condition",[Op("b.eq",[pr_lab l]),branch,Label l],
    [Op("b.ne",[pr_lab other]),Label l])
  val () = check("invert bit test",[Op("tbz",["x0","#3",pr_lab l]),branch,Label l],
    [Op("tbnz",["x0","#3",pr_lab other]),Label l])
  val () = check("forward stack load",[store,Op("ldr",["x1","[sp, #0]"])],
    [store,Op("mov",["x1","x0"])])
  val () = check("forward FP stack load",[storeD,Op("ldr",["d1","[sp, #0]"])],
    [storeD,Op("fmov",["d1","d0"])])
  val () = check("pair stores",[store,Op("str",["x1","[sp, #8]"])],
    [Op("stp",["x0","x1","[sp, #0]"])])
  val () = check("pair loads at boundary",
    [Op("ldr",["d0","[sp, #504]"]),Op("ldr",["d1","[sp, #512]"])],
    [Op("ldp",["d0","d1","[sp, #504]"])])
  val () = unchanged("pair out of range",
    [Op("ldr",["x0","[sp, #512]"]),Op("ldr",["x1","[sp, #520]"])])
  val () = unchanged("pair duplicate load destination",
    [Op("ldr",["x0","[sp, #0]"]),Op("ldr",["x0","[sp, #8]"])])
  val () = unchanged("unknown memory",
    [Op("str",["x0","[x2, #0]"]),Op("ldr",["x1","[x2, #0]"])])
  val () = unchanged("mixed register widths",[store,Op("ldr",["w1","[sp, #0]"])])
  val () = unchanged("mixed register banks",[store,Op("ldr",["d1","[sp, #0]"])])
  val () = unchanged("metadata barrier",[store,Directive ".p2align 3",Op("str",["x1","[sp, #8]"])])
  val () = unchanged("label barrier",[store,Label l,Op("str",["x1","[sp, #8]"])])
  val () = unchanged("writeback",[Op("str",["x0","[sp, #0]!"]),Op("ldr",["x1","[sp, #0]!"])])
  val () = List.app (fn lv =>
    case I.RI.lv_to_reg lv of
      X n => if I.RI.is_callee_save_ccall lv = (n >= 19 andalso n <= 26)
             then () else raise Fail "ARM64 C-preserved allocation register"
    | _ => raise Fail "ARM64 integer palette") I.RI.caller_save_phregs
end
(* Range proofs include worst-case padding and inline data. Unknown sizes and
 * cross-section targets must never justify a short branch or ADR. *)
local
  open I
  val target = LocalLab(AddressLabels.new_named "range_target")
  val name = pr_lab target
  fun opCount name code = length(List.filter
    (fn Op(n,_) => n = name | _ => false) code)
  fun expect (name,ok) = if ok then () else raise Fail("ARM64 relaxation: " ^ name)
  fun operands opn = if opn = "tbz" then ["x0","#0",name] else ["x0",name]
  fun forward opn gap = relax [Op(opn,operands opn),Directive(".space " ^ Int.toString gap),Label target]
  fun backward opn gap = relax [Label target,Directive(".space " ^ Int.toString gap),Op(opn,operands opn)]
  val address = [Op("adrp",["x30",name ^ "@PAGE"]),
                 Op("add",["x30","x30",name ^ "@PAGEOFF"])]
  val table = [Op("cbz",["x0",name]),Directive ".p2align 3",
               Directive ".quad 1,2,3",Label target]
in
  val () = expect("near conditional",opCount "b" (forward "cbz" 16) = 0)
  val () = expect("near bit test",opCount "b" (forward "tbz" 16) = 0)
  val () = expect("forward conditional fits bound",opCount "b" (forward "cbz" 1048564) = 0)
  val () = expect("forward conditional too far",opCount "b" (forward "cbz" 1048576) = 1)
  val () = expect("backward conditional boundary",opCount "b" (backward "cbz" 1048576) = 0)
  val () = expect("backward conditional too far",opCount "b" (backward "cbz" 1048580) = 1)
  val () = expect("forward bit-test fits bound",opCount "b" (forward "tbz" 32756) = 0)
  val () = expect("forward bit-test too far",opCount "b" (forward "tbz" 32768) = 1)
  val () = expect("backward bit-test boundary",opCount "b" (backward "tbz" 32768) = 0)
  val () = expect("backward bit-test too far",opCount "b" (backward "tbz" 32772) = 1)
  val () = expect("inline table",opCount "b" (relax table) = 0)
  val () = expect("alignment can exceed range",opCount "b" (relax
    [Op("cbz",["x0",name]),Directive ".space 1048564",Directive ".p2align 4",Label target]) = 1)
  val () = expect("unknown directive",opCount "b" (relax
    [Op("cbz",["x0",name]),Directive ".fill 100,4,0",Label target]) = 1)
  val () = expect("section switch",opCount "b" (relax
    [Op("cbz",["x0",name]),Directive ".data",Label target]) = 1)
  val () = expect("text span across data",opCount "b" (relax
    [Directive ".text",Op("cbz",["x0",name]),Directive ".data",
     Directive ".space 2000000",Directive ".text",Label target]) = 0)
  val () = expect("unknown section cannot alias text",opCount "b" (relax
    [Directive ".text",Op("cbz",["x0",name]),Directive ".section __TEXT,__const",Label target]) = 1)
  val () = expect("unknown size invalidates saved text offset",opCount "b" (relax
    [Directive ".text",Op("cbz",["x0",name]),Directive ".fill 100,4,0",
     Directive ".text",Label target]) = 1)
  val () = expect("near return address",opCount "adr" (relax(address @ [Label target])) = 1)
  val () = expect("distant address",opCount "adr" (relax
    (address @ [Directive ".space 1048576",Label target])) = 0)
  val () = expect("cross-section address",opCount "adr" (relax
    (address @ [Directive ".data",Label target])) = 0)
end
(* Both register palettes can be used for allocation across C calls. *)
val () = List.app (fn lv => case I.RI.lv_to_reg lv of
    I.X n => if List.exists (fn r => r = n) AbiArm64.reservedGPRs
             then raise Fail "reserved ARM register in allocation palette" else ()
  | _ => raise Fail "non-GPR in integer allocation palette")
  (I.RI.caller_save_phregs @ I.RI.callee_save_ccall_phregs)
val regs = {arg_regs = I.RI.args_phreg,arg_fregs = I.RI.args_phfreg,res_regs = I.RI.res_phreg}
fun fresh n = List.tabulate(n,fn _ => Lvars.newLvar())
fun convention (a,r,locals) =
  let val (cc,_,_) = CallConv.resolve_cc FrameLayout.arm64 regs
        (CallConv.mk_cc{clos = NONE,args = fresh a,reg_args = [],fargs = [],res = fresh r})
  in CallConv.add_frame_size(cc,locals)
  end
fun x n = S.PHREG_ATY(I.X n)
fun num n = S.WORD_ATY{value = IntInf.fromInt n,precision = 64}
fun assign (a,b) = L.ASSIGN{pat = a,bind = L.ATOM{aty = b}}
fun emitCase (count,grow,resolved) =
  let val main = AddressLabels.new_named "result_main"
      val target = AddressLabels.new_named "result_target"
      val tail = AddressLabels.new_named "result_tail"
      val fsz = FrameLayout.alignFrame FrameLayout.arm64 {locals = count,call = 0}
      val dests = List.tabulate(count,fn i => S.STACK_ATY i)
      val targetArgs = if grow then 11 else 9
      val callerArgs = if grow then 9 else 11
      val callee = convention(targetArgs,count,0)
      val spill = CallConv.get_spilled_res_with_offsets callee
      val outputs = List.tabulate(count,fn i => if i<3 then x i else S.STACK_ATY(#2(List.nth(spill,i-3))))
      val args = List.tabulate(targetArgs,fn i => num i)
      val actual = List.tabulate(callerArgs,fn i => num i)
      val check = List.concat(List.tabulate(count,fn i =>
        [L.CCALL{name = "putchar",args = [List.nth(dests,i)],rhos_for_result = [],res = []}]))
      fun place values =
        if resolved then List.take(values,Int.min(8,length values)) else []
      fun operands values =
        if resolved then List.tabulate(length values,fn i =>
          if i<8 then x i else List.nth(values,i)) else values
      fun setup values = ListPair.map assign
        (List.tabulate(length(place values),x),place values)
      val returns = if resolved then List.tabulate(count,fn i =>
        if i<3 then x i else List.nth(dests,i)) else dests
      val fetch = if resolved then ListPair.map assign
        (List.take(dests,3),List.tabulate(3,x)) else []
      val code = [L.FUN(main,convention(0,0,fsz),
          setup actual @
          (L.FUNCALL{opr = tail,args = operands actual,reg_args = [],fargs = [],clos = NONE,res = returns,bv = []}::
           fetch @ check)),
        L.FUN(tail,convention(callerArgs,count,0),
          setup args @
          [L.JMP{opr = target,args = operands args,reg_args = [],fargs = [],clos = NONE,res = outputs,bv = []}]),
        L.FUN(target,callee,ListPair.map assign(outputs,List.tabulate(count,fn i => num(65+i))))]
      val base = "results" ^ Int.toString count ^ (if grow then "" else "-shrink") ^
                 (if resolved then "-resolved" else "")
  in G.emit(G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false},base ^ ".s");
     G.emit(G.generate_link_code([main],([],[])),base ^ "-link.s")
  end
val () = List.app (fn n => List.app (fn resolved =>
  (emitCase(n,true,resolved);emitCase(n,false,resolved))) [false,true]) [4,5,6,7]

(* Record filling must preserve a source that aliases the destination. Also
 * exercise a non-aliasing register destination and a spilled destination. *)
local
  val main = AddressLabels.new_named "record_destinations"
  fun sample (i,dst) =
    let val fields = [x 19,S.STACK_ATY 0]
        fun put field =
          [L.ASSIGN{pat = x 0,bind = L.SELECT(field,dst)},
           L.CCALL{name = "putchar",args = [x 0],rhos_for_result = [],res = []}]
    in
      [assign(x 19,num(65+i)),assign(S.STACK_ATY 0,num(97+i)),
       L.ASSIGN{pat = dst,bind = L.RECORD{elems = fields,
         alloc = L.ATTOP_LF(S.REG_F_ATY 15,0),tag = BackendInfo.tag_record(false,2),
         maybeuntag = false}}] @ put 0 @ put 1
    end
  val body = List.concat(CodeGenUtilArm64.mapi sample [x 19,x 20,S.STACK_ATY 1])
  val code = [L.FUN(main,convention(0,0,16),body)]
in
  val () = G.emit(G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false},
                  "record-destinations.s")
  val () = G.emit(G.generate_link_code([main],([],[])),"record-destinations-link.s")
end

(* Identity wrappers must not touch the frame; reordered arguments must still
 * be shuffled. Self loops reuse only an empty local frame, with register args.
 * Keep forced-polling and profiling entries on the ordinary path. *)
local
  val main = AddressLabels.new_named "tail_frames"
  val target = AddressLabels.new_named "tail_target"
  val wrapper = AddressLabels.new_named "tail_identity"
  val reorder = AddressLabels.new_named "tail_reorder"
  val loop = AddressLabels.new_named "tail_loop"
  val locals = AddressLabels.new_named "tail_locals"
  val stackLoop = AddressLabels.new_named "tail_stack"
  val swapLoop = AddressLabels.new_named "tail_swap"
  val fpTarget = AddressLabels.new_named "tail_fp_target"
  val fpWrapper = AddressLabels.new_named "tail_fp_wrapper"
  val fp = S.PHREG_ATY(I.D 0)
  fun tail lab args fargs = L.JMP{opr = lab,args = args,reg_args = [],fargs = fargs,
                                 clos = NONE,res = [x 0],bv = []}
  fun call lab args fargs = L.FUNCALL{opr = lab,args = args,reg_args = [],fargs = fargs,
                                    clos = NONE,res = [x 0],bv = []}
  val put = L.CCALL{name = "putchar",args = [x 0],rhos_for_result = [],res = []}
  fun loopBody lab = [L.SWITCH_W{precision = 64,
    switch = L.SWITCH(x 0,[(0,[assign(x 0,x 1)])],
      [L.PRIM{name = PrimName.Minus_int64ub,args = [x 0,num 1],res = [x 0]},
       tail lab [x 0,x 1] []])}]
  val (fpcc,_,_) = CallConv.resolve_cc FrameLayout.arm64 regs
    (CallConv.mk_cc{clos = NONE,args = fresh 1,reg_args = [],fargs = fresh 1,res = fresh 1})
  val fpcc = CallConv.add_frame_size(fpcc,0)
  val stackcc = convention(9,1,0)
  val stackArg = S.STACK_ATY(#2(hd(CallConv.get_spilled_args_with_offsets stackcc)))
  fun fallbackBody lab args = [L.SWITCH_W{precision = 64,
    switch = L.SWITCH(x 0,[(0,[assign(x 0,x 1)])],
      [L.PRIM{name = PrimName.Minus_int64ub,args = [x 0,num 1],res = [x 0]},
       tail lab args []])}]
  val code = [L.FUN(main,convention(0,0,0),
      [call wrapper [num 0,num 65] [],put,
       call reorder [num 66,num 0] [],put,
       call loop [num 100000,num 67] [],put,
       call locals [num 100000,num 68] [],put,
       assign(fp,num 69),call fpWrapper [num 0] [fp],put,
       call stackLoop ([num 100000,num 70] @ List.tabulate(7,fn _ => num 0)) [],put,
       call swapLoop [num 100001,num 0,num 71] [],put]),
    L.FUN(wrapper,convention(2,1,2),[L.SCOPE{pat = [],scope = [L.LETREGION{rhos = [],body = [tail target [x 0,x 1] []]}]}]),
    L.FUN(reorder,convention(2,1,0),[tail target [x 1,x 0] []]),
    L.FUN(target,convention(2,1,0),[assign(x 0,x 1)]),
    L.FUN(loop,convention(2,1,0),loopBody loop),
    L.FUN(locals,convention(2,1,2),loopBody locals),
    L.FUN(fpWrapper,fpcc,[tail fpTarget [x 0] [fp]]),
    L.FUN(fpTarget,fpcc,[assign(x 0,fp)]),
    L.FUN(stackLoop,stackcc,fallbackBody stackLoop (List.tabulate(8,x) @ [stackArg])),
    L.FUN(swapLoop,convention(3,1,0),fallbackBody swapLoop [x 0,x 2,x 1])]
  fun generate () = G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false}
  fun after lab [] = raise Fail "missing frame-test function"
    | after lab (I.Label l::rest) = if I.pr_lab l = I.pr_lab(I.MLFunLab lab) then rest else after lab rest
    | after lab (_::rest) = after lab rest
  fun direct lab target code =
    case after lab code of
      I.Op("b",[to])::_ => to = I.pr_lab(I.MLFunLab target)
    | _ => false
  fun branches lab code = List.exists
    (fn I.Op("b",[to]) => to = I.pr_lab(I.MLFunLab lab) | _ => false) code
  fun expect (name,ok) = if ok then () else raise Fail("ARM64 tail frames: " ^ name)
  val normal = generate()
  val () = expect("identity wrapper",direct wrapper target normal)
  val () = expect("FP identity wrapper",direct fpWrapper fpTarget normal)
  val () = expect("argument permutation",not(direct reorder target normal))
  val () = expect("self-loop entry",not(branches loop normal))
  val () = expect("local frame fallback",branches locals normal)
  val () = expect("stack argument fallback",branches stackLoop normal)
  val () = expect("self-call shuffle fallback",branches swapLoop normal)
  val () = G.emit(normal,"tail-frames.s")
  val () = G.emit(G.generate_link_code([main],([],[])),"tail-frames-link.s")
  fun conservative flag =
    let
      val () = Flags.turn_on "garbage_collection"
      val () = Flags.turn_on flag
      val assembly = generate()
      val () = Flags.turn_off flag
      val () = Flags.turn_off "garbage_collection"
    in
      expect(flag ^ " wrapper",not(direct wrapper target assembly));
      expect(flag ^ " loop",branches loop assembly)
    end
in
  val () = List.app conservative ["extra_gc_checks","region_profiling"]
end

(* Region calls have compile-time save sets. Substitute hostile helpers that
 * clobber every C-volatile ML register, check SP alignment, and touch the passed
 * region descriptor. This exercises empty, odd and mixed-bank save sets. *)
local
  open CodeGenUtilArm64
  val main = AddressLabels.new_named "region_live"
  val target = AddressLabels.new_named "region_live_return"
  val flowTarget = AddressLabels.new_named "region_live_flow"
  val yes = AddressLabels.new_named "region_live_yes"
  val no = AddressLabels.new_named "region_live_no"
  val flow = S.FLOW_VAR_ATY(Lvars.newLvar(),yes,no)
  val (rho,_) = Effect.freshRhoWithTy(Effect.TOP_RT,Effect.emptyCone)
  fun region off body = L.LETREGION{rhos = [((rho,L.INF),off)],body = body}
  fun aty r = S.PHREG_ATY r
  fun put off = L.CCALL{name = "putchar",args = [S.STACK_ATY off],rhos_for_result = [],res = []}
  fun sample (inputs,outputs,firstChar) =
    let
      val setup = CodeGenUtilArm64.mapi (fn (i,r) => assign(aty r,num(firstChar+i))) inputs
      val capture = CodeGenUtilArm64.mapi (fn (i,r) => L.FLUSH(aty r,i)) inputs
      val produce = CodeGenUtilArm64.mapi
        (fn (i,r) => assign(aty r,num(firstChar+length inputs+i))) outputs
      val finish = CodeGenUtilArm64.mapi (fn (i,r) => L.FLUSH(aty r,length inputs+i)) outputs
    in setup @ [region 15 (capture @ produce)] @ finish @
       List.tabulate(length inputs+length outputs,put)
    end
  val body = sample([I.X 4],[I.X 5],65) @
    sample([I.D 0,I.D 16,I.D 29],[I.D 1,I.D 17,I.D 28],67) @
    sample([I.X 4,I.D 16,I.D 8,I.X 19],[I.D 7],73) @
    [region 15 [region 23 []],put 15,
     L.FUNCALL{opr = target,args = [],reg_args = [],fargs = [],clos = NONE,res = [x 0],bv = []},
     L.CCALL{name = "putchar",args = [x 0],rhos_for_result = [],res = []},
     L.FUNCALL{opr = flowTarget,args = [num 80],reg_args = [],fargs = [],clos = NONE,res = [x 0],bv = []},
     L.CCALL{name = "putchar",args = [x 0],rhos_for_result = [],res = []}]
  val flowBody = [assign(x 4,x 0),
    region 15 [L.ASSIGN{pat = flow,bind = L.CON0{con = Con.con_TRUE,
      con_kind = L.ENUM 1,aux_regions = [],alloc = L.IGNORE}}],
    assign(x 4,num 0),
    L.SWITCH_C(L.SWITCH(flow,[((Con.con_TRUE,L.ENUM 1),[assign(x 0,x 4)])],
      [assign(x 0,num 0)]))]
  val code = [L.FUN(main,convention(0,0,32),body),
              L.FUN(target,convention(0,1,16),[region 15 [assign(x 0,num 79)]]),
              L.FUN(flowTarget,convention(1,1,16),flowBody)]
  fun redirect (I.Op("bl",["_allocateRegion"])) = I.Op("bl",["_live_region_enter"])
    | redirect (I.Op("bl",["_deallocateRegion"])) = I.Op("bl",["_live_region_exit"])
    | redirect i = i
  val volatile = List.tabulate(16,I.X) @ List.tabulate(8,I.D) @
                 List.tabulate(14,fn i => I.D(i+16))
  fun helper (name,enter) =
    function(I.NameLab name) @ [ins "mov" ["x16","sp"],ins "tst" ["x16","#15"],
      ins "b.eq" ["1f"],ins "brk" ["#1"],I.Directive "1:"] @
    (if enter then constant(78,I.X 16) @ store(I.X 16,I.X 1,0) else []) @
    constant(0,I.X 16) @ List.concat(map (fn r => move(I.X 16,r)) volatile) @ [ins "ret" []]
in
  val () = G.emit(map redirect
    (G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false}) @
    helper("live_region_enter",true) @ helper("live_region_exit",false),"region-live.s")
  val () = G.emit(G.generate_link_code([main],([],[])),"region-live-link.s")
end

(* Nested statement emission must preserve the following code suffix exactly
 * once, including through empty region scopes. Check it by executing AB. *)
local
  val main = AddressLabels.new_named "nested_scopes"
  fun put n = L.CCALL{name = "putchar",args = [num n],rhos_for_result = [],res = []}
  fun nest (0,body) = body
    | nest (n,body) =
        nest(n-1,[L.SCOPE{pat = [],scope = [L.LETREGION{rhos = [],body = body}]}])
  val body = nest(2000,[put 65]) @ [put 66]
  val code = [L.FUN(main,convention(0,0,0),body)]
in
  val () = G.emit(G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false},
                  "nested-scopes.s")
  val () = G.emit(G.generate_link_code([main],([],[])),"nested-scopes-link.s")
end

(* Exercise the production scalar C-call emitter, including the ABI types not
 * exposed by the source-language automatic FFI. Values are raw IEEE bits. *)
local
  open CodeGenUtilArm64
  structure B = AbiArm64
  fun bits s = valOf(IntInf.fromString s)
  fun probe (name,target,fixed,variadic,values) =
    function(NameLab name) @ stack(true,16) @ store(X 29,SP,0) @ store(X 30,SP,8) @ move(SP,X 29) @
    scalarCall{name = target,fixed = fixed,variadic = variadic,protectGC = false,
      loadArgument = fn(i,_) => constant(List.nth(values,i),X 16)} @
    load(SP,0,X 29) @ load(SP,8,X 30) @ stack(false,16) @ [ins "ret" []]
  val doubles = map bits ["4607182418800017408","4611686018427387904","4613937818241073152",
    "4616189618054758400","4617315517961601024","4618441417868443648","4619567317775286272",
    "4620693217682128896","4621256167635550208","4621819117588971520"]
  val mixed = probe("arm64_mixed_probe","arm64_mixed_check",
    List.tabulate(8,fn _ => B.I64) @ [B.I8,B.U8] @ List.tabulate(10,fn _ => B.F64) @ [B.I16],[],
    map IntInf.fromInt [1,2,3,4,5,6,7,8,~3,250] @ doubles @ [~1234])
  val variadic = probe("arm64_variadic_probe","arm64_variadic_check",[B.I32],
    [B.I8,B.F32,B.F64,B.I64],map bits ["4","~3","1069547520","4612811918334230528","99"])
  val floatResult = probe("arm64_float_result_probe","arm64_float_result_check",[B.F64],[],[hd doubles])
  val narrow = probe("arm64_narrow_probe","arm64_narrow_check",[B.I8,B.U16,B.I32],[],[~5,60000,17])
in val () = G.emit(mixed @ variadic @ floatResult @ narrow,"scalar-calls.s") end

(* Exercise forward/backward branches beyond both short branch ranges. *)
local
  open CodeGenUtilArm64
  val done = LocalLab(AddressLabels.new_named "far_done")
  val start = LocalLab(AddressLabels.new_named "far_start")
  val next = LocalLab(AddressLabels.new_named "far_next")
  val last = LocalLab(AddressLabels.new_named "far_last")
  val code = function(NameLab "main") @ constant(0,X 0) @ [ins "b" [pr_lab start],
    Label done,ins "ret" [],Directive ".space 1100000",Label start,
    ins "cbz" ["x0",pr_lab next],ins "brk" ["#1"],Directive ".space 1100000",Label next,
    ins "tbz" ["x0","#0",pr_lab last],ins "brk" ["#2"],Directive ".space 65536",Label last,
    ins "cmp" ["x0","#0"],ins "b.eq" [pr_lab done],ins "brk" ["#3"]]
in val () = G.emit(code,"long-branches.s") end

(* Direct allocation probes cover exact page boundaries, expansion, large
 * objects, dynamic finite regions, tag-free payloads, and both reset paths. *)
local
  fun raw n = S.INTEGER_ATY{value = IntInf.fromInt n,precision = 0}
  val live = [I.X 0,I.X 4,I.X 8,I.X 15,I.X 21,I.X 26,I.D 0,I.D 7,I.D 15,I.D 29]
  fun probeMode (suffix,gc,gen) =
    let
      val () = List.app Flags.turn_off
        ["garbage_collection","generational_garbage_collection","tag_values","region_profiling"]
      val () = if gc then List.app Flags.turn_on ["garbage_collection","tag_values"] else ()
      val () = if gen then Flags.turn_on "generational_garbage_collection" else ()
      val main = AddressLabels.new_named "allocation_paths"
      fun sample id =
        let
          val region = x 19
          val sma = if id = 5 orelse id = 15 orelse id = 16 then L.SAT_FF(region,0)
                    else L.ATTOP_FF(region,0)
          val words = case id of 1 => 2 | 2 => 1 | 3 => 2048 | 6 => 4 | 7 => 4 | _ => 3
          val operation = if id >= 10 then
              L.RESET_REGIONS{force = id<>15 andalso id<>16,regions_for_resetting = [sma]}
            else L.ASSIGN{pat = x 20,bind = L.PASS_PTR_TO_MEM(sma,words,id = 6 orelse id = 7)}
          val seeds = CodeGenUtilArm64.mapi (fn (i,reg) => assign(S.PHREG_ATY reg,raw(100+i))) live
          val setup = L.CCALL{name = "allocation_prepare",args = [x 28,raw id],rhos_for_result = [],res = [region]}
          val check = L.CCALL{name = "allocation_check",
            args = [x 28,region,(if id >= 10 then raw 0 else x 20),raw id] @ map S.PHREG_ATY live,
            rhos_for_result = [],res = []}
        in
          setup :: seeds @ [operation,check]
        end
      val ids = [0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,17]
      val code = [L.FUN(main,convention(0,0,0),List.concat(map sample ids))]
    in
      G.emit(G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false},
             "allocation-" ^ suffix ^ ".s");
      G.emit(G.generate_link_code([main],([],[])),"allocation-" ^ suffix ^ "-link.s")
    end
in
  val () = List.app probeMode [("plain",false,false),("gc",true,false),("gengc",true,true)]
  val () = List.app Flags.turn_off ["garbage_collection","generational_garbage_collection","tag_values"]
end
