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
