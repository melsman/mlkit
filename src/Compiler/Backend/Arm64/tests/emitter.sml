(* Exercise stack results directly: source-level tuple returns may remain
 * boxed, so they cannot establish coverage of the internal multi-result ABI. *)
structure N = BackendArm64.NativeCompile
structure G = BackendArm64.CodeGen
structure L = N.LineStmt
structure S = N.SubstAndSimplify
structure I = InstsArm64
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
fun emitCase (count,grow) =
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
      val code = [L.FUN(main,convention(0,0,fsz),
          L.FUNCALL{opr = tail,args = actual,reg_args = [],fargs = [],clos = NONE,res = dests,bv = []}::check),
        L.FUN(tail,convention(callerArgs,count,0),
          [L.JMP{opr = target,args = args,reg_args = [],fargs = [],clos = NONE,res = outputs,bv = []}]),
        L.FUN(target,callee,ListPair.map assign(outputs,List.tabulate(count,fn i => num(65+i))))]
      val base = "results" ^ Int.toString count ^ (if grow then "" else "-shrink")
  in G.emit(G.CG{main_lab = main,code = code,imports = ([],[]),exports = ([],[]),safe = false},base ^ ".s");
     G.emit(G.generate_link_code([main],([],[])),base ^ "-link.s")
  end
val () = List.app (fn n => (emitCase(n,true);emitCase(n,false))) [4,5,6,7]

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
