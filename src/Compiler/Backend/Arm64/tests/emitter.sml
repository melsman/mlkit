(* Exercise stack results directly: source-level tuple returns may remain
 * boxed, so they cannot establish coverage of the internal multi-result ABI. *)
structure N = BackendArm64.NativeCompile
structure G = BackendArm64.CodeGen
structure L = N.LineStmt
structure S = N.SubstAndSimplify
structure I = InstsArm64
val regs = {arg_regs=I.RI.args_phreg,arg_fregs=I.RI.args_phfreg,res_regs=I.RI.res_phreg}
fun fresh n = List.tabulate(n,fn _=>Lvars.newLvar())
fun convention (a,r,locals) =
  let val (cc,_,_) = CallConv.resolve_cc FrameLayout.arm64 regs
        (CallConv.mk_cc{clos=NONE,args=fresh a,reg_args=[],fargs=[],res=fresh r})
  in CallConv.add_frame_size(cc,locals) end
fun x n = S.PHREG_ATY(I.X n)
fun num n = S.WORD_ATY{value=IntInf.fromInt n,precision=64}
fun assign (a,b) = L.ASSIGN{pat=a,bind=L.ATOM{aty=b}}
fun emitCase (count,grow) =
  let val main=AddressLabels.new_named "result_main"
      val target=AddressLabels.new_named "result_target"
      val tail=AddressLabels.new_named "result_tail"
      val fsz=FrameLayout.alignFrame FrameLayout.arm64 {locals=count,call=0}
      val dests=List.tabulate(count,fn i=>S.STACK_ATY i)
      val targetArgs=if grow then 11 else 9
      val callerArgs=if grow then 9 else 11
      val callee=convention(targetArgs,count,0)
      val spill=CallConv.get_spilled_res_with_offsets callee
      val outputs=List.tabulate(count,fn i=>if i<3 then x i else S.STACK_ATY(#2(List.nth(spill,i-3))))
      val args=List.tabulate(targetArgs,fn i=>num i)
      val actual=List.tabulate(callerArgs,fn i=>num i)
      val check=List.concat(List.tabulate(count,fn i=>
        [L.CCALL{name="putchar",args=[List.nth(dests,i)],rhos_for_result=[],res=[]}]))
      val code=[L.FUN(main,convention(0,0,fsz),
          L.FUNCALL{opr=tail,args=actual,reg_args=[],fargs=[],clos=NONE,res=dests,bv=[]}::check),
        L.FUN(tail,convention(callerArgs,count,0),
          [L.JMP{opr=target,args=args,reg_args=[],fargs=[],clos=NONE,res=outputs,bv=[]}]),
        L.FUN(target,callee,ListPair.map assign(outputs,List.tabulate(count,fn i=>num(65+i))))]
      val base="results" ^ Int.toString count ^ (if grow then "" else "-shrink")
  in G.emit(G.CG{main_lab=main,code=code,imports=([],[]),exports=([],[]),safe=false},base ^ ".s");
     G.emit(G.generate_link_code([main],([],[])),base ^ "-link.s") end
val () = List.app (fn n=>(emitCase(n,true);emitCase(n,false))) [4,5,6,7]
