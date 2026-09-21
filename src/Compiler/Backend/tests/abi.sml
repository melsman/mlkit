fun check name condition = if condition then () else raise Fail name
val noRegs = {arg_regs=[],arg_fregs=[],res_regs=[]}
val cc0 = {clos=NONE,args=[1,2],reg_args=[],fargs=[],res=[3]}
fun testFrame (frame,header,padding) =
  let val (cc,_,_) = CallConv.resolve_cc frame noRegs (CallConv.mk_cc cc0)
      val (args,res) = CallConv.resolve_act_cc frame noRegs cc0
  in check "argument order" (args=[(1,1+header+padding),(2,2+header+padding)]);
     check "result offset" (res=[(3,padding)]);
     check "callee offsets" (CallConv.get_spilled_args_with_offsets cc=[(2,~1),(1,~2)]);
     check "result below header" (CallConv.get_spilled_res_with_offsets cc=[(3,~(3+header))]);
     check "call size" (CallConv.get_cc_size frame cc=3+header+padding);
     List.app (fn locals =>
       let val n=FrameLayout.alignFrame frame {locals=locals,call=3+header+padding}
       in check "aligned with minimal padding" (n>=locals andalso n<=locals+1 andalso (n+3+header+padding) mod 2=0) end)
       (List.tabulate(10,fn i=>i))
  end
val () = testFrame(FrameLayout.x64,1,0)
val () = testFrame(FrameLayout.arm64,2,1)
val () = check "return delivery is independent of saved frame layout"
  (FrameLayout.returnDelivery FrameLayout.x64=FrameLayout.StackHeader andalso
   FrameLayout.returnDelivery FrameLayout.arm64=FrameLayout.LinkRegister AbiArm64.linkRegister andalso
   FrameLayout.headerWords FrameLayout.arm64=2)
val () = check "return PC within header"
  (FrameLayout.returnOffsetFromTop FrameLayout.x64=0 andalso
   FrameLayout.returnOffsetFromTop FrameLayout.arm64=1)
(* Caller/callee agreement with a closure, exhausted banks, region/FP
 * arguments, and a spilled result. Offsets must include the whole header. *)
val mixedRegs = {arg_regs=[100,101],arg_fregs=[200],res_regs=[300]}
val mixedCall = {clos=SOME 10,args=[11,12],reg_args=[13],fargs=[14,15],res=[16,17]}
fun checkMixed frame =
  let val (cc,_,_) = CallConv.resolve_cc frame mixedRegs (CallConv.mk_cc mixedCall)
      val (args,res) = CallConv.resolve_act_cc frame mixedRegs mixedCall
      val n = CallConv.get_cc_size frame cc
      val formal = CallConv.get_spilled_args_with_offsets cc @ CallConv.get_spilled_res_with_offsets cc
  in check "caller/callee spilled-slot agreement"
       (List.all (fn (lv,off)=>List.exists (fn (v,f)=>v=lv andalso f+n=off) formal) (args@res));
     check "region and FP arguments excluded from register roots"
       (CallConv.get_register_args_excluding_region_and_float_args cc=[100,101])
  end
val () = checkMixed FrameLayout.x64
val () = checkMixed FrameLayout.arm64
local open AbiArm64
  fun locations fixed variadic =
      map #location (#arguments(arguments{fixed=fixed,variadic=variadic}))
in
val () = check "independent C banks"
  (locations [I64,F64,I32,F32] [] = [GPR 0,FPR 0,GPR 1,FPR 1])
val () = check "Darwin packed stack bytes"
  (List.drop(locations (List.tabulate(10,fn _=>I8)) [],8)
   = [Stack{offset=0,bytes=1},Stack{offset=1,bytes=1}])
val () = check "packed stack stores retain their natural width"
  (#extension(List.last(#arguments(arguments{fixed=List.tabulate(9,fn _=>I8),variadic=[]})))=None)
val () = check "stack natural alignment"
  (List.drop(locations (List.tabulate(8,fn _=>I64)@[I8,I64,I16]) [],8)
   = [Stack{offset=0,bytes=1},Stack{offset=8,bytes=8},Stack{offset=16,bytes=2}])
val () = check "variadic arguments never use spare registers"
  (locations [Ptr] [I32,F64]=[GPR 0,Stack{offset=0,bytes=8},Stack{offset=8,bytes=8}])
val promoted = #arguments(arguments{fixed=[],variadic=[I8,U16,F32]})
val () = check "default argument promotions" (map #passed promoted=[I32,I32,F64])
val () = check "narrow named extension"
  (map #extension (#arguments(arguments{fixed=[I8,U8,I16,U16],variadic=[]}))
   = [SignTo32,ZeroTo32,SignTo32,ZeroTo32])
val () = check "overflow floating bank"
  (List.last(locations (List.tabulate(9,fn _=>F64)) [])=Stack{offset=0,bytes=8})
val () = check "C stack alignment"
  (#stackBytes(arguments{fixed=List.tabulate(10,fn _=>I8),variadic=[]})=16)
val () = check "result registers" (result(SOME F64)=SOME(FPR 0) andalso result(SOME Ptr)=SOME(GPR 0) andalso result NONE=NONE)
val () = check "reserved registers excluded"
  (List.all (fn r=>not(List.exists (fn a=>a=r) allocatableGPRs)) reservedGPRs)
end

val () = List.app (fn a=>List.app(fn r=>
  let val call={clos=NONE,args=List.tabulate(a,fn i=>i),reg_args=[],fargs=[],res=List.tabulate(r,fn i=>100+i)}
      val(cc,_,_)=CallConv.resolve_cc FrameLayout.arm64 noRegs (CallConv.mk_cc call)
      val(args,res)=CallConv.resolve_act_cc FrameLayout.arm64 noRegs call
      val formal=CallConv.get_spilled_args_with_offsets cc @ CallConv.get_spilled_res_with_offsets cc
      val n=CallConv.get_cc_size FrameLayout.arm64 cc
  in check "all ARM argument/result padding combinations" (n mod 2=0 andalso
       List.all(fn(lv,off)=>List.exists(fn(v,f)=>v=lv andalso f+n=off) formal)(args@res)) end)
  (List.tabulate(8,fn i=>i))) (List.tabulate(8,fn i=>i))

val () = print "ABI layout tests passed\n"
