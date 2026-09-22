structure CodeGenUtilArm64 = struct
  structure A = InstsArm64
  open InstsArm64
  fun branch c = case c of
      EQ => A.b_eq | NE => A.b_ne | LT => A.b_lt | GE => A.b_ge
    | GT => A.b_gt | LE => A.b_le | LO => A.b_lo | HS => A.b_hs
    | HI => A.b_hi | LS => A.b_ls | MI => A.b_mi | PL => A.b_pl
    | VS => A.b_vs | VC => A.b_vc
  fun unsupported s =
    let
      val msg = "ARM64 backend: " ^ s ^ " is not implemented"
    in
      TextIO.output(TextIO.stdErr,msg ^ "\n"); raise Fail msg
    end
  fun mapi f xs =
    let
      fun loop (_,[]) = []
        | loop (i,x::xs) = f(i,x)::loop(i+1,xs)
    in
      loop(0,xs)
    end
  fun foldri f code xs =
    let
      fun loop (_,[],code) = code
        | loop (i,x::xs,code) = f(i,x,loop(i+1,xs,code))
    in
      loop(0,xs,code)
    end
  val r = R
  fun imm n = I(IntInf.fromInt n)

  (* An Into helper prepends directly to the supplied code suffix. The list
   * wrappers retain the utility API used by standalone emitter tests. *)
  fun moveInto (a,rhs) code =
    if a = rhs then code
    else (case (a,rhs) of (D _,_) => A.fmov | (_,D _) => A.fmov | _ => A.mov) (R(rhs),R(a)) :: code
  fun move args = moveInto args []
  fun constantInto (n:IntInf.int,d) code =
    let
      val modulus : IntInf.int = 18446744073709551616
      val n = IntInf.mod(n,modulus)
      fun loop (4,_,code) = code
        | loop (k,n,code) =
          let
            val part = IntInf.toInt(IntInf.mod(n,65536))
            val i = (if k = 0 then A.movz else A.movk) (R(d),imm part,ShiftImm(LSL,k*16))
          in
            if k > 0 andalso part = 0 then loop(k+1,IntInf.div(n,65536),code)
            else i :: loop(k+1,IntInf.div(n,65536),code)
          end
    in
      loop(0,n,code)
    end
  fun constant args = constantInto args []
  fun memory (base,off) =
    if off >= 0 andalso off mod 8 = 0 andalso off <= 32760 then
      M(base,off)
    else unsupported "large/unaligned memory offset"
  fun addOffsetInto (base,off,d) code =
    if off >= 0 andalso off < 4096 then A.add (R(d),R(base),imm off) :: code
    else constantInto (IntInf.fromInt off,d) (A.add (R(d),R(base),R(d)) :: code)
  fun addOffset args = addOffsetInto args []
  fun loadInto (base,off,d) code =
    if off >= 0 andalso off mod 8 = 0 andalso off <= 32760 then
      A.ldr (R(d),memory(base,off)) :: code
    else
      let
        val tmp = if base = X 17 then X 16 else X 17
      in
        addOffsetInto (base,off,tmp) (A.ldr (R(d),M(tmp,0)) :: code)
      end
  fun load args = loadInto args []
  fun storeInto (s,base,off) code =
    if off >= 0 andalso off mod 8 = 0 andalso off <= 32760 then
      A.str (R(s),memory(base,off)) :: code
    else
      let
        val tmp = if s = X 17 orelse base = X 17 then X 16 else X 17
      in
        if tmp = s orelse tmp = base then
          (* Both scratch registers are occupied by the value and base. *)
          A.sub (R(SP),R(SP),I(16)) :: A.str (R(s),M(SP,0)) ::
          A.str (R(base),M(SP,8)) ::
          constantInto (IntInf.fromInt off,s)
            (A.add (R(s),R(base),R(s)) :: A.ldr (R(base),M(SP,0)) ::
             A.str (R(base),M(s,0)) :: A.ldr (R(s),M(SP,0)) ::
             A.ldr (R(base),M(SP,8)) :: A.add (R(SP),R(SP),I(16)) :: code)
        else addOffsetInto (base,off,tmp) (A.str (R(s),M(tmp,0)) :: code)
      end
  fun store args = storeInto args []
  fun stackInto (allocate,n) code =
    if n = 0 then code
    else if n < 0 orelse n mod 16 <> 0 then raise Fail "ARM64: unaligned stack adjustment"
    else
      let
        val chunk = Int.min(n,4080)
      in
        (if allocate then A.sub else A.add) (R(SP),R(SP),imm chunk) ::
        stackInto (allocate,n-chunk) code
      end
  fun stack args = stackInto args []
  (* External symbols use Mach-O GOT relocations, including across ML units. *)
  fun addressInto (l as LocalLab _,d) code =
    A.adrp (R(d),Page l) ::
    A.add (R(d),R(d),PageOff l) :: code
    | addressInto (l,d) code =
    A.adrp (R(d),GotPage l) ::
    A.ldr (R(d),GotMemory(d,l)) :: code
  fun address args = addressInto args []
  fun functionInto l code =
    Directive(Text) :: Directive(Align 2) ::
    Directive(Global (l)) :: Label l :: code
  fun function l = functionInto l []
  (* loadArgument(i,extraWords) puts argument i's raw bits in x16.
   * All inputs are staged before any ABI register is overwritten. *)
  fun scalarCallInto {name,fixed,variadic,loadArgument,protectGC} code =
    let
      val {arguments,stackBytes} = AbiArm64.arguments{fixed = fixed,variadic = variadic}
      val n = length arguments
      val bytes = stackBytes+16*((n+1) div 2)+16
      fun promoted (i,{source,passed,...}:AbiArm64.argument,code) =
        let
          val code = storeInto (X 16,SP,stackBytes+8*i) code
          val code =
            if source = AbiArm64.F32 andalso passed = AbiArm64.F64 then
              A.fmov (R(S 30),R(W 16)) :: A.fcvt (R(D 30),R(S 30)) ::
              A.fmov (R(X 16),R(D 30)) :: code
            else case source of
              AbiArm64.I8 => A.sxtb (R(W 16),R(W 16)) :: code
            | AbiArm64.I16 => A.sxth (R(W 16),R(W 16)) :: code
            | AbiArm64.U8 => A.uxtb (R(W 16),R(W 16)) :: code
            | AbiArm64.U16 => A.uxth (R(W 16),R(W 16)) :: code
            | _ => code
        in
          loadArgument(i,bytes div 8) code
        end
      fun place (i,{passed,location,...}:AbiArm64.argument,code) =
        case location of
          AbiArm64.GPR n => loadInto (SP,stackBytes+8*i,X n) code
        | AbiArm64.FPR n => loadInto (SP,stackBytes+8*i,D n) code
        | AbiArm64.Stack{offset,bytes} =>
            loadInto (SP,stackBytes+8*i,X 16) (addOffsetInto (SP,offset,X 17)
              ((case bytes of 1 => A.strb | 2 => A.strh | _ => A.str) (if bytes<8 then R(W 16) else R(X 16),M(X 17,0)) :: code))
      val code = stackInto (false,bytes) code
      val code = if protectGC then loadInto (SP,bytes-16,X 17)
                   (addressInto (NameLab "disable_gc",X 16) (storeInto (X 17,X 16,0) code))
                 else code
      val code = foldri place (A.bl (L(NameLab name)) :: code) arguments
      val code = if protectGC then addressInto (NameLab "disable_gc",X 16)
                   (loadInto (X 16,0,X 17) (storeInto (X 17,SP,bytes-16)
                     (constantInto (1,X 17) (storeInto (X 17,X 16,0) code))))
                 else code
    in
      stackInto (true,bytes) (foldri promoted code arguments)
    end
  fun scalarCall {name,fixed,variadic,loadArgument,protectGC} =
    scalarCallInto {name = name,fixed = fixed,variadic = variadic,protectGC = protectGC,
      loadArgument = fn args => fn code => foldr (op ::) code (loadArgument args)} []
end
