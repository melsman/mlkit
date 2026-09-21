structure CodeGenUtilArm64 = struct
  open InstsArm64
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
  fun ins name args = Op(name,args)
  val r = pr_reg
  fun imm n = "#" ^ Int.toString n

  (* An Into helper prepends directly to the supplied code suffix. The list
   * wrappers retain the utility API used by standalone emitter tests. *)
  fun moveInto (a,b) code =
    if a = b then code
    else ins (case (a,b) of (D _,_) => "fmov" | (_,D _) => "fmov" | _ => "mov")
             [r b,r a] :: code
  fun move args = moveInto args []
  fun constantInto (n:IntInf.int,d) code =
    let
      val modulus : IntInf.int = 18446744073709551616
      val n = IntInf.mod(n,modulus)
      fun loop (4,_,code) = code
        | loop (k,n,code) =
          let
            val part = IntInf.toInt(IntInf.mod(n,65536))
            val i = ins (if k = 0 then "movz" else "movk")
                        [r d,imm part,"lsl " ^ imm (k*16)]
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
      "[" ^ r base ^ ", " ^ imm off ^ "]"
    else unsupported "large/unaligned memory offset"
  fun addOffsetInto (base,off,d) code =
    if off >= 0 andalso off < 4096 then ins "add" [r d,r base,imm off] :: code
    else constantInto (IntInf.fromInt off,d) (ins "add" [r d,r base,r d] :: code)
  fun addOffset args = addOffsetInto args []
  fun loadInto (base,off,d) code =
    if off >= 0 andalso off mod 8 = 0 andalso off <= 32760 then
      ins "ldr" [r d,memory(base,off)] :: code
    else
      let
        val tmp = if base = X 17 then X 16 else X 17
      in
        addOffsetInto (base,off,tmp) (ins "ldr" [r d,"[" ^ r tmp ^ "]"] :: code)
      end
  fun load args = loadInto args []
  fun storeInto (s,base,off) code =
    if off >= 0 andalso off mod 8 = 0 andalso off <= 32760 then
      ins "str" [r s,memory(base,off)] :: code
    else
      let
        val tmp = if s = X 17 orelse base = X 17 then X 16 else X 17
      in
        if tmp = s orelse tmp = base then
          (* Both scratch registers are occupied by the value and base. *)
          ins "sub" ["sp","sp","#16"] :: ins "str" [r s,"[sp]"] ::
          ins "str" [r base,"[sp, #8]"] ::
          constantInto (IntInf.fromInt off,s)
            (ins "add" [r s,r base,r s] :: ins "ldr" [r base,"[sp]"] ::
             ins "str" [r base,"[" ^ r s ^ "]"] :: ins "ldr" [r s,"[sp]"] ::
             ins "ldr" [r base,"[sp, #8]"] :: ins "add" ["sp","sp","#16"] :: code)
        else addOffsetInto (base,off,tmp) (ins "str" [r s,"[" ^ r tmp ^ "]"] :: code)
      end
  fun store args = storeInto args []
  fun stackInto (allocate,n) code =
    if n = 0 then code
    else if n < 0 orelse n mod 16 <> 0 then raise Fail "ARM64: unaligned stack adjustment"
    else
      let
        val chunk = Int.min(n,4080)
      in
        ins (if allocate then "sub" else "add") ["sp","sp",imm chunk] ::
        stackInto (allocate,n-chunk) code
      end
  fun stack args = stackInto args []
  (* External symbols use Mach-O GOT relocations, including across ML units. *)
  fun addressInto (l as LocalLab _,d) code =
    ins "adrp" [r d,pr_lab l ^ "@PAGE"] ::
    ins "add" [r d,r d,pr_lab l ^ "@PAGEOFF"] :: code
    | addressInto (l,d) code =
    ins "adrp" [r d,pr_lab l ^ "@GOTPAGE"] ::
    ins "ldr" [r d,"[" ^ r d ^ ", " ^ pr_lab l ^ "@GOTPAGEOFF]"] :: code
  fun address args = addressInto args []
  fun functionInto l code =
    Directive ".text" :: Directive ".p2align 2" ::
    Directive(".globl " ^ pr_lab l) :: Label l :: code
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
              ins "fmov" ["s30","w16"] :: ins "fcvt" ["d30","s30"] ::
              ins "fmov" ["x16","d30"] :: code
            else case source of
              AbiArm64.I8 => ins "sxtb" ["w16","w16"] :: code
            | AbiArm64.I16 => ins "sxth" ["w16","w16"] :: code
            | AbiArm64.U8 => ins "uxtb" ["w16","w16"] :: code
            | AbiArm64.U16 => ins "uxth" ["w16","w16"] :: code
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
              (ins (case bytes of 1 => "strb" | 2 => "strh" | _ => "str")
                [if bytes<8 then "w16" else "x16","[x17]"] :: code))
      val code = stackInto (false,bytes) code
      val code = if protectGC then loadInto (SP,bytes-16,X 17)
                   (addressInto (NameLab "disable_gc",X 16) (storeInto (X 17,X 16,0) code))
                 else code
      val code = foldri place (ins "bl" [pr_lab(NameLab name)] :: code) arguments
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
