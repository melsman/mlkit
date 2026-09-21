structure CodeGenUtilArm64 = struct
  open InstsArm64
  fun unsupported s =
    let val msg = "ARM64 backend: " ^ s ^ " is not implemented"
    in TextIO.output(TextIO.stdErr,msg ^ "\n"); raise Fail msg end
  fun mapi f xs =
    let fun loop (_,[]) = [] | loop (i,x::xs) = f(i,x)::loop(i+1,xs)
    in loop(0,xs) end
  fun ins name args = Op(name,args)
  val r = pr_reg
  fun imm n = "#" ^ Int.toString n
  fun move (a,b) = if a=b then [] else
    [ins (case (a,b) of (D _,_) => "fmov" | (_,D _) => "fmov" | _ => "mov") [r b,r a]]
  fun constant (n:IntInf.int,d) =
    let val modulus : IntInf.int = 18446744073709551616
        val n = IntInf.mod(n,modulus)
        fun loop (0,_,acc) = rev acc
          | loop (k,n,acc) =
            let val shift = (4-k)*16
                val part = IntInf.toInt(IntInf.mod(n,65536))
                val i = ins (if k=4 then "movz" else "movk")
                            [r d,imm part,"lsl " ^ imm shift]
            in loop(k-1,IntInf.div(n,65536),i::acc) end
    in loop(4,n,[]) end
  fun memory (base,off) =
    if off >= 0 andalso off mod 8=0 andalso off <= 32760 then
      "[" ^ r base ^ ", " ^ imm off ^ "]"
    else unsupported "large/unaligned memory offset"
  fun addOffset (base,off,d) =
    if off >= 0 andalso off < 4096 then [ins "add" [r d,r base,imm off]]
    else constant(IntInf.fromInt off,d) @ [ins "add" [r d,r base,r d]]
  fun load (base,off,d) =
    if off >= 0 andalso off mod 8=0 andalso off <= 32760 then [ins "ldr" [r d,memory(base,off)]]
    else let val tmp = if base=X 17 then X 16 else X 17
         in addOffset(base,off,tmp) @ [ins "ldr" [r d,"[" ^ r tmp ^ "]"]] end
  fun store (s,base,off) =
    if off >= 0 andalso off mod 8=0 andalso off <= 32760 then [ins "str" [r s,memory(base,off)]]
    else let val tmp = if s=X 17 orelse base=X 17 then X 16 else X 17
         in if tmp=s orelse tmp=base then
              (* Both scratch registers are occupied by the value and base. *)
              [ins "sub" ["sp","sp","#16"],ins "str" [r s,"[sp]"],
               ins "str" [r base,"[sp, #8]"]] @ constant(IntInf.fromInt off,s) @
              [ins "add" [r s,r base,r s],ins "ldr" [r base,"[sp]"],
               ins "str" [r base,"[" ^ r s ^ "]"],ins "ldr" [r s,"[sp]"],
               ins "ldr" [r base,"[sp, #8]"],ins "add" ["sp","sp","#16"]]
            else addOffset(base,off,tmp) @ [ins "str" [r s,"[" ^ r tmp ^ "]"]] end
  fun stack (allocate,n) =
    if n=0 then []
    else if n < 0 orelse n mod 16 <> 0 then raise Fail "ARM64: unaligned stack adjustment"
    else let val chunk = Int.min(n,4080)
         in ins (if allocate then "sub" else "add") ["sp","sp",imm chunk] ::
            stack(allocate,n-chunk) end
  (* External symbols use Mach-O GOT relocations, including across ML units. *)
  fun address (l as LocalLab _,d) =
    [ins "adrp" [r d,pr_lab l ^ "@PAGE"],
     ins "add" [r d,r d,pr_lab l ^ "@PAGEOFF"]]
    | address (l,d) =
    [ins "adrp" [r d,pr_lab l ^ "@GOTPAGE"],
     ins "ldr" [r d,"[" ^ r d ^ ", " ^ pr_lab l ^ "@GOTPAGEOFF]"]]
  fun function l = [Directive ".text", Directive ".p2align 2",
                    Directive(".globl " ^ pr_lab l),Label l]
  (* loadArgument(i,extraWords) puts argument i's raw bits in x16.
   * All inputs are staged before any ABI register is overwritten. *)
  fun scalarCall {name,fixed,variadic,loadArgument,protectGC} =
    let val {arguments,stackBytes}=AbiArm64.arguments{fixed=fixed,variadic=variadic}
        val n=length arguments
        val bytes=stackBytes+16*((n+1) div 2)+16
        fun promoted (i,{source,passed,...}:AbiArm64.argument) =
          loadArgument(i,bytes div 8) @
          (if source=AbiArm64.F32 andalso passed=AbiArm64.F64 then
             [ins "fmov" ["s30","w16"],ins "fcvt" ["d30","s30"],ins "fmov" ["x16","d30"]]
           else case source of
             AbiArm64.I8=>[ins "sxtb" ["w16","w16"]]
           | AbiArm64.I16=>[ins "sxth" ["w16","w16"]]
           | AbiArm64.U8=>[ins "uxtb" ["w16","w16"]]
           | AbiArm64.U16=>[ins "uxth" ["w16","w16"]]
           | _=>[]) @ store(X 16,SP,stackBytes+8*i)
        fun place (i,{passed,location,...}:AbiArm64.argument) =
          case location of AbiArm64.GPR n=>load(SP,stackBytes+8*i,X n)
          | AbiArm64.FPR n=>load(SP,stackBytes+8*i,D n)
          | AbiArm64.Stack{offset,bytes}=>load(SP,stackBytes+8*i,X 16) @
              addOffset(SP,offset,X 17) @
              [ins (case bytes of 1=>"strb" | 2=>"strh" | _=>"str")
                [if bytes<8 then "w16" else "x16","[x17]"]]
        val pause=if not protectGC then [] else address(NameLab "disable_gc",X 16) @
          load(X 16,0,X 17) @ store(X 17,SP,bytes-16) @ constant(1,X 17) @ store(X 17,X 16,0)
        val resume=if not protectGC then [] else load(SP,bytes-16,X 17) @
          address(NameLab "disable_gc",X 16) @ store(X 17,X 16,0)
    in stack(true,bytes) @ List.concat(mapi promoted arguments) @ pause @
       List.concat(mapi place arguments) @ [ins "bl" [pr_lab(NameLab name)]] @ resume @ stack(false,bytes) end

end
