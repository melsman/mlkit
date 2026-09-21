structure CodeGenUtilArm64 = struct
  open InstsArm64
  fun unsupported s =
    let val msg = "ARM64 milestone 3: " ^ s ^ " is not implemented"
    in TextIO.output(TextIO.stdErr,msg ^ "\n"); raise Fail msg end
  fun mapi f xs =
    let fun loop (_,[]) = [] | loop (i,x::xs) = f(i,x)::loop(i+1,xs)
    in loop(0,xs) end
  fun ins name args = Op(name,args)
  val r = pr_reg
  fun imm n = "#" ^ Int.toString n
  fun move (a,b) = if a=b then [] else [ins "mov" [r b,r a]]
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
  fun load (base,off,d) = [ins "ldr" [r d,memory(base,off)]]
  fun store (s,base,off) = [ins "str" [r s,memory(base,off)]]
  fun stack (allocate,n) =
    if n=0 then []
    else if n < 0 orelse n mod 16 <> 0 then raise Fail "ARM64: unaligned stack adjustment"
    else let val chunk = Int.min(n,4080)
         in ins (if allocate then "sub" else "add") ["sp","sp",imm chunk] ::
            stack(allocate,n-chunk) end
  (* External symbols use Mach-O GOT relocations, including across ML units. *)
  fun address (l,d) =
    [ins "adrp" [r d,pr_lab l ^ "@GOTPAGE"],
     ins "ldr" [r d,"[" ^ r d ^ ", " ^ pr_lab l ^ "@GOTPAGEOFF]"]]
  fun function l = [Directive ".text", Directive ".p2align 2",
                    Directive(".globl " ^ pr_lab l),Label l]
end
