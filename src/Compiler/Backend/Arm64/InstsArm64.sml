structure InstsArm64 : INSTS_ARM64 = struct
  open InstsBase
  type lvar = Lvars.lvar
  datatype reg = X of int | D of int | SP
  datatype inst = Label of lab | Directive of string | Op of string * string list
  type AsmPrg = inst list
  fun pr_reg (X n) = if n >= 0 andalso n <= 30 then "x" ^ Int.toString n
                     else raise Fail "ARM64: invalid integer register"
    | pr_reg (D n) = if n >= 0 andalso n <= 31 then "d" ^ Int.toString n
                     else raise Fail "ARM64: invalid FP register"
    | pr_reg SP = "sp"
  fun inverse s = case s of
      "b.eq" => SOME "b.ne" | "b.ne" => SOME "b.eq"
    | "b.lt" => SOME "b.ge" | "b.ge" => SOME "b.lt"
    | "b.gt" => SOME "b.le" | "b.le" => SOME "b.gt"
    | "b.lo" => SOME "b.hs" | "b.hs" => SOME "b.lo"
    | "b.hi" => SOME "b.ls" | "b.ls" => SOME "b.hi"
    | "b.mi" => SOME "b.pl" | "b.pl" => SOME "b.mi"
    | "b.vs" => SOME "b.vc" | "b.vc" => SOME "b.vs"
    | "cbz" => SOME "cbnz" | "cbnz" => SOME "cbz"
    | "tbz" => SOME "tbnz" | "tbnz" => SOME "tbz" | _ => NONE
  (* Keep labels and directives intact: they can be referenced by GC metadata,
   * exception handlers, or address materialisation outside this local window. *)
  fun registerClass s =
    if size s < 2 then NONE
    else
      case Int.fromString(String.extract(s,1,NONE)) of
        SOME n =>
          if n < 0 orelse String.extract(s,1,NONE) <> Int.toString n then NONE
          else (case String.sub(s,0) of
            #"x" => if n <= 30 then SOME "mov" else NONE
          | #"d" => if n <= 31 then SOME "fmov" else NONE
          | _ => NONE)
      | NONE => NONE
  fun decimal n = String.translate (fn #"~" => "-" | c => str c) (Int.toString n)
  fun memory (base,off) = "[" ^ base ^ ", #" ^ decimal off ^ "]"
  (* Only ordinary constant-offset addresses; no writeback or register index. *)
  fun memoryOffset s =
    case String.tokens (fn c => c = #"[" orelse c = #"]" orelse
                               c = #"," orelse c = #" ") s of
      [base,n] =>
        if (base <> "sp" andalso registerClass base <> SOME "mov") orelse
           not(String.isPrefix "#" n) then NONE
        else
          (case Int.fromString(String.extract(n,1,NONE)) of
             SOME off => if s = memory(base,off) then SOME(base,off) else NONE
           | NONE => NONE)
    | _ => NONE
  fun stackOffset s =
    case memoryOffset s of SOME("sp",off) => SOME off | _ => NONE
  (* Resolve only label-only blocks ending in B. Never remove the labels:
   * return PCs, GC descriptors and exception tables can still reference them. *)
  fun threadBranches code =
    let
      fun scan (Label l::rest,pending,table) = scan(rest,l::pending,table)
        | scan (Op("b",[target])::rest,pending,table) =
            scan(rest,[],foldl (fn (l,t) => StringFinMap.add(pr_lab l,target,t)) table pending)
        | scan (_::rest,_,table) = scan(rest,[],table)
        | scan ([],_,table) = table
      val table = scan(code,[],StringFinMap.empty)
      fun resolve original =
        let
          fun follow (target,seen) =
            if List.exists (fn s => s = target) seen then original
            else case StringFinMap.lookup table target of
              SOME next => follow(next,target::seen)
            | NONE => target
        in follow(original,[])
        end
      fun rewrite (Op(opn,args)) =
            if (opn = "b" orelse Option.isSome(inverse opn)) andalso not(null args) then
              Op(opn,List.take(args,length args-1) @ [resolve(List.last args)])
            else Op(opn,args)
        | rewrite i = i
    in map rewrite code
    end
  fun optimise code =
    let
      fun sameClass (a,b) =
        case (registerClass a,registerClass b) of
          (SOME x,SOME y) => x = y
        | _ => false
      fun pair (opn,a,ma,b,mb) =
        if not(sameClass(a,b)) orelse (opn = "ldr" andalso a = b) then NONE
        else case (memoryOffset ma,memoryOffset mb) of
          (SOME(base,x),SOME(base',y)) =>
            let val low = Int.min(x,y)
            in
              if base = base' andalso low mod 8 = 0 andalso low >= ~512 andalso
                 low <= 504 andalso Int.abs(x-y) = 8 andalso
                 not(opn = "ldr" andalso a = base) then
                SOME(Op(if opn = "ldr" then "ldp" else "stp",
                        (if x < y then [a,b] else [b,a]) @ [memory(base,low)]))
              else NONE
            end
        | _ => NONE
      fun integer s = registerClass s = SOME "mov"
      fun immediateALU opn = List.exists (fn n => n = opn)
        ["add","sub","and","orr","eor","lsr","lsl","asr"]
      fun loop ([],acc) = rev acc
        | loop ((i as Op(opn,[a,b]))::rest,acc) =
            if opn = "mov" andalso a = b andalso registerClass a = SOME opn then loop(rest,acc)
            else window(i,rest,acc)
        | loop (i::rest,acc) = window(i,rest,acc)
      and window (i,rest,acc) =
        case (i,rest) of
          (Op("mov",[a,b]),Op("mov",[c,d])::tail) =>
            if integer a andalso integer b andalso a = d andalso b = c then
              loop(i::tail,acc)
            else loop(rest,i::acc)
        | (Op("mov",[a,b]),Op(opn,[dst,addr])::tail) =>
            if integer a andalso integer b andalso dst = a andalso
               List.exists (fn n => n = opn) ["ldr","ldrb","ldrh"] then
              (case memoryOffset addr of
                 SOME(base,off) =>
                   if base = a then loop(Op(opn,[dst,memory(b,off)])::tail,acc)
                   else loop(rest,i::acc)
               | NONE => loop(rest,i::acc))
            else loop(rest,i::acc)
        | (Op("mov",[a,b]),Op(opn,[dst,src,imm])::tail) =>
            if integer a andalso integer b andalso dst = a andalso src = a andalso
               immediateALU opn andalso String.isPrefix "#" imm then
              loop(Op(opn,[dst,b,imm])::tail,acc)
            else loop(rest,i::acc)
        | (Op("movz",[zero,"#0","lsl #0"]),Op("orr",[dst,a,b])::tail) =>
            if integer zero andalso integer dst andalso
               ((a = dst andalso b = zero) orelse (a = zero andalso b = dst)) then
              loop(i::tail,acc)
            else loop(rest,i::acc)
        | (Op("b",[target]),(lab as Label l)::tail) =>
            if target = pr_lab l then loop(lab::tail,acc)
            else loop(rest,i::acc)
        | (Op(opn,args),(lab as Label l)::tail) =>
            if Option.isSome(inverse opn) andalso not(null args) andalso
               List.last args = pr_lab l then loop(lab::tail,acc)
            else loop(rest,i::acc)
        | (Op(opn,args),Op("b",[other])::(lab as Label l)::tail) =>
            (case inverse opn of
               SOME inv =>
                 if not(null args) andalso List.last args = pr_lab l then
                   loop(Op(inv,List.take(args,length args-1) @ [other])::lab::tail,acc)
                 else loop(rest,i::acc)
             | NONE => loop(rest,i::acc))
        | (Op("str",[a,ma]),Op("ldr",[b,mb])::tail) =>
            if ma = mb andalso Option.isSome(stackOffset ma) andalso sameClass(a,b) then
              loop(Op(valOf(registerClass a),[b,a])::tail,i::acc)
            else loop(rest,i::acc)
        | (Op(opn,[a,ma]),Op(opn2,[b,mb])::tail) =>
            if (opn = "str" orelse opn = "ldr") andalso opn = opn2 then
              (case pair(opn,a,ma,b,mb) of
                 SOME paired => loop(tail,paired::acc)
               | NONE => loop(rest,i::acc))
            else loop(rest,i::acc)
        | _ => loop(rest,i::acc)
      (* Bounded extra sweeps expose adjacent patterns hidden by a copy or
       * branch removed on the preceding sweep; compilation remains linear
       * apart from finite-map lookup and branch-chain resolution. *)
      val code = threadBranches code
      val code = loop(code,[])
      val code = loop(code,[])
    in loop(code,[])
    end
  (* Bound distances with every conditional branch expanded, every ADRP/ADD
   * pair intact, and maximum alignment padding. Shrinking code cannot make
   * any proven local span larger, so no iterative relaxation is necessary.
   * Track text/data independently across switches, including cold blocks.
   * Unknown directives end all proofs rather than guessing their size or
   * section. Cross-span and external targets keep the conservative form. *)
  fun relax code =
    let
      fun decimal s =
        if size s > 0 andalso List.all Char.isDigit (String.explode s)
        then Int.fromString s else NONE
      fun directive s =
        case String.tokens Char.isSpace s of
          [".p2align",n] =>
            (case decimal n of
               SOME n => if n <= 20 then SOME(IntInf.toInt(IntInf.pow(2,n))-1) else NONE
             | NONE => NONE)
        | [".space",n] => decimal n
        | ".globl"::_ => SOME 0
        | kind::fields =>
            if kind = ".quad" orelse kind = ".double" orelse kind = ".byte" then
              SOME((if kind = ".byte" then 1 else 8) *
                length(String.tokens (fn c => c = #",") (String.concat fields)))
            else NONE
        | _ => NONE
      val textOffset = ref (1,0)
      val dataOffset = ref (2,0)
      val unknownOffset = ref (0,0)
      val current = ref unknownOffset
      val serial = ref 2
      fun fresh () = (serial := !serial+1; (!serial,0))
      fun barrier () =
        (textOffset := fresh(); dataOffset := fresh();
         unknownOffset := fresh(); current := unknownOffset)
      fun advance n =
        let val p = !current
            val (segment,offset) = !p
        in p := (segment,offset+n)
        end
      fun layout ([],labels,acc) = (labels,rev acc)
        | layout (i::rest,labels,acc) =
            let val here = !(!current)
                val labels = case i of Label l => StringFinMap.add(pr_lab l,here,labels)
                                           | _ => labels
                val () = case i of
                    Label _ => ()
                  | Op(opn,_) => advance(if Option.isSome(inverse opn) then 8 else 4)
                  | Directive ".text" => current := textOffset
                  | Directive ".data" => current := dataOffset
                  | Directive s =>
                      (case directive s of SOME n => advance n | NONE => barrier())
            in layout(rest,labels,(i,here)::acc)
            end
      val (labels,located) = layout(code,StringFinMap.empty,[])
      fun within (segment,offset) target low high =
        case StringFinMap.lookup labels target of
          SOME (s,p) => s = segment andalso p-offset >= low andalso p-offset <= high
        | NONE => false
      fun localAddress (page,off) =
        if String.isSuffix "@PAGE" page andalso
           off = String.substring(page,0,size page-5) ^ "@PAGEOFF" then
          SOME(String.substring(page,0,size page-5))
        else NONE
      fun loop ([],acc) = rev acc
        | loop ((i as Op("adrp",[dst,page]),here)::
                (j as Op("add",[dst',base,off]),there)::rest,acc) =
            (case localAddress(page,off) of
               SOME target =>
                 if dst = dst' andalso dst = base andalso
                    within here target (~1048576) 1048575 then
                   loop(rest,Op("adr",[dst,target])::acc)
                 else step(i,here,(j,there)::rest,acc)
             | NONE => step(i,here,(j,there)::rest,acc))
        | loop ((i,here)::rest,acc) = step(i,here,rest,acc)
      and step (i,here,rest,acc) =
        case i of
          Op(opn,args) =>
            (case inverse opn of
               NONE => loop(rest,i::acc)
             | SOME opposite =>
                 let val target = List.last args
                     val reach = if opn = "tbz" orelse opn = "tbnz" then 32768 else 1048576
                 in
                   if within here target (~reach) (reach-4) then loop(rest,i::acc)
                   else
                     let val skip = LocalLab(AddressLabels.new_named "arm64_branch_skip")
                         val operands = List.take(args,length args-1)
                     in loop(rest,Label skip :: Op("b",[target]) ::
                          Op(opposite,operands @ [pr_lab skip]) :: acc)
                     end
                 end)
        | _ => loop(rest,i::acc)
    in loop(located,[])
    end
  fun emit (code,file) =
    let val code = relax code
        val os = TextIO.openOut file
        fun line (Label l) = pr_lab l ^ ":\n"
          | line (Directive s) = s ^ "\n"
          | line (Op (s,args)) = "\t" ^ s ^ " " ^ String.concatWith ", " args ^ "\n"
    in (List.app (fn i => TextIO.output(os,line i)) code; TextIO.closeOut os)
       handle e => (TextIO.closeOut os; raise e)
    end
  structure RI = struct
    type reg = reg
    type lvar = Lvars.lvar
    val frame_layout = AbiArm64.frame
    val pr_reg = pr_reg
    val reg_eq = op =
    val gp = map X (List.tabulate(31,fn i => i)) @ [SP]
    val fp = map D (List.tabulate(32,fn i => i))
    fun entry r =
      let val lv = Lvars.new_named_lvar(pr_reg r)
          val () = case r of D _ => Lvars.set_ubf64 lv | _ => ()
      in (lv,r)
      end
    val gs = map entry gp
    val fs = map entry fp
    val table = Lvars.Map.fromList(gs@fs)
    fun lv_to_reg lv = case Lvars.Map.lookup table lv of
                        SOME r => r | NONE => raise Fail "ARM64: unknown physical register"
    fun is_reg lv = Option.isSome(Lvars.Map.lookup table lv)
    fun reg_to_lv r = case List.find (fn (_,s) => r = s) (gs@fs) of
                       SOME (lv,_) => lv | NONE => raise Fail "ARM64: unknown register"
    val all_regs = map #1 gs
    val args_phreg = map (reg_to_lv o X) AbiArm64.mlArgumentGPRs
    val res_phreg = map (reg_to_lv o X) AbiArm64.mlResultGPRs
    val args_phfreg = map (reg_to_lv o D) AbiArm64.mlArgumentFPRs
    val f64_phregs = map (reg_to_lv o D) (AbiArm64.allocatableFPRs @ AbiArm64.spillFPRs)
    val allocatable_f64_phregs = map (reg_to_lv o D) AbiArm64.allocatableFPRs
    val caller_save_phregs = map (reg_to_lv o X) AbiArm64.allocatableGPRs
    val callers = Lvarset.lvarsetof caller_save_phregs
    fun is_caller_save lv = Lvarset.member(lv,callers)
    val args_reg_ccall = map X AbiArm64.mlArgumentGPRs
    val args_phreg_ccall = map reg_to_lv args_reg_ccall
    val res_phreg_ccall = [reg_to_lv(X 0)]
    (* RegAlloc uses this set as an allocation palette, not merely an ABI
     * preservation description. Keep context and exception registers out. *)
    val callee_save_ccall_phregs = map (reg_to_lv o X)
      (List.filter (fn r => List.exists (fn a => a = r) AbiArm64.allocatableGPRs)
        AbiArm64.cCalleeSaveGPRs)
    val cPreserved = Lvarset.lvarsetof callee_save_ccall_phregs
    fun is_callee_save_ccall lv = Lvarset.member(lv,cPreserved)
    val treg0 = X 16
    val treg1 = X 17
    val tfreg0 = D 30
    val tfreg1 = D 31
    val spreg = SP
  end
end
