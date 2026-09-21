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
  fun stackOffset s =
    case String.tokens (fn c => c = #"[" orelse c = #"]" orelse
                               c = #"," orelse c = #" ") s of
      ["sp",n] =>
        if not(String.isPrefix "#" n) then NONE
        else
          (case Int.fromString(String.extract(n,1,NONE)) of
             SOME off => if off >= 0 andalso s = "[sp, #" ^ Int.toString off ^ "]"
                         then SOME off else NONE
           | NONE => NONE)
    | _ => NONE
  fun optimise code =
    let
      fun sameClass (a,b) =
        case (registerClass a,registerClass b) of
          (SOME x,SOME y) => x = y
        | _ => false
      fun pair (opn,a,ma,b,mb) =
        if not(sameClass(a,b)) orelse (opn = "ldr" andalso a = b) then NONE
        else case (stackOffset ma,stackOffset mb) of
          (SOME x,SOME y) =>
            if x mod 8 = 0 andalso x <= 504 andalso y = x+8 then
              SOME(Op(if opn = "ldr" then "ldp" else "stp",[a,b,ma]))
            else NONE
        | _ => NONE
      fun loop ([],acc) = rev acc
        | loop ((i as Op(opn,[a,b]))::rest,acc) =
            if opn = "mov" andalso a = b andalso registerClass a = SOME opn then loop(rest,acc)
            else window(i,rest,acc)
        | loop (i::rest,acc) = window(i,rest,acc)
      and window (i,rest,acc) =
        case (i,rest) of
          (Op("b",[target]),(lab as Label l)::tail) =>
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
    in loop(code,[])
    end
  fun emit (code,file) =
    let val os = TextIO.openOut file
        val serial = ref 0
        fun operation (s,args) = "\t" ^ s ^ " " ^ String.concatWith ", " args ^ "\n"
        (* Generated parser functions exceed the conditional branch range.
         * Use a nearby inverted test followed by the wider unconditional B.
         * This also covers TBZ/TBNZ's much smaller 32 KiB reach. *)
        fun line (Label l) = pr_lab l ^ ":\n"
          | line (Directive s) = s ^ "\n"
          | line (Op (s,args)) = case inverse s of NONE => operation(s,args)
              | SOME opposite =>
                let val skip = "L_mlkit_branch_skip_" ^ Int.toString(!serial)
                    val () = serial := !serial+1
                    val target = List.last args
                    val operands = List.take(args,length args-1)
                in operation(opposite,operands@[skip]) ^ operation("b",[target]) ^ skip ^ ":\n"
                end
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
