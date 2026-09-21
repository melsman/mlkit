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
  fun emit (code,file) =
    let val os = TextIO.openOut file
        val serial = ref 0
        fun operation (s,args) = "\t" ^ s ^ " " ^ String.concatWith ", " args ^ "\n"
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
    (* Keep the existing conservative flush policy across C calls. *)
    fun is_callee_save_ccall _ = false
    val treg0 = X 16
    val treg1 = X 17
    val tfreg0 = D 30
    val tfreg1 = D 31
    val spreg = SP
  end
end
