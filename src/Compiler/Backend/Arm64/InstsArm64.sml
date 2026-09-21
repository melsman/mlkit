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
    val gp = map X (List.tabulate(31,fn i=>i)) @ [SP]
    val fp = map D (List.tabulate(32,fn i=>i))
    fun entry r = let val lv = Lvars.new_named_lvar(pr_reg r)
                     val () = case r of D _ => Lvars.set_ubf64 lv | _ => ()
                 in (lv,r) end
    val gs = map entry gp
    val fs = map entry fp
    val table = Lvars.Map.fromList(gs@fs)
    fun lv_to_reg lv = case Lvars.Map.lookup table lv of
                        SOME r => r | NONE => raise Fail "ARM64: unknown physical register"
    fun is_reg lv = Option.isSome(Lvars.Map.lookup table lv)
    fun reg_to_lv r = case List.find (fn (_,s)=>r=s) (gs@fs) of
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
    val callee_save_ccall_phregs = map (reg_to_lv o X) AbiArm64.cCalleeSaveGPRs
    (* Keep the existing conservative flush policy across C calls. *)
    fun is_callee_save_ccall _ = false
    val treg0 = X 16
    val treg1 = X 17
    val tfreg0 = D 30
    val tfreg1 = D 31
    val spreg = SP
  end
end
