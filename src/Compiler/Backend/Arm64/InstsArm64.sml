structure InstsArm64 : INSTS_ARM64 = struct
  structure A = Arm64Instructions
  open Arm64Instructions
  type lvar = Lvars.lvar
  type AsmPrg = inst list
  val pr_reg = PrintArm64.pr_reg
  val pr_inst = PrintArm64.pr_inst
  fun eq_ea (R a,R rhs) = a = rhs
    | eq_ea (I a,I rhs) = a = rhs
    | eq_ea (L a,L rhs) = eq_lab(a,rhs)
    | eq_ea (Forward a,Forward rhs) = a = rhs
    | eq_ea (M a,M rhs) = a = rhs
    | eq_ea (PreIndex a,PreIndex rhs) = a = rhs
    | eq_ea (Indexed a,Indexed rhs) = a = rhs
    | eq_ea (Shifted a,Shifted rhs) = a = rhs
    | eq_ea (ShiftImm a,ShiftImm rhs) = a = rhs
    | eq_ea (Page a,Page rhs) = eq_lab(a,rhs)
    | eq_ea (PageOff a,PageOff rhs) = eq_lab(a,rhs)
    | eq_ea (GotPage a,GotPage rhs) = eq_lab(a,rhs)
    | eq_ea (GotMemory(r,a),GotMemory(s,rhs)) = r = s andalso eq_lab(a,rhs)
    | eq_ea (C a,C rhs) = a = rhs
    | eq_ea _ = false
  fun integer (R(X _)) = true | integer _ = false
  fun sameClass (R(X _),R(X _)) = true
    | sameClass (R(D _),R(D _)) = true
    | sameClass _ = false
  (* Return the target, retargeting/inversion constructors and byte reach. *)
  fun conditional i = case i of
      b_eq t => SOME(t,b_eq,b_ne,1048576)
    | b_ne t => SOME(t,b_ne,b_eq,1048576)
    | b_lt t => SOME(t,b_lt,b_ge,1048576)
    | b_ge t => SOME(t,b_ge,b_lt,1048576)
    | b_gt t => SOME(t,b_gt,b_le,1048576)
    | b_le t => SOME(t,b_le,b_gt,1048576)
    | b_lo t => SOME(t,b_lo,b_hs,1048576)
    | b_hs t => SOME(t,b_hs,b_lo,1048576)
    | b_hi t => SOME(t,b_hi,b_ls,1048576)
    | b_ls t => SOME(t,b_ls,b_hi,1048576)
    | b_mi t => SOME(t,b_mi,b_pl,1048576)
    | b_pl t => SOME(t,b_pl,b_mi,1048576)
    | b_vs t => SOME(t,b_vs,b_vc,1048576)
    | b_vc t => SOME(t,b_vc,b_vs,1048576)
    | cbz(r,t) => SOME(t,fn t => cbz(r,t),fn t => cbnz(r,t),1048576)
    | cbnz(r,t) => SOME(t,fn t => cbnz(r,t),fn t => cbz(r,t),1048576)
    | tbz(r,n,t) => SOME(t,fn t => tbz(r,n,t),fn t => tbnz(r,n,t),32768)
    | tbnz(r,n,t) => SOME(t,fn t => tbnz(r,n,t),fn t => tbz(r,n,t),32768)
    | _ => NONE
  (* Label tables use identity, never pretty-printed assembly names. *)
  fun labelHash l = case l of
      LocalLab l => #1(AddressLabels.key l)
    | DatLab l => #1(AddressLabels.key l)
    | MLFunLab l => #1(AddressLabels.key l)
    | NameLab s => Word.toIntX(Pickle.Hash.hash(Pickle.Hash.string s Pickle.Hash.init))
  fun labelTable () = Polyhash.mkTable(labelHash,eq_lab)(127,Fail "ARM64 label")
  fun threadBranches code =
    let
      val table = labelTable()
      fun scan (Label l::rest,pending) = scan(rest,l::pending)
        | scan (A.b target::rest,pending) =
            (List.app (fn l => Polyhash.insert table (l,target)) pending; scan(rest,[]))
        | scan (_::rest,_) = scan(rest,[])
        | scan ([],_) = ()
      val () = scan(code,[])
      fun resolve original =
        let
          fun follow (target as L l,seen) =
                if List.exists (fn t => eq_lab(t,l)) seen then original
                else (case Polyhash.peek table l of
                        SOME next => follow(next,l::seen) | NONE => target)
            | follow (target,_) = target
        in follow(original,[])
        end
      fun rewrite (A.b target) = A.b(resolve target)
        | rewrite i = case conditional i of
            SOME(target,make,_,_) => make(resolve target) | NONE => i
    in map rewrite code
    end
  fun optimise code =
    let
      fun pair (load,make,a,M(base,x),rhs,M(base',y)) =
            let val low = Int.min(x,y)
            in
              if sameClass(a,rhs) andalso base = base' andalso low mod 8 = 0 andalso
                 low >= ~512 andalso low <= 504 andalso Int.abs(x-y) = 8 andalso
                 not(load andalso (eq_ea(a,rhs) orelse eq_ea(a,R base))) then
                SOME(if x < y then make(a,rhs,M(base,low)) else make(rhs,a,M(base,low)))
              else NONE
            end
        | pair _ = NONE
      (* Copy folding overwrites the copied register: all register/flag values
       * remain unchanged, with no liveness assumptions. *)
      fun foldCopy (a,rhs,i) =
        let
          fun load make (dst,M(base,off)) =
                if eq_ea(dst,a) andalso eq_ea(R base,a) then
                  (case rhs of R src => SOME(make(dst,M(src,off))) | _ => NONE)
                else NONE
            | load _ _ = NONE
          fun alu make (dst,src,imm as I _) =
                if eq_ea(dst,a) andalso eq_ea(src,a) then SOME(make(dst,rhs,imm)) else NONE
            | alu _ _ = NONE
        in
          if not(integer a andalso integer rhs) then NONE
          else case i of
            ldr args => load ldr args | ldrb args => load ldrb args | ldrh args => load ldrh args
          | add args => alu add args | sub args => alu sub args | and_ args => alu and_ args
          | orr args => alu orr args | eor args => alu eor args | lsl args => alu lsl args
          | lsr args => alu lsr args | asr args => alu asr args | _ => NONE
        end
      fun loop ([],acc) = rev acc
        | loop ((i as mov(a,rhs))::rest,acc) =
            if integer a andalso eq_ea(a,rhs) then loop(rest,acc) else window(i,rest,acc)
        | loop (i::rest,acc) = window(i,rest,acc)
      and window (i,rest,acc) =
        case (i,rest) of
          (mov(a,rhs),mov(c,d)::tail) =>
            if integer a andalso integer rhs andalso eq_ea(a,d) andalso eq_ea(rhs,c) then
              loop(i::tail,acc) else loop(rest,i::acc)
        | (mov(a,rhs),j::tail) =>
            (case foldCopy(a,rhs,j) of SOME j => loop(j::tail,acc) | NONE => loop(rest,i::acc))
        | (movz(zero,I 0,ShiftImm(LSL,0)),orr(dst,a,rhs)::tail) =>
            if integer zero andalso integer dst andalso
               ((eq_ea(a,dst) andalso eq_ea(rhs,zero)) orelse
                (eq_ea(a,zero) andalso eq_ea(rhs,dst))) then loop(i::tail,acc)
            else loop(rest,i::acc)
        | (A.b(L target),(lab as Label l)::tail) =>
            if eq_lab(target,l) then loop(lab::tail,acc) else loop(rest,i::acc)
        | (_,(lab as Label l)::tail) =>
            (case conditional i of
               SOME(L target,_,_,_) =>
                 if eq_lab(target,l) then loop(lab::tail,acc) else loop(rest,i::acc)
             | _ => loop(rest,i::acc))
        | (_,A.b other::(lab as Label l)::tail) =>
            (case conditional i of
               SOME(L target,_,opposite,_) =>
                 if eq_lab(target,l) then loop(opposite other::lab::tail,acc)
                 else loop(rest,i::acc)
             | _ => loop(rest,i::acc))
        | (A.str(a,ma as M(SP,x)),ldr(rhs,M(SP,y))::tail) =>
            if x = y andalso sameClass(a,rhs) then
              loop((case a of R(D _) => fmov(rhs,a) | _ => mov(rhs,a))::tail,i::acc)
            else loop(rest,i::acc)
        | (ldr(a,ma),ldr(rhs,mb)::tail) =>
            (case pair(true,ldp,a,ma,rhs,mb) of
               SOME p => loop(tail,p::acc) | NONE => loop(rest,i::acc))
        | (A.str(a,ma),A.str(rhs,mb)::tail) =>
            (case pair(false,stp,a,ma,rhs,mb) of
               SOME p => loop(tail,p::acc) | NONE => loop(rest,i::acc))
        | _ => loop(rest,i::acc)
      val code = loop(threadBranches code,[])
      val code = loop(code,[])
    in loop(code,[])
    end
  fun relax code =
    let
      fun directive d = case d of
          Align n => if n >= 0 andalso n <= 20 then SOME(IntInf.toInt(IntInf.pow(2,n))-1) else NONE
        | Space n => if n >= 0 then SOME n else NONE
        | Global _ => SOME 0 | NumericLabel _ => SOME 0
        | Quad values => SOME(8*length values) | Bytes values => SOME(length values)
        | Double _ => SOME 8 | _ => NONE
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
                val labels = case i of Label l => (Polyhash.insert labels (l,here); labels)
                                           | _ => labels
                val () = case i of
                    Label _ => ()
                  | Directive Text => current := textOffset
                  | Directive Data => current := dataOffset
                  | Directive s =>
                      (case directive s of SOME n => advance n | NONE => barrier())
                  | _ => advance(if Option.isSome(conditional i) then 8 else 4)
            in layout(rest,labels,(i,here)::acc)
            end
      val (labels,located) = layout(code,labelTable(),[])
      fun within (segment,offset) target low high =
        case Polyhash.peek labels target of
          SOME (s,p) => s = segment andalso p-offset >= low andalso p-offset <= high
        | NONE => false
      fun loop ([],acc) = rev acc
        | loop ((i as adrp(R dst,Page target),here)::
                (j as add(R dst',R base,PageOff off),there)::rest,acc) =
            if dst = dst' andalso dst = base andalso eq_lab(target,off) andalso
               within here target (~1048576) 1048575 then
              loop(rest,adr(R dst,L target)::acc)
            else step(i,here,(j,there)::rest,acc)
        | loop ((i,here)::rest,acc) = step(i,here,rest,acc)
      and step (i,here,rest,acc) =
        case conditional i of
          NONE => loop(rest,i::acc)
        | SOME(target,_,opposite,reach) =>
            if (case target of L l => within here l (~reach) (reach-4) | _ => false) then
              loop(rest,i::acc)
            else
              let val skip = LocalLab(AddressLabels.new_named "arm64_branch_skip")
              in loop(rest,Label skip :: A.b target :: opposite(L skip) :: acc)
              end
    in loop(located,[])
    end
  fun emit (code,file) =
    let val code = relax code
        val os = TextIO.openOut file
    in (List.app (fn i => TextIO.output(os,pr_inst i)) code; TextIO.closeOut os)
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
