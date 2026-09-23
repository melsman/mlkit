(* The only translation from typed instructions to assembler syntax. *)
structure PrintArm64 = struct
  structure A = Arm64Instructions
  open Arm64Instructions
  fun pr_reg r =
    let
      fun numbered (prefix,n,limit) =
        if n >= 0 andalso n <= limit then prefix ^ Int.toString n
        else raise Fail "ARM64: invalid register"
    in
      case r of X n => numbered("x",n,30) | W n => numbered("w",n,30)
              | D n => numbered("d",n,31) | S n => numbered("s",n,31)
              | SP => "sp"
    end
  fun pr_condition c = case c of
      EQ => "eq" | NE => "ne" | LT => "lt" | GE => "ge" | GT => "gt" | LE => "le"
    | LO => "lo" | HS => "hs" | HI => "hi" | LS => "ls" | MI => "mi" | PL => "pl"
    | VS => "vs" | VC => "vc"
  fun pr_shift s = case s of LSL => "lsl" | LSR => "lsr" | ASR => "asr"
  fun pr_ea a = case a of
      R r => pr_reg r | I n => "#" ^ InstsBase.intToStr n
    | L l => InstsBase.pr_lab l | Forward n => Int.toString n ^ "f"
    | M(r,n) => "[" ^ pr_reg r ^ ", #" ^ InstsBase.i2s n ^ "]"
    | PreIndex(r,n) => "[" ^ pr_reg r ^ ", #" ^ InstsBase.i2s n ^ "]!"
    | Indexed(a,rhs,s,n) => "[" ^ pr_reg a ^ ", " ^ pr_reg rhs ^ ", " ^ pr_shift s ^ " #" ^ Int.toString n ^ "]"
    | Shifted(r,s,n) => pr_reg r ^ ", " ^ pr_shift s ^ " #" ^ Int.toString n
    | ShiftImm(s,n) => pr_shift s ^ " #" ^ Int.toString n
    | Page l => InstsBase.pr_lab l ^ "@PAGE"
    | PageOff l => InstsBase.pr_lab l ^ "@PAGEOFF"
    | GotPage l => InstsBase.pr_lab l ^ "@GOTPAGE"
    | GotMemory(r,l) => "[" ^ pr_reg r ^ ", " ^ InstsBase.pr_lab l ^ "@GOTPAGEOFF]"
    | C c => pr_condition c
  fun pr_directive d = case d of
      Text => ".text" | Data => ".data" | Align n => ".p2align " ^ Int.toString n
    | Global l => ".globl " ^ InstsBase.pr_lab l
    | Quad values => ".quad " ^ String.concatWith "," values
    | Bytes values => ".byte " ^ String.concatWith "," values
    | Double value => ".double " ^ value | Space n => ".space " ^ Int.toString n
    | NumericLabel n => Int.toString n ^ ":" | Raw text => text
  fun pr_inst i =
    let fun op1 n a = "\t" ^ n ^ " " ^ pr_ea a ^ "\n"
        fun op2 n (a,rhs) = "\t" ^ n ^ " " ^ pr_ea a ^ ", " ^ pr_ea rhs ^ "\n"
        fun op3 n (a,rhs,c) = "\t" ^ n ^ " " ^ pr_ea a ^ ", " ^ pr_ea rhs ^ ", " ^ pr_ea c ^ "\n"
        fun op4 n (a,rhs,c,d) = "\t" ^ n ^ " " ^ pr_ea a ^ ", " ^ pr_ea rhs ^ ", " ^ pr_ea c ^ ", " ^ pr_ea d ^ "\n"
    in
      case i of Label l => InstsBase.pr_lab l ^ ":\n"
              | Directive d => pr_directive d ^ "\n"
              | A.ret => "\tret\n"
              | A.b args => op1 "b" args
              | bl args => op1 "bl" args
              | br args => op1 "br" args
              | blr args => op1 "blr" args
              | brk args => op1 "brk" args
              | b_eq args => op1 "b.eq" args
              | b_ne args => op1 "b.ne" args
              | b_lt args => op1 "b.lt" args
              | b_ge args => op1 "b.ge" args
              | b_gt args => op1 "b.gt" args
              | b_le args => op1 "b.le" args
              | b_lo args => op1 "b.lo" args
              | b_hs args => op1 "b.hs" args
              | b_hi args => op1 "b.hi" args
              | b_ls args => op1 "b.ls" args
              | b_mi args => op1 "b.mi" args
              | b_pl args => op1 "b.pl" args
              | b_vs args => op1 "b.vs" args
              | b_vc args => op1 "b.vc" args
              | mov args => op2 "mov" args
              | fmov args => op2 "fmov" args
              | ldr args => op2 "ldr" args
              | ldrb args => op2 "ldrb" args
              | ldrh args => op2 "ldrh" args
              | A.str args => op2 "str" args
              | strb args => op2 "strb" args
              | strh args => op2 "strh" args
              | cmp args => op2 "cmp" args
              | fcmp args => op2 "fcmp" args
              | tst args => op2 "tst" args
              | adr args => op2 "adr" args
              | adrp args => op2 "adrp" args
              | negs args => op2 "negs" args
              | fneg args => op2 "fneg" args
              | fabs args => op2 "fabs" args
              | fsqrt args => op2 "fsqrt" args
              | fcvt args => op2 "fcvt" args
              | fcvtzs args => op2 "fcvtzs" args
              | scvtf args => op2 "scvtf" args
              | sxtb args => op2 "sxtb" args
              | sxth args => op2 "sxth" args
              | sxtw args => op2 "sxtw" args
              | uxtb args => op2 "uxtb" args
              | uxth args => op2 "uxth" args
              | uxtw args => op2 "uxtw" args
              | ldaxr args => op2 "ldaxr" args
              | cset args => op2 "cset" args
              | cbz args => op2 "cbz" args
              | cbnz args => op2 "cbnz" args
              | add args => op3 "add" args
              | adds args => op3 "adds" args
              | sub args => op3 "sub" args
              | subs args => op3 "subs" args
              | and_ args => op3 "and" args
              | orr args => op3 "orr" args
              | eor args => op3 "eor" args
              | lsl args => op3 "lsl" args
              | lsr args => op3 "lsr" args
              | asr args => op3 "asr" args
              | mul args => op3 "mul" args
              | smulh args => op3 "smulh" args
              | fadd args => op3 "fadd" args
              | fsub args => op3 "fsub" args
              | fmul args => op3 "fmul" args
              | fdiv args => op3 "fdiv" args
              | fmax args => op3 "fmax" args
              | fmin args => op3 "fmin" args
              | movz args => op3 "movz" args
              | movk args => op3 "movk" args
              | ldp args => op3 "ldp" args
              | stp args => op3 "stp" args
              | stlxr args => op3 "stlxr" args
              | tbz args => op3 "tbz" args
              | tbnz args => op3 "tbnz" args
              | sbfx args => op4 "sbfx" args
              | ubfx args => op4 "ubfx" args
              | csel args => op4 "csel" args
    end
end
