(* Shared instruction representation. Assembly syntax belongs to PrintArm64. *)
structure Arm64Instructions = struct
  open InstsBase
  datatype reg = X of int | W of int | D of int | S of int | SP
  datatype condition = EQ | NE | LT | GE | GT | LE | LO | HS | HI | LS | MI | PL | VS | VC
  datatype shift = LSL | LSR | ASR
  datatype ea = R of reg | I of IntInf.int | L of lab | Forward of int
              | M of reg * int | PreIndex of reg * int
              | Indexed of reg * reg * shift * int
              | Shifted of reg * shift * int | ShiftImm of shift * int
              | Page of lab | PageOff of lab | GotPage of lab | GotMemory of reg * lab
              | C of condition
  datatype directive = Text | Data | Align of int | Global of lab
                     | Quad of string list | Bytes of string list | Double of string
                     | Space of int | NumericLabel of int | Raw of string
  datatype inst = Label of lab | Directive of directive
    | b of ea
    | bl of ea
    | br of ea
    | blr of ea
    | brk of ea
    | b_eq of ea
    | b_ne of ea
    | b_lt of ea
    | b_ge of ea
    | b_gt of ea
    | b_le of ea
    | b_lo of ea
    | b_hs of ea
    | b_hi of ea
    | b_ls of ea
    | b_mi of ea
    | b_pl of ea
    | b_vs of ea
    | b_vc of ea
    | mov of ea * ea
    | fmov of ea * ea
    | ldr of ea * ea
    | ldrb of ea * ea
    | ldrh of ea * ea
    | str of ea * ea
    | strb of ea * ea
    | strh of ea * ea
    | cmp of ea * ea
    | fcmp of ea * ea
    | tst of ea * ea
    | adr of ea * ea
    | adrp of ea * ea
    | negs of ea * ea
    | fneg of ea * ea
    | fabs of ea * ea
    | fsqrt of ea * ea
    | fcvt of ea * ea
    | fcvtzs of ea * ea
    | scvtf of ea * ea
    | sxtb of ea * ea
    | sxth of ea * ea
    | sxtw of ea * ea
    | uxtb of ea * ea
    | uxth of ea * ea
    | uxtw of ea * ea
    | ldaxr of ea * ea
    | cset of ea * ea
    | cbz of ea * ea
    | cbnz of ea * ea
    | add of ea * ea * ea
    | adds of ea * ea * ea
    | sub of ea * ea * ea
    | subs of ea * ea * ea
    | and_ of ea * ea * ea
    | orr of ea * ea * ea
    | eor of ea * ea * ea
    | lsl of ea * ea * ea
    | lsr of ea * ea * ea
    | asr of ea * ea * ea
    | mul of ea * ea * ea
    | smulh of ea * ea * ea
    | fadd of ea * ea * ea
    | fsub of ea * ea * ea
    | fmul of ea * ea * ea
    | fdiv of ea * ea * ea
    | fmax of ea * ea * ea
    | fmin of ea * ea * ea
    | movz of ea * ea * ea
    | movk of ea * ea * ea
    | ldp of ea * ea * ea
    | stp of ea * ea * ea
    | stlxr of ea * ea * ea
    | tbz of ea * ea * ea
    | tbnz of ea * ea * ea
    | sbfx of ea * ea * ea * ea
    | ubfx of ea * ea * ea * ea
    | csel of ea * ea * ea * ea
    | ret
end
