(* Specification of HPPA Risc code. *)

signature HP_PA_RISC =
  sig
    
    (*----------------------------------------------------------*)
    (*                  Register definitions.                   *)
    (*----------------------------------------------------------*)

    datatype reg = Gen of int   (* General Purpose Register *)
		 | Float of int (* Floating Point Register  *)
		 | Ctrl of int  (* Control Register         *)
		 | Space of int (* Space Register           *)

    val dp       : reg  (* Data pointer.  *)
    val sp       : reg  (* Stack pointer. *)
    val rp       : reg  (* Return link.   *)
    val mrp      : reg  (* Milicode return link. *)

    val tmp_gr1  : reg
    val tmp_reg0 : reg
    val tmp_reg1 : reg
    val tmp_reg2 : reg (* Used in inline_alloc only *)
    val tmp_reg3 : reg (* Used in inline_alloc only *)

    val arg0     : reg  (* Argument and return registers *)
    val arg1     : reg  (* for C function calls. *)
    val arg2     : reg
    val arg3     : reg
    val ret0     : reg  (* Result from ordinary calls. *)
    val ret1     : reg  (* Result from millicode calls. *)

    val tmp_float_reg0 : reg  (* 8-11 are caller-saves regs. *)
    val tmp_float_reg1 : reg
    val tmp_float_reg2 : reg
    val arg_float0     : reg
    val ret_float0     : reg

    val reg_eq   : reg*reg -> bool

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)
    type lvar
    val all_regs                : reg list
    val is_reg                  : lvar -> bool
    val lv_to_reg               : lvar -> reg
    val lv_to_reg_no            : lvar -> int
    val reg_args_as_lvs         : lvar list
    val reg_res_as_lvs          : lvar list
    val reg_args_ccall_as_lvs   : lvar list
    val reg_res_ccall_as_lvs    : lvar list
    val callee_save_regs_mlkit        : reg list
    val callee_save_regs_mlkit_as_lvs : lvar list
    val caller_save_regs_mlkit        : reg list
    val caller_save_regs_mlkit_as_lvs : lvar list
    val callee_save_regs_ccall        : reg list
    val callee_save_regs_ccall_as_lvs : lvar list
    val caller_save_regs_ccall        : reg list
    val caller_save_regs_ccall_as_lvs : lvar list
      
    (*----------------------------------------------------------*)
    (*                     HPPA RISC Syntax                     *)
    (*                                                          *)
    (* We do not specify cache hints in instructions...         *)
    (*                                                          *)
    (*----------------------------------------------------------*)

    val is_im5  : int -> bool
    val is_im11 : int -> bool
    val is_im12 : int -> bool
    val is_im14 : int -> bool
    val is_im17 : int -> bool
    val is_im19 : int -> bool

    type label
    datatype lab = 
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        jumps to millicode, code label, finish 
			        label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

    val eq_lab : lab * lab -> bool

    datatype cond = NEVER
                  | ALWAYS
                  | EQUAL
                  | NOTEQUAL
                  | GREATERTHAN
                  | GREATEREQUAL
                  | LESSTHAN
                  | LESSEQUAL
                  | GREATERTHAN_UNSIGNED
                  | GREATEREQUAL_UNSIGNED
                  | LESSTHAN_UNSIGNED
                  | LESSEQUAL_UNSIGNED
                  | ODD
                  | EVEN

    val revCond : cond -> cond
 
    datatype comp = EMPTY
                  | MODIFYBEFORE
                  | MODIFYAFTER

    datatype fmt = DBL | SGL | QUAD

    datatype RiscInst = 
        ADD of {cond: cond, r1: reg, r2: reg, t: reg} 
      | ADDO of {cond: cond, r1: reg, r2: reg, t: reg} 
      | ADDI of  {cond: cond, i: string, r: reg, t: reg} 
      | ADDIO of  {cond: cond, i: string, r: reg, t: reg} 
      | ADDIL of {i: string, r: reg} 
      | ADDIL' of {pr_i: unit->string, r: reg} 
      | AND of   {cond: cond, r1: reg, r2: reg, t: reg} 
      | ANDCM of {cond: cond, r1: reg, r2: reg, t: reg} 

      | B of     {n: bool, target: lab} 
      | BL of    {n: bool, target: lab, t: reg} 
      | BLE of   {n: bool, wd: string, sr: reg, b: reg} 
      | BV of    {n: bool, x: reg, b: reg} 
      | BB of    {n: bool, cond: cond, r: reg, p: int, target: lab}

      | COMB of  {cond: cond, n: bool, r1: reg, r2: reg, target: lab} 
      | COMCLR of {cond: cond, r1: reg, r2: reg, t: reg} 
      | COPY of  {r: reg, t: reg} 

      | DEPI of  {cond: cond, i: string, p: string, len: string, t: reg}

      | FABS of  {fmt: fmt, r: reg, t: reg}
      | FADD of  {fmt: fmt, r1: reg, r2: reg, t: reg}
      | FCMP of  {fmt: fmt, cond: cond, r1: reg, r2: reg}
      | FLDDS of {complt: comp, d:string, s: reg, b:reg, t:reg} 
      | FMPY of  {fmt: fmt, r1: reg, r2: reg, t: reg}
      | FSTDS of {complt: comp, r:reg, d:string, s: reg, b:reg} 
      | FSUB of  {fmt: fmt, r1: reg, r2: reg, t: reg}
      | FTEST
      | XMPYU of {r1:reg, r2: reg, t:reg} 

      | LDI of   {i: string, t: reg} 
      | LDIL of  {i: string, t: reg} 
      | LDO of   {d: string, b: reg, t: reg} 
      | LDO' of  {pr_d: unit->string, b: reg, t: reg} 
      | LDW of   {d: string, s: reg, b: reg, t: reg}
      | LDWS of  {cmplt: comp, d: string, s: reg, b: reg, t: reg}
      | LDWM of  {d: string, s: reg, b: reg, t: reg} 

      | NOP  

      | OR of    {cond: cond, r1: reg, r2: reg, t: reg} 
      | XOR of    {cond: cond, r1: reg, r2: reg, t: reg} 
      | SH1ADD of {cond: cond, r1: reg, r2: reg, t: reg} 
      | SH2ADD of {cond: cond, r1: reg, r2: reg, t: reg} 

      | SHD of   {cond: cond, r1: reg, r2: reg, p: string, t: reg} 
      | SUB of   {cond: cond, r1: reg, r2: reg, t: reg} 
      | SUBO of   {cond: cond, r1: reg, r2: reg, t: reg} 
      | SUBI of  {cond: cond, i: string, r: reg, t: reg} 
      | STW of   {r: reg, d: string, s: reg, b: reg} 
      | STWS of  {cmplt: comp, r: reg, d: string, s: reg, b: reg} 
      | STWM of  {r: reg, d: string, s: reg, b: reg} 
      | ZVDEP of {cond:cond, r:reg,d:string,t:reg}
      | MTSAR of {r:reg}
      | VEXTRS of {cond:cond, r: reg,d:string,t:reg}
      | VSHD of {cond:cond, r1:reg, r2:reg,t:reg}
      | LABEL of lab 
      | COMMENT of string 
      | NOT_IMPL of string 

      | DOT_ALIGN of   int 
      | DOT_BLOCKZ of  int 
      | DOT_CALL of    string 
      | DOT_CALLINFO of string 
      | DOT_CODE 
      | DOT_DATA 
      | DOT_DOUBLE of string
      | DOT_END 
      | DOT_ENTER 
      | DOT_ENTRY
      | DOT_EQU of     int 
      | DOT_EXPORT of  lab * string 
      | DOT_IMPORT of  lab * string 
      | DOT_LEAVE
      | DOT_EXIT
      | DOT_PROC 
      | DOT_PROCEND 
      | DOT_STRINGZ of string 
      | DOT_WORD of string 
      | DOT_BYTE of string 

      | META_IF of {cond: cond, r1: reg, r2: reg, target: lab}
      | META_BL of {n: bool, target: lab, rpLink: reg, callStr : string}
      | META_BV of {n: bool, x: reg, b: reg}
      | META_IF_BIT of {r: reg, bitNo: int, target: lab}
      | META_B of {n: bool, target: lab}

      | SEQ of RiscInst * RiscInst
    
    datatype TopDecl =
        FUN of label * RiscInst list
      | FN of label * RiscInst list

    type AsmPrg = {top_decls: TopDecl list,
		   init_code: RiscInst list,
		   exit_code: RiscInst list,
		   static_data: RiscInst list}

    (*******************************)
    (* Basic Compilation Functions *)
    (*******************************)
    val load_label        : lab * reg -> RiscInst
    val regs_defd         : RiscInst -> reg list
    val regs_used         : RiscInst -> reg list
    val does_inst_nullify : RiscInst -> bool
    val is_jmp            : RiscInst -> bool
    val is_asm_directive  : RiscInst -> bool

    (******************)
    (* PrettyPrinting *)
    (******************)
    type StringTree
    val layout_AsmPrg : AsmPrg -> StringTree

    (* To Emit Code *)
    val pr_inst        : RiscInst -> string
    val pp_lab         : lab -> string
    val pr_reg         : reg -> string
    val output_AsmPrg : TextIO.outstream * AsmPrg -> unit

  end

