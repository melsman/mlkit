(* Specification of HPPA Risc code. *)

functor HpPaRisc(structure Labels : ADDRESS_LABELS
		 structure Lvars : LVARS
		 structure PP : PRETTYPRINT
		 structure Crash : CRASH
		     ):HP_PA_RISC =
  struct

    (***********)
    (* Logging *)
    (***********)
    fun die s  = Crash.impossible ("SubstAndSimplify." ^ s)

    (*----------------------------------------------------------*)
    (*                  Register definitions.                   *)
    (*----------------------------------------------------------*)

    datatype reg = Gen of int   (* General Purpose Register *)
		 | Float of int (* Floating Point Register  *)
		 | Ctrl of int  (* Control Register         *)
		 | Space of int (* Space Register           *)

    val dp       = Gen 27  (* Data pointer.  *)
    val sp       = Gen 30  (* Stack pointer. *)
    val rp       = Gen 2   (* Return link.   *)
    val mrp      = Gen 31  (* (Milicode) return link. *)

    val tmp_reg1 = Gen 19
    val tmp_reg2 = Gen 20
(*    val tmp_reg3 = Gen 21
    val tmp_reg4 = Gen 22*)

    val arg0     = Gen 26  (* Argument and return registers *)
    val arg1     = Gen 25  (* for C function calls. *)
    val arg2     = Gen 24
    val arg3     = Gen 23
    val ret0     = Gen 28  (* Result from ordinary calls. *)

    fun reg_eq(Gen i1,Gen i2) = i1 = i2
      | reg_eq(Float i1,Float i2) = i2 = i2
      | reg_eq(Ctrl i1,Ctrl i2) = i1 = i2
      | reg_eq(Space i1,Space i2) = i1 = i2
      | reg_eq _ = false

    (*-----------------------------------------------------------*)
    (* Converting Between General Registers and Precolored Lvars *)
    (* As Used In The Phases Preceeding Code Generation          *)
    (*-----------------------------------------------------------*)
    type lvar = Lvars.lvar
    local
      structure LvarFinMap = Lvars.Map

      val regs = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
      val reg_gens = map Gen regs
      val reg_lvs = map (fn i => Lvars.new_named_lvar ("ph"^Int.toString i)) regs
      val map_lvs_to_reg = LvarFinMap.fromList(ListPair.zip(reg_lvs,reg_gens))
      val map_reg_to_lvs = Vector.fromList reg_lvs
    in
      fun is_reg lv = 
	(case LvarFinMap.lookup map_lvs_to_reg lv of
	   SOME reg => true
	 | NONE  => false)

      fun lv_to_reg lv = 
	(case LvarFinMap.lookup map_lvs_to_reg lv of
	   NONE => die "lv_to_phreg: lv not a register"
	 | SOME i => i)

      fun reg_to_lv(Gen i) = Vector.sub(map_reg_to_lvs,i)
	| reg_to_lv _ = die "reg_to_lv: reg is not a general register (Gen)"

      val reg_args = map Gen [1,2,3,4,5,6,7,8,9,10] 
      val reg_args_as_lvs = map reg_to_lv reg_args
      val reg_res = map Gen [10,9,8,7,6,5,4,3,2,1] 
      val reg_res_as_lvs = map reg_to_lv reg_res

      val reg_args_ccall = map Gen [1,2,3,4]
      val reg_args_ccall_as_lvs = map reg_to_lv reg_args_ccall
      val reg_res_ccall = map Gen [4,3,2,1] 
      val reg_res_ccall_as_lvs = map reg_to_lv reg_res_ccall

      val callee_save_regs = map Gen [7,8,9,10]
      val callee_save_regs_as_lvs = map reg_to_lv callee_save_regs

      val caller_save_regs = map Gen [1,2,3,4]
      val caller_save_regs_as_lvs = map reg_to_lv caller_save_regs
    end

    (*----------------------------------------------------------*)
    (*                     Some Basic Tools                     *)
    (*----------------------------------------------------------*)

    fun die s = Crash.impossible ("HpPaRisc." ^ s)

    fun is_im5  n = n <    16 andalso n >=    ~16
    fun is_im11 n = n <  1024 andalso n >=  ~1024
    fun is_im12 n = n <  2048 andalso n >=  ~2048
    fun is_im14 n = n <  8192 andalso n >=  ~8192
    fun is_im17 n = n < 65536 andalso n >= ~65536
    fun is_im19 n = n < 262144 andalso n >= ~262144

    (*----------------------------------------------------------*)
    (*                          Code                            *)
    (*----------------------------------------------------------*)

    type label = Labels.label
    datatype lab = DatLab of label      (* For data to propagate across program units *)
		 | LocalLab of label    (* Local label inside a block *)
		 | NameLab of string    (* For ml strings, jumps to runtime system,
					   jumps to millicode, code label, finish 
					   label, etc. *)

    fun eq_lab (DatLab label1, DatLab label2) = Labels.eq(label1,label2)
      | eq_lab (LocalLab label1, LocalLab label2) = Labels.eq(label1,label2)
      | eq_lab (NameLab s1, NameLab s2) = s1 = s2
      | eq_lab _ = false

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

    fun revCond NEVER = ALWAYS
      | revCond ALWAYS = NEVER
      | revCond EQUAL = NOTEQUAL
      | revCond NOTEQUAL = EQUAL
      | revCond GREATERTHAN = LESSEQUAL
      | revCond GREATEREQUAL = LESSTHAN
      | revCond LESSTHAN = GREATEREQUAL
      | revCond LESSEQUAL = GREATERTHAN
      | revCond GREATERTHAN_UNSIGNED = LESSEQUAL_UNSIGNED
      | revCond GREATEREQUAL_UNSIGNED = LESSTHAN_UNSIGNED
      | revCond LESSTHAN_UNSIGNED = GREATEREQUAL_UNSIGNED
      | revCond LESSEQUAL_UNSIGNED = GREATERTHAN_UNSIGNED
      | revCond ODD = EVEN
      | revCond EVEN = ODD

    datatype comp = EMPTY
		  | MODIFYBEFORE
		  | MODIFYAFTER

    datatype fmt = DBL | SGL | QUAD

    datatype RiscInst = 
        ADD of {cond: cond, r1: reg, r2: reg, t: reg} 
      | ADDO of {cond: cond, r1: reg, r2: reg, t: reg} 
      | ADDI of  {cond: cond, i: string, r: reg, t: reg} 
      | ADDIO of  {cond: cond, i: string, r: reg, t: reg} (* Trap on overflow *)
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
      | SUBO of  {cond: cond, r1: reg, r2: reg, t: reg} 
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

    type RiscPrg = {top_decls: TopDecl list,
		    init_code: RiscInst list,
		    exit_code: RiscInst list,
		    static_data: RiscInst list}

    (*----------------------------------------------------------*)
    (*                    Pretty printing                       *)
    (*----------------------------------------------------------*)

    local
      val output_stream : TextIO.outstream ref = ref TextIO.stdOut
      fun out str = TextIO.output(!output_stream,str)
    in
      fun reset_output_stream () = output_stream := TextIO.stdOut
      fun set_out_stream stream = output_stream := stream
      fun out_list str_list = out (concat str_list)
    end

    fun pp_i i = Int.toString i
    fun pp_reg(Gen i,acc) = "%r"::(pp_i i)::acc
      | pp_reg(Float i,acc) = "%fr"::(pp_i i)::acc
      | pp_reg(Ctrl i,acc) = "%cr"::(pp_i i)::acc
      | pp_reg(Space i,acc) = "%sr"::(pp_i i)::acc

    fun pr_reg reg = concat(pp_reg(reg,[]))

    fun pp_lab (DatLab l) = "DatLab" ^ Int.toString (Labels.key l)
      | pp_lab (LocalLab l) = "L$" ^ Labels.pr_label l ^ Int.toString (Labels.key l) (* L$ is not allowed in HP's as *)
      | pp_lab (NameLab s) = s

    fun pp_lab' (DatLab l,acc) = "DatLab" :: (Int.toString (Labels.key l)) :: acc
      | pp_lab' (LocalLab l,acc) = "L$" :: (Labels.pr_label l) :: (Int.toString (Labels.key l)) :: acc (* L$ is not allowed in HP's as *)
      | pp_lab' (NameLab s,acc) = s :: acc

    fun pp_cond NEVER = ""
      | pp_cond ALWAYS = ",TR"
      | pp_cond EQUAL = ",="
      | pp_cond NOTEQUAL = ",<>"
      | pp_cond GREATERTHAN = ",>"
      | pp_cond GREATEREQUAL = ",>="
      | pp_cond LESSTHAN = ",<"
      | pp_cond LESSEQUAL = ",<="
      | pp_cond GREATERTHAN_UNSIGNED = ",>>"
      | pp_cond GREATEREQUAL_UNSIGNED = ",>>="
      | pp_cond LESSTHAN_UNSIGNED = ",<<"
      | pp_cond LESSEQUAL_UNSIGNED = ",<<="
      | pp_cond ODD = ",OD"
      | pp_cond EVEN = ",EV"

    fun pp_comp EMPTY = ""
      | pp_comp MODIFYBEFORE = ",MB"
      | pp_comp MODIFYAFTER = ",MA"

    fun pp_fmt SGL = ",SGL"
      | pp_fmt DBL = ",DBL"
      | pp_fmt QUAD = ",QUAD"

    val indent = "\t"

    fun pp_inst (inst,acc) : string list =
      case inst of
	ADD {cond, r1, r2, t} =>
	   (indent::"ADD"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | ADDO {cond, r1, r2, t} =>
	   (indent::"ADDO"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | ADDI {cond, i, r, t} =>
	   (indent::"ADDI"::(pp_cond cond)::indent::i::", "::(pp_reg (r,", "::(pp_reg (t,acc)))))
      | ADDIO {cond, i, r, t} =>
	   (indent::"ADDIO"::(pp_cond cond)::indent::i::", "::(pp_reg (r,", "::(pp_reg (t,acc)))))
      | ADDIL {i, r} =>
	   (indent::"ADDIL"::indent::i::", "::(pp_reg (r,acc)))
      | ADDIL' {pr_i, r} =>
	   (indent::"ADDIL"::indent::(pr_i())::", "::(pp_reg (r,acc)))
      | AND {cond, r1, r2, t} =>
	   (indent::"AND"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | ANDCM {cond, r1, r2, t} =>
	   (indent::"ANDCM"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | B {n, target} =>
	   (indent::"B"::(if n then ",n" else "")::indent::(pp_lab' (target,acc)))
      | BB {n, cond, r, p, target} => 
	   (indent::"BB"::(pp_cond cond)::(if n then ",n" else "")::indent::(pp_reg (r,", "::(Int.toString p)::", "::(pp_lab' (target,acc)))))
      | BL {n, target, t} =>
	   (indent::"BL"::(if n then ",n" else "")::indent::(pp_lab' (target,", "::(pp_reg (t,acc)))))
      | BLE {n, wd, sr, b} =>
	   (indent::"BLE"::(if n then ",n" else "")::indent::wd::"("::(pp_reg (sr,", "::(pp_reg (b,")"::acc)))))
      | BV {n, x, b} =>
	   (indent::"BV"::(if n then ",n" else "")::indent::(pp_reg (x,"("::(pp_reg (b,")"::acc)))))
      | COMB {cond, n, r1, r2, target} => 
	   (indent::"COMB"::(pp_cond cond)::(if n then ",n" else "")::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_lab' (target,acc)))))))
      | COMCLR {cond, r1, r2, t} =>
	   (indent::"COMCLR"::(pp_cond cond)::indent::pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc))))))
      | COPY {r, t} => 
	   (indent::"COPY"::indent::(pp_reg (r,", "::(pp_reg (t,acc)))))
      | DEPI {cond, i, p, len, t} =>
	   (indent::"DEPI"::(pp_cond cond)::indent::i::", "::p::", "::len::", "::(pp_reg (t,acc)))
      | FABS {fmt, r, t} =>
	   (indent::"FABS"::(pp_fmt fmt)::indent::(pp_reg (r,", "::(pp_reg (t,acc)))))
      | FADD {fmt, r1, r2, t} =>
	   (indent::"FADD"::(pp_fmt fmt)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | FCMP {fmt, cond, r1, r2} =>
	   (indent::"FCMP"::(pp_fmt fmt)::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,acc)))))
      | FLDDS {complt, d, s, b, t} =>
	   (indent::"FLDDS"::(pp_comp complt)::indent::d::"("::(pp_reg (s,", "::(pp_reg (b,"), "::(pp_reg (t,acc)))))))
      | FMPY {fmt, r1, r2, t} =>
	   (indent::"FMPY"::(pp_fmt fmt)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | FSTDS {complt, r, d, s, b} =>
	   (indent::"FSTDS"::(pp_comp complt)::indent::(pp_reg (r,","::d::"("::(pp_reg (s,", "::(pp_reg (b,")"::acc)))))))
      | FSUB {fmt, r1, r2, t} =>
	   (indent::"FSUB"::(pp_fmt fmt)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | FTEST => (indent::"FTEST"::acc)
      | XMPYU {r1, r2, t} =>
	   (indent::"XMPYU"::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | LDI {i, t} =>
	   (indent::"LDI"::indent::i::", "::(pp_reg (t,acc)))
      | LDIL {i, t} =>
	   (indent::"LDIL"::indent::i::", "::(pp_reg (t,acc)))
      | LDO {d, b, t} =>
	   (indent::"LDO"::indent::d::"("::(pp_reg (b,"), "::(pp_reg (t,acc)))))
      | LDO' {pr_d, b, t} =>
	   (indent::"LDO"::indent::(pr_d())::"("::(pp_reg (b,"), "::(pp_reg (t,acc)))))
      | LDW {d, s, b, t} =>
	   (indent::"LDW"::indent::d::"("::(pp_reg (s,", "::(pp_reg (b,"), "::(pp_reg (t,acc)))))))
      | LDWS {cmplt, d, s, b, t} =>
	   (indent::"LDWS"::(pp_comp cmplt)::indent::d::"("::(pp_reg (s,", "::(pp_reg (b,"), "::(pp_reg (t,acc)))))))
      | LDWM {d, s, b, t} =>
	   (indent::"LDWM"::indent::d::"("::(pp_reg (s,", "::(pp_reg (b,"), "::(pp_reg (t,acc)))))))
      | NOP => (indent::"NOP"::acc)
      | OR {cond, r1, r2, t} =>
	   (indent::"OR"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | XOR {cond, r1, r2, t} =>
	   (indent::"XOR"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | SHD {cond, r1, r2, p, t} =>
	   (indent::"SHD"::(pp_cond cond)::indent::pp_reg (r1,", "::(pp_reg (r2,", "::p::", "::(pp_reg (t,acc))))))
      | SH1ADD {cond, r1, r2, t} =>
	   (indent::"SH1ADD"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | SH2ADD {cond, r1, r2, t} =>
	   (indent::"SH2ADD"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | SUB {cond, r1, r2, t} =>
	   (indent::"SUB"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | SUBO {cond, r1, r2, t} =>
	   (indent::"SUBO"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))
      | SUBI {cond, i, r, t} =>
	   (indent::"SUBI"::(pp_cond cond)::indent::i::", "::(pp_reg (r,", "::(pp_reg (t,acc)))))
      | STW {r, d, s, b} =>
	   (indent::"STW"::indent::(pp_reg (r,", "::d::"("::(pp_reg (s,", "::(pp_reg (b,")"::acc)))))))
      | STWS {cmplt, r, d, s, b} =>
	   (indent::"STWS"::(pp_comp cmplt)::indent::(pp_reg (r,", "::d::"("::(pp_reg (s,", "::(pp_reg (b,")"::acc)))))))
      | STWM {r, d, s, b} =>
	   (indent::"STWM"::indent::(pp_reg (r,", "::d::"("::(pp_reg (s,", "::(pp_reg (b,")"::acc)))))))

      | ZVDEP {cond,r,d,t} =>
	   (indent::"ZVDEP"::indent::(pp_cond cond)::indent::(pp_reg (r,", "::d::", "::(pp_reg (t,acc)))))
      | MTSAR {r} =>
           (indent::"MTSAR"::indent::(pp_reg(r,acc)))
      | VEXTRS {cond,r,d,t} =>
	   (indent::"VEXTRS"::indent::(pp_cond cond)::indent::(pp_reg (r,", "::d::", "::(pp_reg (t,acc)))))
      | VSHD {cond:cond,r1:reg, r2:reg,t:reg} =>
	   (indent::"VSHD"::(pp_cond cond)::indent::(pp_reg (r1,", "::(pp_reg (r2,", "::(pp_reg (t,acc)))))))

      | LABEL lab => pp_lab' (lab,acc)
      | COMMENT s => (indent::indent::indent::indent::"; "::s::acc)
      | NOT_IMPL s => (indent::indent::indent::";NOT IMPLEMENTED "::s::acc)
      | DOT_ALIGN i => (indent::".ALIGN "::(Int.toString i)::acc)
      | DOT_BLOCKZ i=> (indent::".BLOCKZ "::(Int.toString i)::acc)
      | DOT_CALL s => (indent::".CALL "::s::acc)
      | DOT_CALLINFO s => (indent::".CALLINFO "::s::acc)
      | DOT_CODE => (indent::".CODE"::acc)
      | DOT_DATA => (indent::".DATA"::acc)
      | DOT_DOUBLE s => (indent::".DOUBLE "::s::acc)
      | DOT_END => (indent::".END"::acc)
      | DOT_ENTER => (indent::".ENTER"::acc)
      | DOT_ENTRY => (indent::".ENTRY"::acc)
      | DOT_EQU i => (indent::".EQU "::(Int.toString i)::acc)
      | DOT_EXPORT (lab, s) => (indent::".EXPORT "::(pp_lab lab)::", "::s::acc)
      | DOT_IMPORT (lab, s) => (indent::".IMPORT "::(pp_lab lab)::", "::s::acc)
      | DOT_LEAVE => (indent::".LEAVE"::acc)
      | DOT_EXIT => (indent::".EXIT"::acc)
      | DOT_PROC => (indent::".PROC"::acc)
      | DOT_PROCEND => (indent::".PROCEND"::acc)
      | DOT_STRINGZ s => 
            (* generate a .BYTE pseudo instuction for each character in
               the string and generate a .BYTE 0 instruction at the end. *)
            foldr(fn (ch, acc) => 
                       indent :: ".BYTE " :: Int.toString(ord ch) :: "\n" :: acc
                      )(indent :: ".BYTE 0" :: acc) (explode s)
      | DOT_WORD w => (indent::".WORD "::w::acc)
      | DOT_BYTE b => (indent::".BYTE "::b::acc)

      | META_IF {cond, r1, r2, target} =>
	   (indent::"META_IF(cond: "::(pp_cond cond)::", r1: "::(pp_reg (r1,", r2: ":: 
	    (pp_reg (r2,", target: "::(pp_lab' (target,")"::acc)))))))
      | META_BL {n, target, rpLink, callStr} =>
	   (indent::"META_BL(n: "::(if n then "true" else "false")::", target: "::(pp_lab' (target,", rpLink: ":: 
	    (pp_reg (rpLink,", callStr: "::callStr::")"::acc)))))
      | META_BV {n, x, b} =>
	   (indent::"META_BV(n: "::(if n then "true" else "false")::", x: "::(pp_reg (x,", b: ":: 
	    (pp_reg (b,")"::acc)))))
      | META_IF_BIT {r, bitNo, target} =>
	   (indent::"META_IF_BIT(r: "::(pp_reg (r,", bitNo: "::(Int.toString bitNo)::", target: "::(pp_lab' (target,")"::acc)))))
      | META_B {n, target} =>
	   (indent::"META_B(n: "::(if n then "true" else "false")::", target: "::(pp_lab' (target,")"::acc)))

      | SEQ _ => die "pp_inst - SEQ"

    fun output_RiscPrg (os,{top_decls,init_code,exit_code,static_data}) =
      let
	fun fold ([], acc) = acc
	  | fold (inst::insts, acc) = "\n"::(pp_inst(inst, fold (insts, acc)))
	fun out_risc_insts insts = out_list (fold(insts, []))
	fun pp_top_decl(FUN(lab,insts)) = 
	  (TextIO.output(os,"\nfun " ^ Labels.pr_label lab ^ " is");
	   out_risc_insts insts)
	  | pp_top_decl(FN(lab,insts)) =
	  (TextIO.output(os,"\nfn " ^ Labels.pr_label lab ^ " is");
	   out_risc_insts insts)
      in
	(set_out_stream os;
	 TextIO.output(os,"; Start of HPPA Code");
	 out_risc_insts init_code;
	 List.app pp_top_decl top_decls;
	 out_risc_insts exit_code;
	 out_risc_insts static_data;
	 TextIO.output(os,"\n; End of HPPA Code\n");
	 reset_output_stream())
      end

    type StringTree = PP.StringTree
    fun layout_RiscPrg({top_decls,init_code,exit_code,static_data}) =
      let
	open PP
	fun layout_risc_inst i = LEAF(concat(pp_inst(i,[])))
	val init_node = NODE{start="Begin InitCode",
			     finish="End InitCode",
			     indent=2,
			     childsep=NOSEP,
			     children = map layout_risc_inst init_code}
	val exit_node = NODE{start="Begin ExitCode",
			     finish="End ExitCode",
			     indent=2,
			     childsep=NOSEP,
			     children=map layout_risc_inst exit_code}
	val static_data_node = NODE{start="Begin Static Data",
				    finish="End Static Data",
				    indent=2,
				    childsep=NOSEP,
				    children=map layout_risc_inst static_data}
	fun layout_top_decl(FUN(lab,risc_insts)) =
          NODE{start = "FUN " ^ Labels.pr_label lab ^ " is {",
	       finish = "}", 
	       indent = 2, 
	       childsep = RIGHT ";",
	       children = map layout_risc_inst risc_insts}
	  | layout_top_decl (FN(lab,risc_insts)) =
	  NODE{start = "FN " ^ Labels.pr_label lab ^ " is {",
	       finish = "}", 
	       indent = 2, 
	       childsep = RIGHT ";", 
	       children = map layout_risc_inst risc_insts}
	val body_node = NODE{start="",
			     finish="",
			     indent=0,
			     childsep=RIGHT " ",
			     children=map layout_top_decl top_decls}
      in
	NODE{start="HP-PARISC program begin",
	     finish="HP-PARISC program end",
	     indent=2,
	     childsep=NOSEP,
	     children = [init_node,body_node,exit_node,static_data_node]}
      end

    (*----------------------------------------------------------*)
    (*             Basic Compilation Functions                  *)
    (*----------------------------------------------------------*)

    infix ++
    val op ++ = SEQ

    fun load_label (lab, destReg) = 
      ADDIL' {pr_i=fn() => "L'" ^ pp_lab lab ^ "-$global$", r=dp} ++
      LDO'   {pr_d=fn() => "R'" ^ pp_lab lab ^ "-$global$", b=Gen 1, t=destReg}

    (*----------------------------------------------------------*)
    (*             Defs and Uses (for scheduling)               *)
    (*----------------------------------------------------------*)

    fun regs_defd i = 
      case i of
	ADD       {cond, r1, r2, t} => [t]
      | ADDO      {cond, r1, r2, t} => [t]
      | ADDI      {cond, i, r, t} => [t]
      | ADDIO     {cond, i, r, t} => [t]
      | ADDIL     {i, r} => [Gen 1]
      | ADDIL'    {pr_i, r} => [Gen 1]
      | AND       {cond, r1, r2, t} => [t]
      | ANDCM     {cond, r1, r2, t} => [t]

      | B         {n, target} => []
      | BB        {n, cond, r, p, target} => []
      | BL        {n, target, t} => [t]
      | BLE       {n, wd, sr, b} => []
      | BV        {n, x, b} => []

      | COMB      {cond, n, r1, r2, target} => die "regs_defd - COMB" 
      | COMCLR    {cond, r1, r2, t} => [t]
      | COPY      {r, t} => [t]

      | DEPI      {cond, i, p, len, t} => [t]

      | FABS      {fmt, r, t} => [t] 
      | FADD      {fmt, r1, r2, t} => [t] 
      | FCMP      {fmt, cond, r1, r2} => [] (* FStatusReg *)
      | FLDDS     {complt, d, s, b, t} => [t,b]
      | FMPY      {fmt, r1, r2, t} => [t] 
      | FSTDS     {complt, r, d, s, b} => [b]
      | FSUB      {fmt, r1, r2, t} => [t] 
      | FTEST      => []
      | XMPYU     {r1, r2, t} => [t]

      | LDI       {i, t} => [t]
      | LDIL      {i, t} => [t]
      | LDO       {d, b, t} => [t]
      | LDO'      {pr_d, b, t} => [t]
      | LDW       {d, s, b, t} => [t]
      | LDWS      {cmplt, d, s, b, t} => [t]
      | LDWM      {d, s, b, t} => [b, t]

      | NOP       => die "regs_defd - NOP" 

      | OR        {cond, r1, r2, t} => [t]
      | XOR        {cond, r1, r2, t} => [t]
      | SH1ADD    {cond, r1, r2, t} => [t]
      | SH2ADD    {cond, r1, r2, t} => [t]

      | SHD {cond, r1, r2, p, t} => [t]
      | SUB {cond, r1, r2, t} => [t]
      | SUBO {cond, r1, r2, t} => [t]
      | SUBI {cond, i, r, t} => [t]
      | STW {r, d, s, b} => []
      | STWS {cmplt, r, d, s, b} => []
      | STWM {r, d, s, b} => [b]

      | ZVDEP {cond, r,d,t} => [t]
      | MTSAR {r} => [Ctrl 11]
      | VEXTRS {cond,r,d,t} => [t]
      | VSHD {cond, r1,r2,t} => [t]

      | LABEL lab => []
      | COMMENT s => []
      | NOT_IMPL s => []
      | DOT_ALIGN i => []
      | DOT_BLOCKZ i=> []
      | DOT_CALL s => []
      | DOT_CALLINFO s => []
      | DOT_CODE => []
      | DOT_DATA => []
      | DOT_DOUBLE s => []
      | DOT_END => []
      | DOT_ENTER => []
      | DOT_ENTRY => []
      | DOT_EQU i => []
      | DOT_EXPORT (lab, s) => []
      | DOT_IMPORT (lab, s) => []
      | DOT_LEAVE => []
      | DOT_EXIT => []
      | DOT_PROC => []
      | DOT_PROCEND => []
      | DOT_STRINGZ s => []
      | DOT_WORD w => []
      | DOT_BYTE b => []

      | META_IF {cond, r1, r2, target} => []
      | META_BL {n, target, rpLink, callStr} => []
      | META_BV {n, x, b} => []
      | META_IF_BIT {r, bitNo, target} => []
      | META_B {n, target} => []

      | SEQ _ => die "regs_defd - SEQ" 

    fun regs_used i = 
      case i of
	ADD       {cond, r1, r2, t} => [r1,r2]
      | ADDO      {cond, r1, r2, t} => [r1,r2]
      | ADDI      {cond, i, r, t} => [r]
      | ADDIO     {cond, i, r, t} => [r]
      | ADDIL     {i, r} => [r]
      | ADDIL'    {pr_i, r} => [r]
      | AND       {cond, r1, r2, t} => [r1,r2]
      | ANDCM     {cond, r1, r2, t} => [r1,r2]

      | B         {n, target} => []
      | BB        {n, cond, r, p, target} => [r]
      | BL        {n, target, t} => []
      | BLE       {n, wd, sr, b} => [b]
      | BV        {n, x, b} => [b,x]

      | COMB      {cond, n, r1, r2, target} => [r1,r2]
      | COMCLR    {cond, r1, r2, t} => [r1,r2]
      | COPY      {r, t} => [r]

      | DEPI      {cond, i, p, len, t} => [t] (* both use and def *)

      | FABS      {fmt, r, t} => [r] 
      | FADD      {fmt, r1, r2, t} => [r1,r2] 
      | FCMP      {fmt, cond, r1, r2} => [r1,r2] 
      | FLDDS     {complt, d, s, b, t} => [b]
      | FMPY      {fmt, r1, r2, t} => [r1,r2] 
      | FSTDS     {complt, r, d, s, b} => [r,b]
      | FSUB      {fmt, r1, r2, t} => [r1,r2] 
      | FTEST      => [] (* FStatusReg *)
      | XMPYU     {r1, r2, t} => [r1,r2]

      | LDI       {i, t} => []
      | LDIL      {i, t} => []
      | LDO       {d, b, t} => [b]
      | LDO'      {pr_d, b, t} => [b]
      | LDW       {d, s, b, t} => [b]
      | LDWS      {cmplt, d, s, b, t} => [b]
      | LDWM      {d, s, b, t} => [b]

      | NOP       => die "regs_used - NOP" 

      | OR        {cond, r1, r2, t} => [r1,r2]
      | XOR        {cond, r1, r2, t} => [r1,r2]
      | SH1ADD    {cond, r1, r2, t} => [r1,r2]
      | SH2ADD    {cond, r1, r2, t} => [r1,r2]

      | SHD       {cond, r1, r2, p, t} => [r1,r2]
      | SUB       {cond, r1, r2, t} => [r1,r2]
      | SUBO      {cond, r1, r2, t} => [r1,r2]
      | SUBI      {cond, i, r, t} => [r]
      | STW       {r, d, s, b} => [b,r]
      | STWS      {cmplt, r, d, s, b} => [b,r]
      | STWM      {r, d, s, b} => [b,r]

      | ZVDEP {cond, r,d,t} => [r,Ctrl 11]
      | MTSAR {r} => [r]
      | VEXTRS {cond,r,d,t} => [r,Ctrl 11]
      | VSHD {cond, r1,r2,t} => [r1,r2,Ctrl 11]

      | LABEL lab => []
      | COMMENT s => []
      | NOT_IMPL s => []
      | DOT_ALIGN i => []
      | DOT_BLOCKZ i=> []
      | DOT_CALL s => []
      | DOT_CALLINFO s => []
      | DOT_CODE => []
      | DOT_DATA => []
      | DOT_DOUBLE s => []
      | DOT_END => []
      | DOT_ENTER => []
      | DOT_ENTRY => []
      | DOT_EQU i => []
      | DOT_EXPORT (lab, s) => []
      | DOT_IMPORT (lab, s) => []
      | DOT_LEAVE => []
      | DOT_EXIT => []
      | DOT_PROC => []
      | DOT_PROCEND => []
      | DOT_STRINGZ s => []
      | DOT_WORD w => []
      | DOT_BYTE b => []

      | META_IF {cond, r1, r2, target} => []
      | META_BL {n, target, rpLink, callStr} => []
      | META_BV {n, x, b} => []
      | META_IF_BIT {r, bitNo, target} => []
      | META_B {n, target} => []

      | SEQ _ => die "regs_used - SEQ" 

    fun does_inst_nullify(i) =
      case i of
        ADD       {cond, r1, r2, t} => cond<>NEVER
      | ADDO      {cond, r1, r2, t} => cond<>NEVER
      | ADDI      {cond, i, r, t}   => cond<>NEVER
      | ADDIO     {cond, i, r, t}   => cond<>NEVER
      | ADDIL     {i, r} => false     
      | ADDIL'    {pr_i, r} => false     
      | AND       {cond, r1, r2, t} => cond<>NEVER
      | ANDCM     {cond, r1, r2, t} => cond<>NEVER
	  
      | B         {n, target}             => true
      | BB        {n, cond, r, p, target} => true
      | BL        {n, target, t}          => true
      | BLE       {n, wd, sr, b}          => true
      | BV        {n, x, b}               => true
	  
      | COMB      {cond, n, r1, r2, target} => true
      | COMCLR    {cond, r1, r2, t}         => cond<>NEVER
      | COPY      {r, t}                    => false
	  
      | DEPI      {cond, i, p, len, t} => cond<>NEVER
	  
      | FABS      {fmt, r, t}          => true
      | FADD      {fmt, r1, r2, t}     => true
      | FCMP      {fmt, cond, r1, r2}  => true
      | FLDDS     {complt, d, s, b, t} => true
      | FMPY      {fmt, r1, r2, t}     => true
      | FSTDS     {complt, r, d, s, b} => true
      | FSUB      {fmt, r1, r2, t}     => true
      | FTEST                          => true
      | XMPYU     {r1, r2, t}          => true
	  
      | LDI       {i, t}              => false
      | LDIL      {i, t}              => false
      | LDO       {d, b, t}           => false
      | LDO'      {pr_d, b, t}           => false
      | LDW       {d, s, b, t}        => false
      | LDWS      {cmplt, d, s, b, t} => false
      | LDWM      {d, s, b, t}        => false

      | NOP       => false
	  
      | OR        {cond, r1, r2, t} => cond<>NEVER
      | XOR       {cond, r1, r2, t} => cond<>NEVER
      | SH1ADD    {cond, r1, r2, t} => cond<>NEVER
      | SH2ADD    {cond, r1, r2, t} => cond<>NEVER
	  
      | SHD       {cond, r1, r2, p, t} => cond<>NEVER
      | SUB       {cond, r1, r2, t}    => cond<>NEVER
      | SUBO      {cond, r1, r2, t}    => cond<>NEVER
      | SUBI      {cond, i, r, t}      => cond<>NEVER
      | STW       {r, d, s, b}         => false
      | STWS      {cmplt, r, d, s, b}  => false
      | STWM      {r, d, s, b}         => false
	  
      | ZVDEP {cond, r,d,t} => cond<>NEVER
      | MTSAR {r} => false
      | VEXTRS {cond,r,d,t} => cond<>NEVER
      | VSHD {cond, r1,r2,t} => cond<>NEVER

      | LABEL lab             => false
      | COMMENT s             => false
      | NOT_IMPL s            => die "DelaySlotOptimization - doesInstNullify - NOT_IMPL" 
      | DOT_ALIGN i           => false
      | DOT_BLOCKZ i          => false
      | DOT_CALL s            => false
      | DOT_CALLINFO s        => false
      | DOT_CODE              => false
      | DOT_DATA              => false
      | DOT_DOUBLE s          => false
      | DOT_END               => false
      | DOT_ENTER             => false
      | DOT_ENTRY             => false
      | DOT_EQU i             => false
      | DOT_EXPORT (seg, sym) => false
      | DOT_IMPORT (sym, seg) => false
      | DOT_LEAVE             => false
      | DOT_EXIT              => false
      | DOT_PROC              => false
      | DOT_PROCEND           => false
      | DOT_STRINGZ s         => false
      | DOT_WORD w            => false
      | DOT_BYTE b            => false
	  
      | META_IF {cond, r1, r2, target}       => die "DelaySlotOptimization - doesInstNullify - META_IF" 
      | META_BL {n, target, rpLink, callStr} => die "DelaySlotOptimization - doesInstNullify - META_BL" 
      | META_BV {n, x, b}                    => die "DelaySlotOptimization - doesInstNullify - META_BV"
      | META_IF_BIT {r, bitNo, target}       => die "DelaySlotOptimization - doesInstNullify - META_IF_BIT" 
      | META_B {n, target}                   => die "DelaySlotOptimization - doesInstNullify - META_B"
	  
      | SEQ _ => die "DelaySlotOptimization - doesFirstInstNullify - SEQ"

    fun is_jmp i = 
      case i of
	ADD      _ => false
      | ADDO     _ => false
      | ADDI     _ => false
      | ADDIO    _ => false
      | ADDIL    _ => false
      | ADDIL'   _ => false
      | AND      _ => false
      | ANDCM    _ => false
		 
      | B        _ => true
      | BB       _ => true
      | BL       _ => true
      | BLE      _ => true
      | BV       _ => true
		 
      | COMB     _ => true
      | COMCLR   _ => false
      | COPY     _ => false
		 
      | DEPI     _ => false
		 
      | FABS     _ => false
      | FADD     _ => false
      | FCMP     _ => false
      | FLDDS    _ => false
      | FMPY     _ => false
      | FSTDS    _ => false
      | FSUB     _ => false
      | FTEST      => false
      | XMPYU    _ => false
		 
      | LDI      _ => false
      | LDIL     _ => false
      | LDO      _ => false
      | LDO'     _ => false
      | LDW      _ => false
      | LDWS     _ => false
      | LDWM     _ => false
		 
      | NOP        => false
		 
      | OR       _ => false
      | XOR      _ => false
      | SH1ADD   _ => false
      | SH2ADD   _ => false
		 
      | SHD      _ => false
      | SUB      _ => false
      | SUBO     _ => false
      | SUBI     _ => false
      | STW      _ => false
      | STWS     _ => false
      | STWM     _ => false

      | ZVDEP _ => false
      | MTSAR _ => false
      | VEXTRS _ => false
      | VSHD _ => false

      | LABEL            _ => false
      | COMMENT          _ => false
      | NOT_IMPL         _ => false
      | DOT_ALIGN        _ => false
      | DOT_BLOCKZ       _ => false
      | DOT_CALL         _ => false
      | DOT_CALLINFO     _ => false
      | DOT_CODE           => false
      | DOT_DATA           => false
      | DOT_DOUBLE       _ => false
      | DOT_END            => false
      | DOT_ENTER          => false
      | DOT_ENTRY          => false
      | DOT_EQU          _ => false
      | DOT_EXPORT       _ => false
      | DOT_IMPORT       _ => false
      | DOT_LEAVE          => false
      | DOT_EXIT           => false
      | DOT_PROC           => false
      | DOT_PROCEND        => false
      | DOT_STRINGZ      _ => false
      | DOT_WORD         _ => false
      | DOT_BYTE         _ => false

      | META_IF          _ => true
      | META_BL          _ => true
      | META_BV          _ => true
      | META_IF_BIT      _ => true
      | META_B           _ => true

      | SEQ              _ => false

    fun is_asm_directive i = 
      case i of
	ADD      _ => false
      | ADDO     _ => false
      | ADDI     _ => false
      | ADDIO    _ => false
      | ADDIL    _ => false
      | ADDIL'   _ => false
      | AND      _ => false
      | ANDCM    _ => false
		 
      | B        _ => false
      | BB       _ => false
      | BL       _ => false
      | BLE      _ => false
      | BV       _ => false
		 
      | COMB     _ => false
      | COMCLR   _ => false
      | COPY     _ => false
		 
      | DEPI     _ => false
		 
      | FABS     _ => false
      | FADD     _ => false
      | FCMP     _ => false
      | FLDDS    _ => false
      | FMPY     _ => false
      | FSTDS    _ => false
      | FSUB     _ => false
      | FTEST      => false
      | XMPYU    _ => false
		 
      | LDI      _ => false
      | LDIL     _ => false
      | LDO      _ => false
      | LDO'     _ => false
      | LDW      _ => false
      | LDWS     _ => false
      | LDWM     _ => false
		 
      | NOP        => false
		 
      | OR       _ => false
      | XOR      _ => false
      | SH1ADD   _ => false
      | SH2ADD   _ => false
		 
      | SHD      _ => false
      | SUB      _ => false
      | SUBO     _ => false
      | SUBI     _ => false
      | STW      _ => false
      | STWS     _ => false
      | STWM     _ => false

      | ZVDEP _ => false
      | MTSAR _ => false
      | VEXTRS _ => false
      | VSHD _ => false

      | LABEL            _ => true
      | COMMENT          _ => true
      | NOT_IMPL         _ => true
      | DOT_ALIGN        _ => true
      | DOT_BLOCKZ       _ => true
      | DOT_CALL         _ => true
      | DOT_CALLINFO     _ => true
      | DOT_CODE           => true
      | DOT_DATA           => true
      | DOT_DOUBLE       _ => true
      | DOT_END            => true
      | DOT_ENTER          => true
      | DOT_ENTRY          => true
      | DOT_EQU          _ => true
      | DOT_EXPORT       _ => true
      | DOT_IMPORT       _ => true
      | DOT_LEAVE          => true
      | DOT_EXIT           => true
      | DOT_PROC           => true
      | DOT_PROCEND        => true
      | DOT_STRINGZ      _ => true
      | DOT_WORD         _ => true
      | DOT_BYTE         _ => true

      | META_IF          _ => die "Not possible at assembler level."
      | META_BL          _ => die "Not possible at assembler level."
      | META_BV          _ => die "Not possible at assembler level."
      | META_IF_BIT      _ => die "Not possible at assembler level."
      | META_B           _ => die "Not possible at assembler level."

      | SEQ              _ => die "Not possible at assembler level."

  end


