(* Specification of HPPA Risc code. *)

signature KAM =
  sig
    
    (*----------------------------------------------------------*)
    (*                  Register definitions.                   *)
    (*----------------------------------------------------------*)

    (*----------------------------------------------------------*)
    (*                     HPPA RISC Syntax                     *)
    (*                                                          *)
    (* We do not specify cache hints in instructions...         *)
    (*                                                          *)
    (*----------------------------------------------------------*)

(*    val is_im5  : int -> bool
    val is_im11 : int -> bool
    val is_im12 : int -> bool
    val is_im14 : int -> bool
    val is_im17 : int -> bool
    val is_im19 : int -> bool 18/09-2000, Niels *)

    type label
(*    datatype lab = 
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        code label, finish label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

    val eq_lab : lab * lab -> bool
18/09-2000, Niels*)
(*    datatype cond = NEVER
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
                  | EVEN 18/09-2000, Niels *)

(*    val revCond : cond -> cond 18/09-2000, Niels *)
 
    datatype KamInst = 
        Alloc of int
      | AllocIfInf of int
      | AllocSatInf of int
      | AllocSatIfInf of int
      | AllocAtbot of int
      
      | BlockAlloc of int
      | BlockAllocIfInf of int
      | BlockAllocSatInf of int
      | Block of int
      | BlockAllocSatIfInf of int
      | BlockAllocAtbot of int
      | ClearAtbotBit
      | SetAtbotBit

      | SetBit30
      | SetBit31
      | ClearBit30And31
	
      | SelectStack of int
      | SelectEnv of int
      | Select of int
      | Store of int

      | StackAddrInfBit of int
      | StackAddr of int
      | EnvToAcc

      |	ImmedInt of int
      | ImmedString of string
      | ImmedReal of string
	
      | Push 
      | PushLbl of label
      | Pop of int
	
      | ApplyFnCall of int
      | ApplyFnJmp of int * int
      | ApplyFunCall of label * int
      | ApplyFunCallNoClos of label * int
      | ApplyFunJmp of label * int * int
      | ApplyFunJmpNoClos of label * int * int
      | Return of int * int

      | Label of label
      | Jmp of label

      | Raise
      | PushExnPtr
      | PopExnPtr

      | LetregionFin of int
      | LetregionInf
      | EndregionInf
      | ResetRegion
      | MaybeResetRegion
      | ResetRegionIfInf

      | FetchGlobal of label
      | StoreGlobal of label

      | Comment of string
      | Nop

    datatype TopDecl =
        FUN of label * KamInst list
      | FN of label * KamInst list

    type AsmPrg = {top_decls: TopDecl list,
		   init_code: KamInst list,
		   exit_code: KamInst list,
		   static_data: KamInst list}


    (******************)
    (* PrettyPrinting *)
    (******************)
    type StringTree
    val layout_AsmPrg : AsmPrg -> StringTree

    (* To Emit Code *)
    val pr_inst        : KamInst -> string
    val pp_lab         : label -> string
    val output_AsmPrg : TextIO.outstream * AsmPrg -> unit

  end

