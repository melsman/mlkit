(* Specification of the Kit Abstract Machine. *)

signature KAM =
  sig
    
    type label
    val eq_lab : label * label -> bool
 
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

      | SetBit30              (* for unboxed data constructors *)
      | SetBit31              (* .. *)
      | ClearBit30And31
      | UbTagCon
	
      | SelectStack of int * string      (* string for debug only *)
      | SelectEnv of int * string        (* string for debug only *)
      | Select of int
      | Store of int

      | StackAddrInfBit of int * string  (* string for debug only *)
      | StackAddr of int * string        (* string for debug only *)
      | EnvToAcc

      |	ImmedInt of Int32.int
      | ImmedString of string
      | ImmedReal of string
	
      | Push 
      | PushLbl of label
      | Pop of int
	
      | ApplyFnCall of int
      | ApplyFnJmp of int * int
      | ApplyFunCall of label * int
      | ApplyFunJmp of label * int * int
      | Return of int * int

      | Ccall of int * int

      | Label of label
      | JmpRel of label

      | IfNotEqJmpRelImmed of label * Int32.int
      | IfLessThanJmpRelImmed of label * Int32.int
      | IfGreaterThanJmpRelImmed of label * Int32.int
      | DotLabel of label
      | JmpVector of label * Int32.int * Int32.int
                             (*start*)   (*length*)
      | Raise
      | PushExnPtr
      | PopExnPtr

      | LetregionFin of int
      | LetregionInf
      | EndregionInf
      | ResetRegion
      | MaybeResetRegion
      | ResetRegionIfInf

      | FetchData of label
      | StoreData of label

      | Comment of string
      | Nop

      (* The following instructions are purely for optimization *)

      | StackOffset of int
      | PopPush of int
      | ImmedIntPush of Int32.int
      | SelectPush of int
      | SelectEnvPush of int
      | SelectEnvClearAtbotBitPush of int
      | StackAddrPush of int * string (* string is for debugging *) 
      | StackAddrInfBitAtbotBitPush of int
      | SelectStackPush of int 
      | EnvPush

      | PrimEquali
      | PrimSubi1
      | PrimSubi2
      | PrimSubi
      | PrimAddi1
      | PrimAddi2
      | PrimAddi
      | PrimMuli
      | PrimNegi
      | PrimAbsi

      | PrimAddf
      | PrimSubf
      | PrimMulf
      | PrimDivf
      | PrimNegf
      | PrimAbsf

      | PrimLessThanFloat
      | PrimLessEqualFloat
      | PrimGreaterThanFloat
      | PrimGreaterEqualFloat

      | PrimLessThan
      | PrimLessEqual
      | PrimGreaterThan
      | PrimGreaterEqual

      | PrimLessThanUnsigned
      | PrimGreaterThanUnsigned
      | PrimLessEqualUnsigned
      | PrimGreaterEqualUnsigned
	
      | PrimAndw
      | PrimOrw
      | PrimXorw
      | PrimShiftLeftw
      | PrimShiftRightSignedw
      | PrimShiftRightUnsignedw	
      | PrimAddw
      | PrimSubw
      | PrimMulw

      | PrimSubi31
      | PrimAddi31
      | PrimMuli31
      | PrimNegi31
      | PrimAbsi31
      | PrimXorw31
      | PrimShiftLeftw31
      | PrimShiftRightSignedw31
      | PrimShiftRightUnsignedw31	
      | PrimAddw31
      | PrimSubw31
      | PrimMulw31

      | Primi31Toi	
      | PrimiToi31
      | Primw31Tow
      | PrimwTow31
      | Primw31TowX
      | PrimwToi

      | PrimFreshExname

      | PrimByteTableSub
      | PrimByteTableUpdate
      | PrimWordTableSub
      | PrimWordTableUpdate
      | PrimTableSize

    datatype TopDecl =
        FUN of label * KamInst list
      | FN of label * KamInst list

    type AsmPrg = {top_decls: TopDecl list,
		   main_lab_opt: label option,
		   imports_code: label list,     (* code imports *)
		   imports_data: label list,     (* data imports *)
		   exports_code: label list,     (* code exports *)
		   exports_data: label list}     (* data exports *)

    (******************)
    (* PrettyPrinting *)
    (******************)
    type StringTree
    val layout_AsmPrg : AsmPrg -> StringTree

    (* To Emit Code *)
    val pr_inst        : KamInst -> string
    val pp_lab         : label -> string
  end

