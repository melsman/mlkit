(* Specification of the Kit Abstract Machine (Byte code machine). *)

structure Kam : KAM =
  struct
    structure Labels = AddressLabels
    structure PP = PrettyPrint

    (***********)
    (* Logging *)
    (***********)
    fun die s  = Crash.impossible ("KAM." ^ s)

    (*----------------------------------------------------------*)
    (*                          Code                            *)
    (*----------------------------------------------------------*)

    type label = Labels.label
    fun eq_lab(l1,l2) = Labels.eq(l1,l2)

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
      | UbTagCon
	
      | SelectStack of int * string       (* string for debug only *)
      | SelectEnv of int * string         (* string for debug only *)
      | Select of int
      | Store of int

      | StackAddrInfBit of int * string   (* string for debug only *)
      | StackAddr of int * string         (* string for debug only *)
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

      | Halt
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

      (* primitives *)

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

      | PrimIsNull


    datatype TopDecl =
        FUN of label * KamInst list
      | FN of label * KamInst list

    type AsmPrg = {top_decls: TopDecl list,
		   main_lab_opt: label option,
		   imports_code: label list,
		   imports_data: label list,
		   exports_code: label list,
		   exports_data: label list}

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

    local
      fun remove_ctrl s = "Lab" ^ String.implode (List.filter Char.isAlphaNum (String.explode s))
      fun remove_ctrl' s = String.implode (List.filter Char.isPrint (String.explode s))
    in
      fun pp_lab l = Labels.pr_label l
      fun pp_lab' (l,acc) = Labels.pr_label l :: acc
    end

    val indent = "\t"

    fun pp_inst (inst,acc) : string list =
      case inst of
        Alloc(n) => "Alloc(" :: (pp_i n) :: ")" :: acc
      | AllocIfInf(n)  => "AllocIfInf(" :: (pp_i n) :: ")" :: acc
      | AllocSatInf(n) => "AllocSatInf(" :: (pp_i n) :: ")" :: acc
      | AllocSatIfInf(n) => "AllocSatIfInf(" :: (pp_i n) :: ")" :: acc
      | AllocAtbot(n) => "AllocAtbot(" :: (pp_i n) :: ")" :: acc

      | BlockAlloc(n) => "BlockAlloc(" :: (pp_i n) :: ")" :: acc
      | BlockAllocIfInf(n)  => "BlockAllocIfInf(" :: (pp_i n) :: ")" :: acc
      | BlockAllocSatInf(n) => "BlockAllocSatInf(" :: (pp_i n) :: ")" :: acc
      | Block(n) => "Block(" :: (pp_i n) :: ")" :: acc
      | BlockAllocSatIfInf(n) => "BlockAllocSatIfInf(" :: (pp_i n) :: ")" :: acc
      | BlockAllocAtbot(n) => "BlockAllocAtbot(" :: (pp_i n) :: ")" :: acc

      | ClearAtbotBit => "ClearAtbotBit" :: acc
      | SetAtbotBit => "SetAtbotBit" :: acc

      | SetBit30 => "SetBit30" :: acc
      | SetBit31 => "SetBit31" :: acc
      | ClearBit30And31 => "ClearBit30And31" :: acc
      | UbTagCon => "UbTagCon" :: acc
	
      | SelectStack(off,s) => "SelectStack(" :: (pp_i off) :: "," :: s :: ")" :: acc
      | SelectEnv(off,s) => "SelectEnv(" :: (pp_i off) :: "," :: s :: ")" :: acc
      | Select(off) => "Select(" :: (pp_i off) :: ")" :: acc
      | Store(off) => "Store(" :: (pp_i off) :: ")" :: acc

      | StackAddrInfBit(off,s) => "StackAddrInfBit(" :: (pp_i off) :: "," :: s :: ")" :: acc
      | StackAddr(off,s) => "StackAddr(" :: (pp_i off) :: "," :: s :: ")" :: acc
      | EnvToAcc => "EnvToAcc" :: acc

      |	ImmedInt(i) => "ImmedInt(" :: Int32.toString i :: ")" :: acc
      | ImmedString(s) => "ImmedString(\"" :: String.toString s :: "\")" :: acc
      | ImmedReal(r) => "ImmedReal(" :: r :: ")" :: acc
	
      | Push => "Push" :: acc
      | PushLbl(lab) => "PushLbl(" :: (pp_lab lab) :: ")" :: acc
      | Pop(n) => "Pop(" :: (pp_i n) :: ")" :: acc
	
      | ApplyFnCall(n) => "ApplyFnCall(" :: (pp_i n) :: ")" :: acc
      | ApplyFnJmp(n1,n2) => "ApplyFnJmp(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ApplyFunCall(lab,n) => "ApplyFunCall(" :: (pp_lab lab) :: "," :: (pp_i n) :: ")" :: acc
      | ApplyFunJmp(lab,n1,n2) => "ApplyFunJmp(" :: (pp_lab lab) :: "," :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | Return(n1,n2) => "Return(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc

      | Ccall(idx,arity) => "Ccall(" :: (pp_i idx) :: "," :: (pp_i arity) :: ")" :: acc

      | Label(lab) => "Label(" :: (pp_lab lab) :: ")" :: acc
      | JmpRel(lab) => "JmpRel(" :: (pp_lab lab) :: ")" :: acc

      | IfNotEqJmpRelImmed(lab,i) => "IfNotEqJmpRelImmed(" :: (pp_lab lab) :: "," :: Int32.toString i :: ")" :: acc
      | IfLessThanJmpRelImmed(lab,i) => "IfLessThanJmpRelImmed(" :: (pp_lab lab) :: "," :: Int32.toString i ::")" :: acc
      | IfGreaterThanJmpRelImmed(lab,i) => "IfGreaterThanJmpRelImmed(" :: (pp_lab lab) :: "," :: Int32.toString i :: ")" :: acc
      | DotLabel(lab) => "DotLabel(" :: (pp_lab lab) :: ")" :: acc
      | JmpVector(lab,first_sel,length) => "JmpVector(" :: (pp_lab lab) :: "," :: (Int32.toString first_sel) :: "," :: (Int32.toString length) :: ")" :: acc

      | Raise => "Raise" :: acc
      | PushExnPtr => "PushExnPtr" :: acc
      | PopExnPtr => "PopExnPtr" :: acc
	  
      | LetregionFin(n) => "LetregionFin(" :: (pp_i n) :: ")" :: acc
      | LetregionInf => "LetregionInf" :: acc
      | EndregionInf => "EndregionInf" :: acc
      | ResetRegion => "ResetRegion" :: acc
      | MaybeResetRegion => "MaybeResetRegion" :: acc
      | ResetRegionIfInf => "ResetRegionIfInf" :: acc

      | FetchData(lab) => "FetchData(" :: (pp_lab lab) :: ")" :: acc
      | StoreData(lab) => "StoreData(" :: (pp_lab lab) :: ")" :: acc

      | Halt => "Halt" :: acc
      | Comment(s) => "Comment[" :: s :: "]" :: acc
      | Nop => "Nop" :: acc

      (* The following instructions are purely for optimization *)

      | StackOffset i => "StackOffset(" :: Int.toString i :: ")" :: acc
      | PopPush i => "PopPush(" :: Int.toString i :: ")" :: acc
      | ImmedIntPush i => "ImmedIntPush(" :: Int32.toString i :: ")" :: acc
      | SelectPush i => "SelectPush(" :: Int.toString i :: ")" :: acc
      | SelectEnvPush i => "SelectEnvPush(" :: Int.toString i :: ")" :: acc
      | SelectEnvClearAtbotBitPush i => "SelectEnvClearAtbotBitPush(" :: Int.toString i :: ")" :: acc
      | StackAddrPush (i,s) => "StackAddrPush(" :: Int.toString i :: "," :: s :: ")" :: acc (* string is for debugging *) 
      | StackAddrInfBitAtbotBitPush i => "StackAddrInfBitAtbotBitPush(" :: Int.toString i :: ")" :: acc
      | SelectStackPush i => "SelectStackPush(" :: Int.toString i :: ")" :: acc
      | EnvPush => "EnvPush" :: acc

      (* primitives *)

      | PrimEquali => "PrimEquali" :: acc
      | PrimSubi1 => "PrimSubi1" :: acc
      | PrimSubi2 => "PrimSubi2" :: acc
      | PrimSubi => "PrimSubi" :: acc
      | PrimAddi1 => "PrimAddi1" :: acc
      | PrimAddi2 => "PrimAddi2" :: acc
      | PrimAddi => "PrimAddi" :: acc
      | PrimMuli => "PrimMuli" :: acc
      | PrimNegi => "PrimNegi" :: acc                    
      | PrimAbsi => "PrimAbsi" :: acc

      | PrimAddf => "PrimAddf" :: acc
      | PrimSubf => "PrimSubf" :: acc                    
      | PrimMulf => "PrimMulf" :: acc
      | PrimDivf => "PrimDivf" :: acc
      | PrimNegf => "PrimNegf" :: acc
      | PrimAbsf => "PrimAbsf" :: acc                    

      | PrimLessThanFloat => "PrimLessThanFloat" :: acc
      | PrimLessEqualFloat => "PrimLessEqualFloat" :: acc
      | PrimGreaterThanFloat => "PrimGreaterThanFloat" :: acc
      | PrimGreaterEqualFloat => "PrimGreaterEqualFloat" :: acc

      | PrimLessThan => "PrimLessThan" :: acc
      | PrimLessEqual => "PrimLessEqual" :: acc
      | PrimGreaterThan => "PrimGreaterThan" :: acc
      | PrimGreaterEqual => "PrimGreaterEqual" :: acc
					                              
      | PrimLessThanUnsigned => "PrimLessThanUnsigned" :: acc
      | PrimGreaterThanUnsigned	=> "PrimGreaterThanUnsigned" :: acc
      | PrimLessEqualUnsigned => "PrimLessEqualUnsigned" :: acc
      | PrimGreaterEqualUnsigned => "PrimGreaterEqualUnsigned" :: acc
					                              
      | PrimAndw => "PrimAndw" :: acc
      | PrimOrw => "PrimOrw" :: acc
      | PrimXorw => "PrimXorw" :: acc
      | PrimShiftLeftw => "PrimShiftLeftw" :: acc
      | PrimShiftRightSignedw => "PrimShiftRightSignedw" :: acc
      | PrimShiftRightUnsignedw	=> "PrimShiftRightUnsignedw" :: acc
					                              
      | PrimAddw => "PrimAddw" :: acc
      | PrimSubw => "PrimSubw" :: acc
      | PrimMulw => "PrimMulw" :: acc

      | PrimSubi31 => "PrimSubi31" :: acc
      | PrimAddi31 => "PrimAddi31" :: acc
      | PrimMuli31 => "PrimMuli31" :: acc
      | PrimNegi31 => "PrimNegi31" :: acc
      | PrimAbsi31 => "PrimAbsi31" :: acc
      | PrimXorw31 => "PrimXorw31" :: acc
      | PrimShiftLeftw31 => "PrimShiftLeftw31" :: acc
      | PrimShiftRightSignedw31 => "PrimShiftRightSignedw31" :: acc
      | PrimShiftRightUnsignedw31 => "PrimShiftRightUnsignedw31" :: acc	
      | PrimAddw31 => "PrimAddw31" :: acc
      | PrimSubw31 => "PrimSubw31" :: acc
      | PrimMulw31 => "PrimMulw31" :: acc

      | Primi31Toi => "Primi31Toi" :: acc	
      | PrimiToi31 => "PrimiToi31" :: acc
      | Primw31Tow => "Primw31Tow" :: acc
      | PrimwTow31 => "PrimwTow31" :: acc
      | Primw31TowX => "Primw31TowX" :: acc
      | PrimwToi => "PrimwToi" :: acc
					                              
      | PrimFreshExname => "PrimFreshExname" :: acc

      | PrimByteTableSub => "PrimByteTableSub" :: acc
      | PrimByteTableUpdate => "PrimByteTableUpdate" :: acc
      | PrimWordTableSub => "PrimWordTableSub" :: acc
      | PrimWordTableUpdate => "PrimWordTableUpdate" :: acc
      | PrimTableSize => "PrimTableSize" :: acc
      | PrimIsNull => "PrimIsNull" :: acc

    fun pr_inst i = concat(pp_inst(i,[]))

    type StringTree = PP.StringTree
    fun layout_AsmPrg({top_decls,
		       main_lab_opt,
		       imports_code,
		       imports_data,
		       exports_code,
		       exports_data}) =
      let
	open PP
	fun layout_kam_inst i = LEAF(concat(pp_inst(i,[])))
	fun layout_top_decl(FUN(lab,kam_insts)) =
          NODE{start = "FUN " ^ Labels.pr_label lab ^ " is {",
	       finish = "}", 
	       indent = 2, 
	       childsep = RIGHT ";",
	       children = map layout_kam_inst kam_insts}
	  | layout_top_decl (FN(lab,kam_insts)) =
	  NODE{start = "FN " ^ Labels.pr_label lab ^ " is {",
	       finish = "}", 
	       indent = 2, 
	       childsep = RIGHT ";", 
	       children = map layout_kam_inst kam_insts}
	val body_node = NODE{start="",
			     finish="",
			     indent=0,
			     childsep=RIGHT " ",
			     children=map layout_top_decl top_decls}
	fun labels s labs = NODE{start=s ^ " = [",finish="]",indent=2,childsep=RIGHT",",
				 children=map (LEAF o Labels.pr_label) labs}
	val header_node = NODE {start="HEADER is {",
				finish="}", childsep=RIGHT "; ", indent=2,
				children=[LEAF ("Main label option = " ^ 
						(case main_lab_opt
						   of SOME lab => "SOME " ^ Labels.pr_label lab
						    | NONE => "NONE")),
					  labels "Imports code" imports_code,
					  labels "Imports data" imports_data,
					  labels "Exports code" exports_code,
					  labels "Exports data" exports_data]}
      in
	NODE{start="KAM program begin",
	     finish="KAM program end",
	     indent=2,
	     childsep=NOSEP,
	     children = [header_node,body_node]}
      end
  end


