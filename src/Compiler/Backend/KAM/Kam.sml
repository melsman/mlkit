(* Specification of the Kit Abstract Machine (Byte code machine). *)

functor Kam(structure Labels : ADDRESS_LABELS
	    structure PP : PRETTYPRINT
	    structure Crash : CRASH):KAM =
  struct

    (***********)
    (* Logging *)
    (***********)
    fun die s  = Crash.impossible ("KAM." ^ s)

(*    fun is_im5  n = n <    16 andalso n >=    ~16
    fun is_im11 n = n <  1024 andalso n >=  ~1024
    fun is_im12 n = n <  2048 andalso n >=  ~2048
    fun is_im14 n = n <  8192 andalso n >=  ~8192
    fun is_im17 n = n < 65536 andalso n >= ~65536
    fun is_im19 n = n < 262144 andalso n >= ~262144
 18/09-2000, Niels *)

    (*----------------------------------------------------------*)
    (*                          Code                            *)
    (*----------------------------------------------------------*)

    type label = Labels.label
    fun eq_lab(l1,l2) = Labels.eq(l1,l2)

(*    datatype lab = 
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        code label, finish label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

    fun eq_lab (DatLab label1, DatLab label2) = Labels.eq(label1,label2)
      | eq_lab (LocalLab label1, LocalLab label2) = Labels.eq(label1,label2)
      | eq_lab (NameLab s1, NameLab s2) = s1 = s2
      | eq_lab (MLFunLab label1, MLFunLab label2) = Labels.eq(label1,label2)
      | eq_lab _ = false 18/09-2000, Niels *)

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
		  | EVEN
18/09-2000, Niels *)
(*    fun revCond NEVER = ALWAYS
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
18/09-2000, Niels *)

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
      | ReturnNoClos of int * int

      | Ccall of int * int

      | Label of label
      | JmpRel of label
      | IfNotEqJmpRel of label
      | IfLessThanJmpRel of label
      | IfGreaterThanJmpRel of label
      | DotLabel of label
      | JmpVector of label * int

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

      | PrimEquali
      | PrimSubi
      | PrimAddi
      | PrimNegi
      | PrimAbsi

      | PrimAddf
      | PrimSubf
      | PrimMulf
      | PrimNegf
      | PrimAbsf
      | PrimLessThan
      | PrimLessEqual
      | PrimGreaterThan
      | PrimGreaterEqual
	
      | PrimLessThanUnsigned
      | PrimGreaterThanUnsigned
      | PrimLessEqualUnsigned
      | PrimGreaterEqualUnsigned
	
      | PrimAddw8
      | PrimSubw8
	
      | PrimAndi
      | PrimOri
      | PrimXori
      | PrimShiftLefti
      | PrimShiftRightSignedi
      | PrimShiftRightUnsignedi
	
      | PrimAddw
      | PrimSubw
	
      | PrimFreshExname

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
(*      fun pp_lab (DatLab l) = remove_ctrl(Labels.pr_label l)
	| pp_lab (LocalLab l) = remove_ctrl(Labels.pr_label l) 
	| pp_lab (NameLab s) = s
	| pp_lab (MLFunLab l) = remove_ctrl(Labels.pr_label l) 18/09-2000, Niels *)
      fun pp_lab l = Labels.pr_label l

(*      fun pp_lab' (DatLab l,acc)   = remove_ctrl(Labels.pr_label l) :: acc
	| pp_lab' (LocalLab l,acc) = remove_ctrl(Labels.pr_label l) :: acc 
	| pp_lab' (NameLab s,acc)  = s :: acc
	| pp_lab' (MLFunLab l,acc) = remove_ctrl(Labels.pr_label l) :: acc 18/09-2000, Niels *)
	fun pp_lab' (l,acc) = Labels.pr_label l :: acc
    end

(*    fun pp_cond NEVER = ""
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
      | pp_cond EVEN = ",EV" 18/09-2000, Niels *)

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

      |	ImmedInt(i) => "ImmedInt(" :: (pp_i i) :: ")" :: acc
      | ImmedString(s) => "ImmedString(\"" :: String.toString s :: "\")" :: acc
      | ImmedReal(r) => "ImmedReal(" :: r :: ")" :: acc
	
      | Push => "Push" :: acc
      | PushLbl(lab) => "PushLbl(" :: (pp_lab lab) :: ")" :: acc
      | Pop(n) => if n = 1 then "Pop" :: acc else "Pop(" :: (pp_i n) :: ")" :: acc
	
      | ApplyFnCall(n) => "ApplyFnCall(" :: (pp_i n) :: ")" :: acc
      | ApplyFnJmp(n1,n2) => "ApplyFnJmp(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ApplyFunCall(lab,n) => "ApplyFunCall(" :: (pp_lab lab) :: "," :: (pp_i n) :: ")" :: acc
      | ApplyFunCallNoClos(lab,n) => "ApplyFunCallNoClos(" :: (pp_lab lab) :: "," :: (pp_i n) :: ")" :: acc
      | ApplyFunJmp(lab,n1,n2) => "ApplyFunJmp(" :: (pp_lab lab) :: "," :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ApplyFunJmpNoClos(lab,n1,n2) => "ApplyFunJmpNoClos(" :: (pp_lab lab) :: "," :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | Return(n1,n2) => "Return(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ReturnNoClos(n1,n2) => "ReturnNoClos(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc

      | Ccall(idx,arity) => "Ccall(" :: (pp_i idx) :: "," :: (pp_i arity) :: ")" :: acc

      | Label(lab) => "Label(" :: (pp_lab lab) :: ")" :: acc
      | JmpRel(lab) => "JmpRel(" :: (pp_lab lab) :: ")" :: acc
      | IfNotEqJmpRel(lab) => "IfNotEqJmpRel(" :: (pp_lab lab) :: ")" :: acc
      | IfLessThanJmpRel(lab) => "IfLessThanJmpRel(" :: (pp_lab lab) :: ")" :: acc
      | IfGreaterThanJmpRel(lab) => "IfGreaterThanJmpRel(" :: (pp_lab lab) :: ")" :: acc
      | DotLabel(lab) => "DotLabel(" :: (pp_lab lab) :: ")" :: acc
      | JmpVector(lab,first_sel) => "JmpVector(" :: (pp_lab lab) :: "," :: (pp_i first_sel) :: ")" :: acc

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

      | Comment(s) => "Comment[" :: s :: "]" :: acc
      | Nop => "Nop" :: acc

      | PrimEquali => "PrimEquali" :: acc
      | PrimSubi => "PrimSubi" :: acc
      | PrimAddi => "PrimAddi" :: acc
      | PrimNegi => "PrimNegi" :: acc                    
      | PrimAbsi => "PrimAbsi" :: acc

      | PrimAddf => "PrimAddf" :: acc
      | PrimSubf => "PrimSubf" :: acc                    
      | PrimMulf => "PrimMulf" :: acc
      | PrimNegf => "PrimNegf" :: acc
      | PrimAbsf => "PrimAbsf" :: acc                    

      | PrimLessThan => "PrimLessThan" :: acc
      | PrimLessEqual => "PrimLessEqual" :: acc
      | PrimGreaterThan => "PrimGreaterThan" :: acc
      | PrimGreaterEqual => "PrimGreaterEqual" :: acc
					                              
      | PrimLessThanUnsigned => "PrimLessThanUnsigned" :: acc
      | PrimGreaterThanUnsigned	=> "PrimGreaterThanUnsigned" :: acc
      | PrimLessEqualUnsigned => "PrimLessEqualUnsigned" :: acc
      | PrimGreaterEqualUnsigned => "PrimGreaterEqualUnsigned" :: acc
					                              
      | PrimAddw8 => "PrimAddw8" :: acc
      | PrimSubw8 => "PrimSubw8" :: acc
					                              
      | PrimAndi => "PrimAndi" :: acc
      | PrimOri => "PrimOri" :: acc
      | PrimXori => "PrimXori" :: acc
      | PrimShiftLefti => "PrimShiftLefti" :: acc
      | PrimShiftRightSignedi => "PrimShiftRightSignedi" :: acc
      | PrimShiftRightUnsignedi	=> "PrimShiftRightUnsignedi" :: acc
					                              
      | PrimAddw => "PrimAddw" :: acc
      | PrimSubw => "PrimSubw" :: acc
					                              
      | PrimFreshExname => "PrimFreshExname" :: acc

    fun pr_inst i = concat(pp_inst(i,[]))

(*
    fun output_AsmPrg (os, {top_decls, main_lab_opt, import_size, export_size, data_size}) =
      let
	fun fold ([], acc) = acc
	  | fold (inst::insts, acc) = "\n"::(pp_inst(inst, fold (insts, acc)))
	fun out_kam_insts insts = out_list (fold(insts, []))
	fun pp_top_decl(FUN(lab,insts)) = 
	  (TextIO.output(os,"\n;fun " ^ Labels.pr_label lab ^ " is {");
	   out_kam_insts insts;
	   TextIO.output(os,"\n;}\n"))
	  | pp_top_decl(FN(lab,insts)) =
	  (TextIO.output(os,"\n;fn " ^ Labels.pr_label lab ^ " is {");
	   out_kam_insts insts;
	   TextIO.output(os,"\n;}\n"))
      in
	(set_out_stream os;
	 TextIO.output(os, "\nHEADER is {\n  Main label option = " ^ 
		       (case main_lab_opt
			  of SOME lab => "SOME " ^ Labels.pr_label lab
			   | NONE => "NONE"));
	 TextIO.output(os, "\n  Import size = " ^ Int.toString import_size);
	 TextIO.output(os, "\n  Export size = " ^ Int.toString export_size);
	 TextIO.output(os, "\n  Data size = " ^ Int.toString data_size ^ "}");
	 List.app pp_top_decl top_decls;
	 TextIO.output(os,"\n\n");
	 reset_output_stream())
      end
*)

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


