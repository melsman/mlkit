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
        BlockAlloc of int
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

      | Comment of string
      | Nop

    datatype TopDecl =
        FUN of label * KamInst list
      | FN of label * KamInst list

    type AsmPrg = {top_decls: TopDecl list,
		   init_code: KamInst list,
		   exit_code: KamInst list,
		   static_data: KamInst list}

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
        BlockAlloc(n) => "BlockAlloc(" :: (pp_i n) :: ")" :: acc
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
	
      | SelectStack(off) => "SelectStack(" :: (pp_i off) :: ")" :: acc
      | SelectEnv(off) => "SelectEnv(" :: (pp_i off) :: ")" :: acc
      | Select(off) => "Select(" :: (pp_i off) :: ")" :: acc

      | StackAddrInfBit(off) => "StackAddrInfBit(" :: (pp_i off) :: ")" :: acc
      | StackAddr(off) => "StackAddr(" :: (pp_i off) :: ")" :: acc
      | EnvToAcc => "EnvToAcc" :: acc

      |	ImmedInt(i) => "ImmedInt(" :: (pp_i i) :: ")" :: acc
      | ImmedString(s) => "ImmedString(" :: s :: ")" :: acc
      | ImmedReal(r) => "ImmedReal(" :: r :: ")" :: acc
	
      | Push => "Push" :: acc
      | PushLbl(lab) => "PushLbl(" :: (pp_lab lab) :: ")" :: acc
      | Pop(n) => if n = 0 then "Pop" :: acc else "Pop(" :: (pp_i n) :: ")" :: acc
	
      | ApplyFnCall(n) => "ApplyFnCall(" :: (pp_i n) :: ")" :: acc
      | ApplyFnJmp(n1,n2) => "ApplyFnJmp(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ApplyFunCall(lab,n) => "ApplyFunCall(" :: (pp_lab lab) :: "," :: (pp_i n) :: ")" :: acc
      | ApplyFunCallNoClos(lab,n) => "ApplyFunCallNoClos(" :: (pp_lab lab) :: "," :: (pp_i n) :: ")" :: acc
      | ApplyFunJmp(lab,n1,n2) => "ApplyFunJmp(" :: (pp_lab lab) :: "," :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | ApplyFunJmpNoClos(lab,n1,n2) => "ApplyFunJmpNoClos(" :: (pp_lab lab) :: "," :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc
      | Return(n1,n2) => "Return(" :: (pp_i n1) :: "," :: (pp_i n2) :: ")" :: acc

      | Label(lab) => "Label(" :: (pp_lab lab) :: ")" :: acc
      | Jmp(lab) => "Jmp(" :: (pp_lab lab) :: ")" :: acc

      | Raise => "Raise" :: acc
      | PushExnPtr => "PushExnPtr" :: acc
      | PopExnPtr => "PopExnPtr" :: acc
	  
      | LetregionFin(n) => "LetregionFin(" :: (pp_i n) :: ")" :: acc
      | LetregionInf => "LetregionInf" :: acc
      | EndregionInf => "EndregionInf" :: acc
      | ResetRegion => "ResetRegion" :: acc
      | MaybeResetRegion => "MaybeResetRegion" :: acc

      | Comment(s) => "Comment[" :: s :: "]" :: acc
      | Nop => "Nop" :: acc

    fun pr_inst i = concat(pp_inst(i,[]))

    fun output_AsmPrg (os,{top_decls,init_code,exit_code,static_data}) =
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
	 out_kam_insts init_code;
	 List.app pp_top_decl top_decls;
	 out_kam_insts exit_code;
	 out_kam_insts static_data;
	 TextIO.output(os,"\n\n");
	 reset_output_stream())
      end

    type StringTree = PP.StringTree
    fun layout_AsmPrg({top_decls,init_code,exit_code,static_data}) =
      let
	open PP
	fun layout_kam_inst i = LEAF(concat(pp_inst(i,[])))
	val init_node = NODE{start="Begin InitCode",
			     finish="End InitCode",
			     indent=2,
			     childsep=RIGHT " ",
			     children = map layout_kam_inst init_code}
	val exit_node = NODE{start="Begin ExitCode",
			     finish="End ExitCode",
			     indent=2,
			     childsep=RIGHT " ",
			     children=map layout_kam_inst exit_code}
	val static_data_node = NODE{start="Begin Static Data",
				    finish="End Static Data",
				    indent=2,
				    childsep=RIGHT " ",
				    children=map layout_kam_inst static_data}
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
      in
	NODE{start="KAM program begin",
	     finish="KAM program end",
	     indent=2,
	     childsep=NOSEP,
	     children = [init_node,body_node,exit_node,static_data_node]}
      end
  end


