functor JumpTables(structure BI : BACKEND_INFO) : JUMP_TABLES =
  struct

    fun linear_search(sels: ('sel * 'sinst list) list,
		      default: 'sinst list,
		      comment: string * 'inst list -> 'inst list,
		      new_label: string -> 'label,
		      compile_sel: 'sel * 'inst list -> 'inst list,
		      if_no_match_go_lab: 'label * 'inst list -> 'inst list,
		      compile_insts: 'sinst list * 'inst list -> 'inst list,
		      label: 'label * 'inst list -> 'inst list,
		      jmp: 'label * 'inst list -> 'inst list,
		      C: 'inst list) =
      let
	val lab_exit = new_label "exitSwitchLab"
      in
	comment("begin linear switch",
		foldr (fn ((sel,insts),C) =>
		       let
			 val new_lab = new_label "newSelLab"
		       in
			 compile_sel(sel,
				     if_no_match_go_lab(new_lab,
							compile_insts(insts,
								      jmp(lab_exit,
									  label(new_lab,C)))))
		       end) (compile_insts(default,
					   label(lab_exit,
						 comment("end linear switch",C)))) sels)
      end
  end

(*
    fun code_for_switch(sel:('const*'sinst list) list,
			default: 'sinst list,
			new_label : string -> 'label,
			compile_const: 'const * 'inst list -> 'inst list,
			if_not_equal_go_lab: 'label * 'inst list -> 'inst list,
			compile_insts: 'sinst list * 'inst list -> 'inst list,
			label: 'label * 'inst list -> 'inst list,
			comment: string * 'inst list -> 'inst list,
			sel_dist: 'const * 'const -> int,
			jump_table_header: 'label * 'const * 'inst list -> 'inst list,
			eq_lab : 'label * 'label -> bool,
			C: 'inst list) =
      let

	fun bin_search(sels:(int * 'sinst list) list,
		       default: 'sinst list,
		       C: 'inst list) =
	  let
	    (*-----------------------------------------------------------------*)
	    (*  Compilation functions for the binary switch with jump tables.  *)
	    (* Switch([(const,is),...,(const,is)],default)                     *)
	    (*-----------------------------------------------------------------*)
	      
	    fun add_group(startSel,finishSel,accGrp) acc =
	      let
		val lenAccGrp = length accGrp
		fun add_group' [] acc = acc
		  | add_group' ((sel,selCode)::rest) acc =
		  add_group' rest ((sel,sel,[(sel,selCode)])::acc)
	      in
		if lenAccGrp > 1 andalso lenAccGrp < BI.minJumpTabSize then
		  add_group' (rev accGrp) acc
		else
		  (startSel, finishSel, accGrp)::acc
	      end

	    (* Group the selections into groups where jumptables are resonable. *)
	    (* acc : int * int * ((int * code) list) list                       *)
	    fun group_sel_list(NONE, NONE, [], [], acc) = acc
	      | group_sel_list(SOME startSel, SOME finishSel, [], accGrp, acc) =
	      add_group (startSel,finishSel,accGrp) acc
	      | group_sel_list(NONE, NONE, (sel, selCode)::rest, [], acc) =
	      group_sel_list(SOME sel, SOME sel, rest, [(sel,selCode)], acc)
	      | group_sel_list(SOME startSel, SOME finishSel, (sel,selCode)::rest, accGrp, acc) =
	      if sel_dist(startSel,sel) <= BI.maxDiff then
		group_sel_list(SOME sel, SOME finishSel, rest, (sel,selCode)::accGrp, acc)
	      else
		group_sel_list(SOME sel, SOME sel, rest, [(sel,selCode)],
			       add_group (startSel,finishSel,accGrp) acc)
	      | groupSelList _ = die "JumpTables.group_sel_list"
		
	    (* The lists returned may not be reversed! *)
	    fun split_list [] = (NONE, NONE, NONE)
	      | split_list xs =
	      let
		val lenxs = length xs
		fun split_list' n xs1 (x::rest) =
		  if n = 0 then
		    let
		      val xsLeft = case xs1
				     of [] => NONE
				      | _ => SOME xs1
		      val xsRight = case rest 
				      of [] => NONE
				       | _ => SOME rest
		    in
		      (xsLeft, SOME x, xsRight)
		    end
		  else
		    split_list' (n-1) (xs1@[x]) rest
		  | split_list' n xs1 [] = die "JumpTables:split_list"
	      in
		split_list' (lenxs div 2) [] xs
	      end
	      
	    fun switch_with_jump_table([],defaultLab,C) = die "JumpTables: switch_with_jump_table has no selections."
	      | switch_with_jump_table([(sel, selCode)],defaultLab,C) = compile_insts(selCode,C)
	      | switch_with_jump_table(sels as (sel,selCode)::rest, defaultLab,C) =
	      let
		val jumpTableLab = new_label "jumpTabLab"
		fun make_sel_code(next,[],C) = ([],C)
		  | make_sel_code(next,(sel,selCode)::rest,C) =
		  if next = sel then
		    let
		      val (jumpTable,C') = make_sel_code(next+1,rest,C)
		      val selLabel = new_label "selLab"
		      val C'' = label(selLabel,
				      compile_insts(selCode,C'))
		    in
		      (add_label_to_jump_tab(selLabel,jumpTable),C'')
		    end
		  else
		    let
		      val (jumpTable,C') = make_sel_code(next+1,(sel,selCode)::rest,C)
		    in
		      (add_label_to_jump_tab(defaultLab,jumpTable),C')
		    end
		fun merge([],C) = C
		  | merge(x::xs,C) = x::merge(xs,C)
		  
		val (jumpTableCode, switchCode) = make_sel_code(sel,sels,C)
		val jumpTableCode' = label(jumpTableLab,merge(jumpTableCode,switchCode)) 
	      in
		if length sels < BI.minJumpTabSize then
		  die "JumpTables:switch_with_jump_table has too few table entries"
		else
		  jump_table_header(jumpTableLab,sel,jumpTableCode')
	      end
		  
	    fun bin_search_code (leftLab,start,finish,rightLab,C) =
	      if start = finish then
		if eq_lab(leftLab,rightLab) then
		  if_sel_eq_opr_go_lab(start,leftLab,C)
		else
		  bin_compare1(start,leftLab,rightLab,C)
	      else
		bin_compare2(start,finish,leftLab,rightLab,C)

	    fun genBinarySwitch ((SOME leftSels,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
		val rightSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,rightSideLab,
				switch_with_jump_table(sels,defaultLab,
						       label(leftSideLab,
							     genBinarySwitch(split_list leftSels,defaultLab,
									     label(rightSideLab,
										   genBinarySwitch(split_list rightSels,defaultLab,C))))))
	      end
	      | genBinarySwitch ((SOME leftSels,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,defaultLab,
				switch_with_jump_table(sels,defaultLab,
						       label(leftSideLab,
							     genBinarySwitch(split_list leftSels,defaultLab,C))))
	      end
	      | genBinarySwitch ((NONE,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val rightSideLab = new_label ""
	      in
		binSearchCode(defaultLab,startSel,finishSel,rightSideLab,
			      switch_with_jump_table(sels,defaultLab,
						     label(rightSideLab,
							   genBinarySwitch(split_list rightSels,opr,defaultLab,C))))
	      end
	      | gen_binary_switch ((NONE,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      bin_search_code(defaultLab,startSel,finishSel,defaultLab,
			      switch_with_jump_table(sels,defaultLab,C))
	      | genBinarySwitch ((NONE, NONE, NONE),defaultLab,C) = die "JumpTables. genBinarySwitch, no selections."
	      | genBinarySwitch _ = die "JumpTables.genBinarySwitch"
		    
	    val selSorted = ListSort.sort 
	      (fn (i1, is1) => 
	       (fn (i2, is2) => i1 > i2)) sel
		    
	    val groupSels = groupSelList (NONE, NONE, selSorted, [], [])
	    val defaultLab = new_label "defaultLab"
	  in
	    comment("Begin binary switch with jump tables",
		    genBinarySwitch(split_list groupSels,defaultLab,
				    label(defaultLab,
					  compile_insts(default,
							comment("End binary switch with jump tables",C)))))
	  end
      in
	if (length sel) < BI.minCodeInBinSearch then
	  linearSearch()
	else
	  binSearch()
      end
*)