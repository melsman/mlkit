functor JumpTables(structure BI : BACKEND_INFO
		   structure Crash : CRASH) : JUMP_TABLES =
  struct

    (***********)
    (* Logging *)
    (***********)
    fun die s  = Crash.impossible ("JumpTables." ^ s)
(*
    fun linear_search(sels: ('sel * 'sinst) list,
		      default: 'sinst,
		      comment: string * 'inst list -> 'inst list,
		      new_label: string -> 'label,
		      compile_sel: 'sel * 'inst list -> 'inst list,
		      if_no_match_go_lab: 'label * 'inst list -> 'inst list,
		      compile_insts: 'sinst * 'inst list -> 'inst list,
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
*)
    fun linear_search_new(sels: ('sel * 'sinst) list,
			  default: 'sinst,
			  comment: string * 'inst list -> 'inst list,
			  new_label: string -> 'label,
			  if_no_match_go_lab_sel: 'label * 'sel * 'inst list -> 'inst list,
			  compile_insts: 'sinst * 'inst list -> 'inst list,
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
			 if_no_match_go_lab_sel(new_lab, sel,
						compile_insts(insts,
							      jmp(lab_exit,
								  label(new_lab,C))))
		       end) (compile_insts(default,
					   label(lab_exit,
						 comment("end linear switch",C)))) sels)
      end

    fun linear_search(sels: ('sel * 'sinst) list,
		      default: 'sinst,
		      comment: string * 'inst list -> 'inst list,
		      new_label: string -> 'label,
		      compile_sel: 'sel * 'inst list -> 'inst list,
		      if_no_match_go_lab: 'label * 'inst list -> 'inst list,
		      compile_insts: 'sinst * 'inst list -> 'inst list,
		      label: 'label * 'inst list -> 'inst list,
		      jmp: 'label * 'inst list -> 'inst list,
		      C: 'inst list) =
      linear_search_new(sels,
			default,
			comment,
			new_label,
			fn (l:'label,s:'sel,il:'inst list) => compile_sel(s, if_no_match_go_lab(l,il)),
			compile_insts,
			label,
			jmp,
			C)
(*
    fun binary_search(sels:(int*'sinst) list,
		      default: 'sinst,
		      comment: string * 'inst list -> 'inst list,
		      new_label : string -> 'label,
		      compile_sel: int * 'inst list -> 'inst list,
		      if_not_equal_go_lab: 'label * 'inst list -> 'inst list,
		      if_less_than_go_lab: 'label * 'inst list -> 'inst list,
		      if_greater_than_go_lab: 'label * 'inst list -> 'inst list,
		      compile_insts: 'sinst * 'inst list -> 'inst list,
		      label: 'label * 'inst list -> 'inst list,
		      jmp: 'label * 'inst list -> 'inst list,
		      sel_dist: int * int -> int,
		      jump_table_header: 'label * int * 'inst list -> 'inst list,
		      add_label_to_jump_tab: 'label * 'inst list -> 'inst list,
		      eq_lab : 'label * 'label -> bool,
		      C: 'inst list) =
      let
	val lab_exit = new_label "exitSwitchLab"

	fun bin_search() =
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

	    (* Group the selections into groups where jump tables are resonable. *)
	    (* acc : int * int * ((int * 'inst list) list) list                  *)
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
	      | group_sel_list _ = die "JumpTables.group_sel_list"
		
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
	      | switch_with_jump_table([(sel, selCode)],defaultLab,C) = compile_insts(selCode,jmp(lab_exit,C))
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
				      compile_insts(selCode,jmp(lab_exit,C')))
		    in
		      (add_label_to_jump_tab(selLabel,jumpTable),C'')  (* DOT_WORD pp_lab selLabel :: jumpTable *)
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
		  compile_sel(start,
		  if_not_equal_go_lab(leftLab,C))
		else
		  compile_sel(start,
		  if_less_than_go_lab(leftLab,
                  if_greater_than_go_lab(rightLab,C)))
	      else
		compile_sel(start,
                if_less_than_go_lab(leftLab,
                compile_sel(finish,
                if_greater_than_go_lab(rightLab,C))))

	    fun gen_binary_switch ((SOME leftSels,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
		val rightSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,rightSideLab,
				switch_with_jump_table(sels,defaultLab,
				label(leftSideLab,
				gen_binary_switch(split_list leftSels,defaultLab,
				label(rightSideLab,
				gen_binary_switch(split_list rightSels,defaultLab,C))))))
	      end
	      | gen_binary_switch ((SOME leftSels,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,defaultLab,
				switch_with_jump_table(sels,defaultLab,
				label(leftSideLab,
				gen_binary_switch(split_list leftSels,defaultLab,C))))
	      end
	      | gen_binary_switch ((NONE,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val rightSideLab = new_label ""
	      in
		bin_search_code(defaultLab,startSel,finishSel,rightSideLab,
			      switch_with_jump_table(sels,defaultLab,
			      label(rightSideLab,
			      gen_binary_switch(split_list rightSels,defaultLab,C))))
	      end
	      | gen_binary_switch ((NONE,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      bin_search_code(defaultLab,startSel,finishSel,defaultLab,
			      switch_with_jump_table(sels,defaultLab,C))
	      | gen_binary_switch ((NONE, NONE, NONE),defaultLab,C) = die "JumpTables. genBinarySwitch, no selections."
	      | gen_binary_switch _ = die "JumpTables.genBinarySwitch"
		    
	    val sels_sorted = ListSort.sort 
	      (fn (i1, is1) => 
	       (fn (i2, is2) => i1 > i2)) sels
		    
	    val group_sels = group_sel_list (NONE, NONE, sels_sorted, [], [])
	    val default_lab = new_label "defaultLab"
	  in
	    comment("Begin binary switch with jump tables",
		    gen_binary_switch(split_list group_sels,default_lab,
				      label(default_lab,
					    compile_insts(default,
							  label(lab_exit,
							  comment("End binary switch with jump tables",C))))))
	  end
      in
	if (length sels) < BI.minCodeInBinSearch then
	  linear_search(sels,
			default,
			comment,
			new_label,
			compile_sel,
			if_not_equal_go_lab,
			compile_insts,
			label,
			jmp,
			C)
	else
	  bin_search()
      end
*)

    fun binary_search_new(sels:(int*'sinst) list,
			  default: 'sinst,
			  comment: string * 'inst list -> 'inst list,
			  new_label : string -> 'label,
			  if_not_equal_go_lab_sel: 'label * int * 'inst list -> 'inst list,
			  if_less_than_go_lab_sel: 'label * int * 'inst list -> 'inst list,
			  if_greater_than_go_lab_sel: 'label * int * 'inst list -> 'inst list,
			  compile_insts: 'sinst * 'inst list -> 'inst list,
			  label: 'label * 'inst list -> 'inst list,
			  jmp: 'label * 'inst list -> 'inst list,
			  sel_dist: int * int -> int,
			  jump_table_header: 'label * int * 'inst list -> 'inst list,
			  add_label_to_jump_tab: 'label * 'inst list -> 'inst list,
			  eq_lab : 'label * 'label -> bool,
			  C: 'inst list) =
      let
	val lab_exit = new_label "exitSwitchLab"

	fun bin_search() =
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

	    (* Group the selections into groups where jump tables are resonable. *)
	    (* acc : int * int * ((int * 'inst list) list) list                  *)
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
	      | group_sel_list _ = die "JumpTables.group_sel_list"
		
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
	      | switch_with_jump_table([(sel, selCode)],defaultLab,C) = compile_insts(selCode,jmp(lab_exit,C))
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
				      compile_insts(selCode,jmp(lab_exit,C')))
		    in
		      (add_label_to_jump_tab(selLabel,jumpTable),C'')  (* DOT_WORD pp_lab selLabel :: jumpTable *)
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
		  if_not_equal_go_lab_sel(leftLab,start,C)
		else
		  if_less_than_go_lab_sel(leftLab,start,
                  if_greater_than_go_lab_sel(rightLab,start,C))
	      else
                if_less_than_go_lab_sel(leftLab,start,
                if_greater_than_go_lab_sel(rightLab,finish,C))

	    fun gen_binary_switch ((SOME leftSels,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
		val rightSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,rightSideLab,
				switch_with_jump_table(sels,defaultLab,
				label(leftSideLab,
				gen_binary_switch(split_list leftSels,defaultLab,
				label(rightSideLab,
				gen_binary_switch(split_list rightSels,defaultLab,C))))))
	      end
	      | gen_binary_switch ((SOME leftSels,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      let
		val leftSideLab = new_label ""
	      in
		bin_search_code(leftSideLab,startSel,finishSel,defaultLab,
				switch_with_jump_table(sels,defaultLab,
				label(leftSideLab,
				gen_binary_switch(split_list leftSels,defaultLab,C))))
	      end
	      | gen_binary_switch ((NONE,SOME (startSel,finishSel,sels),SOME rightSels),defaultLab,C) =
	      let
		val rightSideLab = new_label ""
	      in
		bin_search_code(defaultLab,startSel,finishSel,rightSideLab,
			      switch_with_jump_table(sels,defaultLab,
			      label(rightSideLab,
			      gen_binary_switch(split_list rightSels,defaultLab,C))))
	      end
	      | gen_binary_switch ((NONE,SOME (startSel,finishSel,sels),NONE),defaultLab,C) =
	      bin_search_code(defaultLab,startSel,finishSel,defaultLab,
			      switch_with_jump_table(sels,defaultLab,C))
	      | gen_binary_switch ((NONE, NONE, NONE),defaultLab,C) = die "JumpTables. genBinarySwitch, no selections."
	      | gen_binary_switch _ = die "JumpTables.genBinarySwitch"
		    
	    val sels_sorted = ListSort.sort 
	      (fn (i1, is1) => 
	       (fn (i2, is2) => i1 > i2)) sels
		    
	    val group_sels = group_sel_list (NONE, NONE, sels_sorted, [], [])
	    val default_lab = new_label "defaultLab"
	  in
	    comment("Begin binary switch with jump tables",
		    gen_binary_switch(split_list group_sels,default_lab,
				      label(default_lab,
					    compile_insts(default,
							  label(lab_exit,
							  comment("End binary switch with jump tables",C))))))
	  end
      in
	if (length sels) < BI.minCodeInBinSearch then
	  linear_search_new(sels,
			    default,
			    comment,
			    new_label,
			    if_not_equal_go_lab_sel,
			    compile_insts,
			    label,
			    jmp,
			    C)
	else
	  bin_search()
      end

    fun binary_search(sels:(int*'sinst) list,
		      default: 'sinst,
		      comment: string * 'inst list -> 'inst list,
		      new_label : string -> 'label,
		      compile_sel: int * 'inst list -> 'inst list,
		      if_not_equal_go_lab: 'label * 'inst list -> 'inst list,
		      if_less_than_go_lab: 'label * 'inst list -> 'inst list,
		      if_greater_than_go_lab: 'label * 'inst list -> 'inst list,
		      compile_insts: 'sinst * 'inst list -> 'inst list,
		      label: 'label * 'inst list -> 'inst list,
		      jmp: 'label * 'inst list -> 'inst list,
		      sel_dist: int * int -> int,
		      jump_table_header: 'label * int * 'inst list -> 'inst list,
		      add_label_to_jump_tab: 'label * 'inst list -> 'inst list,
		      eq_lab : 'label * 'label -> bool,
		      C: 'inst list) =
      binary_search_new(sels,
			default,
			comment,
			new_label,
			fn (l,s,il) => compile_sel(s,if_not_equal_go_lab(l,il)),
			fn (l,s,il) => compile_sel(s,if_less_than_go_lab(l,il)),
			fn (l,s,il) => compile_sel(s,if_greater_than_go_lab(l,il)),
			compile_insts,
			label,
			jmp,
			sel_dist,
			jump_table_header,
			add_label_to_jump_tab,
			eq_lab,
			C)

  end
