signature JUMP_TABLES =
sig

 val linear_search_new : (*sels*) ('sel * 'sinst) list *
                         (*default*) 'sinst *
			 (*comment*) (string * 'inst list -> 'inst list) *
			 (*new_label*) (string -> 'label) *
			 (*if_no_match_go_lab_sel*) ('label * 'sel * 'inst list -> 'inst list) *
			 (*compile_insts*) ('sinst * 'inst list -> 'inst list) *
			 (*label*) ('label * 'inst list -> 'inst list) *
			 (*jmp*) ('label * 'inst list -> 'inst list) *
                         (*inline_cont*) ('inst list -> ('inst list -> 'inst list) option) *
			 (*C*) 'inst list -> 'inst list

 val binary_search_new : (*sels *) (IntInf.int*'sinst) list *
			 (*default*) 'sinst *
			 (*comment*) (string * 'inst list -> 'inst list) *
			 (*new_label*) (string -> 'label) *
			 (*if_not_equal_go_lab_sel*) ('label * IntInf.int * 'inst list -> 'inst list) *
			 (*if_less_than_go_lab_sel*) ('label * IntInf.int * 'inst list -> 'inst list) *
			 (*if_greater_than_go_lab_sel*) ('label * IntInf.int * 'inst list -> 'inst list) *
			 (*compile_insts*) ('sinst * 'inst list -> 'inst list) *
			 (*label*) ('label * 'inst list -> 'inst list) *
			 (*jmp*) ('label * 'inst list -> 'inst list) *
			 (*sel_dist*) (IntInf.int * IntInf.int -> IntInf.int) *

                                                         (*start*)   (*length*)
			 (*jump_table_header*) ('label * IntInf.int * IntInf.int * 'inst list -> 'inst list) *
			 (*add_label_to_jump_tab*) ('label * 'label * 'inst list -> 'inst list) *
			 (*eq_lab*) ('label * 'label -> bool) *
                         (*inline_cont*) ('inst list -> ('inst list -> 'inst list) option) *
			 (*C*) 'inst list -> 'inst list

end
