signature JUMP_TABLES =
sig

 val linear_search : (*sels*) ('sel * 'sinst list) list *
		     (*default*) 'sinst list *
                     (*comment*) (string * 'inst list -> 'inst list) *
		     (*new_label*) (string -> 'label) *
		     (*compile_sel*) ('sel * 'inst list -> 'inst list) *
		     (*if_no_match_go_lab*) ('label * 'inst list -> 'inst list) *
		     (*compile_insts*) ('sinst list * 'inst list -> 'inst list) *
		     (*label*) ('label * 'inst list -> 'inst list) *
                     (*jmp*) ('label * 'inst list -> 'inst list) *
		     (*C*) 'inst list -> 'inst list

 val binary_search : (*sels *) (int*'sinst list) list *
		     (*default*) 'sinst list *
		     (*comment*) (string * 'inst list -> 'inst list) *
		     (*new_label*) (string -> 'label) *
		     (*compile_sel*) (int * 'inst list -> 'inst list) *
		     (*if_not_equal_go_lab*) ('label * 'inst list -> 'inst list) *
		     (*if_less_than_go_lab*) ('label * 'inst list -> 'inst list) *
		     (*if_greater_than_go_lab*) ('label * 'inst list -> 'inst list) *
		     (*compile_insts*) ('sinst list * 'inst list -> 'inst list) *
		     (*label*) ('label * 'inst list -> 'inst list) *
		     (*jmp*) ('label * 'inst list -> 'inst list) *
		     (*sel_dist*) (int * int -> int) *
		     (*jump_table_header*) ('label * int * 'inst list -> 'inst list) *
		     (*add_label_to_jump_tab*) ('label * 'inst list -> 'inst list) *
		     (*eq_lab*) ('label * 'label -> bool) *
		     (*C*) 'inst list -> 'inst list

end