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


end