(* Handlings of local labels and backpatching *)
(* Taken from the Moscow ML compiler *)

functor ResolveLocalLabels(structure BC : BUFF_CODE
			   structure IntStringFinMap : MONO_FINMAP where type dom = int * string
			   structure Labels : ADDRESS_LABELS
			   structure Crash : CRASH) : RESOLVE_LOCAL_LABELS =
  struct

    structure M = IntStringFinMap
    fun die s  = Crash.impossible ("ResolveLocalLabels." ^ s)

    type label = Labels.label
    datatype label_definition =
      Label_defined of int
    | Label_undefined of (int * int) list     (* aren't the two integers the same always? ME 2000-10-24 *)

    val label_table : label_definition M.map ref = ref M.empty

    fun reset_label_table () = label_table := M.empty

    fun define_label lbl =
      let
	val lbl_k = Labels.key lbl
	fun define_label_in_map L =
	  let 
	    val curr_pos = !BC.out_position 
	  in
	    label_table := M.add (lbl_k, Label_defined curr_pos, !label_table);
	    case L of
	      [] => ()
	    |  _ => (* Backpatching the list L of pending labels: *)
		(List.app (fn (pos,orig) => 
			   (BC.out_position := pos;
			    BC.out_long_i (curr_pos - orig)))
		 L;
		 BC.out_position := curr_pos)
	  end
      in
	case M.lookup (!label_table) lbl_k
	  of NONE => define_label_in_map []
	| SOME (Label_defined _) => die ("define_label : label " ^ (Labels.pr_label lbl) ^ " already defined.")
	| SOME (Label_undefined L) => define_label_in_map L
      end

    fun out_label_with_orig orig lbl =
      let
	val lbl_k = Labels.key lbl
	fun out_label L =
	  (label_table := M.add (lbl_k, Label_undefined ((!BC.out_position, orig) :: L), !label_table);
	   BC.out_long_i (#1 lbl_k))  (* instead of 0 - we put the label key as a place holder; used for 
				       * data-labels in the KAM machine; mael 2004-03-17: How is this used?? For
				       * now, we just take the int-part of the name, but that probably won't work
				       * with the mlb-compilation technique, where the ints are not unique. *)
      in
	case M.lookup (!label_table) lbl_k 
	  of NONE => out_label []
	  | SOME (Label_defined def) => BC.out_long_i (def - orig)
	  | SOME (Label_undefined L) => out_label L
      end

    fun out_label l = out_label_with_orig (!BC.out_position) l    (* for relative jumps *)

    fun imports (labels: label list): (int * label) list =   (* the ints are relative addresses to
							      * code positions that refers to the 
							      * labels *)
      let 
	fun each (l,acc) =
	  case M.lookup (!label_table) (Labels.key l) 
	    of SOME (Label_undefined L) => 
	      foldl (fn ((a,b),acc) => if a <> b then die "imports - no, the two integers are not always identical!"
				       else (a, l) :: acc) acc L
	    | SOME _ => die "imports - Label_undefined expected"
	    | NONE => die "imports - NONE"
      in foldl each nil labels
      end

    fun exports (labels: label list) : (label * int) list =      (* returns relative addresses for the labels *)
      let 
	fun each l =
	  case M.lookup (!label_table) (Labels.key l) 
	    of SOME (Label_defined i) => (l,i)
	     | SOME _ => die "exports - Label_defined expected"
	     | NONE => die "exports - NONE"
      in map each labels
      end
  end
