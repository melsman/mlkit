(* Handlings of local labels and backpatching *)
(* Taken from the Moscow ML compiler *)

signature RESOLVE_LOCAL_LABELS =
  sig
    type label
    
    val reset_label_table : unit -> unit
    val define_label : label -> unit
    val out_label_with_orig : int -> label -> unit
    val out_label : label -> unit
  end