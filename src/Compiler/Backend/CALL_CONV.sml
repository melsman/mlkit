signature CALL_CONV =
  sig

    (* Implements a Call Convention. *)

    type lvar    
    type phreg = word
    type offset = int
    type cc

    val mk_cc_fn      : lvar list * lvar option * lvar list * lvar list -> cc
    val mk_cc_fun     : lvar list * lvar option * lvar list * lvar option * lvar list * lvar list -> cc
    val get_res_lvars : cc -> lvar list
    val resolve_cc    : cc -> cc * (lvar*phreg) list * (lvar*phreg) list
    val resolve_app   : (phreg -> 'a) -> 
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} ->
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} * ('a*phreg) list * ('a*phreg) list
    val resolve_ccall : (phreg -> 'a) ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} * ('a*phreg) list * ('a*phreg) list

    val get_spilled_args              : cc -> lvar list
    val get_spilled_args_with_offsets : cc -> (lvar * offset) list

    val get_spilled_res               : cc -> lvar list
    val get_spilled_res_with_offsets  : cc -> (lvar * offset) list

    val get_frame_size                : cc -> int
    val add_frame_size                : cc * int -> cc


    (******************)
    (* PrettyPrinting *)
    (******************)
    val pr_cc     : cc -> string

  end








