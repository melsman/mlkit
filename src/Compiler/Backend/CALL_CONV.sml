signature CALL_CONV =
  sig

    (* Implements a Call Convention. *)

    type lvar    
    type offset = int
    type cc

    val mk_cc_fn      : lvar list * lvar option * lvar list * lvar list -> cc
    val mk_cc_fun     : lvar list * lvar option * lvar list * lvar option * lvar list * lvar list -> cc
    val get_res_lvars : cc -> lvar list
    val resolve_cc    : cc -> cc * (lvar*lvar) list * (lvar*lvar) list
    val resolve_app   : (lvar -> 'a) -> 
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} ->
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} * ('a*lvar) list * ('a*lvar) list
    val resolve_ccall : (lvar -> 'a) ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} * ('a*lvar) list * ('a*lvar) list

    val get_spilled_args              : cc -> lvar list
    val get_spilled_args_with_offsets : cc -> (lvar * offset) list

    val get_spilled_res               : cc -> lvar list
    val get_spilled_res_with_offsets  : cc -> (lvar * offset) list

    val get_frame_size                : cc -> int
    val add_frame_size                : cc * int -> cc
    val get_cc_size                   : cc -> int
    val get_rcf_size                  : cc -> int
    val get_ccf_size                  : cc -> int

    val resolve_act_cc                : {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} ->
                                        ('a * int) list * ('a * int) list * int

    val handl_return_phreg            : unit -> lvar
    val handl_arg_phreg               : unit -> lvar * lvar

    (******************)
    (* PrettyPrinting *)
    (******************)
    val pr_cc     : cc -> string

  end








