signature CALL_CONV =
  sig

    (* Implements a Call Convention. *)

    type lvar    
    type cc

    val mk_cc_fn      : lvar list * lvar option * lvar list * lvar list -> cc
    val mk_cc_fun     : lvar list * lvar option * lvar list * lvar option * lvar list * lvar list -> cc
    val get_res_lvars : cc -> lvar list
    val resolve_cc    : cc -> cc * (lvar*int) list * (lvar*int) list
    val resolve_app   : (int -> 'a) -> 
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} ->
                        {clos: 'a option, free: 'a list, args: 'a list, reg_vec: 'a option, reg_args: 'a list, res: 'a list} * ('a*int) list * ('a*int) list
    val resolve_ccall : (int -> 'a) ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} ->
                        {args: 'a list, rhos_for_result: 'a list, res: 'a list} * ('a*int) list * ('a*int) list

    val pr_cc     : cc -> string

  end








