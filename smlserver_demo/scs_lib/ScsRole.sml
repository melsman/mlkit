signature SCS_ROLE =
  sig
    (* This structure is the ML API for the Oracle package scs_role,
       see scs-roles-create.sql *)

    type role = string

    (* [has_p uid role] returns true if user uid has role role;
       otherwise returns false. *)
    val has_p     : int -> role -> bool

    (* [has_one_p uid roles] returns true if user uid has atleast one
       of the roles in the role list roles *)
    val has_one_p : int -> role list -> bool
  end

structure ScsRole :> SCS_ROLE =
  struct
    type role = string
    fun has_p uid role =
let
val v =
      Db.oneField `select scs_role.has_p(^(Int.toString uid),^(Db.qqq role))
                     from dual` = "t"
val _ = Ns.log(Ns.Notice,"ScsRole:" ^ (Int.toString uid) ^ " and " ^ role ^ " = " ^ Bool.toString v)
in
v
end

    fun has_one_p uid [] = false
      | has_one_p uid (x::xs) = has_p uid x orelse (has_one_p uid xs)
  end