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
      Db.oneField `select scs_role.has_p(^(Int.toString uid),^(Db.qqq role))
                     from dual` = "t"

    fun has_one_p uid roles =
      List.foldl (fn (r,acc) => has_p uid r orelse acc) false roles
  end