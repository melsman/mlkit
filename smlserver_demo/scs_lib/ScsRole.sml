signature SCS_ROLE =
  sig
    (* This structure is the ML API for the Oracle package scs_role,
       see scs-roles-create.sql *)

    (* [role] Pre-defined roles. This list is likely to change depending 
       on what kind of web-site you are building. *)
    datatype role = 
      SiteAdm
    | StudAdm
    | OaAdm
    | SupervisorAdm
    | Other of string

    (* [fromString str] returns the corresponding role which is either
       one of the pre-defined roles in the role datatype or the role
       Other. The function is used to convert between the datatype
       role and the representation in the database. *)
    val fromString : string -> role
 
    (* [toString role] returns the string representation of the role
       as stored in the database. *)
    val toString : role -> string

    (* [has_p uid role] returns true if user uid has role role;
       otherwise returns false. *)
    val has_p     : int -> role -> bool

    (* [has_one_p uid roles] returns true if user uid has atleast one
       of the roles in the role list roles *)
    val has_one_p : int -> role list -> bool

    (* [has_one_or_empty_p uid roles] same as has_one_p except that it
       returns true if the role list roles is empty. *)
    val has_one_or_empty_p : int -> role list -> bool

  end

structure ScsRole :> SCS_ROLE =
  struct
    datatype role = 
      SiteAdm
    | StudAdm
    | OaAdm
    | SupervisorAdm
    | Other of string

    fun fromString str = 
      case str of
        "SiteAdm" => SiteAdm
      | "StudAdm" => StudAdm
      | "VejlederAdm" => SupervisorAdm
      | "OaAdm" => OaAdm
      | s => Other s
 
    (* [toString role] returns the string representation of the role
       as stored in the database. *)
    fun toString role =
      case role of
        SiteAdm => "SiteAdm"
      | StudAdm => "StudAdm"
      | OaAdm   => "OaAdm"
      | SupervisorAdm => "VejlederAdm"
      | Other s => s

    fun has_p uid role =
      Db.oneField `select scs_role.has_p(^(Int.toString uid),^(Db.qqq (toString role)))
                     from dual` = "t"

    fun has_one_p uid [] = false
      | has_one_p uid (x::xs) = has_p uid x orelse (has_one_p uid xs)

    fun has_one_or_empty_p uid [] = true
      | has_one_or_empty_p uid xs = has_one_p uid xs

  end
