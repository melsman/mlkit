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
    | UcsPbSupervisorAdm  (* Created in ucs-pb-supervisor-lists-initialdata-create.sql *)
    | UcsEbEventEditor    (* Created in ucs-events-initialdata-create.sql *)
    | ScsPersonAdm        (* Created in scs-users-initialdata-create.sql *)
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
    | UcsPbSupervisorAdm
    | UcsEbEventEditor
    | ScsPersonAdm
    | Other of string

    fun fromString str = 
      case str of
        "SiteAdm" => SiteAdm
      | "StudAdm" => StudAdm
      | "UcsPbVejlederAdm" => UcsPbSupervisorAdm
      | "OaAdm" => OaAdm
      | "UcsEbEventEditor" => UcsEbEventEditor
      | "ScsPersonAdm" => ScsPersonAdm
      | s => Other s
 
    (* [toString role] returns the string representation of the role
       as stored in the database. *)
    fun toString (role:role) =
      case role of
        SiteAdm => "SiteAdm"
      | StudAdm => "StudAdm"
      | OaAdm   => "OaAdm"
      | UcsPbSupervisorAdm => "UcsPbVejlederAdm"
      | UcsEbEventEditor => "UcsEbEventEditor"
      | ScsPersonAdm => "ScsPersonAdm"
      | Other s => s

    fun has_p uid (role:role) =
      let
        val role_sql = `
          select scs_role.has_p(^(Int.toString uid),^(Db.qqq (toString role)))
            from dual`
      in
	Db.oneField role_sql = "t"
      end 

    fun has_one_p uid [] = false
      | has_one_p uid (x::xs) = has_p uid x orelse (has_one_p uid xs)

    fun has_one_or_empty_p uid [] = true
      | has_one_or_empty_p uid xs = has_one_p uid xs

  end





