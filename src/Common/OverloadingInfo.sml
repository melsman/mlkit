(* Overloading information *)

structure OverloadingInfo: OVERLOADING_INFO =
  struct
    type RecType = StatObject.RecType
    type TyVar = StatObject.TyVar
    type StringTree = PrettyPrint.StringTree

    datatype OverloadingInfo =
      UNRESOLVED_IDENT of TyVar
    | UNRESOLVED_DOTDOTDOT of RecType
    | RESOLVED_INT31
    | RESOLVED_INT32
    | RESOLVED_INT63
    | RESOLVED_INT64
    | RESOLVED_INTINF
    | RESOLVED_REAL
    | RESOLVED_STRING
    | RESOLVED_CHAR
    | RESOLVED_WORD8
    | RESOLVED_WORD31
    | RESOLVED_WORD32
    | RESOLVED_WORD63
    | RESOLVED_WORD64

    val values_64bit = Flags.is_on0 "values_64bit"
    val tag_values = Flags.is_on0 "tag_values"

    fun resolvedIntDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true) => RESOLVED_INT63
          | (false, true) => RESOLVED_INT64
          | (true,  false) => RESOLVED_INT31
          | (false, false) => RESOLVED_INT32

    fun resolvedWordDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true) => RESOLVED_WORD63
          | (false, true) => RESOLVED_WORD64
          | (true,  false) => RESOLVED_WORD31
          | (false, false) => RESOLVED_WORD32

    fun string (UNRESOLVED_IDENT tyvars) = "UNRESOLVED_IDENT"
      | string (UNRESOLVED_DOTDOTDOT tau) = "UNRESOLVED_DOTDOTDOT"
      | string RESOLVED_INT31 =  "RESOLVED_INT31"
      | string RESOLVED_INT32 =  "RESOLVED_INT32"
      | string RESOLVED_INT63 =  "RESOLVED_INT63"
      | string RESOLVED_INT64 =  "RESOLVED_INT64"
      | string RESOLVED_INTINF = "RESOLVED_INTINF"
      | string RESOLVED_REAL =   "RESOLVED_REAL"
      | string RESOLVED_STRING = "RESOLVED_STRING"
      | string RESOLVED_CHAR =   "RESOLVED_CHAR"
      | string RESOLVED_WORD8 =  "RESOLVED_WORD8"
      | string RESOLVED_WORD31 = "RESOLVED_WORD31"
      | string RESOLVED_WORD32 = "RESOLVED_WORD32"
      | string RESOLVED_WORD63 = "RESOLVED_WORD63"
      | string RESOLVED_WORD64 = "RESOLVED_WORD64"

    val layout = PrettyPrint.LEAF o string

  end;
