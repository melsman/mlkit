(* Overloading information *)

functor OverloadingInfo (structure StatObject : STATOBJECT
			 structure PrettyPrint : PRETTYPRINT
			 structure Flags : FLAGS
			   ) : OVERLOADING_INFO =
  struct
    type RecType = StatObject.RecType
    type TyVar = StatObject.TyVar
    type StringTree = PrettyPrint.StringTree

    datatype OverloadingInfo =
      UNRESOLVED_IDENT of TyVar
    | UNRESOLVED_DOTDOTDOT of RecType
    | RESOLVED_INT31
    | RESOLVED_INT32
    | RESOLVED_REAL		
    | RESOLVED_STRING
    | RESOLVED_CHAR
    | RESOLVED_WORD8
    | RESOLVED_WORD31
    | RESOLVED_WORD32

    val tag_values = Flags.is_on0 "tag_values"

    fun resolvedIntDefault () = 
      if tag_values() then RESOLVED_INT31
      else RESOLVED_INT32

    fun resolvedWordDefault () = 
      if tag_values() then RESOLVED_WORD31
      else RESOLVED_WORD32

    fun string (UNRESOLVED_IDENT tyvars) = "UNRESOLVED_IDENT"
      | string (UNRESOLVED_DOTDOTDOT tau) = "UNRESOLVED_DOTDOTDOT"
      | string RESOLVED_INT31 =  "RESOLVED_INT31"
      | string RESOLVED_INT32 =  "RESOLVED_INT32"
      | string RESOLVED_REAL =   "RESOLVED_REAL"
      | string RESOLVED_STRING = "RESOLVED_STRING"
      | string RESOLVED_CHAR =   "RESOLVED_CHAR"
      | string RESOLVED_WORD8 =  "RESOLVED_WORD8"
      | string RESOLVED_WORD31 = "RESOLVED_WORD31"
      | string RESOLVED_WORD32 = "RESOLVED_WORD32"

    val layout = PrettyPrint.LEAF o string

  end;
