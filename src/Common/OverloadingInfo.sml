(* Overloading information *)

(*$OverloadingInfo: 
	STATOBJECT PRETTYPRINT OVERLOADING_INFO
*)

functor OverloadingInfo (structure StatObject : STATOBJECT
			 structure PrettyPrint : PRETTYPRINT
			   ) : OVERLOADING_INFO =
  struct
    type RecType = StatObject.RecType
    type TyVar = StatObject.TyVar
    type StringTree = PrettyPrint.StringTree

    datatype OverloadingInfo =
      UNRESOLVED_IDENT of TyVar
    | UNRESOLVED_DOTDOTDOT of RecType
    | RESOLVED_INT
    | RESOLVED_REAL		
    | RESOLVED_STRING
    | RESOLVED_CHAR
    | RESOLVED_WORD

    fun string (UNRESOLVED_IDENT tyvars) = "UNRESOLVED_IDENT"
      | string (UNRESOLVED_DOTDOTDOT tau) = "UNRESOLVED_DOTDOTDOT"
      | string RESOLVED_INT =    "RESOLVED_INT"
      | string RESOLVED_REAL =   "RESOLVED_REAL"
      | string RESOLVED_STRING = "RESOLVED_STRING"
      | string RESOLVED_CHAR =   "RESOLVED_CHAR"
      | string RESOLVED_WORD =   "RESOLVED_WORD"
    val layout = PrettyPrint.LEAF o string
  end;
