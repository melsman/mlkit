(*OverloadingInfo is part of the ElabInfo.  See ELAB_INFO for an
 overview of the different kinds of info.*)

(*$OVERLOADING_INFO*)

signature OVERLOADING_INFO =
  sig
    type TyVar
    type RecType
    type StringTree

    datatype OverloadingInfo =
      UNRESOLVED_IDENT of TyVar
    | UNRESOLVED_DOTDOTDOT of RecType
    | RESOLVED_INT
    | RESOLVED_REAL
    | RESOLVED_STRING
    | RESOLVED_CHAR
    | RESOLVED_WORD

    val string : OverloadingInfo -> string
    val layout : OverloadingInfo -> StringTree
end;
