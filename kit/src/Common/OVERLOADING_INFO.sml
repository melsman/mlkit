(*OverloadingInfo is part of the ElabInfo.  See ELAB_INFO for an
 overview of the different kinds of info.*)

signature OVERLOADING_INFO =
  sig
    type TyVar
    type RecType
    type StringTree

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

    val resolvedWordDefault : unit -> OverloadingInfo
    val resolvedIntDefault : unit -> OverloadingInfo

    val string : OverloadingInfo -> string
    val layout : OverloadingInfo -> StringTree
end;
