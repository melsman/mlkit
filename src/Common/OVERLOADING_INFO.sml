(*OverloadingInfo is part of the ElabInfo.  See ELAB_INFO for an
 overview of the different kinds of info.*)

(*$OVERLOADING_INFO*)

signature OVERLOADING_INFO =
  sig
    type Type
    type StringTree

    datatype OverloadingInfo =
      UNRESOLVED of Type
    | RESOLVED_INT
    | RESOLVED_REAL

    val string : OverloadingInfo -> string
    val layout : OverloadingInfo -> StringTree
end;
