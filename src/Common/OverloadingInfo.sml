(* Overloading information *)

(*$OverloadingInfo: 
	STATOBJECT PRETTYPRINT OVERLOADING_INFO
*)

functor OverloadingInfo (structure StatObject : STATOBJECT
			 structure PrettyPrint : PRETTYPRINT
			   ) : OVERLOADING_INFO =
  struct
    type Type = StatObject.Type
    type StringTree = PrettyPrint.StringTree

    datatype OverloadingInfo =
      UNRESOLVED of Type
    | RESOLVED_INT
    | RESOLVED_REAL		

    fun string (UNRESOLVED tv) = "UNRESOLVED"
      | string RESOLVED_INT = "RESOLVED_INT"
      | string RESOLVED_REAL = "RESOLVED_REAL"
    val layout = PrettyPrint.LEAF o string
  end;
