(*$ERROR_TRAVERSE: ERROR_CODE*)
signature ERROR_TRAVERSE =
  sig
    structure ErrorCode : ERROR_CODE

    type topdec
    type Report

    datatype result = SUCCESS
		    | FAILURE of Report * ErrorCode.ErrorCode list

    val traverse: topdec -> result
  end;
