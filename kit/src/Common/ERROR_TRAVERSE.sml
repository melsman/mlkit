(*$ERROR_TRAVERSE*)
signature ERROR_TRAVERSE =
  sig
    type topdec
    type Report

    datatype result = SUCCESS
		    | FAILURE of Report

    val traverse: topdec -> result
  end;
