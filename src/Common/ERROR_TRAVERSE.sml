(*$ERROR_TRAVERSE*)
signature ERROR_TRAVERSE =
  sig
    type topdec
    type Report

    datatype result = SUCCESS of Report 
		    | FAILURE of Report
    (*If `report' in `SUCCESS report' is not Report.null,
     it is a warning about escaping tyvars.*)

    val traverse: topdec -> result
  end;
