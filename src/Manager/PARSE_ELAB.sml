(*$PARSE_ELAB: ERROR_CODE*)

signature PARSE_ELAB =
  sig
    structure ErrorCode : ERROR_CODE

    type Report and InfixBasis and ElabBasis and topdec

    datatype Result = SUCCESS of {report: Report, infB: InfixBasis, 
				  elabB: ElabBasis, topdec: topdec}
		    | FAILURE of Report * ErrorCode.ErrorCode list

    val parse_elab : {infB: InfixBasis, elabB: ElabBasis, 
		      file: string} -> Result

  end