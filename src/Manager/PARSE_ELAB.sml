(*$PARSE_ELAB*)

signature PARSE_ELAB =
  sig

    type Report and InfixBasis and ElabBasis and topdec

    datatype Result = SUCCESS of {report: Report, infB: InfixBasis, 
				  elabB: ElabBasis, topdec: topdec}
		    | FAILURE of Report

    val parse_elab : {infB: InfixBasis, elabB: ElabBasis, 
		      file: string} -> Result

  end