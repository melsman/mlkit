
(*$PARSE_ELAB*)

signature PARSE_ELAB =
  sig

    type Report and Basis and topdec

    datatype Result = SUCCESS of {report: Report, basis: Basis, topdec: topdec}
		    | FAILURE of Report

    val parse_elab : {Basis: Basis, file: string} -> Result

  end