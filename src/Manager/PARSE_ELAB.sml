
signature PARSE_ELAB =
  sig
    structure ErrorCode : ERROR_CODE

    datatype src = SrcFile of string
                 | SrcString of string

    type Report and InfixBasis and ElabBasis and topdec

    type absprjid (* absolute project identifier *)

    datatype Result = SUCCESS of {report: Report, infB: InfixBasis, 
				  elabB: ElabBasis, topdec: topdec}
		    | FAILURE of Report * ErrorCode.ErrorCode list

    val parse_elab : {infB: InfixBasis, elabB: ElabBasis, 
		      absprjid: absprjid, src: src} -> Result

  end
