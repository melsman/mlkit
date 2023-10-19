
signature PARSE_ELAB =
  sig
    structure ErrorCode : ERROR_CODE

    type Report and InfixBasis and ElabBasis and topdec

    type absprjid (* absolute project identifier *)

    datatype src = SrcFile of string
                 | SrcString of string

    type renderer = TopLevelReport.renderer
    datatype Result = SUCCESS of {doreport: renderer option -> Report,
                                  infB: InfixBasis,
				  elabB: ElabBasis, topdec: topdec}
		    | FAILURE of Report * ErrorCode.ErrorCode list

    val parse_elab : {infB: InfixBasis, elabB: ElabBasis,
		      absprjid: absprjid, src: src} -> Result

    type State
    val begin_stdin      : unit -> State
    val parse_elab_stdin : {infB: InfixBasis, elabB: ElabBasis,
		            absprjid: absprjid, state: State}
                           -> State option * Result
  end


(**

[parse_elab arg] either succeeds or fails. The optional state is to
allow for the parser to proceed with partial input, which is relevant
for the REPL, but not for file-based input.

*)
