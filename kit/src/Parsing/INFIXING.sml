(* INFIXING - resolve infix expressions and patterns, and try to make some
	      sense of `fun'-declarations. *)

(*$INFIXING*)
signature INFIXING =
  sig
    type InfixBasis
    type topdec
    type Report

    datatype 'a result = SUCCESS of 'a | FAILURE of Report
    val resolve: InfixBasis * topdec -> (InfixBasis * topdec) result
  end;










