(* INFIX_STACK - we've distilled out all the common algorithmic code for
		 dealing with precedence of operators, and put it into
		 a functor which can be parameterised for operation over
		 expressions or patterns. Non-trivial, though - expressions
		 and patterns are rather wildly different things. *)

(*$INFIX_STACK*)
signature INFIX_STACK =
  sig
    type InfixBasis
    type AtomObject
    type FullObject

    val resolveInfix: InfixBasis * AtomObject list -> FullObject
  end;
