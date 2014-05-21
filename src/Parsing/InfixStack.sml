(* InfixStack - we have to implement an operator precedence algorithm both
		for expressions and for patterns; they share the type of
		infix basis. Rather than trying to get too polymorphic in
		the core-ML code in Infixing, it seems neater to have a
		functor and apply it once for expressions and once for
		patterns. *)

functor InfixStack(type FullObject	(* exp or pat *)
		   type AtomObject	(* atexp or atpat *)
		   type id = InfixBasis.id
		   val pr_id: id -> string

		   val atomToFull: AtomObject -> FullObject
		   val fullToAtom: FullObject -> AtomObject

		   val pair: FullObject * FullObject -> AtomObject

		   val asId: AtomObject -> id option
		   val applyId: id * AtomObject -> FullObject
		   val applyObj: FullObject * AtomObject -> FullObject

		   exception InfixStack_error of string
		  ): INFIX_STACK =
  struct
    fun impossible s = Crash.impossible ("InfixStack." ^ s)
    open InfixBasis
    type InfixBasis = Basis

    type FullObject = FullObject
     and AtomObject = AtomObject

   (* We can stack "operators" (infixes and the implicit applications). *)

    datatype StackEntry = INFIXentry of id * int
			| INFIXRentry of id * int
			| APPLentry

   (* The list of output operands consists of atomic exps/pats (things
      which are passed through unchanged, for example) as well as exps/pats
      (results of applications). *)

    datatype OutputEntry = ATOM of AtomObject
      			 | FULL of FullObject

   (* Coerce an object in the output list to be an expression or an atomic
      expression, according to what we want to do with it. *)

    fun atom(ATOM x) = x
      | atom(FULL x) = fullToAtom x

    and full(ATOM x) = atomToFull x
      | full(FULL x) = x

   (* Keep track of the last thing we passed - needed to spot
      applications (two operands successively, with no intervening
      operator). *)

    datatype LastObj = ARG | OPER | VOID

   (* apply - apply a stack entry (infix(r) or appl) to the list of
      generated arguments. Might fail on source input like "A ::" where we
      run out of arguments while flushing the stack, so we make apply
      bulletproof. Note that the args list is in reverse order. *)

    fun apply(entry, (snd :: fst :: rest): OutputEntry list) =
          let
	    val thePair = pair(full fst, full snd)
	  in
	    FULL(case entry
		   of INFIXentry(id, _) => applyId(id, thePair)
		    | INFIXRentry(id, _) => applyId(id, thePair)
		    | APPLentry => applyObj(full fst, atom snd)
	        ) :: rest
	  end

      | apply(entry, _) =
	  raise InfixStack_error
	        ("Give "
		 ^ (case entry of
		      INFIXentry(id, _) => "the infix `" ^ pr_id id ^ "' more arguments."
		    | INFIXRentry(id, _) => "the infixr `" ^ pr_id id ^ "' more arguments."
		    | APPLentry => impossible "apply"))

    (*assocLeft(op1, op2) - precedence comparison of infix(r) and appl
     stack entries.  Application is the highest priority, and associates
     to the left. Other operators associate according to precedence
     (sec. 2.6, Definition of sml'96): If the precedences are not equal,
     association is to the side with the highest precedence, otherwise,
     the operators must associate to the same side according to their
     infix/infixr status (if they do not, an error message is given).*)

    and assocLeft (APPLentry, _) = true	(* APPL is highest (left) precedence. *)
      | assocLeft (_, APPLentry) = false
      | assocLeft (op1, op2) =
          precedence op1 > precedence op2 orelse
	  precedence op1 = precedence op2 andalso both_associate_left (op1, op2)

    and precedence (INFIXRentry(id,i)) = i
      | precedence (INFIXentry(id,i)) = i
      | precedence _  = impossible "precedence"

    and op_as_string (INFIXentry(id,prec)) = pr_id id
      | op_as_string (INFIXRentry(id,prec)) = pr_id id
      | op_as_string _ = impossible "op_as_string"

    and side (INFIXentry _) = "left"
      | side (INFIXRentry _) = "right"
      | side _ = impossible "side"

    (*both_associate_left (op1,op2): op1 and op2 must have the same precedence*)
    and both_associate_left (INFIXentry(id1,prec1),INFIXentry(id2,prec2)) = true
      | both_associate_left (INFIXRentry(id1,prec1),INFIXRentry(id2,prec2)) = false
      | both_associate_left (op1,op2) =
          raise InfixStack_error
	    ("Insert parentheses.  `"
	     ^ op_as_string op1 ^ "' and `" ^ op_as_string op2
	     ^ "' have the same precedence\nbut associate "
	     ^ side op1 ^ " and " ^ side op2 ^ ", respectively.")

   (* flushHigher - flush out all entries in the stack with higher
      effective precedence than "entry". Take INFIX, INFIXR, APPL status
      into account. *)

    fun flushHigher(entry, stack, output) =
      case stack
	of nil => (nil, output)
	 | top :: rest =>
	     if assocLeft(top, entry) then
	       flushHigher(entry, rest, apply(top, output))
	     else
	       (stack, output)


   (* flushAll - clear out all the stacked operators at the end of an
      atexp sequence. *)

    fun flushAll ([],          [item]) = item
      | flushAll ([],          output) = impossible "flushAll"
      | flushAll (top :: rest, output) = flushAll (rest, apply (top, output))

   (* process - the shunting function, with the usual Rothwell interpretation
      (viz. apply any outgoing operators to things in the output list, rather
      than add them to the list in reverse polish notation). *)

    fun process(iBas: InfixBasis.Basis,
		input: AtomObject list, stack: StackEntry list,
		last: LastObj, output: OutputEntry list
	       ): OutputEntry =
      case input
	of nil =>			(* Nothing more to process *)
	     flushAll(stack, output)

	 | this :: rest =>
	     (case asId this
		of SOME id =>		(* Dealing with an unqual. id. *)
		     (case lookup iBas id
			of INFIX n =>
			     operator(iBas, INFIXentry(id, n),
				      rest, stack, output
				     )

			 | INFIXR n =>
			     operator(iBas, INFIXRentry(id, n),
				      rest, stack, output
				     )

			 | NONFIX =>
			     (case last
				of ARG =>	(* Must generate an appl. *)
				     operator(iBas, APPLentry, input,
					      stack, output
					     )

				 | _ =>		(* Just pass the nonfix. *)
				     process(iBas, rest, stack, ARG,
					     ATOM this :: output
					    )
			     )
		     )

		 | None =>		(* Not an unqual. identifier. *)
		     (case last
			of ARG =>	(* Must generate an application *)
			     operator(iBas, APPLentry, input, stack, output)

			 | _ =>		(* Just pass it through. *)
			     process(iBas, rest, stack, ARG,
				     ATOM this :: output
				    )
		     )
	     )

   (* operator - flush all stack entries with higher precedence, and then
      stack this one. *)

    and operator(iBas, entry, input, stack, output) =
      let
	val (stack', output') = flushHigher(entry, stack, output)
      in
	process(iBas, input, entry :: stack', OPER, output')
      end

   (* resolveInfix - takes a list of atomic exps/pats and returns a
      single exp/pat with the nonfix and infix applications in place.
      Usual Dijkstra shunting algorithm stuff, except that we have to
      identify runs of nonfixes (they must be applications), and we have to
      detect ill-formed cases (the input grammer was only specific enough to
      deliver a list of atexps, which includes things like "2 ::"). *)

    fun resolveInfix(iBas, atoms) =
      full(process(iBas, atoms, nil, VOID, nil))
  end;
