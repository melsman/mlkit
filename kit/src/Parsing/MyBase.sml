(*$MyBase*)

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* This file is (roughly) the concatenation of base.sig, stream.sml,
   lrtable.sml, join.sml, parser2.sml with everything functorised
   (and ported to the SML library for portability, of course) . *)

(* STREAM: signature for a lazy stream.*)

signature STREAM =
 sig type 'xa stream
     val streamify : (unit -> '_a) -> '_a stream
     val cons : '_a * '_a stream -> '_a stream
     val get : '_a stream -> '_a * '_a stream
 end

(* LR_TABLE: signature for an LR Table.

   The list of actions and gotos passed to mkLrTable must be ordered by state
   number. The values for state 0 are the first in the list, the values for
    state 1 are next, etc.
*)

signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
	datatype state = STATE of int
	datatype term = T of int
	datatype nonterm = NT of int
	datatype action = SHIFT of state
			| REDUCE of int
			| ACCEPT
			| ERROR
	type table
	
	val numStates : table -> int
	val numRules : table -> int
	val describeActions : table -> state ->
				(term,action) pairlist * action
	val describeGoto : table -> state -> (nonterm,state) pairlist
	val action : table -> state * term -> action
	val goto : table -> state * nonterm -> state
	val initialState : table -> state
	exception Goto of state * nonterm

       (* NICK: the array structure may now be left closed by default but
	  the type `array' seems global. This is nonportable.
	  The SML Library uses type `Array'. *)

	val mkLrTable: {actions : ((term,action) pairlist * action) Array.Array,
			gotos : (nonterm,state) pairlist Array.Array,
			numStates : int, numRules : int,
			initialState : state} -> table
    end

(* TOKEN: signature revealing the internal structure of a token. This signature
   TOKEN distinct from the signature {parser name}_TOKENS produced by ML-Yacc.
   The {parser name}_TOKENS structures contain some types and functions to
    construct tokens from values and positions.

   The representation of token was very carefully chosen here to allow the
   polymorphic parser to work without knowing the types of semantic values
   or line numbers.

   This has had an impact on the TOKENS structure produced by SML-Yacc, which
   is a structure parameter to lexer functors.  We would like to have some
   type 'a token which functions to construct tokens would create.  A
   constructor function for a integer token might be

	  INT: int * 'a * 'a -> 'a token.
 
   This is not possible because we need to have tokens with the representation
   given below for the polymorphic parser.

   Thus our constructur functions for tokens have the form:

	  INT: int * 'a * 'a -> (svalue,'a) token

   This in turn has had an impact on the signature that lexers for SML-Yacc
   must match and the types that a user must declare in the user declarations
   section of lexers.
*)

signature TOKEN =
    sig
	structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	val sameToken : ('a,'b) token * ('a,'b) token -> bool
    end

(* LR_PARSER: signature for a polymorphic LR parser *)

signature LR_PARSER =
    sig
	structure Stream: STREAM
	structure LrTable : LR_TABLE
	structure Token : TOKEN

	sharing LrTable = Token.LrTable

	exception ParseError

	val parse : {table : LrTable.table,
		     lexer : ('_b,'_c) Token.token Stream.stream,
		     arg: 'arg,
		     saction : int *
			       '_c *
				(LrTable.state * ('_b * '_c * '_c)) list * 
				'arg ->
				     LrTable.nonterm *
				     ('_b * '_c * '_c) *
				     ((LrTable.state *('_b * '_c * '_c)) list),
		     void : '_b,
		     ec : { is_keyword : LrTable.term -> bool,
			    noShift : LrTable.term -> bool,
			    preferred_subst : LrTable.term -> LrTable.term list,
			    preferred_insert : LrTable.term -> bool,
			    errtermvalue : LrTable.term -> '_b,
			    showTerminal : LrTable.term -> string,
			    terms: LrTable.term list,
			    error : string * '_c * '_c -> unit
			   },
		     lookahead : int  (* max amount of lookahead used in *)
				      (* error correction *)
			} -> '_b *
			     (('_b,'_c) Token.token Stream.stream)
    end

(* LEXER: a signature that most lexers produced for use with SML-Yacc's
   output will match.  The user is responsible for declaring type token,
   type pos, and type svalue in the UserDeclarations section of a lexer.

   Note that type token is abstract in the lexer.  This allows SML-Yacc to
   create a TOKENS signature for use with lexers produced by ML-Lex that
   treats the type token abstractly.  Lexers that are functors parametrized by
   a Tokens structure matching a TOKENS signature cannot examine the structure
   of tokens.
*)

signature LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
	   end
	val makeLexer : (int -> string) -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* ARG_LEXER: the %arg option of ML-Lex allows users to produce lexers which
   also take an argument before yielding a function from unit to a token
*)

signature ARG_LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
		type arg
	   end
	val makeLexer : (int -> string) -> UserDeclarations.arg -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* PARSER_DATA: the signature of ParserData structures in {parser name}LrValsFun
   produced by  SML-Yacc.  All such structures match this signature.  

   The {parser name}LrValsFun produces a structure which contains all the values
   except for the lexer needed to call the polymorphic parser mentioned
   before.

*)

signature PARSER_DATA =
   sig
        (* the type of line numbers *)

	type pos

	(* the type of semantic values *)

	type svalue

         (* the type of the user-supplied argument to the parser *)
 	type arg
 
	(* the intended type of the result of the parser.  This value is
	   produced by applying extract from the structure Actions to the
	   final semantic value resultiing from a parse.
	 *)

	type result

	structure LrTable : LR_TABLE
	structure Token : TOKEN
	sharing Token.LrTable = LrTable

	(* structure Actions contains the functions which mantain the
	   semantic values stack in the parser.  Void is used to provide
	   a default value for the semantic stack.
	 *)

	structure Actions : 
	  sig
	      val actions : int * pos *
		   (LrTable.state * (svalue * pos * pos)) list * arg->
		         LrTable.nonterm * (svalue * pos * pos) *
			 ((LrTable.state *(svalue * pos * pos)) list)
	      val void : svalue
	      val extract : svalue -> result
	  end

	(* structure EC contains information used to improve error
	   recovery in an error-correcting parser *)

	structure EC :
	   sig
     val is_keyword : LrTable.term -> bool
	     val noShift : LrTable.term -> bool
	     val preferred_subst : LrTable.term -> LrTable.term list
	     val preferred_insert : LrTable.term -> bool
	     val errtermvalue : LrTable.term -> svalue
	     val showTerminal : LrTable.term -> string
	     val terms: LrTable.term list
	   end

	(* table is the LR table for the parser *)

	val table : LrTable.table
    end

(* signature PARSER is the signature that most user parsers created by 
   SML-Yacc will match.
*)

signature PARSER =
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	(* type pos is the type of line numbers *)

	type pos

	(* type result is the type of the result from the parser *)

	type result

         (* the type of the user-supplied argument to the parser *)
 	type arg
	
	(* type svalue is the type of semantic values for the semantic value
	   stack
	 *)

	type svalue

	(* val makeLexer is used to create a stream of tokens for the parser *)

	val makeLexer : (int -> string) ->
			 (svalue,pos) Token.token Stream.stream

	(* val parse takes a stream of tokens and a function to print
	   errors and returns a value of type result and a stream containing
	   the unused tokens
	 *)

	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end

(* signature ARG_PARSER is the signature that will be matched by parsers whose
    lexer takes an additional argument.
*)

signature ARG_PARSER = 
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	type arg
	type lexarg
	type pos
	type result
	type svalue

	val makeLexer : (int -> string) -> lexarg ->
			 (svalue,pos) Token.token Stream.stream
	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end;

functor Stream(): STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
	    let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;

functor LrTable(): LR_TABLE = 
    struct
        open Array (*List*) (*NICK*)
	infix 9 sub
	datatype ('a,'b) pairlist = EMPTY
				  | PAIR of 'a * 'b * ('a,'b) pairlist
	datatype term = T of int
	datatype nonterm = NT of int
	datatype state = STATE of int
	datatype action = SHIFT of state
			| REDUCE of int (* rulenum from grammar *)
			| ACCEPT
			| ERROR
	exception Goto of state * nonterm
	type table = {states: int, rules : int,initialState: state,
		      action: ((term,action) pairlist * action) Array.Array,
		      goto :  (nonterm,state) pairlist Array.Array}
	val numStates = fn ({states,...} : table) => states
	val numRules = fn ({rules,...} : table) => rules
	val describeActions =
	   fn ({action,...} : table) => 
	           fn (STATE s) => action sub s
	val describeGoto =
	   fn ({goto,...} : table) =>
	           fn (STATE s) => goto sub s
	fun findTerm (T term,row,default) =
	    let fun find (PAIR (T key,data,r)) =
		       if key < term then find r
		       else if key=term then data
		       else default
		   | find EMPTY = default
	    in find row
	    end
	fun findNonterm (NT nt,row) =
	    let fun find (PAIR (NT key,data,r)) =
		       if key < nt then find r
		       else if key=nt then Some data
		       else None
		   | find EMPTY = None
	    in find row
	    end
	val action = fn ({action,...} : table) =>
		fn (STATE state,term) =>
		  let val (row,default) = action sub state
		  in findTerm(term,row,default)
		  end
	val goto = fn ({goto,...} : table) =>
			fn (a as (STATE state,nonterm)) =>
			  case findNonterm(nonterm,goto sub state)
			  of Some state => state
			   | None => raise (Goto a)
	val initialState = fn ({initialState,...} : table) => initialState
	val mkLrTable = fn {actions,gotos,initialState,numStates,numRules} =>
	     ({action=actions,goto=gotos,
	       states=numStates,
	       rules=numRules,
               initialState=initialState} : table)
  end;

(* functor Join creates a user parser by putting together a Lexer structure,
   an LrValues structure, and a polymorphic parser structure.  Note that
   the Lexer and LrValues structure must share the type pos (i.e. the type
   of line numbers), the type svalues for semantic values, and the type
   of tokens.
*)

functor Join(structure Lex : LEXER
	     structure ParserData: PARSER_DATA
	     structure LrParser : LR_PARSER
	     sharing ParserData.LrTable = LrParser.LrTable
	     sharing ParserData.Token = LrParser.Token
	     sharing type Lex.UserDeclarations.svalue = ParserData.svalue
	     sharing type Lex.UserDeclarations.pos = ParserData.pos
	     sharing type Lex.UserDeclarations.token = ParserData.Token.token)
		 : PARSER =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream
 
    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue
    val makeLexer = LrParser.Stream.streamify o Lex.makeLexer
    val parse = fn (lookahead,lexer,error,arg) =>
	(fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
	        lexer=lexer,
		lookahead=lookahead,
		saction = ParserData.Actions.actions,
		arg=arg,
		void= ParserData.Actions.void,
	        ec = {is_keyword = ParserData.EC.is_keyword,
		      noShift = ParserData.EC.noShift,
		      preferred_subst = ParserData.EC.preferred_subst,
		      preferred_insert= ParserData.EC.preferred_insert,
		      errtermvalue = ParserData.EC.errtermvalue,
		      error=error,
		      showTerminal = ParserData.EC.showTerminal,
		      terms = ParserData.EC.terms}}
      )
     val sameToken = Token.sameToken
end

(* functor JoinWithArg creates a variant of the parser structure produced 
   above.  In this case, the makeLexer take an additional argument before
   yielding a value of type unit -> (svalue,pos) token
 *)

functor JoinWithArg(structure Lex : ARG_LEXER
	     structure ParserData: PARSER_DATA
	     structure LrParser : LR_PARSER
	     sharing ParserData.LrTable = LrParser.LrTable
	     sharing ParserData.Token = LrParser.Token
	     sharing type Lex.UserDeclarations.svalue = ParserData.svalue
	     sharing type Lex.UserDeclarations.pos = ParserData.pos
	     sharing type Lex.UserDeclarations.token = ParserData.Token.token)
		 : ARG_PARSER  =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream

    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type lexarg = Lex.UserDeclarations.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue

    val makeLexer = fn s => fn arg =>
		 LrParser.Stream.streamify (Lex.makeLexer s arg)
    val parse = fn (lookahead,lexer,error,arg) =>
	(fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
	        lexer=lexer,
		lookahead=lookahead,
		saction = ParserData.Actions.actions,
		arg=arg,
		void= ParserData.Actions.void,
	        ec = {is_keyword = ParserData.EC.is_keyword,
		      noShift = ParserData.EC.noShift,
		      preferred_subst = ParserData.EC.preferred_subst,
		      preferred_insert= ParserData.EC.preferred_insert,
		      errtermvalue = ParserData.EC.errtermvalue,
		      error=error,
		      showTerminal = ParserData.EC.showTerminal,
		      terms = ParserData.EC.terms}}
      )
    val sameToken = Token.sameToken
end;

(* parser.sml:  This is a parser driver for LR tables with an error-recovery
   routine added to it.  The routine used is described in detail in this
   article:

	'A Practical Method for LR and LL Syntactic Error Diagnosis and
	 Recovery', by M. Burke and G. Fisher, ACM Transactions on
	 Programming Langauges and Systems, Vol. 9, No. 2, April 1987,
	 pp. 164-197.

    This program is an implementation is the partial, deferred method discussed
    in the article.  The algorithm and data structures used in the program
    are described below.  

    This program assumes that all semantic actions are delayed.  A semantic
    action should produce a function from unit -> value instead of producing the
    normal value.  The parser returns the semantic value on the top of the
    stack when accept is encountered.  The user can deconstruct this value
    and apply the unit -> value function in it to get the answer.

    It also assumes that the lexer is a lazy stream.

    Data Structures:
    ----------------
	
	* The parser:

	   The state stack has the type

		 (state * (semantic value * line # * line #)) list

	   The parser keeps a queue of (state stack * lexer pair).  A lexer pair
	 consists of a terminal * value pair and a lexer.  This allows the 
	 parser to reconstruct the states for terminals to the left of a
	 syntax error, and attempt to make error corrections there.

	   The queue consists of a pair of lists (x,y).  New additions to
	 the queue are cons'ed onto y.  The first element of x is the top
	 of the queue.  If x is nil, then y is reversed and used
	 in place of x.

    Algorithm:
    ----------

	* The steady-state parser:  

	    This parser keeps the length of the queue of state stacks at
	a steady state by always removing an element from the front when
	another element is placed on the end.

	    It has these arguments:

	   stack: current stack
	   queue: value of the queue
	   lexPair ((terminal,value),lex stream)

	When SHIFT is encountered, the state to shift to and the value are
	are pushed onto the state stack.  The state stack and lexPair are
	placed on the queue.  The front element of the queue is removed.

	When REDUCTION is encountered, the rule is applied to the current
	stack to yield a triple (nonterm,value,new stack).  A new
	stack is formed by adding (goto(top state of stack,nonterm),value)
	to the stack.

	When ACCEPT is encountered, the top value from the stack and the
	lexer are returned.

	When an ERROR is encountered, fixError is called.  FixError
	takes the arguments to the parser, fixes the error if possible and
        returns a new set of arguments.

	* The distance-parser:

	This parser includes an additional argument distance.  It pushes
	elements on the queue until it has parsed distance tokens, or an
	ACCEPT or ERROR occurs.  It returns a stack, lexer, the number of
	tokens left unparsed, a queue, and an action option.
*)

signature FIFO = 
  sig type 'a queue
      val empty : 'a queue
      exception Empty
      val get : 'a queue -> 'a * 'a queue
      val put : 'a * 'a queue -> 'a queue
  end;

functor ParserGen(structure LrTable : LR_TABLE
		  structure Stream : STREAM) : LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Stream = Stream

      structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
	    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	    val sameToken = fn (TOKEN(t,_),TOKEN(t',_)) => t=t'
        end

      open LrTable
      open Token

      val DEBUG1 = false
      val DEBUG2 = false
      exception ParseError
      exception ParseImpossible of int

      val println = fn s => Outstream.write std_out (s ^ "\n")

      structure (*abstraction*) Fifo : FIFO =
        struct
	  type 'a queue = ('a list * 'a list)
	  val empty = (nil,nil)
	  exception Empty
	  fun get(a::x, y) = (a, (x,y))
	    | get(nil, nil) = raise Empty
	    | get(nil, y) = get(rev y, nil)
 	  fun put(a,(x,y)) = (x,a::y)
        end

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list
      type ('a,'b) lexv = ('a,'b) token
      type ('a,'b) lexpair = ('a,'b) lexv * (('a,'b) lexv Stream.stream)
      type ('a,'b) distanceParse =
		 ('a,'b) lexpair *
		 ('a,'b) stack * 
		 (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		 int ->
		   ('a,'b) lexpair *
		   ('a,'b) stack * 
		   (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		   int *
		   action Option

      type ('a,'b) ecRecord =
	 {is_keyword : term -> bool,
          preferred_subst : term -> term list,
	  preferred_insert : term -> bool,
	  error : string * 'b * 'b -> unit,
	  errtermvalue : term -> 'a,
	  terms : term list,
	  showTerminal : term -> string,
	  noShift : term -> bool}

      local 
	 val showState = fn (STATE s) => "STATE " ^ (Int.string s)
      in
        fun printStack(stack: ('a,'b) stack, n: int) =
         case stack
           of (state,_) :: rest =>
                 (Outstream.write std_out ("\t" ^ Int.string n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1))
            | nil => ()
                
        fun prAction showTerminal
		 (stack as (state,_) :: _, next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              Outstream.write std_out ("       state="
                         ^ showState state	
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT state => println ("SHIFT " ^ (showState state))
                 | REDUCE i => println ("REDUCE " ^ (Int.string i))
                 | ERROR => println "ERROR"
		 | ACCEPT => println "ACCEPT")
        | prAction _ (_,_,action) = ()
     end

    (* ssParse: parser which maintains the queue of (state * lexvalues) in a
	steady-state.  It takes a table, showTerminal function, saction
	function, and fixError function.  It parses until an ACCEPT is
	encountered, or an exception is raised.  When an error is encountered,
	fixError is called with the arguments of parseStep (lexv,stack,and
	queue).  It returns the lexv, and a new stack and queue adjusted so
	that the lexv can be parsed *)
	
    val ssParse =
      fn (table,showTerminal,saction,fixError,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(args as
			 (lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				      ),
			  stack as (state,_) :: _,
			  queue)) =
	      let val nextAction = action (state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		      val (_,newQueue) =Fifo.get(Fifo.put((newStack,newLexPair),
							    queue))
		  in parseStep(newLexPair,(s,value)::stack,newQueue)
		  end
		 | REDUCE i =>
		     (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		          parseStep(lexPair,(goto(state,nonterm),value)::stack,
				    queue)
		       | _ => raise (ParseImpossible 197))
		 | ERROR => parseStep(fixError args)
		 | ACCEPT => 
			(case stack
			 of (_,(topvalue,_,_)) :: _ =>
				let val (token,restLexer) = lexPair
				in (topvalue,Stream.cons(token,restLexer))
				end
			  | _ => raise (ParseImpossible 202))
	      end
	    | parseStep _ = raise (ParseImpossible 204)
	in parseStep
	end

    (*  distanceParse: parse until n tokens are shifted, or accept or
	error are encountered.  Takes a table, showTerminal function, and
	semantic action function.  Returns a parser which takes a lexPair
	(lex result * lexer), a state stack, a queue, and a distance
	(must be > 0) to parse.  The parser returns a new lex-value, a stack
	with the nth token shifted on top, a queue, a distance, and action
	option. *)

    val distanceParse =
      fn (table,showTerminal,saction,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(lexPair,stack,queue,0) = (lexPair,stack,queue,0,None)
	      | parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				     ),
			  stack as (state,_) :: _,
			  queue,distance) =
	      let val nextAction = action(state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		  in parseStep(newLexPair,(s,value)::stack,
			       Fifo.put((newStack,newLexPair),queue),distance-1)
		  end
		 | REDUCE i =>
		    (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		         parseStep(lexPair,(goto(state,nonterm),value)::stack,
				 queue,distance)
		      | _ => raise (ParseImpossible 240))
		 | ERROR => (lexPair,stack,queue,distance,Some nextAction)
		 | ACCEPT => (lexPair,stack,queue,distance,Some nextAction)
	      end
	   | parseStep _ = raise (ParseImpossible 242)
	in parseStep : ('_a,'_b) distanceParse 
	end

(* mkFixError: function to create fixError function which adjusts parser state
   so that parse may continue in the presence of an error *)

val mkFixError = fn ({is_keyword,preferred_subst,terms,errtermvalue,
		      preferred_insert,noShift,
		      showTerminal,error,...} : ('_a,'_b) ecRecord,
		      distanceParse : ('_a,'_b) distanceParse,
		      minAdvance,maxAdvance) =>
  let fun FixError(lexv as (TOKEN (term,value as (_,leftPos,_)),_),
		   stack,queue) =
    let val lexVList = map (fn t => TOKEN (t,(errtermvalue t,leftPos,leftPos)))
		       terms
	val _ = if DEBUG2 then
			error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos)
		else ()

	val minDelta = 3

	(* pull all the state * lexv elements from the queue *)

	val stateList = 
	   let fun f q = let val (elem,newQueue) = Fifo.get q
			 in elem :: (f newQueue)
			 end handle Fifo.Empty => nil
	   in f queue
	   end

	(* now number elements of stateList, giving distance from
	   error token *)

	val (_,numStateList) = List.foldR (fn a => fn (num,r) => (num+1,(a,num)::r))
				(0,nil) stateList

	(* Represent the set of potential changes as a linked list.

	   Values of datatype Change hold information about a potential change.

	   oper = oper to be applied
	   pos = the # of the element in stateList that would be altered.
	   distance = the number of tokens beyond the error token which the
	     change allows us to parse.
	   new = new terminal * value pair at that point
	   orig = original terminal * value pair at the point being changed.
	 *)

	datatype oper = INSERT | DELETE  | SUBST
	datatype ('a,'b) change = CHANGE of
	   {oper : oper, pos : int, distance : int,
	    new : ('a,'b) lexv, orig : ('a,'b) lexv}

	val operToString = 
	       fn INSERT => "INSERT "
		| SUBST  => "SUBST "
		| DELETE => "DELETE "

	 val printChange = fn c =>
	  let val CHANGE {oper,distance,new=TOKEN (t,_),
			  orig=TOKEN (t',_),pos,...} = c
	  in (Outstream.write std_out  ("{distance= " ^ (Int.string distance));
	      Outstream.write std_out  (",orig = " ^ (showTerminal t'));
	      Outstream.write std_out  (",new = " ^ (showTerminal t));
	      Outstream.write std_out  (",oper= " ^ (operToString oper));
	      Outstream.write std_out  (",pos= " ^ (Int.string pos));
	      println "}")
	  end

	val printChangeList = List.apply printChange

(* parse: given a lexPair, a stack, and the distance from the error
   token, return the distance past the error token that we are able to parse.*)

	fun parse (lexPair,stack,queuePos : int) =
	    let val (_,_,_,distance,action) =
		  distanceParse(lexPair,stack,Fifo.empty,queuePos+maxAdvance+1)
	    in maxAdvance - distance - 1
	    end

(* foldStateList: accumulates results while scanning numStateList *)


	fun foldStateList f start = List.foldR (General.curry f) start numStateList

(* foldLexVList: accumulates results while scanning lexVList *)

	fun foldLexVList f start = List.foldR (General.curry f) start lexVList

(* deleteFold: function which accumulates results of deleting the
   current terminal.  Does not delete the current terminal if that terminal
   cannot be shifted *)

	val deleteFold =
		fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),
			queuePos),r) =>
		 if noShift term then r
		 else
		   let val newLexPair as (new,_) = Stream.get lexer
		       val distance = parse(newLexPair,stack,queuePos-1)
		   in if distance >= minAdvance then
			CHANGE {pos=queuePos,distance=distance,orig=orig,
				new=new,oper=DELETE} :: r
		      else r
		   end


(* insertFold: accumulate results of trying to insert tokens before
   the current terminal *)

	val insertFold =
	   fn (((stack,lexPair as (orig,lexer)),queuePos),r) =>
	    let val lexer = Stream.cons lexPair
	    in foldLexVList (fn (newLexV,r) =>
		let val distance = parse((newLexV,lexer),stack,queuePos+1)
		in if distance >= minAdvance
			 then CHANGE{pos=queuePos,distance=distance,orig=orig,
					new=newLexV,oper=INSERT} :: r
			 else r
		end) r
	    end

(* substFold: accumulate results of deleting the current terminal
   and then trying to insert tokens *)

	val substFold =
	    fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),queuePos),
		r) =>
	      if noShift term then r
	      else
		  foldLexVList (fn (newLexV,r) =>
		   let val distance = parse((newLexV,lexer),stack,queuePos)
		   in if distance >= minAdvance then
			   CHANGE{pos=queuePos,distance=distance,orig=orig,
				  new=newLexV,oper=SUBST} :: r
		     else r
		   end) r

	val changes = (foldStateList insertFold nil) @
			  (foldStateList substFold nil) @
				(foldStateList deleteFold nil)

	val findMaxDist = fn l => 
	  List.foldR (fn CHANGE {distance,...} => fn high => Int.max distance high) 0 l

(* maxDist: max distance past error taken that we could parse *)

	val maxDist = findMaxDist changes

(* sieve: keep only the elements of a list for which pred is true *)

	val sieve = fn pred => fn l => 
	  List.foldR (fn elem => fn rest => if pred elem then elem::rest else rest) l nil

(* remove changes which did not parse maxDist tokens past the error token *)

	val changes = sieve (fn CHANGE{distance=a,...} => a = maxDist) changes

(* Find preferred elements *)

        val preferredInsertChanges =
		sieve (fn CHANGE {new=TOKEN (term,_),oper=INSERT,...} => 
				 preferred_insert term
		        | _ => false) changes

        val preferredSubstChanges =
		sieve
		    (fn CHANGE {new=TOKEN(t,_),orig=TOKEN (t',_),
				oper=SUBST,...} =>
			  List.exists (fn a => a =t) (preferred_subst t')
		      | _ => false) changes

        val _ = if DEBUG2 then
	    (println "preferred insert:";
	     printChangeList preferredInsertChanges;
	     println "preferred subst:";
	     printChangeList preferredSubstChanges
	    ) else ()

(* Remove keywords which don't meet the long parse check
   (minAdvance+minDelta) *)

         val changes =
	    sieve (fn CHANGE {new=TOKEN (term,_),distance,...} =>
		(not (is_keyword term) orelse distance >= minAdvance+minDelta))
			changes


         val changes =
	       preferredInsertChanges @ (preferredSubstChanges @ changes)

         in case changes 
	     of (l as _ :: _) =>
	        let fun print_msg (CHANGE {new=TOKEN (term,_),oper,
					   orig=TOKEN (t',(_,leftPos,rightPos)),
					   ...}) =
		     let val s = 
		       case oper
			 of DELETE => "deleting " ^ (showTerminal t')
			  | INSERT => "inserting " ^ (showTerminal term)
		          | SUBST => "replacing " ^ (showTerminal t') ^
				   " with " ^ (showTerminal term)
		     in error ("syntax error: " ^ s,leftPos,rightPos)
		     end
		   
		   exception Hd
		   fun hd(x :: _) = x
		     | hd _ = raise Hd

		   val a = 
		     (if List.size l > 1 andalso DEBUG2 then
			(println "multiple fixes possible; could fix it by:";
		 	 map print_msg l;
		 	 println "chosen correction:")
		      else ();
		      print_msg (hd l); (hd l))

		    (* findNth: find nth queue entry from the error
		       entry.  Returns the Nth queue entry and the  portion of
		       the queue from the beginning to the nth-1 entry.  The
		       error entry is at the end of the queue.

			Examples:

			queue = a b c d e
		        findNth 0 = (e,a b c d)
			findNth 1 =  (d,a b c)
		    *)

		    val findNth = fn n =>
		     let fun f (h::t,0) = (h,rev t)
			   | f (h::t,n) = f(t,n-1)
			   | f (nil,_) = let exception FindNth
					   in raise FindNth
					   end
		     in f (rev stateList,n)
		     end
		
		    val CHANGE {pos,oper,new=TOKEN (term,(value,_,_)),...} = a
		    val (last,queueFront) = findNth pos
		    val (stack,lexPair as (orig,lexer)) = last
		    val TOKEN (_,(_,leftPos,rightPos)) = orig
 		    val newLexV = TOKEN (term,(value,leftPos,rightPos))

		    val newLexPair =
			case oper
			of DELETE => Stream.get lexer
			 | SUBST => (newLexV,lexer)
			 | INSERT => (newLexV,Stream.cons lexPair)

		    val restQueue = 
		     Fifo.put((stack,newLexPair),
			      List.foldL (General.curry Fifo.put) Fifo.empty queueFront)

	 	   val (lexPair,stack,queue,_,_) =
			distanceParse(newLexPair,stack,restQueue,pos)

	      in (lexPair,stack,queue)
	      end
	  | nil => (error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos); raise ParseError)
	end
     in FixError
     end

   val parse = fn {arg,table,lexer,saction,void,lookahead,
		   ec=ec as {showTerminal,...} : ('_a,'_b) ecRecord} =>
	let val distance = 15   (* defer distance tokens *)
	    val minAdvance = 1  (* must parse at least 1 token past error *)
	    val maxAdvance = Int.max lookahead 0 (* max distance for parse check *)
	    val lexPair = Stream.get lexer
	    val (TOKEN (_,(_,leftPos,_)),_) = lexPair
	    val startStack = [(initialState table,(void,leftPos,leftPos))]
	    val startQueue = Fifo.put((startStack,lexPair),Fifo.empty)
	    val distanceParse = distanceParse(table,showTerminal,saction,arg)
	    val fixError = mkFixError(ec,distanceParse,minAdvance,maxAdvance)
	    val ssParse = ssParse(table,showTerminal,saction,fixError,arg)
	    fun loop (lexPair,stack,queue,_,Some ACCEPT) =
		   ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,0,_) = ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,distance,Some ERROR) =
		 let val (lexPair,stack,queue) = fixError(lexPair,stack,queue)
		 in loop (distanceParse(lexPair,stack,queue,distance))
		 end
	      | loop _ = let exception ParseInternal
			 in raise ParseInternal
			 end
	in loop (distanceParse(lexPair,startStack,startQueue,distance))
	end
   end;
