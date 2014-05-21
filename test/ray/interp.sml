(* interp.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

structure Interp =
  struct

    local
      val exit = OS.Process.exit
      fun ordof(s, i) = Char.ord(String.sub(s, i))
      exception NotAChar
      exception NotAReal
      fun fromStr x = 
        (case Char.fromString x
          of SOME c => c
           | NONE => raise NotAChar)

     fun strToReal s = 
      (case Real.fromString s
        of SOME r => r
        | _ => raise NotAReal)

    fun intToReal x = 
     (strToReal ((Int.toString x) ^ ".0"))


      val explode = (fn x => map Char.toString (explode x))
      val implode = (fn x => implode (map fromStr x))

      open Objects
      val dict = ref ([] : {key : string, value : object} list)
      fun dictInsert (NAME key, value) = let
	    fun find [] = [{key=key, value=value}]
	      | find (x::r) = if (key = #key x)
		  then {key=key, value=value}::r
		  else x :: (find r)
	    in
	      dict := find(!dict)
	    end
	| dictInsert _ = raise Fail "dictInsert"
      fun prObj outStrm obj = let
	    fun printf args = TextIO.output(outStrm, implode args)
	    fun pr (NUMBER n) = printf["  ", Real.toString n, "\n"]
	      | pr (NAME s) = printf["  ",  s, "\n"]
	      | pr (LITERAL s) = printf["  ", s, "\n"]
	      | pr (LIST l) = app pr l
	      | pr MARK = printf["  MARK\n"]
	      | pr (OPERATOR _) = printf["  <operator>\n"]
	      | pr TOP = printf["  TOP OF STACK\n"]
	      | pr _ = printf["  <object>\n"]
	    in
	      pr obj
	    end
    in

    exception Stop

    fun error opName stk = let
	  fun prStk ([], _) = ()
	    | prStk (_, 0) = ()
	    | prStk (obj::r, i) = (prObj TextIO.stdErr obj; prStk(r, i-1))
	  in
	    TextIO.output(TextIO.stdErr, "ERROR: "^opName^"\n");
	    prStk (stk, 10);
	    raise (Fail opName)
	  end

    fun installOperator (name, rator) =
	  dictInsert (NAME name, OPERATOR rator)

    fun ps_def (v::k::r) = (dictInsert(k, v); r)
      | ps_def stk = error "ps_def" stk

    local
      fun binOp (f, opName) = let
	    fun g ((NUMBER arg1)::(NUMBER arg2)::r) =
		  NUMBER(f(arg2, arg1)) :: r
	      | g stk = error opName stk
	    in
	      g
	    end
    in
    val ps_add = binOp (op +, "add")
    val ps_sub = binOp (op -, "sub")
    val ps_mul = binOp (op *, "mul")
    val ps_div = binOp (op /, "div")
    end

    fun ps_rand stk = (NUMBER 0.5)::stk (** ??? **)

    fun ps_print (obj::r) = (prObj TextIO.stdOut obj; r)
      | ps_print stk = error "print" stk

    fun ps_dup (obj::r) = (obj::obj::r)
      | ps_dup stk = error "dup" stk

    fun ps_stop _ = raise Stop

  (* initialize dictionary and begin parsing input *)
    fun parse inStrm = let
	  fun getc () = case TextIO.input1 inStrm of NONE => ""
                               | SOME c => Char.toString c
	  fun peek () = case TextIO.lookahead inStrm
                         of SOME x => Char.toString x
                          | _ => ""
	(* parse one token from inStrm *)
	  fun toke deferred = let
		fun doChar "" = exit 0
		  | doChar "%" = let
		      fun lp "\n" = doChar(getc())
			| lp "" = exit 0
			| lp _ = lp(getc())
		      in
			lp(getc())
		      end
		  | doChar "{" = (MARK, deferred+1)
		  | doChar "}" = (UNMARK, deferred-1)
		  | doChar c = if Char.isSpace (fromStr c)
		      then doChar(getc())
		      else let
			fun lp buf = (case peek()
			       of "{" => buf
				| "}" => buf
				| "%" => buf
				| c => if Char.isSpace(fromStr c)
				    then buf
				    else (getc(); lp(c::buf))
			      (* end case *))
			val tok = implode (rev (lp [c]))
			val hd = ordof(tok, 0)
			in
			  if (hd = ord (#"/"))
			    then (LITERAL(substring(tok, 1, size tok - 1)), deferred)
			  else 
                            if ((Char.isDigit (chr hd)) orelse (hd = ord (#"-")))
			    then (NUMBER(strToReal(tok)), deferred)
			    else (NAME tok, deferred)
			end
		in
		  doChar(getc())
		end
	(* execute a token (if not deferred) *)
	  fun exec (UNMARK, stk, _) = let
		fun lp ([], _) = raise Fail "MARK"
		  | lp (MARK::r, l) = (LIST l)::r
		  | lp (x::r, l) = lp (r, x::l)
		  in
		    lp (stk, [])
		  end
	    | exec (OPERATOR f, stk, 0) = f stk
	    | exec (LIST l, stk, 0) = let
		fun execBody ([], stk) = stk
		  | execBody (obj::r, stk) = (exec(obj, stk, 0); execBody(r, stk))
		in
		  execBody (l, stk)
		end
	    | exec (NAME s, stk, 0) = let
		fun find [] = raise Fail "undefined name"
		  | find ({key, value}::r) = if (key = s) then value else find r
		in
		  exec (find (!dict), stk, 0)
		end
	    | exec (obj, stk, _) = obj::stk
	  fun lp (stk, level) = let
		val (obj, level) = toke level
		val stk = exec (obj, stk, level)
		in
		  lp (stk, level)
		end
	  in
	    installOperator ("add", ps_add);
	    installOperator ("def", ps_def);
	    installOperator ("div", ps_div);
	    installOperator ("dup", ps_dup);
	    installOperator ("mul", ps_mul);
	    installOperator ("print", ps_print);
	    installOperator ("rand", ps_rand);
	    installOperator ("stop", ps_stop);
	    installOperator ("sub", ps_sub);
	    (lp ([], 0)) handle Stop => ()
	  end (* parse *)

    end (* local *)

  end (* Interp *)
