functor MlbProject () : MLB_PROJECT = 
    struct
	type bid = string
	fun bid s = s
	fun pp_bid s = s

	datatype bexp = SEQbexp of bexp * bexp
                      | LETbexp of bdec * bexp
                      | FILEbexp of string        (* file.sml *)
                      | BIDbexp of bid
	              | EMPTYbexp 

             and bdec = SEQbdec of bdec * bdec
                      | LOCALbdec of bdec * bdec
                      | BASbdec of bid * bexp
                      | OPENbdec of bexp
		      | USEbdec of string         (* file.mlb *)
	              | EMPTYbdec 

	fun error (s : string) = 
	    (print ("\nError: " ^ s ^ ".\n\n"); raise Fail "MlbProject.error")

	fun warn (s : string) = print ("\nWarning: " ^ s ^ ".\n\n")

	fun quot s = "'" ^ s ^ "'"

	fun has_ext(s: string,ext) = 
	    case OS.Path.ext s of 
		SOME ext' => ext = ext'
	      | NONE => false
		    
	fun member s l = let fun m [] = false
			       | m (x::xs) = x = s orelse m xs
			 in m l
			 end
		     
	fun drop_comments (file:string) (l: char list) : char list =
	  let fun loop(n, #"(" :: #"*" :: rest ) = loop(n+1, rest)
		| loop(n, #"*" :: #")" :: rest ) = loop(n-1, if n=1 then #" "::rest else rest)
		| loop(0, ch ::rest) = ch :: loop (0,rest)
		| loop(0, []) = []
		| loop(n, ch ::rest) = loop(n,rest)
		| loop(n, []) = error ("Unclosed comment in project " ^ quot(file))
	  in loop(0,l)
	  end

	fun is_symbol #"(" = true
	  | is_symbol #")" = true
	  | is_symbol _ = false

	fun lex chs : string list =
	    let 
		fun lex_symbol (c::chs) =
		    if is_symbol c then SOME (c,chs) else NONE
		  | lex_symbol nil = NONE

		fun lex_whitesp (all as c::rest) = 
		    if Char.isSpace c then lex_whitesp rest else all 
		  | lex_whitesp [] = []

		fun lex_string (c::rest, acc) = 
		    if is_symbol c then (implode(rev acc), c::rest)
		    else if Char.isSpace c then (implode(rev acc), rest)
			 else lex_string (rest, c::acc)
		  | lex_string ([], acc) = (implode(rev acc), [])

		fun lex0 (chs : char list, acc) : string list =
		    case lex_whitesp chs of 
			[] => rev acc
		      | chs => lex0 (case lex_symbol chs of 
					 SOME (c,chs) => (chs, Char.toString c :: acc)
				       | NONE => let val (s, chs) = lex_string(chs,[])
						 in (chs, s::acc) 
						 end)
	    in lex0(chs,[])
	    end

	fun is_symb s =
	    case explode s of
		[c] => is_symbol c
	      | _ => false

	fun is_mlbfile s =
	    has_ext (s,"mlb")

	fun is_smlfile s = 
	    has_ext(s,"sml") orelse has_ext(s,"sig")

	fun is_keyword s = 
	    case s of
		"open" => true
	      | "let" => true
	      | "in" => true
	      | "end" => true
	      | "bas" => true
	      | _ => false

	fun is_id s = 
	    case explode s of
		c::cs => Char.isAlpha c 
		    andalso List.all (fn c => Char.isAlphaNum c 
				      orelse c = #"_"
				      orelse c = #"'") cs
	      | _ => false

	fun is_bid s = 
	    is_id s andalso not (is_keyword s)

	fun print_ss ss = (print "Tokens: "; app (fn s => print (s ^ " ")) ss; print "\n")
			
        fun parse_error mlbfile msg = error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg)
        fun parse_error1 mlbfile (msg, rest: string list) = 
          case rest of 
            [] => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached end of file)")
          | s::_ => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached " ^ quot s ^ ")")



	fun parse_bexp_more mlbfile (bexp,ss) =
	    case parse_bexp_opt mlbfile ss of
		SOME(bexp',ss) => SOME(SEQbexp(bexp,bexp'),ss)
	      | NONE => SOME(bexp,ss)

	and parse_bdec_more mlbfile (bdec,ss) =
	    case parse_bdec_opt mlbfile ss of
		SOME(bdec',ss) => SOME(SEQbdec(bdec,bdec'),ss)
	      | NONE => SOME(bdec,ss)

	and parse_bexp_opt mlbfile (ss:string list) : (bexp * string list) option =	    
	    case ss of
		nil => NONE
	      | "let" :: ss => 
		    let 
			fun parse_rest'(bdec,bexp,ss) =
			    case ss of 
				"end" :: ss => parse_bexp_more mlbfile (LETbexp(bdec,bexp),ss)
			      | _ => parse_error1 mlbfile ("I expect an 'end'", ss)
			fun parse_rest(bdec,ss) =
			    case ss of 
				"in" :: ss => 
				    (case parse_bexp_opt mlbfile ss of 
					 NONE => parse_rest'(bdec,EMPTYbexp,ss)
				       | SOME(bexp,ss) => parse_rest'(bdec,bexp,ss))
			      | _ => parse_error1 mlbfile ("I expect an 'in'", ss)
		    in case parse_bdec_opt mlbfile ss of 
			NONE => parse_rest(EMPTYbdec,ss)
		      | SOME(bdec,ss) => parse_rest(bdec,ss)
		    end
	      | "(" :: ss => 
		    let 
			fun parse_rest(bexp,ss) =
			    case ss of
				")" :: ss => parse_bexp_more mlbfile (bexp,ss)
			      | _ => parse_error1 mlbfile ("I expect an ')'", ss)
		    in
			case parse_bexp_opt mlbfile ss of 
			    NONE => parse_rest(EMPTYbexp,ss)
			  | SOME(bexp,ss) => parse_rest(bexp,ss)
		    end
	      | s :: ss => 
		    if is_smlfile s then parse_bexp_more mlbfile (FILEbexp s,ss)
		    else if is_bid s then parse_bexp_more mlbfile (BIDbexp s,ss)
			 else NONE

	and parse_bdec_opt mlbfile ss =
	    case ss of
		"local" :: ss => 
		    let 
			fun parse_rest'(bdec,bdec',ss) =
			    case ss of 
				"end" :: ss => parse_bdec_more mlbfile (LOCALbdec(bdec,bdec'),ss)
			      | _ => parse_error1 mlbfile ("I expect an 'end'", ss)
			fun parse_rest(bdec,ss) =
			    case ss of 
				"in" :: ss => 
				    (case parse_bdec_opt mlbfile ss of 
					 NONE => parse_rest'(bdec,EMPTYbdec,ss)
				       | SOME(bdec',ss) => parse_rest'(bdec,bdec',ss))
			      | _ => parse_error1 mlbfile ("I expect an 'in'", ss)
		    in print "Entering local\n";
			print_ss ss;
			case parse_bdec_opt mlbfile ss of 
			NONE => parse_rest(EMPTYbdec,ss)
		      | SOME(bdec,ss) => (print "ok:"; print_ss ss; parse_rest(bdec,ss))
		    end
	      | "bas" :: ss =>
		    (case ss of
			 nil => parse_error1 mlbfile ("I expect a basis identifier", ss)
		       | bid :: ss => 
			     (case ss of
				  "=" :: ss => 
				      (case parse_bexp_opt mlbfile ss of
					   SOME (bexp,ss) => parse_bdec_more mlbfile (BASbdec(bid,bexp),ss)
					 | NONE => parse_error1 mlbfile ("I expect a basis expression", ss))
				| _ => parse_error1 mlbfile ("I expect an '='", ss)))
	      | "open" :: ss => 	
		   (case ss of
			f :: ss' => 
			    if is_mlbfile f then parse_bdec_more mlbfile (USEbdec f,ss')
			    else
				(case parse_bexp_opt mlbfile ss of
				     SOME(bexp,ss) => parse_bdec_more mlbfile (OPENbdec bexp,ss)
				   | NONE => parse_error1 mlbfile ("I expect a basis expression", ss))
		      | nil => parse_error1 mlbfile ("I expect a basis expression or a basis file", ss))
	      | _ => NONE

        fun fromFile (filename:string) =
	    let val is = TextIO.openIn filename 
		(*val _ = TextIO.output(TextIO.stdOut, "opened " ^filename^"\n")*)
		val s = TextIO.inputAll is handle E => (TextIO.closeIn is; raise E)
	    in TextIO.closeIn is; s
	    end

	fun parse (mlbfile: string) : bdec =
	    if not (has_ext(mlbfile, "mlb")) then 
		error ("The basis file " ^ quot mlbfile ^ " does not have extension 'mlb'")	    
	    else
		let val ss = (lex o (drop_comments mlbfile) o explode o fromFile) mlbfile
		    val _ = print_ss ss
		in  case parse_bdec_opt mlbfile ss of
		    SOME (bdec,nil) => bdec
		  | SOME (bdec,ss) => parse_error1 mlbfile ("I expect a basis declaration", ss)
		  | NONE => parse_error1 mlbfile ("I expect a basis declaration", ss)
		end
	    handle IO.Io {name=io_s,...} => error ("The basis file " ^ quot mlbfile ^ " cannot be opened")


	(* Support for writing dependency information to disk in .d-files. *)

	structure DepEnv = 
	    struct
		type env = (string * string list) list
		fun lookup env bid : string list option =
		    let fun look nil = NONE
			  | look ((x,xs)::rest) = if bid = x then SOME xs
						  else look rest
		    in look env
		    end
		fun plus (e1, e2) = e2 @ e1
		fun add env bid xs = (bid,xs)::env
		val empty = nil
	    end

	type L = string list
	type D = DepEnv.env
	    
	val op + = DepEnv.plus

	fun dep_bdec (L:L) (D:D) bdec : D * L =
	    case bdec of
		SEQbdec (bdec1,bdec2) =>
		    let val (D1,L1) = dep_bdec L D bdec1
			val (D2,L2) = dep_bdec (L @ L1) (D + D1) bdec2
		    in (D1 + D2, L1 @ L2)
		    end
	      | LOCALbdec (bdec1,bdec2) =>
		    let val (D1,L1) = dep_bdec L D bdec1
			val (D2,L2) = dep_bdec (L @ L1) (D + D1) bdec2
		    in (D2, L2)
		    end
	      | BASbdec (bid, bexp) =>
		    let val L' = dep_bexp L D bexp
		    in (DepEnv.add DepEnv.empty bid L', nil)
		    end
	      | OPENbdec bexp =>
		    let val L' = dep_bexp L D bexp
		    in (DepEnv.empty, L')
		    end
	      | USEbdec mlbfile =>
		    dep_bdec_file mlbfile
	      | EMPTYbdec => 
		    (DepEnv.empty,nil)

	and dep_bexp (L:L) (D:D) bexp : L =
	    case bexp of
		SEQbexp (bexp1,bexp2) =>
		    let val L1 = dep_bexp L D bexp1
			val L2 = dep_bexp (L@L1) D bexp2
		    in L1 @ L2
		    end
	      | LETbexp (bdec,bexp) =>
		    let val (D1,L1) = dep_bdec L D bdec
			val L2 = dep_bexp (L@L1) (D+D) bexp
		    in L2
		    end
	      | FILEbexp s =>
		    (let val os = TextIO.openOut ("PM/" ^ s ^ ".d")       (* what about general paths *)
		     in (  app (fn u => TextIO.output(os,u^" ")) L
			 ; TextIO.closeOut os
			 ; [s]
			 ) handle ? => (TextIO.closeOut os; raise ?)
		     end handle _ => error("Failed to read source file " ^ quot s))
	      | BIDbexp bid =>
		    (case DepEnv.lookup D bid of
			 SOME L => L
		       | NONE => error ("The basis identifier " ^ quot bid ^ " is undefined"))
	      | EMPTYbexp => nil

	and dep_bdec_file mlbfile : D * L =
	    let val bdec = parse mlbfile
	    in dep_bdec nil DepEnv.empty bdec
	    end

	fun dep (mlbfile : string) : unit =
	    (dep_bdec_file mlbfile; ())


	(* Support for finding the source files of a basis file *)
	fun srcs_bdec_file mlbs mlb =
	    if member mlb mlbs then (nil,mlbs)
	    else let val bdec = parse mlb
		     val (ss, mlbs) = srcs_bdec mlbs bdec
		 in (ss,mlb::mlbs)
		 end
	and srcs_bdec mlbs bdec =
	    case bdec of 
		SEQbdec (bdec1,bdec2) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec1
			val (ss2,mlbs) = srcs_bdec mlbs bdec2
		    in (ss1@ss2,mlbs)
		    end
	      | LOCALbdec (bdec1,bdec2) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec1
			val (ss2,mlbs) = srcs_bdec mlbs bdec2
		    in (ss1@ss2,mlbs)
		    end
	      | BASbdec (bid,bexp) => srcs_bexp mlbs bexp
	      | OPENbdec bexp => srcs_bexp mlbs bexp
	      | USEbdec mlb => srcs_bdec_file mlbs mlb
	      | EMPTYbdec => (nil,mlbs)
		    
	and srcs_bexp mlbs bexp =
	    case bexp of
		SEQbexp (bexp1,bexp2) =>
		    let val (ss1,mlbs) = srcs_bexp mlbs bexp1
			val (ss2,mlbs) = srcs_bexp mlbs bexp2
		    in (ss1@ss2,mlbs)
		    end
	      | LETbexp (bdec,bexp) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec
			val (ss2,mlbs) = srcs_bexp mlbs bexp
		    in (ss1@ss2,mlbs)
		    end
	      | FILEbexp s => ([s],mlbs)
	      | BIDbexp bid => (nil,mlbs)
	      | EMPTYbexp => (nil,mlbs)
	
	fun sources (mlbfile : string) : string list =
	    #1 (srcs_bdec_file nil mlbfile)
    
    end
	