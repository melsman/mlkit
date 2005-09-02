
signature TESTFILE =
  sig

    type opt = string
(*
    datatype opt = 
      NoBasisLib 
    | NoOptimiser 
    | CompareCompilerLogs
    | TimeExecutable 
    | TimeCompiler
    | ExpectCompileTimeError 
    | Profiling
    | GC
    | Tags
    | UncaughtException
*)      
    datatype entry = MLB of string * opt list
                   | SML of string * opt list
      
    val parse : string -> (string * entry list) option     (* returns NONE if parse error occurs *)
      (* the string holds the entire test file *)
  end


structure TestFile : TESTFILE =
  struct

    type opt = string
(*
    datatype opt = 
      NoBasisLib 
    | NoOptimiser
    | CompareCompilerLogs
    | TimeExecutable 
    | TimeCompiler
    | ExpectCompileTimeError 
    | Profiling
    | GC
    | Tags
    | UncaughtException
*)      
    datatype entry = MLB of string * opt list
                   | SML of string * opt list
      
    exception ParseFile of string
    fun drop_comments (l: char list) : char list =
      let fun loop(n, #"(" :: #"*" :: rest ) = loop(n+1, rest)
	    | loop(n, #"*" :: #")" :: rest ) = loop(n-1, if n=1 then #" "::rest else rest)
	    | loop(0, ch ::rest) = ch :: loop (0,rest) 
	    | loop(0, []) = []
	    | loop(n, ch ::rest) = loop(n,rest)
	    | loop(n, []) = raise ParseFile "Unclosed comment"
      in loop(0,l)
      end
    
    fun white c =
      case c
	of #"\n" => true
	 | #" " => true
	 | #"\t" => true
	 | c => not(Char.isPrint c)

    fun lex ([], NONE, acc) = rev acc
      | lex ([], SOME s, acc) = rev (implode (rev s)::acc)
      | lex (c::rest, SOME s, acc) = lex(if white c then (rest,NONE, implode (rev s) :: acc)
					 else (rest,SOME(c::s),acc))
      | lex (c::rest, NONE, acc) = lex(if white c then (rest,NONE,acc)
				       else (rest,SOME[c],acc))

    fun lexing l = lex(l,NONE,[])
  
    fun parse (s : string) : (string * entry list) option =
      let 
	(* an option is a sequence of characters not including white space or punktuation *)
	fun contains c s = CharVector.foldl (fn (e,b) => b orelse c = e) false s
	  
	fun read_opt [] = (NONE,[])
	  | read_opt (all as s::rest) = if contains #"." s then (NONE,all)
					else (SOME s, rest)
(*
	  case s
	    of "nobasislib" => (SOME NoBasisLib, rest)
	     | "nooptimiser" => (SOME NoOptimiser, rest)
	     | "ccl" => (SOME CompareCompilerLogs, rest)
	     | "tx" => (SOME TimeExecutable, rest)
	     | "tc" => (SOME TimeCompiler, rest)
	     | "ecte" => (SOME ExpectCompileTimeError, rest)
	     | "prof" => (SOME Profiling, rest)
	     | "gc" => (SOME GC, rest)
	     | "tags" => (SOME Tags, rest)
	     | "ue" => (SOME UncaughtException, rest)
	     | _ => (NONE, all)
*)
	fun read_opts (l, acc) =
	  case read_opt l
	    of (SOME opt, rest) => read_opts(rest,opt::acc)
	     | (NONE, rest) => (acc, rest)

	fun read_entry [] = NONE
	  | read_entry (all as s::rest) =
	  case OS.Path.ext s
	    of SOME "sml" => let val (opts,rest) = read_opts (rest,[])
			     in SOME(SML(s,opts),rest)
			     end
	     | SOME "sig" => let val (opts,rest) = read_opts (rest,[])
			     in SOME(SML(s,opts),rest)
			     end
	     | SOME "mlb" => let val (opts,rest) = read_opts (rest,[])
			    in SOME(MLB(s,opts),rest)
			    end
	     | SOME ext => raise ParseFile ("file name with unknown extension `" ^ ext ^ "'")
	     | NONE => raise ParseFile ("file name expected, but found `" ^ s ^ "'")

	fun read_entries(l,acc) =
	  case read_entry l
	    of (SOME(e,rest)) => read_entries(rest,e::acc)
	     | NONE => rev acc

	fun read l = read_entries(l,[])
	val is = TextIO.openIn s
	
      in let val file_string : string = (TextIO.inputAll) is before (TextIO.closeIn is)
	 in SOME(file_string, (read o lexing o drop_comments o explode) file_string)
	 end handle ParseFile s => (print ("Parse error: " ^ s ^ ".\n"); TextIO.closeIn is; NONE)
	          | IO.Io _ => (print("Error when trying to read file `" ^ s ^ "'.\n"); TextIO.closeIn is; NONE)
	          | _ => (print("Unknown error during parsing .\n"); TextIO.closeIn is; NONE)
      end

  end
