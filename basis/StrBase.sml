(* StrBase -- internal utilities for String and Substring *)

structure StrBase : STR_BASE = struct

  (* Primitives *)
  fun sub_unsafe (s : string, i : int) : char = prim ("__bytetable_sub", (s,i))
  fun size (s:string): int = prim ("__bytetable_size", s)
  fun implode (chars: char list): string = prim ("implodeCharsML", chars)
  fun concat(ss:string list) : string = prim("implodeStringML", ss)

  fun chr(i:int) : char =
    if i>=0 andalso i<256 then prim ("id", i)
    else raise Chr

  fun ord (c : char) : int = prim ("id", c)

  fun not false = true
    | not true = false

  fun explode s =
    let fun h (j, res) = if j < 0 then res
			 else h (j-1, sub_unsafe (s, j) :: res)
    in h (size s - 1, nil)
    end

  fun str (c : char) : string = implode [c]

  (* Body *)
  type substring = string * int * int

  local

    fun scanl0 pred (j, stop, s) = 
      let 
	fun scan0 (j, stop,s) =
	  if j < stop andalso pred(sub_unsafe(s, j)) then scan0 (j+1, stop, s)
	  else j
      in scan0 (j,stop,s)
      end

    fun scanl (chop, scan, (s, i, n)) = chop (s, i, n, scan (i,i+n,s) - i)

    fun scanr (chop, pred, (s, i, n)) =
	let val stop = i-1
	    fun scan j = if j > stop andalso pred(sub_unsafe(s, j)) then scan(j-1)
			 else j
	in chop (s, i, n, scan (i+n-1) - i + 1)
	end
  in
    fun dropl  p ss = scanl (fn (s, i, n, k) => (s, i+k, n-k), scanl0 p, ss)
    fun dropr  p ss = scanr (fn (s, i, n, k) => (s, i, k), p, ss)
    fun takel  p ss = scanl (fn (s, i, n, k) => (s, i, k), scanl0 p, ss)
    fun taker  p ss = scanr (fn (s, i, n, k) => (s, i+k, n-k), p, ss)
    fun splitl p ss = scanl (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k)), scanl0 p, ss)
    fun splitr p ss = scanr (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k)), p, ss)
  end (* local *)

  fun translate f (s,i,n) =
    let val stop = i+n
	  fun h (j, res) = if j >= stop then res
			   else h (j+1, f(sub_unsafe(s, j)) :: res)
    in concat(rev(h(i,nil)))
    end

  fun tokens isDelim ss =
    let fun findTok ss = dropl isDelim ss
        fun h (remains as (_, _, n)) res =
	    if n = 0 then rev res
	    else let val (token, aftertoken) =
	               splitl (fn c => not(isDelim c)) remains
		 in h (findTok aftertoken) (token :: res) 
		 end
    in h (findTok ss) [] 
    end


  fun fields isDelim ss =
    let fun rest (ss as (s, i, n)) = if n = 0 then ss else (s, i+1, n-1)
	  fun h ss res =
	    let val (field, afterfield as (_, _, n)) =
		 splitl (fn c => not(isDelim c)) ss
	    in if n = 0 then rev (field :: res)
	       else h (rest afterfield) (field :: res)
	    end
    in h ss [] 
    end

  fun foldl f e (s,i,n) =
    let val stop = i+n
        fun h (j, res) = if j >= stop then res
			 else h (j+1, f (sub_unsafe(s, j), res))
    in h (i, e)
    end


  local    (* Conversion to and from ML and C character escape sequences *)
    exception BadEscape = Chr

    (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
    fun decval c = ord c - 48;
    fun digit n = chr(48 + n);
    fun hexval c = if #"0" <= c andalso c <= #"9" then ord c - 48
		     else (ord c - 55) mod 32;
    fun isOctDigit c = #"0" <= c andalso c <= #"7"
    fun isHexDigit c = #"0" <= c andalso c <= #"9"
	               orelse #"a" <= c andalso c <= #"f"
	               orelse #"A" <= c andalso c <= #"F"

  in

    fun fromMLescape getc source =
	let fun decimal cont src code =
	      case getc src
		of NONE => raise BadEscape
		 | SOME(c, rest) => if #"0" <= c andalso c <= #"9"
				      then cont rest (code * 10 + ord c - 48)
				    else raise BadEscape
	    val from3Dec = decimal (decimal (decimal (fn src => fn code => (chr code, src))))
	    fun skipform src =
	      case getc src 
		of NONE => NONE
		 | SOME(#"\\", src1) =>
		    (case getc src1
		       of NONE => NONE
			| SOME(#"\\", src2) => fromMLescape getc src2
			| res => res)
		 | SOME(c, rest) =>
		       if c = #" " orelse #"\009" <= c andalso c <= #"\013" then
			 skipform rest
		       else NONE
	in
	  case getc source
	    of NONE => NONE
	     | SOME(#"a", rest) => SOME(#"\007", rest) (* BEL *)
	     | SOME(#"b", rest) => SOME(#"\008", rest) (* BS  *)
	     | SOME(#"t", rest) => SOME(#"\009", rest) (* HT  *)

	     | SOME(#"n", rest) => SOME(#"\010", rest) (* LF  *)
	     | SOME(#"r", rest) => SOME(#"\013", rest) (* CR  *)

	     | SOME(#"v", rest) => SOME(#"\011", rest) (* VT  *)
	     | SOME(#"f", rest) => SOME(#"\012", rest) (* FF  *)
	     | SOME(#"\"", rest) => SOME(#"\"", rest)
	     | SOME(#"\\", rest) => SOME(#"\\", rest)
	     | SOME(#" ", rest) => skipform rest
	     | SOME(#"\n", rest) => skipform rest
	     | SOME(#"\t", rest) => skipform rest
	     | SOME(#"^", rest)  =>
		(case getc rest 
		   of NONE => NONE
		    | SOME(c, rest) =>
		     if #"@" <= c andalso c <= #"_" then
		       SOME(chr (ord c - 64), rest)
		     else NONE)
	     | _ => SOME (from3Dec source 0)
		   handle BadEscape => NONE
	end

  fun toMLescape c =
	case c of
	    #"\\"   => "\\\\"
	  | #"\""   => "\\\""
	  | _       =>
	    if #"\032" <= c then
		if c <= #"\126" then str c
		else let val n = ord c
		     in implode [#"\\", digit(n div 100), digit(n div 10 mod 10),
				 digit(n mod 10)]
		     end
	    else
		(case c of
		     #"\007" => "\\a"			(* BEL,  7 *)
		   | #"\008" => "\\b"			(* BS,   8 *)
		   | #"\009" => "\\t"			(* HT,   9 *)

		   | #"\010" => "\\n"			(* LF,  10 *)
		   | #"\013" => "\\r"			(* CR,  13 *)

		   | #"\011" => "\\v"			(* VT,  11 *)
		   | #"\012" => "\\f"			(* FF,  12 *)
                 | _       => let val n = ord c
				in implode [#"\\", #"^", chr (ord c + 64)]
				end)

  (* C character escape functions, 1995-10-30 *)
  (* C character escape codes according to Kernighan & Ritchie: The C  *
   * Programming Language, second edition, page 193                    *)

  fun toCescape c =
	case c of
	    #"\\"   => "\\\\"
	  | #"?"    => "\\?"
	  | #"'"    => "\\'"
	  | #"\""   => "\\\""
	  | _       =>
	    if #"\032" <= c andalso c <= #"\126" then str c
	    else
		(case c of
		     #"\010" => "\\n"			(* LF,  10 *)
		   | #"\013" => "\\r"			(* CR,  13 *)

		   | #"\009" => "\\t"			(* HT,   9 *)
		   | #"\011" => "\\v"			(* VT,  11 *)
		   | #"\008" => "\\b"			(* BS,   8 *)
		   | #"\012" => "\\f"			(* FF,  12 *)
		   | #"\007" => "\\a"			(* BEL,  7 *)
                 | _       => let val n = ord c
				in implode[#"\\", digit(n div 64), digit(n div 8 mod 8),
					   digit(n mod 8)]
				end)

  fun fromCescape' getc src =		(* raises BadEscape *)
    let fun fromHex src code =
	    case getc src
	      of NONE => (chr code, src)
	       | SOME(c, rest) => if isHexDigit c then fromHex rest (code * 16 + hexval c)
				  else (chr code, src)
	  fun octalOpt cont src code =
	    case getc src 
	      of NONE => (chr code, src)
	       | SOME(c, rest) => if #"0" <= c andalso c <= #"7"
				    then cont rest (code * 8 + ord c - 48)
				  else (chr code, src)
	  val fromOct =	octalOpt (octalOpt (fn src => fn code => (chr code, src)))
	in
	    case getc src
	      of NONE              => raise BadEscape

	       | SOME(#"n",  src1) => (#"\n",   src1)
	       | SOME(#"r",  src1) => (#"\013", src1)

	       | SOME(#"t",  src1) => (#"\009", src1)
	       | SOME(#"v",  src1) => (#"\011", src1)
	       | SOME(#"b",  src1) => (#"\008", src1)
	       | SOME(#"f",  src1) => (#"\012", src1)
	       | SOME(#"a",  src1) => (#"\007", src1)
	       | SOME(#"\\", src1) => (#"\\",   src1)
	       | SOME(#"?",  src1) => (#"?",    src1)
	       | SOME(#"'",  src1) => (#"'",    src1)
	       | SOME(#"\"", src1) => (#"\"",   src1)
	       | SOME(#"x",  src1) =>
		     (case getc src1 
			of NONE => raise BadEscape
			 | SOME(c, src2) => if isHexDigit c then fromHex src2 (hexval c)
					    else raise BadEscape)
	       | SOME(c,     src1) =>
			  if isOctDigit c then fromOct src1 (decval c)
			  else raise BadEscape
    end

  fun fromCescape getc src =		(* Returns a char option *)
	SOME (fromCescape' getc src)
	handle
	   BadEscape => NONE (* Illegal C escape sequence or character code *)
	 | Overflow  => NONE (* Character code far too large                *)

  fun fromCString s =
	let fun getc (c::cs) = SOME (c,cs)
	      | getc [] = NONE
	    fun h acc src =
	      case getc src
		of NONE => implode(rev acc)
		 | SOME(#"\\", src1) => let val (c, src2) = fromCescape' getc src1
					in h (c::acc) src2 
					end
		 | SOME(c, src1) => (h (c::acc) src1)
	in SOME (h [] (explode s))
	  handle
	  BadEscape => NONE (* Illegal C escape sequence or character code *)
	| Overflow  => NONE (* Character code far too large                *)
	end

  end (* local *)

end 

