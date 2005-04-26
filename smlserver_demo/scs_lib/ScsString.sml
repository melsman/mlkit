signature SCS_STRING =
  sig
    val translate  : (char -> string) -> string -> string

    (* [replace from to str] replaces all occurences of from with to in str *)
    val replace	   : string -> string -> string -> string

    val lower      : string -> string
    val upper      : string -> string
    val lowerFirst : string -> string
    val upperFirst : string -> string
    val inverse	   : string -> string

    (* [upperFirstInEveryWord str] transforms 
       ""                                 -> ""
       "Kennie"				  -> "Kennie"
       "kennie"				  -> "Kennie"
       "   kennie  "			  -> "Kennie"
       "   kennie   Nybo     pontoppidan" -> "Kennie Nybo Pontoppidan"
    *)
    val upperFirstInEveryWord : string -> string

    (* [canonical s] returns a canonical representation of s, that is,
       all words are separated by only one space; new lines etc. has
       been removed *)
    val canonical : string -> string

    (* [shorten text len] returns the string text shortended to
       maximum lenght len appended with "...". *)
    val shorten : string -> int -> string

    (* [keepFirst text len] returns the string text shortended to
       maximum lenght len *)
    val keepFirst : string -> int -> string

    (* [trim str] returns the trimmed str, that is, str will all
        prefix and postfix spaces removed. *)
    val trim : string -> string

    (* [maybe str1 str2] returns str2 if str1 is non
       empty. If empty, then the empty string is returned. *)
    val maybe     : string -> string -> string

    (* [valOf strOpt] if strOpt is SOME str then returns str otherwise
       the empty string *)
    val valOf     : string option -> string

    (* [toOpt str] if str is empty then returns NONE otherwise SOME str *)
    val toOpt     : string -> string option

    (* [mk_search_pattern pat] returns a pattern with % added at each
        space and pattern lowered case *)
    val mk_search_pattern : string -> string

    val quoteString : string -> string
    val padLedt  : string -> int -> string -> string
    val padRight : string -> int -> string -> string
  end

structure ScsString =
  struct
    fun translate f s  = concat (map f (explode s))
    fun lower s = CharVector.fromList (List.map Char.toLower (explode s))
    fun upper s = CharVector.fromList (List.map Char.toUpper (explode s))
    val inverse = CharVector.fromList o rev o explode

    fun lowerFirst str = 
      let
        val (first,rest) = Substring.splitAt (Substring.all str,1)
      in
        (Char.toString o Char.toLower o valOf o Substring.first) first
	^ Substring.string rest
      end
      handle _ => str

    fun upperFirst str = 
      let
        val (first,rest) = Substring.splitAt (Substring.all str,1)
      in
        (Char.toString o Char.toUpper o valOf o Substring.first) first
	^ lower (Substring.string rest)
      end
      handle _ => str

    fun trim s = 
      Substring.string (Substring.dropr Char.isSpace (Substring.dropl Char.isSpace (Substring.all s)))

    fun toWords (str,acc) = 
      let
        val substr = Substring.all str
      in 
	if Substring.isEmpty substr then (rev acc)
	else 
	  let
	    val (first,rest) = Substring.position " " substr
	    val rest_trimmed = trim (Substring.string rest)
	    val new_acc = first::acc
	  in 
	    toWords (rest_trimmed, new_acc)
	  end
      end

    fun replace from to str = 
      let
        val result = []
	val substr_to = Substring.all to
	fun repl (substr,acc) = 
	  let
	    val (first,rest) = Substring.position from substr
	  in
	    if Substring.isEmpty substr then (rev acc)
	    else 
	      let 
		val (new_rest, new_acc) =
		  if Substring.isEmpty rest then
		    (rest, first :: acc)
		  else  (
		    Substring.triml (size from) rest,
		    substr_to :: first :: acc)
	      in
		repl(new_rest, new_acc)
	      end
	  end
      in
        Substring.concat (repl (Substring.all str,result))
      end

    fun upperFirstInEveryWord str = 
      let
        val words = toWords (str, [])
	val words = map Substring.string words
      in
        String.concatWith " " (map upperFirst words)
      end

    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)

    fun keepFirst text length = 
      String.substring( text, 0, Int.min(length, String.size text) ) 

    fun shorten text length = keepFirst text length ^ "..."

    fun maybe str1 str2 = if str1 = "" then "" else str2

    fun valOf (SOME s) = s
      | valOf NONE = ""

    fun mk_search_pattern pat =
      if pat = "%" then
	pat
      else
	if pat = "" then 
	  "%" 
	else 
	  "%"^(String.concatWith "%" (String.tokens Char.isSpace pat)) ^ "%"

    fun quoteString str = Quot.toString `"^str"`

    fun toOpt str = if str = "" then NONE else SOME str

    fun padLeft pad n s =
      let
	fun padL (0,acc) = acc
	  | padL (n,acc) = padL (n-1,pad^acc)
      in
	if n < 0 then s else padL (n,s)
      end

    fun padRight pad n s =
      let
	fun padR (0,acc) = acc
	  | padR (n,acc) = padR (n-1,acc^pad)
      in
	if n < 0 then s else padR (n,s)
      end
  end
