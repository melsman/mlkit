signature SCS_STRING =
  sig
    val translate  : (char -> string) -> string -> string
    val lower      : string -> string
    val upper      : string -> string
    val lowerFirst : string -> string


    (* [canonical s] returns a canonical representation of s, that is,
       all words are separated by only one space; new lines etc. has
       been removed *)
    val canonical : string -> string

    (* [shorten text len] returns the string text shortended to
       maximum lenght len. *)
    val shorten : string -> int -> string

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

    (* [ml_search_pattern pat] returns a pattern with % added at each
        space *)
    val mk_search_pattern : string -> string

    val quoteString : string -> string
  end

structure ScsString =
  struct
    fun translate f s  = concat (map f (explode s))
    fun lower s = CharVector.fromList (List.map Char.toLower (explode s))
    fun upper s = CharVector.fromList (List.map Char.toUpper (explode s))

    fun lowerFirst str = 
      let
        val (first,rest) = Substring.splitAt (Substring.all str,1)
      in
        (Char.toString o Char.toLower o valOf o Substring.first) first
	^ Substring.string rest
      end
      handle _ => str


    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)

    fun trim s = 
      Substring.string (Substring.dropr Char.isSpace (Substring.dropl Char.isSpace (Substring.all s)))

    fun shorten text length = 
      String.substring( text, 0, Int.min(length, String.size text) ) ^ "..."

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
	  "%"^(String.concatWith "%" (String.tokens Char.isSpace pat))^"%"

    fun quoteString str = Quot.toString `"^str"`

    fun toOpt str = if str = "" then NONE else SOME str
  end
