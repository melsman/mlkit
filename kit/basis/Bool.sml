(*Bool.sml*)
   
structure Bool : BOOL =    (* Depends on String and StringCvt *)
  struct

    structure Char = String.Char

    type bool = bool

    fun not true = false
      | not false = true

    fun toString false = "false"
      | toString true  = "true"

    fun getstring str getc source =
      let val len = size str
	  fun toLower c = if #"A" <= c andalso c <= #"Z" then Char.chr (Char.ord c + 32)
			  else c
	  fun h i src = if i >= len then SOME src 
			else case getc src 
			       of NONE => NONE
				| SOME(c, rest) => 
                                 if  toLower c >= (*#"a"*) String.sub(str,i) then h (i+1) rest
                                 else NONE
      in h 0 source 
      end

    fun scan getc source =
      let val src = StringCvt.dropl Char.isSpace getc source 
      in case getstring "true" getc src 
	   of SOME rest => SOME(true, rest)
	    | NONE => 
	 case getstring "false" getc src 
	   of SOME rest => SOME(false, rest)
	    | NONE => NONE
      end

    fun fromString s = StringCvt.scanString scan s

  end


