signature SCS_REAL =
  sig
    val toString : ScsLang.lang -> real -> string
    val fromString : ScsLang.lang -> string -> real
  end

structure ScsReal :> SCS_REAL =
  struct
    fun toString ScsLang.Danish r = 
      String.map (fn c => if c = #"." then #"," else if c = #"," then #"." else c)
      (Real.fmt (StringCvt.FIX(SOME 2)) r)
    | toString ScsLang.English r = Real.fmt (StringCvt.FIX(SOME 2)) r

    local
      fun fromString' s =
	case Real.fromString s of
	  NONE => ScsError.panic `ScsLang.fromString: ^s not a real`
	| SOME r => r
    in
      fun fromString ScsLang.Danish s =
	fromString' (String.map (fn c => if c = #"." then #"," else if c = #"," then #"." else c) s)
      | fromString ScsLang.English s = fromString' s
    end
  end
