signature SCS_QUOT =
  sig
    val translate : (char -> quot) -> quot -> quot
    val lower     : quot -> quot
    val upper     : quot -> quot

    (* [maybe str1 str2] returns str2 if str1 is non
       empty. If empty, then the empty string is returned. *)
    val maybe     : quot -> quot -> quot
  end

structure ScsQuot =
  struct
    fun translate f s  = Quot.concat (map f (Quot.explode s))
    fun lower s = Quot.fromString (CharVector.fromList (List.map Char.toLower (Quot.explode s)))
    fun upper s = Quot.fromString (CharVector.fromList (List.map Char.toUpper (Quot.explode s)))

    fun maybe q1 q2 = if Quot.==(q1,``) then `` else q2
  end