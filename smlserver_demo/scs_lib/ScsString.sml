signature SCS_STRING =
  sig
    val translate : (char -> string) -> string -> string
    val lower     : string -> string
    val upper     : string -> string

    (* [maybe str1 str2] returns str2 if str1 is non
       empty. If empty, then the empty string is returned. *)
    val maybe     : string -> string -> string
  end

structure ScsString =
  struct
    fun translate f s  = concat (map f (explode s))
    fun lower s = CharVector.fromList (List.map Char.toLower (explode s))
    fun upper s = CharVector.fromList (List.map Char.toUpper (explode s))

    fun maybe str1 str2 = if str1 = "" then "" else str2
  end