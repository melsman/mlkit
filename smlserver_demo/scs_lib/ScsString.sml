signature SCS_STRING =
  sig
    val translate : (char -> string) -> string -> string
    val lower     : string -> string
    val upper     : string -> string
  end

structure ScsString =
  struct
    fun translate f s  = concat (map f (explode s))
    fun lower s = CharVector.fromList (List.map Char.toLower (explode s))
    fun upper s = CharVector.fromList (List.map Char.toUpper (explode s))
  end