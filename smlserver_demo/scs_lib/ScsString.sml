signature SCS_STRING =
  sig
    val translate : (char -> string) -> string -> string
  end

structure ScsString =
  struct
    fun translate f s  = concat (map f (explode s))
  end