structure Option : OPTION =
  struct
    exception Option = Option
    datatype option = datatype option

    val getOpt = getOpt
    val isSome = isSome
    val valOf = valOf

    fun filter p x = if p x then SOME x else NONE

    fun map f NONE = NONE
      | map f (SOME x) = SOME (f x)

    fun app f NONE = ()
      | app f (SOME x) = f x

    fun join NONE = NONE
      | join (SOME x) = x

    fun mapPartial f NONE = NONE
      | mapPartial f (SOME x) = f x

    fun compose (f, g) x =
      case g x
	of NONE => NONE
	 | SOME y => SOME (f y)

    fun composePartial (f, g) x =
      case g x
	of NONE => NONE
	 | SOME y => f y
  end
