signature SCS_LANG =
  sig
    exception ScsLang of string
    datatype lang = Danish | English
    val toString : lang -> string
    val fromString : string -> lang
  end

structure ScsLang :> SCS_LANG =
  struct
    exception ScsLang of string
    datatype lang = Danish | English
    fun toString Danish = "Danish"
      | toString English = "English"
    fun fromString "Danish" = Danish
      | fromString "English" = English
      | fromString l = raise ScsLang ("Language " ^ l ^ " not known")
  end
