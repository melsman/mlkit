signature SMLS_LANG =
  sig
    exception SmlsLang of string
    datatype lang = Danish | English
    val toString : lang -> string
    val fromString : string -> lang
  end

structure SmlsLang :> SMLS_LANG =
  struct
    exception SmlsLang of string
    datatype lang = Danish | English
    fun toString Danish = "Danish"
      | toString English = "English"
    fun fromString "Danish" = Danish
      | fromString "English" = English
      | fromString l = raise SmlsLang ("Language " ^ l ^ " not known")
  end
