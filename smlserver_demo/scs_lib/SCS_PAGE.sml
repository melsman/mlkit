signature SCS_PAGE =
  sig
    val navbar : (string * string) list -> quot
    val returnPg  : string -> quot -> quot
  end