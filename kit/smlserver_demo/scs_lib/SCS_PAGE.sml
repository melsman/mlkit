signature SCS_PAGE =
  sig
    type navbar = (quot * quot) list
    val navbar    : navbar -> quot
    val returnPg  : string -> quot -> Ns.status
  end