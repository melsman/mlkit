signature SCS_PAGE =
  sig
    type navbar = (quot * quot) list
    val navbar    : navbar -> quot

    val returnTop : string -> Ns.status
    val write     : quot -> Ns.status
    val returnBot : unit -> Ns.status

    val returnPg  : string -> quot -> Ns.status
  end