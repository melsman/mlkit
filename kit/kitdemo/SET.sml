signature SET =
  sig
    eqtype elem and set
    val empty : set
    val singleton : elem -> set
    val mem : elem -> set -> bool
    val union : set * set -> set
    val pr : set -> string
  end