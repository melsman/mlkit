signature INT_SET =
  sig
    type 'a set
    val empty : int set
    val singleton : int -> int set
    val mem : int * int set -> bool
    val union : int set * int set -> int set
  end