(* region variables *)

signature REGVAR = sig
  eqtype regvar
  val mk_Fresh : string -> regvar
  val mk_Named : string -> regvar
  val pr       : regvar -> string
  val pu       : regvar Pickle.pu
end
