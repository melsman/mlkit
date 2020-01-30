(* region variables *)

signature REGVAR = sig
  type regvar
  val mk_Fresh : string -> regvar
  val mk_Named : string -> regvar
  val pr       : regvar -> string
  val pu       : regvar Pickle.pu

  val eq       : regvar * regvar -> bool
  val eqs      : regvar list * regvar list -> bool
  val eq_opt   : regvar option * regvar option -> bool

  val attach_location_report : regvar -> (unit -> Report.Report) -> unit
  val get_location_report    : regvar -> Report.Report option

  structure Map : MONO_FINMAP
                      where type StringTree = PrettyPrint.StringTree
                             and type dom = regvar
end
