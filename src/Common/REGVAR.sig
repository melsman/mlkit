(* region variables *)

signature REGVAR = sig
  datatype storage_mode = ATBOT | SAT | ATTOP
  type regvar
  val mk_Fresh : string -> regvar
  val mk_Named : string -> regvar
  val with_storage_mode : storage_mode * regvar -> regvar
  val storage_mode : regvar -> storage_mode option
  val same_annotation : regvar * regvar -> bool
  val pr       : regvar -> string
  val pu       : regvar Pickle.pu

  val eq       : regvar * regvar -> bool
  val eqs      : regvar list * regvar list -> bool
  val eq_opt   : regvar option * regvar option -> bool

  val is_effvar : regvar -> bool

  val attach_location_report : regvar -> (unit -> Report.Report) -> unit
  val get_location_report    : regvar -> Report.Report option

  structure Map : MONO_FINMAP where type dom = regvar
end
