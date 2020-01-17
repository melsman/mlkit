(* region variables *)

structure RegVar :> REGVAR = struct
  type regvar = string
  val mk_Fresh : string -> regvar =
      let val count = ref 0
      in fn s => s ^ Int.toString (!count before count := !count + 1)
      end
  fun mk_Named s = s
  fun pr s = s
  val pu = Pickle.string
end
