(* explicit region variables *)

structure RegVar :> REGVAR = struct
  type regvar = {name:string,loc_rep:(unit->Report.Report) option ref}

  fun is_effvar (v:regvar) =
      String.isPrefix "e" (#name v)

  val mk_Fresh : string -> regvar =
      let val count = ref 0
      in fn s => {name=s ^ Int.toString (!count before count := !count + 1),
                  loc_rep=ref NONE}
      end
  fun mk_Named s = {name=s,loc_rep=ref NONE}
  fun name (r:regvar) = #name r
  fun pr r = name r
  val pu = Pickle.convert (fn s => {name=s,loc_rep=ref NONE},
                           name) Pickle.string

  fun eq (r1,r2) = name r1 = name r2

  fun eqs (x::xs,y::ys) = eq(x,y) andalso eqs (xs,ys)
    | eqs (nil,nil) = true
    | eqs _ = false

  fun eq_opt (NONE,NONE) = true
    | eq_opt (SOME rv,SOME rv') = eq(rv,rv')
    | eq_opt _ = false

  fun attach_location_report (r:regvar) (f:unit -> Report.Report) : unit =
      #loc_rep r := SOME f

  fun get_location_report (r:regvar) : Report.Report option =
      case !(#loc_rep r) of
          SOME f => SOME(f())
        | NONE => NONE

  structure Map = OrderFinMap(struct type t = regvar
                                     fun lt (a:t, b:t) = #name a < #name b
                              end)
end
