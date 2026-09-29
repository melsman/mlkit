(* explicit region variables *)

structure RegVar :> REGVAR = struct
  datatype storage_mode = ATBOT | SAT | ATTOP
  type regvar = {mode:storage_mode option,name:string,loc_rep:(unit->Report.Report) option ref}

  fun is_effvar (v:regvar) =
      String.isPrefix "e" (#name v)

  val mk_Fresh : string -> regvar =
      let val count = ref 0
      in fn s => {mode=NONE,name=s ^ Int.toString (!count before count := !count + 1),
                  loc_rep=ref NONE}
      end
  fun mk_Named s = {mode=NONE,name=s,loc_rep=ref NONE}
  fun name (r:regvar) = #name r
  fun storage_mode (r:regvar) = #mode r
  fun with_storage_mode (mode,r:regvar) =
      {mode=SOME mode,name= #name r,loc_rep= #loc_rep r}
  fun pr r =
      (case storage_mode r of NONE => "" | SOME ATBOT => "atbot "
                            | SOME SAT => "sat " | SOME ATTOP => "attop ") ^ name r
  (* Keep the existing encoding of unannotated variables. Region names cannot
   * contain spaces, so annotated occurrences have an unambiguous prefix. *)
  val pu = Pickle.convert
      (fn s => case String.tokens Char.isSpace s of
                   ["atbot",r] => with_storage_mode (ATBOT,mk_Named r)
                 | ["sat",r] => with_storage_mode (SAT,mk_Named r)
                 | ["attop",r] => with_storage_mode (ATTOP,mk_Named r)
                 | _ => mk_Named s,
       pr) Pickle.string

  fun eq (r1,r2) = name r1 = name r2

  fun same_annotation (a,b) = eq(a,b) andalso storage_mode a = storage_mode b

  fun eqs (x::xs,y::ys) = eq(x,y) andalso eqs (xs,ys)
    | eqs (nil,nil) = true
    | eqs _ = false

  fun eq_opt (NONE,NONE) = true
    | eq_opt (SOME rv,SOME rv') = same_annotation(rv,rv')
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
