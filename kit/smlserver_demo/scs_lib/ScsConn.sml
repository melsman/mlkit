signature SCS_CONN =
  sig
    (* [returnRedirect url hvs] builds an url using target url url and appends the
       hidden variables hvs. Then redirects to the target url. *)
   val returnRedirect : string -> (string * string) list -> Ns.status
  end

structure ScsConn :> SCS_CONN =
  struct
    fun returnRedirect url hvs = Ns.returnRedirect (Html.genUrl url hvs)
  end
