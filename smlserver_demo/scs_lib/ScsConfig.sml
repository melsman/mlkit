(* Functions used to access the configuration parameters in the
   nsd.<site>.tcl configuration file *)

signature SCS_CONFIG =
  sig
    val scs_print_journal   : unit -> string
    val scs_print_preview   : unit -> string
    val scs_site_index_page : unit -> string
    val scs_site_adm_email  : unit -> string
    val scs_site_name       : unit -> string
    val scs_site_url        : unit -> string
  end

structure ScsConfig :> SCS_CONFIG =
  struct
    local
      fun getInfo key =
	case (Ns.Info.configGetValueExact {sectionName="ns/server/"^Ns.Conn.server()^"/SCS",key=key}) of
	  NONE => raise Fail ("ScsConfig.getInfo: can't find key " ^ key)
	| SOME v => v
    in
      fun scs_print_journal () = getInfo "scs_print_journal" 
      fun scs_print_preview () = getInfo "scs_print_preview" 
      fun scs_site_index_page () = getInfo "scs_site_index_page" 
      fun scs_site_adm_email () = getInfo "scs_site_adm_email" 
      fun scs_site_name() = getInfo "scs_site_name"
      fun scs_site_url() = getInfo "scs_site_url"
    end
  end