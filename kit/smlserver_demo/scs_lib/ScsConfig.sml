(* Functions used to access the configuration parameters in the
   nsd.<site>.tcl configuration file *)

signature SCS_CONFIG =
  sig
    val scs_print_journal     : unit -> string
    val scs_print_preview     : unit -> string
    val scs_site_index_page   : unit -> string
    val scs_site_adm_email    : unit -> string
    val scs_site_name         : unit -> string
    val scs_site_url          : unit -> string (* url to site from external link, that is, 
                                                  with https, eg. https://www.x.com/ *)
    val scs_home_path         : unit -> string (* path to home from internal link, that is, 
                                                  without https, eg / *)
    val scs_file_storage_root : unit -> string
    val scs_tmp 	      : unit -> string
    val scs_debug_p           : unit -> string
    val scs_email_when_debug  : unit -> string
    val scs_pdflatex          : unit -> string
    val scs_latex	      : unit -> string
    val scs_dvips	      : unit -> string
    val scs_ps2pdf	      : unit -> string

    val scs_identify          : unit -> string
    val scs_convert           : unit -> string
    val scs_cp                : unit -> string

    (* [commaListContains s key] returns true if key is in the comma
        separated string s. Match is case insensitive. *)
    val commaListContains     : string -> string -> bool
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
      fun scs_home_path() = getInfo "scs_home_path"
      fun scs_file_storage_root() = getInfo "scs_file_storage_root"
      fun scs_tmp () = getInfo "scs_tmp"
      fun scs_debug_p() = getInfo "scs_debug_p"
      fun scs_email_when_debug() = getInfo "scs_email_when_debug"
      fun scs_pdflatex() = getInfo "scs_pdflatex"
      fun scs_latex() = getInfo "scs_latex"
      fun scs_dvips() = getInfo "scs_dvips"
      fun scs_ps2pdf() = getInfo "scs_ps2pdf"
      fun scs_identify() = getInfo "scs_identify"
      fun scs_convert() = getInfo "scs_convert"
      fun scs_cp() = getInfo "scs_cp"
    end

    fun commaListContains s key =
      let
	val tokens = String.tokens (fn c => c = #"," orelse Char.isSpace c) s
	val tokens = List.map (ScsString.lower o ScsString.trim) tokens
      in
	ScsList.contains (ScsString.lower key) tokens 
      end
  end
