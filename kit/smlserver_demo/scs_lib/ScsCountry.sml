(* $Id$ *)

signature SCS_COUNTRY =
  sig

    val selectCountry : string -> int option -> quot

  end (* of SCS_COUNTRY *)

structure ScsCountry :> SCS_COUNTRY =
  struct

    local
      fun genSql lang =
	  `select country_id, scs_text.getText(
	     country_name_tid, ^(Db.qqq (ScsLang.toString lang))
	   ) as name 
	   from scs_country_codes
	  order by name`
    in
      fun selectCountry fv v_opt =
	let
	  val opts =
	    ScsError.wrapPanic
	    ( Db.list (fn g => (g "name", g "country_id")) )
	    ( genSql (ScsLogin.user_lang()) )
	in
	  case v_opt of
	      NONE   => ScsWidget.select opts fv
	    | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end
    end

  end
