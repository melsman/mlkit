(* $Id$ *)

signature SCS_COUNTRY =
  sig

    val selectCountry : string -> int option -> quot

    val valOfCountry : int option -> string

    val countryName : int -> ScsLang.lang -> string

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

    fun countryName country_id lang = Db.oneField `
      select scs_text.getText(cc.country_name_tid,
			      ^(Db.qqq (ScsLang.toString lang)))
        from scs_country_codes cc
       where cc.country_id = '^(Int.toString country_id)'`

    fun valOfCountry NONE = ""
      | valOfCountry (SOME country_id) = countryName country_id (ScsLogin.user_lang())

  end
