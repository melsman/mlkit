signature SCS_ENUM =
  sig
    type enum_id = int
    type val_id = int
    type enum_name = string

    val select     : enum_id -> ScsLang.lang -> string -> quot
    val selectName : enum_name -> ScsLang.lang -> string -> quot
    val enumName   : enum_id -> string
    val valName    : val_id -> ScsLang.lang -> string
  end

structure ScsEnum :> SCS_ENUM =
  struct
    type enum_id = int
    type val_id = int
    type enum_name = string

    local
      fun genSql wh lang =
	`select ev.val_id, 
         	scs_text.getText(ev.text_id,^(ScsLang.toString lang)) as text
           from scs_enumerations e, scs_enum_values ev
          where e.enum_id = ev.enum_id
            and ` ^^ wh
    in
      fun select enum_id lang fv =
	let
	  val opts =
	    Db.list (fn g => (g "text", g "val_id")) 
 	      (genSql `e.enum_id = '^(Int.toString enum_id)'` lang)
	in
	  ScsWidget.select opts fv
	end

      fun selectName enum_name lang fv =
	let
	  val opts =
	    Db.list (fn g => (g "text", g "val_id")) 
	      (genSql `e.name = '^(Db.qqq enum_name)'` lang)
	in
	       ScsWidget.select opts fv
	end
    end

    fun enumName enum_id =
      Db.oneField `select scs_enum.getName('^(Int.toString enum_id)')
                     from dual`

    fun valName val_id lang =
      Db.oneField `select scs_text.getText(ev.text_id,^(Db.qqq (ScsLang.toString lang))) as text
                     from scs_enum_values ev
                    where ev.val_id = '^(Int.toString val_id)'`
  end