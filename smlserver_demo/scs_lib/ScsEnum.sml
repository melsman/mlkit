signature SCS_ENUM =
  sig
    type enum_id = int
    type val_id = int
    type enum_name = string

    (* [select enum_id lang fv v_opt] returns a selection widget for
       enumeration identified by enum_id. The choises are shown in language
       lang and the form variable used is fv. If v_opt is SOME v, then
       choise with val_id = v is pre-selected. *)
    val select     : enum_id -> ScsLang.lang -> string -> val_id option -> quot

    (* [selectName enum_name lang fv v_opt] is similar to select
       except that the enumeration is identified by the name (i.e.,
       enum_name) instead of the enum_id *)
    val selectName : enum_name -> ScsLang.lang -> string -> val_id option -> quot

    (* [getEnumErr enum_name] returns a form variable check function
       which checks that a value is one of the posibilities in the
       enumeration with name enum_name. *)
    val getEnumErr : enum_name -> val_id ScsFormVar.formvar_fn

    (* [enumName enum_id] returns the enumeration name of enumeration
       identified by enum_id. *)
    val enumName   : enum_id -> string

    (* [valName val_id lang] returns the name of the enumeration value
       val_id in language lang. *)
    val valName    : val_id -> ScsLang.lang -> string

    (* [allValues enum_name] returns a list of all enumeration values in the
       enumeration with name enum_name *)
    val allValues : enum_name -> int list
  end

structure ScsEnum :> SCS_ENUM =
  struct
    type enum_id = int
    type val_id = int
    type enum_name = string

    local
      fun genSql wh lang =
	`select ev.val_id, 
         	scs_text.getText(ev.text_id,'^(ScsLang.toString lang)') as text
           from scs_enumerations e, scs_enum_values ev
          where e.enum_id = ev.enum_id
            and ev.active_p = 't'
            and ` ^^ wh ^^ `
          order by ev.ordering`
    in
      fun select enum_id lang fv v_opt =
	let
	  val opts =
	    Db.list (fn g => (g "text", g "val_id")) 
 	      (genSql `e.enum_id = '^(Int.toString enum_id)'` lang)
	in
          case v_opt of
            NONE => ScsWidget.select opts fv
          | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end

      fun selectName enum_name lang fv v_opt =
	let
	  val opts =
	    Db.list (fn g => (g "text", g "val_id")) 
	      (genSql `e.name = ^(Db.qqq enum_name)` lang)
	in
          case v_opt of
            NONE => ScsWidget.select opts fv
          | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end
    end

    fun allValues enum_name = Db.list (fn g => Option.valOf (Int.fromString (g "val_id"))) 
                                `select val_id
                                   from scs_enum_values, scs_enumerations
                                  where scs_enumerations.name = ^(Db.qqq enum_name)
                                    and scs_enumerations.enum_id = scs_enum_values.enum_id
                                    and scs_enum_values.active_p = 't'`

    fun valName val_id lang =
      Db.oneField `select scs_text.getText(ev.text_id,^(Db.qqq (ScsLang.toString lang))) as text
                     from scs_enum_values ev
                    where ev.val_id = '^(Int.toString val_id)'`

    fun getEnumErr enum_name = 
      let
	val all_values = allValues enum_name
        fun msgEnum s =
  	  (case ScsLogin.user_lang of
	     ScsLang.en => `^s
	       You must choose among the following enumerations:
	       <blockquote>
	       ^(String.concatWith "," (List.map (fn enum => valName enum ScsLogin.user_lang) all_values))
	       </blockquote>`
	   | ScsLang.da => `^s
	       Du skal indtaste en af de følgende værdier:
	       <blockquote>
	       ^(String.concatWith "," (List.map (fn enum => valName enum ScsLogin.user_lang) all_values))
	       </blockquote>`)
        fun chkEnum v =
          case Int.fromString v of
            SOME val_id => (case List.find (fn enum => val_id = enum) all_values
                              of NONE => false
                            | SOME _ => true)
          | NONE => false
      in
        ScsFormVar.getErr 0 (Option.valOf o Int.fromString) 
          (ScsDict.s [(ScsLang.en,`enumeration`),(ScsLang.da,`enumerering`)]) msgEnum chkEnum
      end

    fun enumName enum_id =
      Db.oneField `select scs_enum.getName('^(Int.toString enum_id)')
                     from dual`

  end
