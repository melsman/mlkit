signature SCS_ENUM =
  sig
    type enum_id = int
    type val_id = int
    type enum_name = string
    type value = string

    type enum_value = 
      {val_id    : val_id,
       enum_id   : enum_id,
       enum_name : enum_name,
       text_id   : int,
       text_da   : string,
       text_eng  : string,
       value     : value,
       active_p  : bool,
       ordering  : int}

    (* [select enum_id lang fv v_opt] returns a selection widget for
       enumeration identified by enum_id. The choises are shown in language
       lang and the form variable used is fv. If v_opt is SOME v, then
       choise with val_id = v is pre-selected. *)
    val select     : enum_id -> ScsLang.lang -> string -> val_id option -> quot

    (* [selectName enum_name lang fv v_opt] is similar to select
       except that the enumeration is identified by the name (i.e.,
       enum_name) instead of the enum_id *)
    val selectName : enum_name -> ScsLang.lang -> string -> val_id option -> quot

    (* [selectNameExcerpt enum_name values lang fv v_opt] is similar to 
	selectName except that the possible values are restricted by values *)
    val selectNameExcerpt : enum_name -> value list -> ScsLang.lang -> string -> val_id option -> quot

    (* [selectName' enum_name lang add_opts fv v_opt] is similar to selectName
       except that the it is possible to prepend additional
	(value, name) pairs in the list add_opts *)
    val selectName' : enum_name -> ScsLang.lang -> (string*string) list -> 
      string -> val_id option -> quot

    (* [radioList enum_id lang fv v_opt] returns a list of (radio button 
       input tag,text) for
       enumeration identified by enum_id. The choises are shown in language
       lang and the form variable used is fv. If v_opt is SOME v, then
       choise with val_id = v is pre-selected. *)
    val radioList : enum_id -> ScsLang.lang -> string -> val_id option -> 
      (quot*quot) list

    (* [radioListName enum_name lang fv v_opt] is similar to radioList
       except that the enumeration is identified by the name (i.e.,
       enum_name) instead of the enum_id *)
    val radioListName : enum_name -> ScsLang.lang -> string -> val_id option 
      -> (quot*quot) list


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

    (* [getVal val_id] returns the value represented by
       val_id. Returns NONE if no val_id exists. *)
    val getVal : val_id -> value option

    (* [getVid enum_name value] returns the vid represented by the pair
       (enum_name, value). Returns NONE if no pair exists. *)
    val getVid : enum_name -> value -> int option

    (* [valToText (value,enum_name,lang)] returns the description of
        enumeration value val for the enumeration enum_name. Returns
        "" if no enum_name or value exists. *)
    val valToText : value * enum_name * ScsLang.lang -> string

    (* [allValues enum_name] returns a list of all enumeration values in the
       enumeration with name enum_name *)
    val allValues : enum_name -> int list

    (* [allValues' enum_name enum_from_DB_fn] returns a list of pairs 
       (value, text) for all enumeration values in the enumeration with 
       name enum_name *)
    val allValues' : enum_name -> (string -> 'a) -> ('a*string) list

   (* [allValuesAsText enum_name lang] returns the string representation of 
      the enumeration in language lang *)
    val allValuesAsText : enum_name -> ScsLang.lang -> string list

   (* [selectEnumeration fv v_opt] returns a selection list for
       chosing an enumeration, that is, enum_id. *)
    val selectEnumeration : string -> enum_id option -> quot

    (* [getEnumerationErr] is a form variable check function which
        checks that a form variable is an enum_id. Flaw: currently the
        database is not checked. *)
    val getEnumerationErr : enum_id ScsFormVar.formvar_fn

    (* [getEnumerationsFromDb enum_id] returns a list of
        enumerationvalues for an particular enum_id. *)
    val getEnumerationsFromDb : enum_id -> enum_value list

    (*********)
    (* DICTS *)
    (*********)
    val value_dict    : ScsDict.dict
    val text_da_dict  : ScsDict.dict
    val text_eng_dict : ScsDict.dict
    val active_p_dict : ScsDict.dict
    val ordering_dict : ScsDict.dict
    val edit_dict     : ScsDict.dict
    val save_dict     : ScsDict.dict
  end

structure ScsEnum :> SCS_ENUM =
  struct
    type enum_id = int
    type val_id = int
    type enum_name = string
    type value = string

    type enum_value = 
      {val_id    : val_id,
       enum_id   : enum_id,
       enum_name : enum_name,
       text_id   : int,
       text_da   : string,
       text_eng  : string,
       value     : value,
       active_p  : bool,
       ordering  : int}

    local
      fun genSql() =
	`select enum_id, name
           from scs_enumerations
          order by name`
    in
      fun selectEnumeration fv v_opt =
	let
	  val opts =
	    ScsError.wrapPanic
	    (Db.list (fn g => (g "name", g "enum_id")))
	    (genSql ())
	in
          case v_opt of
            NONE => ScsWidget.select opts fv
          | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end

      val getEnumerationErr =
	let
	  fun msgEnum s =
	    (case ScsLogin.user_lang() of
	       ScsLang.en => `^s
		 You must choose an enumeration.`
	     | ScsLang.da => `^s
		 Du skal vælge en enumerering.`)
	  fun chkEnum v =
	    case Int.fromString v of
	      SOME enum_id => true
	    | NONE => false
      in
        ScsFormVar.getErr 0 (Option.valOf o Int.fromString) 
	[(ScsLang.en,`enumeration`),(ScsLang.da,`enumerering`)] msgEnum chkEnum
      end
    end

    local
      fun genSql wh lang =
	`select ev.val_id, 
         	scs_text.getText(ev.text_id,'^(ScsLang.toString lang)') as text
           from scs_enumerations e, scs_enum_values ev
          where e.enum_id = ev.enum_id
            and ev.active_p = 't'
            and ` ^^ wh ^^ `
          order by ev.ordering`

      fun mk_opts sql_part lang = Db.list (fn g => (g "text", g "val_id")) 
 	      (genSql sql_part lang)

    in
      fun select enum_id lang fv v_opt =
	let
	  val opts = mk_opts `e.enum_id = '^(Int.toString enum_id)'` lang
	in
          case v_opt of
            NONE => ScsWidget.select opts fv
          | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end

      local
	fun selectName_template enum_name lang add_opts fv v_opt =
	  let
	    val opts = add_opts @ (mk_opts `e.name = ^(Db.qqq enum_name)` lang)
	  in
	    case v_opt of
	      NONE => ScsWidget.select opts fv
	    | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	  end
      in
        fun selectName enum_name lang fv v_opt = 
	  selectName_template enum_name lang [] fv v_opt
        fun selectName' enum_name lang add_opts fv v_opt = 
	  selectName_template enum_name lang add_opts fv v_opt
      end

      fun selectNameExcerpt enum_name values lang fv v_opt =
	let
	  val opts = (mk_opts `e.name = ^(Db.qqq enum_name) and ev.value in (^(String.concatWith ", " (map Db.qqq values)))` lang)
	in
 	  case v_opt of
	      NONE   => ScsWidget.select opts fv
            | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end

      fun mk_radio_input_tag fv v_opt (text, value)  = 
	let
	  val attr = case v_opt of 
	      SOME v => if (Int.toString v)=value then "checked" else ""
	    | NONE   => ""
	in
	  (Html.inradio {name=fv, value=value} attr, `^text`)
	end

      fun radioList enum_id lang fv v_opt =
	let
	  val opts = mk_opts `e.enum_id = '^(Int.toString enum_id)'` lang
	in
	  map (mk_radio_input_tag fv v_opt) opts
	end

      fun radioListName enum_name lang fv v_opt = 
	let
	  val opts = mk_opts `e.name = ^(Db.qqq enum_name)` lang
	in
	  map (mk_radio_input_tag fv v_opt) opts
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

    fun getVal val_id =
      ScsError.wrapOpt 
      Db.oneField `select value
                     from scs_enum_values
                    where val_id = '^(Int.toString val_id)'`

    fun getVid enum_name value  = 
      let
        val vid_opt = ScsError.wrapOpt Db.oneField `
	select scs_enumeration.getVid(
	         ^(Db.qqq enum_name),
		 ^(Db.qqq value)
	       ) 
          from dual`
      in
        case vid_opt of
	    SOME vid => Int.fromString vid
	  | NONE     => NONE 
      end

    fun valToText (value,enum_name,lang) =
      Db.oneField `select text
                     from scs_enums
                    where name = ^(Db.qqq enum_name)
                      and value = ^(Db.qqq value)
                      and language = '^(ScsLang.toString lang)'`
     handle _ => ""

    fun getEnumErr enum_name = 
      let
	val all_values = allValues enum_name
        fun msgEnum s =
  	  (case ScsLogin.user_lang() of
	     ScsLang.en => `^s
	       You must choose among the following enumerations:
	       <blockquote>
	       ^(String.concatWith "," (List.map (fn enum => valName enum (ScsLogin.user_lang())) all_values))
	       </blockquote>`
	   | ScsLang.da => `^s
	       Du skal indtaste en af de følgende værdier:
	       <blockquote>
	       ^(String.concatWith "," (List.map (fn enum => valName enum (ScsLogin.user_lang())) all_values))
	       </blockquote>`)
        fun chkEnum v =
          case Int.fromString v of
            SOME val_id => (case List.find (fn enum => val_id = enum) all_values
                              of NONE => false
                            | SOME _ => true)
          | NONE => false
      in
        ScsFormVar.getErr 0 (Option.valOf o Int.fromString) 
          [(ScsLang.en,`enumeration`),(ScsLang.da,`enumerering`)] msgEnum chkEnum
      end

    fun enumName enum_id =
      Db.oneField `select scs_enum.getName('^(Int.toString enum_id)')
                     from dual`

    fun getEnumerationsFromDb enum_id =
      let
	val sql = `select ev.val_id,
                          ev.text_id,
                          scs_text.getText(ev.text_id,^(Db.qqq (ScsLang.toString ScsLang.en))) as text_eng,
                          scs_text.getText(ev.text_id,^(Db.qqq (ScsLang.toString ScsLang.da))) as text_da,
			  ev.value,
			  ev.active_p,
			  ev.ordering,
			  e.name
                     from scs_enumerations e, scs_enum_values ev
                    where e.enum_id = ev.enum_id
                      and e.enum_id = '^(Int.toString enum_id)'`
	fun f g : enum_value =
	  {val_id    = (ScsError.valOf o Int.fromString) (g "val_id"),
	   enum_id   = enum_id,
	   enum_name = g "enum_name",
	   text_id   = (ScsError.valOf o Int.fromString) (g "text_id"), 
	   text_da   = g "text_da",
	   text_eng  = g "text_eng",
	   value     = g "value",
	   active_p  = (ScsError.valOf o Db.toBool) (g "active_p"),
	   ordering  = (ScsError.valOf o Int.fromString) (g "ordering")}
      in
	ScsError.wrapPanic
	(Db.list f)
	sql
      end

    (*********)
    (* DICTS *)
    (*********)
    val value_dict    = [(ScsLang.da,`Værdi`),(ScsLang.en,`Value`)]
    val text_da_dict  = [(ScsLang.da,`Tekst (dansk)`),(ScsLang.en,`Text (Danish)`)]
    val text_eng_dict = [(ScsLang.da,`Tekst (engelsk)`),(ScsLang.en,`Text (English)`)]
    val active_p_dict = [(ScsLang.da,`Aktiv`),(ScsLang.en,`Active`)]
    val ordering_dict = [(ScsLang.da,`Rækkefølge`),(ScsLang.en,`Ordering`)]
    val edit_dict     = [(ScsLang.da,`Ret`),(ScsLang.en,`Edit`)]
    val save_dict     = [(ScsLang.da,`Gem`),(ScsLang.en,`Store`)]

    fun allValues' enum_name enum_from_DB_fn =
      let
	fun status_enumeration_sql () = `
	  select ev.val_id, ev.value, scs_text.getText(
	    ev.text_id, ^(Db.qqq (ScsLang.toString (ScsLogin.user_lang())))) as text
	    from scs_enum_values ev, scs_enumerations e
	    where ev.enum_id = e.enum_id
	      and e.name = ^(Db.qqq enum_name )`
	fun f g = ((enum_from_DB_fn o g) "value", g "text")
      in
        Db.list f (status_enumeration_sql())
      end

    fun allValuesAsText enum_name lang =
      let
	fun status_enumeration_sql () = `
	  select scs_text.getText(
	    ev.text_id, ^(Db.qqq (ScsLang.toString lang))
	  ) as text
	    from scs_enum_values ev, scs_enumerations e
	    where ev.enum_id = e.enum_id
	      and e.name = ^(Db.qqq enum_name )`
      in
        Db.list (fn g => g "text") (status_enumeration_sql())
      end

  end
