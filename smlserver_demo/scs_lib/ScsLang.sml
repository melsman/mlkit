signature SCS_LANG =
  sig
    (* This module uses the languages defined in table scs_lang *)
    exception ScsLang of string
    datatype lang = da | en

    val all : lang list
  
    (* [all_as_text lang] returns a list of supported languages in the
    language lang. If the language lang is not supported, then the
    empty list is returned. *)
    val all_as_text : lang -> string list

    (* [all_for_sel_box lang] returns a list of pair (text,value) that
    may be used to build a selection list of all languages in the
    user's preferred language. *)
    val all_for_sel_box : lang -> (string * string) list

    (* [toText lang to_lang] return the text-description of language
    lang in language to_lang. Used when building selection-boxes. *)
    val toText      : lang -> lang -> string

    (* [toString lang] returns the string representation of the
       language lang as stored in the database. *)
    val toString : lang -> string

    (* [fromString str] returns the language described by str as
       stored in the database. Raises ScsLang if the str is not
       recognized. *)
    val fromString : string -> lang

    (* [ppLang out_lang l] pretty prints language l in language out_lang. *)
    val ppLang : lang -> lang -> string

    val isDanish: lang -> bool
    val isEnglish: lang -> bool
  end

structure ScsLang :> SCS_LANG =
  struct
    exception ScsLang of string
    datatype lang = da | en

    fun isDanish da = true
      | isDanish _  = false

    fun isEnglish en = true
      | isEnglish _  = false

    val all = [da,en]

    fun toString da = "da"
      | toString en = "en"
    fun fromString "da" = da
      | fromString "en" = en
      | fromString l = raise ScsLang ("Language " ^ l ^ " not known")

    fun all_as_text lang =
      (Db.list (fn g => g "text") `select scs_text.getText(scs_lang.language_name_tid,^(Db.qqq (toString lang))) as text
                                     from scs_lang
                                    order by text`)
      handle _ => []

    (* We cache the result for 24 hours.
       Cache def: (lang, to_lang) -> string
       We flush the cache when we edit roles, (i.e., se script files
       in directory /web/ucs/www/scs/admin/role/ *)

    local
      val lang_cache_def = 
	Ns.Cache.get(Ns.Cache.Pair Ns.Cache.String Ns.Cache.String,
		     Ns.Cache.String,
		     "ScsLang",
		     Ns.Cache.WhileUsed (24*60*60))

      fun toText' (lang, to_lang) = Db.oneField `
	select scs_text.getText(
	  	 scs_lang.language_name_tid, ^(Db.qqq to_lang)
	       ) as text
	  from scs_lang
	 where scs_lang.language = ^(Db.qqq lang)`

      val toText'_cache = Ns.Cache.memoize lang_cache_def toText'
    in
      fun toText' (lang, to_lang) = toText'_cache (lang, to_lang)
    end

    fun toText lang to_lang = toText' (toString lang, toString to_lang)

    fun all_for_sel_box lang =
      (Db.list (fn g => (g "text",g "value")) 
       `select scs_text.getText(scs_lang.language_name_tid,^(Db.qqq (toString lang))) as text, 
               scs_lang.language as value
          from scs_lang
         order by text`)
      handle _ => []

    fun ppLang out_lang l =
      case l of 
	da => 
	  (case out_lang of 
	     da => "dansk"
	     | en => "Danish")
      | en => 
         (case out_lang of
	    da => "engelsk"
	  | en => "English")
  end

