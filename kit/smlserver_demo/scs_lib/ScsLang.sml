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

    val toString : lang -> string
    val fromString : string -> lang
  end

structure ScsLang :> SCS_LANG =
  struct
    exception ScsLang of string
    datatype lang = da | en

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
    fun toText lang to_lang =
      Db.oneField `select scs_text.getText(scs_lang.language_name_tid,^(Db.qqq (toString to_lang))) as text
                     from scs_lang
                    where scs_lang.language = '^(toString lang)'`

    fun all_for_sel_box lang =
      (Db.list (fn g => (g "text",g "value")) 
       `select scs_text.getText(scs_lang.language_name_tid,^(Db.qqq (toString lang))) as text, 
               scs_lang.language as value
          from scs_lang
         order by text`)
      handle _ => []

  end
