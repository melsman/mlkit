val % = ScsDict.d ScsLang.English

val (target_lang,errs) = ScsFormVar.getLangErr ("target_lang", %"target language", ScsFormVar.emptyErr)
val (search_lang,errs) = ScsFormVar.getLangErr ("search_lang", %"search language", errs)
val (regexp,errs)      = ScsFormVar.getRegExpErr ("regexp", %"regular expression", errs)
val (limit_rows,errs)  = ScsFormVar.getNatErr ("limit_rows",%"limit rows", errs)
val _ = ScsFormVar.anyErrors errs

val _ = ScsPage.returnPg "Dictionary Entry Form" `hi there`
(*   (ScsWidget.grpTable
   {hdcolor="silver",row_col1="grey",row_col2="lightgrey",
    header=`<th>DictId</th><th>PhraseId</th><th>lang</th><th>text</th><th>&nbsp;</th>`,
    align="center",footer=``} 
    (fn (dict_id,phrase_id,lang,text) => 
     `<form method=post action=dict_upd.sml>
      <td>^dict_id</td>
     <td>^phrase_id</td>
     <td><input type=text value="^lang"></td>
     <td><input type=text value="^text"></td>
     <td><input type=submit value="Upd"></form>`) #2 dicts)*)
