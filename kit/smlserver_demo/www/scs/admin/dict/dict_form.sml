

val dicts = Db.list (fn g => (g "dict_id", g "phrase_id", g "lang", g "text"),
		     `select dict_id, phrase_id, lang, text from scs_dict order by phrase_id, lang`)

val _ = ScsPage.returnPg "Dictionary" 
   (ScsWidget.grpTable
   {hdcolor="silver",row_col1="grey",row_col2="lightgrey",
    header=`<th>DictId</th><th>PhraseId</th><th>lang</th><th>text</th><th>&nbsp;</th>`,
    align="center",footer=``} 
    (fn (dict_id,phrase_id,lang,text) => 
     `<form method=post action=dict_upd.sml>
      <td>^dict_id</td>
     <td>^phrase_id</td>
     <td><input type=text value="^lang"></td>
     <td><input type=text value="^text"></td>
     <td><input type=submit value="Upd"></form>`) #2 dicts)

