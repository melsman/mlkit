val (on_ok_url,errs) = ScsFormVar.getStringErr("on_ok_url","On ok url",ScsFormVar.emptyErr)
val (title,errs) = ScsFormVar.getStringErr("title","Title",errs)
val (message,errs) = ScsFormVar.getStringErr("message","Message",errs)
val _ = ScsFormVar.anyErrors errs

val _ = ScsPage.returnPg title `
  ^message
 
  <p>Click <a href="^on_ok_url">ok</a> to complete the action.<p>
  Click the browsers back-button to return to the previous page.<p>
`
