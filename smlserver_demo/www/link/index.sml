
val query = 
  `select person.person_id, person.name, link_id,
          person.url as purl, link.url, link.text
     from person, link
    where person.person_id = link.person_id`

fun delete g =
  if valOf (Int.fromString (g"person_id")) 
    = Login.person_id then 
    ` (<a href=/link/delete.sml?link_id=^(g"link_id")>delete</a>)` 
  else ``

fun layoutRow (g, acc) =
  `<li><a href="^(g"url")">^(g"text")</a> - added
   by <a href="^(g"purl")">^(g"name")</a>` ^^ 
   delete g ^^ acc

val list = Db.fold (layoutRow, ``, query)

val _ =
  Page.return "Web-sites that use SMLserver"
  (`<ul>` ^^ list ^^
   `<p><li><a href=/link/add_form.sml>Add Web-site</a>
    </ul>`)