
val query = 
  `select person.person_id, person.name, link_id,
          person.url as purl, link.url, link.text
     from person, link
    where person.person_id = link.person_id`

fun delete g =
  if valOf (Int.fromString (g"person_id")) 
    = Login.person_id then 
    ` (<a href=delete.sml?link_id=^(g"link_id")>delete</a>)` 
  else ``

fun layoutRow (g, acc) =
  `<li><a href="^(g"url")">^(g"text")</a> - added
   by <a href="^(g"purl")">^(g"name")</a>` ^^ 
   delete g ^^ acc

val loginout = 
  if Login.person_id = 0 then
    `To manage links that you have entered, you must 
     <a href=../auth_form.sml?target=link/>login</a>.`
  else 
    let val name = 
      Db.oneField 
      `select name 
       from person
       where person_id = ^(Int.toString Login.person_id)`
    in
      `You are logged in as user ^name - you may 
       <a href=../auth_logout.sml>logout</a>.`
    end
      
val list = Db.fold (layoutRow, ``, query)

val _ =
  Page.return "Web-sites that use SMLserver"
  (loginout ^^ `<ul>` ^^ list ^^ 
   `<p><li><a href=add_form.sml>Add Web-site</a></ul>`)