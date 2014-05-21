fun log x = Web.log(Web.Debug, x)

val _ = log ("1")
  val person = Auth.verifyPerson()
val _ = log ("1")

val pid = Web.Info.pid()
val _ = log("pid: " ^ (Int.toString pid))

  val query = 
    `select person.person_id, person.name, link_id,
	    person.url as purl, link.url, link.text
       from person, link
      where person.person_id = link.person_id`

val _ = log ("1")
  fun delete g =
    if Int.fromString (g"person_id") = person
      then 
	` <a href=delete.sml?link_id=^(g "link_id")>delete</a>` 
    else ``

val _ = log ("1")
  fun layoutRow (g, acc) =
    `<li><table width=100% cellspacing=0 cellpadding=0 
		border=0><tr>
       <td width=50%><a href="^(g"url")">^(g"text")</a>
       <td>added by <a href="^(g "purl")">^(g"name")</a>
       <td align=right>` ^^ delete g ^^ 
    `</tr></table>` ^^ acc

val _ = log ("1")
  val loginout = 
    case person
      of NONE =>
	`To manage links that you have entered, please
	 <a href=../auth_form.sml?target=link/>login</a>.`
       | SOME p => 
	let val name = Db.oneField 
	      `select name from person
	       where person_id = ^(Int.toString p)`
	in `You are logged in as user ^(name) - you may 
	    <a href=../auth_logout.sml>logout</a>.`
	end

val _ = log ("2")
  val list = Db.fold layoutRow `` query 
val _ = log ("3")

  val _ =
    Page.return "Web sites that use SMLserver"
    (loginout ^^ `<ul>` ^^ list ^^ 
     `<p><li><a href=add_form.sml>Add Web site</a></ul>`)
