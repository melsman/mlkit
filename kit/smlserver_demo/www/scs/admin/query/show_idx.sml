val qs = (Db.fold (fn (g,acc) => acc ^^ `
		   <li>^(g "category"): <a href="show.sml?id=^(g "id")">^(g "name")</a>`) `<ul>`
            `select id, category, name
               from scs_query
              order by category, name`) ^^ `</ul>`

val _ = ScsPage.returnPg "Query Index" (`Index of defined queries in the database<p>

` ^^ qs)
