val examples =
  [("Time-of-day", "time_of_day.sml", []),
   ("Count up and down", "counter.sml", []),
   ("Temperature conversion", "temp.html", ["temp.sml"]),
   ("Dynamic recipe", "recipe.html", ["recipe.sml"]),
   ("Guess with Bill", "guess.sml", []),
   ("Form variables", "formvar.sml", ["formvar_chk.sml"]),
   ("Server information", "server.sml", []),
   ("Regular Expressions", "regexp.sml", []),
   ("Dictionary Cache","cache.sml", ["cache_add.sml","cache_lookup.sml"]),
   ("Currency service", "currency_cache.html", ["currency_cache.sml"]),
   ("Send an email", "mail_form.sml", ["mail.sml"]),
   ("Employee search (DB)", "employee/index.sml", ["search.sml","update.sml"]),
   ("Best Wines (DB)", "rating/index.sml", ["rating/rating.sql", "rating/add0.sml", 
						"rating/add.sml", "rating/wine.sml"]),
   ("Link database (DB)", "link/index.sml", ["link/add_form.sml", "link/add.sml", 
						  "link/delete.sml"]),
(*   ("Game of life", "life.sml"),  *)
   ("Hello world (MSP)", "hello.msp", []),
   ("Multiplication (MSP)", "mul.msp", []),
   ("Calendars (MSP)", "calendar.msp", []),
   ("Tables (MSP)", "test.msp", []),
   ("SMLserver images", "images/index.html", []),
   ("This index page", "index.sml", [])]

fun src_link n s = `<a href="^s.txt">^(Int.toString n)</a>`

fun sources n nil = ``
  | sources n [s] = src_link n s
  | sources n (s::ss) = src_link n s ^^ `, ` ^^ sources (n+1) ss

fun mkrow (desc, src, srcs) = 
  `<tr><td><font size=-2><a href="^src">^desc</a></font></td><td align=right>
   <font size=-2>` 
  ^^ sources 1 (src::srcs) ^^ `</font></td></tr>`

val _ = Page.return "SMLserver Examples" 
  (`See the <a href=http://www.smlserver.org>SMLserver 
    Home Page</a> for SMLserver news and updates.<p>
    <TABLE>
    <TR><TH>Example</TH><TH>source</TH></TR>` 
    ^^ Quot.concat (List.map mkrow examples) ^^ 
   `</TABLE>
    <p>
    Some of the <b>*.msp</b> examples are from the <a 
    href="http://ellemose.dina.kvl.dk/~sestoft/msp/index.msp">ML 
    Server Pages (MSP) homepage</a>.`)


