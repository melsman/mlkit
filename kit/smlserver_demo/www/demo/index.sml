val examples =
  [("Time-of-day", "time_of_day.sml", []),
   ("Count up and down", "counter.sml", []),
   ("Temperature conversion", "temp.html", ["temp.sml"]),
   ("Dynamic recipe", "recipe.html", ["recipe.sml"]),
   ("Guess with Bill", "guess.sml", []),
   ("Form variables", "formvar.sml", ["formvar_chk.sml"]),
   ("Server information", "server.sml", []),
   ("Currency service", "currency_cache.html", ["currency_cache.sml"]),
   ("Send an email", "mail_form.sml", ["mail.sml"]),
(*   ("Employee search", "employee/index.html"), *)
   ("Best Wines web-site", "rating/index.sml", ["rating/rating.sql", "rating/add0.sml", 
						"rating/add.sml", "rating/wine.sml"]),
   ("SMLserver link database (password protected)", "link/index.sml", ["link/add_form.sml", "link/add.sml", 
								       "link/delete.sml"]),
(*   ("Game of life", "life.sml"),  *)
   ("Hello world", "hello.msp", []),
   ("Multiplication table", "mul.msp", []),
   ("Generating calendars", "calendar.msp", []),
   ("A demo of various MSP styles", "test.msp", []),
   ("SMLserver images", "images/index.html", []),
   ("This index page", "index.sml", [])]

fun src_link n s = `<a href="^s.txt">^(Int.toString n)</a>`

fun sources n nil = ``
  | sources n [s1,s2] = src_link n s1 ^^ `, ` ^^ src_link (n+1) s2
  | sources n (s::ss) = src_link n s ^^ `, ` ^^ sources (n+1) ss

fun mkrow (desc, src, srcs) = 
  `<tr><td><a href="^src">^desc</a></td><td>` 
  ^^ sources 1 (src::srcs) ^^ `</td></tr>`

val _ = Page.return "SMLserver Examples" 
  (`See the <a href=http://www.smlserver.org>SMLserver 
    Home Page</a> for SMLserver news and updates.<p>
    <h2>Examples</h2>

    <TABLE BORDER>
    <TR><TH>Example</TH><TH>source files</TH></TR>` 
    ^^ Quot.concat (List.map mkrow examples) ^^ 
   `</TABLE>
    <p>
    Some of the <b>*.msp</b> examples are from the <a 
    href="http://ellemose.dina.kvl.dk/~sestoft/msp/index.msp">ML 
    Server Pages (MSP) homepage</a>.`)


