val examples =
  [("Time of day", "time_of_day.sml", []),
   ("Count up and down", "counter.sml", []),
   ("Temperature conversion", "temp.html", ["temp.sml"]),
   ("Dynamic recipe", "recipe.html", ["recipe.sml"]),
   ("Guess with Bill", "guess.sml", []),
   ("Form variables", "formvar.sml", ["formvar_chk.sml"]),
   ("Server information", "server.sml", []),
   ("Regular Expressions", "regexp.sml", []),
   ("Dictionary Cache","cache.sml", ["cache_add.sml","cache_lookup.sml","cache_fib.sml"]),
   ("Currency service", "currency_cache.html", ["currency_cache.sml"]),
   ("Currency exchange", "exchange.sml", []),
   ("Send an email", "mail_form.sml", ["mail.sml"]),
   ("Guest book (DB)", "guest.sml", ["guest_add.sml"]),
   ("Employee search (DB)", "employee/index.sml", ["employee/search.sml","employee/update.sml"]),
   ("Best Wines (DB)", "rating/index.sml", ["rating/rating.sql", "rating/add0.sml", 
						"rating/add.sml", "rating/wine.sml"]),
   ("Link database (DB)", "link/index.sml", ["link/add_form.sml", "link/add.sml", 
						  "link/delete.sml"]),
   ("Cookie example", "cookie.sml", ["cookie_set.sml", "cookie_delete.sml"]),
(*   ("Game of life", "life.sml"),  *)
   ("Hello world (MSP)", "hello.msp", []),
   ("Multiplication (MSP)", "mul.msp", []),
   ("Calendars (MSP)", "calendar.msp", []),
   ("Tables (MSP)", "test.msp", []),
   ("Database testing (DB)", "db_test.sml", []),
   ("SMLserver images", "images/index.html", []),
   ("Trap","trap.txt", []),
   ("Upload", "upload/upload_form.sml",[]),
   ("This index page", "index.sml", [])]

fun src_link n s = `<a href="return_file.sml?path=^(Ns.encodeUrl ("demo/" ^ s))">^(Int.toString n)</a>`

fun sources n nil = ``
  | sources n [s] = src_link n s
  | sources n (s::ss) = src_link n s ^^ `, ` ^^ sources (n+1) ss

fun mkrow (desc, src, srcs) = 
  `<tr><td><font size=-1><a href="^src">^desc</a></font></td><td align=right>
   <font size=-1>` 
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
