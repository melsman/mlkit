val examples =
  [("Time-of-day", "time_of_day.sml"),
   ("Count up and down", "counter.sml"),
   ("Temperature conversion", "temp.html"),
   ("Dynamic recipe", "recipe.html"),
   ("Guess with Bill", "guess.sml"),
   ("Form variables", "formvar.sml"),
   ("Server information", "server.sml"),
   ("Currency service", "currency_cache.html"),
   ("Send an email", "mail_form.sml"),
(*   ("Employee search", "employee/index.html"), *)
   ("Best Wines web-site", "rating/index.sml"),
   ("SMLserver link database (password protected)", "link/index.sml"),
(*   ("Game of life", "life.sml"),  *)
   ("Hello world", "hello.msp"),
   ("Multiplication table", "mul.msp"),
   ("Generating calendars", "calendar.msp"),
   ("A demo of various MSP styles", "test.msp"),
   ("SMLserver images", "images/index.html"),
   ("This index page", "index.sml")]

fun mkrow (desc, file) = `
  <tr><td>^desc</td><td><a href="^file">^file</a></td></tr>`

val _ = Page.return "SMLserver Examples" 
  (`See the <a href=http://www.smlserver.org>SMLserver 
    Home Page</a> for SMLserver news and updates.<p>
    <h2>Examples</h2>

    <TABLE BORDER>
    <TR><TH>Description</TH><TH>Run script</TH></TR>` 
    ^^ Quot.concat (List.map mkrow examples) ^^ 
   `</TABLE>
    <p>
    Some of the <b>*.msp</b> examples are from the <a 
    href="http://ellemose.dina.kvl.dk/~sestoft/msp/index.sml">ML 
    Server Pages (MSP) homepage</a>.`)


