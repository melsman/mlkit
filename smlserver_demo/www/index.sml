val _ = ScsPage.returnPg "SMLserver Examples" (`
  See the <a href=http://www.smlserver.org>SMLserver Home Page</a> for
  SMLserver news and updates.<p>

  <h2>Examples</h2>

  <TABLE BORDER>
<TR><TH>Description</TH><TH>Run script</TH></TR>
` ^^ 
let
  fun mkrow (desc, file) = `
    <tr><td>^desc</td><td><a href="^file">^file</a></td></tr>`
in 
  Quot.concat (List.map mkrow 
	       [("Hello world", "hello.msp"),
		("Fibonacci", "hello.sml"),
		("Generating a calendar", "calendar.msp"),
		("Server Information", "server.sml"),
		("Cache Demonstration", "cache.sml"),
		("Time-of-day", "time_of_day.sml"),
		("Multiplication Table", "mul.msp"),
		("Temperature Conversion", "temp.html"),
		("Dynamic Recipe", "recipe.html"),
		("Guess a Number", "guess.sml"),
		("Count Up and Down", "counter.sml"),
		("Generating an index of a subdirectory", "fileindex.msp"),
		("Logging information to a file", "logtofile.msp"),
		("A demo of various MSP styles", "test.msp"),
		("Send an Email", "mail_form.sml"),
		("Character Support", "cs.html"),
		("User Group Example", "ug.sml"),
		("The RegExp Structure", "regexp.sml"),
		("Currency Service", "currency.html"),
		("Cookies", "cookie.sml"),
		("Form variables", "formvar.sml"),
		("Authentication", "auth_example.sml"),
		("Email Example (password protected, see the <a href=\"auth_example.sml\">authentication</a> example)",
		 "email_form.sml"),
		("SMLserver Images", "images/index.html"),
		("This index page", "index.sml")])
end ^^ `

</TABLE>

<p>
Most of the <b>*.msp</b> examples are taken from the <a
href="http://ellemose.dina.kvl.dk/~sestoft/msp/index.sml">ML Server
Pages (MSP) homepage</a>. The first MSP implementation used <a
href="http://www.dina.kvl.dk/~sestoft/mosml.html">Moscow ML</a>.
`)


