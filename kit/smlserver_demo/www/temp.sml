  fun calculate c = concat 
    ["<html> <body bgcolor=white> ",
     "<h2>Temperature Conversion</h2> ",
     Int.toString c, " degrees Celcius equals ",
     Int.toString (9 * c div 5 + 32), 
     " degrees Fahrenheit. <p> Go ",
     "<a href=temp.html>calculate a new temperature</a>.",
     "<hr> <i>Served by <a href=http://www.smlserver.org>",
     "SMLserver</a></i> </body></html>"]

  val _ = Ns.Conn.return 
    (case FormVar.wrapOpt FormVar.getIntErr "temp_c"
       of NONE => "Go back and enter an interger!"
	| SOME i => calculate i)
