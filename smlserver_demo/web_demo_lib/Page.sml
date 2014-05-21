signature PAGE =
  sig
    val return : string -> quot -> unit
    val panic : quot -> 'a
  end

(*

[returnPg head body] writes a standard page to the client containing
the heading `head' and the body `body'.

[panic body] writes a standard error message to the client and reports
the error in the log file, whereafter the function calls Web.exit.

*)

structure Page : PAGE =
  struct
    fun return head body = Web.return 
      (`<html>
          <head><title>^head</title>
          </head>
          <body bgcolor=white>
             <h2>^head</h2> ` ^^ 
             body ^^
             `<hr><i>Served by 
              <a href=http://www.smlserver.org>SMLserver</a></i>,
	     <i><a href="/web/index.sml">Back to index page</a>.</i>
          </body>
        </html>`)

    fun panic body =
      (Web.log (Web.Error, Quot.toString body);
       return "Internal Error" body;
       Web.exit())
  end
         
