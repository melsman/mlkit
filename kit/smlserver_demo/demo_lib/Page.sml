signature PAGE =
  sig
    val return : string -> quot -> Ns.status
    val panic : quot -> 'a
  end

(*

[returnPg head body] writes a standard page to the client containing
the heading `head' and the body `body'.

[panic body] writes a standard error message to the client and reports
the error in the log file, whereafter the function calls Ns.exit.

*)

structure Page : PAGE =
  struct
    fun return head body = Ns.return 
      (`<html>
          <head><title>^head</title>
          </head>
          <body bgcolor=white>
             <h2>^head</h2> ` ^^ 
             body ^^
             `<hr><i>Served by 
              <a href=http://www.smlserver.org>SMLserver</a></i>,
	     <i><a href="/demo/index.sml">Back to index page</a>.</i>
          </body>
        </html>`)

    fun panic body =
      (Ns.log (Ns.Error, Quot.toString body);
       return "Internal Error" body;
       Ns.exit())
  end
         
