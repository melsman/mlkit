structure ScsPage :> SCS_PAGE =
  struct
    type navbar = (quot * quot) list
    fun navbar nb = Quot.concatWith "/" (List.map (fn (l,n) => Html.ahref l n) nb)

    fun returnPg title body = Ns.return 
      (case ScsLogin.user_lang of
	 ScsLang.English => `
	   <html>
	   <head>
	   <title>^title</title>
	   </head>
	   <body bgcolor=white>
	   <table cellspacing=0 cellpadding=1 bgcolor=white border=0 width=100%>
	   <tr><th align=left width=30%><a href="http://smlserver.org">
           <img border=0 src=/images/smlserver_logo_color_medium.png></a></th>
	   <th width=40%>&nbsp;</th>
	   <th align=left width=30%><a href="http://www.it.edu">
	   <img border=0 src=/images/itc_logo_white.png></a></th></tr>
	   </table> <p>
	   <table width=100% border=0 cellpadding=10 cellspacing=0><tr><td>
	   <h1>^title</h1>
	   ` ^^ body ^^ `
	   </td></tr></table>
	   <p><hr>
	   <a href=http://smlserver.org><img border=0 src=/images/poweredby_smlserver_logo1.png align=right
	   alt="You are free to use this logo on your SMLserver scripts"></a>

	   <a href="http://www.smlserver.org/">SMLserver Home Page</a>,  
	   <a href="/index.sml">Example Home Page</a> 
	   (<a href="mailto:smlserver@it.edu">smlserver@it.edu</a>) ^(ScsDate.ppIso (ScsDate.now_local()))
	   </body>
	   </html>`
	 | ScsLang.Danish => `
	   <html>
	   <head>
	   <title>^title</title>
	   </head>
	   <body bgcolor=white>
	   <table cellspacing=0 cellpadding=1 bgcolor=white border=0 width=100%>
	   <tr><th align=left width=30%><a href="http://smlserver.org">
           <img border=0 src=/images/smlserver_logo_color_medium.png></a></th>
	   <th width=40%>&nbsp;</th>
	   <th align=left width=30%><a href="http://www.it-c.dk">
	   <img border=0 src=/images/itc_logo_white.png></a></th></tr>
	   </table> <p>
	   <table width=100% border=0 cellpadding=10 cellspacing=0><tr><td>
	   <h1>^title</h1>
	   ` ^^ body ^^ `
	   </td></tr></table>
	   <p><hr>
	   <a href=http://smlserver.org><img border=0 src=/images/poweredby_smlserver_logo1.png align=right
	   alt="Det er tilladt at anvende disse logoer i dine SMLserver programmer"></a>

	   <a href="http://www.smlserver.org/">Hjemmeside for SMLserver</a>,  
	   <a href="/index.sml">Eksempelsiden</a> 
	   (<a href="mailto:smlserver@it.edu">smlserver@it.edu</a>) ^(ScsDate.ppIso (ScsDate.now_local()))
	   </body>
	   </html>`)
  end
