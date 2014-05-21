fun url (x : string ,y) = y ^^ `<tr><th>Url</th><td> ^(x) </td></tr>`

val _ = Page.return "Server Information" (`
<table border=1>
<tr><th>Hostname</th>           <td> ^(Web.Info.hostname()) </td></tr>
<tr><th>Pid</th>                <td> ^(Int.toString (Web.Info.pid())) </td></tr>
<tr><th>Uptime (seconds)</th>   <td> ^(Int.toString (Web.Info.uptime())) </td></tr>
<tr><th>Pageroot</th>           <td> ^(Web.Info.pageRoot()) </td></tr>
<tr><th>User</th>               <td> ^(Option.getOpt(Web.Info.getUser(),"")) </td></tr> 
<tr><th>AuthType</th>           <td> ^(Option.getOpt(Web.Info.getAuthType(),"")) </td></tr> 
</table>

<h2>Connection Information</h2>
<table border=1>
<tr><th>Scheme</th>             <td> ^(Web.Conn.scheme()) </td></tr>
<tr><th>Host</th>               <td> ^(Web.Conn.host()) </td></tr>
<tr><th>Location</th>           <td> ^(Web.Conn.location()) </td></tr>`
^^ (foldl url `` (Web.Conn.url())) ^^
`<tr><th>Peer</th>               <td> ^(Web.Conn.peer()) </td></tr>
<tr><th>Server Port</th>        <td> ^(Int.toString (Web.Conn.port())) </td></tr>
<tr><th>Server Name</th>        <td> ^(Web.Conn.server()) </td></tr> 
<tr><th>Method</th>             <td> ^(Web.Conn.method()) </td></tr> 
<tr><th>Content Length</th>    	<td> ^(Int.toString(Web.Conn.contentLength())) </td></tr> 
</table>

<h2>Headers Information</h2>
<table border=1>
<tr><th>Key</th><th>Value</th></tr>
^(concat(Web.Set.foldr(fn ((k,v),acc) => 
		      "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc) 
	 nil (Web.Conn.headers())))
</table>

<h2>Form Data</h2>
<table border=1>
<tr><th>Key</th><th>Value</th></tr>
^(case Web.Conn.getQuery()
    of SOME s => 
      concat(Web.Set.foldr(fn ((k,v),acc) => 
			  "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc) 
	     nil s)
     | NONE => "<tr><td colspan=2>No form data</td></tr>")
</table>

<h2>Some Configuration Information</h2>
<table border=1>
<tr><th>MailRelay</th>  <td> ^(case Web.Info.configGetValue(Web.Info.Type.String, "MailRelay") 
                                of NONE => " " 
                                 | SOME(s) => s) </td></tr>
<tr><th>Number of heap caches</th><td> ^(Int.toString (Option.valOf
                                           (Web.Info.configGetValue
                                              (Web.Info.Type.Int,"MaxHeapPoolSz")))) </td></tr>
</table>

<h2>Request Data</h2>
<table border=1>
<tr><td>^(Web.Conn.getRequestData())
</td>
</tr></table>
`
)
