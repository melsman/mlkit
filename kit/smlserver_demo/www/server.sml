val user = getOpt(Ns.Info.configGetValue{sectionName="ns/parameters", key="user"}, "---")
val _ =
Ns.Quot.return `<HTML>
<BODY bgcolor=lightgreen>
<h2>Server Information</h2>

<table bgcolor=grey border=1 align=center>
<tr><th>Configuration File</th> <td> ^(Ns.Info.configFile()) </td></tr>
<tr><th>Error Log</th>          <td> ^(Ns.Info.errorLog()) </td></tr>
<tr><th>Home Path</th>          <td> ^(Ns.Info.homePath()) </td></tr>
<tr><th>Hostname</th>           <td> ^(Ns.Info.hostname()) </td></tr>
<tr><th>Pid</th>                <td> ^(Int.toString (Ns.Info.pid())) </td></tr>
<tr><th>Server Version</th>     <td> ^(Ns.Info.serverVersion()) </td></tr>
<tr><th>Uptime (seconds)</th>   <td> ^(Int.toString (Ns.Info.uptime())) </td></tr>
<tr><th>Pageroot</th>           <td> ^(Ns.Info.pageRoot()) </td></tr>
</table>

<h2>Connection Information</h2>
<table bgcolor=grey border=1 align=center>
<tr><th>Host</th>               <td> ^(Ns.Conn.host()) </td></tr>
<tr><th>Location</th>           <td> ^(Ns.Conn.location()) </td></tr>
<tr><th>Url</th>                <td> ^(Ns.Conn.url()) </td></tr>
<tr><th>Peer</th>               <td> ^(Ns.Conn.peer()) </td></tr>
<tr><th>Peer Port</th>          <td> ^(Int.toString (Ns.Conn.peerPort())) </td></tr>
<tr><th>Server Port</th>        <td> ^(Int.toString (Ns.Conn.port())) </td></tr>
<tr><th>Server Name</th>        <td> ^(Ns.Conn.server()) </td></tr>
</table>

<h2>Headers Information</h2>
<table bgcolor=grey border=1 align=center>
<tr><th>Key</th><th>Value</th></tr>
^(concat(Ns.Set.foldr(fn ((k,v),acc) => 
		      "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc) 
	 nil (Ns.Conn.headers())))
</table>

<h2>Form Data</h2>
<table bgcolor=grey border=1 align=center>
<tr><th>Key</th><th>Value</th></tr>
^(case Ns.Conn.getQuery()
    of SOME s => 
      concat(Ns.Set.foldr(fn ((k,v),acc) => 
			  "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc) 
	     nil s)
     | NONE => "<tr><td colspan=2>No form data</td></tr>")
</table>

<h2>Some Mime-Types</h2>
<table bgcolor=grey border=1 align=center>
<tr><th>File</th><th>Mime-Type</th></tr>
^(concat(foldr (fn (f, acc) => 
		"<tr><td>" :: f :: "</td><td>" ::
		Ns.getMimeType f :: "</td></tr>" :: acc) nil
	 ["file.png", "file.tcl", "file.jpg", "file.dvi", 
	  "file.pdf", "file.ps", "file.doc", "file.gif"]))
</table>

<h2>Some Configuration File Information</h2>
<table bgcolor=grey border=1 align=center>
<tr><th>ns/parameters debug</th>
    <td>^(getOpt(Ns.Info.configGetValue{sectionName="ns/parameters", key="debug"}, "none")) </td>
</tr>
<tr><th>ns/server/^user directoryfile</th>
    <td>^(getOpt(Ns.Info.configGetValue{sectionName="ns/server/"^user, key="directoryfile"}, "none")) </td>
</tr>
<tr><th>ns/server/^user/db</th>
    <td>^(getOpt(Ns.Info.configGetValue{sectionName="ns/server/"^user^"/db", key="Pools"}, "none")) </td>
</tr>
<tr><th>ns/parameters not-there</th>
    <td>^(getOpt(Ns.Info.configGetValue{sectionName="ns/parameters", key="not-there"}, "none")) </td>
</tr>
</table>

</BODY>
</HTML>`
