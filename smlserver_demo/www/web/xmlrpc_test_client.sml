local 
    open Web.XMLrpc
    fun call name t1 t2 a =
	rpc t1 t2 {url="http://localhost/web/xmlrpc_test_server.sml",
		   method=name} a
in
    val add = call "add" (pair(int,int)) int
    val neg = call "neg" int int
end

val res1 = Int.toString (neg(add(11,neg 5)))
  handle Web.XMLrpc.TypeConversion => "TypeConversion Error"

val res2 = Int.toString (add(12,200))
  handle Web.XMLrpc.TypeConversion => "TypeConversion Error"

val res3 = Int.toString (neg 12)
  handle Web.XMLrpc.TypeConversion => "TypeConversion Error"

val () = Web.return `
<html><body>
<h3>XML-RPC Example</h3>

<p>
Each of the calculations below are made using <a
href="http://en.wikipedia.org/wiki/XML-RPC">XML-RPC</a> client calls to an XML-RPC
server, which implements the neg and add11 operations. To see how easy
it is to make your ML functions available as XML-RPC methods, see the
source code for the server and the client available from the <a
href="index.sml">index page</a>.
</p>

<table border=1>
<tr><th>Expression</th><th>Result</th></tr>
<tr><td>neg(add(11,neg 5))</td><td>^res1</td></tr>
<tr><td>add(12,200)</td><td>^res2</td></tr>
<tr><td>neg 12</td><td>^res3</td></tr>
</table>
</body></html>`
