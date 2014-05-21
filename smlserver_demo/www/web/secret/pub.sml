fun url (x : string ,y) = y ^^ `<tr><th>Url</th><td> ^(x) </td></tr>`

val _ = Page.return "Information" (`
<table border=1> `
^^ (foldl url `` (Web.Conn.url())) ^^
`</table>`)

